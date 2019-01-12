%   Matteo Formenti 830594
:- consult(io).
:- consult(input_manipulation).
:- consult(instructions).
:- consult(compiler).
:- consult(emulator).
:- consult(util).

%   Compile and run assembly file
lmc_run(Filename, InputQueue, OutputQueue) :- !,
    lmc_load(Filename, Memory),
    execution_loop(state(0, 0, Memory, InputQueue, [], noflag), OutputQueue).
%   Compile and load assemblu file
lmc_load(Filename, PaddedMem) :-
    open(Filename, read, Stream),
    read_string(Stream, _, File),
    close(Stream),
    split_string(File, "\n", "\n", SplitMem),
    asserta(define_label('', '')),
    parse_lines(SplitMem, UnresolvedMem, 0),
    resolve_labels(UnresolvedMem, Mem, 0),
    retractall(define_label(_, _)),
    asserta(define_label('', '')),
    memory_size(Mem, PaddedMem),
    ansi_format(fg(cyan), "Input ~w compiled correctly~n", [Filename]).
%   Fail if memory has more than 100 elements
memory_size(Mem, _) :-
    length(Mem, Length),
    Length>100, !,
    ansi_format(fg(red),
                "Memory too big, max allowed is 100 [current: ~w]~n",
                [Length]),
    fail.
%   Padding the memory to 100 elements
memory_size(Mem, PaddedMem) :-
    length(Mem, Length),
    Length<100,
    pad_memory(Mem, PaddedMem, Length).
%   Base case
memory_size(Mem, Mem).
%   Recursive list padding
pad_memory(Mem, Mem, 100) :- !.
pad_memory(Mem, PaddedMem, X) :-
    NewX is X+1,
    append(Mem, ['000'], Tmp),
    pad_memory(Tmp, PaddedMem, NewX).

one_instruction(State, NewState) :-
    State=..[state, _, Pc, Mem, _, _, _],
    get_cell(Mem, Pc, Instruction),
    atom_chars(Instruction, [OpCodeAtom|ValueChars]),
    atom_chars(ValueAtom, ValueChars),
    atom_number(OpCodeAtom, OpCode),
    atom_number(ValueAtom, Value), !,
    execute(OpCode, Value, State, NewState).
one_instruction(State, NewState) :- !,
    State=..[state, _, Pc, Mem, _, _, _],
    get_cell(Mem, Pc, Instruction),
    atom_chars(Instruction, [OpCodeAtom]),
    atom_number(OpCodeAtom, OpCode),
    execute(OpCode, _, State, NewState).
execution_loop(State, Out) :-
    one_instruction(State, NewState),
    NewState=..[state, _, _, _, _, _, _], !,
    execution_loop(NewState, Out).
execution_loop(State, Out) :-
    one_instruction(State, NewState),
    NewState=..[halted_state, _, _, _, _, Out, _], !,
    ansi_format(fg(blue), "Execution completed~n", []).