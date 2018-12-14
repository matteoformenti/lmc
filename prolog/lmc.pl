:- consult(io).
:- consult(input_manipulation).
:- consult(instructions).
:- consult(compiler).
:- consult(emulator).
:- consult(util).

/* Assmbly compilation and loading */
lmc_load(Filename, Mem) :-
    open(Filename, read, Stream),
    read_string(Stream, _, File),
    close(Stream),
    split_string(File, "\n", "\n", SplitMem),
    parse_lines(SplitMem, UnresolvedMem, 0),
    resolve_labels(UnresolvedMem, Mem, 0),
    ansi_format(fg(cyan), "Input ~w compiled correctly~n", [Filename]).


one_instruction(State, NewState) :-
    State=..[state, _, Pc, Mem, _, _, _],
    get_cell(Mem, Pc, Instruction),
    atom_chars(Instruction, [OpCodeAtom|ValueChars]),
    atom_chars(ValueAtom, ValueChars),
    atom_number(OpCodeAtom, OpCode),
    atom_number(ValueAtom, Value),
    execute(OpCode, Value, State, NewState).