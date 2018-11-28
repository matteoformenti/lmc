:- consult(input_manipulation).
:- consult(instructions).
:- consult(compiler).

/* Assmbly compilation and loading */
lmc_load(Filename, Mem) :-
  open(Filename, read, Stream),
  read_single_line(Stream, Mem, 0),
  close(Stream).

/*  Line-By-line file reading and decoding */
read_single_line(Stream, [], _) :-
  at_end_of_stream(Stream), !.
read_single_line(Stream, [Compiled | OtherInstructions], Line) :-
  \+ at_end_of_stream(Stream),
  read_string(Stream, "\n", "", _, Row),
  sanitize(Row, Sanitized),
  compile_instruction(Sanitized, Line, Compiled),
  NextLine is Line+1,
  read_single_line(Stream, OtherInstructions, NextLine).


/* Compilazione del codice assembly */
/*compile_instruction(["ADD", Param | _], CompiledInstruction, Line) :-
  concat("_1_", Param, CompiledInstruction), !.
compile_instruction(["SUB", Param | _], CompiledInstruction, Line) :-
  concat("2", Param, CompiledInstruction), !.
compile_instruction(["STA", Param | _], CompiledInstruction, Line) :-
  concat("3", Param, CompiledInstruction), !.
compile_instruction(["LDA", Param | _], CompiledInstruction, Line) :-
  concat("5", Param, CompiledInstruction), !.
compile_instruction(["BRA", Param | _], CompiledInstruction, Line) :-
  concat("6", Param, CompiledInstruction), !.
compile_instruction(["BRZ", Param | _], CompiledInstruction, Line) :-
  concat("7", Param, CompiledInstruction), !.
compile_instruction(["BRP", Param | _], CompiledInstruction, Line) :-
  concat("8", Param, CompiledInstruction), !.
compile_instruction(["INP" | _], "901") :- !.
compile_instruction(["OUT" |[Instruction] _], "902") :- !.
compile_instruction(["HLT" | _], "000") :- !.
compile_instruction(["DAT" | Param], Param) :- !.
compile_instruction(["DAT" | _], "0") :- !.

compile_instruction([Label, "ADD", Param | _], CompiledInstruction, Line) :-
  assert(define_label(Label, Line)),
  concat("1", Param, Concat1), concat(Label, Concat1, CompiledInstruction), !.

compile_instruction(_, _, Line) :-
  format("~s  is not a valid LMC function at line ~i ~n", [Instruction, Line]), fail, !.
*/
