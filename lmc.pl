consult(string_manipulation);

/* Compilazione del file assembly e caricamento della memoria. */
lmc_load(Filename, Mem) :-
  open(Filename, read, Stream),
  read_single_line(Stream, Mem, 0),
  close(Stream),
  write(Mem), nl.
/*  Lettura di una singola riga dallo stream */
read_single_line(Stream, []) :-
  at_end_of_stream(Stream), !.
read_single_line(Stream, [CompiledInstruction | OtherInstructions], Line) :-
  \+ at_end_of_stream(Stream),
  read_string(Stream, "\n", "", _, InstructionRaw),
  validate_instruction(InstructionRaw, Instruction),
  split_string(Instruction, " ", "", SplittedInstruction),
  Line is Line+1,
  compile_instruction(SplittedInstruction, CompiledInstruction, Line),
  read_single_line(Stream, OtherInstructions).
/* Pulizia degli eventuali commenti o spazi inutili e verifica della validità */
validate_instruction(Instruction, ValidatedInstruction) :-
  string_upper(Instruction, ValidatedInstruction_part1),
  remove_spaces(ValidatedInstruction_part1, ValidatedInstruction_part2),
  remove_comments(ValidatedInstruction_part2, ValidatedInstruction).
  %check_instruction_syntax(ValidatedInstruction_part3, ValidatedInstruction).



/* Compilazione del codice assembly */
compile_instruction(["ADD", Param | _], CompiledInstruction, Line) :-
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
compile_instruction(["OUT" | _], "902") :- !.
compile_instruction(["HLT" | _], "000") :- !.
compile_instruction(["DAT" | Param], Param) :- !.
compile_instruction(["DAT" | _], "0") :- !.

compile_instruction([Label, "ADD", Param | _], CompiledInstruction, Line) :-
  assert(define_label(Label, Line)),
  concat("1", Param, Concat1), concat(Label, Concat1, CompiledInstruction), !.

compile_instruction([Instruction | _], _, Line) :-
  format("~s  is not a valid LMC function at line ~i ~n", [Instruction, Line]), fail, !.
