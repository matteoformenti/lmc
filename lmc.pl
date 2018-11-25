/* Compilazione del file assembly e caricamento della memoria. */
lmc_load(Filename, Mem) :-
  open(Filename, read, Stream),
  read_single_line(Stream, Mem),
  close(Stream),
  write(Mem), nl.
/*  Lettura di una singola riga dallo stream */
read_single_line(Stream, []) :-
  at_end_of_stream(Stream), !.
read_single_line(Stream, [CompiledInstruction | OtherInstructions]) :-
  \+ at_end_of_stream(Stream),
  read_string(Stream, "\n", "", _, InstructionRaw),
  validate_instruction(InstructionRaw, Instruction),
  split_string(Instruction, " ", "", SplittedInstruction),
  compile_instruction(SplittedInstruction, CompiledInstruction),
  read_single_line(Stream, OtherInstructions).
/* Pulizia degli eventuali commenti o spazi inutili e verifica della validità */
validate_instruction(Instruction, ValidatedInstruction) :-
  string_upper(Instruction, ValidatedInstruction_part1),
  remove_spaces(ValidatedInstruction_part1, ValidatedInstruction_part2),
  remove_comments(ValidatedInstruction_part2, ValidatedInstruction).
  %check_instruction_syntax(ValidatedInstruction_part3, ValidatedInstruction).
/* Rimozione dei doppi spazi */
remove_spaces(Instruction, ValidatedInstruction) :-
  string_chars(Instruction, InstructionChars),
  remove_spaces_iterations(InstructionChars, ValidatedChars),
  string_chars(ValidatedInstruction, ValidatedChars).
remove_spaces_iterations([], []) :- !.
remove_spaces_iterations([Char], [Char]) :- !.
remove_spaces_iterations([Char, Char | Other], RecOther) :-
  Char = ' ',
  remove_spaces_iterations([Char | Other], RecOther),
  !.
remove_spaces_iterations([Char1, Char2 | Other], [Char1 | RecOther]) :-
  remove_spaces_iterations([Char2 | Other], RecOther).
/* Rimozione dei commenti */
remove_comments(Instruction, ValidatedInstruction) :-
  string_chars(Instruction, InstructionChars),
  remove_comments_iterations(InstructionChars, ValidatedChars),
  string_chars(ValidatedInstruction, ValidatedChars).
remove_comments_iterations([], []) :- !.
remove_comments_iterations([Char], [Char]) :- !.
remove_comments_iterations([Char, Char | _], []) :-
  Char = '/', !.
remove_comments_iterations([Char1, Char2 | Other], [Char1 | RecOther]) :-
  remove_comments_iterations([Char2 | Other], RecOther).
/* Compilazione del codice assembly */
compile_instruction(["ADD", Param], CompiledInstruction) :-
  concat("1", Param, CompiledInstruction), !.
compile_instruction(["SUB", Param], CompiledInstruction) :-
  concat("2", Param, CompiledInstruction), !.
compile_instruction(["STA", Param], CompiledInstruction) :-
  concat("3", Param, CompiledInstruction), !.
compile_instruction(["LDA", Param], CompiledInstruction) :-
  concat("5", Param, CompiledInstruction), !.
compile_instruction(["BRA", Param], CompiledInstruction) :-
  concat("6", Param, CompiledInstruction), !.
compile_instruction(["BRZ", Param], CompiledInstruction) :-
  concat("7", Param, CompiledInstruction), !.
compile_instruction(["BRP", Param], CompiledInstruction) :-
  concat("8", Param, CompiledInstruction), !.
compile_instruction(["INP"], "901") :- !.
compile_instruction(["OUT"], "902") :- !.
compile_instruction(["HLT"], "000") :- !.
compile_instruction(["DAT", Param], Param) :- !.
compile_instruction(["DAT"], "0") :- !.
compile_instruction([Instruction | _], _) :-
  format("~s is not a valid LMC function ~n", [Instruction]), fail, !.
/*
compile_instructions([], []).
compile_instructions([Instruction | OtherInstructions], [CompiledInstruction | CompiledOtherInstructions]) :-
  string_chars(Instruction, CharInstruction),
  compile_instruction(CharInstruction, CompiledInstruction),
  compile_instructions(OtherInstructions, CompiledOtherInstructions).

compile_instruction([], CompiledInstruction) :-
*/
