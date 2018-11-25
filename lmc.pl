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
compile_instruction(["ADD", Param | _], CompiledInstruction) :-
  concat("1", Param, CompiledInstruction), !.
compile_instruction(["SUB", Param | _], CompiledInstruction) :-
  concat("2", Param, CompiledInstruction), !.
compile_instruction(["STA", Param | _], CompiledInstruction) :-
  concat("3", Param, CompiledInstruction), !.
compile_instruction(["LDA", Param | _], CompiledInstruction) :-
  concat("5", Param, CompiledInstruction), !.
compile_instruction(["BRA", Param | _], CompiledInstruction) :-
  concat("6", Param, CompiledInstruction), !.
compile_instruction(["BRZ", Param | _], CompiledInstruction) :-
  concat("7", Param, CompiledInstruction), !.
compile_instruction(["BRP", Param | _], CompiledInstruction) :-
  concat("8", Param, CompiledInstruction), !.
compile_instruction(["INP" | _], "901") :- !.
compile_instruction(["OUT" | _], "902") :- !.
compile_instruction(["HLT" | _], "000") :- !.
compile_instruction(["DAT" | _, Param], Param) :- !.
compile_instruction(["DAT" | _], "0") :- !.
compile_instruction([Instruction | _], _) :-
  format("~s is not a valid LMC function ~n", [Instruction]), fail, !.
