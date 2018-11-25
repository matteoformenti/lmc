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
  compile_instruction(Instruction, CompiledInstruction)
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

compile_instruction(Instruction, CompiledInstruction) :-


/*
compile_instructions([], []).
compile_instructions([Instruction | OtherInstructions], [CompiledInstruction | CompiledOtherInstructions]) :-
  string_chars(Instruction, CharInstruction),
  compile_instruction(CharInstruction, CompiledInstruction),
  compile_instructions(OtherInstructions, CompiledOtherInstructions).

compile_instruction([], CompiledInstruction) :-
*/
