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
