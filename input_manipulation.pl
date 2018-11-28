/* Input preparation, checks for comments and atomizes the string */
sanitize(In, Out) :-
  string_lower(In, Lower),
  remove_comments(Lower, Out).

/* Comments removal */
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
