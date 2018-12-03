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

/* Parse all lines and remove empty ones */
parse_lines([], [], _).
parse_lines([Row | NextRows], OtherInstructions, LineNumber) :-
  sanitize(Row, Sanitized),
  string_length(Sanitized, Length),
  Length = 0,
  NextLine = LineNumber,
  parse_lines(NextRows, OtherInstructions, NextLine), !.
parse_lines([Row | NextRows], [CompiledRow | OtherInstructions], LineNumber) :-
  sanitize(Row, Sanitized),
  compile_instruction(Sanitized, LineNumber, CompiledRow),
  NextLine is LineNumber+1,
  parse_lines(NextRows, OtherInstructions, NextLine), !.

/* Resolve labels */
resolve_labels([], []).
resolve_labels([Instruction | Memory], [ResolvedInstruction | UnifiedMemory]) :-
  resolve_label(Instruction, ResolvedInstruction),
  resolve_labels(Memory, UnifiedMemory).
/* do nothing in unlabeled instructions */
resolve_label(Instruction, Instruction) :-
  \+ is_list(Instruction), !.
/* resolve standard labels */
resolve_label([Instruction, Label], ResolvedInstruction) :-
  define_label(Label, ResolvedLabel),
  atom_concat(Instruction, ResolvedLabel, ResolvedInstruction), !.
/* fail if label is not defined*/
resolve_label([_, Label], _) :-
  \+ define_label(Label, _),
  ansi_format(fg(red), "Label ~w is not defined~n", [Label]), fail.
/* resolve lonley labels */
resolve_label([Label], ResolvedLabel) :-
  define_label(Label, ResolvedLabel), !.
/* fail if label is lonley not defined*/
resolve_label([Label], _) :-
  \+ define_label(Label, _),
  ansi_format(fg(red), "Label ~w is not defined~n", [Label]), fail.
