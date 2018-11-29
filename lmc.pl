:- consult(input_manipulation).
:- consult(instructions).
:- consult(compiler).

/* Assmbly compilation and loading */
lmc_load(Filename, Mem) :-
  open(Filename, read, Stream),
  read_string(Stream, _, File),
  close(Stream),
  split_string(File, "\n", "\n", SplitMem),
  parse_lines(SplitMem, UnresolvedMem, 0),
  resolve_labels(UnresolvedMem, Mem).

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
/* fail if label is not defined */
resolve_label([_, Label], _) :-
  \+ define_label(Label, _),
  format("Label ~w is not defined~n", [Label]), fail.
/* resolve lonley labels */
resolve_label([Label], ResolvedLabel) :-
  define_label(Label, ResolvedLabel), !.
