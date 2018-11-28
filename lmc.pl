:- consult(input_manipulation).
:- consult(instructions).
:- consult(compiler).

/* Assmbly compilation and loading */
lmc_load(Filename, Mem) :-
  open(Filename, read, Stream),
  read_single_line(Stream, Mem, 0),
  close(Stream),
  listing(define_label),
  listing(needs_label).

/*  Line-By-line file reading and decoding */
read_single_line(Stream, [], _) :-
  at_end_of_stream(Stream), !.
read_single_line(Stream, [Compiled | OtherInstructions], Line) :-
  \+ at_end_of_stream(Stream),
  read_string(Stream, "\n", "", _, Row),
  parse_line(Row, Compiled, Line, NextLine),
  read_single_line(Stream, OtherInstructions, NextLine), !.

parse_line(Row, Compiled, Line, NextLine) :-
  sanitize(Row, Sanitized),
  string_length(Sanitized, StringLength),
  StringLength > 0,
  compile_instruction(Sanitized, Line, Compiled),
  NextLine is Line+1.

parse_line(Row, [], Line, NextLine) :-
  sanitize(Row, Sanitized),
  string_length(Sanitized, StringLength),
  StringLength = 0,
  NextLine = Line.
