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


state(Acc, PC, Mem, In, Out, Flag) =.. List.
one_instruction(state(Acc, PC, Mem, In, Out, Flag), X) =.. Ts.
