:- consult(io).
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
    resolve_labels(UnresolvedMem, Mem, 0),
    ansi_format(fg(cyan), "Input ~w compiled correctly~n", [Filename]).