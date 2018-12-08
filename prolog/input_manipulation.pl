sanitize(In, L) :-
    string_lower(In, Lower),
    remove_comments(Lower, Out),
    split_string(Out, "", " \t\n", [L]).
%COMMENTS REMOVAL
remove_comments(Instruction, Validated) :-
    string_chars(Instruction, Chars),
    remove_comments_iterations(Chars, ValidatedChars),
    string_chars(Validated, ValidatedChars).
%   Base case with empty list
remove_comments_iterations([], []) :- !.
%   Base case with single character
remove_comments_iterations([Char], [Char]) :- !.
%   Unifies with two forward slash followed by anything
remove_comments_iterations([Char, Char|_], []) :-
    Char=(/), !.
%   Unifies with non-comment lines and recursion
remove_comments_iterations([Char1, Char2|Other], [Char1|RecOther]) :-
    remove_comments_iterations([Char2|Other], RecOther).
%EMPTY LINES REMOVAL
%   Base case
parse_lines([], [], _).
%   Unifies with empty lines
parse_lines([Row|NextRows], Next, LineNumber) :-
    sanitize(Row, Sanitized),
    string_length(Sanitized, Length),
    Length=0, !,
    NextLine=LineNumber,
    parse_lines(NextRows, Next, NextLine).
%   Unifies with non-empty lines
parse_lines([Row|NextRows], [CompiledRow|Next], LineNumber) :-
    sanitize(Row, Sanitized),
    compile_instruction(Sanitized, LineNumber, CompiledRow),
    NextLine is LineNumber+1,
    parse_lines(NextRows, Next, NextLine), !.
%LABEL RESOLUTION 
%   Base case
resolve_labels([], [], _).
%   This unifies with all the instructions in the non-yet-unified memory
resolve_labels([Instruction|Memory], [ResolvedInstruction|UnifiedMemory], LN) :-
    resolve_label(Instruction, ResolvedInstruction, LN),
    NextLine is LN+1,
    resolve_labels(Memory, UnifiedMemory, NextLine).
%   This only unifies with a list containing instruction and a label
resolve_label([Instruction, Label], ResolvedInstruction, LN) :-
    define_label(Label, ResolvedLabel), !,
    valid_parameter(ResolvedLabel, X, LN),
    atom_concat(Instruction, X, ResolvedInstruction).
%   Fail if label is not defined
resolve_label([_, Label], _, LN) :-
    err(lbl_not_defined, Label, LN), !,
    fail.
%   This unifies with already correct instructions and ignores them
resolve_label(Instruction, Atom, _) :-
    atom_concat('', Instruction, Atom), !.
%   List of string to list of atoms
string_to_atoms([], []).
strings_to_atoms([String], [Atom]) :-
    atom_string(Atom, String).
strings_to_atoms([String|OtherS], [Atom|OtherA]) :-
    atom_string(Atom, String),
    strings_to_atoms(OtherS, OtherA).

