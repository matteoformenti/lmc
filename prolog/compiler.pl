%LINE COMPILATION
compile_instruction(Instruction, LineNumber, Compiled) :-
    split_string(Instruction, " \t", " \t", List),
    strings_to_atoms(List, Atoms),
    compile(Atoms, Compiled, LineNumber).
%   Compile base instructions
compile([inp], 901, _) :- !.
compile([out], 902, _) :- !.
compile([hlt], 0, _) :- !.
compile([dat], 0, _) :- !.
%   Fail on other instructions without parameter
compile([Instruction], _, LN) :-
    instruction(Instruction),
    requires_parameter(Instruction), !,
    err(param_required, LN, Instruction),
    fail.
%   Fail DAT when parameter > 999 or < 0
compile([dat, Atom], Number, LN) :-
    atom_number(Atom, Number),
    Number<0,
    Number>999, !,
    err(dat_out_of_bounds, LN, Number).
%   DAT with number parameter
compile([dat, Atom], Number, _) :-
    atom_number(Atom, Number), !,
    Number>=0,
    Number=<999.
%   Fail on DAT with label parameter
compile([dat, _], _, LN) :-
    err(dat_param_lbl, LN), !,
    fail.
%   Instruction with number parameter
compile([Instruction, Atom], Compiled, LN) :-
    instruction(Instruction),
    requires_parameter(Instruction),
    atom_number(Atom, Number), !,
    number(Number), !,
    valid_parameter(Number, NN, LN),
    code(Instruction, Code),
    atom_concat(Code, NN, Compiled).
%   Instruction with label parameter
compile([Instruction, Label], [Code, Label], LN) :-
    instruction(Instruction),
    requires_parameter(Instruction), !,
    valid_label(Label, LN),
    code(Instruction, Code).
% Unifies when the first element is a label already defined
compile([Label|_], _, LN) :-
    define_label(Label, _), !,
    err(lbl_already_defined, LN, Label),
    fail.
% Unifies when the first element is a new label
compile([Label|Rest], Compiled, LN) :-
    asserta(define_label(Label, LN)),
    compile(Rest, Compiled, LN).