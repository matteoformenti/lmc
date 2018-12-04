%LINE COMPILATION
compile_instruction(Instruction, LineNumber, Compiled) :-
    split_string(Instruction, " ", " ", List),
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
%   DAT with number parameter
compile([dat, Number], Number, LN) :- !.
%   Instruction with number parameter
compile([Instruction, Number], Compiled, LN) :-
    instruction(Instruction),
    requires_parameter(Instruction),
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

% Unifies when the first element is a label
compile([Label|Rest], Compiled, LN) :-
    asserta(define_label(Label, LN)),
    compile(Rest, Compiled, LN).