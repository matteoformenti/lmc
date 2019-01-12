%   Matteo Formenti 830594
%   Unify if word is an instruction
instruction(Word) :-
    member(Word, [add, sub, sta, lda, bra, brz, brp, inp, out, dat, hlt]).
%LABEL CHECKS
%   Fail if Label is an instruction
valid_label(Label, LN) :-
    instruction(Label),
    err(reserved_label, LN),
    fail.
%   Fail if Label is a number
valid_label(Label, LN) :-
    number(Label),
    err(numeric_label, LN),
    fail.
%   Label is valid
valid_label(_, _).
%PARAMETER CHECKS
%   Parameter is a number between 10 and 99
valid_parameter(Parameter, Atom, _) :-
    number(Parameter),
    Parameter>=10,
    Parameter<100, !,
    atom_number(Atom, Parameter).    
%   Parameter is a number between 0 and 10
valid_parameter(Parameter, NP, _) :-
    number(Parameter),
    Parameter<10,
    Parameter>=0, !,
    atom_concat(0, Parameter, NP).
%   Fail if parameter is greater than 99
valid_parameter(Parameter, _, LN) :-
    number(Parameter),
    Parameter>100, !,
    err(param_overflow, LN),
    fail.
%   Fail if parameter is less than 0
valid_parameter(Parameter, _, LN) :-
    number(Parameter),
    Parameter<0, !,
    err(param_underflow, LN),
    fail.
%   Fail otherwise
valid_parameter(P, _, LN) :-
    err(invalid_param, LN, P),
    fail.
%   True if instruction requires a parameter
requires_parameter(Instruction) :-
    member(Instruction, [add, sub, sta, lda, bra, brz, brp, dat]).

code(add, 1) :- !.
code(sub, 2) :- !.
code(sta, 3) :- !.
code(lda, 5) :- !.
code(bra, 6) :- !.
code(brz, 7) :- !.
code(brp, 8) :- !.