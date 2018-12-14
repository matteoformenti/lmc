:- (dynamic get_cell/3).
:- (dynamic set_cell/4).
:- (dynamic err/2).

%   Execute SUM
execute(1, Cell, State, NewState) :-
    State=..[state, Acc, Pc, Mem, In, Out, Flag],
    get_cell(Mem, Cell, Value),
    NewAcc is Acc+Value,
    NewPc is Pc+1,
    check_flag(Acc, Flag, NewFlag),
    NewState=[state, NewAcc, NewPc, Mem, In, Out, NewFlag], !.

%   Execute SUB
execute(2, Cell, State, NewState) :-
    State=..[state, Acc, Pc, Mem, In, Out, Flag],
    get_cell(Mem, Cell, Value),
    NewAcc is Acc-Value,
    NewPc is Pc+1,
    check_flag(Acc, Flag, NewFlag),
    NewState=[state, NewAcc, NewPc, Mem, In, Out, NewFlag], !.

%   Execute STORE
execute(3, Cell, State, NewState) :-
    State=..[state, Acc, Pc, Mem, In, Out, Flag],
    set_cell(Mem, Cell, Acc, NewMem),
    NewPc is Pc+1,
    NewState=[state, Acc, NewPc, NewMem, In, Out, Flag], !.

%   Execute LOAD
execute(5, Cell, State, NewState) :-
    State=..[state, _, Pc, Mem, In, Out, Flag],
    get_cell(Mem, Cell, NewAcc),
    NewPc is Pc+1,
    NewState=[state, NewAcc, NewPc, Mem, In, Out, Flag], !.

%   Execute BRANCH
execute(6, Value, State, NewState) :-
    State=..[state, Acc, _, Mem, In, Out, Flag],
    NewState=[state, Acc, Value, Mem, In, Out, Flag], !.

%   Execute BRANCH-IF-ZERO
execute(7, BranchPc, State, NewState) :-
    State=..[state, Acc, _, Mem, In, Out, noflag],
    Acc=0, !,
    NewState=[state, Acc, BranchPc, Mem, In, Out, noflag].
execute(7, _, State, NewState) :-
    State=..[state, Acc, Pc, Mem, In, Out, flag],
    NewPc is Pc+1,
    NewState=[state, Acc, NewPc, Mem, In, Out, flag], !.

%   Execute BRANCH-IF-POSITIVE
execute(8, BranchPc, State, NewState) :-
    State=..[state, Acc, _, Mem, In, Out, noflag], !,
    NewState=[state, Acc, BranchPc, Mem, In, Out, noflag].
execute(8, _, State, NewState) :-
    State=..[state, Acc, Pc, Mem, In, Out, flag],
    NewPc is Pc+1,
    NewState=[state, Acc, NewPc, Mem, In, Out, flag], !.

%   Execute INPUT
execute(9, 1, State, NewState) :-
    State=..[state, _, Pc, Mem, [HIn, TIn], Out, Flag],
    NewPc is Pc+1,
    NewState=[state, HIn, NewPc, Mem, TIn, Out, Flag], !.

%   Execute OUTPUT
execute(9, 2, State, NewState) :-
    State=..[state, Acc, Pc, Mem, In, Out, Flag],
    NewPc is Pc+1,
    NewState=[state, Acc, NewPc, Mem, In, [Out|Acc], Flag], !.

%   Execute HALT
execute(0, _, State, NewState) :-
    State=..[state, Acc, Pc, Mem, In, Out, Flag],
    NewState=..[halted_state, Acc, Pc, Mem, In, Out, Flag], !.

%   ERROR
execute(OpCode, _, State, NewState) :-
    State=..[state, Acc, Pc, Mem, In, Out, Flag],
    NewState=..[halted_state, Acc, Pc, Mem, In, Out, Flag],
    err(invalid_instruction, OpCode), !,
    fail.

%   Controls wheter the flag should be present or not
check_flag(_, flag, flag) :- !.
check_flag(Acc, _, flag) :-
    Acc>999.
check_flag(Acc, _, flag) :-
    Acc<0.
check_flag(Acc, Flag, Flag) :-
    Acc>=0,
    Acc=<999.

