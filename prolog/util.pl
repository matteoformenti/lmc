set_cell(Mem, Index, Value, NewMemory) :-
    nth0(Index, Mem, _, Rem),
    nth0(Index, NewMemory, Value, Rem).

%   Wraper for the nth0 function
get_cell(Mem, Index, ValueN) :-
    nth0(Index, Mem, Value),
    atom(Value),
    atom_number(Value, ValueN).

get_cell(Mem, Index, Value) :-
    nth0(Index, Mem, Value),
    number(Value).