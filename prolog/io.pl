err(lbl_not_defined, Label, LN) :-
    ansi_format([fg(red), bold],
                "Label ~w is not defined on line ~w~n",
                [Label, LN]).
err(param_not_allowed, LN, Instruction) :-
    ansi_format([fg(red), bold],
                "Instruction ~w does not accept parameters on line ~w~n",
                [Instruction, LN]).
err(lbl_already_defined, LN, Label) :-
    ansi_format([fg(red), bold],
                "Label ~w is already defined line ~w~n",
                [Label, LN]).
err(dat_out_of_bounds, LN, Param) :-
    ansi_format([fg(red), bold],
                "Parameter ~w for DAT on line ~w is outside the range 0 ~ 999 ~n",
                [Param, LN]).
err(param_required, LN, Instruction) :-
    ansi_format([fg(red), bold],
                "Instruction ~w requires a parameter on line ~w~n",
                [Instruction, LN]).
err(invalid_param, LN, P) :-
    ansi_format([fg(red), bold],
                "Parameter on line ~w isn't valid (~w)~n",
                [LN, P]).
err(param_overflow, LN) :-
    ansi_format([fg(red), bold],
                "Parameter on line ~w exceeds maximum value~n",
                [LN]).
err(param_underflow, LN) :-
    ansi_format([fg(red), bold],
                "Parameter on line ~w exceeds minimum value~n",
                [LN]).
err(dat_param_lbl, LN) :-
    ansi_format([fg(red), bold],
                "Instruction DAT does not accept labels as parameters on line ~w~n",
                [LN]).

err(invalid_instruction, OpCode) :-
    ansi_format([fg(red), bold],
                "OpCode ~w is not a valid instruction~n",
                [OpCode]).
err(empty_input) :-
    ansi_format([fg(red), bold], "Input queue is empty~n", []).
