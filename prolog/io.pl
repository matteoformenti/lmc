err(lbl_not_defined, Label, LN) :-
    ansi_format([fg(red), bold],
                "Label ~w is not defined on line ~w~n",
                [Label, LN]).
err(param_not_allowed, LN, Instruction) :-
    ansi_format([fg(red), bold],
                "Instruction ~w does not accept parameters on line ~w~n",
                [Instruction, LN]).
err(param_required, LN, Instruction) :-
    ansi_format([fg(red), bold],
                "Instruction ~w requires a parameter on line ~w~n",
                [Instruction, LN]).
err(invalid_param, LN, P) :-
    ansi_format([fg(red), bold],
                "Parameter on line ~w isn't valid (~w)",
                [LN, P]).
err(param_overflow, LN) :-
    ansi_format([fg(red), bold],
                "Parameter on line ~w exceeds maximum value~n",
                [LN]).
err(param_underflow, LN) :-
    ansi_format([fg(red), bold],
                "Parameter on line ~w exceeds minimum value~n",
                [LN]).
