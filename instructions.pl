is_instruction(add).
is_instruction(sub).
is_instruction(sta).
is_instruction(lda).
is_instruction(bra).
is_instruction(brz).
is_instruction(brp).
is_instruction(inp).
is_instruction(out).
is_instruction(dat).
is_instruction(hlt).

is_valid(Label, Instruction, Param) :-
  is_instruction(Instruction),
  \+ is_instruction(Param),
  \+ is_instruction(Label),
  \+ member(Instruction, [hlt]).

is_valid(Instruction, Param) :-
  is_instruction(Instruction),
  \+ is_instruction(Param),
  \+ member(Instruction, [hlt]).

is_valid(Label, Instruction) :-
  is_instruction(Instruction),
  \+ is_instruction(Label),
  member(Instruction, [hlt, dat]).

is_valid(Instruction) :-
  is_instruction(Instruction),
  member(Instruction, [hlt, dat]).
