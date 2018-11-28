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
  \+ member(Instruction, [hlt, inp, out]).

is_valid(Instruction, Param) :-
  is_instruction(Instruction),
  \+ is_instruction(Param),
  \+ member(Instruction, [hlt, inp, out]).

is_valid(Label, Instruction) :-
  is_instruction(Instruction),
  \+ is_instruction(Label),
  member(Instruction, [hlt, dat, inp, out]).

is_valid(Instruction) :-
  is_instruction(Instruction),
  member(Instruction, [hlt, dat, inp, out]).

instruction_code(add, 1).
instruction_code(sub, 2).
instruction_code(sta, 3).
instruction_code(lda, 5).
instruction_code(bra, 6).
instruction_code(brz, 7).
instruction_code(brp, 8).
instruction_code(inp, 901).
instruction_code(out, 902).
instruction_code(hlt, 000).
