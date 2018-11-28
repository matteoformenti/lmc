/* Compiles a string  */
compile_instruction(Instruction, LineNumber, Compiled) :-
  split_string(Instruction, " ", " ", List),
  compile(List, Compiled, LineNumber).
/* label instruction index */
compile([Label_S, Instruction_S, Parameter_S], CompiledInstruction, LineNumber) :-
  atom_string(Label, Label_S),
  atom_string(Instruction, Instruction_S),
  atom_number(Parameter_S, Parameter),
  is_valid(Label, Instruction, Parameter),
  integer(Parameter),
  assert(define_label(Label, LineNumber)),
  instruction_code(Instruction, Code),
  atomic_list_concat([Code, Parameter], CompiledInstruction),
  format("~d_1: [label ~w] ~w ~w ~t ~w ~n", [LineNumber, Label, Instruction, Parameter, CompiledInstruction]), !.
/* label instruction label */
compile([Label_S, Instruction_S, Parameter_S], [Code, Parameter], LineNumber) :-
  atom_string(Label, Label_S),
  atom_string(Instruction, Instruction_S),
  atom_string(Parameter, Parameter_S),
  is_valid(Label, Instruction, Parameter),
  instruction_code(Instruction, Code),
  assert(define_label(Label, LineNumber)),
  format("~d_2: [label ~w] ~w ~w ~t [~w ~w]~n", [LineNumber, Label, Instruction, Parameter, Code, Parameter]), !.
/* label instruction */
compile([Label_S, Instruction_S], Code, LineNumber) :-
  atom_string(Label, Label_S),
  atom_string(Instruction, Instruction_S),
  is_valid(Label, Instruction),
  instruction_code(Instruction, Code),
  assert(define_label(Label, LineNumber)),
  format("~d_3: [label ~w] ~w ~t ~w ~n", [LineNumber, Label, Instruction, Code]), !.

  /* label hlt */
/* label dat */
compile([Label_S, Instruction_S], 000, LineNumber) :-
  atom_string(Label, Label_S),
  atom_string(Instruction, Instruction_S),
  is_valid(Label, Instruction),
  Instruction = 'dat',
  assert(define_label(Label, LineNumber)),
  format("~d_4: [label ~w] ~w ~t ~w ~n", [LineNumber, Label, Instruction, 000]), !.
/* instruction index */
compile([Instruction_S, Parameter_S], CompiledInstruction, LineNumber) :-
  atom_string(Instruction, Instruction_S),
  number_string(Parameter, Parameter_S),
  integer(Parameter),
  is_valid(Instruction, Parameter),
  instruction_code(Instruction, Code),
  atomic_list_concat([Code, Parameter], CompiledInstruction),
  format("~d_5: ~w ~w ~t ~w ~n", [LineNumber, Instruction, Parameter, CompiledInstruction]), !.
/* instruction label */
compile([Instruction_S, Parameter_S], [Code, Parameter], LineNumber) :-
  atom_string(Instruction, Instruction_S),
  atom_string(Parameter, Parameter_S),
  is_valid(Instruction, Parameter),
  instruction_code(Instruction, Code),
  format("~d_6: ~w ~w ~t [~w ~w] ~n", [LineNumber, Instruction, Parameter, Code, Parameter]), !.
/* dat */
compile([Instruction], 000, LineNumber) :-
  atom_string(Label, Label_S),
  atom_string(Instruction, Instruction_S),
  is_valid(Instruction),
  Instruction = 'dat',
  assert(define_label(Label, LineNumber)),
  format("~d_4: ~w ~t ~w ~n", [LineNumber, Instruction, 000]), !.
/* instruction */
compile([Instruction], Code, LineNumber) :-
  atom_string(Label, Label_S),
  atom_string(Instruction, Instruction_S),
  is_valid(Instruction),
  instruction_code(Instruction, Code),
  assert(define_label(Label, LineNumber)),
  format("~d_4: ~w ~t ~w ~n", [LineNumber, Instruction, Code]), !.
/* catch all */
compile(Input, _, LineNumber) :-
  format("[COMPILATION_ERROR] line ~d [~w]~n", [LineNumber, Input]), fail.
