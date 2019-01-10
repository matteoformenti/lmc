# Little Man Computer

*Matteo formenti 830594*
Implementation of "little man computer" by Matteo Formenti in prolog and common lisp.

## Notes

- In the prolog Implementation, the ansi_term.pl is needed to format the output
- A label can be ANYTHING that isn't a number (1label IS a label, 1 is NOT)
- A label CAN'T be defined with the same name as any of the instructions
- Labels are case-INsensitive

## File structure

### Prolog

- **lmc** Entry point for the program, contains the methods that are defined in the PDF
- **io** Error and debug output
- **input_manipulation** Sanitizes the assembly input, comment removal and label resolution
- **instructions** Instruction definitions, instruction codes and instruction rules
- **compile** Compilation logic
- **util** Some handy wrappers
- **emulator** Instruction emulation logic

### Lisp

- **lmc** The whole LMC implementation is written in this single file

## Version

- **Prolog** SWI-Prolog 7.6.4 on Ubuntu 18.10
- **Lisp** Clisp 2.49.92 (2018-02-18) on Ubuntu 18.10