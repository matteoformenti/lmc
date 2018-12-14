<h1>Little Man Computer</h1>
<h2>Formenti Matteo</h2>

<span>
  Implementation of "little man computer" by Matteo Formenti in prolog and common lisp.
</span>

<hr>
<h3>Notes</h3>
<ul>
  <li>In the prolog Implementation, the ansi_term.pl is needed to format the output</li>
  <li>A label can be ANYTHING that isn't a number (1label IS a label, 1 is NOT)</li>
  <li>A label CAN'T be defined with the same name as any of the instructions</li>
  <li>Labels are case-INsensitive</li>
</ul>

<h3>File description</h3>
<ul>
  <li>
    <h4>PROLOG</h4>
    <ul>
      <li><b>lmc.pl</b> Entry point for the program, contains the methods that are defined
        in the PDF</li>
      <li><b>io.pl</b> Error and debug output</li>
      <li><b>input_manipulation.pl</b> Sanitizes the assembly input, comment removal and 
        label resolution</li>
      <li><b>instructions.pl</b> Here are defined the instructions, the instruction codes 
        and instruction rules</li>
      <li><b>compile.pl</b> The compilation logic</li>
      <li><b>util.pl</b> Some handy wrappers</li>
      <li><b>emulator.pl</b> Instruction emulation logic</li>
    </ul>
  </li>
</ul>