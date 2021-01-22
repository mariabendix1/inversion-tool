# Inversion tool
A tool implementation of Kirkeby and Glück's local inversion framework [1].

## How to use
The tool is available as a web-version (recommended) and as the source program.

### Web-version
The easiest way of using the Inversion Tool is to go to the web-version: [Not yet available](https://topps.diku.dk/pirc).

Here, you can play around with different CCS examples and the four different rule inverters. Or you can write your own CCS program and invert it! Click "About" for detailed information on how to use the tool.

### Build and run source program
You can also compile and run the program manually:
- Clone repository.
- Make sure you have `stack` installed (run `curl -sSL https://get.haskellstack.org/ | sh` on a Unix-like operating system).
- Run `stack build`. Stack will take care of compilation and packages/libraries.
- The program is now ready!

Now it gets a little complicated. Navigate to the `src` directory. From here, you can run the `Main` program, which has many functionalities. The input arguments determine what action `Main` should do (inversion, generate diagnostics, and so on). Here are some use cases: 

* You have a file `fun.ccs` with CCS rules for a function `fun` with arity 2 and co-arity 2 (written in COCO-format or the format in [1]), which you want to invert with respect to I = {1} and O = {1,2}. Run:

        $ ./Main inv path/to/fun.ccs fun "1" "1 2"
  The syntax being `./Main inv {file name} {rule to invert} {I-set} {O-set}`. This will generate four files, `funtriv.out`, `funfull.out`, `funpart.out`, and `funsemi.out`. These are four inverted programs using the trivial, pure full, partial and semi rule inverters, respectively.
* You want get the detailed diagnostics of the inverted program generated with the partial rule inverter. Run:

        $ ./Main diagnostics full funpart.out
  This prints a diagnostics table to the terminal.
* You want to compare the original program's diagnostics to those of the inverted program generated with the partial rule inverter. Run:

        $ ./Main diagnostics compare path/to/fun.ccs funpart.out
  This prints a less detailed diagnostics table to the terminal, with two columns, one for each program.
* You want to generate LaTeX code for you new inversion. There are two options, `flat` and `noflat`. The `noflat` version is better for larger programs, where the rules might otherwise go over the right margin in your document. Run:

        $./Main latex mulpart.out mulpart.tex flat
  This generates a file with the name of the third argument `mulpart.tex`, which you can copy to your LaTeX project.
  
[1] M. H. Kirkeby and R. Glück, “Inversion framework: Reasoning about inversion  by  conditional  term  rewriting  systems". PPDP  
