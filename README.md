# Inversion tool
A tool implementation of Kirkeby and Glück's local inversion framework [1].

## How to use
The tool is available as a web-version (recommended) and as the source program.

### Web-version
The easiest way of using the Inversion Tool is to go to the web-version: [Not yet available](https://topps.diku.dk/pirc).

Here, you can play around with different CCS examples and the four different rule inverters. Or you can write your own CCS program and invert it! Click "About" for detailed information on how to use the tool.

The code is based on Claus Skou Nielsen and Michael Budde's Janus Playground, which can be found here: [Janus Playground](https://topps.diku.dk/pirc/janus-playground/).

### Build and run source program
You can also compile and run the program manually:
* Clone repository and go to `src` directory.
* If you have `stack` installed (run `curl -sSL https://get.haskellstack.org/ | sh` on a Unix-like operating system), run: 

        $ stack build
        $ stack ghc Main
  Stack will take care of compilation and packages/libraries.
  
  Or if you already have Haskell installed, simply run:
       
        $ ghc Main
* The program is now ready!

Now it gets a little complicated. Make sure you are in the `src` directory. From here, you can run the `Main` program, which has many functionalities. The input arguments to `Main` determine what the tool should do (inversion, generate diagnostics, and so on). Here are some use cases: 

* Assuming that there is a file `fun.ccs` with CCS rules in the `CCSs` library defining a function symbol `addsub` with arity 2 and co-arity 2 (written in COCO-format or the format in [1]), which you want to invert with respect to I = {1,2} and O = {1}. Run following command for this example:
 
        $ ./Main inv ../CCSs/fun.ccs addsub "1 2" "1"
The syntax of the command is `./Main inv {relative file name} {rule to invert} {I-set} {O-set}`.  The example will generate or overwrite the four files, `results/addsubtriv.out`, ` results/addsubfull.out`, ` results/addsubpart.out`, and `results/addsubsemi.out`. These are four inverted programs using the trivial, pure full, partial and semi rule inverters, respectively.
 
* To obtain a detailed properties diagnostics of a CCS file use `./Main diagnostics full {relative file name}`, e.g., a detailed properties diagnostics of the above generated inversion of the program with the semi rule inverter, can be obtained with the following command:
 
        $ ./Main diagnostics full results/addsubsemi.out
  This prints a diagnostics table to the terminal.
 
* To compare properties of two files, e.g., the original program `../CCSs/fun.ccs` and those of the inverted program using the semi-inverter `results/addsubsemi.out`. Run:
 
        $ ./Main diagnostics compare ../CCSs/fun.ccs results/addsubsemi.out
  This prints a less detailed diagnostics table to the terminal, with two columns, one for each program.
 
* To generate LaTeX code for your inversion. There are two options, `flat` and `noflat`. The `noflat` version is intended for larger programs, where the rules might otherwise spill over the right margin in your LaTeX document. Run:
 
        $ ./Main latex results/addsubsemi.out addsubsemi flat
  This generates/overwrites a file with the relative filename `tex/addsubsemi.tex` generated from the third argument `addsubsemi`. You can use the new file or its content directly in your LaTeX project.
 
[1] M. H. Kirkeby and R. Glück, “Inversion framework: Reasoning about inversion  by  conditional  term  rewriting  systems". PPDP  
