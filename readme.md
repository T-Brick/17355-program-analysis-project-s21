# OCaml Constant Propagation Analysis

Uses the OCaml compiler-libs to parse simple OCaml programs into an AST. We then convert the AST into our simplified AST. We then convert our simplified AST into a CFG, which we run constant propagation analysis on.

We use OCaml version 4.09.0

Run using `ocamlc -I +compiler-libs ocamlcommon.cma <file>`
For using the REPL: `#load "compiler-libs/ocamlcommon.cma";;`

For example, `ocamlc -I +compiler-libs ocamlcommon.cma ast_test.ml`

## File Structure
- `ast_conversion.ml`: takes in OCaml code and converts it to our simplified language AST, then to a `listing`, then to a CFG to run analysis on
- `lang-def.ml`: defines our simplified AST (adapted from `hw3`)
- `cfg.ml`: defines functions related to creation and use of the CFG (adapted from `hw3`)
- `ast_test.ml`: used for printing out OCaml ASTs for testing
- `df.ml` : defines the domain, state, and the actual flow (partially adapted from `hw3`)
- `launch.ml` : used to load in everything needed for the analysis
- `tests/*.ml` : sample Ocaml code to run the analysis on

## Useage

To use the analysis simply open the REPL and run the following directives at the start (make sure that Core is properly installed and available first):
```
#load "compiler-libs/ocamlcommon.cma";;
#use "lang_def.ml";;
#use "cfg.ml";;
#use "df.ml";;
#use "ast_conversion.ml";;
```
Alternatively you can simply enter the REPL and type
```
#use "launch.ml";;
```
which will load in all the appropriate files.

You can then simply apply `run "filepath"` to evaluate a specific file. This adds a dummy binding in the last state, which is used to print out the last state of the flow. Doing this will print out the state of the program after each associated line in the input file.

## Language Subset
Currently, we only handle OCaml programs that take the form of multiple
`let` declarations in a row, which bind singular variables. As for expressions, we only handle constants, integer variables, basic integer operations (addition, subtraction, multiplication, division), lambda values that take in a singular variable argument, application of such lambda values, and some basic boolean operations (not, equals, greater than, less than, if then else).

Directives are not supported by the language, and attempting to analysis a file with directives will raise an `Unsupported` exception. Using definitions that are not supported will simply be skipped, and the state will be assumed to remain the same. Using expressions that are not supported will be assumed to be mapped to top.

The simplified AST can be found in `lang_def.ml`.
