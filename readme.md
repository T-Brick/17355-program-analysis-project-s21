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

## Language Subset
Currently, we only handle OCaml programs that take the form of multiple
`let` declarations in a row, which bind singular variables. As for expressions, we only handle constants, integer variables, basic integer operations (addition, subtraction, multiplication, division), lambda values that take in a singular variable argument, and application of such lambda values.
