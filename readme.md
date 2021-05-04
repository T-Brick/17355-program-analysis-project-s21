Uses the ocaml compiler-libs to parse ocaml expressions into an AST. We then convert the ocaml AST into our simplified AST. We then convert our simplified AST into a CFG, which we run constant analysis on.

We use Ocaml version 4.09.0

Run using `ocamlc -I +compiler-libs ocamlcommon.cma <file>`
For using the REPL: `#load "compiler-libs/ocamlcommon.cma";;`

For example, `ocamlc -I +compiler-libs ocamlcommon.cma ast_test.ml`
