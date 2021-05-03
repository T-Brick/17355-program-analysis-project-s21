Uses the ocaml compiler-libs to parse ocaml expressions into an AST. We then construct a control flow graph from the AST.

We use Ocaml version 4.09.0

Run using `ocamlc -I +compiler-libs ocamlcommon.cma <file>`
For using the REPL: `#load "compiler-libs/ocamlcommon.cma";;`

For example, `ocamlc -I +compiler-libs ocamlcommon.cma ast_test.ml`
