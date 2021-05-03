Uses the ocaml compiler-libs to parse ocaml expressions into an AST. We then construct a control flow graph from the AST.

Run using `ocamlc -I +compiler-libs ocamlcommon.cma <file>`

For example, `ocamlc -I +compiler-libs ocamlcommon.cma ast_test.ml`
