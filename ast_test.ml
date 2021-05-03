
(* Prints the AST for the expression e *)
let e = "let x = 100 + 50 in x"
let lexBuf = Lexing.from_string e
let parseTree = Parse.expression (lexBuf)
let () = Pprintast.expression (Format.std_formatter) parseTree
