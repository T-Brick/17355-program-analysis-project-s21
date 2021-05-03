open Printf

(* Prints the AST for the ocaml code *)
let e = "let x = 100 + 50"
let lexBuf = Lexing.from_string e
let parseTree = Parse.use_file (lexBuf)
let printer = (fun x -> Pprintast.toplevel_phrase (Format.std_formatter) x ; printf "\n")
let _ = List.map printer parseTree
