open Printf

(* prints the AST for the ocaml code *)
let e = "let x = 100 + 50\nlet y = x + x\nlet t = fun a -> a + y\nlet z = t x"
let lexBuf = Lexing.from_string e
let parseTree = Parse.use_file (lexBuf)
let printer = (fun x -> Printast.top_phrase (Format.std_formatter) x ; printf "\n")
let _ = List.map printer parseTree