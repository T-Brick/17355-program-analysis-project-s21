open Core
open Printf

(* prints the AST for the ocaml code *)
let filename = "tests/lets.ml"
let s = (In_channel.read_all filename) ^ "\n" ^ "let dummy = -1"
let lexBuf = Lexing.from_string s
let parseTree = Parse.use_file (lexBuf)
let printer = (fun x -> Printast.top_phrase (Format.std_formatter) x ; printf "\n")
let _ = List.map parseTree printer
