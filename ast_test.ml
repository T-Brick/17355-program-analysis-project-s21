open Printf

(* Prints the AST for the ocaml code *)
let e = "let x = 100 + 50"
let lexBuf = Lexing.from_string e
let parseTree = Parse.use_file (lexBuf)
let printer = (fun x -> Pprintast.top_phrase (Format.std_formatter) x ; printf "\n")
let _ = List.map printer parseTree

let pat_to_var p =
  match p.ppat_desc with
    | Ppat_var s -> s.txt
    | _ -> raise Failure "too lazy"

let convert_astexpr e = function

let convert_binding b =
  let id = pat_to_var (b.pvb_pat) in
  let exp = convert_astexpr (b.pvb_expr) in
  Assign (id, exp)
  

let convert_struct_item_desc = function
  | Pstr_value (Nonrecursive, [binding]) ->  
  | _ -> raise Failure "todo"

let convert_struct_item i =
  let _ = i.pstr_desc

let convert_phrase = function
  | Ptop_def s -> List.fold ...
  | Ptop_dir _ -> raise Failure "test"
