open Printf

(* prints the AST for the ocaml code *)
let e = "let x = 100 + 50"
let lexBuf = Lexing.from_string e
let parseTree = Parse.use_file (lexBuf)
let printer = (fun x -> Pprintast.top_phrase (Format.std_formatter) x ; printf "\n")
let _ = List.map printer parseTree

(* convert pattern to variable *)
let pat_to_var p =
  match p.ppat_desc with
    | Ppat_var s -> s.txt
    | _ -> raise Failure "not handled"

(* convert AST expression to lang expression *)
let convert_astexpr e =
  match e.pexp_desc with
    | Pexp_ident c ->
      match c.txt with
        | Lident v -> Var v
        | _ -> raise Failure "not handled"
    | Pexp_constant (Pconst_integer (ns, _)) -> Const (int_of_string ns)
    | Pexp_apply (e, args) ->
      match (e.pexp_desc, args) with
        | (Pexp_ident f, args) ->
          match (f.txt, args) with
            | (Lident "+", [(_, e1), (_, e2)]) -> Add (convert_astexpr e1, convert_astexpr e2)
            | (Lident "-", [(_, e1), (_, e2)]) -> Sub (convert_astexpr e1, convert_astexpr e2)
            | (Lident "*", [(_, e1), (_, e2)]) -> Mul (convert_astexpr e1, convert_astexpr e2)
            | (Lident "/", [(_, e1), (_, e2)]) -> Div (convert_astexpr e1, convert_astexpr e2)
            | (Lident f, [(_, arg)]) -> App (Var f, convert_astexpr arg)
            | _ -> raise Failure "not handled"
        | (Pexp_fun _, [(_, arg)]) -> App (convert_astexpr e, convert_astexpr arg)
        | _ -> raise Failure "not handled"
    | Pexp_fun (_, _, p, e) -> Lam (pat_to_var p, convert_astexpr e)
    | _ -> raise Failure "not handled"

(* convert binding to Assign instr *)
let convert_binding b =
  let id = pat_to_var (b.pvb_pat) in
  let exp = convert_astexpr (b.pvb_expr) in
  Assign (id, exp)
  
(* convert structure_item_desc to instr *)
let convert_struct_item_desc = function
  | Pstr_value (Nonrecursive, [binding]) ->  convert_binding bindin
  | _ -> raise Failure "todo"

(* convert structure_item to instr *)
let convert_struct_item i =
  let _ = i.pstr_desc

(* convert phrase to instr map *)
let convert_phrase = function
  | Ptop_def s -> List.fold ...
  | Ptop_dir _ -> raise Failure "not handled"
