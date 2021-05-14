open Core
open Printf
open Parsetree


(* convert pattern to variable *)
let pat_to_var p =
  match p.ppat_desc with
    | Ppat_var s -> s.txt
    | _ -> raise (Failure "not handled")

(* converts curried function application to lang expression *)
let rec curried_application (acc : expr) = function
  | [(_, arg)] -> App (acc, convert_astexpr arg)
  | (_, arg)::args -> curried_application (App (acc, convert_astexpr arg)) args

(* convert AST expression to lang expression *)
let rec convert_astexpr (e : expression) : expr =
  match e.pexp_desc with
    | Pexp_ident c -> (
      match c.txt with
        | Lident v -> Var v
        | _ -> raise (Failure "not handled")
    )
    | Pexp_constant (Pconst_integer (ns, _)) -> Const (int_of_string ns)
    | Pexp_apply (e, args) -> (
      match (e.pexp_desc, args) with
        | (Pexp_ident f, _) -> (
          match (f.txt, args) with
            | (Lident "+", (_, e1)::(_, e2)::_) -> Add (convert_astexpr e1, convert_astexpr e2)
            | (Lident "-", (_, e1)::(_, e2)::_) -> Sub (convert_astexpr e1, convert_astexpr e2)
            | (Lident "*", (_, e1)::(_, e2)::_) -> Mul (convert_astexpr e1, convert_astexpr e2)
            | (Lident "/", (_, e1)::(_, e2)::_) -> Div (convert_astexpr e1, convert_astexpr e2)
            | (Lident f, args) -> curried_application (Var f) args
            | _ -> raise (Failure "not handled")
        )
        (* application of lambda *)
        | (Pexp_fun _, [(_, arg)]) -> App (convert_astexpr e, convert_astexpr arg)
        | _ -> raise (Failure "not handled")
      )
    (* lambda value *)
    | Pexp_fun (_, _, p, e) -> Lam (pat_to_var p, convert_astexpr e)
    | _ -> raise (Failure "not handled")

(* convert binding to Bind instr *)
let convert_binding (b : value_binding) : instr =
  let id = pat_to_var (b.pvb_pat) in
  let exp = convert_astexpr (b.pvb_expr) in
  Bind (id, exp)
  
(* convert structure_item_desc to instr *)
let convert_struct_item_desc : structure_item_desc -> instr = function
  | Pstr_value (Nonrecursive, [binding]) -> convert_binding binding
  | _ -> raise (Failure "not handled")

(* add structure_item to accumulator *)
let convert_struct_item (i, cur_map) (s : structure_item) : lineno * instr Int.Map.t =
  let res = convert_struct_item_desc (s.pstr_desc) in
  let new_map = Int.Map.add_exn cur_map i res in
  (i + 1, new_map)

(* convert phrase to instr map *)
let convert_phrase (i, cur_map) : toplevel_phrase -> int * instr Int.Map.t = function
  | Ptop_def s -> List.fold_left s ~init:(i, cur_map) ~f:convert_struct_item
  | Ptop_dir _ -> raise (Failure "not handled")

(* convert phrase list to instr map *)
let convert_phrase_list ps =
  let init : int * instr Int.Map.t = (0, Int.Map.empty) in
  List.fold_left ps ~init:init ~f:convert_phrase 

(* creates and prints the AST for the ocaml code *)
let run filename = 
  let s = (In_channel.read_all filename) ^ "\n" ^ "let dummy = -1" in
  let lexBuf = Lexing.from_string s in
  let parseTree = Parse.use_file (lexBuf) in
  let (n, listing) = convert_phrase_list parseTree in
  let cfg = of_listing listing in
    kildall cfg
      |> string_of_results
      |> Format.printf "%s\n"


let _ = run "tests/basic.ml"
