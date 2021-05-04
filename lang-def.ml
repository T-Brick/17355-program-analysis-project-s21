open Core

type id = string
type lineno = int

type expr = 
  | Var of id 
  | Const of int 
  | App of expr * expr 
  | Lam of id * expr
  (* built-in arithmetic *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

type instr =
  | Assign of id * expr

type listing = instr Int.Map.t (* map from line numbers to instructions *)

type program = int * listing

let string_of_expr = function
  | Var v -> Format.sprintf "%s" v
  | Const n -> Format.sprintf "%d" n
  | App (e1, e2) -> Format.sprintf "%s %s" (string_of_expr e1) (string_of_expr e2)
  | Lam (v, e) -> Format.sprintf "%s %s" v (string_of_expr e)
  | Add (e1, e2) -> Format.sprintf "%s + %s" (string_of_expr e1) (string_of_expr e2)
  | Sub (e1, e2) -> Format.sprintf "%s - %s" (string_of_expr e1) (string_of_expr e2)
  | Mul (e1, e2) -> Format.sprintf "%s * %s" (string_of_expr e1) (string_of_expr e2)
  | Div (e1, e2) -> Format.sprintf "%s / %s" (string_of_expr e1) (string_of_expr e2)

let string_of_instr i = function
  | Assign (v, e) -> Format.sprintf "%d: %s := %s" i v (string_of_expr e)

let string_of_listing listing =
  Int.Map.fold_right listing ~init:[]
    ~f:(fun ~key:lineno ~data:i accum -> (string_of_instr lineno i) :: accum)
  |> String.concat ~sep:"\n"

let string_of_program (pc, listing) =
  Format.sprintf "PC: %d\nListing:\n%s" pc (string_of_listing listing)
