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
  | Bind of id * expr

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
  | Bind (v, e) -> Format.sprintf "%d: %s := %s" i v (string_of_expr e)

let string_of_listing listing =
  Int.Map.fold_right listing ~init:[]
    ~f:(fun ~key:lineno ~data:i accum -> (string_of_instr lineno i) :: accum)
  |> String.concat ~sep:"\n"

let string_of_program (pc, listing) =
  Format.sprintf "PC: %d\nListing:\n%s" pc (string_of_listing listing)

(* [x/y]e *)
let sub_var (x:id) (y:id) (e:expr) =
  match e with
    | Var i -> if y = i then Var x else e
    | Const i -> e
    | App (e1, e2) -> App (sub_var x y e1, sub_var x y e2)
    | Lam (z, e1) -> if y = z then Lam (x, sub_var x y e1) else Lam (z, sub_var x y e1)
    | Add (e1, e2) -> Add (sub_var x y e1, sub_var x y e2)
    | Sub (e1, e2) -> Sub (sub_var x y e1, sub_var x y e2)
    | Mul (e1, e2) -> Mul (sub_var x y e1, sub_var x y e2)
    | Div (e1, e2) -> Div (sub_var x y e1, sub_var x y e2)

(* e1 \cong e2 *)
let expr_equal e1 e2 =
  match (e1, e2) with
    | (Var x, Var y) -> x = y
    | (Const i, Const j) -> i = j
    | (App (n1, n2), App (m1, m2)) -> (expr_equal n1 m1) and (expr_equal n2 m2)
    | (Lam (x, r), Lam (y, t)) -> expr_equal r (sub_var x y t)
    | (Add (n1, n2), Add (m1, m2)) -> (expr_equal n1 m1) and (expr_equal n2 m2)
    | (Sub (n1, n2), Sub (m1, m2)) -> (expr_equal n1 m1) and (expr_equal n2 m2)
    | (Mul (n1, n2), Mul (m1, m2)) -> (expr_equal n1 m1) and (expr_equal n2 m2)
    | (Div (n1, n2), Div (m1, m2)) -> (expr_equal n1 m1) and (expr_equal n2 m2)
    | _ -> false
