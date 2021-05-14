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
  (* boolean operations *)
  | Bool of bool
  | Not of expr
  | Eq of expr * expr         (* e1 = e2 *)
  | Gt of expr * expr         (* e1 > e2 *)
  | Lt of expr * expr         (* e1 < e2 *)
  | IfE of expr * expr * expr (* if e1 then e2 else e3 *)

type instr =
  | Bind of id * expr
  (*
  | Type of id * ?
  *)
  
type listing = instr Int.Map.t (* map from line numbers to instructions *)

type program = int * listing

let rec string_of_expr = function
  | Var v -> Format.sprintf "%s" v
  | Const n -> Format.sprintf "%d" n
  | App (e1, e2) -> Format.sprintf "%s %s" (string_of_expr e1) (string_of_expr e2)
  | Lam (v, e) -> Format.sprintf "λ%s.(%s)" v (string_of_expr e)
  | Add (e1, e2) -> Format.sprintf "%s + %s" (string_of_expr e1) (string_of_expr e2)
  | Sub (e1, e2) -> Format.sprintf "%s - %s" (string_of_expr e1) (string_of_expr e2)
  | Mul (e1, e2) -> Format.sprintf "%s * %s" (string_of_expr e1) (string_of_expr e2)
  | Div (e1, e2) -> Format.sprintf "%s / %s" (string_of_expr e1) (string_of_expr e2)
  | Bool b -> if b then "true" else "false"
  | Not e -> Format.sprintf "~%s" (string_of_expr e)
  | Eq (e1, e2) -> Format.sprintf "%s = %s" (string_of_expr e1) (string_of_expr e2)
  | Gt (e1, e2) -> Format.sprintf "%s > %s" (string_of_expr e1) (string_of_expr e2)
  | Lt (e1, e2) -> Format.sprintf "%s < %s" (string_of_expr e1) (string_of_expr e2)
  | IfE (e1, e2, e3) -> Format.sprintf "if %s then %s else %s" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)


let string_of_instr i = function
  | Bind (v, e) -> Format.sprintf "%d: %s := %s" i v (string_of_expr e)

let string_of_listing listing =
  Int.Map.fold_right listing ~init:[]
    ~f:(fun ~key:lineno ~data:i accum -> (string_of_instr lineno i) :: accum)
  |> String.concat ~sep:"\n"

let string_of_program (pc, listing) =
  Format.sprintf "PC: %d\nListing:\n%s" pc (string_of_listing listing)

(* [x/y]e *)
let rec sub_var (x:id) (y:id) (e:expr) =
  match e with
    | Var i -> if Poly.(=) y i then Var x else e
    | Const i -> e
    | App (e1, e2) -> App (sub_var x y e1, sub_var x y e2)
    | Lam (z, e1) -> if Poly.(=) y z then Lam (x, sub_var x y e1) else Lam (z, sub_var x y e1)
    | Add (e1, e2) -> Add (sub_var x y e1, sub_var x y e2)
    | Sub (e1, e2) -> Sub (sub_var x y e1, sub_var x y e2)
    | Mul (e1, e2) -> Mul (sub_var x y e1, sub_var x y e2)
    | Div (e1, e2) -> Div (sub_var x y e1, sub_var x y e2)
    | Bool b -> e
    | Not e1 -> Not (sub_var x y e1)
    | Eq (e1, e2) -> Eq (sub_var x y e1, sub_var x y e2)
    | Gt (e1, e2) -> Gt (sub_var x y e1, sub_var x y e2)
    | Lt (e1, e2) -> Lt (sub_var x y e1, sub_var x y e2)
    | IfE (e1, e2, e3) -> IfE (sub_var x y e1, sub_var x y e2, sub_var x y e3)

(* e1 \cong e2 *)
let rec expr_equal e1 e2 =
  match (e1, e2) with
    | (Var x, Var y) -> Poly.(=) x y
    | (Const i, Const j) -> i = j
    | (App (n1, n2), App (m1, m2)) -> (expr_equal n1 m1) && (expr_equal n2 m2)
    | (Lam (x, r), Lam (y, t)) -> expr_equal r (sub_var x y t)
    | (Add (n1, n2), Add (m1, m2)) -> (expr_equal n1 m1) && (expr_equal n2 m2)
    | (Sub (n1, n2), Sub (m1, m2)) -> (expr_equal n1 m1) && (expr_equal n2 m2)
    | (Mul (n1, n2), Mul (m1, m2)) -> (expr_equal n1 m1) && (expr_equal n2 m2)
    | (Div (n1, n2), Div (m1, m2)) -> (expr_equal n1 m1) && (expr_equal n2 m2)
    | (Bool b1, Bool b2) -> Poly.(=) b1 b2
    | (Not e1, Not e2) -> expr_equal e1 e2
    | (Eq (n1, n2), Eq (m1, m2)) -> (expr_equal n1 m1) && (expr_equal n2 m2)
    | (Gt (n1, n2), Gt (m1, m2)) -> (expr_equal n1 m1) && (expr_equal n2 m2)
    | (Lt (n1, n2), Lt (m1, m2)) -> (expr_equal n1 m1) && (expr_equal n2 m2)
    | (IfE (n1, n2, n3), IfE (m1, m2, m3)) -> (expr_equal n1 m1) && (expr_equal n2 m2) && (expr_equal n3 m3)
    | _ -> false
