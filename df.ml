open Core
open Lang_def
open Cfg

type domain =
  | Top
  | Bot
  | Constant of expr

let string_of_dom = function
  | Top -> "Top"
  | Bot -> "Bot"
  | Constant e -> string_of_expr e

let dom_equal d1 d2 =
  match (d1, d2) with
    | (Top, Top) -> true
    | (Bot, Bot) -> true
    | (Constant e1, Constant e2) -> expr_equal e1 e2
    | _ -> false

type sigma = domain String.Map.t

let string_of_sigma sigma =
  let show_abstract_val ~key:variable ~data:abstract_value values =
    Format.sprintf "%s%s = %s; " values variable (string_of_dom abstract_value)
  in
  let values = String.Map.fold sigma ~init:"" ~f:show_abstract_val in
  Format.sprintf "[ %s]" values


type df_results = sigma Int.Map.t

let string_of_results results =
  let show_result ~key:location ~data:sigma results =
    Format.sprintf "%s\n%d: %s" results location (string_of_sigma sigma)
  in
  Int.Map.fold results ~init:"Results before node n" ~f:show_result

let string_of_results_listing listing results =
  let show_instr ~key:loc ~data:instr collected =
    let result =
      match Int.Map.find results loc with
      | None -> "[ ]"
      | Some sigma -> string_of_sigma sigma
    in
    Format.sprintf "%s\n%s\n%s" collected result (string_of_instr loc instr)
  in
  Int.Map.fold listing ~init:"Results:" ~f:show_instr
  
(* lifts expressions to the domain *)
let alpha (n : expr) : domain = 
  match n with
    | Const i -> Constant n
    | Lam (x, e) -> Constant n
    | Var x -> Bot (* uncertain about these last two, double check later *)
    | _ -> Top
  
let join_values (v1 : domain) (v2 : domain) =
  match (v1, v2) with
    | (Top, _) | (_, Top) -> Top
    | (Bot, x) -> x
    | (x, Bot) -> x 
    | (Constant e1, Constant e2) -> if expr_equal i j then Constant e1 else Top
    | (_, _) -> Top

let sigma_ne (state1 : sigma) (state2 : sigma) : bool =
  not (String.Map.equal (dom_equal) state1 state2)
    
let flow (state : sigma) (code : instr) (e_type : Cfg.edge): sigma =
  raise Failure "Not implemented"
