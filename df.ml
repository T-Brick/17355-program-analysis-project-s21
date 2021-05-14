open Core
(* open Lang_def
open Cfg *)

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
    | (Constant e1, Constant e2) -> if expr_equal e1 e2 then Constant e1 else Top

let join (s1 : sigma) (s2 : sigma) : sigma =
  String.Map.merge s1 s2 
    ~f:(fun ~key:_ -> function
    | `Left _ | `Right _ -> failwith "States contain different number of variables!"
    | `Both (v1, v2) -> Some (join_values v1 v2))

let sigma_ne (state1 : sigma) (state2 : sigma) : bool =
  not (String.Map.equal (dom_equal) state1 state2)

let (!=) = sigma_ne

(* swap out known bindings in an expression. This means that shadowing
  later wont make certain functions equal when they shouldn't be *)
let rec subst_state (state : sigma) (e : expr) : expr =
  match e with
    | Var x -> (
      match String.Map.find state x with
        | Some (Constant e') -> e'
        | _ -> e
    )
    | Const n -> e
    | Lam (y, e') -> Lam (y, subst_state (String.Map.set state ~key:y ~data:Top) e')
    | App (e1, e2) -> App (subst_state state e1, subst_state state e2)
    | Add (e1, e2) -> Add (subst_state state e1, subst_state state e2)
    | Sub (e1, e2) -> Sub (subst_state state e1, subst_state state e2)
    | Mul (e1, e2) -> Mul (subst_state state e1, subst_state state e2)
    | Div (e1, e2) -> Div (subst_state state e1, subst_state state e2)

(* reduces an expression to a domain given a state *)
let rec reduce (state : sigma) (e : expr) : domain = 
  match e with
    | Var x -> String.Map.find_exn state x
    | Const n -> Constant e
    | Lam (y, e') -> (
      let state' = (String.Map.set state ~key:y ~data:Top) in
      match (reduce state' e') with
        | Top -> Constant (subst_state state' e)
        | Bot -> Constant (subst_state state' e)
        | Constant re' -> Constant (Lam(y, re'))
    )
    | App (Lam (y, e'), e'') -> reduce (String.Map.set state ~key:y ~data:(reduce state e'')) e'
    | App (e1, e2) -> (
      match reduce state e1 with
        | Top -> Top
        | Bot -> Bot
        | Constant l -> reduce state (App (l, e2))
    )
    | App (_, _) -> Bot            (* malformed, application must be on lambda *)
    | Add (e1, e2) -> binOpReduce state (+) e1 e2
    | Sub (e1, e2) -> binOpReduce state (-) e1 e2
    | Mul (e1, e2) -> binOpReduce state ( * ) e1 e2
    | Div (e1, e2) -> binOpReduce state (/) e1 e2
(* helper function for reducing binary operators *)
and binOpReduce state (f : int -> int -> int) e1 e2 =
    match (reduce state e1, reduce state e2) with 
      | (Top, _) | (_, Top) -> Top
      | (Bot, x) -> x
      | (x, Bot) -> x
      | (Constant (Const n1), Constant (Const n2)) -> Constant (Const (f n1 n2))
      | _ -> Bot (* malformed *)
    
let flow (state : sigma) (code : instr) (e_type : edge): sigma =
  match code with
    | Bind (x, e) -> String.Map.set state ~key:x ~data:(
      match e with
        | Var y   -> String.Map.find_exn state y
        | Const _ -> Constant e
        | Lam _   -> reduce state e
        | App _   -> reduce state e 
        | Add _   -> reduce state e  
        | Sub _   -> reduce state e  
        | Mul _   -> reduce state e  
        | Div _   -> reduce state e  
      )

(* This initializes a state for the cfg by mapping all variables to the passed in abstract value *)      
let initializeSigma (cfg: t) (value: domain) : sigma = 
  let nodes = Int.Map.keys cfg.nodes in 
  let add_var (s) (n) =
    match (Int.Map.find_exn cfg.nodes n) with
    | Bind (x, _) -> String.Map.set s ~key:x ~data:value
  in
  List.fold nodes ~init:(String.Map.empty) ~f:(add_var)

(*
  Loops through all the successor edges, computing the flow and adding them to worklist if needed.
 *)
let rec next (ns : lineno list) (inputs : df_results) (fl) = function
  | [] -> inputs, ns
  | (j, e)::s ->
    let inputj = Int.Map.find_exn inputs j in
    let newInput = join (inputj) (fl e) in
      if newInput != inputj 
      then next (j::ns) (Int.Map.set inputs ~key:j ~data:newInput) (fl) s
      else next ns inputs fl s

let kildall (cfg : t) : df_results =
  let rec work (inputs : df_results) = function
    | [] -> inputs (* while worklist is not empty *)
    | n :: ns -> (* take node n off of the worklist *)
       let instr_n = Int.Map.find_exn cfg.nodes n in
       (* find_exn throws an exception if the key is not found in the map.
        * Because of how this algorithm works, we should always have an input
        * state of some kind for any node we're pulling off the worklist, so if
        * this throws an exception something has gone wrong *)
       let input_n = Int.Map.find_exn inputs n in
       let inputs', worklist' = next ns inputs (flow input_n instr_n) (outgoing cfg n)
       in work inputs' worklist'
  in
  (* This initializes the inputMap, mapping every variable to Top for the first instruction
     in the code/node in the graph, and mapping every variable to Bot for the rest of the nodes. *)
  let botSigma = initializeSigma (cfg) (Bot) in
  let topSigma = initializeSigma (cfg) (Top) in
  let inputMap = Int.Map.map cfg.nodes ~f:(fun k -> botSigma) in
  (* let inputMap = Int.Map.set inputMap ~key:0 ~data: topSigma in *)
  work (inputMap) (List.sort (Int.Map.keys cfg.nodes) ~compare:Int.compare)
