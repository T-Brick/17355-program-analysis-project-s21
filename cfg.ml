open Core
open While3addr.Lang

type edge = Direct | NoEdge

let string_of_edge = function
  | Direct -> "Direct"
  | NoEdge -> ""

module Pair = struct
  module T = struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match compare x0 x1 with
      | 0 -> compare y0 y1
      | c -> c
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
  end
  include T
  include Comparable.Make(T)
end

module PairMap = Map.Make(Pair)

type t =
  { nodes : instr Int.Map.t;
    edges : edge PairMap.t;
  }

let find cfg edge =
  match PairMap.find cfg.edges edge with
  | None -> NoEdge
  | Some e -> e

let succs cfg node_id =
  let f ~key:(source, target) ~data:_ targets =
    if source = node_id then
      target :: targets
    else
      targets
  in
  PairMap.fold cfg.edges ~init:[] ~f

let preds cfg node_id =
  let f ~key:(source, target) ~data:_ sources =
    if target = node_id then
      source :: sources
    else
      sources
  in
  PairMap.fold cfg.edges ~init:[] ~f

let incoming cfg node_id : (int * edge) list =
  let is_target (_, target) = target = node_id in
  let filtered = PairMap.filter_keys cfg.edges ~f:is_target in
  let f ~key:(source, _) ~data cur = (source, data) :: cur in
  PairMap.fold filtered ~init:[] ~f

let outgoing cfg node_id : (int * edge) list =
  let is_source (source, _) = source = node_id in
  let filtered = PairMap.filter_keys cfg.edges ~f:is_source in
  let f ~key:(_, target) ~data cur = (target, data) :: cur in
  PairMap.fold filtered ~init:[] ~f

let of_listing listing =
  let edges =
    let collect_edges ~key:cur_loc ~data:instr edges =
      (* hacky way to deal with the end of the program, since I haven't put in
         special start/end nodes.  Less hacky than it was before, at least. *)
      let next_loc = cur_loc + 1 in
      let not_last = Int.Map.mem listing next_loc in
      let add_edge edges ~edge:(loc1, loc2) ~kind =
        PairMap.set ~key:(loc1, loc2) ~data:kind edges
      in
      match instr with
      | Goto target ->
         add_edge edges ~edge:(cur_loc, target) ~kind:Direct
      | IfGoto (_, _, target) ->
         let edges = add_edge edges ~edge:(cur_loc, target) ~kind:CondT in
         if not_last then
           add_edge edges ~edge:(cur_loc, next_loc) ~kind:CondF
         else
           edges
      | _ when not_last ->
         add_edge edges ~edge:(cur_loc, next_loc) ~kind:Direct
      | _ -> edges
    in
    Int.Map.fold listing ~init:PairMap.empty ~f:collect_edges
  in
  { nodes = listing; edges; }

let of_prog (_, listing) = of_listing listing

let show cfg =
  let rec show_list = function
    | (n, Direct) :: rest -> Printf.sprintf "%d; %s" n (show_list rest)
    | (_, NoEdge) :: rest -> show_list rest
    | _ -> ""
  in
  let print_preds node_id =
    match incoming cfg node_id with
    | [] -> Printf.printf "\nNode [%d]: no predecessors" node_id
    | x -> Printf.printf "\nNode [%d]: %s" node_id (show_list x)
  in
  let print_succs node_id =
    match outgoing cfg node_id with
    | [] -> Printf.printf "\nNode [%d]: no successors" node_id
    | x -> Printf.printf "\nNode [%d]: %s" node_id (show_list x)
  in

  Printf.printf "Predecessors: ";
  Int.Map.iter_keys cfg.nodes ~f:print_preds;
  Printf.printf "\nSuccessors: ";
  Int.Map.iter_keys cfg.nodes ~f:print_succs
