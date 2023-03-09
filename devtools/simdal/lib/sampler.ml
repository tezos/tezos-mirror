open G

(** {2 Core type definitions} *)

(** ['kind cfg] is the type of the configuration of the network sampler, parametric
    over node ['kind]. *)
type 'kind cfg = {
  bounds : vertex -> int * int;
      (** [bounds v] is the inclusive interval in which the degree of [v] should be. *)
  kind : vertex -> 'kind;  (** [kind v] is the kind of [v]. *)
  compat : 'kind -> 'kind -> bool;
      (** [compat k1 k2] is a symmetric and reflexive relation on kinds.
              It does not need to be transitive. *)
  edges : Edge.t array;
      (** [edges] contain all edges [v1, v2] such that [compat (kind v1) (kind v2)] holds.
              It represents the maximal graph satisfying [compat]. *)
}

type state =
  | State : {
      graph : G.t;  (** [graph] represents the network. *)
      degrees : int Vertex_map.t;
          (** [degrees] is used to bypass the slow [G.out_degree]  *)
      error : float;
          (** [error] is the total error over all nodes. Error is computed for each
                node as the absolute value of the difference between the current degree
                of the node and the closest bound of its bounding interval. Hence if
                the degree lies in the interval, the error is 0 for that node. *)
      cfg : 'kind cfg;
    }
      -> state

(** {2 Accessors} *)

let graph (State {graph; _}) = graph

let error (State {error; _}) = error

let vertex_count (State {graph; _}) = G.nb_vertex graph

let edge_count (State {cfg = {edges; _}; _}) = Array.length edges

(** {2 State initialization helpers} *)

let rec product_outer l1 l2 f acc =
  match l1 with
  | [] -> acc
  | x1 :: tl1 ->
      let acc = product_inner x1 l2 f acc in
      product_outer tl1 l2 f acc

and product_inner x1 l2 f acc =
  match l2 with
  | [] -> acc
  | x2 :: tl2 ->
      let acc = f x1 x2 acc in
      product_inner x1 tl2 f acc

let product l1 l2 f acc = product_outer l1 l2 f acc

(** {2 Helpers to compute the error induced by violated degree bound constraints} *)

let err delta = abs_float delta

let degree (State {degrees; _}) v =
  Vertex_map.find_opt v degrees |> Option.value ~default:0

let incr_degree degrees v =
  Vertex_map.update
    v
    (function None -> Some 1 | Some d -> Some (d + 1))
    degrees

let decr_degree degrees v =
  Vertex_map.update
    v
    (function None -> assert false | Some d -> Some (d - 1))
    degrees

let node_error (lo, hi) deg =
  assert (0 <= lo && lo <= hi) ;
  if lo <= deg then
    if deg <= hi then 0.0 else (* deg > hi *)
                            err (float_of_int (deg - hi))
  else (* deg < low *)
    err (float_of_int (lo - deg))

(** {2 State initialization} *)

let create_empty n ~kind ~compat ~bounds =
  if n <= 0 then invalid_arg "create_empty: n <= 0" ;
  let graph =
    Seq.unfold (fun i -> if i >= n then None else Some (i, i + 1)) 0
    |> Seq.fold_left G.add_vertex G.empty
  in
  let vertices = G.fold_vertex (fun x l -> x :: l) graph [] in
  let edges =
    product
      vertices
      vertices
      (fun v v' acc ->
        if v >= v' then (* Important: we exclude self edges *)
          acc
        else
          let k1 = kind v in
          let k2 = kind v' in
          if compat k1 k2 then Edge_set.add (v, v') acc else acc)
      Edge_set.empty
  in
  let edges = Array.of_seq (Edge_set.to_seq edges) in
  assert (Array.length edges > 0) ;
  Format.printf "Maximal graph has %d edges@." (Array.length edges) ;
  let error =
    G.fold_vertex
      (fun v acc -> acc +. node_error (bounds v) (G.out_degree graph v))
      graph
      0.0
  in
  Format.printf "initial error: %f@." error ;
  let cfg = {bounds; kind; compat; edges} in
  let degrees = Vertex_map.empty in
  State {graph; error; cfg; degrees}

(** {2 Monte Carlo-based sampling of networks respecting the degree bound constraints} *)

(** [quality_delta graph_before bounds (v, v') is_added] computes the network quality increment starting
    from [graph_before], under degree constraints specified by [bounds],
    when the edge [v, v'] is flipped. If [add] is [true], the
    edge is added, if not it is removed. *)
let quality_delta graph_before bounds (v, v') is_added =
  let vbounds = bounds v in
  let v'bounds = bounds v' in
  let deg_v = degree graph_before v in
  let deg_v' = degree graph_before v' in
  let deg_incr = if is_added then 1 else -1 in
  (* error before *)
  let v_bef = node_error vbounds deg_v in
  let v_aft = node_error vbounds (deg_v + deg_incr) in
  (* error after *)
  let v'_bef = node_error v'bounds deg_v' in
  let v'_aft = node_error v'bounds (deg_v' + deg_incr) in
  (* delta *)
  v'_aft +. v_aft -. v'_bef -. v_bef

(** Parameters of the network sampler. *)
module MH_parameters = struct
  type t = state

  let pp fmtr (State state) = Format.fprintf fmtr "error=%f@." state.error

  (** [proposal state rng_state] samples uniformly at random in the maximal graph
      an edge to flip. If the edge exists, it is removed. If not, it is added. *)
  let proposal (State state as s : t) rng_state =
    let {edges; bounds; _} = state.cfg in
    let graph = state.graph in
    let error = state.error in
    let i = Random.State.int rng_state (Array.length edges) in
    let ((v, v') as edge) = edges.(i) in
    if G.mem_edge_e graph edge then
      let graph' = G.remove_edge_e graph edge in
      let error = error +. quality_delta s bounds edge false in
      let degrees = decr_degree state.degrees v in
      let degrees = decr_degree degrees v' in
      State {state with graph = graph'; error; degrees}
    else
      let graph' = G.add_edge_e graph edge in
      let error' = error +. quality_delta s bounds edge true in
      let degrees = incr_degree state.degrees v in
      let degrees = incr_degree degrees v' in
      State {state with graph = graph'; error = error'; degrees}

  let proposal_log_density _ _ = Stats.Log_space.one

  (** [log_weight] encodes the logarithm of the objective function that the sampler will try
      to maximize.
      The objective function is proportional to [exp (- error^2)], hence the smaller the error
      the bigger the objective function. *)
  let log_weight (State {error; _}) =
    (* If we don't square the error, the ratio of a big error and a slightly bigger error
       is still close to 1 - so we stay in error land. By squaring, we make Metropolis 'feel'
       that increasing a big error is more bad than increasing a small error. *)
    Stats.Log_space.unsafe_cast ~-.(Float.max 1. error ** 2.)
end

(** [Network_sampler] instantiates the network sampler. *)
module Network_sampler = Stats.Mh.Make (MH_parameters)

let network = Network_sampler.mcmc

(** {2 Basic statistics over networks} *)

module Network_stats_helpers = Stats.Graph.Make (struct
  include G

  module V = struct
    include V

    let pp = Format.pp_print_int
  end
end)

(** [avg_degree state] computes the average degree of the network. *)
let avg_degree (State state) =
  let g = state.graph in
  let vertices = G.nb_vertex g in
  let volume = G.fold_vertex (fun v acc -> acc + G.out_degree g v) g 0 in
  float volume /. float vertices

(** {2 Sampling routings uniformly at random and deriving bandwidth statistics. } *)

let op (x, y) = (y, x) [@@ocaml.inline]

type bandwidth_stats = {
  incoming : float ref Vertex_table.t;
  outgoing : float ref Vertex_table.t;
}

let create_bandwidth_stats () =
  {incoming = Vertex_table.create 51; outgoing = Vertex_table.create 51}

let normalize_bandwidth_stats stats count =
  let nrm = 1. /. float count in
  Vertex_table.iter (fun _v r -> r := !r *. nrm) stats.incoming ;
  Vertex_table.iter (fun _v r -> r := !r *. nrm) stats.outgoing

let float_incr table key dx =
  match Vertex_table.find_opt table key with
  | None -> Vertex_table.add table key (ref 0.0)
  | Some x -> x := !x +. dx
  [@@ocaml.inline]

let uniform_spanning_trees ~graph ~source ~subgraph_predicate =
  if not (subgraph_predicate source) then
    Format.kasprintf
      invalid_arg
      "uniform_spanning_trees: source vertex %d is not in subraph induced by \
       predicate"
      source ;
  let random_spanning_tree =
    Network_stats_helpers.aldous_broder graph source subgraph_predicate
  in
  Stats.Gen.iid random_spanning_tree

let vertices_of_tree tree =
  let verts = ref [] in
  Network_stats_helpers.Tree.iter_vertices tree (fun x -> verts := x :: !verts) ;
  let verts = Array.of_list !verts in
  Array.sort Int.compare verts ;
  verts

let estimate_bandwidth ~state ~subgraph_predicate ~counters ~spanning_trees
    rng_state =
  let (State {graph; cfg = _; _}) = state in
  let {incoming; outgoing} = counters in

  let nsamples = List.length spanning_trees in
  assert (nsamples > 0) ;
  (* [db] is the bandwidth increment, pre-normalized by [nsamples] *)
  let db = 1. /. float nsamples in
  (* Note we avoid reallocating this rather large table at each iteration. *)
  let add_edge_to_routing routing ((src, dst) as e) =
    float_incr incoming dst db ;
    float_incr outgoing src db ;
    Edge_table.add routing e ()
    [@@ocaml.inline]
  in
  (* Invariant: all spanning trees have the same support, corresponding to the subgraph predicate.
     Hence, we optimize as follows:
     - compute [verts], the set of vertices of the first spanning tree, corresponding
       equal to the set of vertices in the subgraph.
     - compute [succs], the set of successors of each element of [verts] that are in the
       subgraph.
     Use [verts] and [succs] in the loop below, to iterate efficiently on those edges
     that are in the subgraph but not in the spanning tree.
     Without this optimization, we need to iterate over all successors in the {e full}
     graph, which is potentially much larger. *)
  let verts = vertices_of_tree (List.hd spanning_trees) in
  let succs =
    let tbl = Hashtbl.create 11 in
    Array.iter
      (fun v ->
        G.iter_succ
          (fun v' -> if subgraph_predicate v' then Hashtbl.add tbl v v')
          graph
          v)
      verts ;
    tbl
  in
  let routing = Edge_table.create (Hashtbl.length succs) in
  List.iter
    (fun spanning_tree ->
      Edge_table.clear routing ;
      Network_stats_helpers.Tree.iter_edges spanning_tree (fun e ->
          add_edge_to_routing routing e) ;
      Array.iter
        (fun v ->
          let succs = Hashtbl.find_all succs v in
          List.iter
            (fun v' ->
              let e = (v, v') in
              (* [e] or [opp e] is possibly in the routing table,
                 in which case we have the property that it has
                 already been accounted for in the stats *)
              if Edge_table.mem routing e then ()
              else
                let flip = Random.State.bool rng_state in
                add_edge_to_routing routing (if flip then e else op e))
            succs)
        verts)
    spanning_trees
