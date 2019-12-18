(* Statistics on (simple, undirected) graphs. *)

(* We use an OCamlgraph-compatible module type to describe
   undirected graphs. *)

module type Graph_sig = sig
  type t

  module V : sig
    type t

    val compare : t -> t -> int

    val hash : t -> int
  end

  type vertex = V.t

  type edge

  val nb_vertex : t -> int

  val nb_edges : t -> int

  val out_degree : t -> vertex -> int

  val mem_vertex : t -> vertex -> bool

  val mem_edge : t -> vertex -> vertex -> bool

  val succ : t -> vertex -> vertex list

  val succ_e : t -> vertex -> edge list

  val iter_vertex : (vertex -> unit) -> t -> unit

  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_edges : (vertex -> vertex -> unit) -> t -> unit

  val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_succ : (vertex -> unit) -> t -> vertex -> unit

  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
end

module Dist = struct
  type t = Inf | Fin of int

  let zero = Fin 0

  let one = Fin 1

  let infty = Inf

  let ( + ) d1 d2 =
    match (d1, d2) with
    | (Inf, _) | (_, Inf) -> Inf
    | (Fin i, Fin j) -> Fin (i + j)

  let ( > ) d1 d2 =
    match (d1, d2) with
    | (Inf, Inf) -> false
    | (Inf, _) -> true
    | (_, Inf) -> false
    | (Fin i1, Fin i2) -> i1 > i2

  let max d1 d2 = if d1 < d2 then d2 else d1
end

module Make (Graph : Graph_sig) = struct
  let canon v1 v2 =
    let c = Graph.V.compare v1 v2 in
    match c with -1 | 0 -> (v1, v2) | 1 -> (v2, v1) | _ -> assert false

  module Undirected_edge = struct
    type t = Graph.vertex * Graph.vertex

    let equal (v1, v2) (v1', v2') =
      let (v1, v2) = canon v1 v2 in
      let (v1', v2') = canon v1' v2' in
      Graph.V.compare v1 v1' = 0 && Graph.V.compare v2 v2' = 0

    let hash (v1, v2) =
      let (v1, v2) = canon v1 v2 in
      Hashtbl.hash (Graph.V.hash v1, Graph.V.hash v2)
  end

  module Table = Hashtbl.Make (Undirected_edge)
  module Vertex_bij = Finbij.Make (Graph.V)

  let adjacency_matrix graph : Numerics.Float64.Mat.t * Vertex_bij.t =
    let open Numerics in
    let nb_vertex = Graph.nb_vertex graph in
    let vertices = Graph.fold_vertex (fun v l -> v :: l) graph [] in
    let vbij = Vertex_bij.of_list vertices in
    let matrix =
      Float64.Mat.init ~lines:nb_vertex ~cols:nb_vertex ~f:(fun i j ->
          let vi = Vertex_bij.nth_exn vbij i in
          let vj = Vertex_bij.nth_exn vbij j in
          if Graph.mem_edge graph vi vj then 1.0 else 0.0)
    in
    (matrix, vbij)

  type distance_table = (Graph.vertex * Graph.vertex, Dist.t) Hashtbl.t

  let floyd_warshall graph =
    let nb_vertex = Graph.nb_vertex graph in
    let table = Table.create (nb_vertex * nb_vertex * 2) in
    let find_dist table v1 v2 =
      match Table.find_opt table (canon v1 v2) with
      | None -> Dist.infty
      | Some dist -> dist
    in
    let set_dist table v1 v2 dist = Table.replace table (canon v1 v2) dist in
    Graph.iter_vertex (fun v -> Table.add table (v, v) Dist.zero) graph ;
    Graph.iter_edges (fun v1 v2 -> Table.add table (canon v1 v2) Dist.one) graph ;
    Graph.iter_vertex
      (fun vi ->
        Graph.iter_vertex
          (fun vj ->
            Graph.iter_vertex
              (fun vk ->
                let dij = find_dist table vi vj in
                let dik = find_dist table vi vk in
                let dkj = find_dist table vk vj in
                let len = Dist.(dik + dkj) in
                if Dist.(dij > len) then set_dist table vi vj len else ())
              graph)
          graph)
      graph ;
    table

  let diameter graph =
    Table.fold
      (fun _ dist acc -> Dist.max dist acc)
      (floyd_warshall graph)
      Dist.zero
end
