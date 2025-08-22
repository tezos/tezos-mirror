(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
include Object_graph_intf

module Events = struct
  include Internal_event.Simple

  let section = ["brassaia-eio"; "object_graph"]

  let object_graph_iter =
    declare_5
      ~section
      ~level:Debug
      ~name:"object_graph_iter"
      ~msg:
        "iter: depth: {depth}; rev: {rev}; min: {min}; max: {max}; cache_size: \
         {cache_size}"
      ("depth", Data_encoding.int64)
      ("rev", Data_encoding.bool)
      ("min", Data_encoding.(list string))
      ("max", Data_encoding.(list string))
      ("cache_size", Data_encoding.(option int64))

  let treat_key =
    declare_1
      ~section
      ~level:Debug
      ~name:"treat_key"
      ~msg:"Treat key {key}"
      ("key", Data_encoding.string)

  let visit_key_level =
    declare_2
      ~section
      ~level:Debug
      ~name:"visit_key_level"
      ~msg:"Visit key {key} at level {level}"
      ("key", Data_encoding.string)
      ("level", Data_encoding.int64)

  let output =
    declare_1
      ~section
      ~level:Debug
      ~name:"output"
      ~msg:"Output {name}"
      ("name", Data_encoding.string)
end

let src = Logs.Src.create "brassaia.graph" ~doc:"Brassaia graph support"

module Log = (val Logs.src_log src : Logs.LOG)

let list_partition_map f t =
  let rec aux fst snd = function
    | [] -> (List.rev fst, List.rev snd)
    | h :: t -> (
        match f h with
        | `Fst x -> aux (x :: fst) snd t
        | `Snd x -> aux fst (x :: snd) t)
  in
  aux [] [] t

module Make
    (Contents_key : Type.S)
    (Node_key : Type.S)
    (Commit_key : Type.S)
    (Branch : Type.S) =
struct
  module X = struct
    type t =
      [ `Contents of Contents_key.t
      | `Node of Node_key.t
      | `Commit of Commit_key.t
      | `Branch of Branch.t ]
    [@@deriving brassaia]

    let encoding =
      let open Data_encoding in
      union
        [
          case
            (Tag 1)
            ~title:"`Contents"
            Contents_key.encoding
            (function `Contents k -> Some k | _ -> None)
            (fun k -> `Contents k);
          case
            (Tag 2)
            ~title:"`Node"
            Node_key.encoding
            (function `Node k -> Some k | _ -> None)
            (fun k -> `Node k);
          case
            (Tag 3)
            ~title:"`Commit"
            Commit_key.encoding
            (function `Commit c -> Some c | _ -> None)
            (fun c -> `Commit c);
          case
            (Tag 4)
            ~title:"`Branch "
            Branch.encoding
            (function `Branch b -> Some b | _ -> None)
            (fun b -> `Branch b);
        ]

    let equal = Type.(unstage (equal t))

    let compare = Type.(unstage (compare t))

    let hash_contents = Type.(unstage (short_hash Contents_key.t))

    let hash_commit = Type.(unstage (short_hash Commit_key.t))

    let hash_node = Type.(unstage (short_hash Node_key.t))

    let hash_branch = Type.(unstage (short_hash Branch.t))

    (* we are using cryptographic hashes here, so the first bytes
       are good enough to be used as short hashes. *)
    let hash (t : t) : int =
      match t with
      | `Contents c -> hash_contents c
      | `Node n -> hash_node n
      | `Commit c -> hash_commit c
      | `Branch b -> hash_branch b
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional (X)
  module GO = Graph.Oper.I (G)
  module Topological = Graph.Topological.Make (G)

  module Table : sig
    type t

    val create : int option -> t

    val add : t -> X.t -> int -> unit

    val mem : t -> X.t -> bool
  end = struct
    module Lru = Lru.Make (X)
    module Tbl = Hashtbl.Make (X)

    type t = L of int Lru.t | T of int Tbl.t

    let create = function
      | None -> T (Tbl.create 1024)
      | Some n -> L (Lru.create n)

    let add t k v = match t with L t -> Lru.add t k v | T t -> Tbl.add t k v

    let mem t k = match t with L t -> Lru.mem t k | T t -> Tbl.mem t k
  end

  module Set = Set.Make (X)
  include G
  include GO

  type dump = vertex list * (vertex * vertex) list

  (* XXX: for the binary format, we can use offsets in the vertex list
     to save space. *)
  module Dump = struct
    type t = X.t list * (X.t * X.t) list [@@deriving brassaia]

    let encoding =
      Data_encoding.(tup2 (list X.encoding) (list (tup2 X.encoding X.encoding)))
  end

  let vertex g = G.fold_vertex (fun k set -> k :: set) g []

  let edges g = G.fold_edges (fun k1 k2 list -> (k1, k2) :: list) g []

  type action = Visit of (X.t * int) | Treat of X.t

  let iter ?cache_size ?(depth = max_int) ~pred ~min ~max ~node ?edge ~skip ~rev
      () =
    Events.(emit__dont_wait__use_with_care object_graph_iter)
      ( Int64.of_int depth,
        rev,
        List.map (Logging.to_string_exn X.encoding) min,
        List.map (Logging.to_string_exn X.encoding) max,
        Option.map Int64.of_int cache_size ) ;
    let marks = Table.create cache_size in
    let mark key level = Table.add marks key level in
    let todo = Stack.create () in
    (* if a branch is in [min], add the commit it is pointing to too. *)
    let min =
      List.fold_left
        (fun acc -> function
          | `Branch _ as x -> (x :: pred x) @ acc | x -> x :: acc)
        []
        min
    in
    let min = Set.of_list min in
    let has_mark key = Table.mem marks key in
    List.iter (fun k -> Stack.push (Visit (k, 0)) todo) max ;
    let treat key =
      Events.(emit_at_top_level treat_key)
        (Logging.to_string_exn X.encoding key) ;
      node key ;
      if not (Set.mem key min) then
        (* the edge function is optional to prevent an unnecessary computation
           of the preds .*)
        match edge with
        | None -> ()
        | Some edge ->
            let keys = pred key in
            List.map (fun k () -> edge key k) keys |> Eio.Fiber.all
      else ()
    in
    let visit_predecessors ~filter_history key level =
      let keys = pred key in
      (*if a commit is in [min] cut the history but still visit
        its nodes. *)
      List.iter
        (function
          | `Commit _ when filter_history -> ()
          | k -> Stack.push (Visit (k, level + 1)) todo)
        keys
    in
    let visit key level =
      if level >= depth then ()
      else if has_mark key then ()
      else
        match skip key with
        | true -> ()
        | false ->
            let () =
              Events.(emit_at_top_level visit_key_level)
                (Logging.to_string_exn X.encoding key, Int64.of_int level) ;
              mark key level ;
              if rev then Stack.push (Treat key) todo ;
              match key with
              | `Commit _ ->
                  visit_predecessors ~filter_history:(Set.mem key min) key level
              | _ ->
                  if Set.mem key min then ()
                  else visit_predecessors ~filter_history:false key level
            in
            if not rev then Stack.push (Treat key) todo
    in
    let rec pop () =
      match Stack.pop todo with
      | exception Stack.Empty -> ()
      | Treat key -> pop (treat key)
      | Visit (key, level) -> pop (visit key level)
    in
    pop ()

  let breadth_first_traversal ?cache_size ~pred ~max ~node () =
    let marks = Table.create cache_size in
    let mark key level = Table.add marks key level in
    let todo = Queue.create () in
    let has_mark key = Table.mem marks key in
    List.iter (fun k -> Queue.push (Visit (k, 0)) todo) max ;
    let treat key =
      Events.(emit__dont_wait__use_with_care treat_key)
        (Logging.to_string_exn X.encoding key) ;
      node key
    in
    let visit_predecessors key level =
      let keys = pred key in
      List.iter (fun k -> Queue.push (Visit (k, level + 1)) todo) keys
    in
    let visit key level =
      if has_mark key then ()
      else (
        Events.(emit__dont_wait__use_with_care visit_key_level)
          (Logging.to_string_exn X.encoding key, Int64.of_int level) ;
        mark key level ;
        treat key ;
        visit_predecessors key level)
    in
    let rec pop () =
      match Queue.pop todo with
      | exception Queue.Empty -> ()
      | Treat _ ->
          Fmt.failwith "in bfs always treat the node as soon as its visited"
      | Visit (key, level) -> pop (visit key level)
    in
    pop ()

  let closure ?(depth = max_int) ~pred ~min ~max () =
    let g = G.create ~size:1024 () in
    List.iter (G.add_vertex g) max ;
    let node key =
      if not (G.mem_vertex g key) then G.add_vertex g key else ()
    in
    let edge node pred = G.add_edge g pred node in
    let skip _ = false in
    iter ~depth ~pred ~min ~max ~node ~edge ~skip ~rev:false () ;
    g

  let min g =
    G.fold_vertex
      (fun v acc -> if G.in_degree g v = 0 then v :: acc else acc)
      g
      []

  let max g =
    G.fold_vertex
      (fun v acc -> if G.out_degree g v = 0 then v :: acc else acc)
      g
      []

  let vertex_attributes = ref (fun _ -> [])

  let edge_attributes = ref (fun _ -> [])

  let graph_name = ref None

  module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes k = !edge_attributes k

    let default_edge_attributes _ = []

    let vertex_name k =
      let str t v = "\"" ^ Type.to_string t v ^ "\"" in
      match k with
      | `Node n -> str Node_key.t n
      | `Commit c -> str Commit_key.t c
      | `Contents c -> str Contents_key.t c
      | `Branch b -> str Branch.t b

    let vertex_attributes k = !vertex_attributes k

    let default_vertex_attributes _ = []

    let get_subgraph _ = None

    let graph_attributes _ =
      match !graph_name with None -> [] | Some n -> [`Label n]
  end)

  let export t = (vertex t, edges t)

  let import (vs, es) =
    let g = G.create ~size:(List.length vs) () in
    List.iter (G.add_vertex g) vs ;
    List.iter (fun (v1, v2) -> G.add_edge g v1 v2) es ;
    g

  let output ppf vertex edges name =
    Events.(emit__dont_wait__use_with_care output) name ;
    let g = G.create ~size:(List.length vertex) () in
    List.iter (fun (v, _) -> G.add_vertex g v) vertex ;
    List.iter (fun (v1, _, v2) -> G.add_edge g v1 v2) edges ;
    let eattrs (v1, v2) =
      try
        let l = List.filter (fun (x, _, y) -> x = v1 && y = v2) edges in
        let l = List.fold_left (fun acc (_, l, _) -> l @ acc) [] l in
        let labels, others =
          list_partition_map (function `Label l -> `Fst l | x -> `Snd x) l
        in
        match labels with
        | [] -> others
        | [l] -> `Label l :: others
        | _ -> `Label (String.concat "," labels) :: others
      with Not_found -> []
    in
    let vattrs v = try List.assoc v vertex with Not_found -> [] in
    vertex_attributes := vattrs ;
    edge_attributes := eattrs ;
    graph_name := Some name ;
    Dot.fprint_graph ppf g
end
