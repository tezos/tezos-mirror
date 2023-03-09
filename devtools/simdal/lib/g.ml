include Graph.Persistent.Graph.Concrete (struct
  include Int

  let hash = Hashtbl.hash
end)

type vertex = V.t

module Edge = struct
  type t = vertex * vertex

  let canonical ((v1, v2) as e) = if v1 <= v2 then e else (v2, v1)

  let compare e1 e2 =
    let v1, v2 = canonical e1 in
    let v1', v2' = canonical e2 in
    let c = Int.compare v1 v1' in
    if c = 0 then Int.compare v2 v2' else c

  let pp fmtr (v1, v2) = Format.fprintf fmtr "[%d;%d]" v1 v2

  let equal e1 e2 =
    let v1, v2 = canonical e1 in
    let v1', v2' = canonical e2 in
    Int.equal v1 v1' && Int.equal v2 v2'

  let hash e = Hashtbl.hash (canonical e)
end

module Vertex_map = Map.Make (V)
module Vertex_table = Hashtbl.Make (V)
module Edge_set = Set.Make (Edge)
module Edge_table = Hashtbl.Make (Edge)
