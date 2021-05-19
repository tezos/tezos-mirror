(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type location = Micheline.canonical_location

let location_encoding = Micheline.canonical_location_encoding

type annot = Micheline.annot

type expr = Michelson_v1_primitives.prim Micheline.canonical

type lazy_expr = expr Data_encoding.lazy_t

type node = (location, Michelson_v1_primitives.prim) Micheline.node

let expr_encoding =
  Micheline.canonical_encoding_v1
    ~variant:"michelson_v1"
    Michelson_v1_primitives.prim_encoding

type error += Lazy_script_decode (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"invalid_binary_format"
    ~title:"Invalid binary format"
    ~description:
      "Could not deserialize some piece of data from its binary representation"
    Data_encoding.empty
    (function Lazy_script_decode -> Some () | _ -> None)
    (fun () -> Lazy_script_decode)

let lazy_expr_encoding = Data_encoding.lazy_encoding expr_encoding

let lazy_expr expr = Data_encoding.make_lazy expr_encoding expr

type t = {code : lazy_expr; storage : lazy_expr}

let encoding =
  let open Data_encoding in
  def "scripted.contracts"
  @@ conv
       (fun {code; storage} -> (code, storage))
       (fun (code, storage) -> {code; storage})
       (obj2 (req "code" lazy_expr_encoding) (req "storage" lazy_expr_encoding))

module S = Saturation_repr

module Micheline_size = struct
  type t = {
    nodes : S.may_saturate S.t;
    string_bytes : S.may_saturate S.t;
    z_bytes : S.may_saturate S.t;
  }

  let make ~nodes ~string_bytes ~z_bytes = {nodes; string_bytes; z_bytes}

  let zero = {nodes = S.zero; string_bytes = S.zero; z_bytes = S.zero}

  let add s1 s2 =
    {
      nodes = S.add s1.nodes s2.nodes;
      string_bytes = S.add s1.string_bytes s2.string_bytes;
      z_bytes = S.add s1.z_bytes s2.z_bytes;
    }

  let of_int n =
    let numbits = Z.numbits n in
    let z_bytes =
      S.safe_int ((numbits + 7) / 8)
      (* Compute the number of bytes in a Z.t *)
    in
    {nodes = S.safe_int 1; string_bytes = S.zero; z_bytes}

  let of_string n =
    let string_bytes = S.safe_int (String.length n) in
    {nodes = S.safe_int 1; string_bytes; z_bytes = S.zero}

  let of_bytes n =
    let string_bytes = S.safe_int (Bytes.length n) in
    {nodes = S.safe_int 1; string_bytes; z_bytes = S.zero}

  (* We model annotations as Seqs of Strings *)
  let of_annots acc annots =
    List.fold_left (fun acc s -> add acc (of_string s)) acc annots

  let rec of_nodes acc nodes more_nodes =
    let open Micheline in
    match nodes with
    | [] -> (
      match more_nodes with
      | [] ->
          acc
      | nodes :: more_nodes ->
          (of_nodes [@ocaml.tailcall]) acc nodes more_nodes )
    | Int (_, n) :: nodes ->
        (of_nodes [@ocaml.tailcall]) (add acc (of_int n)) nodes more_nodes
    | String (_, s) :: nodes ->
        (of_nodes [@ocaml.tailcall]) (add acc (of_string s)) nodes more_nodes
    | Bytes (_, s) :: nodes ->
        (of_nodes [@ocaml.tailcall]) (add acc (of_bytes s)) nodes more_nodes
    | Prim (_, _, args, annots) :: nodes ->
        let acc = of_annots acc annots in
        (of_nodes [@ocaml.tailcall]) acc args (nodes :: more_nodes)
    | Seq (_, args) :: nodes ->
        (of_nodes [@ocaml.tailcall]) acc args (nodes :: more_nodes)

  let of_node node = of_nodes zero [node] []

  let dot_product s1 s2 =
    S.add
      (S.mul s1.nodes s2.nodes)
      (S.add
         (S.mul s1.string_bytes s2.string_bytes)
         (S.mul s1.z_bytes s2.z_bytes))
end

(* Costs pertaining to deserialization of Micheline values (bytes to Micheline).
   The costs are given in atomic steps (see [Gas_limit_repr]). *)
module Micheline_decoding = struct
  (* Cost vector allowing to compute decoding costs as a function of the
     size of the Micheline term *)
  let micheline_size_dependent_cost =
    let traversal_cost = S.safe_int 60 in
    let string__per_byte_cost = S.safe_int 10 in
    let z__per_byte_cost = S.safe_int 10 in
    Micheline_size.make
      ~nodes:traversal_cost
      ~string_bytes:string__per_byte_cost
      ~z_bytes:z__per_byte_cost

  let bytes_dependent_cost = S.safe_int 20
end

(* Costs pertaining to serialization of Micheline values (Micheline to bytes)
   The costs are given in atomic steps (see [Gas_limit_repr]). *)
module Micheline_encoding = struct
  (* Cost vector allowing to compute encoding cost as a function of the
     size of the Micheline term *)
  let micheline_size_dependent_cost =
    let traversal_cost = S.safe_int 100 in
    let string__per_byte_cost = S.safe_int 10 in
    let z__per_byte_cost = S.safe_int 25 in
    Micheline_size.make
      ~nodes:traversal_cost
      ~string_bytes:string__per_byte_cost
      ~z_bytes:z__per_byte_cost

  let bytes_dependent_cost = S.safe_int 33
end

let expr_size expr = Micheline_size.of_node (Micheline.root expr)

(* Compute the cost of serializing a term of given [size]. *)
let serialization_cost size =
  Gas_limit_repr.atomic_step_cost
  @@ Micheline_size.dot_product
       size
       Micheline_encoding.micheline_size_dependent_cost

(* Compute the cost of deserializing a term of given [size]. *)
let deserialization_cost size =
  Gas_limit_repr.atomic_step_cost
  @@ Micheline_size.dot_product
       size
       Micheline_decoding.micheline_size_dependent_cost

(* Estimate the cost of deserializing a term encoded in [bytes_len] bytes. *)
let deserialization_cost_estimated_from_bytes bytes_len =
  Gas_limit_repr.atomic_step_cost
  @@ S.mul Micheline_decoding.bytes_dependent_cost (S.safe_int bytes_len)

(* Estimate the cost of serializing a term from its encoded form,
   having [bytes_len] bytes. *)
let serialization_cost_estimated_from_bytes bytes_len =
  Gas_limit_repr.atomic_step_cost
  @@ S.mul Micheline_encoding.bytes_dependent_cost (S.safe_int bytes_len)

(* Cost of running [strip_locations] on a term with [size] nodes.
   Note that [strip_locations] will reallocate a fresh Micheline tree.
   This only depends on the total number of nodes (not the size of
   the leaves). *)
let cost_micheline_strip_locations size =
  Gas_limit_repr.atomic_step_cost @@ S.mul (S.safe_int size) (S.safe_int 51)

(* This is currently used to estimate the cost of serializing an operation. *)
let bytes_node_cost s =
  serialization_cost_estimated_from_bytes (Bytes.length s)

let deserialized_cost expr =
  Gas_limit_repr.atomic_step_cost @@ deserialization_cost (expr_size expr)

let serialized_cost bytes =
  Gas_limit_repr.atomic_step_cost
  @@ serialization_cost_estimated_from_bytes (Bytes.length bytes)

let force_decode_cost lexpr =
  Data_encoding.apply_lazy
    ~fun_value:(fun _ -> Gas_limit_repr.free)
    ~fun_bytes:(fun b ->
      deserialization_cost_estimated_from_bytes (Bytes.length b))
    ~fun_combine:(fun _ _ -> Gas_limit_repr.free)
    lexpr

let force_decode lexpr =
  match Data_encoding.force_decode lexpr with
  | Some v ->
      ok v
  | None ->
      error Lazy_script_decode

let force_bytes_cost expr =
  (* Estimating the cost directly from the bytes would be cheaper, but
           using [serialized_cost] is more accurate. *)
  Data_encoding.apply_lazy
    ~fun_value:(fun v -> serialization_cost (expr_size v))
    ~fun_bytes:(fun _ -> Gas_limit_repr.free)
    ~fun_combine:(fun _ _ -> Gas_limit_repr.free)
    expr

let force_bytes expr =
  match Data_encoding.force_bytes expr with
  | bytes ->
      ok bytes
  | exception _ ->
      error Lazy_script_decode

let unit =
  Micheline.strip_locations (Prim (0, Michelson_v1_primitives.D_Unit, [], []))

let unit_parameter = lazy_expr unit

let is_unit_parameter =
  let unit_bytes = Data_encoding.force_bytes unit_parameter in
  Data_encoding.apply_lazy
    ~fun_value:(fun v ->
      match Micheline.root v with
      | Prim (_, Michelson_v1_primitives.D_Unit, [], []) ->
          true
      | _ ->
          false)
    ~fun_bytes:(fun b -> Compare.Bytes.equal b unit_bytes)
    ~fun_combine:(fun res _ -> res)

let rec strip_annotations node =
  let open Micheline in
  match node with
  | (Int (_, _) | String (_, _) | Bytes (_, _)) as leaf ->
      leaf
  | Prim (loc, name, args, _) ->
      Prim (loc, name, List.map strip_annotations args, [])
  | Seq (loc, args) ->
      Seq (loc, List.map strip_annotations args)

let rec micheline_nodes node acc k =
  match node with
  | Micheline.Int (_, _) ->
      k (acc + 1)
  | Micheline.String (_, _) ->
      k (acc + 1)
  | Micheline.Bytes (_, _) ->
      k (acc + 1)
  | Micheline.Prim (_, _, subterms, _) ->
      micheline_nodes_list subterms (acc + 1) k
  | Micheline.Seq (_, subterms) ->
      micheline_nodes_list subterms (acc + 1) k

and micheline_nodes_list subterms acc k =
  match subterms with
  | [] ->
      k acc
  | n :: nodes ->
      micheline_nodes_list nodes acc (fun acc -> micheline_nodes n acc k)

let micheline_nodes node = micheline_nodes node 0 (fun x -> x)

let strip_locations_cost node =
  let nodes = micheline_nodes node in
  cost_micheline_strip_locations nodes
