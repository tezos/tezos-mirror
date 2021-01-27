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

let int_node_size_of_numbits n = (1, 1 + ((n + 63) / 64))

let int_node_size n = int_node_size_of_numbits (Z.numbits n)

let string_node_size_of_length s = (1, 1 + ((s + 7) / 8))

let string_node_size s = string_node_size_of_length (String.length s)

let bytes_node_size_of_length s =
  (* approx cost of indirection to the C heap *)
  (2, 1 + ((s + 7) / 8) + 12)

let bytes_node_size s = bytes_node_size_of_length (Bytes.length s)

let prim_node_size_nonrec_of_lengths n_args annots =
  let annots_length =
    List.fold_left (fun acc s -> acc + String.length s) 0 annots
  in
  if Compare.Int.(annots_length = 0) then (1 + n_args, 2 + (2 * n_args))
  else (2 + n_args, 4 + (2 * n_args) + ((annots_length + 7) / 8))

let prim_node_size_nonrec args annots =
  let n_args = List.length args in
  prim_node_size_nonrec_of_lengths n_args annots

let seq_node_size_nonrec_of_length n_args = (1 + n_args, 2 + (2 * n_args))

let seq_node_size_nonrec args =
  let n_args = List.length args in
  seq_node_size_nonrec_of_length n_args

module S = Saturation_repr

let convert_pair (i1, i2) = (S.safe_int i1, S.safe_int i2)

let rec node_size node =
  let open Micheline in
  match node with
  | Int (_, n) ->
      convert_pair (int_node_size n)
  | String (_, s) ->
      convert_pair (string_node_size s)
  | Bytes (_, s) ->
      convert_pair (bytes_node_size s)
  | Prim (_, _, args, annot) ->
      List.fold_left
        (fun (blocks, words) node ->
          let (nblocks, nwords) = node_size node in
          (S.add blocks nblocks, S.add words nwords))
        (convert_pair (prim_node_size_nonrec args annot))
        args
  | Seq (_, args) ->
      List.fold_left
        (fun (blocks, words) node ->
          let (nblocks, nwords) = node_size node in
          (S.add blocks nblocks, S.add words nwords))
        (convert_pair (seq_node_size_nonrec args))
        args

let expr_size expr = node_size (Micheline.root expr)

let traversal_cost node =
  let (blocks, _words) = node_size node in
  Gas_limit_repr.step_cost blocks

let cost_of_size (blocks, words) =
  let open Gas_limit_repr in
  (S.sub blocks (S.safe_int 1) *@ alloc_cost S.zero)
  +@ alloc_cost words +@ step_cost blocks

let cost_of_size_int pair = cost_of_size (convert_pair pair)

let int_node_cost n = cost_of_size_int (int_node_size n)

let int_node_cost_of_numbits n = cost_of_size_int (int_node_size_of_numbits n)

let string_node_cost s = cost_of_size_int (string_node_size s)

let string_node_cost_of_length s =
  cost_of_size_int (string_node_size_of_length s)

let bytes_node_cost s = cost_of_size_int (bytes_node_size s)

let bytes_node_cost_of_length s =
  cost_of_size_int (bytes_node_size_of_length s)

let prim_node_cost_nonrec args annot =
  cost_of_size_int (prim_node_size_nonrec args annot)

let seq_node_cost_nonrec args = cost_of_size_int (seq_node_size_nonrec args)

let seq_node_cost_nonrec_of_length n_args =
  cost_of_size_int (seq_node_size_nonrec_of_length n_args)

let deserialized_cost expr = cost_of_size (expr_size expr)

let serialized_cost bytes =
  let open Gas_limit_repr in
  alloc_bytes_cost (Bytes.length bytes)

let force_decode lexpr =
  let account_deserialization_cost =
    Data_encoding.apply_lazy
      ~fun_value:(fun _ -> false)
      ~fun_bytes:(fun _ -> true)
      ~fun_combine:(fun _ _ -> false)
      lexpr
  in
  match Data_encoding.force_decode lexpr with
  | Some v ->
      if account_deserialization_cost then ok (v, deserialized_cost v)
      else ok (v, Gas_limit_repr.free)
  | None ->
      error Lazy_script_decode

let force_bytes expr =
  let open Gas_limit_repr in
  let account_serialization_cost =
    Data_encoding.apply_lazy
      ~fun_value:(fun v -> Some v)
      ~fun_bytes:(fun _ -> None)
      ~fun_combine:(fun _ _ -> None)
      expr
  in
  match Data_encoding.force_bytes expr with
  | bytes -> (
    match account_serialization_cost with
    | Some v ->
        ok (bytes, traversal_cost (Micheline.root v) +@ serialized_cost bytes)
    | None ->
        ok (bytes, Gas_limit_repr.free) )
  | exception _ ->
      error Lazy_script_decode

let minimal_deserialize_cost lexpr =
  Data_encoding.apply_lazy
    ~fun_value:(fun _ -> Gas_limit_repr.free)
    ~fun_bytes:(fun b -> serialized_cost b)
    ~fun_combine:(fun c_free _ -> c_free)
    lexpr

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

let cost_MICHELINE_STRIP_LOCATIONS size =
  S.mul (S.safe_int size) (S.safe_int 100)

let strip_locations_cost node =
  let nodes = micheline_nodes node in
  Gas_limit_repr.atomic_step_cost (cost_MICHELINE_STRIP_LOCATIONS nodes)
