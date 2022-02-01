(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type t = int

type micheline_size = {traversal : t; int_bytes : t; string_bytes : t}

(* ------------------------------------------------------------------------- *)
(* encoding *)

let encoding : t Data_encoding.encoding =
  let open Data_encoding in
  conv (fun i -> Int64.of_int i) (fun l -> Int64.to_int l) int64

let micheline_size_encoding : micheline_size Data_encoding.encoding =
  let open Data_encoding in
  conv
    (fun {traversal; int_bytes; string_bytes} ->
      (traversal, int_bytes, string_bytes))
    (fun (traversal, int_bytes, string_bytes) ->
      {traversal; int_bytes; string_bytes})
    (tup3 encoding encoding encoding)

(* ------------------------------------------------------------------------- *)

let zero = 0

let one = 1

let add = ( + )

let sub = ( - )

let mul = ( * )

let div = ( / )

let max x y = if Compare.Int.(x < y) then y else x

let min x y = if Compare.Int.(x < y) then x else y

module Ops = struct
  let ( * ) = mul

  let ( / ) = div

  let ( + ) = add

  let ( - ) = sub
end

let compare = Compare.Int.compare

let equal = Compare.Int.( = )

let lt = Compare.Int.( < )

let leq = Compare.Int.( <= )

let pp = Format.pp_print_int

let pp_micheline_size fmtr {traversal; int_bytes; string_bytes} =
  Format.fprintf
    fmtr
    "@[{ traversal = %a;@; int_bytes = %a;@; string_bytes = %a;@,}@]"
    pp
    traversal
    pp
    int_bytes
    pp
    string_bytes

let show = string_of_int

let to_int x = x

let of_int x = x

let log2 x = Z.log2 (Z.of_int x)

let unit : t = 1

let integer (i : 'a Alpha_context.Script_int.num) : t =
  Z.numbits (Alpha_context.Script_int.to_zint i) / 8

let string = String.length

let script_string = Alpha_context.Script_string.length

let bytes (b : Bytes.t) : t = Bytes.length b

let mutez (_tez : Alpha_context.Tez.tez) : t =
  (* Up to now, mutez are stored on 8 bytes (int64). *)
  8

let bool (_ : bool) : t = 1

let signature (_signature : Script_typed_ir.Script_signature.t) : t =
  Script_typed_ir.Script_signature.size

let key_hash (_keyhash : Signature.public_key_hash) : t =
  Signature.Public_key_hash.size

let public_key (public_key : Signature.public_key) : t =
  Signature.Public_key.size public_key

let chain_id (_chain_id : Script_typed_ir.Script_chain_id.t) : t =
  Script_typed_ir.Script_chain_id.size

let address (addr : Script_typed_ir.address) : t =
  let entrypoint = addr.entrypoint in
  Signature.Public_key_hash.size
  + String.length (Alpha_context.Entrypoint.to_string entrypoint)

let tx_rollup_l2_address x =
  Tx_rollup_l2_address.Indexable.(size @@ forget_value x)

let list (list : 'a Script_typed_ir.boxed_list) : t =
  list.Script_typed_ir.length

let set (set : 'a Script_typed_ir.set) : t =
  let res = Alpha_context.Script_int.to_int (Script_set.size set) in
  match res with None -> assert false | Some x -> x

let map (map : ('a, 'b) Script_typed_ir.map) : t =
  let res = Alpha_context.Script_int.to_int (Script_map.size map) in
  match res with None -> assert false | Some x -> x

let timestamp (tstamp : Alpha_context.Script_timestamp.t) : t =
  Z.numbits (Alpha_context.Script_timestamp.to_zint tstamp) / 8

let rec size_of_comparable_value :
    type a. a Script_typed_ir.comparable_ty -> a -> t =
  fun (type a) (wit : a Script_typed_ir.comparable_ty) (v : a) ->
   match wit with
   | Never_key -> zero
   | Unit_key -> unit
   | Int_key -> integer v
   | Nat_key -> integer v
   | String_key -> script_string v
   | Bytes_key -> bytes v
   | Mutez_key -> mutez v
   | Bool_key -> bool v
   | Key_hash_key -> key_hash v
   | Timestamp_key -> timestamp v
   | Address_key -> address v
   | Tx_rollup_l2_address_key -> tx_rollup_l2_address v
   | Pair_key (leaf, node, _) ->
       let (lv, rv) = v in
       let size =
         add
           (size_of_comparable_value leaf lv)
           (size_of_comparable_value node rv)
       in
       add size one
   | Union_key (left, right, _) ->
       let size =
         match v with
         | L v -> size_of_comparable_value left v
         | R v -> size_of_comparable_value right v
       in
       add size one
   | Option_key (ty, _) -> (
       match v with
       | None -> one
       | Some x -> add (size_of_comparable_value ty x) one)
   | Signature_key -> signature v
   | Key_key -> public_key v
   | Chain_id_key -> chain_id v

(* ------------------------------------------------------------------------- *)
(* Micheline/Michelson-related *)

let micheline_zero = {traversal = 0; int_bytes = 0; string_bytes = 0}

let ( ++ ) x y =
  {
    traversal = Ops.(x.traversal + y.traversal);
    int_bytes = Ops.(x.int_bytes + y.int_bytes);
    string_bytes = Ops.(x.string_bytes + y.string_bytes);
  }

let node leaves =
  let r = List.fold_left ( ++ ) micheline_zero leaves in
  {r with traversal = Ops.(r.traversal + 1)}

let rec of_micheline (x : ('a, 'b) Micheline.node) =
  match x with
  | Micheline.Int (_loc, z) ->
      let int_bytes = integer (Alpha_context.Script_int.of_zint z) in
      {traversal = 1; int_bytes; string_bytes = 0}
  | Micheline.String (_loc, s) ->
      let string_bytes = String.length s in
      {traversal = 1; int_bytes = 0; string_bytes}
  | Micheline.Bytes (_loc, b) ->
      let string_bytes = bytes b in
      {traversal = 1; int_bytes = 0; string_bytes}
  | Micheline.Prim (_loc, _prim, subterms, _annot) ->
      node (List.map of_micheline subterms)
  | Micheline.Seq (_loc, subterms) -> node (List.map of_micheline subterms)

(* ------------------------------------------------------------------------- *)
(* Sapling-related *)

let sapling_transaction_inputs : Alpha_context.Sapling.transaction -> t =
 fun tx -> List.length tx.inputs

let sapling_transaction_outputs : Alpha_context.Sapling.transaction -> t =
 fun tx -> List.length tx.outputs
