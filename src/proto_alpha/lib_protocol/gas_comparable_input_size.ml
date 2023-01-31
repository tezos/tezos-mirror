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

let add = ( + )

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

let to_int x = x

let of_int x = x

let unit : t = 1

let integer (i : 'a Script_int.num) : t = Z.numbits (Script_int.to_zint i) / 8

let string = String.length

let script_string = Script_string.length

let bytes (b : Bytes.t) : t = Bytes.length b

let mutez (_tez : Alpha_context.Tez.tez) : t =
  (* Up to now, mutez are stored on 8 bytes (int64). *)
  8

let bool (_ : bool) : t = 1

let signature (signature : Script_typed_ir.Script_signature.t) : t =
  Script_typed_ir.Script_signature.size signature

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
  Tx_rollup_l2_address.Indexable.size @@ Indexable.forget x

let timestamp (tstamp : Script_timestamp.t) : t =
  Z.numbits (Script_timestamp.to_zint tstamp) / 8

let rec size_of_comparable_value :
    type a. a Script_typed_ir.comparable_ty -> a -> t =
  fun (type a) (wit : a Script_typed_ir.comparable_ty) (v : a) ->
   match wit with
   | Never_t -> ( match v with _ -> .)
   | Unit_t -> unit
   | Int_t -> integer v
   | Nat_t -> integer v
   | String_t -> script_string v
   | Bytes_t -> bytes v
   | Mutez_t -> mutez v
   | Bool_t -> bool v
   | Key_hash_t -> key_hash v
   | Timestamp_t -> timestamp v
   | Address_t -> address v
   | Tx_rollup_l2_address_t -> tx_rollup_l2_address v
   | Pair_t (leaf, node, _, YesYes) ->
       let lv, rv = v in
       let size =
         size_of_comparable_value leaf lv + size_of_comparable_value node rv
       in
       size + 1
   | Or_t (left, right, _, YesYes) ->
       let size =
         match v with
         | L v -> size_of_comparable_value left v
         | R v -> size_of_comparable_value right v
       in
       size + 1
   | Option_t (ty, _, Yes) -> (
       match v with None -> 1 | Some x -> size_of_comparable_value ty x + 1)
   | Signature_t -> signature v
   | Key_t -> public_key v
   | Chain_id_t -> chain_id v
