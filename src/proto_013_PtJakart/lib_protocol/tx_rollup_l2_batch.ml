(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

open Tx_rollup_l2_context_sig

let tag_size = `Uint8

let bls_pk_encoding =
  let open Data_encoding in
  conv_with_guard
    Bls_signature.pk_to_bytes
    (fun x ->
      match Bls_signature.pk_of_bytes_opt x with
      | Some x -> ok x
      | None -> Error "not a BLS public key")
    (Fixed.bytes Bls_signature.pk_size_in_bytes)

type signer = Bls_pk of Bls_signature.pk | L2_addr of Tx_rollup_l2_address.t

module Signer_indexable = Indexable.Make (struct
  type t = signer

  let pp fmt = function
    | Bls_pk _ -> Format.pp_print_string fmt "<bls_signature>"
    | L2_addr addr -> Tx_rollup_l2_address.pp fmt addr

  let compare x y =
    match (x, y) with
    | (Bls_pk pk1, Bls_pk pk2) ->
        Bytes.compare
          (Bls_signature.pk_to_bytes pk1)
          (Bls_signature.pk_to_bytes pk2)
    | (L2_addr addr1, L2_addr addr2) -> Tx_rollup_l2_address.compare addr1 addr2
    | (L2_addr _, Bls_pk _) -> -1
    | (Bls_pk _, L2_addr _) -> 1

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"bls_pk"
          (Tag 0)
          bls_pk_encoding
          (function Bls_pk pk -> Some pk | _ -> None)
          (fun pk -> Bls_pk pk);
        case
          ~title:"l2_addr"
          (Tag 1)
          Tx_rollup_l2_address.encoding
          (function L2_addr addr -> Some addr | _ -> None)
          (fun addr -> L2_addr addr);
      ]
end)

module V1 = struct
  type 'status operation_content =
    | Withdraw of {
        destination : Signature.Public_key_hash.t;
        ticket_hash : Alpha_context.Ticket_hash.t;
        qty : Tx_rollup_l2_qty.t;
      }
    | Transfer of {
        destination : 'status Tx_rollup_l2_address.Indexable.t;
        ticket_hash : 'status Ticket_indexable.t;
        qty : Tx_rollup_l2_qty.t;
      }

  type ('signer, 'content) operation = {
    signer : 'signer Signer_indexable.t;
    counter : int64;
    contents : 'content operation_content list;
  }

  type ('signer, 'content) transaction = ('signer, 'content) operation list

  type signature = Bls_signature.signature

  type ('signer, 'content) t = {
    contents : ('signer, 'content) transaction list;
    aggregated_signature : signature;
  }

  (* --- ENCODING ------------------------------------------------------------- *)

  (* --- [operation_content]                                                    *)

  let compact_operation_content =
    let open Data_encoding.Compact in
    union
      [
        case
          ~title:"withdraw"
          (obj3
             (req "destination" (payload Signature.Public_key_hash.encoding))
             (req "ticket_hash" (payload Alpha_context.Ticket_hash.encoding))
             (req "qty" Tx_rollup_l2_qty.compact_encoding))
          (function
            | Withdraw {destination; ticket_hash; qty} ->
                Some (destination, ticket_hash, qty)
            | _ -> None)
          (fun (destination, ticket_hash, qty) ->
            Withdraw {destination; ticket_hash; qty});
        case
          ~title:"transfer"
          (obj3
             (req
                "destination"
                (Indexable.compact Tx_rollup_l2_address.encoding))
             (req "ticket_hash" Ticket_indexable.compact)
             (req "qty" Tx_rollup_l2_qty.compact_encoding))
          (function
            | Transfer {destination; ticket_hash; qty} ->
                Some (destination, ticket_hash, qty)
            | _ -> None)
          (fun (destination, ticket_hash, qty) ->
            Transfer {destination; ticket_hash; qty});
      ]

  let operation_content_encoding =
    Data_encoding.Compact.make ~tag_size compact_operation_content

  let compact_operation encoding_signer =
    Data_encoding.Compact.(
      conv
        (fun {signer; counter; contents} -> (signer, counter, contents))
        (fun (signer, counter, contents) -> {signer; counter; contents})
      @@ obj3
           (req "signer" encoding_signer)
           (req "counter" int64)
           (req "contents" @@ list ~bits:4 operation_content_encoding))

  let operation_encoding encoding_signer =
    Data_encoding.Compact.(make ~tag_size (compact_operation encoding_signer))

  let compact_transaction encoding_signer =
    Data_encoding.Compact.list ~bits:8 (operation_encoding encoding_signer)

  let transaction_encoding :
      'a -> ('b, Indexable.unknown) transaction Data_encoding.t =
   fun encoding_signer ->
    Data_encoding.Compact.(make ~tag_size (compact_transaction encoding_signer))

  let compact_signer_index =
    Data_encoding.Compact.(conv Indexable.to_int32 Indexable.index_exn int32)

  let compact_signer_either = Signer_indexable.compact

  let compact_operation = compact_operation compact_signer_either

  let compact_transaction_signer_index =
    compact_transaction compact_signer_index

  let compact_transaction = compact_transaction compact_signer_either

  let transaction_encoding = transaction_encoding compact_signer_either

  let compact ~bits :
      (Indexable.unknown, Indexable.unknown) t Data_encoding.Compact.t =
    Data_encoding.Compact.(
      conv
        (fun {aggregated_signature; contents} ->
          (aggregated_signature, contents))
        (fun (aggregated_signature, contents) ->
          {aggregated_signature; contents})
      @@ obj2
           (req "aggregated_signature"
           @@ payload Tx_rollup_l2_context_sig.signature_encoding)
           (req "contents" @@ list ~bits transaction_encoding))
end

type ('signer, 'content) t = V1 of ('signer, 'content) V1.t

(** We use two bits for the versioning of the layer-2 batches, which
    leaves six bits in the shared tag of compact encoding. We use
    these six bits to efficiently encode small lists.

    To ensure backward compatibility, the value of the label
    [tag_bits] cannot be modified. To have more than 3 versions of the
    encoding, one would have to use the fourth case to wrap a new
    union.

{[
   union
     ~tag_bits:2
     ~inner_bits:6
     [
       case "V1" ...;
       case "V2" ...;
       case "V3" ...;
       case "V_next" ...
              (union [ case "V4" ... ; ... ]);
     ]
]} *)
let compact =
  Data_encoding.Compact.(
    union
      ~union_tag_bits:2
      ~cases_tag_bits:6
      [
        case
          ~title:"V1"
          (V1.compact ~bits:6)
          (function V1 x -> Some x)
          (fun x -> V1 x);
      ])

(** An encoding for [t] that uses a specialized, space-efficient encoding
    for the list of transactions. *)
let encoding : (Indexable.unknown, Indexable.unknown) t Data_encoding.t =
  Data_encoding.Compact.make ~tag_size compact
