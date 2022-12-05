(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module provides different services related to DAL slots. *)

(* We cannot include a raw mli file. But this will be removed once full
   migration is done. *)
include module type of Services_legacy

open Tezos_crypto_dal

type 'rpc service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output) Tezos_rpc.Service.service
  constraint
    'rpc =
    < meth : 'meth
    ; prefix : 'prefix
    ; params : 'params
    ; query : 'query
    ; input : 'input
    ; output : 'output >

module Types : sig
  (** A Tezos level. *)
  type level = int32

  (** An index of a DAL slot header. *)
  type slot_index = int

  (** An ID associated to a slot or to its commitment. *)
  type slot_id = {slot_level : level; slot_index : slot_index}

  (** The status of a header successfully included in a block. The status can
      take one of the values below. *)
  type header_attestation_status =
    [ `Waiting_for_attestations
      (** The slot header was included and applied in an L1 block but remains to
          be attested. *)
    | `Attested
      (** The slot header was included in an L1 block and attested. *)
    | `Unattested
      (** The slot header was included in an L1 block but not timely attested. *)
    ]

  (** The status of a header a DAL node is aware of: *)
  type header_status =
    [ `Not_selected
      (** The slot header was included in an L1 block but was not selected as
          the slot header for that slot index. *)
    | `Unseen
      (** The slot header was never seen in an L1 block. For instance, this
          could happen if the RPC `PATCH /slots/<commitment>` was called but the
          corresponding slot header was never included into a block. *)
    | header_attestation_status ]
  (** The slot header was successfully included in a block. see
      {!header_attestation_status} for more details. *)

  val slot_id_encoding : slot_id Data_encoding.t

  (** Return the string representation for values of type
      {!header_attestation_status}. *)
  val header_attestation_status_to_string : header_attestation_status -> string

  (** Return the string representation for values of type {!header_status}. *)
  val header_status_to_string : header_status -> string
end

(** Add the given slot in the node if not already present. The corresponding
    commitment is returned. See {!val:
    Slot_manager.add_slot} for more details. *)
val post_slots :
  < meth : [`POST]
  ; input : Cryptobox.slot
  ; output : Cryptobox.commitment
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

(** Associate a commitment to a level and a slot index. See {!val:
    Slot_manager.add_slot_id} for more details. *)
val patch_slot :
  < meth : [`PATCH]
  ; input : Types.slot_id
  ; output : unit
  ; prefix : unit
  ; params : unit * Cryptobox.commitment
  ; query : unit >
  service

(** Retrieve the content of the slot associated with the given commitment. *)
val get_slot :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.slot
  ; prefix : unit
  ; params : unit * Cryptobox.commitment
  ; query : unit >
  service

(** Compute the proof associated to a commitment. *)
val get_slot_commitment_proof :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.commitment_proof
  ; prefix : unit
  ; params : unit * Cryptobox.commitment
  ; query : unit >
  service
