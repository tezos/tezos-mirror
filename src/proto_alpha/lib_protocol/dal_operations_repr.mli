(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** {1 DAL operations}

    This module gathers the datatypes for the payloads of DAL operations.

    This module ensures the consistency with the internal
   data-structures like [Dal_slot_index_repr.t]. *)

module Publish_commitment : sig
  (** A "publish slot header" operation contains

      - a [slot_index] which is the slot index associated with the
     commitment.

      - a [commitment] which is a commitment to the slot data published
     onto the DAL

      - a [commitment_proof] which aims to prove that the size
     of the slot data does not exceed a limit set by the
     protocol. *)
  type t = {
    slot_index : Dal_slot_index_repr.t;
    commitment : Dal_slot_repr.Commitment.t;
    commitment_proof : Dal_slot_repr.Commitment_proof.t;
  }

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  (** [slot_header ~cryptobox ~number_of_slots ~current_level
     operation] constructs a valid slot header. This function can fail
     in the following cases:

      - The [published_level] is not equal to [current_level]

      - The [commitment_proof] is invalid

      - The [slot_index] is invalid *)
  val slot_header :
    cryptobox:Dal.t ->
    number_of_slots:int ->
    current_level:Raw_level_repr.t ->
    t ->
    Dal_slot_repr.Header.t tzresult
end
