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

module Publish_slot_header : sig
  (** A "publish slot header" operation contains

      - a [published_level] which is the block level this operation
     should be included to.
      
      - a [slot_index] which is the slot index associated with the
     commitment.

      - a [commitment] which is a commitment to the slot data published
     onto the DAL

      - a [commitment_proof] which aims to prove that the size 
     of the slot data does not exceed a limit set by the
     protocol. *)
  type t = {
    published_level : Raw_level_repr.t;
    slot_index : Dal_slot_index_repr.t;
    commitment : Dal_slot_repr.Commitment.t;
    commitment_proof : Dal_slot_repr.Commitment_proof.t;
  }

  val encoding : t Data_encoding.t
end
