(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

module Hash = Tezos_crypto.Hashed.Smart_rollup_commitment_hash

module V1 : sig
  (** Type of smart rollup commitments.
    See {!Protocol.Sc_rollup_commitment_repr.t}. *)
  type t = {
    compressed_state : State_hash.t;
    inbox_level : int32;
    predecessor : Hash.t;
    number_of_ticks : int64;
  }

  (** Pretty printing protocol agnostic commitments. *)
  val pp : Format.formatter -> t -> unit

  (** Encoding for commitments. *)
  val encoding : t Data_encoding.t

  (** Hashing a commitment. *)
  val hash : t -> Hash.t

  (** [genesis_commitment ~origination_level ~genesis_state_hash] is the
    commitment that is published when originating a new rollup.
    See {!Protocol.Sc_rollup_commitment_repr.genesis_commitment}. *)
  val genesis_commitment :
    origination_level:int32 -> genesis_state_hash:State_hash.t -> t
end

include Versioned_data.S with type t = V1.t

include module type of V1 with type t = V1.t
