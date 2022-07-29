(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Sc_rollup_repr

module Hash : sig
  include S.HASH

  include Storage_description.INDEX with type t := t
end

(** A commitment represents a claim about the state of the Inbox and PVM at
    some Inbox level.

    More formally, a commitment is a claim that:

    {ul
      {li assuming the PVM and Inbox are in a state implied by [predecessor]}
      {li the PVM consumes [number_of_messages] messages tagged with
      [inbox_level] from the Inbox}
      {li the PVM advances to the state [compressed_state] over
      [number_of_ticks] ticks }
    }

    Commitments are disjoint. The next correct commitment is a function of the
    previous machine state and Inbox.

    [number_of_messages] and [inbox_level] can be proven/disproven by Merkle
    proofs on the Inbox state.

    [compressed_state] and [number_of_ticks] can be proven/disproven by PVM
    execution, or equivalently, by an interactive proof game between
    conflicting parties, such that a correct executor always wins the game.
*)
module V1 : sig
  type t = {
    compressed_state : State_hash.t;
    inbox_level : Raw_level_repr.t;
    predecessor : Hash.t;
    number_of_messages : Number_of_messages.t;
    number_of_ticks : Number_of_ticks.t;
  }

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val hash : t -> Hash.t
end

(** Versioning, see {!Sc_rollup_data_version_sig.S} for more information. *)
include Sc_rollup_data_version_sig.S with type t = V1.t

include module type of V1 with type t = V1.t
