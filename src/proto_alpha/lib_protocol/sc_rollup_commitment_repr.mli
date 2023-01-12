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
      {li the PVM consumes all the messages until [inbox_level] (not included)
          from the inbox ; }
      {li the PVM advances to the state [compressed_state] over
          [number_of_ticks] ticks. }
    }

    Commitments are disjoint. The next correct commitment is a function of the
    previous machine state and Inbox.

    [compressed_state] and [number_of_ticks] can be proven/disproven by PVM
    execution, or equivalently, by an interactive proof game between
    conflicting parties, such that a correct executor always wins the game.
*)
module V1 : sig
  type t = {
    compressed_state : State_hash.t;
    inbox_level : Raw_level_repr.t;
    predecessor : Hash.t;
    number_of_ticks : Number_of_ticks.t;
  }

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val hash_uncarbonated : t -> Hash.t

  (** [genesis_commitment ~origination_level ~genesis_state_hash] is the
      commitment that the protocol "publish" and "cement" when originating a new
      rollup. Each rollup have a different [genesis_commitment] because the
      [compressed_state] is computed after the boot sector is set. It has the
      following values:

      {ul {li [compressed_state] = [genesis_state_hash]}
          {li [inbox_level] = [origination_level]}
          {li [predecessor] = {!Hash.zero}}
          {li [number_of_ticks] = {!Sc_rollup_repr.Number_of_ticks.min_value}}}

      where {!Sc_rollup_repr.Number_of_messages.min_value} and
      {!Sc_rollup_repr.Number_of_ticks.min_value} are equal to [zero].

      See {!Sc_rollup_storage.originate} for the usage. *)
  val genesis_commitment :
    origination_level:Raw_level_repr.t ->
    genesis_state_hash:Sc_rollup_repr.State_hash.t ->
    t

  (** The genesis of a rollup is characterized by the Tezos level of
      the rollup origination, and the hash of the commitment computed
      by the protocol to specialize the PVM initial state with the
      provided boot sector. *)
  type genesis_info = {level : Raw_level_repr.t; commitment_hash : Hash.t}

  val genesis_info_encoding : genesis_info Data_encoding.t
end

(** Versioning, see {!Sc_rollup_data_version_sig.S} for more information. *)
include Sc_rollup_data_version_sig.S with type t = V1.t

include module type of V1 with type t = V1.t
