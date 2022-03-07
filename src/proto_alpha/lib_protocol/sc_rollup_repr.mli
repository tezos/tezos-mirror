(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** The basic components of an optimistic rollup for smart-contracts. *)

(**

   An optimistic rollup for smart-contracts is made of two main
   components:

   - a proof generating virtual machine (PVM), which provides the
   essential semantics for the rollup operations to be validated by
   the layer 1 in case of dispute about a commitment ;

   - a database which maintains the cemented operations of the rollup
   as well as the potentially-disputed operations.

*)
module PVM : sig
  (** A PVM instance can be initialized by setting a boot sector. *)
  type boot_sector

  val boot_sector_encoding : boot_sector Data_encoding.t

  val boot_sector_of_string : string -> boot_sector

  val boot_sector_to_string : boot_sector -> string
end

(** A smart-contract rollup has an address starting with "scr1". *)
module Address : sig
  include S.HASH

  (** [from_nonce nonce] produces an address completely determined by
     an operation hash and an origination counter. *)
  val from_nonce : Origination_nonce.t -> t tzresult

  (** [encoded_size] is the number of bytes needed to represent an address. *)
  val encoded_size : int
end

module Commitment_hash : S.HASH

module State_hash : S.HASH

(** Number of messages consumed by a single commitment. This represents a claim
    about the shape of the Inbox, which can be disputed as part of a commitment
    dispute.

    See also {!Sc_rollup_repr.Commitments}. *)
module Number_of_messages : Bounded.Int32.S

(** Number of ticks computed by a single commitment. This represents a claim
    about the state of the PVM, which can be disputed as part of a commitment
    dispute.

    See also {!Sc_rollup_repr.Commitments}. *)
module Number_of_ticks : Bounded.Int32.S

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
module Commitment : sig
  type t = {
    compressed_state : State_hash.t;
    inbox_level : Raw_level_repr.t;
    predecessor : Commitment_hash.t;
    number_of_messages : Number_of_messages.t;
    number_of_ticks : Number_of_ticks.t;
  }

  val encoding : t Data_encoding.t

  val hash : t -> Commitment_hash.t
end

(** A smart contract rollup is identified by its address. *)
type t = Address.t

(** A [Staker] is an implicit account, identified by its public key hash. *)
module Staker :
  S.SIGNATURE_PUBLIC_KEY_HASH with type t = Signature.Public_key_hash.t

val encoding : t Data_encoding.t

val rpc_arg : t RPC_arg.t

val pp : Format.formatter -> t -> unit

(** The data model uses an index of these addresses. *)
module Index : Storage_description.INDEX with type t = Address.t

module Commitment_hash_index :
  Storage_description.INDEX with type t = Commitment_hash.t

(** A smart contract rollup has a kind, which assigns meaning to
   rollup operations. *)
module Kind : sig
  (**

     The list of available rollup kinds.

     This list must only be appended for backward compatibility.
  *)
  type t = Example_arith

  val encoding : t Data_encoding.t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit
end
