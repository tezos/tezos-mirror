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

(** A smart rollup has an address starting with "sr1". *)
module Address : sig
  include S.HASH

  (** [encoded_size] is the number of bytes needed to represent an address. *)
  val encoded_size : int

  val of_b58data : Base58.data -> t option

  (** [prefix] is the prefix of smart contract rollup addresses. *)
  val prefix : string
end

module Internal_for_tests : sig
  val originated_sc_rollup : Origination_nonce.t -> Address.t
end

module State_hash : sig
  include S.HASH

  (** [context_hash_to_state_hash ch] turns an (Irmin) context hash
      into a state hash. *)
  val context_hash_to_state_hash : Context_hash.t -> t

  (* Hackish way to disable hash_bytes and hash_string to force people to use
     context_hash_to_state_hash (without changing content of HASH.S) *)
  type unreachable = |

  val hash_bytes : unreachable -> t

  val hash_string : unreachable -> t
end

(** Number of ticks computed by a single commitment. This represents a claim
    about the state of the PVM, which can be disputed as part of a commitment
    dispute.

    See also {!Commitment_repr}. *)
module Number_of_ticks : sig
  include Bounded.S with type ocaml_type := int64

  val zero : t
end

(** A smart contract rollup is identified by its address. *)
type t = Address.t

val encoding : t Data_encoding.t

val rpc_arg : t RPC_arg.t

val pp : Format.formatter -> t -> unit

(** [in_memory_size sc_rollup] returns the number of bytes [sc_rollup]
    uses in RAM. *)
val in_memory_size : t -> Cache_memory_helpers.sint

(** A [Staker] is an implicit account, identified by its public key hash. *)
module Staker :
  S.SIGNATURE_PUBLIC_KEY_HASH with type t = Signature.Public_key_hash.t

(** The data model uses an index of these addresses. *)
module Index : Storage_description.INDEX with type t = Address.t
