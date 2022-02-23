(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** This module defines identifiers for two basic types of contracts. It also
    specifies how to compute originated contract's hash from origination
    nonce. *)

(** A contract is simply an account on the blockchain ledger. There are two
    types of contracts:
    - implicit contracts represent accounts of users of the blockchain;
    - originated are special accounts with a Michelson script attached to
    them. Every time a transaction is sent to an originated account, its
    associated script is run in order to trigger some action in response.

    An implicit account is identified by the hash of the public key which was
    used to create it. The owner of the corresponding private key is the
    holder of the account. An originated contract's hash is derived from its
    origination nonce (see below). *)
type t = private
  | Implicit of Signature.Public_key_hash.t
  | Originated of Contract_hash.t

type contract = t

include Compare.S with type t := contract

val public_key_hash_in_memory_size : Cache_memory_helpers.sint

val in_memory_size : t -> Cache_memory_helpers.sint

(** {2 Implicit contracts} *)

val implicit_contract : Signature.Public_key_hash.t -> contract

val is_implicit : contract -> Signature.Public_key_hash.t option

(** {2 Originated contracts} *)

(** [originated_contract nonce] is the contract address originated from [nonce].
*)
val originated_contract : Origination_nonce.t -> contract

(** [originated_contracts ~since ~until] is the contract addresses originated
    from [since] until [until]. The operation hash of nonce [since] and [until]
    must be the same or it will fail with an [assert]. [since] < [until] or the
    returned list is empty *)
val originated_contracts :
  since:Origination_nonce.t -> until:Origination_nonce.t -> contract list

val is_originated : contract -> Contract_hash.t option

(** {2 Human readable notation} *)

type error += Invalid_contract_notation of string (* `Permanent *)

val to_b58check : contract -> string

val of_b58check : string -> contract tzresult

val pp : Format.formatter -> contract -> unit

val pp_short : Format.formatter -> contract -> unit

(** {2 Serializers} *)

val encoding : contract Data_encoding.t

val rpc_arg : contract RPC_arg.arg

module Index : Storage_description.INDEX with type t = t
