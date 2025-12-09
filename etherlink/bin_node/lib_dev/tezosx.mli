(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Additional runtimes which can be enabled alongside the default Ethereum
    runtime in Tezos X. *)
type runtime = Tezos

val runtime_encoding : runtime Data_encoding.t

val runtime_of_string_opt : string -> runtime option

val string_of_runtime : runtime -> string

val pp_runtime : Format.formatter -> runtime -> unit

(** The list of known runtimes which can be enabled in Tezos X. *)
val known_runtimes : runtime list

(** [feature_flag runtime] is the path of the feature flag for [runtime].
    Creating a file at this path in the durable storage of the kernel will
    enable the feature. *)
val feature_flag : runtime -> string

module Ethereum_runtime : sig
  type address = Ethereum_types.address
end

module Tezos_runtime : sig
  type address = Signature.V2.public_key_hash

  type account_info = {
    balance : Tezos_types.Tez.t;
    nonce : int64;
    public_key : Signature.V2.public_key option;
  }

  val decode_account_info : bytes -> account_info tzresult

  val encode_account_info : account_info -> bytes

  val ethereum_alias : Signature.V2.public_key_hash -> Ethereum_types.address
end

module Foreign_address : sig
  type t =
    [`Tezos of Tezos_runtime.address | `Ethereum of Ethereum_types.address]

  val encode : t -> bytes
end

module Durable_storage_path : sig
  module Accounts : sig
    module Tezos : sig
      val info : Tezos_runtime.address -> Durable_storage_path.path

      val ethereum_alias : Tezos_runtime.address -> Durable_storage_path.path
    end
  end
end

type address =
  | Ethereum_address of Ethereum_runtime.address
  | Tezos_address of Tezos_runtime.address

val address_of_string : string -> address tzresult
