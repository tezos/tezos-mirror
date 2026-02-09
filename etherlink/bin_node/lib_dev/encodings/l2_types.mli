(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

type chain_id = Chain_id of Z.t [@@unboxed]

module Chain_id : sig
  val encoding : chain_id Data_encoding.t

  (** [of_string_exn hex] transforms a string to a chain id.
  It raises an exception if the string is not an number
  (base 10, hexa, binary, octal ...) *)
  val of_string_exn : string -> chain_id

  val to_string : chain_id -> string

  val decode_le : bytes -> chain_id

  val decode_be : bytes -> chain_id

  val compare : chain_id -> chain_id -> int

  val pp : Format.formatter -> chain_id -> unit
end

(** Each L2 chain has a chain family wich is either [EVM] (for
    Etherlink) or [Michelson] (for Tezlink). We use GADTs to
    statically distinguish the parts of the codebase corresponding to
    each.

    The types [evm_chain_family] and [michelson_chain_family] are only
    used to tag other types. The only thing that matters is that these
    two types are considered different by the OCaml type checker.
*)

type evm_chain_family = Evm_chain_family

type michelson_chain_family = Michelson_chain_family

type 'f chain_family =
  | EVM : evm_chain_family chain_family
  | Michelson : michelson_chain_family chain_family

(** The type [ex_chain_family] is an
    {{:https://octez.tezos.com/docs/developer/gadt.html#building-complex-expressions}existential
    type} abstracting over the type parameter of [chain_family]. This
    is useful to type the result of functions returning a chain family
    because we don't know which one will be returned before calling
    the function (otherwise we would not need the function in the
    first place). So a typical pattern is to have a function fetching
    the chain family (for example
    [Configuration.retrieve_chain_family])) whose return type is
    [ex_chain_family] and each time we call it, we immeditalely unwrap
    the [Ex_chain_family] constructor to get a ['f chain_family] for an
    unknown ['f] instead:

    {[
      let (Ex_chain_family chain_family) =
        Configuration.retrieve_chain_family ~l2_chains
      in
    ]}

    In general, the rule of thumb is that functions {e returning} a
    chain family have type [... -> ex_chain_family] but functions {e
    using} a chain family have type ['f chain_family -> ...].
*)

type ex_chain_family = Ex_chain_family : _ chain_family -> ex_chain_family

module Chain_family : sig
  val encoding : ex_chain_family Data_encoding.t

  (** [of_string_exn s] returns the chain family corresponding to the string [s].
      The comparison is case-insensitive, so ["Evm"], ["evm"], ["EVM"], etc. are all valid.
      @raise Invalid_argument if [s] does not correspond to a recognized chain family.
  *)
  val of_string_exn : string -> ex_chain_family

  val to_string : _ chain_family -> string

  val pp : Format.formatter -> _ chain_family -> unit
end

module Tezos_block : sig
  type t = {
    hash : Ethereum_types.block_hash;
    level : int32;
    timestamp : Time.Protocol.t;
    parent_hash : Ethereum_types.block_hash;
    operations : bytes;
  }

  val decode_block_hash : bytes -> Ethereum_types.block_hash

  val genesis_parent_hash : Ethereum_types.block_hash

  val block_from_kernel : bytes -> t

  val encode_block_for_store : t -> (string, string) result

  val decode_block_for_store : string -> (t, string) result

  module Internal_for_test : sig
    module Legacy : sig
      type nonrec t = t

      val encode_block_for_store : t -> (string, string) result
    end
  end
end

type 'a block = Eth of 'a Ethereum_types.block | Tez of Tezos_block.t

val block_hash : 'a block -> Ethereum_types.block_hash

val block_number : 'a block -> Ethereum_types.quantity

val block_number_of_transactions : 'a block -> int

val block_parent : 'a block -> Ethereum_types.block_hash

val decode_block_hash :
  chain_family:_ chain_family -> bytes -> Ethereum_types.block_hash

val genesis_parent_hash :
  chain_family:_ chain_family -> Ethereum_types.block_hash

val block_from_bytes :
  chain_family:_ chain_family ->
  bytes ->
  Ethereum_types.legacy_transaction_object block
