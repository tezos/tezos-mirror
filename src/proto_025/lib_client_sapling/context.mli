(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2019-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

(**
   This module allows the creation of Sapling transactions: shield, unshield and
   transfer.
   Because Sapling uses an UTXO model, it is necessary for the client to
   maintain locally the set of unspent outputs for each viewing key, for each
   smart contract. This operation is called scanning.
   This local cache is updated downloading from the node only the difference
   from the last scanned state.
*)

open Tezos_sapling.Core.Client

module Tez : module type of Protocol.Alpha_context.Tez

(** This module is used to represent any shielded token to avoid confusing it
    with Tez. *)
module Shielded_tez : sig
  type t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val zero : t

  val of_mutez : int64 -> t option

  val to_mutez : t -> int64

  val of_tez : Tez.t -> t

  val ( +? ) : t -> t -> t tzresult
end

(** Convert a Sapling transaction to a suitable argument for the Smart Contract. *)
val sapling_transaction_as_arg : UTXO.transaction -> string

(** Account corresponding to a contract and a viewing key *)
module Account : sig
  type t

  val balance : t -> Shielded_tez.t

  val pp_unspent : Format.formatter -> t -> unit
end

(** State of a contract, potentially involving several viewing keys *)
module Contract_state : sig
  type t

  val find_account : Viewing_key.t -> t -> Account.t option
end

module Client_state : sig
  type t

  val find :
    Protocol_client_context.full ->
    Protocol.Contract_hash.t ->
    t ->
    Contract_state.t tzresult Lwt.t

  val register :
    Protocol_client_context.full ->
    force:bool ->
    default_memo_size:int option ->
    Protocol.Contract_hash.t ->
    Viewing_key.t ->
    unit tzresult Lwt.t

  (** Synchronise our local state with the blockchain's.
      The state must be recent enough to craft correct transactions.
      The limit enforced by the protocol if 120 blocks.
      Also scans, ie. checks for incoming payments and add
      them to our balance.
   **)
  val sync_and_scan :
    Protocol_client_context.full ->
    Protocol.Contract_hash.t ->
    Contract_state.t tzresult Lwt.t
end

(** [shield ~message ~dst tez cstate anti-replay] returns a transaction
    shielding [tez] tez to a sapling address [dst] using a sapling
    storage [cstate] and the anti-replay string. *)
val shield :
  #Client_context.full ->
  dst:Viewing_key.address ->
  ?message:bytes ->
  Tez.t ->
  Contract_state.t ->
  string ->
  UTXO.transaction tzresult Lwt.t

(** [unshield ~src_name ~src ~dst ~backdst stez cstate storage] returns
    a transaction unshielding [stez] shielded tokens from a sapling wallet
    [src] to a transparent tezos address [dst], sending the change back to
    [backdst] and using a Sapling storage [cstate] and a anti-replay string.
    The transaction is refused if there is an insufficient amount of shielded
    tez in the wallet [src], the error is raised with [src_name].
   *)
val unshield :
  src:Spending_key.t ->
  bound_data:string ->
  backdst:Viewing_key.address ->
  Shielded_tez.t ->
  Contract_state.t ->
  string ->
  UTXO.transaction tzresult

(** [transfer ~message ~src ~dst ~backdst amount cstate anti-replay] creates a
    Sapling transaction of [amount] shielded tez from Sapling wallet [src] to
    Sapling address [dst], sending the change to [backdst], using a Sapling
    storage [cstate] and a anti-replay string.
    [~message] is a message that will be uploaded encrypted on chain. *)
val transfer :
  #Client_context.full ->
  src:Spending_key.t ->
  dst:Viewing_key.address ->
  backdst:Viewing_key.address ->
  ?message:bytes ->
  Shielded_tez.t ->
  Contract_state.t ->
  string ->
  UTXO.transaction tzresult Lwt.t
