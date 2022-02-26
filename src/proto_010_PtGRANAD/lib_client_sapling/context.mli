(* The MIT License (MIT)
 *
 * Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

(**
   This module allows the creation Sapling transactions: shield, unshield and
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

(** Actual input to a smart contract handling Sapling transactions *)
module Shielded_tez_contract_input : sig
  type t

  val create : ?pkh:Signature.Public_key_hash.t -> UTXO.transaction -> t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val as_arg : t -> string
end

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
    Protocol.Alpha_context.Contract.t ->
    t ->
    Contract_state.t tzresult Lwt.t

  val register :
    Protocol_client_context.full ->
    force:bool ->
    default_memo_size:int option ->
    Protocol.Alpha_context.Contract.t ->
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
    Protocol.Alpha_context.Contract.t ->
    Contract_state.t tzresult Lwt.t
end
