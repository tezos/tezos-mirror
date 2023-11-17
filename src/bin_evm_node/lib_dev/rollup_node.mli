(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** List of services supported to communicate with a rollup node. *)
module type S = sig
  (** [balance address] returns the [address]'s balance. *)
  val balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t

  (** [nonce address] returns the [address]'s nonce. *)
  val nonce :
    Ethereum_types.address -> Ethereum_types.quantity option tzresult Lwt.t

  (** [code address] returns the [address]'s code. *)
  val code : Ethereum_types.address -> Ethereum_types.hex tzresult Lwt.t

  (** [inject_raw_transactions ~smart_rollup_address ~transactions] crafts the
      hashes and chunks of each transaction of [transactions]. Injects the chunks
      and returns the hashes of injected transactions. *)
  val inject_raw_transactions :
    smart_rollup_address:string ->
    transactions:Ethereum_types.hex list ->
    Ethereum_types.hash list tzresult Lwt.t

  (** [current_block ~full_transaction_object] returns the most recent
        processed and stored block.

      If [full_transaction_object] is [true], returns the transaction objects,
      the transactions hashes otherwise.
    *)
  val current_block :
    full_transaction_object:bool -> Ethereum_types.block tzresult Lwt.t

  (** [current_block_number ()]  returns the most recent processed and stored block
        number. *)
  val current_block_number : unit -> Ethereum_types.block_height tzresult Lwt.t

  (** [nth_block ~full_transaction_object n] returns the [n]th processed and
        stored block.

      If [full_transaction_object] is [true], returns the transaction objects,
      the transactions hashes otherwise.
    *)
  val nth_block :
    full_transaction_object:bool -> Z.t -> Ethereum_types.block tzresult Lwt.t

  (** [block_by_hash ~full_transaction_object hash] returns the block with the
      given [hash].

      If [full_transaction_object] is [true], returns the transaction objects,
      the transactions hashes otherwise.
    *)
  val block_by_hash :
    full_transaction_object:bool ->
    Ethereum_types.block_hash ->
    Ethereum_types.block tzresult Lwt.t

  (** [transaction_receipt tx_hash] returns the receipt of [tx_hash]. *)
  val transaction_receipt :
    Ethereum_types.hash ->
    Ethereum_types.transaction_receipt option tzresult Lwt.t

  (** [transaction_object tx_hash] returns the informations of [tx_hash]. *)
  val transaction_object :
    Ethereum_types.hash ->
    Ethereum_types.transaction_object option tzresult Lwt.t

  (** [chain_id ()] returns chain id defined by the rollup. *)
  val chain_id : unit -> Ethereum_types.quantity tzresult Lwt.t

  (** [base_fee_per_gas ()] returns base fee defined by the rollup. *)
  val base_fee_per_gas : unit -> Ethereum_types.quantity tzresult Lwt.t

  (** [kernel_version ()] returns the internal kernel version (i.e the commit hash where
        the kernel was compiled). *)
  val kernel_version : unit -> string tzresult Lwt.t

  (** [simulate_call call_info] asks the rollup to simulate a call, and returns the
        result. *)
  val simulate_call : Ethereum_types.call -> Ethereum_types.hash tzresult Lwt.t

  (** [estimate_gas call_info] asks the rollup to simulate a call, and returns the
        gas used to execute the call. *)
  val estimate_gas :
    Ethereum_types.call -> Ethereum_types.quantity tzresult Lwt.t

  (** [is_tx_valid tx_raw] checks if the transaction is valid. Checks if the nonce is correct
      and returns the associated public key of transaction. *)
  val is_tx_valid :
    Ethereum_types.hex -> (Ethereum_types.address, string) result tzresult Lwt.t

  (** [storage_at address pos] returns the value at index [pos] of the
      account [address]'s storage. *)
  val storage_at :
    Ethereum_types.address ->
    Ethereum_types.quantity ->
    Ethereum_types.hex tzresult Lwt.t
end

(** Instantiate a module of type {!S} that communicates with a rollup
      node endpoint given by [Base.base]. *)
module Make : functor
  (Base : sig
     val base : Uri.t
   end)
  -> S
