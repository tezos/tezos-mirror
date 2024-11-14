(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
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

(** [no_0x s] removes the prefix [0x] of [s] if it exists. *)
val no_0x : string -> string

(** [normalize s] calls {!no_0x} and {!String.lowercase_ascii} on [s]. *)
val normalize : string -> string

(** Path in the Wasm PVM durable storage. *)
type path = string

val delayed_inbox : path

(** [kernel_root_hash] is the path to the current kernel root hash. *)
val kernel_root_hash : path

(** [indexes] is the directory with all indexes category. *)
val indexes : path

(** [eth_accounts] is the path to ethereum accounts. *)
val eth_accounts : path

(** [eth_account addr] is the path to the [addr] account. The address
    is "normalized", i.e. lowered and removed the prefix [0x] if it exists. *)
val eth_account : string -> path

(** [balance addr] is the path to the [addr] account's balance. *)
val balance : string -> path

(** [code addr] is the path to the [addr] account's code. *)
val code : string -> path

(** [storage addr ?key ()] is the path to the [addr] storage's code. [key]
    can be provided to get the path of a sub-element in the storage. *)
val storage : string -> ?key:string -> unit -> path

(** [admin] is the path to the administrator contract. *)
val admin : path

(** [kernel_governance] is the path to the kernel governance contract. *)
val kernel_governance : path

(** [kernel_security_governance] is the path to the security governance contract. *)
val kernel_security_governance : path

(** [sequencer_governance] is the path to the governance contract
    administrating the sequencer. *)
val sequencer_governance : path

(** [ticketer] is the path to the ticketer contract. *)
val ticketer : path

(** [sequencer] is the path to the sequencer flag. *)
val sequencer : path

(** [sequencer_pool_address] is the path to the L2 address credited with DA fees. *)
val sequencer_pool_address : path

(** [kernel_boot_wasm] is the path to the kernel `boot.wasm`. *)
val kernel_boot_wasm : path

(** [delayed_bridge_path] is the path to the delayed transaction bridge contract. *)
val delayed_bridge_path : path

(** [da_fee_per_byte_path] is the path to the da fee per byte, charged on every transaction. *)
val da_fee_per_byte_path : path

(** [minimum_base_fee_per_gas] is the path to the minimum base fee per gas the kernel will charge. *)
val minimum_base_fee_per_gas : path

(** [delayed_inbox_timeout] is the path to the timeout for
    delayed transactions. *)
val delayed_inbox_timeout : path

(** [delayed_inbox_min_levels] is the path to the minimum number of L1 levels
    needed to have passed to consider a timeout. *)
val delayed_inbox_min_levels : path

(** [reveal_config] is the path to the storage configuration. *)
val reveal_config : path

(** [enable_fa_bridge] is the path to the feature flag to activate the FA bridge. *)
val enable_fa_bridge : path

module Ticket_table : sig
  (** [balance ~ticket_hash ~account] returns the path where the balance of
      [account] of ticket [ticket_hash] is. *)
  val balance : ticket_hash:path -> account:path -> path
end

module Ghostnet : sig
  val eth_accounts : path

  val eth_account : path -> path

  val balance : path -> path

  val code : path -> path

  val storage : path -> ?key:path -> unit -> path

  val da_fee_per_byte_path : path

  val minimum_base_fee_per_gas : path
end
