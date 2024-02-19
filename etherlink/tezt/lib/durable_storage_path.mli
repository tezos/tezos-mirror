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

(** Path in the Wasm PVM durable storage. *)
type path = string

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

(** [sequencer_admin] is the path to the contract administrating the
    sequencer. *)
val sequencer_admin : path

(** [ticketer] is the path to the ticketer contract. *)
val ticketer : path

(** [sequencer] is the path to the sequencer flag. *)
val sequencer : path

(** [kernel_boot_wasm] is the path to the kernel `boot.wasm`. *)
val kernel_boot_wasm : path

(** [delayed_bridge_path] is the path to the delayed transaction bridge contract. *)
val delayed_bridge_path : path

(** [da_fee_per_byte_path] is the path to the da fee per byte, charged on every transaction. *)
val da_fee_per_byte_path : path

(** [delayed_inbox_timeout] is the path to the timeout for
    delayed transactions. *)
val delayed_inbox_timeout : path

(** [delayed_inbox_min_levels] is the path to the minimum number of L1 levels
    needed to have passed to consider a timeout. *)
val delayed_inbox_min_levels : path

(** [config_root_hash] is the path to the root hash of the
    storage configuration.  *)
val config_root_hash : path
