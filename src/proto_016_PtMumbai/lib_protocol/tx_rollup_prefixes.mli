(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = {
  b58check_prefix : string;
  prefix : string;
  hash_size : int;
  b58check_size : int;
}

(** See {!Tx_rollup_repr}. *)
val rollup_address : t

(** See {!Tx_rollup_inbox_repr}. *)
val inbox_hash : t

(** See {!Tx_rollup_message_repr}. *)
val message_hash : t

(** See {!Tx_rollup_commitment_repr}. *)
val commitment_hash : t

(** See {!Tx_rollup_commitment_repr}. *)
val message_result_hash : t

(** See {!Tx_rollup_message_result_repr.Merkle}. *)
val message_result_list_hash : t

(** See {!Tx_rollup_withdraw_repr}. *)
val withdraw_list_hash : t

(** See {!Tx_rollup_inbox_repr.inbox_hash}. *)
val inbox_list_hash : t

(** [check_encoding spec encoding] checks that [encoding] satisfies
    [spec]. Raises an exception otherwise. *)
val check_encoding : t -> 'a Base58.encoding -> unit
