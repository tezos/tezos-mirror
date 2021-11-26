(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module defines identifiers for transaction only rollup (or
    tx rollup).  It also specifies how to compute originated
    tx rollup's hash from origination nonce. *)

(** A specialized Blake2B implementation for hashing tx_rollup identifiers with
    "tru1" as a base58 prefix *)
module Hash : sig
  val rollup_hash : string

  include S.HASH
end

type t = private Hash.t

type tx_rollup = t

include Compare.S with type t := t

val to_b58check : t -> string

val of_b58check : string -> t tzresult

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

(** [originated_tx_rollup nonce] is the tx_rollup address originated from
    [nonce]. See [Origination_nonce.t] for more information. *)
val originated_tx_rollup : Origination_nonce.t -> t

val rpc_arg : t RPC_arg.arg

module Index : Storage_description.INDEX with type t = t

(** [state] is an empty type but will be changed in a future MR to represent the
    state of a tx_rollup. *)
type state

val state_encoding : state Data_encoding.t

(** [empty_state] is the initial value at the origination of a
    tx_rollup. It contains no inboxes. *)
val empty_state : state

val pp_state : Format.formatter -> state -> unit
