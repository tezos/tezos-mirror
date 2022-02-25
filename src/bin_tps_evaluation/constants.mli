(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** The protocol we are using. *)
val protocol : Protocol.t

(** The protocol constants we are using. *)
val protocol_constants : Protocol.constants

(** The bootstrap account that is supposed to originate smart contracts. *)
val originating_bootstrap : Account.key

(** Produce a list of delegate aliases given the total number of bootstrap accounts. *)
val make_delegates : int -> string list

(** The number of bootstrap accounts available by default. *)
val default_bootstraps_count : int

(** TPS that is used when protocol limits are lifted. This is a high enough
    value (not yet achievable) so that we're not limited by it. This value
    affects the total number of accounts we will need, so we cannot use a
    ridiculously high number here. *)
val lifted_limits_tps : int

(** Safety margin for gas estimations. *)
val gas_safety_margin : int
