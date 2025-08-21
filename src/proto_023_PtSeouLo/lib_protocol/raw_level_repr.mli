(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** The shell's notion of a level: an integer indicating the number of blocks
    since genesis: genesis is 0, all other blocks have increasing levels from
    there. *)
type t

type raw_level = t

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

(** @raise Invalid_argument when the level to encode is not positive *)
val encoding : raw_level Data_encoding.t

val rpc_arg : raw_level RPC_arg.arg

val pp : Format.formatter -> raw_level -> unit

include Compare.S with type t := raw_level

val to_int32 : raw_level -> int32

val to_int32_non_negative : raw_level -> Bounded.Non_negative_int32.t

(** @raise Invalid_argument when the level to encode is negative *)
val of_int32_exn : int32 -> raw_level

(** Can trigger Unexpected_level error when the level to encode is negative *)
val of_int32 : int32 -> raw_level tzresult

val of_int32_non_negative : Bounded.Non_negative_int32.t -> raw_level

val diff : raw_level -> raw_level -> int32

val root : raw_level

val succ : raw_level -> raw_level

val pred : raw_level -> raw_level option

(** Return the predecessor of [l] when [l >= 2], otherwise return [None]. *)
val pred_dontreturnzero : raw_level -> raw_level option

(** [add l i] i must be positive *)
val add : raw_level -> int -> raw_level

(** [sub l i] i must be positive *)
val sub : raw_level -> int -> raw_level option

module Index : Storage_description.INDEX with type t = raw_level
