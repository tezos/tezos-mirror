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

type t

(** Represents a period of time as a non-negative integer. *)
type period = t

include Compare.S with type t := t

val encoding : period Data_encoding.t

val rpc_arg : period RPC_arg.t

val pp : Format.formatter -> period -> unit

(** Returns the number of seconds contained in the period. *)
val to_seconds : period -> int64

(** Converts a number of seconds to a [period].

    [of_second s] fails if [s] is not positive. *)
val of_seconds : int64 -> period tzresult

(** Converts a number of seconds to [period].

   [of_second s] fails if [s] is not positive.
    It should only be used at toplevel for constants. *)
val of_seconds_exn : int64 -> period

(** Safe addition of periods, guarded against overflow. *)
val add : period -> period -> period tzresult

(** Alias for [add]. *)
val ( +? ) : period -> period -> period tzresult

(** Safe multiplication by a positive integer. Guarded against overflow. *)
val mult : int32 -> period -> period tzresult

val zero : period

val one_second : period

val one_minute : period

val one_hour : period

val one_day : period

(** [compare x y] returns [0] if [x] is equal to [y], a negative
    integer if [x] is shorter than [y], and a positive integer if [x]
    is longer than [y]. *)
val compare : period -> period -> int
