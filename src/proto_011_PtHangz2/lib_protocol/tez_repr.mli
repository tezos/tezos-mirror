(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Internal representation of the Tez currency. Behaves mostly like a natural
    number where number 1 represents 1/1,000,000 Tez (1 micro-Tez or mutez).
    It's protected from ever becoming negative and overflowing by special
    arithmetic functions, which fail in case something undesired would happen.
    When divided, it's always rounded down to 1 mutez.

    Internally encoded as [int64], which may be relevant to guard against
    overflow errors. *)
type t

type tez = t

val zero : t

val one_mutez : t

val one_cent : t

val fifty_cents : t

val one : t

(** Tez subtraction.

    [a -? b] is the difference between [a] and [b] given that [b] is greater or
    equal to [a]. Otherwise an error ([Subtraction underflow]) is returned. *)
val ( -? ) : t -> t -> t tzresult

(** Tez addition.

    [a +? b] is the sum of [a] and [b] or an [Addition overflow] error in case
    of overflow. *)
val ( +? ) : t -> t -> t tzresult

(** Tez multiplication by an integral factor.

    [a *? m] is [a] multiplied by [m] (which must be non-negative) or a
    [Multiplication_overflow] error. *)
val ( *? ) : t -> int64 -> t tzresult

(** Tez division by an integral divisor.

    [a /? d] is [a] divided by [d] (which must be positive). Given that [d]
    is positive, this function is safe. The result is rounded down to
    1 mutez. *)
val ( /? ) : t -> int64 -> t tzresult

val to_mutez : t -> int64

(** [of_mutez n] (micro tez) is None if n is negative *)
val of_mutez : int64 -> t option

(** [of_mutez_exn n] fails if n is negative.
    It should only be used at toplevel for constants. *)
val of_mutez_exn : int64 -> t

(** It should only be used at toplevel for constants. *)
val mul_exn : t -> int -> t

val encoding : t Data_encoding.t

val to_int64 : t -> int64

include Compare.S with type t := t

val pp : Format.formatter -> t -> unit

val of_string : string -> t option

val to_string : t -> string
