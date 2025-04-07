(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** 32-bit integers.

   This module provides operations on the type [int32]
   of signed 32-bit integers.  Unlike the built-in [int] type,
   the type [int32] is guaranteed to be exactly 32-bit wide on all
   platforms.  All arithmetic operations over [int32] are taken
   modulo 2{^32}.

   Performance notice: values of type [int32] occupy more memory
   space than values of type [int], and arithmetic operations on
   [int32] are generally slower than those on [int].  Use [int32]
   only when the application requires exact 32-bit arithmetic.

    Literals for 32-bit integers are suffixed by l:
    {[
      let zero: int32 = 0l
      let one: int32 = 1l
      let m_one: int32 = -1l
    ]}
*)

val zero : int32
(** The 32-bit integer 0. *)

val one : int32
(** The 32-bit integer 1. *)

val minus_one : int32
(** The 32-bit integer -1. *)

external neg : int32 -> int32 = "%int32_neg"
(** Unary negation. *)

external add : int32 -> int32 -> int32 = "%int32_add"
(** Addition. *)

external sub : int32 -> int32 -> int32 = "%int32_sub"
(** Subtraction. *)

external mul : int32 -> int32 -> int32 = "%int32_mul"
(** Multiplication. *)

external div : int32 -> int32 -> int32 = "%int32_div"
(** Integer division. This division rounds the real quotient of
   its arguments towards zero, as specified for {!Stdlib.(/)}.
   @raise Division_by_zero if the second
   argument is zero.  *)

external rem : int32 -> int32 -> int32 = "%int32_mod"
(** Integer remainder.  If [y] is not zero, the result
   of [Int32.rem x y] satisfies the following property:
   [x = Int32.add (Int32.mul (Int32.div x y) y) (Int32.rem x y)].
   If [y = 0], [Int32.rem x y] raises [Division_by_zero]. *)

val succ : int32 -> int32
(** Successor.  [Int32.succ x] is [Int32.add x Int32.one]. *)

val pred : int32 -> int32
(** Predecessor.  [Int32.pred x] is [Int32.sub x Int32.one]. *)

val abs : int32 -> int32
(** Return the absolute value of its argument. *)

val max_int : int32
(** The greatest representable 32-bit integer, 2{^31} - 1. *)

val min_int : int32
(** The smallest representable 32-bit integer, -2{^31}. *)


external logand : int32 -> int32 -> int32 = "%int32_and"
(** Bitwise logical and. *)

external logor : int32 -> int32 -> int32 = "%int32_or"
(** Bitwise logical or. *)

external logxor : int32 -> int32 -> int32 = "%int32_xor"
(** Bitwise logical exclusive or. *)

val lognot : int32 -> int32
(** Bitwise logical negation. *)

external shift_left : int32 -> int -> int32 = "%int32_lsl"
(** [Int32.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 32]. *)

external shift_right : int32 -> int -> int32 = "%int32_asr"
(** [Int32.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 32]. *)

external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
(** [Int32.shift_right_logical x y] shifts [x] to the right by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 32]. *)

external of_int : int -> int32 = "%int32_of_int"
(** Convert the given integer (type [int]) to a 32-bit integer
    (type [int32]). On 64-bit platforms, the argument is taken
    modulo 2{^32}. *)

external to_int : int32 -> int = "%int32_to_int"
(** Convert the given 32-bit integer (type [int32]) to an
   integer (type [int]).  On 32-bit platforms, the 32-bit integer
   is taken modulo 2{^31}, i.e. the high-order bit is lost
   during the conversion.  On 64-bit platforms, the conversion
   is exact. *)

val of_string_opt: string -> int32 option
(** Same as [of_string], but return [None] instead of raising.
    @since 4.05 *)


val to_string : int32 -> string
(** Return the string representation of its argument, in signed decimal. *)

type t = int32
(** An alias for the type of 32-bit integers. *)

val compare: t -> t -> int
(** The comparison function for 32-bit integers, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Int32] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for int32s.
    @since 4.03.0 *)
