(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module provides saturated arithmetic between 0 and 2^62 - 1.

   This means that the arithmetic operations provided by this module
   do not overflow. If an operation would produce an integer [x]
   greater than [2 ^ 62 - 1], it is [saturated] to this
   value. Similarly, if an operation would produce a negative integer,
   it outputs [zero] instead.

   This saturation arithmetic is used to monitor gas levels. While the
   gas model can produce values beyond 2^62 - 1, there is no point in
   distinguishing these values from 2^62 - 1 because the amount of gas
   available is significantly lower than this limit.

   Notice that most saturation arithmetic operations do not behave
   as their standard counterparts when one of their operands is
   saturated. For instance,

              (saturated + saturated) - saturated = 0

   For more information about saturation arithmetic, take a look at:

        https://en.wikipedia.org/wiki/Saturation_arithmetic

*)

(** An integer of type ['a t] is between [0] and [saturated].

    The type parameter ['a] is [mul_safe] if the integer is known
    not to overflow when multiplied with another [mul_safe t].

    The type parameter ['a] is [may_saturate] if the integer is
    not known to be sufficiently small to prevent overflow during
    multiplication.

*)
type 'a t = private int

type mul_safe

type may_saturate

val may_saturate : _ t -> may_saturate t

(** [to_int x] returns the underlying integer representing [x]. *)
val to_int : 'a t -> int

(** 0 *)
val zero : _ t

(** 1 *)
val one : _ t

(** 2^62 - 1 *)
val saturated : may_saturate t

(** We inherit the order over native integers. *)
val ( >= ) : _ t -> _ t -> bool

val ( > ) : _ t -> _ t -> bool

val ( <= ) : _ t -> _ t -> bool

val ( < ) : _ t -> _ t -> bool

val ( = ) : _ t -> _ t -> bool

val ( <> ) : _ t -> _ t -> bool

val equal : _ t -> _ t -> bool

val min : 'a t -> 'a t -> 'a t

val max : 'a t -> 'a t -> 'a t

val compare : 'a t -> 'b t -> int

(** [a >! b] is [a > b]. Avoids using [to_int]. *)
val ( >! ) : _ t -> int -> bool

(** [numbits x] returns the number of bits used in the binary representation
    of [x]. *)
val numbits : 'a t -> int

(** [shift_right x y] behaves like a logical shift of [x] by [y] bits
   to the right. [y] must be between 0 and 63. *)
val shift_right : 'a t -> int -> 'a t

(** [shift_left x y] behaves like a logical shift of [x] by [y] bits
    to the left. [y] must be between 0 and 63. In cases where [x lsl y]
    is overflowing, [shift_left x y] is [saturated]. *)
val shift_left : 'a t -> int -> 'a t

(** [mul x y] behaves like multiplication between native integers as
   long as its result stay below [saturated]. Otherwise, [mul] returns
   [saturated]. *)
val mul : _ t -> _ t -> may_saturate t

(** [mul_safe x] returns a [mul_safe t] only if [x] does not trigger
    overflows when multiplied with another [mul_safe t]. More precisely,
    [x] is safe for fast multiplications if [x < 2147483648]. *)
val mul_safe : _ t -> mul_safe t option

(** [mul_fast x y] exploits the fact that [x] and [y] are known not to
   provoke overflows during multiplication to perform a mere
   multiplication. *)
val mul_fast : mul_safe t -> mul_safe t -> may_saturate t

(** [scale_fast x y] exploits the fact that [x] is known not to
   provoke overflows during multiplication to perform a
   multiplication faster than [mul]. *)
val scale_fast : mul_safe t -> _ t -> may_saturate t

(** [add x y] behaves like addition between native integers as long as
   its result stay below [saturated]. Otherwise, [add] returns
   [saturated]. *)
val add : _ t -> _ t -> may_saturate t

(** [succ x] is like [add one x] *)
val succ : _ t -> may_saturate t

(** [sub x y] behaves like subtraction between native integers as long
   as its result stay positive. Otherwise, [sub] returns [zero].
   This function assumes that [x] is not saturated.
*)
val sub : 'a t -> _ t -> 'a t

(** [sub_opt x y] behaves like subtraction between native integers as
   long as its result stay positive. Otherwise, [sub] returns
   [None]. *)
val sub_opt : 'a t -> _ t -> 'a t option

(** [ediv x y] returns [x / y]. This operation never saturates, hence
   it is exactly the same as its native counterpart. [y] is supposed
   to be strictly greater than 0, otherwise this function raises
   [Division_by_zero]. *)
val ediv : 'a t -> _ t -> 'a t

(** [erem x y] returns [x mod y]. [y] is supposed to be strictly
   greater than 0, otherwise this function raises
   [Division_by_zero]. *)
val erem : _ t -> 'b t -> 'b t

(** [sqrt x] returns the square root of x, rounded down. *)
val sqrt : _ t -> 'a t

(** [of_int_opt x] returns [Some x] if [x >= 0] and [x < saturated],
    and [None] otherwise. *)
val of_int_opt : int -> may_saturate t option

(** [of_z_opt x] returns [Some x] if [x >= 0] and [x < saturated],
    and [None] otherwise. *)
val of_z_opt : Z.t -> may_saturate t option

(** When a saturated integer is sufficiently small (i.e. strictly less
   than 2147483648), we can assign it the type [mul_safe S.t] to use
   it within fast multiplications, named [S.scale_fast] and
   [S.mul_fast].

   The following function allows such type assignment but may raise an
   exception if the assumption is wrong.  Therefore, [mul_safe_exn]
   should only be used to define toplevel values, so that these
   exceptions can only occur during startup.
 *)
val mul_safe_exn : may_saturate t -> mul_safe t

(** [mul_safe_of_int_exn x] is the composition of [of_int_opt] and
   [mul_safe] in the option monad. This function raises [Invalid_argument]
   if [x] is not safe. This function should be used on integer literals
   that are obviously [mul_safe]. *)
val mul_safe_of_int_exn : int -> mul_safe t

(** [safe_z z] is [of_z_opt x |> saturate_if_undef]. *)
val safe_z : Z.t -> may_saturate t

(** [safe_int x] is [of_int_opt x |> saturate_if_undef]. *)
val safe_int : int -> may_saturate t

(** [to_z z] is [Z.of_int]. *)
val to_z : _ t -> Z.t

(** Encoding for [t] through the encoding for [z] integers. *)
val z_encoding : _ t Data_encoding.t

(** Encoding for [t] through the encoding for non-negative integers. *)
val n_encoding : _ t Data_encoding.t

(** A pretty-printer for native integers. *)
val pp : Format.formatter -> _ t -> unit

(** Syntax for simple representations. *)
module Syntax : sig
  val log2 : _ t -> may_saturate t

  val sqrt : _ t -> may_saturate t

  val ( + ) : _ t -> _ t -> may_saturate t

  val ( - ) : _ t -> _ t -> may_saturate t

  val ( * ) : _ t -> _ t -> may_saturate t

  val ( < ) : _ t -> _ t -> bool

  val ( = ) : _ t -> _ t -> bool

  val ( lsr ) : 'a t -> int -> 'a t

  val ( lsl ) : 'a t -> int -> 'a t
end
