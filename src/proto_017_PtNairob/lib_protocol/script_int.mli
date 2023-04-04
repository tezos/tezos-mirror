(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** The types for arbitrary precision integers in Michelson.
    The type variable ['t] is always [n] or [z],
    [n num] and [z num] are incompatible.

    This is internally a [Z.t].
    This module mostly adds signedness preservation guarantees. *)
type 't repr

(** [num] is made algebraic in order to distinguish it from the other type
    parameters of [Script_typed_ir.ty]. *)
type 't num = Num_tag of 't repr [@@ocaml.unboxed]

(** Flag for natural numbers. *)
type n = Natural_tag

(** Flag for relative numbers. *)
type z = Integer_tag

(** Natural zero. *)
val zero_n : n num

(** Natural one. *)
val one_n : n num

(** Natural successor.

    [succ_n x] is the same as [add_n one_n].
 *)
val succ_n : n num -> n num

(** Relative zero. *)
val zero : z num

(** Relative one. *)
val one : z num

(** Compare two numbers as if they were *)
val compare : 'a num -> 'a num -> int

(** Conversion to an OCaml [string] in decimal notation. *)
val to_string : _ num -> string

(** Conversion from an OCaml [string].
    Returns [None] in case of an invalid notation.
    Supports [+] and [-] sign modifiers, and [0x], [0o] and [0b] base modifiers. *)
val of_string : string -> z num option

(** Conversion from an OCaml [int32]. *)
val of_int32 : int32 -> z num

(** Conversion to an OCaml [int64], returns [None] on overflow. *)
val to_int64 : _ num -> int64 option

(** Conversion from an OCaml [int64]. *)
val of_int64 : int64 -> z num

(** Conversion to an OCaml [int], returns [None] on overflow. *)
val to_int : _ num -> int option

(** Conversion from an OCaml [int]. *)
val of_int : int -> z num

(** Conversion from a Zarith integer ([Z.t]). *)
val of_zint : Z.t -> z num

(** Conversion to a Zarith integer ([Z.t]). *)
val to_zint : 'a num -> Z.t

(** Addition between naturals. *)
val add_n : n num -> n num -> n num

(** Multiplication with a natural. *)
val mul_n : n num -> 'a num -> 'a num

(** Euclidean division of a natural.
    [ediv_n n d] returns [None] if divisor is zero,
    or [Some (q, r)] where [n = d * q + r] and [[0 <= r < d]] otherwise. *)
val ediv_n : n num -> 'a num -> ('a num * n num) option

(** Sign agnostic addition.
    Use {!add_n} when working with naturals to preserve the sign. *)
val add : _ num -> _ num -> z num

(** Sign agnostic subtraction. *)
val sub : _ num -> _ num -> z num

(** Sign agnostic multiplication.
    Use {!mul_n} when working with a natural to preserve the sign. *)
val mul : _ num -> _ num -> z num

(** Sign agnostic euclidean division.
    [ediv n d] returns [None] if divisor is zero,
    or [Some (q, r)] where [n = d * q + r] and [[0 <= r < |d|]] otherwise.
    Use {!ediv_n} when working with a natural to preserve the sign. *)
val ediv : _ num -> _ num -> (z num * n num) option

(** Compute the absolute value of a relative, turning it into a natural. *)
val abs : z num -> n num

(** Partial identity over [N]. *)
val is_nat : z num -> n num option

(** Negates a number. *)
val neg : _ num -> z num

(** Turns a natural into a relative, not changing its value. *)
val int : n num -> z num

(** Reverses each bit in the representation of the number.
    Also applies to the sign. *)
val lognot : _ num -> z num

(** Shifts the natural to the left of a number of bits between 0 and 256.
    Returns [None] if the amount is too high. *)
val shift_left_n : n num -> n num -> n num option

(** Shifts the natural to the right of a number of bits between 0 and 256.
    Returns [None] if the amount is too high. *)
val shift_right_n : n num -> n num -> n num option

(** Shifts the number to the left of a number of bits between 0 and 256.
    Returns [None] if the amount is too high. *)
val shift_left : 'a num -> n num -> 'a num option

(** Shifts the number to the right of a number of bits between 0 and 256.
    Returns [None] if the amount is too high. *)
val shift_right : 'a num -> n num -> 'a num option

(** Applies a boolean or operation to each bit. *)
val logor : 'a num -> 'a num -> 'a num

(** Applies a boolean and operation to each bit. *)
val logand : _ num -> n num -> n num

(** Applies a boolean xor operation to each bit. *)
val logxor : n num -> n num -> n num

(** Naturals are encoded using Data_encoding.n *)
val n_encoding : n num Data_encoding.encoding

(** Integers are encoded using Data_encoding.z *)
val z_encoding : z num Data_encoding.encoding
