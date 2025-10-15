(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

(** Semantics of logical and bit-shift operators for bytes *)

(** [bytes_or a b] returns the logical or'ed bytes of [a] and [b].
    If the arguments have different lengths, the shorter one is 0-padded
    on the left before the logical operation. For example:

      0x1200 OR 0x34 = 0x1200 OR 0x0034 = 0x1234
      0x0012 OR 0xff = 0x0012 OR 0x00ff = 0x00ff  (instead of 0xff)
*)
val bytes_or : bytes -> bytes -> bytes

(** [bytes_and a b] returns the logical and'ed bytes of [a] and [b].
    If the arguments have different lengths, the prefix of the longer one
    is removed to have the same length as the shorter one before the logical
    operation. For example:

      0x1234 AND 0x30 = 0x34 AND 0x30 = 0x30
      0x12f00f AND 0x0fff = 0xf00f AND 0x0fff = 0x000f  (instead of 0x0f)
*)
val bytes_and : bytes -> bytes -> bytes

(** [bytes_xor a b] returns the logical xor'ed bytes of [a] and [b].
    If the arguments have different lengths, the shorter one is 0-padded
    on the left before the logical operation. For example:

      0x1200 XOR 0x34 = 0x1200 XOR 0x0034 = 0x1234
      0x0012 XOR 0xff = 0x0012 XOR 0x00ff = 0x00ed  (instead of 0xed)
*)
val bytes_xor : bytes -> bytes -> bytes

(** [bytes_not a] returns the logical not'ed bytes of [a] with the same
    length of [a].  For example:

      NOT 0xff00 = 0x00ff  (instead of 0xff)
*)
val bytes_not : bytes -> bytes

(** [bytes_lsl bytes bits] returns the [bits] left shifted bytes of [bytes].
    If [bits] is more than 64000, it returns [None].

    The function always returns a longer bytes of the input if [bits]
    is not 0.    For example:

      0x12 LSL 1 = 0x0024  (instead of 0x24)
      0x0012 LSL 9 = 0x00002400 (instead of 0x002400 or 0x2400)
*)
val bytes_lsl : bytes -> Script_int.n Script_int.num -> bytes option

(** [bytes_lsr bytes bits] returns the [bits] right shifted bytes of [bytes].

      0x1234 LSR 1 = 0x091a
      0x1234 LSR 8 = 0x12  (instead of 0x0012)
*)
val bytes_lsr : bytes -> Script_int.n Script_int.num -> bytes

(** Convert a natural number to bytes using big-endian encoding. *)
val bytes_of_nat_be : Script_int.n Script_int.num -> bytes

(** Convert bytes to a natural number using big-endian encoding. *)
val nat_of_bytes_be : bytes -> Script_int.n Script_int.num

(** Convert an integer to bytes using big-endian encoding.
    Negative numbers are handled by two's-complement. *)
val bytes_of_int_be : Script_int.z Script_int.num -> bytes

(** Convert bytes to an integer using big-endian encoding.
    Negative numbers are handled by two's-complement. *)
val int_of_bytes_be : bytes -> Script_int.z Script_int.num

module Conversion_BE : sig
  val bytes_of_nat_be : Z.t -> bytes option

  val nat_of_bytes_be : bytes -> Z.t

  val bytes_of_int_be : Z.t -> bytes

  val int_of_bytes_be : bytes -> Z.t
end
