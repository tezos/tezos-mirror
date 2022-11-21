(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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

(** Bitwise AND on bytes.

    If the arguments have different lengths, the prefix of the longer bytes
    is cut to have the same length as the shorter one before taking bitwise
    AND.

      ex. 0xff0f AND 0xff = 0x0f AND 0xff = 0x0f
*)
val logand : bytes -> bytes -> bytes

(** Bitwise OR on bytes.

    If the arguments have different lengths, the shorter bytes is 0-padded
    on the left to have the same length before taking bitwise OR.

      ex. 0xf000 OR 0x0f = 0xf000 OR 0x000f = 0xf00f
*)
val logor : bytes -> bytes -> bytes

(** Bitwise XOR on bytes.

    If the arguments have different lengths, the shorter bytes is 0-padded
    on the left to have the same length before taking bitwise XOR.

      ex. 0xf0ff XOR 0x0f = 0xf0ff XOR 0x000f = 0xf0f0
*)
val logxor : bytes -> bytes -> bytes

(** Bitwise NOT on bytes.

      ex. NOT 0xfff0f0 = 0x000f0f
*)
val lognot : bytes -> bytes

(** Logical shift left on bytes.

    [shift_left bs nbits] shifts the byte contents left by [nbits] bits,
    using big-endian encoding. The vacated bits on the right are filled
    with 0s. The shifted bits are minimally 0-padded on the left in order
    to keep all the original bits: for example, 0x1234 LSL 1 is 0x002468,
    instead of 0x2468 (the left most bit is lost) or 0x00002468 (not
    minimal padding).

    [shift_left bs nbits] raises [Invalid_argument "shift_left"]
    when [nbits < 0].

      ex. 0x1234 LSL 0 = 0x1234
          0xffff LSL 1 = 0x01fffe
          0x1234 LSL 1 = 0x002468  (not 0x2468)
          0x1234 LSL 8 = 0x123400
          0x001234 LSL 1 = 0x00002468  (not 0x002468)
          0x001234 LSL 18 = 0x0048d00000  (not 0x48d00000)
          0x (empty bytes) LSL 1 = 0x00
*)
val shift_left : bytes -> int -> bytes

(** Logical shift right on bytes, using big-endian encoding.

    [shift_right bs nbits] shifts the byte contents right by [nbits] bits,
    using big-endian encoding. The shifted bits are minimally 0-padded on
    the left to fit in bytes. For example, 0x123499 LSR 9 is 0xx091a,
    instead of 0x00091a (not minimal padding).

    [shift_right bs nbits] raises [Invalid_argument "shift_right"]
    when [nbits < 0].

      ex. 0x1234 LSR 0 = 0x1234
          0x1234 LSR 1 = 0x091a
          0x1234 LSR 8 = 0x12   (not 0x0012)
          0x123499 LSR 9 = 0x091a
          0x1234 LSR 18 = 0x
*)
val shift_right : bytes -> int -> bytes
