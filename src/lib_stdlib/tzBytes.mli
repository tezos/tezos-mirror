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

    Example:

    [logand (Bytes.of_string "\xff\x0f") (Bytes.of_string "\xff") = Bytes.of_string "\x0f"]
*)
val logand : bytes -> bytes -> bytes

(** Bitwise OR on bytes.

    If the arguments have different lengths, the shorter bytes is 0-padded
    on the left to have the same length before taking bitwise OR.

    Example:

    [logor (Bytes.of_string "\xf0\x00") (Bytes.of_string "\x0f") = Bytes.of_string "\xf0\x0f"]
*)
val logor : bytes -> bytes -> bytes

(** Bitwise XOR on bytes.

    If the arguments have different lengths, the shorter bytes is 0-padded
    on the left to have the same length before taking bitwise XOR.

    Example:

    [logxor (Bytes.of_string "\xf0\xff") (Bytes.of_string "\x0f") = Bytes.of_string "\xf0\xf0"]
*)
val logxor : bytes -> bytes -> bytes

(** Bitwise NOT on bytes.

    Example:

    [lognot (Bytes.of_string "\xff\xf0\xf0") = Bytes.of_string "\x00\x0f\x0f"]
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

    Examples:

    - [shift_left (Bytes.of_string "\x12\x34") 0 = Bytes.of_string "\x12\x34"]
    - [shift_left (Bytes.of_string "\xff\xff") 1 = Bytes.of_string "\x01\xff\xfe"]
    - [shift_left (Bytes.of_string "\x12\x34") 1 = Bytes.of_string "\x00\x24\x68"] (not ["\x24\x68"])
    - [shift_left (Bytes.of_string "\x00\x12\x34") 1 = Bytes.of_string "\x00\x00\x24\x68"] (not ["\x00\x24\x68"])
    - [shift_left (Bytes.of_string "\x00\x12\x34") 18 = Bytes.of_string "\x00\x48\xd0\x00\x00"] (not ["\x48\xd0\x00\x00"])
    - [shift_left Bytes.empty 1 = Bytes.of_string "\x00"]
*)
val shift_left : bytes -> int -> bytes

(** Logical shift right on bytes, using big-endian encoding.

    [shift_right bs nbits] shifts the byte contents right by [nbits] bits,
    using big-endian encoding. The shifted bits are minimally 0-padded on
    the left to fit in bytes. For example, 0x123499 LSR 9 is 0xx091a,
    instead of 0x00091a (not minimal padding).

    [shift_right bs nbits] raises [Invalid_argument "shift_right"]
    when [nbits < 0].

    Examples:

    - [shift_right (Bytes.of_string "\x12\x34") 0 = Bytes.of_string "\x12\x34"]
    - [shift_right (Bytes.of_string "\x12\x34") 1 = Bytes.of_string "\x09\x1a"]
    - [shift_right (Bytes.of_string "\x12\x34") 8 = Bytes.of_string "\x12"] (not ["\x00\x12"])
    - [shift_right (Bytes.of_string "\x12\x34\x99") 9 = Bytes.of_string "\x09\xa"]
    - [shift_right (Bytes.of_string "\x12\x34") 18 = Bytes.empty]
*)
val shift_right : bytes -> int -> bytes

(** [chunk_bytes n b] chunks the sequence of bytes [b] into a list of bytes,
    each of length [n]. The last chunk may be a non-empty string of length less
    than [n], in which case the behaviour of the function depends on whether
    [error_on_partial_chunk] is set:
      {ul
        {li If [error_on_partial_chunk] is set, then the function returns
        [Error error_on_partial_chunk],}
        {li Otherwise, the function return the list of chunks, where the
        last chunk is a non-empty string of length less than [n].}
      }

    @raise Invalid_argument if [n <= 0]. *)
val chunk_bytes :
  ?error_on_partial_chunk:'a -> int -> bytes -> (bytes list, 'a) result
