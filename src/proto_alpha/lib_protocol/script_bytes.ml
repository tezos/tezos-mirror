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

open Bytes

let bytes_and = logand

let bytes_or = logor

let bytes_xor = logxor

let bytes_not = lognot

let bytes_lsl a n =
  (* We have to limit the number of shifts for LSL *)
  match Script_int.to_int n with
  | Some n when Compare.Int.(n <= 64000) -> Some (shift_left a n)
  | _ -> None

let bytes_lsr a n =
  (* No limit on the number of shifts for LSR *)
  match Script_int.to_int n with
  | None ->
      (* [LSR bytes max_int] can shift out completely the longest
         possible [bytes]. *)
      Bytes.empty
  | Some n -> shift_right a n

module Conversion_BE : sig
  (** Convert a natural number to bytes using big-endian encoding.

      Returns [None] when the argument is negative.

      Examples:

      - [bytes_of_nat_be (Z.of_int 0x00)  = Some Bytes.empty]
      - [bytes_of_nat_be (Z.of_int 0x01)  = Some (Bytes.of_string "\x01")]
      - [bytes_of_nat_be (Z.of_int 0xff)  = Some (Bytes.of_string "\xff")]
      - [bytes_of_nat_be (Z.of_int 0x100) = Some (Bytes.of_strnig "\x01\x00")]
      - [bytes_of_nat_be (Z.of_int (-1))  = None]
  *)
  val bytes_of_nat_be : Z.t -> bytes option

  (** Convert bytes to a natural number using big-endian encoding.

      Examples:

      - [nat_of_bytes_be Bytes.empty                      = Z.of_int 0x00]
      - [nat_of_bytes_be (Bytes.of_string "\x00")         = Z.of_int 0x00]
      - [nat_of_bytes_be (Bytes.of_string "\x01")         = Z.of_int 0x01]
      - [nat_of_bytes_be (Bytes.of_string "\x00\x01")     = Z.of_int 0x01]
      - [nat_of_bytes_be (Bytes.of_string "\xff")         = Z.of_int 0xff]
      - [nat_of_bytes_be (Bytes.of_string "\x00\x00\xff") = Z.of_int 0xff]
      - [nat_of_bytes_be (Bytes.of_string "\x01\x00")     = Z.of_int 0x0100]
  *)
  val nat_of_bytes_be : bytes -> Z.t

  (** Convert an integer to bytes using big-endian encoding.
      Negative numbers are handled by two's-complement.

      Examples:

      - [bytes_of_int_be (Z.of_int 0x00)    = Bytes.empty]
      - [bytes_of_int_be (Z.of_int 0x01)    = Bytes.of_string "\x01"]
      - [bytes_of_int_be (Z.of_int 0x7f)    = Bytes.of_string "\x7f"]
      - [bytes_of_int_be (Z.of_int (-0x80)) = Bytes.of_string "\x80"]
      - [bytes_of_int_be (Z.of_int 0x80)    = Bytes.of_string "\x00\x80"] (not ["\x80"])
      - [bytes_of_int_be (Z.of_int (-0x81)) = Bytes.of_string "\xff\x7f"] (not ["\x7f"])
      - [bytes_of_int_be (Z.of_int 0x8000)  = Bytes.of_string "\x00\x80\x00"], (not ["\x80\x00"])
  *)
  val bytes_of_int_be : Z.t -> bytes

  (** Convert bytes to an integer using big-endian encoding.
      Negative numbers are handled by two's-complement.

      Examples:

      - [int_of_bytes_be Bytes.empty                  = Z.of_int 0x00]
      - [int_of_bytes_be (Bytes.of_string "\x01")     = Z.of_int 0x01]
      - [int_of_bytes_be (Bytes.of_string "\x00\x01") = Z.of_int 0x01]
      - [int_of_bytes_be (Bytes.of_string "\x7f")     = Z.of_int 0x7f]
      - [int_of_bytes_be (Bytes.of_string "\x00\x7f") = Z.of_int 0x7f]
      - [int_of_bytes_be (Bytes.of_string "\x80")     = Z.of_int (-0x80)]  (not [0x80])
      - [int_of_bytes_be (Bytes.of_string "\xff\x80") = Z.of_int (-0x80)]
      - [int_of_bytes_be (Bytes.of_string "\xff\x8f") = Z.of_int (-0x81)]
  *)
  val int_of_bytes_be : bytes -> Z.t
end = struct
  let encode_nat_be nbytes default z =
    (* [nbytes] is the exact number of the bytes to encode [z].

       When encoding an integer to bytes, it is first converted to
       a natural number using 2's complement, and then sent to this function.
       [default] is the prefix byte which may be required for the integer
       encoding.  [Some '\000'] when the integer is zero or positive.
       [Some '\255'] when negative.
    *)
    assert (Compare.Z.(z >= Z.zero)) ;
    (* [Z.to_bits] encodes zero and positive numbers in the little endian.
       The result string can be zero trailed to make its length multiple
       of 4 or 8.
    *)
    let string_le = Z.to_bits z in
    let slen = String.length string_le in
    (* If [slen = nbytes]:
         string_le         aabbcc
         the final output  ccbbaa

       else if [slen > nbytes]:
         string_le         aabbcc0000
         the final output  ccbbaa

       else if [slen < nbytes] and [default= Some DD]:
         This is to encode an integer which requires an extra byte.
           string_le       aabbcc
           encoded       DDccbbaa

       otherwise: error, which should not happen.
    *)
    Bytes.init nbytes (fun i ->
        let j = nbytes - i - 1 in
        if Compare.Int.(j >= slen) then
          Option.value_f default ~default:(fun () ->
              assert false (* it never happens *))
        else string_le.[j])

  let bytes_of_nat_be z =
    match Z.compare z Z.zero with
    | -1 -> None
    | 0 -> Some Bytes.empty
    | _ ->
        let nbits = Z.log2up (Z.succ z) in
        let nbytes = (nbits + 7) / 8 in
        Some (encode_nat_be nbytes None z)

  let bytes_of_int_be z =
    match Z.compare z Z.zero with
    | 0 -> Bytes.empty
    | 1 ->
        let nbits = Z.log2up (Z.succ z) + 1 (* The top bit must be 0 *) in
        let nbytes = (nbits + 7) / 8 in
        encode_nat_be nbytes (Some '\000') z
    | _ ->
        let nbits = Z.log2up Z.(neg z) + 1 (* The top bit must be 1 *) in
        let nbytes = (nbits + 7) / 8 in
        let nbits' = nbytes * 8 in
        let z'' = Z.(add (shift_left one nbits') z) in
        encode_nat_be nbytes (Some '\255') z''

  let nat_of_bytes_be bytes =
    (* [Z.of_bits] ignores trailing zeros *)
    let len = Bytes.length bytes in
    (* Z.of_bits uses little-endian encoding but we want a big-endian
       encoding so we reverse [bytes] while converting it to `string`. *)
    Z.of_bits @@ String.init len (fun i -> Bytes.get bytes (len - i - 1))

  let int_of_bytes_be bytes =
    let nbytes = Bytes.length bytes in
    if Compare.Int.(nbytes = 0) then Z.zero
    else
      let top_bit = Compare.Int.(Char.code (Bytes.get bytes 0) land 128 <> 0) in
      if top_bit then
        (* negative *)
        let z = nat_of_bytes_be bytes in
        let nbits = nbytes * 8 in
        Z.(sub z (shift_left one nbits))
      else nat_of_bytes_be bytes
end

open Script_int

let bytes_of_nat_be (n : n num) =
  (* The function always succeeds since the argument is 0 or positive *)
  match Conversion_BE.bytes_of_nat_be @@ to_zint n with
  | Some bytes -> bytes
  | None -> assert false

let nat_of_bytes_be b = abs @@ of_zint @@ Conversion_BE.nat_of_bytes_be b

let bytes_of_int_be (z : z num) = Conversion_BE.bytes_of_int_be @@ to_zint z

let int_of_bytes_be b = of_zint @@ Conversion_BE.int_of_bytes_be b
