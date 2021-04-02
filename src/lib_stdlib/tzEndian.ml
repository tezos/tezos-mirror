(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let set_int32 = Bytes.set_int32_be

let get_int32 = Bytes.get_int32_be

let get_int32_string s off = Bytes.get_int32_be (Bytes.unsafe_of_string s) off

let set_int8 = Bytes.set_int8

let get_int8 = Bytes.get_int8

let get_int8_string s off = Bytes.get_int8 (Bytes.unsafe_of_string s) off

let set_int16 = Bytes.set_int16_be

let get_int16 = Bytes.get_int16_be

let get_int16_string s off = Bytes.get_int16_be (Bytes.unsafe_of_string s) off

let set_int64 = Bytes.set_int64_be

let get_int64 = Bytes.get_int64_be

let get_int64_string s off = Bytes.get_int64_be (Bytes.unsafe_of_string s) off

let set_uint8 = Bytes.set_uint8

let get_uint8 = Bytes.get_uint8

let get_uint8_string s off = Bytes.get_uint8 (Bytes.unsafe_of_string s) off

let set_uint16 = Bytes.set_uint16_be

let get_uint16 = Bytes.get_uint16_be

let get_uint16_string s off =
  Bytes.get_uint16_be (Bytes.unsafe_of_string s) off

let get_double buff i = Int64.float_of_bits (Bytes.get_int64_be buff i)

let get_double_string buff i =
  Int64.float_of_bits (Bytes.get_int64_be (Bytes.unsafe_of_string buff) i)

let set_double buff i v = Bytes.set_int64_be buff i (Int64.bits_of_float v)
