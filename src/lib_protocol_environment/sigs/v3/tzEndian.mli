(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

val get_int32 : bytes -> int -> int32

val get_int32_string : string -> int -> int32

val set_int32 : bytes -> int -> int32 -> unit

val set_int8 : bytes -> int -> int -> unit

val get_int8 : bytes -> int -> int

val get_int8_string : string -> int -> int

val set_int16 : bytes -> int -> int -> unit

val get_int16 : bytes -> int -> int

val get_int16_string : string -> int -> int

val set_int64 : bytes -> int -> int64 -> unit

val get_int64 : bytes -> int -> int64

val get_int64_string : string -> int -> int64

val get_uint8 : bytes -> int -> int

val get_uint8_string : string -> int -> int

val set_uint8 : bytes -> int -> int -> unit

val get_uint16 : bytes -> int -> int

val get_uint16_string : string -> int -> int

val set_uint16 : bytes -> int -> int -> unit
