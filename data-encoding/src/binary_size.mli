(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Like most other [.mli] files in this directory, this is not intended for
    end-users. Instead, the interface from this file is used internally to
    assemble the end-user-intended module {!Data_encoding}. Refer to that module
    for doucmentation. *)

val bool : int

val int8 : int

val uint8 : int

val char : int

val int16 : int

val uint16 : int

val uint30 : int

val uint32 : int

val uint64 : int

val int31 : int

val int32 : int

val int64 : int

val float : int

val max_size_of_uint30_like_n : int

type tag_size = [`Uint8 | `Uint16] [@@deriving hash]

val tag_size : tag_size -> int

type signed_integer = [`Int31 | `Int16 | `Int8] [@@deriving hash]

type unsigned_integer = [`Uint30 | `Uint16 | `Uint8] [@@deriving hash]

type integer = [signed_integer | unsigned_integer] [@@deriving hash]

type length = [`N | unsigned_integer] [@@deriving hash]

val integer_to_size : [< integer] -> int

val length_to_max_size : [< length] -> int

val min_int : [< integer | `N] -> int

val max_int : [< integer | `N] -> int

val range_to_size : minimum:int -> maximum:int -> integer

val unsigned_range_to_size : int -> unsigned_integer

val enum_size : 'a array -> [> unsigned_integer]
