(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Functori,     <contact@functori.com>                   *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type CONVERSION_INTF = sig
  type t

  val of_proto : 'a Data_encoding.t -> 'a -> t

  val to_proto : 'a Data_encoding.t -> t -> 'a

  val encoding : t Data_encoding.t

  val equal : t -> t -> bool
end

(* Generic implementation of {!CONVERSION_INFT} where types are bytes. *)
module Bytes_conversion_implementation : CONVERSION_INTF = struct
  open Data_encoding

  type t = bytes

  let of_proto encoding v = Binary.to_bytes_exn encoding v

  let to_proto encoding v = Binary.of_bytes_exn encoding v

  let encoding = bytes

  let equal = Bytes.equal
end

module Skip_list_cell : CONVERSION_INTF = Bytes_conversion_implementation

module Skip_list_hash : CONVERSION_INTF = Bytes_conversion_implementation
