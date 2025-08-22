(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type ROUNDTRIP = sig
  type output

  val target : string

  val of_input : 'a Data_encoding.t -> 'a -> output

  val to_input : 'a Data_encoding.t -> output -> 'a
end

type t = (module ROUNDTRIP)

let target : t -> string = fun (module R : ROUNDTRIP) -> R.target

let make : type a. a Data_encoding.t -> t -> a -> a =
 fun encoding (module R : ROUNDTRIP) input ->
  R.of_input encoding input |> R.to_input encoding

let make_with_2_encoding : type a.
    a Data_encoding.t -> a Data_encoding.t -> t -> a -> a =
 fun encoding1 encoding2 (module R : ROUNDTRIP) input ->
  R.of_input encoding1 input |> R.to_input encoding2

let binary : t =
  (module struct
    type output = bytes

    let target = "binary"

    let of_input encoding x = Data_encoding.Binary.to_bytes_exn encoding x

    let to_input = Data_encoding.Binary.of_bytes_exn
  end)

let json : t =
  (module struct
    type output = Data_encoding.Json.json

    let target = "json"

    let of_input encoding x = Data_encoding.Json.construct encoding x

    let to_input encoding j = Data_encoding.Json.destruct encoding j
  end)
