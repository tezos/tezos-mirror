(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

include Data_encoding

module Encoding = struct
  include Encoding

  let lazy_encoding encoding =
    let binary = lazy_encoding encoding in
    let json =
      let open Json_encoding in
      let write (type value)
          (module Repr : Json_repr.Repr with type value = value) le =
        match force_decode le with
        | Some r ->
            Json_repr.convert
              (module Json_repr.Ezjsonm)
              (module Repr)
              (Json.construct encoding r)
        | None ->
            apply_lazy
              ~fun_value:(fun _ -> assert false)
              ~fun_bytes:(fun b ->
                let (`Hex h) = Hex.of_bytes b in
                Repr.repr (`O [("unparsed-binary", Repr.repr (`String h))]))
              ~fun_combine:(fun _ _ -> assert false)
              le
      in
      let read (type value)
          (module Repr : Json_repr.Repr with type value = value) j =
        let j = Json_repr.convert (module Repr) (module Json_repr.Ezjsonm) j in
        make_lazy encoding (Json.destruct encoding j)
      in
      repr_agnostic_custom
        {write; read; is_object = false}
        ~schema:Json_schema.any
    in
    Data_encoding__Encoding.raw_splitted ~json ~binary

  let dynamic_size :
      ?kind:[`Uint30 | `Uint16 | `Uint8] -> 'a encoding -> 'a encoding =
   fun ?kind e ->
    let kind = (kind :> [`N | `Uint30 | `Uint16 | `Uint8] option) in
    dynamic_size ?kind e
end

include Encoding

module Json = struct
  include Data_encoding.Json

  let construct encoding v = construct encoding v

  let destruct encoding j = destruct encoding j
end
