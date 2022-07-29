(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Tezos_webassembly_interpreter
open Lazy_containers

module Make (Tree_encoding : Tree_encoding.S) = struct
  module V = Instance.Vector
  module M = Instance.NameMap
  module C = Chunked_byte_vector.Lwt
  module Wasm_encoding = Wasm_encoding.Make (Tree_encoding)
  include Tree_encoding

  let vector_encoding value_enc =
    Lazy_vector_encoding.Int32.lazy_vector
      (value [] Data_encoding.int32)
      value_enc

  module Lazy_vec = struct
    let raw_encoding vector_encoding =
      let offset = value ["offset"] Data_encoding.int32 in
      let vector = scope ["vector"] vector_encoding in
      conv
        (fun (offset, vector) -> Decode.LazyVec {offset; vector})
        (fun (LazyVec {offset; vector}) -> (offset, vector))
        (tup2 ~flatten:true offset vector)

    let encoding value_encoding = raw_encoding (vector_encoding value_encoding)
  end

  module Byte_vector = struct
    type t' = Decode.byte_vector_kont

    let vkstart_case =
      case
        "VKStart"
        (value [] Data_encoding.unit)
        (function Decode.VKStart -> Some () | _ -> None)
        (fun () -> Decode.VKStart)

    let vkread_case =
      let value_enc =
        let pos = value ["pos"] Data_encoding.int64 in
        let length = value ["length"] Data_encoding.int64 in
        let data_label =
          value ["data_label"] Interpreter_encodings.Ast.data_label_encoding
        in
        tup3 ~flatten:true data_label pos length
      in
      case
        "VKRead"
        value_enc
        (function Decode.VKRead (b, p, l) -> Some (b, p, l) | _ -> None)
        (fun (b, p, l) -> Decode.VKRead (b, p, l))

    let vkstop_case =
      case
        "VKStop"
        (value ["data_label"] Interpreter_encodings.Ast.data_label_encoding)
        (function Decode.VKStop b -> Some b | _ -> None)
        (fun b -> Decode.VKStop b)

    let tag_encoding = value [] Data_encoding.string

    let encoding =
      tagged_union tag_encoding [vkstart_case; vkread_case; vkstop_case]
  end
end
