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

  module Name = struct
    let utf8 = value [] Data_encoding.int31

    let nkstart_case =
      case
        "NKStart"
        (value [] Data_encoding.unit)
        (function Decode.NKStart -> Some () | _ -> None)
        (fun () -> Decode.NKStart)

    let nkparse_case =
      let value_enc =
        let pos = value ["pos"] Data_encoding.int31 in
        let buffer = scope ["lazy_kont"] (Lazy_vec.encoding utf8) in
        let length = value ["length"] Data_encoding.int31 in
        tup3 ~flatten:true pos buffer length
      in
      case
        "NKParse"
        value_enc
        (function Decode.NKParse (p, v, l) -> Some (p, v, l) | _ -> None)
        (fun (p, v, l) -> Decode.NKParse (p, v, l))

    let nkstop_case =
      case
        "NKStop"
        (vector_encoding utf8)
        (function Decode.NKStop v -> Some v | _ -> None)
        (fun v -> Decode.NKStop v)

    let tag_encoding = value [] Data_encoding.string

    let encoding =
      tagged_union tag_encoding [nkstart_case; nkparse_case; nkstop_case]
  end

  module Func_type = struct
    type tags = FKStart | FKIns | FKOut | FKStop

    let value_type_encoding =
      value [] Interpreter_encodings.Types.value_type_encoding

    let fkstart_case =
      case
        "FKStart"
        (value [] Data_encoding.unit)
        (function Decode.FKStart -> Some () | _ -> None)
        (fun () -> FKStart)

    let fkins_case =
      let lazy_vec =
        scope ["ins_kont"] (Lazy_vec.encoding value_type_encoding)
      in
      case
        "FKIns"
        lazy_vec
        (function Decode.FKIns vec -> Some vec | _ -> None)
        (fun vec -> FKIns vec)

    let fkout_case =
      let params = scope ["params"] (vector_encoding value_type_encoding) in
      let lazy_vec =
        scope ["lazy_kont"] (Lazy_vec.encoding value_type_encoding)
      in
      case
        "FKOut"
        (tup2 ~flatten:true params lazy_vec)
        (function Decode.FKOut (p, vec) -> Some (p, vec) | _ -> None)
        (fun (p, vec) -> FKOut (p, vec))

    let func_type_encoding =
      let params = scope ["params"] (vector_encoding value_type_encoding) in
      let result = scope ["result"] (vector_encoding value_type_encoding) in
      conv
        (fun (params, result) -> Types.FuncType (params, result))
        (fun (Types.FuncType (params, result)) -> (params, result))
        (tup2 ~flatten:true params result)

    let fkstop_case =
      case
        "FKStop"
        func_type_encoding
        (function Decode.FKStop ft -> Some ft | _ -> None)
        (fun ft -> FKStop ft)

    let tag_encoding = Data_encoding.string |> value []

    let encoding =
      tagged_union
        tag_encoding
        [fkstart_case; fkins_case; fkout_case; fkstop_case]
  end

  module Import = struct
    let impkstart_case =
      let tag = "ImpKStart" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function Decode.ImpKStart -> Some () | _ -> None)
        (fun () -> ImpKStart)

    let impkmodulename_case =
      case
        "ImpKModuleName"
        Name.encoding
        (function Decode.ImpKModuleName n -> Some n | _ -> None)
        (fun n -> ImpKModuleName n)

    let name_encoding = vector_encoding Name.utf8

    let impkitemname_case =
      case
        "ImpKItemName"
        (tup2 ~flatten:true name_encoding Name.encoding)
        (function Decode.ImpKItemName (m, i) -> Some (m, i) | _ -> None)
        (fun (m, i) -> ImpKItemName (m, i))

    let import_encoding =
      conv
        (fun (module_name, item_name, idesc) ->
          Ast.{module_name; item_name; idesc})
        (fun {module_name; item_name; idesc} -> (module_name, item_name, idesc))
        (tup3
           ~flatten:true
           (scope ["module_name"] name_encoding)
           (scope ["item_name"] name_encoding)
           (value ["idesc"] Interpreter_encodings.Ast.import_desc_encoding))

    let impkstop_case =
      case
        "ImpKStop"
        import_encoding
        (function Decode.ImpKStop i -> Some i | _ -> None)
        (fun i -> ImpKStop i)

    let tag_encoding = value [] Data_encoding.string

    let encoding =
      tagged_union
        tag_encoding
        [impkstart_case; impkmodulename_case; impkitemname_case; impkstop_case]
  end
end
