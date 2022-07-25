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

  module Lazy_stack = struct
    let encoding value_enc =
      (* TODO: The stack can be probably encoded in a unique key in the tree,
         since it is never used concurrently. *)
      let offset = value ["length"] Data_encoding.int32 in
      let vector = scope ["vector"] (vector_encoding value_enc) in
      conv
        (fun (length, vector) -> Decode.LazyStack {length; vector})
        (fun (LazyStack {length; vector}) -> (length, vector))
        (tup2 ~flatten:true offset vector)
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

  let name_encoding = vector_encoding Name.utf8

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

  module Export = struct
    let expkstart_case =
      let tag = "ExpKStart" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function Decode.ExpKStart -> Some () | _ -> None)
        (fun () -> ExpKStart)

    let expkname_case =
      case
        "ExpKName"
        Name.encoding
        (function Decode.ExpKName n -> Some n | _ -> None)
        (fun n -> ExpKName n)

    let export_encoding =
      conv
        (fun (name, edesc) -> Ast.{name; edesc})
        (fun {name; edesc} -> (name, edesc))
        (tup2
           ~flatten:true
           (scope ["name"] name_encoding)
           (value ["edesc"] Interpreter_encodings.Ast.export_desc_encoding))

    let expkstop_case =
      case
        "ExpKStop"
        export_encoding
        (function Decode.ExpKStop e -> Some e | _ -> None)
        (fun e -> ExpKStop e)

    let tags_encoding = value [] Data_encoding.string

    let encoding =
      tagged_union tags_encoding [expkstart_case; expkname_case; expkstop_case]
  end

  module Size = struct
    let encoding =
      conv
        (fun (size, start) -> Decode.{size; start})
        (fun {size; start} -> (size, start))
        (tup2
           ~flatten:true
           (value ["size"] Data_encoding.int31)
           (value ["start"] Data_encoding.int31))
  end

  module Instr_block = struct
    let stop_case =
      case
        "IKStop"
        (value [] Interpreter_encodings.Ast.block_label_encoding)
        (function Decode.IKStop lbl -> Some lbl | _ -> None)
        (fun lbl -> IKStop lbl)

    let next_case =
      case
        "IKNext"
        (value [] Interpreter_encodings.Ast.block_label_encoding)
        (function Decode.IKNext lbl -> Some lbl | _ -> None)
        (fun lbl -> IKNext lbl)

    let block_case =
      let encoding =
        tup2
          ~flatten:true
          (value ["type"] Interpreter_encodings.Ast.block_type_encoding)
          (value ["pos"] Data_encoding.int31)
      in
      case
        "IKBlock"
        encoding
        (function Decode.IKBlock (ty, i) -> Some (ty, i) | _ -> None)
        (fun (ty, i) -> IKBlock (ty, i))

    let loop_case =
      let encoding =
        tup2
          ~flatten:true
          (value ["type"] Interpreter_encodings.Ast.block_type_encoding)
          (value ["pos"] Data_encoding.int31)
      in
      case
        "IKLoop"
        encoding
        (function Decode.IKLoop (ty, i) -> Some (ty, i) | _ -> None)
        (fun (ty, i) -> IKLoop (ty, i))

    let if1_case =
      let encoding =
        tup2
          ~flatten:true
          (value ["type"] Interpreter_encodings.Ast.block_type_encoding)
          (value ["pos"] Data_encoding.int31)
      in
      case
        "IKIf1"
        encoding
        (function Decode.IKIf1 (ty, i) -> Some (ty, i) | _ -> None)
        (fun (ty, i) -> IKIf1 (ty, i))

    let if2_case =
      let encoding =
        tup3
          ~flatten:true
          (value ["type"] Interpreter_encodings.Ast.block_type_encoding)
          (value ["pos"] Data_encoding.int31)
          (value ["else"] Interpreter_encodings.Ast.block_label_encoding)
      in
      case
        "IKIf2"
        encoding
        (function
          | Decode.IKIf2 (ty, i, else_lbl) -> Some (ty, i, else_lbl) | _ -> None)
        (fun (ty, i, else_lbl) -> IKIf2 (ty, i, else_lbl))

    let encoding =
      tagged_union
        (value [] Data_encoding.string)
        [stop_case; next_case; block_case; loop_case; if1_case; if2_case]
  end

  module Block = struct
    let start_case =
      let tag = "BlockStart" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function Decode.BlockStart -> Some () | _ -> None)
        (fun _ -> BlockStart)

    let parse_case =
      case
        "BlockParse"
        (scope [] (Lazy_stack.encoding Instr_block.encoding))
        (function Decode.BlockParse ik -> Some ik | _ -> None)
        (fun ik -> BlockParse ik)

    let stop_case =
      case
        "BlockStop"
        (value [] Interpreter_encodings.Ast.block_label_encoding)
        (function Decode.BlockStop lbl -> Some lbl | _ -> None)
        (fun lbl -> BlockStop lbl)

    let encoding =
      tagged_union
        (value [] Data_encoding.string)
        [start_case; parse_case; stop_case]
  end

  module Code = struct
    let value_type_acc_enc =
      let occurences = value ["occurences"] Data_encoding.int32 in
      let value_type =
        value ["type"] Interpreter_encodings.Types.value_type_encoding
      in
      tup2 ~flatten:true occurences value_type

    let ckstart_case =
      let tag = "CKStart" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function Decode.CKStart -> Some () | _ -> None)
        (fun () -> CKStart)

    let cklocalsparse_case =
      let left = value ["left"] Data_encoding.int31 in
      let size = scope ["size"] Size.encoding in
      let pos = value ["pos"] Data_encoding.int31 in
      let vec_kont =
        scope ["vec_kont"] (Lazy_vec.encoding value_type_acc_enc)
      in
      let locals_size = value ["locals_size"] Data_encoding.int64 in
      case
        "CKLocalsParse"
        (tup5 ~flatten:true left size pos vec_kont locals_size)
        (function
          | Decode.CKLocalsParse {left; size; pos; vec_kont; locals_size} ->
              Some (left, size, pos, vec_kont, locals_size)
          | _ -> None)
        (fun (left, size, pos, vec_kont, locals_size) ->
          Decode.CKLocalsParse {left; size; pos; vec_kont; locals_size})

    let cklocalsaccumulate_case =
      let left = value ["left"] Data_encoding.int31 in
      let size = scope ["size"] Size.encoding in
      let pos = value ["pos"] Data_encoding.int31 in
      let type_vec =
        scope ["type_vec"] (Lazy_vec.encoding value_type_acc_enc)
      in
      let curr_type = scope ["curr_type"] (option value_type_acc_enc) in
      let vec_kont =
        scope ["vec_kont"] (Lazy_vec.encoding Func_type.value_type_encoding)
      in

      case
        "CKLocalsAccumulate"
        (tup6 ~flatten:true left size pos type_vec curr_type vec_kont)
        (function
          | Decode.CKLocalsAccumulate
              {left; size; pos; type_vec; curr_type; vec_kont} ->
              Some (left, size, pos, type_vec, curr_type, vec_kont)
          | _ -> None)
        (fun (left, size, pos, type_vec, curr_type, vec_kont) ->
          Decode.CKLocalsAccumulate
            {left; size; pos; type_vec; curr_type; vec_kont})

    let ckbody_case =
      let left = value ["left"] Data_encoding.int31 in
      let size = scope ["size"] Size.encoding in
      let locals =
        scope ["locals"] (vector_encoding Func_type.value_type_encoding)
      in
      let const_kont = scope ["const_kont"] Block.encoding in
      case
        "CKBody"
        (tup4 ~flatten:true left size locals const_kont)
        (function
          | Decode.CKBody {left; size; locals; const_kont} ->
              Some (left, size, locals, const_kont)
          | _ -> None)
        (fun (left, size, locals, const_kont) ->
          CKBody {left; size; locals; const_kont})

    let func_encoding =
      let ftype = value ["ftype"] Interpreter_encodings.Ast.var_encoding in
      let locals =
        scope ["locals"] (vector_encoding Func_type.value_type_encoding)
      in
      let body =
        value ["body"] Interpreter_encodings.Ast.block_label_encoding
      in
      conv
        (fun (ftype, locals, body) ->
          Source.(Ast.{ftype; locals; body} @@ no_region))
        (fun {it = {ftype; locals; body}; _} -> (ftype, locals, body))
        (tup3 ~flatten:true ftype locals body)

    let ckstop_case =
      case
        "CKStop"
        func_encoding
        (function Decode.CKStop func -> Some func | _ -> None)
        (fun func -> CKStop func)

    let encoding =
      tagged_union
        (value [] Data_encoding.string)
        [
          ckstart_case;
          cklocalsparse_case;
          cklocalsaccumulate_case;
          ckbody_case;
          ckstop_case;
        ]
  end

  module Elem = struct
    let region enc =
      Data_encoding.conv
        (fun p -> p.Source.it)
        (fun v -> Source.(v @@ no_region))
        enc

    let index_kind_encoding =
      Data_encoding.string_enum
        [("Indexed", Decode.Indexed); ("Const", Decode.Const)]

    let ekstart_case =
      let tag = "EKStart" in
      case
        tag
        (value [] (Data_encoding.constant "tag"))
        (function Decode.EKStart -> Some () | _ -> None)
        (fun () -> EKStart)

    let ekmode_case =
      let left = value ["left"] Data_encoding.int31 in
      let index =
        value
          ["index"]
          (Interpreter_encodings.Source.phrase_encoding Data_encoding.int32)
      in
      let index_kind = value ["index_kind"] index_kind_encoding in
      let early_ref_type =
        value_option
          ["early_ref_type"]
          Interpreter_encodings.Types.ref_type_encoding
      in
      let offset_kont = value ["offset_kont"] Data_encoding.int31 in
      let offset_kont_code = scope ["offset_kont_code"] Block.encoding in
      case
        "EKMode"
        (tup6
           ~flatten:true
           left
           index
           index_kind
           early_ref_type
           offset_kont
           offset_kont_code)
        (function
          | Decode.EKMode
              {
                left;
                index;
                index_kind;
                early_ref_type;
                offset_kont = offset_kont, offset_kont_code;
              } ->
              Some
                ( left,
                  index,
                  index_kind,
                  early_ref_type,
                  offset_kont,
                  offset_kont_code )
          | _ -> None)
        (fun ( left,
               index,
               index_kind,
               early_ref_type,
               offset_kont,
               offset_kont_code ) ->
          EKMode
            {
              left;
              index;
              index_kind;
              early_ref_type;
              offset_kont = (offset_kont, offset_kont_code);
            })

    let ekinitindexed_case =
      let mode =
        value ["mode"] Interpreter_encodings.Ast.segment_mode_encoding
      in
      let ref_type =
        value ["ref_type"] Interpreter_encodings.Types.ref_type_encoding
      in
      let einit_vec =
        scope
          ["einit_vec"]
          (Lazy_vec.encoding
             (value
                []
                (Interpreter_encodings.Source.phrase_encoding
                   Interpreter_encodings.Ast.block_label_encoding)))
      in
      case
        "EKInitIndexed"
        (tup3 ~flatten:true mode ref_type einit_vec)
        (function
          | Decode.EKInitIndexed {mode; ref_type; einit_vec} ->
              Some (mode, ref_type, einit_vec)
          | _ -> None)
        (fun (mode, ref_type, einit_vec) ->
          EKInitIndexed {mode; ref_type; einit_vec})

    let ekinitconst_case =
      let mode =
        value ["mode"] Interpreter_encodings.Ast.segment_mode_encoding
      in
      let ref_type =
        value ["ref_type"] Interpreter_encodings.Types.ref_type_encoding
      in
      let einit_vec =
        scope
          ["einit_vec"]
          (Lazy_vec.encoding
             (value
                []
                (Interpreter_encodings.Source.phrase_encoding
                   Interpreter_encodings.Ast.block_label_encoding)))
      in
      let einit_kont_pos = value ["einit_kont_pos"] Data_encoding.int31 in
      let einit_kont_block = scope ["einit_kont_block"] Block.encoding in
      case
        "EKInitConst"
        (tup5
           ~flatten:true
           mode
           ref_type
           einit_vec
           einit_kont_pos
           einit_kont_block)
        (function
          | Decode.EKInitConst
              {mode; ref_type; einit_vec; einit_kont = pos, block} ->
              Some (mode, ref_type, einit_vec, pos, block)
          | _ -> None)
        (fun (mode, ref_type, einit_vec, pos, block) ->
          EKInitConst {mode; ref_type; einit_vec; einit_kont = (pos, block)})

    let elem_encoding =
      let etype =
        value ["ref_type"] Interpreter_encodings.Types.ref_type_encoding
      in
      let einit =
        scope
          ["einit"]
          (vector_encoding (value [] Interpreter_encodings.Ast.const_encoding))
      in
      let emode =
        value ["mode"] Interpreter_encodings.Ast.segment_mode_encoding
      in
      conv
        (fun (etype, einit, emode) -> Ast.{etype; einit; emode})
        (fun Ast.{etype; einit; emode} -> (etype, einit, emode))
        (tup3 ~flatten:true etype einit emode)

    let ekstop_case =
      case
        "EKStop"
        elem_encoding
        (function Decode.EKStop elem -> Some elem | _ -> None)
        (fun elem -> EKStop elem)

    let encoding =
      tagged_union
        (value [] Data_encoding.string)
        [
          ekstart_case;
          ekmode_case;
          ekinitindexed_case;
          ekinitconst_case;
          ekstop_case;
        ]
  end

  module Data = struct
    let dkstart_case =
      let tag = "DKStart" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function Decode.DKStart -> Some () | _ -> None)
        (fun () -> DKStart)

    let dkmode_case =
      let left = value ["left"] Data_encoding.int31 in
      let index =
        value
          ["index"]
          (Interpreter_encodings.Source.phrase_encoding Data_encoding.int32)
      in
      let offset_kont = value ["offset_kont"] Data_encoding.int31 in
      let offset_kont_code = scope ["offset_kont_code"] Block.encoding in
      case
        "DKMode"
        (tup4 ~flatten:true left index offset_kont offset_kont_code)
        (function
          | Decode.DKMode {left; index; offset_kont = pos, block} ->
              Some (left, index, pos, block)
          | _ -> None)
        (fun (left, index, pos, block) ->
          DKMode {left; index; offset_kont = (pos, block)})

    let dkinit_case =
      let dmode =
        value ["dmode"] Interpreter_encodings.Ast.segment_mode_encoding
      in
      let init_kont = scope ["init_kont"] Byte_vector.encoding in
      case
        "DKInit"
        (tup2 ~flatten:true dmode init_kont)
        (function
          | Decode.DKInit {dmode; init_kont} -> Some (dmode, init_kont)
          | _ -> None)
        (fun (dmode, init_kont) -> DKInit {dmode; init_kont})

    let data_segment_encoding =
      let dmode =
        value ["dmode"] Interpreter_encodings.Ast.segment_mode_encoding
      in
      let dinit =
        value ["dinit"] Interpreter_encodings.Ast.data_label_encoding
      in
      conv
        (fun (dinit, dmode) -> Ast.{dinit; dmode})
        (fun {dinit; dmode} -> (dinit, dmode))
        (tup2 ~flatten:true dinit dmode)

    let dkstop_case =
      case
        "DKStop"
        data_segment_encoding
        (function Decode.DKStop data_segment -> Some data_segment | _ -> None)
        (fun data_segment -> DKStop data_segment)

    let encoding =
      tagged_union
        (value [] Data_encoding.string)
        [dkstart_case; dkmode_case; dkinit_case; dkstop_case]
  end
end
