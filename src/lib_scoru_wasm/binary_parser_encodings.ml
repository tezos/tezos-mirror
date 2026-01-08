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
open Tezos_lazy_containers
module V = Instance.Vector
module M = Instance.NameMap
module C = Chunked_byte_vector
open Tezos_tree_encoding

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3566

   Locations should either be dropped or not. *)
let no_region_encoding enc =
  conv (fun s -> Source.(s @@ no_region)) (fun {it; _} -> it) enc

let vector_encoding value_enc =
  Lazy_vector.Int32Vector.encoding (value [] Data_encoding.int32) value_enc

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
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3569

       The stack can be probably encoded in a unique key in the tree,
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
  let buffer_encoding =
    value
      []
      Data_encoding.(
        conv
          (fun b -> (Buffer.contents b, Buffer.length b))
          (fun (content, length) ->
            let b = Buffer.create length in
            Buffer.add_string b content ;
            b)
          (tup2 string int31))

  let nkstart_case =
    case
      "NKStart"
      (value [] Data_encoding.unit)
      (function Decode.NKStart -> Some () | _ -> None)
      (fun () -> Decode.NKStart)

  let nkparse_case =
    let value_enc =
      let pos = value ["pos"] Data_encoding.int31 in
      let buffer = scope ["buffer"] buffer_encoding in
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
      (value [] Data_encoding.string)
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
    let lazy_vec = scope ["ins_kont"] (Lazy_vec.encoding value_type_encoding) in
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

  let fkstop_case =
    case
      "FKStop"
      Wasm_encoding.func_type_encoding
      (function Decode.FKStop ft -> Some ft | _ -> None)
      (fun ft -> FKStop ft)

  let tag_encoding = Data_encoding.string |> value []

  let encoding =
    tagged_union
      tag_encoding
      [fkstart_case; fkins_case; fkout_case; fkstop_case]
end

let name_encoding = value [] Data_encoding.string

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
      (scope ["module_name"] Name.encoding)
      (function Decode.ImpKModuleName n -> Some n | _ -> None)
      (fun n -> ImpKModuleName n)

  let impkitemname_case =
    case
      "ImpKItemName"
      (tup2
         ~flatten:false
         (scope ["module_name"] name_encoding)
         (scope ["item_name"] Name.encoding))
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
    let vec_kont = scope ["vec_kont"] (Lazy_vec.encoding value_type_acc_enc) in
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
    let type_vec = scope ["type_vec"] (Lazy_vec.encoding value_type_acc_enc) in
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
    let body = value ["body"] Interpreter_encodings.Ast.block_label_encoding in
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
      (value [] (Data_encoding.constant tag))
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
             offset_kont_code )
         ->
        EKMode
          {
            left;
            index;
            index_kind;
            early_ref_type;
            offset_kont = (offset_kont, offset_kont_code);
          })

  let ekinitindexed_case =
    let mode = value ["mode"] Interpreter_encodings.Ast.segment_mode_encoding in
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
    let mode = value ["mode"] Interpreter_encodings.Ast.segment_mode_encoding in
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
    let dinit = value ["dinit"] Interpreter_encodings.Ast.data_label_encoding in
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

module Field = struct
  let type_field_encoding =
    scope
      ["module"; "types"]
      (vector_encoding (no_region_encoding Wasm_encoding.func_type_encoding))

  let import_field_encoding =
    scope
      ["module"; "imports"]
      (vector_encoding (no_region_encoding Import.import_encoding))

  let func_field_encoding =
    scope
      ["module"; "funcs"]
      (vector_encoding (value [] Interpreter_encodings.Ast.var_encoding))

  let table_field_encoding =
    scope
      ["module"; "tables"]
      (vector_encoding (value [] Interpreter_encodings.Ast.table_encoding))

  let memory_field_encoding =
    scope
      ["module"; "memories"]
      (vector_encoding (value [] Interpreter_encodings.Ast.memory_encoding))

  let global_field_encoding =
    scope
      ["module"; "globals"]
      (vector_encoding (value [] Interpreter_encodings.Ast.global_encoding))

  let export_field_encoding =
    scope
      ["module"; "exports"]
      (vector_encoding (no_region_encoding Export.export_encoding))

  let start_field_encoding =
    value_option ["module"; "start"] Interpreter_encodings.Ast.start_encoding

  let elem_field_encoding =
    scope
      ["module"; "elem_segments"]
      (vector_encoding (no_region_encoding Elem.elem_encoding))

  let data_count_field_encoding =
    value_option ["module"; "data_count"] Data_encoding.int32

  let code_field_encoding =
    scope ["module"; "code"] (vector_encoding Code.func_encoding)

  let data_field_encoding =
    scope
      ["module"; "data_segments"]
      (vector_encoding (no_region_encoding Data.data_segment_encoding))

  let building_state_encoding =
    conv
      (fun ( types,
             imports,
             vars,
             tables,
             memories,
             globals,
             exports,
             start,
             (elems, data_count, code, datas) )
         ->
        Decode.
          {
            types;
            imports;
            vars;
            tables;
            memories;
            globals;
            exports;
            start;
            elems;
            data_count;
            code;
            datas;
          })
      (fun Decode.
             {
               types;
               imports;
               vars;
               tables;
               memories;
               globals;
               exports;
               start;
               elems;
               data_count;
               code;
               datas;
             }
         ->
        ( types,
          imports,
          vars,
          tables,
          memories,
          globals,
          exports,
          start,
          (elems, data_count, code, datas) ))
      (tup9
         ~flatten:true
         type_field_encoding
         import_field_encoding
         func_field_encoding
         table_field_encoding
         memory_field_encoding
         global_field_encoding
         export_field_encoding
         start_field_encoding
         (tup4
            ~flatten:true
            elem_field_encoding
            data_count_field_encoding
            code_field_encoding
            data_field_encoding))

  (* Only used to encode field_type. *)
  type packed_field_type =
    | FieldType : ('a, 'repr) Decode.field_type -> packed_field_type

  let packed_field_type_encoding =
    let open Decode in
    let type_field_encoding =
      let tag = "TypeField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType TypeField -> Some () | _ -> None)
        (fun () -> FieldType TypeField)
    in
    let import_field_encoding =
      let tag = "ImportField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType ImportField -> Some () | _ -> None)
        (fun () -> FieldType ImportField)
    in
    let func_field_encoding =
      let tag = "FuncField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType FuncField -> Some () | _ -> None)
        (fun () -> FieldType FuncField)
    in
    let table_field_encoding =
      let tag = "TableField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType TableField -> Some () | _ -> None)
        (fun () -> FieldType TableField)
    in
    let memory_field_encoding =
      let tag = "MemoryField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType MemoryField -> Some () | _ -> None)
        (fun () -> FieldType MemoryField)
    in
    let global_field_encoding =
      let tag = "GlobalField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType GlobalField -> Some () | _ -> None)
        (fun () -> FieldType GlobalField)
    in
    let export_field_encoding =
      let tag = "ExportField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType ExportField -> Some () | _ -> None)
        (fun () -> FieldType ExportField)
    in
    let start_field_encoding =
      let tag = "StartField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType StartField -> Some () | _ -> None)
        (fun () -> FieldType StartField)
    in
    let elem_field_encoding =
      let tag = "ElemField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType ElemField -> Some () | _ -> None)
        (fun () -> FieldType ElemField)
    in
    let data_count_field_encoding =
      let tag = "DataCountField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType DataCountField -> Some () | _ -> None)
        (fun () -> FieldType DataCountField)
    in
    let code_field_encoding =
      let tag = "CodeField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType CodeField -> Some () | _ -> None)
        (fun () -> FieldType CodeField)
    in
    let data_field_encoding =
      let tag = "DataField" in
      case
        tag
        (value [] (Data_encoding.constant tag))
        (function FieldType DataField -> Some () | _ -> None)
        (fun () -> FieldType DataField)
    in
    tagged_union
      (value [] Data_encoding.string)
      [
        type_field_encoding;
        import_field_encoding;
        func_field_encoding;
        table_field_encoding;
        memory_field_encoding;
        global_field_encoding;
        export_field_encoding;
        start_field_encoding;
        elem_field_encoding;
        data_count_field_encoding;
        code_field_encoding;
        data_field_encoding;
      ]

  (* Only used to encode lazy vector parameterized by the field type in the
     continuation. *)
  type packed_typed_lazy_vec =
    | TypedLazyVec :
        ('a, Decode.vec_repr) Decode.field_type * 'a Decode.lazy_vec_kont
        -> packed_typed_lazy_vec

  let packed_typed_lazy_vec_encoding =
    let open Decode in
    let type_field_encoding =
      let tag = "TypeField" in
      case
        tag
        (Lazy_vec.raw_encoding type_field_encoding)
        (function TypedLazyVec (TypeField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (TypeField, vec))
    in
    let import_field_encoding =
      let tag = "ImportField" in
      case
        tag
        (Lazy_vec.raw_encoding import_field_encoding)
        (function TypedLazyVec (ImportField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (ImportField, vec))
    in
    let func_field_encoding =
      let tag = "FuncField" in
      case
        tag
        (Lazy_vec.raw_encoding func_field_encoding)
        (function TypedLazyVec (FuncField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (FuncField, vec))
    in
    let table_field_encoding =
      let tag = "TableField" in
      case
        tag
        (Lazy_vec.raw_encoding table_field_encoding)
        (function TypedLazyVec (TableField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (TableField, vec))
    in
    let memory_field_encoding =
      let tag = "MemoryField" in
      case
        tag
        (Lazy_vec.raw_encoding memory_field_encoding)
        (function TypedLazyVec (MemoryField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (MemoryField, vec))
    in
    let global_field_encoding =
      let tag = "GlobalField" in
      case
        tag
        (Lazy_vec.raw_encoding global_field_encoding)
        (function TypedLazyVec (GlobalField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (GlobalField, vec))
    in
    let export_field_encoding =
      let tag = "ExportField" in
      case
        tag
        (Lazy_vec.raw_encoding export_field_encoding)
        (function TypedLazyVec (ExportField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (ExportField, vec))
    in
    let elem_field_encoding =
      let tag = "ElemField" in
      case
        tag
        (Lazy_vec.raw_encoding elem_field_encoding)
        (function TypedLazyVec (ElemField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (ElemField, vec))
    in
    let code_field_encoding =
      let tag = "CodeField" in
      case
        tag
        (Lazy_vec.raw_encoding code_field_encoding)
        (function TypedLazyVec (CodeField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (CodeField, vec))
    in
    let data_field_encoding =
      let tag = "DataField" in
      case
        tag
        (Lazy_vec.raw_encoding data_field_encoding)
        (function TypedLazyVec (DataField, vec) -> Some vec | _ -> None)
        (fun vec -> TypedLazyVec (DataField, vec))
    in
    tagged_union
      (value [] Data_encoding.string)
      [
        type_field_encoding;
        import_field_encoding;
        func_field_encoding;
        table_field_encoding;
        memory_field_encoding;
        global_field_encoding;
        export_field_encoding;
        elem_field_encoding;
        code_field_encoding;
        data_field_encoding;
      ]
end

module Module = struct
  let mkstart_case =
    case
      "MKStart"
      (value [] Data_encoding.unit)
      (function Decode.MKStart -> Some () | _ -> None)
      (fun () -> Decode.MKStart)

  let mkskipcustom_case =
    case
      "MKSkipCustom"
      (option Field.packed_field_type_encoding)
      (function
        | Decode.MKSkipCustom (Some field_type) ->
            Some (Some (Field.FieldType field_type))
        | Decode.MKSkipCustom None -> Some None
        | _ -> None)
      (function
        | None -> MKSkipCustom None
        | Some (FieldType ft) -> MKSkipCustom (Some ft))

  let mkfieldstart_case =
    case
      "MKFieldStart"
      Field.packed_field_type_encoding
      (function
        | Decode.MKFieldStart field_type -> Some (Field.FieldType field_type)
        | _ -> None)
      (fun (FieldType ft) -> MKFieldStart ft)

  let mkfield_case =
    case
      "MKField"
      (tup2 ~flatten:true Field.packed_typed_lazy_vec_encoding Size.encoding)
      (function
        | Decode.MKField (field_type, size, vec) ->
            Some (Field.TypedLazyVec (field_type, vec), size)
        | _ -> None)
      (fun (TypedLazyVec (ft, vec), size) -> MKField (ft, size, vec))

  let mkelaboratefunc_case =
    let func_types = Field.func_field_encoding in
    let func_bodies = Field.code_field_encoding in
    let func_kont =
      scope ["func_kont"] (Lazy_vec.encoding Code.func_encoding)
    in
    let instr_kont =
      scope
        ["instr_kont"]
        (option
           (Lazy_vec.encoding
              (Lazy_vec.encoding Wasm_encoding.instruction_encoding)))
    in
    let no_datas_in_func = value ["no-datas-in-funcs"] Data_encoding.bool in
    case
      "MKElaborateFunc"
      (tup5
         ~flatten:true
         func_types
         func_bodies
         func_kont
         instr_kont
         no_datas_in_func)
      (function
        | Decode.MKElaborateFunc
            (func_types, func_bodies, func_kont, instr_kont, no_datas_in_func)
          ->
            Some
              (func_types, func_bodies, func_kont, instr_kont, no_datas_in_func)
        | _ -> None)
      (fun (func_types, func_bodies, func_kont, instr_kont, no_datas_in_func) ->
        MKElaborateFunc
          (func_types, func_bodies, func_kont, instr_kont, no_datas_in_func))

  let module_funcs_encoding =
    scope ["module"; "funcs"] (vector_encoding Code.func_encoding)

  let mkbuild_case =
    let no_datas_in_func = value ["no-datas-in-funcs"] Data_encoding.bool in
    case
      "MKBuild"
      (tup2 ~flatten:true (option module_funcs_encoding) no_datas_in_func)
      (function
        | Decode.MKBuild (funcs, no_datas_in_func) ->
            Some (funcs, no_datas_in_func)
        | _ -> None)
      (fun (funcs, no_datas_in_func) -> MKBuild (funcs, no_datas_in_func))

  let mktypes_case =
    let func_type_kont = scope ["func_type_kont"] Func_type.encoding in
    let pos = value ["pos"] Data_encoding.int31 in
    let size = scope ["size"] Size.encoding in
    let type_accumulator = Lazy_vec.raw_encoding Field.type_field_encoding in
    case
      "MKTypes"
      (tup4 ~flatten:true func_type_kont pos size type_accumulator)
      (function
        | Decode.MKTypes (func_type_kont, pos, size, types_acc) ->
            Some (func_type_kont, pos, size, types_acc)
        | _ -> None)
      (fun (func_type_kont, pos, size, types_acc) ->
        MKTypes (func_type_kont, pos, size, types_acc))

  let mkimport_case =
    let import_kont = scope ["import_kont"] Import.encoding in
    let pos = value ["pos"] Data_encoding.int31 in
    let size = scope ["size"] Size.encoding in
    let import_accumulator =
      Lazy_vec.raw_encoding Field.import_field_encoding
    in
    case
      "MKImport"
      (tup4 ~flatten:true import_kont pos size import_accumulator)
      (function
        | Decode.MKImport (import_kont, pos, size, import_acc) ->
            Some (import_kont, pos, size, import_acc)
        | _ -> None)
      (fun (import_kont, pos, size, import_acc) ->
        MKImport (import_kont, pos, size, import_acc))

  let mkexport_case =
    let export_kont = scope ["export_kont"] Export.encoding in
    let pos = value ["pos"] Data_encoding.int31 in
    let size = scope ["size"] Size.encoding in
    let export_accumulator =
      Lazy_vec.raw_encoding Field.export_field_encoding
    in
    case
      "MKExport"
      (tup4 ~flatten:true export_kont pos size export_accumulator)
      (function
        | Decode.MKExport (export_kont, pos, size, export_acc) ->
            Some (export_kont, pos, size, export_acc)
        | _ -> None)
      (fun (export_kont, pos, size, export_acc) ->
        MKExport (export_kont, pos, size, export_acc))

  let mkglobal_case =
    let global_type =
      value ["global_type"] Interpreter_encodings.Types.global_type_encoding
    in
    let block_kont = scope ["block_kont"] Block.encoding in
    let pos = value ["pos"] Data_encoding.int31 in
    let size = scope ["size"] Size.encoding in
    let global_accumulator =
      Lazy_vec.raw_encoding Field.global_field_encoding
    in
    case
      "MKGlobal"
      (tup5 ~flatten:true global_type pos block_kont size global_accumulator)
      (function
        | Decode.MKGlobal (global_type, pos, block_kont, size, global_acc) ->
            Some (global_type, pos, block_kont, size, global_acc)
        | _ -> None)
      (fun (global_type, pos, block_kont, size, global_acc) ->
        MKGlobal (global_type, pos, block_kont, size, global_acc))

  let mkdata_case =
    let data_kont = scope ["data_kont"] Data.encoding in
    let pos = value ["pos"] Data_encoding.int31 in
    let size = scope ["size"] Size.encoding in
    let data_accumulator = Lazy_vec.raw_encoding Field.data_field_encoding in
    case
      "MKData"
      (tup4 ~flatten:true data_kont pos size data_accumulator)
      (function
        | Decode.MKData (data_kont, pos, size, data_acc) ->
            Some (data_kont, pos, size, data_acc)
        | _ -> None)
      (fun (data_kont, pos, size, data_acc) ->
        MKData (data_kont, pos, size, data_acc))

  let mkelem_case =
    let elem_kont = scope ["elem_kont"] Elem.encoding in
    let pos = value ["pos"] Data_encoding.int31 in
    let size = scope ["size"] Size.encoding in
    let elem_accumulator = Lazy_vec.raw_encoding Field.elem_field_encoding in
    case
      "MKElem"
      (tup4 ~flatten:true elem_kont pos size elem_accumulator)
      (function
        | Decode.MKElem (elem_kont, pos, size, elem_acc) ->
            Some (elem_kont, pos, size, elem_acc)
        | _ -> None)
      (fun (elem_kont, pos, size, elem_acc) ->
        MKElem (elem_kont, pos, size, elem_acc))

  let mkcode_case =
    let code_kont = scope ["code_kont"] Code.encoding in
    let pos = value ["pos"] Data_encoding.int31 in
    let size = scope ["size"] Size.encoding in
    let code_accumulator = Lazy_vec.raw_encoding Field.code_field_encoding in
    case
      "MKCode"
      (tup4 ~flatten:true code_kont pos size code_accumulator)
      (function
        | Decode.MKCode (code_kont, pos, size, code_acc) ->
            Some (code_kont, pos, size, code_acc)
        | _ -> None)
      (fun (code_kont, pos, size, code_acc) ->
        MKCode (code_kont, pos, size, code_acc))

  let module_encoding =
    let open Field in
    conv
      (fun ( types,
             globals,
             tables,
             memories,
             funcs,
             start,
             elems,
             datas,
             (imports, exports, allocations) )
         ->
        Ast.
          {
            types;
            tables;
            memories;
            globals;
            funcs;
            imports;
            exports;
            elems;
            datas;
            start;
            allocations;
          })
      (fun {
             types;
             tables;
             memories;
             globals;
             funcs;
             imports;
             exports;
             elems;
             datas;
             start;
             allocations;
           }
         ->
        ( types,
          globals,
          tables,
          memories,
          funcs,
          start,
          elems,
          datas,
          (imports, exports, allocations) ))
      (tup9
         ~flatten:true
         type_field_encoding
         global_field_encoding
         table_field_encoding
         memory_field_encoding
         module_funcs_encoding
         start_field_encoding
         elem_field_encoding
         data_field_encoding
         (tup3
            ~flatten:true
            import_field_encoding
            export_field_encoding
            Wasm_encoding.allocations_encoding))

  let mkstop_case =
    case
      "MKStop"
      (no_region_encoding module_encoding)
      (function Decode.MKStop m -> Some m | _ -> None)
      (fun m -> MKStop m)

  let encoding =
    tagged_union
      (value [] Data_encoding.string)
      [
        mkstart_case;
        mkskipcustom_case;
        mkfieldstart_case;
        mkfield_case;
        mkelaboratefunc_case;
        mkbuild_case;
        mkstop_case;
        mktypes_case;
        mkimport_case;
        mkexport_case;
        mkglobal_case;
        mkelem_case;
        mkdata_case;
        mkelem_case;
        mkcode_case;
      ]
end

module Building_state = struct
  let types_encoding =
    vector_encoding (no_region_encoding Wasm_encoding.func_type_encoding)

  let imports_encoding =
    vector_encoding (no_region_encoding Import.import_encoding)

  let vars_encoding =
    vector_encoding (value [] Interpreter_encodings.Ast.var_encoding)

  let tables_encoding =
    vector_encoding (value [] Interpreter_encodings.Ast.table_encoding)

  let memories_encoding =
    vector_encoding (value [] Interpreter_encodings.Ast.memory_encoding)

  let globals_encoding =
    vector_encoding (value [] Interpreter_encodings.Ast.global_encoding)

  let exports_encoding =
    vector_encoding (no_region_encoding Export.export_encoding)

  let start_encoding = value_option [] Interpreter_encodings.Ast.start_encoding

  let elems_encoding = vector_encoding (no_region_encoding Elem.elem_encoding)

  let code_encoding =
    vector_encoding (no_region_encoding Wasm_encoding.func'_encoding)

  let datas_encoding =
    vector_encoding (no_region_encoding Data.data_segment_encoding)

  let encoding =
    conv
      (fun ( types,
             imports,
             vars,
             tables,
             memories,
             globals,
             exports,
             start,
             (elems, data_count, code, datas) )
         ->
        Decode.
          {
            types;
            imports;
            vars;
            tables;
            memories;
            globals;
            exports;
            start;
            elems;
            data_count;
            code;
            datas;
          })
      (fun {
             types;
             imports;
             vars;
             tables;
             memories;
             globals;
             exports;
             start;
             elems;
             data_count;
             code;
             datas;
           }
         ->
        ( types,
          imports,
          vars,
          tables,
          memories,
          globals,
          exports,
          start,
          (elems, data_count, code, datas) ))
      (tup9
         ~flatten:true
         (scope ["types"] types_encoding)
         (scope ["imports"] imports_encoding)
         (scope ["vars"] vars_encoding)
         (scope ["tables"] tables_encoding)
         (scope ["memories"] memories_encoding)
         (scope ["globals"] globals_encoding)
         (scope ["exports"] exports_encoding)
         (scope ["start"] start_encoding)
         (tup4
            ~flatten:true
            (scope ["elems"] elems_encoding)
            (scope ["data_count"] (value_option [] Data_encoding.int32))
            (scope ["code"] code_encoding)
            (scope ["datas"] datas_encoding)))
end

module Decode = struct
  let encoding =
    conv
      (fun ( building_state,
             module_kont,
             allocation_state,
             stream_pos,
             stream_name )
         ->
        Decode.
          {
            building_state;
            module_kont;
            allocation_state;
            stream_pos;
            stream_name;
          })
      (fun {
             building_state;
             module_kont;
             allocation_state;
             stream_pos;
             stream_name;
           }
         ->
        (building_state, module_kont, allocation_state, stream_pos, stream_name))
    @@ tup5
         ~flatten:true
         (scope ["building_state"] Building_state.encoding)
         (scope ["module_kont"] Module.encoding)
         (scope ["allocation_state"] Wasm_encoding.allocations_encoding)
         (value ["stream_pos"] Data_encoding.int31)
         (value ["stream_name"] Data_encoding.string)
end
