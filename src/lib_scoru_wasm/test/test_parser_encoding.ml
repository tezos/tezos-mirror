(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** Testing
    -------
    Component:    Tree_encoding_decoding
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Parser Encodings$"
    Subject:      Parser encoding tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
module Parser = Binary_parser_encodings

module Utils = struct
  module V = Lazy_vector.Int32Vector
  module C = Chunked_byte_vector
  include Encodings_util
  include Test_encodings_util
  include Tezos_tree_encoding
end

module Byte_vector = struct
  open Utils

  let gen_chunked_byte_vector =
    let open QCheck2.Gen in
    let+ values = string in
    C.of_string values

  let gen_buffer =
    let open QCheck2.Gen in
    let* buffer = Ast_generators.data_label_gen in
    let* length = int64 in
    let+ offset = int64 in
    (buffer, offset, length)

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.VKStart in
    let read =
      let+ buffer, offset, length = gen_buffer in
      Decode.VKRead (buffer, offset, length)
    in
    let stop =
      let+ vec = Ast_generators.data_label_gen in
      Decode.VKStop vec
    in
    oneof [start; read; stop]

  let check_buffer (buffer, offset, length) (buffer', offset', length') =
    let open Lwt_result_syntax in
    return
      (buffer = buffer' && Int64.equal offset offset'
     && Int64.equal length length')

  let check_vector vector vector' =
    let open Lwt_result_syntax in
    assert (C.length vector = C.length vector') ;
    let*! str = C.to_string vector in
    let*! str' = C.to_string vector' in
    return (String.equal str str')

  let check bv bv' =
    match (bv, bv') with
    | Decode.VKStart, Decode.VKStart -> Lwt.return_ok true
    | VKRead (buffer, offset, length), VKRead (buffer', offset', length') ->
        check_buffer (buffer, offset, length) (buffer', offset', length')
    | VKStop label, VKStop label' -> Lwt.return_ok (label = label')
    | _, _ -> Lwt.return_ok false

  let tests =
    [
      tztest
        "Byte_vector"
        `Quick
        (make_test Parser.Byte_vector.encoding gen check);
    ]
end

(* Lazy_vector generators *)
module Vec = struct
  open Utils

  let gen gen_values =
    let open QCheck2.Gen in
    let* length = int_range 1 100 in
    let* values = list_repeat length gen_values in
    let vec =
      List.fold_left_i
        (fun index vec value -> V.set (Int32.of_int index) value vec)
        (V.create (Int32.of_int length))
        values
    in
    return vec

  (* Vectors will always be of same type, but some GADTs usage can
     make them virtually of different type. See
     {!Module.check_field_type_value} and it's usage when checking
     equality of two [MKField] states. *)
  let check_possibly_different (eq_value : 'a -> 'b -> (bool, _) result Lwt.t)
      (vector : 'a V.t) (vector' : 'b V.t) =
    let open Lwt_result_syntax in
    assert (V.num_elements vector = V.num_elements vector') ;
    let*! eq_s =
      List.init_es
        ~when_negative_length:()
        (Int32.to_int (V.num_elements vector) - 1)
        (fun index ->
          let*! v = V.get (Int32.of_int index) vector in
          let*! v' = V.get (Int32.of_int index) vector' in
          eq_value v v')
    in
    match eq_s with
    | Ok b -> return (List.for_all Stdlib.(( = ) true) b)
    | Error () -> return false

  (* Checks two vectors are equivalent. *)
  let check (eq_value : 'a -> 'a -> (bool, _) result Lwt.t) (vector : 'a V.t)
      (vector' : 'a V.t) =
    check_possibly_different eq_value vector vector'

  let tests =
    let eq x y = Lwt.return_ok (Int32.equal x y) in
    [
      tztest
        "Vec"
        `Quick
        (make_test
           Parser.(vector_encoding (value [] Data_encoding.int32))
           (gen QCheck2.Gen.int32)
           (check eq));
    ]
end

(* Generators for Lazy_vec, similar to {!Vec}. *)
module LazyVec = struct
  open Utils

  let gen_with_vec gen_vec =
    let open QCheck2.Gen in
    let* vector = gen_vec in
    let* offset = Qcheck2_helpers.int32_range_gen 0l (V.num_elements vector) in
    return (Decode.LazyVec {vector; offset})

  let gen gen_values = gen_with_vec (Vec.gen gen_values)

  let check eq_value (Decode.LazyVec {vector; offset})
      (Decode.LazyVec {vector = vector'; offset = offset'}) =
    let open Lwt_result_syntax in
    let* eq_lzvecs = Vec.check eq_value vector vector' in
    return (eq_lzvecs && offset = offset')

  let check_possibly_different eq_value (Decode.LazyVec {vector; offset})
      (Decode.LazyVec {vector = vector'; offset = offset'}) =
    let open Lwt_result_syntax in
    let* eq_lzvecs = Vec.check_possibly_different eq_value vector vector' in
    return (eq_lzvecs && offset = offset')

  let tests =
    let eq x y = Lwt.return_ok (Int32.equal x y) in
    [
      tztest
        "LazyVec"
        `Quick
        (make_test
           Parser.Lazy_vec.(encoding (value [] Data_encoding.int32))
           (gen QCheck2.Gen.int32)
           (check eq));
    ]
end

module Names = struct
  open Utils

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.NKStart in
    let parse =
      let* length = int_range 0 256 in
      let offset = int_range 0 length in
      let+ contents = string_size offset in
      let buffer = Buffer.create length in
      Buffer.add_string buffer contents ;
      Decode.NKParse (String.length contents, buffer, length)
    in
    let stop =
      let offset = int_range 0 256 in
      let+ buffer = string_size offset in
      Decode.NKStop buffer
    in
    oneof [start; parse; stop]

  let check ns ns' =
    match (ns, ns') with
    | Decode.NKStart, Decode.NKStart -> true
    | NKParse (offset, buffer, length), NKParse (offset', buffer', length') ->
        Buffer.contents buffer = Buffer.contents buffer'
        && Buffer.length buffer = Buffer.length buffer'
        && offset = offset' && length = length'
    | NKStop s, NKStop s' -> String.equal s s'
    | _, _ -> false

  let check ns ns' = Lwt_result.return (check ns ns')

  let tests = [tztest "Names" `Quick (make_test Parser.Name.encoding gen check)]
end

module Func_type = struct
  open Utils

  let func_type_gen =
    let open QCheck2.Gen in
    let* ins = Vec.gen Ast_generators.value_type_gen in
    let+ out = Vec.gen Ast_generators.value_type_gen in
    Types.FuncType (ins, out)

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.FKStart in
    let ins =
      let+ ins = LazyVec.gen Ast_generators.value_type_gen in
      Decode.FKIns ins
    in
    let out =
      let* ins = Vec.gen Ast_generators.value_type_gen in
      let+ out = LazyVec.gen Ast_generators.value_type_gen in
      Decode.FKOut (ins, out)
    in
    let stop =
      let+ ft = func_type_gen in
      Decode.FKStop ft
    in
    oneof [start; ins; out; stop]

  let func_type_check (Types.FuncType (ins, out)) (Types.FuncType (ins', out'))
      =
    let open Lwt_result_syntax in
    let eq_value_types t t' = return Stdlib.(t = t') in
    let* eq_ins = Vec.check eq_value_types ins ins' in
    let+ eq_out = Vec.check eq_value_types out out' in
    eq_ins && eq_out

  let check fk fk' =
    let open Lwt_result_syntax in
    let eq_value_types t t' = return Stdlib.(t = t') in
    match (fk, fk') with
    | Decode.FKStart, Decode.FKStart -> return true
    | FKIns ins, FKIns ins' -> LazyVec.check eq_value_types ins ins'
    | FKOut (ins, out), FKOut (ins', out') ->
        let* eq_ins = Vec.check eq_value_types ins ins' in
        let+ eq_out = LazyVec.check eq_value_types out out' in
        eq_ins && eq_out
    | FKStop ft, FKStop ft' -> func_type_check ft ft'
    | _, _ -> return false

  let tests =
    [tztest "Func_type" `Quick (make_test Parser.Func_type.encoding gen check)]
end

module Imports = struct
  open Utils

  let import_gen =
    let open QCheck2.Gen in
    let* modl = string in
    let* item = string in
    let+ idesc = Ast_generators.import_desc_gen in
    Ast.{module_name = modl; item_name = item; idesc}

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.ImpKStart in
    let item_name =
      let+ modl = Names.gen in
      Decode.ImpKModuleName modl
    in
    let desc =
      let* modl = string in
      let+ item = Names.gen in
      Decode.ImpKItemName (modl, item)
    in
    let stop =
      let+ import = import_gen in
      Decode.ImpKStop import
    in
    oneof [start; item_name; desc; stop]

  let import_check import import' =
    import.Ast.module_name = import'.Ast.module_name
    && import.item_name = import'.item_name
    && import.idesc = import'.idesc

  let check import import' =
    let open Lwt_result_syntax in
    match (import, import') with
    | Decode.ImpKStart, Decode.ImpKStart -> return true
    | ImpKModuleName m, ImpKModuleName m' -> Names.check m m'
    | ImpKItemName (m, i), ImpKItemName (m', i') ->
        let* eq_items = Names.check i i' in
        return (m = m' && eq_items)
    | ImpKStop imp, ImpKStop imp' -> return (import_check imp imp')
    | _, _ -> return false

  let tests =
    [tztest "Imports" `Quick (make_test Parser.Import.encoding gen check)]
end

module LazyStack = struct
  open Utils

  let gen gen_values =
    let open QCheck2.Gen in
    let* vector = Vec.gen gen_values in
    let* length = Qcheck2_helpers.int32_range_gen 0l (V.num_elements vector) in
    return (Decode.LazyStack {vector; length})

  let check eq_value (Decode.LazyStack {vector; length})
      (Decode.LazyStack {vector = vector'; length = length'}) =
    let open Lwt_result_syntax in
    let* eq_lzs = Vec.check eq_value vector vector' in
    return (eq_lzs && length = length')

  let tests =
    let eq x y = Lwt.return_ok (Int32.equal x y) in
    [
      tztest
        "LazyStack"
        `Quick
        (make_test
           Parser.Lazy_stack.(encoding (value [] Data_encoding.int32))
           (gen QCheck2.Gen.int32)
           (check eq));
    ]
end

module Exports = struct
  open Utils

  let export_gen =
    let open QCheck2.Gen in
    let* name = string in
    let+ edesc = Ast_generators.export_desc_gen in
    Ast.{name; edesc}

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.ExpKStart in
    let name =
      let+ name = Names.gen in
      Decode.ExpKName name
    in
    let stop =
      let+ export = export_gen in
      Decode.ExpKStop export
    in
    oneof [start; name; stop]

  let export_check exp exp' =
    exp.Ast.name = exp'.Ast.name && exp.edesc = exp'.edesc

  let check export export' =
    let open Lwt_result_syntax in
    match (export, export') with
    | Decode.ExpKStart, Decode.ExpKStart -> return true
    | ExpKName n, ExpKName n' -> Names.check n n'
    | ExpKStop exp, ExpKStop exp' -> return (export_check exp exp')
    | _, _ -> return false

  let tests =
    [tztest "Exports" `Quick (make_test Parser.Export.encoding gen check)]
end

module Size = struct
  open Utils

  let gen =
    let open QCheck2.Gen in
    let* size = small_nat in
    let+ start = small_nat in
    Decode.{size; start}

  let check s s' =
    let open Lwt_result_syntax in
    return (s.Decode.size = s'.Decode.size && s.start = s'.start)

  let tests = [tztest "Size" `Quick (make_test Parser.Size.encoding gen check)]
end

module Instr_block = struct
  open Utils

  let gen =
    let open QCheck2.Gen in
    let stop =
      let+ lbl = Ast_generators.block_label_gen in
      Decode.IKStop lbl
    in
    let next =
      let+ lbl = Ast_generators.block_label_gen in
      Decode.IKNext lbl
    in
    let block =
      let* ty = Ast_generators.block_type_gen in
      let+ pos = small_nat in
      Decode.IKBlock (ty, pos)
    in
    let loop =
      let* ty = Ast_generators.block_type_gen in
      let+ pos = small_nat in
      Decode.IKLoop (ty, pos)
    in
    let if1 =
      let* ty = Ast_generators.block_type_gen in
      let+ pos = small_nat in
      Decode.IKIf1 (ty, pos)
    in
    let if2 =
      let* ty = Ast_generators.block_type_gen in
      let* pos = small_nat in
      let+ lbl = Ast_generators.block_label_gen in
      Decode.IKIf2 (ty, pos, lbl)
    in
    oneof [stop; next; block; loop; if1; if2]

  let check ik ik' =
    let open Lwt_result_syntax in
    match (ik, ik') with
    | Decode.IKStop l, Decode.IKStop l' | IKNext l, IKNext l' -> return (l = l')
    | IKBlock (ty, pos), IKBlock (ty', pos')
    | IKLoop (ty, pos), IKLoop (ty', pos')
    | IKIf1 (ty, pos), IKIf1 (ty', pos') ->
        return (ty = ty' && pos = pos')
    | IKIf2 (ty, pos, l), IKIf2 (ty', pos', l') ->
        return (ty = ty' && pos = pos' && l = l')
    | _, _ -> return_false

  let tests =
    [
      tztest
        "Instr_block"
        `Quick
        (make_test Parser.Instr_block.encoding gen check);
    ]
end

module Block = struct
  open Utils

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.BlockStart in
    let parse =
      let+ instr_stack = LazyStack.gen Instr_block.gen in
      Decode.BlockParse instr_stack
    in
    let stop =
      let+ lbl = Ast_generators.block_label_gen in
      Decode.BlockStop lbl
    in
    oneof [start; parse; stop]

  let check bl bl' =
    let open Lwt_result_syntax in
    match (bl, bl') with
    | Decode.BlockStart, Decode.BlockStart -> return_true
    | BlockParse is, BlockParse is' -> LazyStack.check Instr_block.check is is'
    | BlockStop l, BlockStop l' -> return (l = l')
    | _, _ -> return_false

  let tests =
    [tztest "Block" `Quick (make_test Parser.Block.encoding gen check)]
end

module Code = struct
  open Utils

  let func_gen =
    let open QCheck2.Gen in
    let* ftype = Ast_generators.var_gen in
    let* locals = Vec.gen Ast_generators.value_type_gen in
    let+ body = Ast_generators.block_label_gen in
    Source.(Ast.{ftype; locals; body} @@ no_region)

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.CKStart in
    let locals_parse =
      let* left = small_nat in
      let* size = Size.gen in
      let* pos = small_nat in
      let* vec_kont = LazyVec.gen (pair int32 Ast_generators.value_type_gen) in
      let+ locals_size = int64 in
      Decode.CKLocalsParse {left; size; pos; vec_kont; locals_size}
    in
    let locals_accumulate =
      let* left = small_nat in
      let* size = Size.gen in
      let* pos = small_nat in
      let* type_vec = LazyVec.gen (pair int32 Ast_generators.value_type_gen) in
      let* curr_type = opt (pair int32 Ast_generators.value_type_gen) in
      let+ vec_kont = LazyVec.gen Ast_generators.value_type_gen in
      Decode.CKLocalsAccumulate {left; size; pos; type_vec; curr_type; vec_kont}
    in
    let body =
      let* left = small_nat in
      let* size = Size.gen in
      let* locals = Vec.gen Ast_generators.value_type_gen in
      let+ const_kont = Block.gen in
      Decode.CKBody {left; size; locals; const_kont}
    in
    let stop =
      let+ func = func_gen in
      Decode.CKStop func
    in
    oneof [start; locals_parse; locals_accumulate; body; stop]

  let check_func Ast.{ftype; locals; body}
      Ast.{ftype = ftype'; locals = locals'; body = body'} =
    let open Lwt_result_syntax in
    let eq_value_type t t' = return (t = t') in
    let+ eq_locals = Vec.check eq_value_type locals locals' in
    ftype = ftype' && body = body' && eq_locals

  let check code code' =
    let open Lwt_result_syntax in
    let eq_value_type t t' = return (t = t') in
    match (code, code') with
    | Decode.CKStart, Decode.CKStart -> return_true
    | ( Decode.CKLocalsParse {left; size; pos; vec_kont; locals_size},
        Decode.CKLocalsParse
          {
            left = left';
            size = size';
            pos = pos';
            vec_kont = vec_kont';
            locals_size = locals_size';
          } ) ->
        let+ eq_vec_kont = LazyVec.check eq_value_type vec_kont vec_kont' in
        eq_vec_kont && left = left' && size = size' && pos = pos'
        && locals_size = locals_size'
    | ( Decode.CKLocalsAccumulate
          {left; size; pos; type_vec; curr_type; vec_kont},
        Decode.CKLocalsAccumulate
          {
            left = left';
            size = size';
            pos = pos';
            type_vec = type_vec';
            curr_type = curr_type';
            vec_kont = vec_kont';
          } ) ->
        let* eq_type_vec = LazyVec.check eq_value_type type_vec type_vec' in
        let+ eq_vec_kont = LazyVec.check eq_value_type vec_kont vec_kont' in
        eq_type_vec && eq_vec_kont && left = left' && size = size' && pos = pos'
        && curr_type = curr_type'
    | ( Decode.CKBody {left; size; locals; const_kont},
        Decode.CKBody
          {
            left = left';
            size = size';
            locals = locals';
            const_kont = const_kont';
          } ) ->
        let* eq_locals = Vec.check eq_value_type locals locals' in
        let+ eq_const_kont = Block.check const_kont const_kont' in
        eq_locals && eq_const_kont && left = left' && size = size'
    | Decode.CKStop Source.{it = func; _}, Decode.CKStop Source.{it = func'; _}
      ->
        check_func func func'
    | _, _ -> return false

  let tests = [tztest "Code" `Quick (make_test Parser.Code.encoding gen check)]
end

module Elem = struct
  open Utils

  let elem_gen =
    let open QCheck2.Gen in
    let open Ast_generators in
    let* etype = ref_type_gen in
    let* emode = segment_mode_gen in
    let+ einit = Vec.gen const_gen in
    Ast.{etype; emode; einit}

  let gen =
    let open QCheck2.Gen in
    let open Ast_generators in
    let start = return Decode.EKStart in
    let mode =
      let* left = small_nat in
      let* index = int32 in
      let* index_kind = oneofl [Decode.Indexed; Decode.Const] in
      let* early_ref_type = opt ref_type_gen in
      let* offset_kont = small_nat in
      let+ offset_kont_code = Block.gen in
      Decode.EKMode
        {
          left;
          index = Source.(index @@ no_region);
          index_kind;
          early_ref_type;
          offset_kont = (offset_kont, offset_kont_code);
        }
    in
    let initindexed =
      let* mode = segment_mode_gen in
      let* ref_type = ref_type_gen in
      let+ einit_vec = LazyVec.gen const_gen in
      Decode.EKInitIndexed {mode; ref_type; einit_vec}
    in
    let initconst =
      let* mode = segment_mode_gen in
      let* ref_type = ref_type_gen in
      let* einit_vec = LazyVec.gen const_gen in
      let* pos = small_int in
      let+ block = Block.gen in
      Decode.EKInitConst {mode; ref_type; einit_vec; einit_kont = (pos, block)}
    in
    let stop =
      let+ elem = elem_gen in
      Decode.EKStop elem
    in
    oneof [start; mode; initindexed; initconst; stop]

  let elem_check Ast.{emode; einit; etype}
      Ast.{emode = emode'; einit = einit'; etype = etype'} =
    let open Lwt_result_syntax in
    let eq_const c c' = return (c = c') in
    let* eq_init = Vec.check eq_const einit einit' in
    return (emode = emode' && eq_init && etype = etype')

  let check ek ek' =
    let open Lwt_result_syntax in
    match (ek, ek') with
    | Decode.EKStart, Decode.EKStart -> return_true
    | ( EKMode
          {
            left;
            index;
            index_kind;
            early_ref_type;
            offset_kont = offset_kont_pos, offset_kont_code;
          },
        EKMode
          {
            left = left';
            index = index';
            index_kind = index_kind';
            early_ref_type = early_ref_type';
            offset_kont = offset_kont_pos', offset_kont_code';
          } ) ->
        let+ eq_code = Block.check offset_kont_code offset_kont_code' in
        left = left' && index = index' && index_kind = index_kind'
        && early_ref_type = early_ref_type'
        && offset_kont_pos = offset_kont_pos'
        && eq_code
    | ( EKInitIndexed {mode; ref_type; einit_vec},
        EKInitIndexed
          {mode = mode'; ref_type = ref_type'; einit_vec = einit_vec'} ) ->
        let eq_const c c' = return (c = c') in
        let+ eq_init = LazyVec.check eq_const einit_vec einit_vec' in
        mode = mode' && ref_type = ref_type' && eq_init
    | ( EKInitConst {mode; ref_type; einit_vec; einit_kont = pos, block},
        EKInitConst
          {
            mode = mode';
            ref_type = ref_type';
            einit_vec = einit_vec';
            einit_kont = pos', block';
          } ) ->
        let eq_const c c' = return (c = c') in
        let* eq_init = LazyVec.check eq_const einit_vec einit_vec' in
        let+ eq_block = Block.check block block' in
        mode = mode' && ref_type = ref_type' && pos = pos' && eq_init
        && eq_block
    | EKStop elem, EKStop elem' -> elem_check elem elem'
    | _, _ -> return_false

  let tests = [tztest "Elem" `Quick (make_test Parser.Elem.encoding gen check)]
end

module Data = struct
  open Utils

  let data_gen =
    let open QCheck2.Gen in
    let* dmode = Ast_generators.segment_mode_gen in
    let+ dinit = Ast_generators.data_label_gen in
    Ast.{dmode; dinit}

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.DKStart in
    let mode =
      let* left = small_nat in
      let* index = int32 in
      let* offset_kont = small_nat in
      let+ offset_kont_code = Block.gen in
      Decode.DKMode
        {
          left;
          index = Source.(index @@ no_region);
          offset_kont = (offset_kont, offset_kont_code);
        }
    in
    let init =
      let* dmode = Ast_generators.segment_mode_gen in
      let+ init_kont = Byte_vector.gen in
      Decode.DKInit {dmode; init_kont}
    in
    let stop =
      let+ data = data_gen in
      Decode.DKStop data
    in
    oneof [start; mode; init; stop]

  let data_check Ast.{dmode; dinit} Ast.{dmode = dmode'; dinit = dinit'} =
    let open Lwt_result_syntax in
    return (dmode = dmode' && dinit = dinit')

  let check dk dk' =
    let open Lwt_result_syntax in
    match (dk, dk') with
    | Decode.DKStart, Decode.DKStart -> return_true
    | ( DKMode {left; index; offset_kont = offset_kont_pos, offset_kont_code},
        DKMode
          {
            left = left';
            index = index';
            offset_kont = offset_kont_pos', offset_kont_code';
          } ) ->
        let+ eq_code = Block.check offset_kont_code offset_kont_code' in
        left = left' && index = index'
        && offset_kont_pos = offset_kont_pos'
        && eq_code
    | DKInit {dmode; init_kont}, DKInit {dmode = dmode'; init_kont = init_kont'}
      ->
        let+ eq_init = Byte_vector.check init_kont init_kont' in
        dmode = dmode' && eq_init
    | DKStop data, DKStop data' -> data_check data data'
    | _, _ -> return false

  let tests = [tztest "Data" `Quick (make_test Parser.Data.encoding gen check)]
end

module Field = struct
  open Utils

  let no_region gen = QCheck2.Gen.map (fun v -> Source.(v @@ no_region)) gen

  let type_field_gen = Vec.gen (no_region Func_type.func_type_gen)

  let import_field_gen = Vec.gen (no_region Imports.import_gen)

  let func_field_gen = Vec.gen Ast_generators.var_gen

  let table_field_gen =
    let open QCheck2.Gen in
    let table_gen =
      let+ ttype = Ast_generators.table_type_gen in
      Ast.{ttype}
    in
    Vec.gen (no_region table_gen)

  let memory_field_gen =
    let open QCheck2.Gen in
    let memory_gen =
      let+ mtype = Ast_generators.memory_type_gen in
      Ast.{mtype}
    in
    Vec.gen (no_region memory_gen)

  let global_field_gen =
    let open QCheck2.Gen in
    let global_gen =
      let* ginit = no_region Ast_generators.block_label_gen in
      let+ gtype = Ast_generators.global_type_gen in
      Ast.{gtype; ginit}
    in
    Vec.gen (no_region global_gen)

  let export_field_gen = Vec.gen (no_region Exports.export_gen)

  let start_field_gen = QCheck2.Gen.opt Ast_generators.start_gen

  let elem_field_gen = Vec.gen (no_region Elem.elem_gen)

  let data_count_field_gen = QCheck2.Gen.(opt int32)

  let code_field_gen = Vec.gen Code.func_gen

  let data_field_gen = Vec.gen (no_region Data.data_gen)

  let field_type_gen =
    let open QCheck2.Gen in
    let pack f = Parser.Field.FieldType f in
    oneofl
      [
        pack Decode.TypeField;
        pack ImportField;
        pack FuncField;
        pack TableField;
        pack MemoryField;
        pack GlobalField;
        pack ExportField;
        pack StartField;
        pack ElemField;
        pack DataCountField;
        pack CodeField;
        pack DataField;
      ]

  let typed_lazy_vec_gen =
    let open QCheck2.Gen in
    let pack f gen_vec =
      let+ vec = LazyVec.gen_with_vec gen_vec in
      Parser.Field.TypedLazyVec (f, vec)
    in
    oneof
      [
        pack Decode.TypeField type_field_gen;
        pack ImportField import_field_gen;
        pack FuncField func_field_gen;
        pack TableField table_field_gen;
        pack MemoryField memory_field_gen;
        pack GlobalField global_field_gen;
        pack ExportField export_field_gen;
        pack ElemField elem_field_gen;
        pack CodeField code_field_gen;
        pack DataField data_field_gen;
      ]

  let check_field_type :
      type a a' repr repr'.
      (a, repr) Decode.field_type -> (a', repr') Decode.field_type -> bool =
   fun ft ft' ->
    match (ft, ft') with
    | Decode.DataCountField, Decode.DataCountField -> true
    | StartField, StartField -> true
    | TypeField, TypeField -> true
    | ImportField, ImportField -> true
    | FuncField, FuncField -> true
    | TableField, TableField -> true
    | MemoryField, MemoryField -> true
    | GlobalField, GlobalField -> true
    | ExportField, ExportField -> true
    | ElemField, ElemField -> true
    | CodeField, CodeField -> true
    | DataField, DataField -> true
    | _, _ -> false

  let check_packed_field_type (Parser.Field.FieldType ft)
      (Parser.Field.FieldType ft') =
    Lwt.return_ok (check_field_type ft ft')

  let check_field_type_value :
      type a a' repr repr'.
      (a, repr) Decode.field_type ->
      (a', repr') Decode.field_type ->
      a ->
      a' ->
      (bool, _) result Lwt.t =
   fun ft ft' x y ->
    let open Lwt_result_syntax in
    match (ft, ft') with
    | Decode.DataCountField, Decode.DataCountField -> return (x = y)
    | StartField, StartField -> return (x = y)
    | TypeField, TypeField -> Func_type.func_type_check x.Source.it y.Source.it
    | ImportField, ImportField ->
        return (Imports.import_check x.Source.it y.Source.it)
    | FuncField, FuncField -> return (x = y)
    | TableField, TableField -> return (x = y)
    | MemoryField, MemoryField -> return (x = y)
    | GlobalField, GlobalField -> return (x = y)
    | ExportField, ExportField ->
        return (Exports.export_check x.Source.it y.Source.it)
    | ElemField, ElemField -> Elem.elem_check x.Source.it y.Source.it
    | CodeField, CodeField -> Code.check_func x.Source.it y.Source.it
    | DataField, DataField -> Data.data_check x.Source.it y.Source.it
    | _, _ -> return_false

  let building_state_gen =
    let open QCheck2.Gen in
    let* types = type_field_gen in
    let* imports = import_field_gen in
    let* vars = func_field_gen in
    let* tables = table_field_gen in
    let* memories = memory_field_gen in
    let* globals = global_field_gen in
    let* exports = export_field_gen in
    let* start = start_field_gen in
    let* elems = elem_field_gen in
    let* data_count = data_count_field_gen in
    let* code = code_field_gen in
    let+ datas = data_field_gen in
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
      }

  let building_state_check
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
        }
      Decode.
        {
          types = types';
          imports = imports';
          vars = vars';
          tables = tables';
          memories = memories';
          globals = globals';
          exports = exports';
          start = start';
          elems = elems';
          data_count = data_count';
          code = code';
          datas = datas';
        } =
    let open Lwt_result_syntax in
    let check_no_region check v v' = check v.Source.it v'.Source.it in
    let check_no_region_lwt check v v' =
      return (check v.Source.it v'.Source.it)
    in
    let eq v v' = return (v = v') in
    let* eq_types =
      Vec.check (check_no_region Func_type.func_type_check) types types'
    in
    let* eq_imports =
      Vec.check (check_no_region_lwt Imports.import_check) imports imports'
    in
    let* eq_vars = Vec.check (check_no_region eq) vars vars' in
    let* eq_tables = Vec.check (check_no_region eq) tables tables' in
    let* eq_memories = Vec.check (check_no_region eq) memories memories' in
    let* eq_globals = Vec.check (check_no_region eq) globals globals' in
    let* eq_exports =
      Vec.check (check_no_region_lwt Exports.export_check) exports exports'
    in
    let* eq_start = return (start = start') in
    let* eq_elems = Vec.check (check_no_region Elem.elem_check) elems elems' in
    let* eq_data_count = return (data_count = data_count') in
    let* eq_code = Vec.check (check_no_region Code.check_func) code code' in
    let+ eq_datas = Vec.check (check_no_region Data.data_check) datas datas' in
    eq_types && eq_imports && eq_vars && eq_tables && eq_memories && eq_globals
    && eq_exports && eq_start && eq_elems && eq_data_count && eq_code
    && eq_datas

  let tests =
    [
      tztest
        "Field"
        `Quick
        (make_test
           Parser.Field.building_state_encoding
           building_state_gen
           building_state_check);
      tztest
        "Field.Packed"
        `Quick
        (make_test
           Parser.Field.packed_field_type_encoding
           field_type_gen
           check_packed_field_type);
    ]
end

module Module = struct
  open Utils

  (* Different version from Ast_generators.allocations_gen: the vector
     is never created lazily. *)
  let allocations_gen =
    let open QCheck2.Gen in
    let* blocks = Vec.gen (Vec.gen Ast_generators.instr_gen) in
    let+ datas = Vec.gen Byte_vector.gen_chunked_byte_vector in
    Ast.{blocks; datas}

  let module_gen =
    let open QCheck2.Gen in
    let open Field in
    let* types = type_field_gen in
    let* globals = global_field_gen in
    let* tables = table_field_gen in
    let* memories = memory_field_gen in
    let* funcs = code_field_gen in
    let* start = start_field_gen in
    let* elems = elem_field_gen in
    let* datas = data_field_gen in
    let* imports = import_field_gen in
    let* exports = export_field_gen in
    let+ allocations = allocations_gen in
    Ast.
      {
        types;
        globals;
        tables;
        memories;
        funcs;
        start;
        elems;
        datas;
        imports;
        exports;
        allocations;
      }

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.MKStart in
    let skip_custom =
      let+ packed_ft_opt = opt Field.field_type_gen in
      match packed_ft_opt with
      | Some (Parser.Field.FieldType ft) -> Decode.MKSkipCustom (Some ft)
      | None -> MKSkipCustom None
    in
    let field_start =
      let+ (Parser.Field.FieldType ft) = Field.field_type_gen in
      Decode.MKFieldStart ft
    in
    let field =
      let* size = Size.gen in
      let+ (Parser.Field.TypedLazyVec (ft, vec)) = Field.typed_lazy_vec_gen in
      Decode.MKField (ft, size, vec)
    in
    let elaborate_func =
      let* func_types = Vec.gen Ast_generators.var_gen in
      let* func_bodies = Field.code_field_gen in
      let* func_kont = LazyVec.gen Code.func_gen in
      let* iterators =
        option (LazyVec.gen (LazyVec.gen Ast_generators.instr_gen))
      in
      let+ datas_in_func = bool in
      Decode.MKElaborateFunc
        (func_types, func_bodies, func_kont, iterators, datas_in_func)
    in
    let build =
      let* funcs = opt (Vec.gen Code.func_gen) in
      let+ datas_in_func = bool in
      Decode.MKBuild (funcs, datas_in_func)
    in
    let stop =
      let+ modl = module_gen in
      Decode.MKStop Source.(modl @@ no_region)
    in
    let types =
      let* func_type_kont = Func_type.gen in
      let* pos = small_nat in
      let* size = Size.gen in
      let+ vec_kont = LazyVec.gen_with_vec Field.type_field_gen in
      Decode.MKTypes (func_type_kont, pos, size, vec_kont)
    in
    let imports =
      let* import_kont = Imports.gen in
      let* pos = small_nat in
      let* size = Size.gen in
      let+ vec_kont = LazyVec.gen_with_vec Field.import_field_gen in
      Decode.MKImport (import_kont, pos, size, vec_kont)
    in
    let exports =
      let* export_kont = Exports.gen in
      let* pos = small_nat in
      let* size = Size.gen in
      let+ vec_kont = LazyVec.gen_with_vec Field.export_field_gen in
      Decode.MKExport (export_kont, pos, size, vec_kont)
    in
    let global =
      let* global_type = Ast_generators.global_type_gen in
      let* pos = small_nat in
      let* block_kont = Block.gen in
      let* size = Size.gen in
      let+ vec_kont = LazyVec.gen_with_vec Field.global_field_gen in
      Decode.MKGlobal (global_type, pos, block_kont, size, vec_kont)
    in
    let elem =
      let* elem_kont = Elem.gen in
      let* pos = small_nat in
      let* size = Size.gen in
      let+ vec_kont = LazyVec.gen_with_vec Field.elem_field_gen in
      Decode.MKElem (elem_kont, pos, size, vec_kont)
    in
    let data =
      let* data_kont = Data.gen in
      let* pos = small_nat in
      let* size = Size.gen in
      let+ vec_kont = LazyVec.gen_with_vec Field.data_field_gen in
      Decode.MKData (data_kont, pos, size, vec_kont)
    in
    let code =
      let* code_kont = Code.gen in
      let* pos = small_nat in
      let* size = Size.gen in
      let+ vec_kont = LazyVec.gen_with_vec Field.code_field_gen in
      Decode.MKCode (code_kont, pos, size, vec_kont)
    in
    oneof
      [
        start;
        skip_custom;
        field_start;
        field;
        elaborate_func;
        build;
        stop;
        types;
        imports;
        exports;
        global;
        elem;
        data;
        code;
      ]

  let check_allocations allocations allocations' =
    let open Lwt_result_syntax in
    let eq_instr i i' = return (i = i') in
    let* eq_blocks =
      Vec.check
        (Vec.check eq_instr)
        allocations.Ast.blocks
        allocations'.Ast.blocks
    in
    let+ eq_datas =
      Vec.check Byte_vector.check_vector allocations.datas allocations'.datas
    in
    eq_blocks && eq_datas

  let check_no_region check v v' = check v.Source.it v'.Source.it

  let check_no_region_lwt check v v' =
    Lwt.return_ok (check v.Source.it v'.Source.it)

  let check_module
      Source.
        {
          it =
            Ast.
              {
                types;
                globals;
                tables;
                memories;
                funcs;
                start;
                elems;
                datas;
                imports;
                exports;
                allocations;
              };
          _;
        }
      Source.
        {
          it =
            Ast.
              {
                types = types';
                globals = globals';
                tables = tables';
                memories = memories';
                funcs = funcs';
                start = start';
                elems = elems';
                datas = datas';
                imports = imports';
                exports = exports';
                allocations = allocations';
              };
          _;
        } =
    let open Lwt_result_syntax in
    let eq v v' = return (v = v') in
    let* eq_types =
      Vec.check (check_no_region Func_type.func_type_check) types types'
    in
    let* eq_globals = Vec.check (check_no_region eq) globals globals' in
    let* eq_tables = Vec.check (check_no_region eq) tables tables' in
    let* eq_memories = Vec.check (check_no_region eq) memories memories' in
    let* eq_funcs = Vec.check (check_no_region Code.check_func) funcs funcs' in
    let* eq_start = return (start = start') in
    let* eq_elems = Vec.check (check_no_region Elem.elem_check) elems elems' in
    let* eq_datas = Vec.check (check_no_region Data.data_check) datas datas' in
    let* eq_imports =
      Vec.check (check_no_region_lwt Imports.import_check) imports imports'
    in
    let* eq_exports =
      Vec.check (check_no_region_lwt Exports.export_check) exports exports'
    in
    let+ eq_allocations = check_allocations allocations allocations' in
    eq_types && eq_globals && eq_funcs && eq_tables && eq_memories && eq_start
    && eq_elems && eq_datas && eq_imports && eq_exports && eq_allocations

  let check_option f x y =
    match (x, y) with
    | Some x, Some y -> f x y
    | None, None -> Lwt_result.return true
    | _, _ -> Lwt_result.return false

  let eq_instr i i' = Lwt_result.return (i = i')

  let check mk mk' =
    let open Lwt_result_syntax in
    match (mk, mk') with
    | Decode.MKStart, Decode.MKStart -> return_true
    | MKSkipCustom None, MKSkipCustom None -> return_true
    | MKSkipCustom (Some ft), MKSkipCustom (Some ft') ->
        return @@ Field.check_field_type ft ft'
    | MKFieldStart ft, MKFieldStart ft' ->
        return @@ Field.check_field_type ft ft'
    | MKField (ft, size, kont), MKField (ft', size', kont') ->
        let* eq_kont =
          LazyVec.check_possibly_different
            (Field.check_field_type_value ft ft')
            kont
            kont'
        in
        let+ eq_size = Size.check size size' in
        eq_kont && eq_size && Field.check_field_type ft ft'
    | ( MKElaborateFunc (fts, fbs, kont, iterators, datas),
        MKElaborateFunc (fts', fbs', kont', iterators', datas') ) ->
        let eq_vars v v' = return (v = v') in
        let* eq_fts = Vec.check eq_vars fts fts' in
        let* eq_fbs = Vec.check (check_no_region Code.check_func) fbs fbs' in
        let* eq_its =
          check_option
            (LazyVec.check (LazyVec.check eq_instr))
            iterators
            iterators'
        in
        let+ eq_kont =
          LazyVec.check (check_no_region Code.check_func) kont kont'
        in
        eq_fts && eq_fbs && eq_kont && eq_its && datas = datas'
    | MKBuild (Some funcs, datas), MKBuild (Some funcs', datas') ->
        let+ eq_funcs =
          Vec.check (check_no_region Code.check_func) funcs funcs'
        in
        eq_funcs && datas = datas'
    | MKBuild (None, datas), MKBuild (None, datas') -> return (datas = datas')
    | MKStop m, MKStop m' -> check_module m m'
    | ( MKTypes (func_type_kont, pos, size, vec_kont),
        MKTypes (func_type_kont', pos', size', vec_kont') ) ->
        let* eq_func_type_kont =
          Func_type.check func_type_kont func_type_kont'
        in
        let* eq_size = Size.check size size' in
        let+ eq_vec_kont =
          LazyVec.check
            (check_no_region Func_type.func_type_check)
            vec_kont
            vec_kont'
        in
        eq_func_type_kont && eq_size && eq_vec_kont && pos = pos'
    | ( MKImport (import_kont, pos, size, vec_kont),
        MKImport (import_kont', pos', size', vec_kont') ) ->
        let* eq_import_kont = Imports.check import_kont import_kont' in
        let* eq_size = Size.check size size' in
        let+ eq_vec_kont =
          LazyVec.check
            (check_no_region_lwt Imports.import_check)
            vec_kont
            vec_kont'
        in
        eq_import_kont && eq_size && eq_vec_kont && pos = pos'
    | ( MKExport (export_kont, pos, size, vec_kont),
        MKExport (export_kont', pos', size', vec_kont') ) ->
        let* eq_export_kont = Exports.check export_kont export_kont' in
        let* eq_size = Size.check size size' in
        let+ eq_vec_kont =
          LazyVec.check
            (check_no_region_lwt Exports.export_check)
            vec_kont
            vec_kont'
        in
        eq_export_kont && eq_size && eq_vec_kont && pos = pos'
    | ( MKGlobal (global_type, pos, block_kont, size, vec_kont),
        MKGlobal (global_type', pos', block_kont', size', vec_kont') ) ->
        let* eq_block_kont = Block.check block_kont block_kont' in
        let* eq_size = Size.check size size' in
        let+ eq_vec_kont =
          LazyVec.check (fun g g' -> return (g = g')) vec_kont vec_kont'
        in
        eq_block_kont && eq_size && eq_vec_kont && pos = pos'
        && global_type = global_type'
    | ( MKElem (elem_kont, pos, size, vec_kont),
        MKElem (elem_kont', pos', size', vec_kont') ) ->
        let* eq_elem_kont = Elem.check elem_kont elem_kont' in
        let* eq_size = Size.check size size' in
        let+ eq_vec_kont =
          LazyVec.check (check_no_region Elem.elem_check) vec_kont vec_kont'
        in
        eq_elem_kont && eq_size && eq_vec_kont && pos = pos'
    | ( MKData (data_kont, pos, size, vec_kont),
        MKData (data_kont', pos', size', vec_kont') ) ->
        let* eq_data_kont = Data.check data_kont data_kont' in
        let* eq_size = Size.check size size' in
        let+ eq_vec_kont =
          LazyVec.check (fun d d' -> return (d = d')) vec_kont vec_kont'
        in
        eq_data_kont && eq_size && eq_vec_kont && pos = pos'
    | ( MKCode (code_kont, pos, size, vec_kont),
        MKCode (code_kont', pos', size', vec_kont') ) ->
        let* eq_code_kont = Code.check code_kont code_kont' in
        let* eq_size = Size.check size size' in
        let+ eq_vec_kont =
          LazyVec.check (check_no_region Code.check_func) vec_kont vec_kont'
        in
        eq_code_kont && eq_size && eq_vec_kont && pos = pos'
    | _, _ -> return_false

  let tests =
    [tztest "Module" `Quick (make_test Parser.Module.encoding gen check)]
end

let tests =
  Byte_vector.tests @ Vec.tests @ LazyVec.tests @ Names.tests @ Func_type.tests
  @ Imports.tests @ LazyStack.tests @ Exports.tests @ Instr_block.tests
  @ Block.tests @ Size.tests @ Code.tests @ Elem.tests @ Data.tests
  @ Field.tests @ Module.tests
