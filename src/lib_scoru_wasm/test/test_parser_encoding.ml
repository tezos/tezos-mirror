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

(** Testing
    -------
    Component:    Tree_encoding_decoding
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Parser encodings$"
    Subject:      Parser encoding tests for the tezos-scoru-wasm library
*)

open Tztest
open Lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm

(* Use context-binary for testing. *)
module Context = Tezos_context_memory.Context_binary

module Tree :
  Tezos_context_sigs.Context.TREE
    with type t = Context.t
     and type tree = Context.tree
     and type key = string list
     and type value = bytes = struct
  type t = Context.t

  type tree = Context.tree

  type key = Context.key

  type value = Context.value

  include Context.Tree
end

type Lazy_containers.Lazy_map.tree += Tree of Tree.tree

module Tree_encoding = struct
  include Tree_encoding.Make (struct
    include Tree

    let select = function
      | Tree t -> t
      | _ -> raise Tree_encoding.Incorrect_tree_type

    let wrap t = Tree t
  end)

  include Lazy_map_encoding.Make (Instance.NameMap)
end

module Parser = Binary_parser_encodings.Make (Tree_encoding)

module Utils = struct
  include Tree_encoding
  module V = Lazy_vector.LwtInt32Vector
  module C = Chunked_byte_vector.Lwt

  let empty_tree () =
    let open Lwt_syntax in
    let* index = Context.init "/tmp" in
    let empty_store = Context.empty index in
    return @@ Context.Tree.empty empty_store

  let test_encode_decode enc value f =
    let open Lwt_result_syntax in
    let*! empty_tree = empty_tree () in
    let*! tree = Tree_encoding.encode enc value empty_tree in
    let*! value' = Tree_encoding.decode enc tree in
    f value'

  let encode_decode enc value = test_encode_decode enc value Lwt.return

  let make_test encoding gen check () =
    Test_wasm_encoding.qcheck gen (fun value ->
        let open Lwt_result_syntax in
        let*! value' = encode_decode encoding value in
        let* res = check value value' in
        (* TODO: a better error reporting could be useful. *)
        if res then return_unit else fail ())
end

module Byte_vector = struct
  open Utils

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
    let* offset =
      Lib_test.Qcheck2_helpers.int32_range_gen 0l (V.num_elements vector)
    in
    return (Decode.LazyVec {vector; offset})

  let gen gen_values = gen_with_vec (Vec.gen gen_values)

  let check eq_value (Decode.LazyVec {vector; offset})
      (Decode.LazyVec {vector = vector'; offset = offset'}) =
    let open Lwt_result_syntax in
    let* eq_lzvecs = Vec.check eq_value vector vector' in
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

  let gen_utf8 = QCheck2.Gen.small_nat

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.NKStart in
    let parse =
      let* (Decode.LazyVec {vector; _} as buffer) = LazyVec.gen gen_utf8 in
      let vector_length = Int32.to_int (V.num_elements vector) in
      let* offset = int_range 0 vector_length in
      let+ length = int_range vector_length (vector_length * 2) in
      Decode.NKParse (offset, buffer, length)
    in
    let stop =
      let+ buffer = Vec.gen small_nat in
      Decode.NKStop buffer
    in
    oneof [start; parse; stop]

  let check ns ns' =
    let open Lwt_result_syntax in
    let eq_value x y = return (x = y) in
    match (ns, ns') with
    | Decode.NKStart, Decode.NKStart -> return true
    | NKParse (offset, buffer, length), NKParse (offset', buffer', length') ->
        let+ eq_bs = LazyVec.check eq_value buffer buffer' in
        eq_bs && offset = offset' && length = length'
    | NKStop vec, NKStop vec' -> Vec.check eq_value vec vec'
    | _, _ -> return false

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
    let* modl = Vec.gen Names.gen_utf8 in
    let* item = Vec.gen Names.gen_utf8 in
    let+ idesc = Ast_generators.import_desc_gen in
    Ast.{module_name = modl; item_name = item; idesc}

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.ImpKStart in
    let module_name =
      let+ modl = Names.gen in
      Decode.ImpKModuleName modl
    in
    let item_name =
      let* modl = Vec.gen Names.gen_utf8 in
      let+ item = Names.gen in
      Decode.ImpKItemName (modl, item)
    in
    let stop =
      let+ import = import_gen in
      Decode.ImpKStop import
    in
    oneof [start; module_name; item_name; stop]

  let import_check import import' =
    let open Lwt_result_syntax in
    let eq_value x y = return (x = y) in
    let* eq_m =
      Vec.check eq_value import.Ast.module_name import'.Ast.module_name
    in
    let+ eq_i = Vec.check eq_value import.item_name import'.item_name in
    eq_m && eq_i && import.idesc = import'.idesc

  let check import import' =
    let open Lwt_result_syntax in
    match (import, import') with
    | Decode.ImpKStart, Decode.ImpKStart -> return true
    | ImpKModuleName m, ImpKModuleName m' -> Names.check m m'
    | ImpKItemName (m, i), ImpKItemName (m', i') ->
        let eq_value x y = return (x = y) in
        let* eq_m = Vec.check eq_value m m' in
        let+ eq_i = Names.check i i' in
        eq_m && eq_i
    | ImpKStop imp, ImpKStop imp' -> import_check imp imp'
    | _, _ -> return false

  let tests =
    [tztest "Imports" `Quick (make_test Parser.Import.encoding gen check)]
end

module LazyStack = struct
  open Utils

  let gen gen_values =
    let open QCheck2.Gen in
    let* vector = Vec.gen gen_values in
    let* length =
      Lib_test.Qcheck2_helpers.int32_range_gen 0l (V.num_elements vector)
    in
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
    let* name = Vec.gen Names.gen_utf8 in
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
    let open Lwt_result_syntax in
    let eq_value x y = return (x = y) in
    let+ eq_n = Vec.check eq_value exp.Ast.name exp'.Ast.name in
    eq_n && exp.edesc = exp'.edesc

  let check export export' =
    let open Lwt_result_syntax in
    match (export, export') with
    | Decode.ExpKStart, Decode.ExpKStart -> return true
    | ExpKName n, ExpKName n' -> Names.check n n'
    | ExpKStop exp, ExpKStop exp' -> export_check exp exp'
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

let tests =
  Byte_vector.tests @ Vec.tests @ LazyVec.tests @ Names.tests @ Func_type.tests
  @ Imports.tests @ LazyStack.tests @ Exports.tests @ Instr_block.tests
  @ Block.tests @ Size.tests @ Code.tests
