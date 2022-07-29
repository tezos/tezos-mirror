(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

module Source : sig
  open Tezos_webassembly_interpreter.Source

  val phrase_encoding : 'a Data_encoding.t -> 'a phrase Data_encoding.t
end

module Types : sig
  open Tezos_webassembly_interpreter.Types

  val num_type_encoding : num_type Data_encoding.t

  val vec_type_encoding : vec_type Data_encoding.t

  val ref_type_encoding : ref_type Data_encoding.t

  val value_type_encoding : value_type Data_encoding.t

  val result_type_encoding : value_type list Data_encoding.t

  val func_type_encoding :
    params_encoding:result_type Data_encoding.t ->
    result_encoding:result_type Data_encoding.t ->
    func_type Data_encoding.t

  val mutability_encoding : mutability Data_encoding.t

  val pack_size_encoding : pack_size Data_encoding.t

  val pack_shape_encoding : pack_shape Data_encoding.t

  val extension_encoding : extension Data_encoding.t

  val vec_extension_encoding : vec_extension Data_encoding.t

  val global_type_encoding : global_type Data_encoding.t
end

module Values : sig
  open Tezos_webassembly_interpreter
  open Values

  val op_encoding :
    'a Data_encoding.t ->
    'b Data_encoding.t ->
    ('a, 'b, 'c, 'd) op Data_encoding.t

  val vecop_encoding : 'a Data_encoding.t -> 'a vecop Data_encoding.t

  val num_encoding : num Data_encoding.t

  val vec_encoding : V128.t vecop Data_encoding.t
end

module V128 : sig
  open Tezos_webassembly_interpreter

  val laneop_encoding :
    'a Data_encoding.t ->
    'b Data_encoding.t ->
    'c Data_encoding.t ->
    'd Data_encoding.t ->
    ('a, 'b, 'c, 'd, 'e, 'f) V128.laneop Data_encoding.t
end

module Ast : sig
  open Tezos_webassembly_interpreter

  module IntOp : sig
    val unop_encoding : Ast.IntOp.unop Data_encoding.t

    val binop_encoding : Ast.IntOp.binop Data_encoding.t

    val testop_encoding : Ast.IntOp.testop Data_encoding.t

    val relop_encoding : Ast.IntOp.relop Data_encoding.t

    val cvtop_encoding : Ast.IntOp.cvtop Data_encoding.t
  end

  module V128Op : sig
    val itestop_encoding : Ast.V128Op.itestop Data_encoding.t

    val iunop_encoding : Ast.V128Op.iunop Data_encoding.t

    val ibinop_encoding : Ast.V128Op.ibinop Data_encoding.t

    val irelop_encoding : Ast.V128Op.irelop Data_encoding.t

    val icvtop_encoding : Ast.V128Op.icvtop Data_encoding.t

    val ishiftop_encoding : Ast.V128Op.ishiftop Data_encoding.t

    val ibitmaskop_encoding : Ast.V128Op.ibitmaskop Data_encoding.t

    val vtestop_encoding : Ast.V128Op.vtestop Data_encoding.t

    val vunop_encoding : Ast.V128Op.vunop Data_encoding.t

    val vbinop_encoding : Ast.V128Op.vbinop Data_encoding.t

    val vternop_encoding : Ast.V128Op.vternop Data_encoding.t

    val testop_encoding : Ast.V128Op.testop Data_encoding.t

    val unop_encoding : Ast.V128Op.unop Data_encoding.t

    val binop_encoding : Ast.V128Op.binop Data_encoding.t

    val relop_encoding : Ast.V128Op.relop Data_encoding.t

    val cvtop_encoding : Ast.V128Op.cvtop Data_encoding.t

    val shiftop_encoding : Ast.V128Op.shiftop Data_encoding.t

    val bitmaskop_encoding : Ast.V128Op.bitmaskop Data_encoding.t

    val nsplatop_encoding : Ast.V128Op.nsplatop Data_encoding.t

    val nextractop_encoding :
      'a Data_encoding.t -> 'a Ast.V128Op.nextractop Data_encoding.t

    val nreplaceop_encoding : Ast.V128Op.nreplaceop Data_encoding.t

    val splatop_encoding : Ast.V128Op.splatop Data_encoding.t

    val extractop_encoding : Ast.V128Op.extractop Data_encoding.t

    val replaceop_encoding : Ast.V128Op.replaceop Data_encoding.t
  end

  val testop_encoding : Ast.testop Data_encoding.t

  val unop_encoding : Ast.unop Data_encoding.t

  val binop_encoding : Ast.binop Data_encoding.t

  val relop_encoding : Ast.relop Data_encoding.t

  val cvtop_encoding : Ast.cvtop Data_encoding.t

  val vec_testop_encoding : Ast.V128Op.testop Values.vecop Data_encoding.t

  val vec_relop_encoding : Ast.V128Op.relop Values.vecop Data_encoding.t

  val vec_unop_encoding : Ast.V128Op.unop Values.vecop Data_encoding.t

  val vec_binop_encoding : Ast.V128Op.binop Values.vecop Data_encoding.t

  val vec_cvtop_encoding : Ast.V128Op.cvtop Values.vecop Data_encoding.t

  val vec_shiftop_encoding : Ast.V128Op.shiftop Values.vecop Data_encoding.t

  val vec_bitmaskop_encoding : Ast.V128Op.bitmaskop Values.vecop Data_encoding.t

  val vec_vtestop_encoding : Ast.V128Op.vtestop Values.vecop Data_encoding.t

  val vec_vunop_encoding : Ast.V128Op.vunop Values.vecop Data_encoding.t

  val vec_vbinop_encoding : Ast.V128Op.vbinop Values.vecop Data_encoding.t

  val vec_vternop_encoding : Ast.V128Op.vternop Values.vecop Data_encoding.t

  val vec_splatop_encoding : Ast.V128Op.splatop Values.vecop Data_encoding.t

  val vec_extractop_encoding : Ast.V128Op.extractop Values.vecop Data_encoding.t

  val vec_replaceop_encoding : Ast.V128Op.replaceop Values.vecop Data_encoding.t

  val memop_encoding :
    'a Data_encoding.t ->
    'b Data_encoding.t ->
    ('a, 'b) Ast.memop Data_encoding.t

  val loadop_encoding :
    (Types.num_type, (Types.pack_size * Types.extension) option) Ast.memop
    Data_encoding.t

  val storeop_encoding :
    (Types.num_type, Types.pack_size option) Ast.memop Data_encoding.t

  val vec_loadop_encoding :
    (Types.vec_type, (Types.pack_size * Types.vec_extension) option) Ast.memop
    Data_encoding.t

  val vec_storeop_encoding : (Types.vec_type, unit) Ast.memop Data_encoding.t

  val vec_laneop_encoding :
    ((Types.vec_type, Types.pack_size) Ast.memop * int) Data_encoding.t

  val var_encoding : int32 Source.phrase Data_encoding.t

  val num_encoding : Values.num Source.phrase Data_encoding.t

  val vec_encoding : V128.t Values.vecop Source.phrase Data_encoding.t

  val block_type_encoding : Ast.block_type Data_encoding.t

  val block_label_encoding : Ast.block_label Data_encoding.t

  val data_label_encoding : Ast.data_label Data_encoding.t

  val import_desc_encoding : Ast.import_desc Data_encoding.t

  val export_desc_encoding : Ast.export_desc Data_encoding.t

  val const_encoding : Ast.const Data_encoding.t

  val segment_mode_encoding : Ast.segment_mode Data_encoding.t

  val table_encoding : Ast.table Data_encoding.t

  val memory_encoding : Ast.memory Data_encoding.t

  val global_encoding : Ast.global Data_encoding.t

  val start_encoding : Ast.start Data_encoding.t
end
