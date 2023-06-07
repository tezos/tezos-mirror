(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Tezos_webassembly_interpreter

exception Uninitialized_current_module

val var_list_encoding : Ast.var list Tezos_tree_encoding.t

val instruction_encoding : Ast.instr Tezos_tree_encoding.t

val func'_encoding : Ast.func' Tezos_tree_encoding.t

val func_encoding : Ast.func Tezos_tree_encoding.t

val module_key_encoding : Instance.module_key Tezos_tree_encoding.t

val function_encoding : Instance.func_inst Tezos_tree_encoding.t

val value_ref_encoding : Values.ref_ Tezos_tree_encoding.t

val value_encoding : Values.value Tezos_tree_encoding.t

val values_encoding : Values.value Instance.Vector.t Tezos_tree_encoding.t

val memory_encoding : Partial_memory.memory Tezos_tree_encoding.t

val table_encoding : Partial_table.table Tezos_tree_encoding.t

val global_encoding : Global.global Tezos_tree_encoding.t

val export_instance_encoding : Instance.export_inst Tezos_tree_encoding.t

val memory_instance_encoding :
  Partial_memory.memory Instance.Vector.t Tezos_tree_encoding.t

val table_vector_encoding :
  Partial_table.table Instance.Vector.t Tezos_tree_encoding.t

val global_vector_encoding :
  Global.global Instance.Vector.t Tezos_tree_encoding.t

val data_label_ref_encoding : Ast.data_label ref Tezos_tree_encoding.t

val function_vector_encoding :
  Instance.func_inst Instance.Vector.t Tezos_tree_encoding.t

val func_type_encoding : Types.func_type Tezos_tree_encoding.t

val function_type_vector_encoding :
  Types.func_type Instance.Vector.t Tezos_tree_encoding.t

val value_ref_vector_encoding :
  Values.ref_ Instance.Vector.t Tezos_tree_encoding.t

val extern_encoding : Instance.extern Tezos_tree_encoding.t

val extern_map_encoding :
  Instance.extern Instance.NameMap.t Tezos_tree_encoding.t

val value_ref_vector_vector_encoding :
  Values.ref_ Instance.Vector.t ref Instance.Vector.t Tezos_tree_encoding.t

val block_table_encoding : Ast.block_table Tezos_tree_encoding.t

val datas_table_encoding : Ast.datas_table Tezos_tree_encoding.t

val allocations_encoding : Ast.allocations Tezos_tree_encoding.t

val module_instance_encoding : Instance.module_inst Tezos_tree_encoding.t

val module_instances_encoding : Instance.module_reg Tezos_tree_encoding.t

val input_buffer_encoding : Input_buffer.t Tezos_tree_encoding.t

val output_buffer_encoding : Output_buffer.t Tezos_tree_encoding.t

val admin_instr_encoding : Eval.admin_instr Tezos_tree_encoding.t

val frame_encoding : Eval.frame Tezos_tree_encoding.t

val config_encoding : Eval.config Tezos_tree_encoding.t

val buffers_encoding : Eval.buffers Tezos_tree_encoding.t
