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
open Tezos_lazy_containers
open Kont_encodings

exception Uninitialized_current_module

module V = Instance.Vector
module M = Instance.NameMap
module C = Chunked_byte_vector
open Tezos_tree_encoding
module NameMap = Lazy_map_encoding.Make (Instance.NameMap)
module ModuleMap = Lazy_map_encoding.Make (Instance.ModuleMap.Map)
module Outboxes = Lazy_map_encoding.Make (Output_buffer.Outboxes.Map)

(** Utility function*)
let string_tag = value [] Data_encoding.string

let lazy_vector_encoding field_name tree_encoding =
  scope
    [field_name]
    (Lazy_vector.Int32Vector.encoding
       (value [] Data_encoding.int32)
       tree_encoding)

let lazy_vector_encoding' tree_encoding =
  Lazy_vector.Int32Vector.encoding (value [] Data_encoding.int32) tree_encoding

let func'_encoding =
  let ftype = value ["ftype"] Interpreter_encodings.Ast.var_encoding in
  let locals =
    lazy_vector_encoding
      "locals"
      (value [] Interpreter_encodings.Types.value_type_encoding)
  in
  let body = value ["body"] Interpreter_encodings.Ast.block_label_encoding in
  conv
    (fun (ftype, locals, body) -> Ast.{ftype; locals; body})
    (fun {ftype; locals; body} -> (ftype, locals, body))
    (tup3 ~flatten:true ftype locals body)

let func_encoding =
  conv
    (fun func -> Source.(func @@ no_region))
    (fun {it = func; _} -> func)
    func'_encoding

let function_type_encoding =
  conv
    (fun (params, result) -> Types.FuncType (params, result))
    (function Types.FuncType (params, result) -> (params, result))
    (tup2
       ~flatten:false
       (lazy_vector_encoding
          "type-params"
          (value [] Interpreter_encodings.Types.value_type_encoding))
       (lazy_vector_encoding
          "type-result"
          (value [] Interpreter_encodings.Types.value_type_encoding)))

let var_list_encoding =
  value [] (Data_encoding.list Interpreter_encodings.Ast.var_encoding)

let block_label_encoding =
  value [] Interpreter_encodings.Ast.block_label_encoding

let data_label_encoding = value [] Interpreter_encodings.Ast.data_label_encoding

let instruction_encoding =
  let unit_encoding = value [] Data_encoding.unit in
  let open Ast in
  conv
    (fun instr -> Source.{at = no_region; it = instr})
    (fun Source.{at = _; it} -> it)
    (tagged_union
       string_tag
       [
         case
           "Unreachable"
           unit_encoding
           (function Unreachable -> Some () | _ -> None)
           (fun () -> Unreachable);
         case
           "Nop"
           unit_encoding
           (function Nop -> Some () | _ -> None)
           (fun () -> Nop);
         case
           "Drop"
           unit_encoding
           (function Drop -> Some () | _ -> None)
           (fun () -> Drop);
         case
           "Select"
           (value
              ["$1"]
              (* `Select` actually accepts only one value, but is a list for some
                 reason. See [Valid.check_instr] for reference or the reference
                 documentation. *)
              Data_encoding.(
                option (list Interpreter_encodings.Types.value_type_encoding)))
           (function Select p -> Some p | _ -> None)
           (fun p -> Select p);
         case
           "Block"
           (tup2
              ~flatten:false
              (value ["$1"] Interpreter_encodings.Ast.block_type_encoding)
              (scope ["$2"] block_label_encoding))
           (function Block (type_, instr) -> Some (type_, instr) | _ -> None)
           (fun (type_, instr) -> Block (type_, instr));
         case
           "Loop"
           (tup2
              ~flatten:false
              (value ["$1"] Interpreter_encodings.Ast.block_type_encoding)
              (scope ["$2"] block_label_encoding))
           (function Loop (type_, instr) -> Some (type_, instr) | _ -> None)
           (fun (type_, instr) -> Loop (type_, instr));
         case
           "If"
           (tup3
              ~flatten:false
              (value ["$1"] Interpreter_encodings.Ast.block_type_encoding)
              (scope ["$2"] block_label_encoding)
              (scope ["$3"] block_label_encoding))
           (function
             | If (type_, instr_if, instrs_else) ->
                 Some (type_, instr_if, instrs_else)
             | _ -> None)
           (fun (type_, instrs_if, instrs_else) ->
             If (type_, instrs_if, instrs_else));
         case
           "Br"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function Br var -> Some var | _ -> None)
           (fun var -> Br var);
         case
           "BrIf"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function BrIf var -> Some var | _ -> None)
           (fun var -> BrIf var);
         case
           "BrTable"
           (tup2
              ~flatten:false
              (scope ["$1"] var_list_encoding)
              (value ["$2"] Interpreter_encodings.Ast.var_encoding))
           (function
             | BrTable (table, target) -> Some (table, target) | _ -> None)
           (fun (table, target) -> BrTable (table, target));
         case
           "Return"
           unit_encoding
           (function Return -> Some () | _ -> None)
           (fun () -> Return);
         case
           "Call"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function Call var -> Some var | _ -> None)
           (fun var -> Call var);
         case
           "CallIndirect"
           (tup2
              ~flatten:false
              (value ["$1"] Interpreter_encodings.Ast.var_encoding)
              (value ["$2"] Interpreter_encodings.Ast.var_encoding))
           (function
             | CallIndirect (var1, var2) -> Some (var1, var2) | _ -> None)
           (fun (var1, var2) -> CallIndirect (var1, var2));
         case
           "LocalGet"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function LocalGet var -> Some var | _ -> None)
           (fun var -> LocalGet var);
         case
           "LocalSet"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function LocalSet var -> Some var | _ -> None)
           (fun var -> LocalSet var);
         case
           "LocalTee"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function LocalTee var -> Some var | _ -> None)
           (fun var -> LocalTee var);
         case
           "GlobalGet"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function GlobalGet var -> Some var | _ -> None)
           (fun var -> GlobalGet var);
         case
           "GlobalSet"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function GlobalSet var -> Some var | _ -> None)
           (fun var -> GlobalSet var);
         case
           "TableGet"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function TableGet var -> Some var | _ -> None)
           (fun var -> TableGet var);
         case
           "TableSet"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function TableSet var -> Some var | _ -> None)
           (fun var -> TableSet var);
         case
           "TableSize"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function TableSize var -> Some var | _ -> None)
           (fun var -> TableSize var);
         case
           "TableGrow"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function TableGrow var -> Some var | _ -> None)
           (fun var -> TableGrow var);
         case
           "TableFill"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function TableFill var -> Some var | _ -> None)
           (fun var -> TableFill var);
         case
           "TableCopy"
           (tup2
              ~flatten:false
              (value ["$1"] Interpreter_encodings.Ast.var_encoding)
              (value ["$2"] Interpreter_encodings.Ast.var_encoding))
           (function TableCopy (var1, var2) -> Some (var1, var2) | _ -> None)
           (fun (var1, var2) -> TableCopy (var1, var2));
         case
           "TableInit"
           (tup2
              ~flatten:false
              (value ["$1"] Interpreter_encodings.Ast.var_encoding)
              (value ["$2"] Interpreter_encodings.Ast.var_encoding))
           (function TableInit (var1, var2) -> Some (var1, var2) | _ -> None)
           (fun (var1, var2) -> TableInit (var1, var2));
         case
           "ElemDrop"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function ElemDrop var -> Some var | _ -> None)
           (fun var -> ElemDrop var);
         case
           "Load"
           (value ["$1"] Interpreter_encodings.Ast.loadop_encoding)
           (function Load loadop -> Some loadop | _ -> None)
           (fun loadop -> Load loadop);
         case
           "Store"
           (value ["$1"] Interpreter_encodings.Ast.storeop_encoding)
           (function Store loadop -> Some loadop | _ -> None)
           (fun loadop -> Store loadop);
         case
           "VecLoad"
           (value ["$1"] Interpreter_encodings.Ast.vec_loadop_encoding)
           (function VecLoad vec_loadop -> Some vec_loadop | _ -> None)
           (fun vec_loadop -> VecLoad vec_loadop);
         case
           "VecStore"
           (value ["$1"] Interpreter_encodings.Ast.vec_storeop_encoding)
           (function VecStore vec_loadop -> Some vec_loadop | _ -> None)
           (fun vec_storeop -> VecStore vec_storeop);
         case
           "VecLoadLane"
           (value ["$1"] Interpreter_encodings.Ast.vec_laneop_encoding)
           (function VecLoadLane vec_laneop -> Some vec_laneop | _ -> None)
           (fun vec_laneop -> VecLoadLane vec_laneop);
         case
           "VecStoreLane"
           (value ["$1"] Interpreter_encodings.Ast.vec_laneop_encoding)
           (function VecStoreLane vec_laneop -> Some vec_laneop | _ -> None)
           (fun vec_laneop -> VecStoreLane vec_laneop);
         case
           "MemorySize"
           unit_encoding
           (function MemorySize -> Some () | _ -> None)
           (fun () -> MemorySize);
         case
           "MemoryGrow"
           unit_encoding
           (function MemoryGrow -> Some () | _ -> None)
           (fun () -> MemoryGrow);
         case
           "MemoryFill"
           unit_encoding
           (function MemoryFill -> Some () | _ -> None)
           (fun () -> MemoryFill);
         case
           "MemoryCopy"
           unit_encoding
           (function MemoryCopy -> Some () | _ -> None)
           (fun () -> MemoryCopy);
         case
           "MemoryInit"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function MemoryInit var -> Some var | _ -> None)
           (fun var -> MemoryInit var);
         case
           "DataDrop"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function DataDrop var -> Some var | _ -> None)
           (fun var -> DataDrop var);
         case
           "RefNull"
           (value ["$1"] Interpreter_encodings.Types.ref_type_encoding)
           (function RefNull ref_type -> Some ref_type | _ -> None)
           (fun ref_type -> RefNull ref_type);
         case
           "RefFunc"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function RefFunc var -> Some var | _ -> None)
           (fun var -> RefFunc var);
         case
           "RefFunc"
           (value ["$1"] Interpreter_encodings.Ast.var_encoding)
           (function RefFunc var -> Some var | _ -> None)
           (fun var -> RefFunc var);
         case
           "RefIsNull"
           unit_encoding
           (function RefIsNull -> Some () | _ -> None)
           (fun () -> RefIsNull);
         case
           "Const"
           (value ["$1"] Interpreter_encodings.Ast.num_encoding)
           (function Const var -> Some var | _ -> None)
           (fun var -> Const var);
         case
           "Test"
           (value ["$1"] Interpreter_encodings.Ast.testop_encoding)
           (function Test var -> Some var | _ -> None)
           (fun var -> Test var);
         case
           "Compare"
           (value ["$1"] Interpreter_encodings.Ast.relop_encoding)
           (function Compare var -> Some var | _ -> None)
           (fun var -> Compare var);
         case
           "Unary"
           (value ["$1"] Interpreter_encodings.Ast.unop_encoding)
           (function Unary var -> Some var | _ -> None)
           (fun var -> Unary var);
         case
           "Binary"
           (value ["$1"] Interpreter_encodings.Ast.binop_encoding)
           (function Binary var -> Some var | _ -> None)
           (fun var -> Binary var);
         case
           "Convert"
           (value ["$1"] Interpreter_encodings.Ast.cvtop_encoding)
           (function Convert var -> Some var | _ -> None)
           (fun var -> Convert var);
         case
           "VecConst"
           (value ["$1"] Interpreter_encodings.Ast.vec_encoding)
           (function VecConst vec -> Some vec | _ -> None)
           (fun vec -> VecConst vec);
         case
           "VecTest"
           (value ["$1"] Interpreter_encodings.Ast.vec_testop_encoding)
           (function VecTest op -> Some op | _ -> None)
           (fun op -> VecTest op);
         case
           "VecCompare"
           (value ["$1"] Interpreter_encodings.Ast.vec_relop_encoding)
           (function VecCompare op -> Some op | _ -> None)
           (fun op -> VecCompare op);
         case
           "VecUnary"
           (value ["$1"] Interpreter_encodings.Ast.vec_unop_encoding)
           (function VecUnary op -> Some op | _ -> None)
           (fun op -> VecUnary op);
         case
           "VecBinary"
           (value ["$1"] Interpreter_encodings.Ast.vec_binop_encoding)
           (function VecBinary op -> Some op | _ -> None)
           (fun op -> VecBinary op);
         case
           "VecConvert"
           (value ["$1"] Interpreter_encodings.Ast.vec_cvtop_encoding)
           (function VecConvert op -> Some op | _ -> None)
           (fun op -> VecConvert op);
         case
           "VecShift"
           (value ["$1"] Interpreter_encodings.Ast.vec_shiftop_encoding)
           (function VecShift op -> Some op | _ -> None)
           (fun op -> VecShift op);
         case
           "VecBitmask"
           (value ["$1"] Interpreter_encodings.Ast.vec_bitmaskop_encoding)
           (function VecBitmask op -> Some op | _ -> None)
           (fun op -> VecBitmask op);
         case
           "VecTestBits"
           (value ["$1"] Interpreter_encodings.Ast.vec_vtestop_encoding)
           (function VecTestBits op -> Some op | _ -> None)
           (fun op -> VecTestBits op);
         case
           "VecUnaryBits"
           (value ["$1"] Interpreter_encodings.Ast.vec_vunop_encoding)
           (function VecUnaryBits op -> Some op | _ -> None)
           (fun op -> VecUnaryBits op);
         case
           "VecBinaryBits"
           (value ["$1"] Interpreter_encodings.Ast.vec_vbinop_encoding)
           (function VecBinaryBits op -> Some op | _ -> None)
           (fun op -> VecBinaryBits op);
         case
           "VecTernaryBits"
           (value ["$1"] Interpreter_encodings.Ast.vec_vternop_encoding)
           (function VecTernaryBits op -> Some op | _ -> None)
           (fun op -> VecTernaryBits op);
         case
           "VecSplat"
           (value ["$1"] Interpreter_encodings.Ast.vec_splatop_encoding)
           (function VecSplat op -> Some op | _ -> None)
           (fun op -> VecSplat op);
         case
           "VecExtract"
           (value ["$1"] Interpreter_encodings.Ast.vec_extractop_encoding)
           (function VecExtract op -> Some op | _ -> None)
           (fun op -> VecExtract op);
         case
           "VecReplace"
           (value ["$1"] Interpreter_encodings.Ast.vec_replaceop_encoding)
           (function VecReplace op -> Some op | _ -> None)
           (fun op -> VecReplace op);
       ])

let func_type_encoding =
  conv
    (fun (type_params, type_result) ->
      Types.FuncType (type_params, type_result))
    (fun (Types.FuncType (type_params, type_result)) ->
      (type_params, type_result))
    (tup2
       ~flatten:false
       (lazy_vector_encoding
          "type_params"
          (value [] Interpreter_encodings.Types.value_type_encoding))
       (lazy_vector_encoding
          "type_result"
          (value [] Interpreter_encodings.Types.value_type_encoding)))

let module_key_encoding =
  conv
    (fun key -> Instance.Module_key key)
    (fun (Instance.Module_key key) -> key)
    (value [] Data_encoding.string)

let function_encoding =
  tagged_union
    string_tag
    [
      case
        "Host"
        (tup2
           ~flatten:false
           func_type_encoding
           (value ["name"] Data_encoding.string))
        (function
          | Func.HostFunc (func_type, name) -> Some (func_type, name)
          | _ -> None)
        (fun (func_type, name) -> Func.HostFunc (func_type, name));
      case
        "Native"
        (tup5
           ~flatten:false
           function_type_encoding
           (scope ["module"] module_key_encoding)
           (value ["ftype"] Interpreter_encodings.Ast.var_encoding)
           (lazy_vector_encoding
              "locals"
              (value [] Interpreter_encodings.Types.value_type_encoding))
           block_label_encoding)
        (function
          | Func.AstFunc (type_, module_, {at = _; it = {ftype; locals; body}})
            ->
              Some (type_, module_, ftype, locals, body)
          | _ -> None)
        (fun (type_, module_, ftype, locals, body) ->
          let func = Source.{at = no_region; it = {Ast.ftype; locals; body}} in
          Func.AstFunc (type_, module_, func));
    ]

let value_ref_encoding =
  tagged_union
    string_tag
    [
      case
        "FuncRef"
        function_encoding
        (fun val_ref ->
          match val_ref with
          | Instance.FuncRef func_inst -> Some func_inst
          | _ -> None)
        (fun func -> Instance.FuncRef func);
      case
        "ExternRef"
        (value [] Data_encoding.int32)
        (function Values.ExternRef v -> Some v | _ -> None)
        (fun v -> Values.ExternRef v);
      case
        "NullRef"
        (value [] Interpreter_encodings.Types.ref_type_encoding)
        (function Values.NullRef v -> Some v | _ -> None)
        (fun v -> Values.NullRef v);
    ]

let value_encoding =
  tagged_union
    string_tag
    [
      case
        "NumType"
        (value [] Interpreter_encodings.Values.num_encoding)
        (function Values.Num n -> Some n | _ -> None)
        (fun n -> Values.Num n);
      case
        "VecType V128Type"
        (value [] Interpreter_encodings.Values.vec_encoding)
        (function Values.Vec v -> Some v | _ -> None)
        (fun v -> Values.Vec v);
      case
        "RefType"
        value_ref_encoding
        (function Values.Ref r -> Some r | _ -> None)
        (fun r -> Values.Ref r);
    ]

let values_encoding = lazy_vector_encoding' value_encoding

let name_encoding key = value [key] Data_encoding.string

let memory_encoding =
  conv
    (fun (min, max, chunks) -> Memory.of_chunks (MemoryType {min; max}) chunks)
    (fun memory_inst ->
      let (MemoryType {min; max}) = Memory.type_of memory_inst in
      let content = Memory.content memory_inst in
      (min, max, content))
    (tup3
       ~flatten:false
       (value ["min"] Data_encoding.int32)
       (value_option ["max"] Data_encoding.int32)
       (scope ["chunks"] Chunked_byte_vector.encoding))

let table_encoding =
  conv
    (fun (min, max, vector, ref_type) ->
      let table_type = Types.TableType ({min; max}, ref_type) in
      Table.of_lazy_vector table_type vector)
    (fun table ->
      let (Types.TableType ({min; max}, ref_type)) = Table.type_of table in
      (min, max, Table.content table, ref_type))
    (tup4
       ~flatten:false
       (value ["min"] Data_encoding.int32)
       (value_option ["max"] Data_encoding.int32)
       (lazy_vector_encoding "refs" value_ref_encoding)
       (value ["ref-type"] Interpreter_encodings.Types.ref_type_encoding))

let global_encoding =
  conv
    (fun (type_, value) ->
      let ty = Types.GlobalType (Values.type_of_value value, type_) in
      Global.alloc ty value)
    (fun global ->
      let (Types.GlobalType (_, mutability)) = Global.type_of global in
      let value = Global.load global in
      (mutability, value))
    (tup2
       ~flatten:false
       (value ["type"] Interpreter_encodings.Types.mutability_encoding)
       (scope ["value"] value_encoding))

let memory_instance_encoding = lazy_vector_encoding "memories" memory_encoding

let table_vector_encoding = lazy_vector_encoding "tables" table_encoding

let global_vector_encoding = lazy_vector_encoding "globals" global_encoding

let data_label_ref_encoding =
  conv (fun x -> ref x) (fun r -> !r) data_label_encoding

let function_vector_encoding =
  lazy_vector_encoding "functions" function_encoding

let function_type_vector_encoding =
  lazy_vector_encoding "types" function_type_encoding

let value_ref_vector_encoding = lazy_vector_encoding "refs" value_ref_encoding

let extern_encoding =
  tagged_union
    string_tag
    [
      case
        "ExternFunc"
        function_encoding
        (function Instance.ExternFunc x -> Some x | _ -> None)
        (fun x -> Instance.ExternFunc x);
      case
        "ExternTable"
        table_encoding
        (function Instance.ExternTable x -> Some x | _ -> None)
        (fun x -> Instance.ExternTable x);
      case
        "ExternMemory"
        memory_encoding
        (function Instance.ExternMemory x -> Some x | _ -> None)
        (fun x -> Instance.ExternMemory x);
      case
        "ExternGlobal"
        global_encoding
        (function Instance.ExternGlobal x -> Some x | _ -> None)
        (fun x -> Instance.ExternGlobal x);
    ]

let export_instance_encoding =
  tup2 ~flatten:false (name_encoding "name") (scope ["extern"] extern_encoding)

let extern_map_encoding = NameMap.lazy_map extern_encoding

let value_ref_vector_vector_encoding =
  lazy_vector_encoding
    "elements"
    (conv (fun x -> ref x) (fun r -> !r) value_ref_vector_encoding)

let data_instance_encoding =
  lazy_vector_encoding "datas" data_label_ref_encoding

let block_table_encoding =
  lazy_vector_encoding
    "block-table"
    (lazy_vector_encoding "instructions" instruction_encoding)

let datas_table_encoding =
  lazy_vector_encoding "datas-table" Chunked_byte_vector.encoding

let allocations_encoding =
  conv
    (fun (blocks, datas) -> Ast.{blocks; datas})
    (fun {blocks; datas} -> (blocks, datas))
    (tup2 ~flatten:false block_table_encoding datas_table_encoding)

let module_instance_encoding =
  conv
    (fun ( types,
           funcs,
           tables,
           memories,
           globals,
           exports,
           elems,
           datas,
           allocations ) ->
      {
        Instance.types;
        funcs;
        tables;
        memories;
        globals;
        exports;
        elems;
        datas;
        allocations;
      })
    (fun {
           Instance.types;
           funcs;
           tables;
           memories;
           globals;
           exports;
           elems;
           datas;
           allocations;
         } ->
      ( types,
        funcs,
        tables,
        memories,
        globals,
        exports,
        elems,
        datas,
        allocations ))
    (tup9
       ~flatten:false
       function_type_vector_encoding
       function_vector_encoding
       table_vector_encoding
       memory_instance_encoding
       global_vector_encoding
       extern_map_encoding
       value_ref_vector_vector_encoding
       data_instance_encoding
       allocations_encoding)

let module_instances_encoding =
  conv
    Instance.ModuleMap.of_immutable
    Instance.ModuleMap.snapshot
    (ModuleMap.lazy_map module_instance_encoding)

let frame_encoding =
  conv
    (fun (inst, locals) -> Eval.{inst; locals})
    (fun Eval.{inst; locals} -> (inst, locals))
    (tup2
       ~flatten:true
       (scope ["module"] module_key_encoding)
       (lazy_vector_encoding "locals" value_encoding))

let admin_instr'_encoding =
  let open Eval in
  tagged_union
    string_tag
    [
      case
        "From_block"
        (tup2
           ~flatten:false
           block_label_encoding
           (value [] Data_encoding.int32))
        (function
          | From_block (block, index) -> Some (block, index) | _ -> None)
        (fun (block, index) -> From_block (block, index));
      case
        "Plain"
        Source.(conv (fun i -> i.it) (at no_region) instruction_encoding)
        (function Plain x -> Some x | _ -> None)
        (fun x -> Plain x);
      case
        "Refer"
        value_ref_encoding
        (function Refer x -> Some x | _ -> None)
        (fun x -> Refer x);
      case
        "Invoke"
        function_encoding
        (function Invoke x -> Some x | _ -> None)
        (fun x -> Invoke x);
      case
        "Trapping"
        (value [] Data_encoding.string)
        (function Trapping x -> Some x | _ -> None)
        (fun x -> Trapping x);
      case
        "Returning"
        values_encoding
        (function Returning x -> Some x | _ -> None)
        (fun x -> Returning x);
      case
        "Breaking"
        (tup2 ~flatten:false (value [] Data_encoding.int32) values_encoding)
        (function
          | Breaking (index, values) -> Some (index, values) | _ -> None)
        (fun (index, values) -> Breaking (index, values));
      case
        "Table_init_meta"
        (tup7
           ~flatten:false
           (value [] Data_encoding.int32)
           value_ref_encoding
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Interpreter_encodings.Ast.var_encoding)
           (value [] Interpreter_encodings.Ast.var_encoding))
        (function
          | Table_init_meta (idx, value, d, s, n, x, y) ->
              Some (idx, value, d, s, n, x, y)
          | _ -> None)
        (fun (idx, value, d, s, n, x, y) ->
          Table_init_meta (idx, value, d, s, n, x, y));
      case
        "Table_fill_meta"
        (tup5
           ~flatten:false
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           value_ref_encoding
           (value [] Interpreter_encodings.Ast.var_encoding))
        (function
          | Table_fill_meta (idx, i, n, r, x) -> Some (idx, i, n, r, x)
          | _ -> None)
        (fun (idx, i, n, r, x) -> Table_fill_meta (idx, i, n, r, x));
      case
        "Table_copy_meta"
        (tup7
           ~flatten:false
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Interpreter_encodings.Ast.var_encoding)
           (value [] Interpreter_encodings.Ast.var_encoding)
           (value [] Data_encoding.bool))
        (function
          | Table_copy_meta (idx, d, s, n, x, y, case) ->
              Some (idx, d, s, n, x, y, case)
          | _ -> None)
        (fun (idx, d, s, n, x, y, case) ->
          Table_copy_meta (idx, d, s, n, x, y, case));
      case
        "Memory_init_meta"
        (tup6
           ~flatten:false
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Interpreter_encodings.Ast.var_encoding))
        (function
          | Memory_init_meta (idx, d, b, n, s, x) -> Some (idx, d, b, n, s, x)
          | _ -> None)
        (fun (idx, d, b, n, s, x) -> Memory_init_meta (idx, d, b, n, s, x));
      case
        "Memory_fill_meta"
        (tup4
           ~flatten:false
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Interpreter_encodings.Values.num_encoding)
           (value [] Data_encoding.int32))
        (function
          | Memory_fill_meta (idx, i, k, n) -> Some (idx, i, k, n) | _ -> None)
        (fun (idx, i, k, n) -> Memory_fill_meta (idx, i, k, n));
      case
        "Memory_copy_meta"
        (tup5
           ~flatten:false
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.int32)
           (value [] Data_encoding.bool))
        (function
          | Memory_copy_meta (idx, d, s, n, case) -> Some (idx, d, s, n, case)
          | _ -> None)
        (fun (idx, d, s, n, case) -> Memory_copy_meta (idx, d, s, n, case));
    ]

let admin_instr_encoding =
  conv Source.(at no_region) Source.(fun x -> x.it) admin_instr'_encoding

let input_buffer_message_encoding =
  conv_lwt
    (fun (raw_level, message_counter, payload) ->
      let open Lwt.Syntax in
      let+ payload = C.to_bytes payload in
      Input_buffer.{raw_level; message_counter; payload})
    (fun Input_buffer.{raw_level; message_counter; payload} ->
      let payload = C.of_bytes payload in
      Lwt.return (raw_level, message_counter, payload))
    (tup3
       ~flatten:true
       (value ["raw-level"] Data_encoding.int32)
       (value ["message-counter"] Data_encoding.z)
       Chunked_byte_vector.encoding)

let input_buffer_encoding =
  conv
    (fun index -> Lazy_vector.Mutable.ZVector.of_immutable index)
    (fun buffer -> Lazy_vector.Mutable.ZVector.snapshot buffer)
    (Lazy_vector.ZVector.encoding
       (value [] Data_encoding.z)
       input_buffer_message_encoding)

let label_encoding =
  conv
    (fun (label_arity, label_break, vs, es) ->
      Eval.{label_arity; label_break; label_code = (vs, es)})
    (fun {label_arity; label_break; label_code = vs, es} ->
      (label_arity, label_break, vs, es))
    (tup4
       ~flatten:true
       (value_option ["arity"] Data_encoding.int32)
       (scope ["label_break"] (option instruction_encoding))
       (scope ["values"] values_encoding)
       (lazy_vector_encoding "instructions" admin_instr_encoding))

let ongoing_label_kont_encoding : Eval.ongoing Eval.label_kont t =
  tagged_union
    string_tag
    [
      case
        "Label_stack"
        (tup2
           ~flatten:true
           (scope ["top"] label_encoding)
           (lazy_vector_encoding "rst" label_encoding))
        (function Eval.Label_stack (label, stack) -> Some (label, stack))
        (fun (label, stack) -> Label_stack (label, stack));
    ]

type packed_label_kont = Packed : 'a Eval.label_kont -> packed_label_kont

let packed_label_kont_encoding : packed_label_kont t =
  tagged_union
    string_tag
    [
      case
        "Label_stack"
        (tup2
           ~flatten:true
           (scope ["top"] label_encoding)
           (lazy_vector_encoding "rst" label_encoding))
        (function
          | Packed (Label_stack (label, stack)) -> Some (label, stack)
          | _ -> None)
        (fun (label, stack) -> Packed (Label_stack (label, stack)));
      case
        "Label_result"
        values_encoding
        (function Packed (Label_result vs0) -> Some vs0 | _ -> None)
        (fun vs0 -> Packed (Label_result vs0));
      case
        "Label_trapped"
        (value [] Data_encoding.string)
        (function Packed (Label_trapped msg) -> Some msg.it | _ -> None)
        (fun msg -> Packed (Label_trapped Source.(msg @@ no_region)));
    ]

let ongoing_frame_stack_encoding =
  conv
    (fun (frame_arity, frame_specs, frame_label_kont) ->
      Eval.{frame_arity; frame_specs; frame_label_kont})
    (fun {frame_arity; frame_specs; frame_label_kont} ->
      (frame_arity, frame_specs, frame_label_kont))
    (tup3
       ~flatten:true
       (value_option ["arity"] Data_encoding.int32)
       (scope ["frame"] frame_encoding)
       (scope ["label_kont"] ongoing_label_kont_encoding))

type packed_frame_stack =
  | Packed_fs : 'a Eval.frame_stack -> packed_frame_stack

let packed_frame_stack_encoding =
  conv
    (fun (frame_arity, frame_specs, Packed frame_label_kont) ->
      Packed_fs Eval.{frame_arity; frame_specs; frame_label_kont})
    (function
      | Packed_fs Eval.{frame_arity; frame_specs; frame_label_kont} ->
          (frame_arity, frame_specs, Packed frame_label_kont))
    (tup3
       ~flatten:true
       (value_option ["arity"] Data_encoding.int32)
       (scope ["frame"] frame_encoding)
       (scope ["label_kont"] packed_label_kont_encoding))

let reveal_hash_encoding = value [] (Data_encoding.string' Hex)

let reveal_encoding =
  tagged_union
    string_tag
    [
      case
        "Reveal_raw_data"
        reveal_hash_encoding
        (function Host_funcs.Reveal_raw_data hash -> Some hash | _ -> None)
        (fun hash -> Reveal_raw_data hash);
      case
        "Reveal_metadata"
        (value [] Data_encoding.unit)
        (function Host_funcs.Reveal_metadata -> Some () | _ -> None)
        (fun () -> Reveal_metadata);
    ]

let invoke_step_kont_encoding =
  tagged_union
    string_tag
    [
      case
        "Inv_start"
        (tup3
           ~flatten:true
           (scope ["func"] function_encoding)
           (scope ["values"] values_encoding)
           (lazy_vector_encoding "instructions" admin_instr_encoding))
        (function
          | Eval.Inv_start {func; code = vs, es} -> Some (func, vs, es)
          | _ -> None)
        (fun (func, vs, es) -> Inv_start {func; code = (vs, es)});
      case
        "Inv_prepare_locals"
        (tup7
           ~flatten:true
           (value ["arity"] Data_encoding.int32)
           (lazy_vector_encoding "args" value_encoding)
           (lazy_vector_encoding "values" value_encoding)
           (lazy_vector_encoding "instructions" admin_instr_encoding)
           (scope ["inst"] module_key_encoding)
           (scope ["func"] func_encoding)
           (scope
              ["kont"]
              (map_kont_encoding
                 (lazy_vector_encoding
                    "x"
                    (value [] Interpreter_encodings.Types.value_type_encoding))
                 (lazy_vector_encoding "y" value_encoding))))
        (function
          | Eval.Inv_prepare_locals
              {arity; args; vs; instructions; inst; func; locals_kont} ->
              Some (arity, args, vs, instructions, inst, func, locals_kont)
          | _ -> None)
        (fun (arity, args, vs, instructions, inst, func, locals_kont) ->
          Inv_prepare_locals
            {arity; args; vs; instructions; inst; func; locals_kont});
      case
        "Inv_prepare_args"
        (tup7
           ~flatten:true
           (value ["arity"] Data_encoding.int32)
           (lazy_vector_encoding "values" value_encoding)
           (lazy_vector_encoding "instructions" admin_instr_encoding)
           (scope ["inst"] module_key_encoding)
           (scope ["func"] func_encoding)
           (lazy_vector_encoding "locals" value_encoding)
           (scope
              ["kont"]
              (map_kont_encoding
                 (lazy_vector_encoding "1" value_encoding)
                 (lazy_vector_encoding "2" value_encoding))))
        (function
          | Eval.Inv_prepare_args
              {arity; vs; instructions; inst; func; locals; args_kont} ->
              Some (arity, vs, instructions, inst, func, locals, args_kont)
          | _ -> None)
        (fun (arity, vs, instructions, inst, func, locals, args_kont) ->
          Inv_prepare_args
            {arity; vs; instructions; inst; func; locals; args_kont});
      case
        "Inv_concat"
        (tup6
           ~flatten:true
           (value ["arity"] Data_encoding.int32)
           (lazy_vector_encoding "values" value_encoding)
           (lazy_vector_encoding "instructions" admin_instr_encoding)
           (scope ["inst"] module_key_encoding)
           (scope ["func"] func_encoding)
           (scope
              ["kont"]
              (concat_kont_encoding (lazy_vector_encoding "2" value_encoding))))
        (function
          | Eval.Inv_concat {arity; vs; instructions; inst; func; concat_kont}
            ->
              Some (arity, vs, instructions, inst, func, concat_kont)
          | _ -> None)
        (fun (arity, vs, instructions, inst, func, concat_kont) ->
          Inv_concat {arity; vs; instructions; inst; func; concat_kont});
      case
        "Inv_reveal_tick"
        (tup5
           ~flatten:true
           (scope ["reveal"] reveal_encoding)
           (value ["base_destination"] Data_encoding.int32)
           (value ["max_bytes"] Data_encoding.int32)
           (lazy_vector_encoding "values" value_encoding)
           (lazy_vector_encoding "instructions" admin_instr_encoding))
        (function
          | Eval.Inv_reveal_tick
              {reveal; base_destination; max_bytes; code = vs, es} ->
              Some (reveal, base_destination, max_bytes, vs, es)
          | _ -> None)
        (fun (reveal, base_destination, max_bytes, vs, es) ->
          Inv_reveal_tick {reveal; base_destination; max_bytes; code = (vs, es)});
      case
        "Inv_stop"
        (tup4
           ~flatten:true
           (scope ["values"] values_encoding)
           (lazy_vector_encoding "instructions" admin_instr_encoding)
           (scope ["fresh_frame"] (option ongoing_frame_stack_encoding))
           (value ["remaining_ticks"] Data_encoding.z))
        (function
          | Eval.Inv_stop {code = vs, es; fresh_frame; remaining_ticks} ->
              Some (vs, es, fresh_frame, remaining_ticks)
          | _ -> None)
        (fun (vs, es, fresh_frame, remaining_ticks) ->
          Inv_stop {code = (vs, es); fresh_frame; remaining_ticks});
    ]

let label_step_kont_encoding =
  tagged_union
    string_tag
    [
      case
        "LS_Start"
        ongoing_label_kont_encoding
        (function Eval.LS_Start label -> Some label | _ -> None)
        (fun label -> LS_Start label);
      case
        "LS_Craft_frame"
        (tup2
           ~flatten:true
           (scope ["label_kont"] ongoing_label_kont_encoding)
           (scope ["invoke_kont"] invoke_step_kont_encoding))
        (function Eval.LS_Craft_frame (l, i) -> Some (l, i) | _ -> None)
        (fun (l, i) -> LS_Craft_frame (l, i));
      case
        "LS_Push_frame"
        (tup2
           ~flatten:true
           (scope ["label_kont"] ongoing_label_kont_encoding)
           (scope ["fresh_frame"] ongoing_frame_stack_encoding))
        (function Eval.LS_Push_frame (l, i) -> Some (l, i) | _ -> None)
        (fun (l, i) -> LS_Push_frame (l, i));
      case
        "LS_Consolidate_top"
        (tup4
           ~flatten:true
           (scope ["label"] label_encoding)
           (scope
              ["kont"]
              (concat_kont_encoding (lazy_vector_encoding' value_encoding)))
           (lazy_vector_encoding "instructions" admin_instr_encoding)
           (lazy_vector_encoding "labels-stack" label_encoding))
        (function
          | Eval.LS_Consolidate_top (l, k, es, s) -> Some (l, k, es, s)
          | _ -> None)
        (fun (l, k, es, s) -> LS_Consolidate_top (l, k, es, s));
      case
        "LS_Modify_top"
        packed_label_kont_encoding
        (function Eval.LS_Modify_top l -> Some (Packed l) | _ -> None)
        (fun (Packed l) -> LS_Modify_top l);
    ]

let step_kont_encoding =
  tagged_union
    string_tag
    [
      case
        "SK_Start"
        (tup2
           ~flatten:true
           (scope ["top"] packed_frame_stack_encoding)
           (lazy_vector_encoding "rst" ongoing_frame_stack_encoding))
        (function
          | Eval.SK_Start (f, rst) -> Some (Packed_fs f, rst) | _ -> None)
        (fun (Packed_fs f, rst) -> SK_Start (f, rst));
      case
        "SK_Next"
        (tup3
           ~flatten:true
           (scope ["top"] packed_frame_stack_encoding)
           (lazy_vector_encoding "rst" ongoing_frame_stack_encoding)
           (scope ["kont"] label_step_kont_encoding))
        (function
          | Eval.SK_Next (f, r, k) -> Some (Packed_fs f, r, k) | _ -> None)
        (fun (Packed_fs f, r, k) -> SK_Next (f, r, k));
      case
        "SK_Consolidate_label_result"
        (tup6
           ~flatten:true
           (scope ["top-frame"] ongoing_frame_stack_encoding)
           (lazy_vector_encoding "frames-stack" ongoing_frame_stack_encoding)
           (scope ["top-label"] label_encoding)
           (scope
              ["kont"]
              (concat_kont_encoding (lazy_vector_encoding' value_encoding)))
           (lazy_vector_encoding "instructions" admin_instr_encoding)
           (lazy_vector_encoding "labels-stack" label_encoding))
        (function
          | Eval.SK_Consolidate_label_result
              (frame', stack, label, vs, es, lstack) ->
              Some (frame', stack, label, vs, es, lstack)
          | _ -> None)
        (fun (frame', stack, label, vs, es, lstack) ->
          SK_Consolidate_label_result (frame', stack, label, vs, es, lstack));
      case
        "SK_Result"
        values_encoding
        (function Eval.SK_Result vs -> Some vs | _ -> None)
        (fun vs -> SK_Result vs);
      case
        "SK_Trapped"
        (value [] Data_encoding.string)
        (function Eval.SK_Trapped msg -> Some msg.it | _ -> None)
        (fun msg -> SK_Trapped Source.(msg @@ no_region));
    ]

let messages_encoding =
  conv
    (fun index -> Output_buffer.Messages.of_immutable index)
    (fun buffer -> Output_buffer.Messages.snapshot buffer)
    (Lazy_vector.ZVector.encoding
       (value [] Data_encoding.z)
       (value [] Data_encoding.bytes))

let outboxes_encoding =
  conv
    Output_buffer.Outboxes.of_immutable
    Output_buffer.Outboxes.snapshot
    (Outboxes.lazy_map messages_encoding)

let output_buffer_encoding =
  let open Output_buffer in
  conv
    (fun (outboxes, last_level, validity_period, message_limit) ->
      {outboxes; last_level; validity_period; message_limit})
    (fun {outboxes; last_level; validity_period; message_limit} ->
      (outboxes, last_level, validity_period, message_limit))
    (tup4
       ~flatten:false
       (scope ["outboxes"] outboxes_encoding)
       (value_option ["last_level"] Data_encoding.int32)
       (value ["validity_period"] Data_encoding.int32)
       (value ["message_limit"] Data_encoding.z))

let config_encoding =
  conv
    (fun (step_kont, stack_size_limit) -> Eval.{step_kont; stack_size_limit})
    (fun Eval.{step_kont; stack_size_limit} -> (step_kont, stack_size_limit))
    (tup2
       ~flatten:true
       (scope ["step_kont"] step_kont_encoding)
       (value ["stack_size_limit"] Data_encoding.int31))

let buffers_encoding =
  conv
    (fun (input, output) -> Eval.{input; output})
    (fun Eval.{input; output; _} -> (input, output))
    (tup2
       ~flatten:true
       (scope ["input"] input_buffer_encoding)
       (scope ["output"] output_buffer_encoding))
