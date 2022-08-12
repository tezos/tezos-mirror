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
open Lazy_containers

exception Uninitialized_current_module

module Make (Tree_encoding : Tree_encoding.S) = struct
  module V = Instance.Vector
  module M = Instance.NameMap
  module C = Chunked_byte_vector.Lwt
  include Tree_encoding
  include Lazy_vector_encoding.Int32
  module NameMap = Lazy_map_encoding.Make (Instance.NameMap)
  module ModuleMap = Lazy_map_encoding.Make (Instance.ModuleMap.Map)

  (** Utility function*)
  let string_tag = value [] Data_encoding.string

  let list_encoding item_enc =
    let vector = lazy_vector (value [] Data_encoding.int32) item_enc in
    (* TODO: #3076
       This should return a [Instance.Vector.t] instead of a list. Once the AST
       has been sufficiently adapted to lazy vectors and maps, this change can
       go forward. *)
    conv_lwt V.to_list (fun list -> Lwt.return (V.of_list list)) vector

  let lazy_vector_encoding field_name tree_encoding =
    scope
      [field_name]
      (lazy_vector (value [] Data_encoding.int32) tree_encoding)

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
    list_encoding (value [] Interpreter_encodings.Ast.var_encoding)

  let block_label_encoding =
    value [] Interpreter_encodings.Ast.block_label_encoding

  let data_label_encoding =
    value [] Interpreter_encodings.Ast.data_label_encoding

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
             (function
               | Block (type_, instr) -> Some (type_, instr) | _ -> None)
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
             (function
               | TableCopy (var1, var2) -> Some (var1, var2) | _ -> None)
             (fun (var1, var2) -> TableCopy (var1, var2));
           case
             "TableInit"
             (tup2
                ~flatten:false
                (value ["$1"] Interpreter_encodings.Ast.var_encoding)
                (value ["$2"] Interpreter_encodings.Ast.var_encoding))
             (function
               | TableInit (var1, var2) -> Some (var1, var2) | _ -> None)
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

  let module_ref_encoding =
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
             (scope ["module"] module_ref_encoding)
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
            let func =
              Source.{at = no_region; it = {Ast.ftype; locals; body}}
            in
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

  let values_encoding = list_encoding value_encoding

  let memory_encoding =
    conv
      (fun (min, max, chunks) ->
        Memory.of_chunks (MemoryType {min; max}) chunks)
      (fun memory_inst ->
        let (MemoryType {min; max}) = Memory.type_of memory_inst in
        let content = Memory.content memory_inst in
        (min, max, content))
      (tup3
         ~flatten:false
         (value ["min"] Data_encoding.int32)
         (value_option ["max"] Data_encoding.int32)
         (scope ["chunks"] chunked_byte_vector))

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

  let extern_map_encoding =
    NameMap.lazy_map
      (tagged_union
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
         ])

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
    lazy_vector_encoding "datas-table" chunked_byte_vector

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
      (scope ["modules"] (ModuleMap.lazy_map module_instance_encoding))

  let frame_encoding =
    let locals_encoding = list_encoding @@ conv ref ( ! ) @@ value_encoding in
    conv
      (fun (inst, locals) -> Eval.{inst; locals})
      (fun Eval.{inst; locals} -> (inst, locals))
      (tup2
         ~flatten:true
         (scope ["module"] module_ref_encoding)
         (scope ["locals"] locals_encoding))

  let rec admin_instr'_encoding () =
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
          "Label"
          (tup4
             ~flatten:false
             (value [] Data_encoding.int32)
             (list_encoding instruction_encoding)
             values_encoding
             (list_encoding (admin_instr_encoding ())))
          (function
            | Label (index, final_instrs, (values, instrs)) ->
                Some (index, final_instrs, values, instrs)
            | _ -> None)
          (fun (index, final_instrs, values, instrs) ->
            Label (index, final_instrs, (values, instrs)));
        case
          "Frame"
          (tup4
             ~flatten:false
             (value [] Data_encoding.int32)
             frame_encoding
             values_encoding
             (list_encoding (admin_instr_encoding ())))
          (function
            | Frame (index, frame, (values, instrs)) ->
                Some (index, frame, values, instrs)
            | _ -> None)
          (fun (index, frame, values, instrs) ->
            Frame (index, frame, (values, instrs)));
      ]

  and admin_instr_encoding () =
    conv
      Source.(at no_region)
      Source.(fun x -> x.it)
      (delayed admin_instr'_encoding)

  let admin_instr_encoding = admin_instr_encoding ()

  let input_buffer_message_encoding =
    conv_lwt
      (fun (rtype, raw_level, message_counter, payload) ->
        let open Lwt.Syntax in
        let+ payload = C.to_bytes payload in
        Input_buffer.{rtype; raw_level; message_counter; payload})
      (fun Input_buffer.{rtype; raw_level; message_counter; payload} ->
        let payload = C.of_bytes payload in
        Lwt.return (rtype, raw_level, message_counter, payload))
      (tup4
         ~flatten:true
         (value ["rtype"] Data_encoding.int32)
         (value ["raw-level"] Data_encoding.int32)
         (value ["message-counter"] Data_encoding.z)
         chunked_byte_vector)

  module InputBufferVec = Lazy_vector_encoding.Make (Lazy_vector.LwtZVector)

  let input_buffer_encoding =
    conv
      (fun (content, num_elements) ->
        {
          Input_buffer.content =
            Lazy_vector.Mutable.LwtZVector.of_immutable content;
          num_elements;
        })
      (fun buffer ->
        Input_buffer.
          ( Lazy_vector.Mutable.LwtZVector.snapshot buffer.content,
            buffer.num_elements ))
      (tup2
         ~flatten:true
         (scope
            ["messages"]
            (InputBufferVec.lazy_vector
               (value [] Data_encoding.z)
               input_buffer_message_encoding))
         (value ["num-messages"] Data_encoding.z))

  let config_encoding ~host_funcs =
    conv
      (fun (frame, input, instrs, values, budget) ->
        Eval.{frame; input; code = (values, instrs); host_funcs; budget})
      (fun Eval.{frame; input; code = values, instrs; budget; _} ->
        (frame, input, instrs, values, budget))
      (tup5
         ~flatten:true
         (scope ["frame"] frame_encoding)
         (scope ["input"] input_buffer_encoding)
         (scope ["instructions"] (list_encoding admin_instr_encoding))
         (scope ["values"] values_encoding)
         (value ["budget"] Data_encoding.int31))
end
