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
open Types
open Data_encoding_utils

module Make (T : Tree.S) = struct
  module Tree = struct
    include T
    module Decoding = Tree_decoding.Make (T)
  end

  open Tree.Decoding

  let list_decoding item_enc =
    let open Syntax in
    let* length = value ["length"] Data_encoding.int32 in
    let* head = value ["head"] Data_encoding.int32 in
    let* get_item = lazy_mapping (fun key -> [Int32.to_string key]) item_enc in
    let produce_value key = get_item (Int32.add head key) in
    let vector = Instance.Vector.create ~produce_value length in
    (* TODO: #3076
       This should return a [Instance.Vector.t] instead of a list. Once the AST
       has been sufficiently adapted to lazy vectors and maps, this change can
       go forward. *)
    of_lwt (Instance.Vector.to_list vector)

  let ref_decoding_for ref_type modules =
    let open Syntax in
    match ref_type with
    | FuncRefType ->
        let* modul_id = value ["module"] Data_encoding.int32 in
        let* func_id = value ["function"] Data_encoding.int32 in
        of_lwt
          (let open Lwt.Syntax in
          let* modul = Instance.Vector.get modul_id modules in
          let+ func = Instance.Vector.get func_id modul.Instance.funcs in
          Instance.FuncRef func)
    | ExternRefType ->
        let+ value = value ["value"] Data_encoding.int32 in
        Script.ExternRef value

  let ref_decoding modules =
    let open Syntax in
    let* ref_type =
      value ["type"] Interpreter_encodings.Types.ref_type_encoding
    in
    ref_decoding_for ref_type modules

  let value_decoding modules =
    let open Syntax in
    let* value_type =
      value ["type"] Interpreter_encodings.Types.value_type_encoding
    in
    let get_value_with enc f =
      let+ value = value ["value"] enc in
      f value
    in
    match value_type with
    | NumType I32Type ->
        get_value_with Data_encoding.int32 (fun x -> Values.Num (Values.I32 x))
    | NumType I64Type ->
        get_value_with Data_encoding.int64 (fun x -> Values.Num (Values.I64 x))
    | VecType V128Type ->
        get_value_with Data_encoding.string (fun x ->
            Values.Vec (V128 (V128.of_bits x)))
    | RefType ref_type ->
        let+ ref_value = ref_decoding_for ref_type modules in
        Values.Ref ref_value
    | _ -> failwith "Unsupported value_type"

  let var_list_decoding =
    list_decoding (value [] Interpreter_encodings.Ast.var_encoding)

  let memory_chunk_decoding =
    let open Syntax in
    let+ bytes = raw [] in
    Chunked_byte_vector.Chunk.of_bytes bytes

  let memory_decoding =
    let open Syntax in
    let+ min_pages = value ["min"] Data_encoding.int32
    and+ max_pages = value ["max"] Data_encoding.int32
    and+ get_chunk =
      tree
        ["chunks"]
        (lazy_mapping
           (fun index -> [Int64.to_string index])
           memory_chunk_decoding)
    in
    let length = Int64.(of_int32 min_pages |> mul Memory.page_size) in
    let chunks = Chunked_byte_vector.Lwt.create ~get_chunk length in
    Memory.of_chunks (MemoryType {min = min_pages; max = Some max_pages}) chunks

  let table_decoding modules =
    let open Syntax in
    let+ min = value ["min"] Data_encoding.int32
    and+ max = value ["max"] Data_encoding.int32
    and+ get_ref =
      tree
        ["refs"]
        (lazy_mapping
           (fun index -> [Int32.to_string index])
           (ref_decoding modules))
    in
    let table_type = TableType ({min; max = Some max}, FuncRefType) in
    let table_entries = Table.Vector.Vector.create ~produce_value:get_ref min in
    Table.of_lazy_vector table_type table_entries

  let global_decoding modules =
    let open Syntax in
    let+ type_ = value ["type"] Interpreter_encodings.Types.mutability_encoding
    and+ value = tree ["value"] (value_decoding modules) in
    let ty = GlobalType (Values.type_of_value value, type_) in
    Global.alloc ty value

  let lazy_vector_decoding field_name tree_encoding =
    let open Syntax in
    let+ count = value ["num-" ^ field_name] Data_encoding.int32
    and+ get_instance =
      tree
        [field_name]
        (lazy_mapping (fun index -> [Int32.to_string index]) tree_encoding)
    in
    Instance.Vector.create ~produce_value:get_instance count

  let memory_instance_decoding = lazy_vector_decoding "memories" memory_decoding

  let table_instance_decoding modules =
    lazy_vector_decoding "tables" (table_decoding modules)

  let global_instance_decoding modules =
    lazy_vector_decoding "globals" (global_decoding modules)

  let data_decoding =
    let open Syntax in
    let+ length = value ["length"] Data_encoding.int64
    and+ get_chunk =
      lazy_mapping (fun index -> [Int64.to_string index]) memory_chunk_decoding
    in
    ref (Chunked_byte_vector.Lwt.create ~get_chunk length)

  let data_instance_decoding = lazy_vector_decoding "datas" data_decoding

  let rec instruction_decoding () =
    let open Ast in
    let open Syntax in
    let* tag = value ["tag"] Data_encoding.string in
    let+ instr =
      match tag with
      | "Unreachable" -> return Unreachable
      | "Nop" -> return Nop
      | "Drop" -> return Drop
      | "Select" ->
          let+ param =
            value
              ["$1"]
              (Data_encoding.option
                 Interpreter_encodings.Types.result_type_encoding)
          in
          Select param
      | "Block" ->
          let+ type_ =
            value ["$1"] Interpreter_encodings.Ast.block_type_encoding
          and+ instrs = tree ["$2"] (instruction_list_decoding ()) in
          Block (type_, instrs)
      | "Loop" ->
          let+ type_ =
            value ["$1"] Interpreter_encodings.Ast.block_type_encoding
          and+ instrs = tree ["$2"] (instruction_list_decoding ()) in
          Loop (type_, instrs)
      | "If" ->
          let+ type_ =
            value ["$1"] Interpreter_encodings.Ast.block_type_encoding
          and+ instrs_if = tree ["$2"] (instruction_list_decoding ())
          and+ instrs_else = tree ["$3"] (instruction_list_decoding ()) in
          If (type_, instrs_if, instrs_else)
      | "Br" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          Br var
      | "BrIf" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          BrIf var
      | "BrTable" ->
          let+ table = tree ["$1"] var_list_decoding
          and+ target = value ["$2"] Interpreter_encodings.Ast.var_encoding in
          BrTable (table, target)
      | "Return" -> return Return
      | "Call" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          Call var
      | "CallIndirect" ->
          let+ var1 = value ["$1"] Interpreter_encodings.Ast.var_encoding
          and+ var2 = value ["$2"] Interpreter_encodings.Ast.var_encoding in
          CallIndirect (var1, var2)
      | "LocalGet" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          LocalGet var
      | "LocalSet" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          LocalSet var
      | "LocalTee" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          LocalTee var
      | "GlobalGet" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          GlobalGet var
      | "GlobalSet" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          GlobalSet var
      | "TableGet" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          TableGet var
      | "TableSet" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          TableSet var
      | "TableSize" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          TableSize var
      | "TableGrow" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          TableGrow var
      | "TableFill" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          TableFill var
      | "TableCopy" ->
          let+ var1 = value ["$1"] Interpreter_encodings.Ast.var_encoding
          and+ var2 = value ["$2"] Interpreter_encodings.Ast.var_encoding in
          TableCopy (var1, var2)
      | "TableInit" ->
          let+ var1 = value ["$1"] Interpreter_encodings.Ast.var_encoding
          and+ var2 = value ["$2"] Interpreter_encodings.Ast.var_encoding in
          TableInit (var1, var2)
      | "ElemDrop" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          ElemDrop var
      | "Load" ->
          let+ loadop =
            value ["$1"] Interpreter_encodings.Ast.loadop_encoding
          in
          Load loadop
      | "Store" ->
          let+ storeop =
            value ["$1"] Interpreter_encodings.Ast.storeop_encoding
          in
          Store storeop
      | "VecLoad" ->
          let+ vec_loadop =
            value ["$1"] Interpreter_encodings.Ast.vec_loadop_encoding
          in
          VecLoad vec_loadop
      | "VecStore" ->
          let+ vec_storeop =
            value ["$1"] Interpreter_encodings.Ast.vec_storeop_encoding
          in
          VecStore vec_storeop
      | "VecLoadLane" ->
          let+ vec_laneop =
            value ["$1"] Interpreter_encodings.Ast.vec_laneop_encoding
          in
          VecLoadLane vec_laneop
      | "VecStoreLane" ->
          let+ vec_laneop =
            value ["$1"] Interpreter_encodings.Ast.vec_laneop_encoding
          in
          VecStoreLane vec_laneop
      | "MemorySize" -> return MemorySize
      | "MemoryGrow" -> return MemoryGrow
      | "MemoryFill" -> return MemoryFill
      | "MemoryCopy" -> return MemoryCopy
      | "MemoryInit" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          MemoryInit var
      | "DataDrop" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          DataDrop var
      | "RefNull" ->
          let+ ref_type =
            value ["$1"] Interpreter_encodings.Types.ref_type_encoding
          in
          RefNull ref_type
      | "RefFunc" ->
          let+ var = value ["$1"] Interpreter_encodings.Ast.var_encoding in
          RefFunc var
      | "RefIsNull" -> return RefIsNull
      | "Const" ->
          let+ num = value ["$1"] Interpreter_encodings.Ast.num_encoding in
          Const num
      | "Test" ->
          let+ param = value ["$1"] Interpreter_encodings.Ast.testop_encoding in
          Test param
      | "Compare" ->
          let+ param = value ["$1"] Interpreter_encodings.Ast.relop_encoding in
          Compare param
      | "Unary" ->
          let+ param = value ["$1"] Interpreter_encodings.Ast.unop_encoding in
          Unary param
      | "Binary" ->
          let+ param = value ["$1"] Interpreter_encodings.Ast.binop_encoding in
          Binary param
      | "Convert" ->
          let+ param = value ["$1"] Interpreter_encodings.Ast.cvtop_encoding in
          Convert param
      | "VecConst" ->
          let+ vec = value ["$1"] Interpreter_encodings.Ast.vec_encoding in
          VecConst vec
      | "VecTest" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_testop_encoding
          in
          VecTest op
      | "VecCompare" ->
          let+ op = value ["$1"] Interpreter_encodings.Ast.vec_relop_encoding in
          VecCompare op
      | "VecUnary" ->
          let+ op = value ["$1"] Interpreter_encodings.Ast.vec_unop_encoding in
          VecUnary op
      | "VecBinary" ->
          let+ op = value ["$1"] Interpreter_encodings.Ast.vec_binop_encoding in
          VecBinary op
      | "VecConvert" ->
          let+ op = value ["$1"] Interpreter_encodings.Ast.vec_cvtop_encoding in
          VecConvert op
      | "VecShift" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_shiftop_encoding
          in
          VecShift op
      | "VecBitmask" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_bitmaskop_encoding
          in
          VecBitmask op
      | "VecTestBits" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_vtestop_encoding
          in
          VecTestBits op
      | "VecUnaryBits" ->
          let+ op = value ["$1"] Interpreter_encodings.Ast.vec_vunop_encoding in
          VecUnaryBits op
      | "VecBinaryBits" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_vbinop_encoding
          in
          VecBinaryBits op
      | "VecTernaryBits" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_vternop_encoding
          in
          VecTernaryBits op
      | "VecSplat" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_splatop_encoding
          in
          VecSplat op
      | "VecExtract" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_extractop_encoding
          in
          VecExtract op
      | "VecReplace" ->
          let+ op =
            value ["$1"] Interpreter_encodings.Ast.vec_replaceop_encoding
          in
          VecReplace op
      | _ -> failwith (Printf.sprintf "Unknown instruction tag %s" tag)
    in
    Source.(instr @@ no_region)

  and instruction_list_decoding () =
    (* TODO: #3149
       Rewrite instruction list encoding using virtual "instruction block"
       pointers. *)
    list_decoding (instruction_decoding ())

  let function_decoding current_module =
    let open Syntax in
    let* is_host_func =
      value
        ["kind"]
        (Data_encoding.string_enum [("host", true); ("native", false)])
    in
    if is_host_func then
      let+ name = value ["name"] Data_encoding.string in
      Host_funcs.lookup name
    else
      let* type_ =
        value ["type"] Interpreter_encodings.Types.func_type_encoding
      in
      let* ftype = value ["ftype"] Interpreter_encodings.Ast.var_encoding in
      let* locals =
        value ["locals"] Interpreter_encodings.Types.result_type_encoding
      in
      let+ body = instruction_list_decoding () in
      let func = Ast.{ftype; locals; body} in
      Func.AstFunc (type_, current_module, Source.(func @@ no_region))

  let function_instance_decoding current_module =
    lazy_vector_decoding "functions" (function_decoding current_module)

  let type_instance_decoding =
    lazy_vector_decoding
      "types"
      (value [] Interpreter_encodings.Types.func_type_encoding)

  let elem_decoding funcs =
    let open Syntax in
    let+ length = value ["length"] Data_encoding.int32
    and+ get_ref =
      lazy_mapping
        (fun index -> [Int32.to_string index])
        (value [] Data_encoding.int32)
    in
    let produce_value index =
      let open Lwt.Syntax in
      let* func_id = get_ref index in
      let+ func = Instance.Vector.get func_id funcs in
      Instance.FuncRef func
    in
    ref (Instance.Vector.create ~produce_value length)

  let elem_instance_decoding funcs =
    lazy_vector_decoding "elements" (elem_decoding funcs)

  type export =
    | ExportFunc of int32
    | ExportTable of int32
    | ExportMemory of int32
    | ExportGlobal of int32

  let export_decoding funcs tables memories globals =
    let open Syntax in
    let* export =
      value
        []
        Data_encoding.(
          union_incr
            [
              case_incr
                "ExportFunc"
                int32
                (function ExportFunc x -> Some x | _ -> None)
                (fun x -> ExportFunc x);
              case_incr
                "ExportTable"
                int32
                (function ExportTable x -> Some x | _ -> None)
                (fun x -> ExportTable x);
              case_incr
                "ExportMemory"
                int32
                (function ExportMemory x -> Some x | _ -> None)
                (fun x -> ExportMemory x);
              case_incr
                "ExportGlobal"
                int32
                (function ExportGlobal x -> Some x | _ -> None)
                (fun x -> ExportGlobal x);
            ])
    in
    match export with
    | ExportFunc index ->
        let+ value = of_lwt (Instance.Vector.get index funcs) in
        Instance.ExternFunc value
    | ExportTable index ->
        let+ value = of_lwt (Instance.Vector.get index tables) in
        Instance.ExternTable value
    | ExportMemory index ->
        let+ value = of_lwt (Instance.Vector.get index memories) in
        Instance.ExternMemory value
    | ExportGlobal index ->
        let+ value = of_lwt (Instance.Vector.get index globals) in
        Instance.ExternGlobal value

  let export_instance_decoding funcs tables memories globals =
    let open Syntax in
    let+ get_export =
      lazy_mapping
        (fun name -> Format.[asprintf "%a" (pp_print_list pp_print_int) name])
        (export_decoding funcs tables memories globals)
    in
    Instance.NameMap.create ~produce_value:get_export ()

  let module_instance_decoding modules =
    let open Syntax in
    let current_module = ref Instance.empty_module_inst in
    let* memories = memory_instance_decoding in
    let* tables = table_instance_decoding modules in
    let* globals = global_instance_decoding modules in
    let* datas = data_instance_decoding in
    let* types = type_instance_decoding in
    let* funcs = function_instance_decoding current_module in
    let* elems = elem_instance_decoding funcs in
    let+ exports = export_instance_decoding funcs tables memories globals in
    let modul =
      Instance.{memories; tables; globals; datas; types; funcs; elems; exports}
    in
    current_module := modul ;
    modul

  let module_instances_decoding =
    let open Syntax in
    let self = ref (fun () -> failwith "Uninitialized!") in
    let* count = value ["count"] Data_encoding.int32 in
    let modules =
      (* This is a proxy for the mapping returned by the function contained in
         [self]. *)
      Instance.Vector.create
        ~produce_value:(fun index -> Lazy_map.LwtInt32Map.get index (!self ()))
        count
    in
    let+ get_module_inst =
      lazy_mapping
        (fun index -> [Int32.to_string index])
        (module_instance_decoding modules)
    in
    let self_modules =
      Lazy_map.LwtInt32Map.create ~produce_value:get_module_inst ()
    in
    (* Update the self pointer to effectively close the recursion
       [self_modules -> get_module_inst -> modules -> self -> self_modules]. *)
    (self := fun () -> self_modules) ;
    modules
end
