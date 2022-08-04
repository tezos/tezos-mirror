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

open Tezos_webassembly_interpreter
open Lazy_containers
open QCheck2.Gen

let no_region it = Source.{it; at = no_region}

let var_gen =
  let+ n = int32 in
  no_region n

(* The types [F32Type] and [F64Type] are not currently supported. *)
let num_type_gen =
  let open Types in
  oneofl [I32Type; I64Type]

let ref_type_gen =
  let open Types in
  oneofl [FuncRefType; ExternRefType]

let value_type_gen =
  let open Types in
  oneof
    [
      map (fun nt -> NumType nt) num_type_gen;
      return (VecType V128Type);
      map (fun rt -> RefType rt) ref_type_gen;
    ]

let value_types_gen = small_list value_type_gen

let block_label_gen =
  let+ l = int32 in
  Ast.Block_label l

let data_label_gen =
  let+ l = int32 in
  Ast.Data_label l

let block_type_gen =
  let open Ast in
  oneof
    [
      map (fun v -> VarBlockType v) var_gen;
      map (fun v -> ValBlockType v) (opt value_type_gen);
    ]

let memop_gen ty_gen pack_gen =
  let+ align = int_range 0 255
  and+ ty = ty_gen
  and+ pack = pack_gen
  and+ offset = int32 in
  {Ast.ty; align; offset; pack}

let pack_size_gen = oneofl [Types.Pack8; Pack16; Pack32; Pack64]

let extension_gen = oneofl [Types.SX; ZX]

let load_op_gen =
  memop_gen num_type_gen (opt (pair pack_size_gen extension_gen))

let store_op_gen = memop_gen num_type_gen (opt pack_size_gen)

let pack_shape_gen = oneofl [Types.Pack8x8; Pack16x4; Pack32x2]

let vec_extension_gen =
  let open Types in
  oneof
    [
      map (fun (ps, e) -> ExtLane (ps, e)) (pair pack_shape_gen extension_gen);
      return ExtSplat;
      return ExtZero;
    ]

let vec_type_gen = return Types.V128Type

let vec_load_op_gen =
  memop_gen vec_type_gen (opt @@ pair pack_size_gen vec_extension_gen)

let vec_store_op_gen = memop_gen vec_type_gen unit

let vec_laneop_gen =
  let+ memop = memop_gen vec_type_gen pack_size_gen and+ n = int_range 0 255 in
  (memop, n)

(** The values [F32] and [F64] are not currently supported. *)
let value_op_gen int32_gen int64_gen =
  let open Values in
  oneof [map (fun x -> I32 x) int32_gen; map (fun x -> I64 x) int64_gen]

let num_gen =
  let+ num = value_op_gen int32 int64 in
  no_region num

let int_relop_gen =
  let open Ast.IntOp in
  oneofl [Eq; Ne; LtS; LtU; LeS; LeU; GtS; GtU; GeS; GeU]

let relop_gen = value_op_gen int_relop_gen int_relop_gen

let int_unop_gen =
  let open Ast.IntOp in
  oneof
    [
      return Clz;
      return Ctz;
      return Popcnt;
      map (fun ps -> ExtendS ps) pack_size_gen;
    ]

let int_binop_gen =
  let open Ast.IntOp in
  oneofl
    [
      Add;
      Sub;
      Mul;
      DivS;
      DivU;
      RemS;
      RemU;
      And;
      Or;
      Xor;
      Shl;
      ShrS;
      ShrU;
      Rotl;
      Rotr;
    ]

let int_cvtop =
  let open Ast.IntOp in
  oneofl
    [
      ExtendSI32;
      ExtendUI32;
      WrapI64;
      TruncSF32;
      TruncUF32;
      TruncSF64;
      TruncUF64;
      TruncSatSF32;
      TruncSatUF32;
      TruncSatSF64;
      TruncSatUF64;
      ReinterpretFloat;
    ]

let cvtop_gen = value_op_gen int_cvtop int_cvtop

let unop_gen = value_op_gen int_unop_gen int_unop_gen

let binop_gen = value_op_gen int_binop_gen int_binop_gen

let vec_gen =
  let* bits = string_size (return 16) in
  let value = V128.of_bits bits in
  return (no_region @@ Values.V128 value)

(*
  Generate instructions. The following are missing:
  - [VecTest]
  - [VecCompare]
  - [VecUnary]
  - [VecBinary]
  - [VecConvert]
  - [VecShift]
  - [VecBitmask]
  - [VecTestBits]
  - [VecUnaryBits]
  - [VecBinaryBits]
  - [VecTernaryBits]
  - [VecSplat]
  - [VecExtract]
  - [VecReplace]
  *)
let instr_gen =
  let open Ast in
  let with_var f = map f var_gen in
  let with_block_type_and_label f =
    let+ bt = block_type_gen and+ l = block_label_gen in
    f (bt, l)
  in
  let+ x =
    oneof
      [
        (* Simple *)
        return Unreachable;
        return Nop;
        return Drop;
        return Return;
        return MemorySize;
        return MemoryGrow;
        return MemoryFill;
        return MemoryCopy;
        return RefIsNull;
        (* Unary with variable payload. *)
        with_var (fun v -> Br v);
        with_var (fun var -> Br var);
        with_var (fun var -> BrIf var);
        with_var (fun var -> Call var);
        with_var (fun var -> LocalGet var);
        with_var (fun var -> LocalSet var);
        with_var (fun var -> LocalTee var);
        with_var (fun var -> GlobalGet var);
        with_var (fun var -> GlobalSet var);
        with_var (fun var -> TableGet var);
        with_var (fun var -> TableSet var);
        with_var (fun var -> TableSize var);
        with_var (fun var -> TableGrow var);
        with_var (fun var -> TableFill var);
        with_var (fun var -> ElemDrop var);
        with_var (fun var -> MemoryInit var);
        with_var (fun var -> DataDrop var);
        with_var (fun var -> RefFunc var);
        (* More complex *)
        map (fun vt -> Select vt) (opt value_types_gen);
        with_block_type_and_label (fun (bt, l) -> Block (bt, l));
        with_block_type_and_label (fun (bt, l) -> Loop (bt, l));
        map
          (fun (t, l2, l3) -> If (t, l2, l3))
          (triple block_type_gen block_label_gen block_label_gen);
        map (fun (vl, v) -> BrTable (vl, v)) (pair (small_list var_gen) var_gen);
        map (fun (v1, v2) -> CallIndirect (v1, v2)) (pair var_gen var_gen);
        map (fun o -> Load o) load_op_gen;
        map (fun o -> Store o) store_op_gen;
        map (fun o -> VecLoad o) vec_load_op_gen;
        map (fun o -> VecStore o) vec_store_op_gen;
        map (fun o -> VecLoadLane o) vec_laneop_gen;
        map (fun o -> VecStoreLane o) vec_laneop_gen;
        map (fun rt -> RefNull rt) ref_type_gen;
        map (fun n -> Const n) num_gen;
        map (fun o -> Compare o) relop_gen;
        map (fun o -> Unary o) unop_gen;
        map (fun o -> Binary o) binop_gen;
        map (fun o -> Convert o) cvtop_gen;
        map (fun o -> VecConst o) vec_gen;
      ]
  in
  no_region x

let vector_gen gen =
  let* len = int_range 0 10 in
  let* seeds = small_list int in
  return
    (Lazy_vector.LwtInt32Vector.create
       ~produce_value:(fun ix ->
         let rand =
           Random.State.make @@ Array.of_list (Int32.to_int ix :: seeds)
         in
         Lwt.return @@ generate1 ~rand gen)
       (Int32.of_int len))

let vector_z_gen gen =
  let* len = int_range 0 10 in
  let* seeds = small_list int in
  return
    (Lazy_vector.LwtZVector.create
       ~produce_value:(fun ix ->
         let rand = Random.State.make @@ Array.of_list (Z.to_int ix :: seeds) in
         Lwt.return @@ generate1 ~rand gen)
       (Z.of_int len))

let result_type_gen = vector_gen value_type_gen

let func_type_gen =
  let+ pt = result_type_gen and+ rt = result_type_gen in
  Types.FuncType (pt, rt)

let block_label_gen =
  let+ n = int32 in
  Ast.Block_label n

let func_gen =
  let* ftype = var_gen in
  let* locals = vector_gen value_type_gen in
  let* body = block_label_gen in
  return @@ no_region {Ast.ftype; locals; body}

let func_gen current_module =
  let ast_func () =
    let* func_type = func_type_gen in
    let* func = func_gen in
    return @@ Func.AstFunc (func_type, current_module, func)
  in
  oneof
    [
      delay ast_func;
      map
        (fun (ft, n) -> Func.HostFunc (ft, Printf.sprintf "host_func_%d" n))
        (pair func_type_gen small_nat);
    ]

let ref_type_gen = oneofl [Types.FuncRefType; Types.ExternRefType]

let ref_gen =
  oneof
    [
      map (fun rt -> Values.NullRef rt) ref_type_gen;
      map (fun n -> Values.ExternRef n) int32;
    ]

let table_gen =
  let* len = frequency [(10, int_range 1 10); (1, int_range 100 200)] in
  let* max = opt @@ map Int32.of_int @@ int_range 1 len in
  let limit = {Types.min = 0l; max} in
  let* ref_type = ref_type_gen in
  let ty = Types.TableType (limit, ref_type) in
  let* seeds = small_list (int_range 0 10_000) in
  let table_entries =
    Table.Vector.Vector.create
      ~produce_value:(fun ix ->
        let rand =
          Random.State.make @@ Array.of_list (Int32.to_int ix :: seeds)
        in
        Lwt.return @@ generate1 ~rand ref_gen)
      (Int32.of_int len)
  in
  return @@ Table.of_lazy_vector ty table_entries

let chunked_byte_vector_gen =
  let* bs = small_string ~gen:char in
  return @@ Chunked_byte_vector.Lwt.of_string bs

let memory_gen =
  let* len = frequency [(10, int_range 1 10); (1, int_range 100 200)] in
  let* max = opt @@ map Int32.of_int @@ int_range 1 len in
  let ty = Types.MemoryType {Types.min = 0l; max} in
  let* chunks = chunked_byte_vector_gen in
  return @@ Memory.of_chunks ty chunks

let value_num_gen nt =
  match nt with
  | Types.I32Type -> map (fun x -> Values.I32 x) int32
  | Types.I64Type -> map (fun x -> Values.I64 x) int64
  | _ -> Stdlib.failwith "Float type not supported"

let typed_value_gen ty =
  match ty with
  | Types.NumType nt -> map (fun n -> Values.Num n) (value_num_gen nt)
  | Types.RefType rt -> return @@ Values.Ref (Values.NullRef rt)
  | Types.VecType _ -> map (fun v -> Values.Vec v.Source.it) vec_gen

let value_gen =
  let* value_type = value_type_gen in
  typed_value_gen value_type

let global_gen =
  let* value_type = value_type_gen in
  let* value = typed_value_gen value_type in
  let* mt = oneofl [Types.Immutable; Types.Mutable] in
  let ty = Types.GlobalType (value_type, mt) in
  return @@ Global.alloc ty value

let extern_gen current_module =
  oneof
    [
      map (fun f -> Instance.ExternFunc f) @@ func_gen current_module;
      map (fun t -> Instance.ExternTable t) table_gen;
      map (fun m -> Instance.ExternMemory m) memory_gen;
      map (fun g -> Instance.ExternGlobal g) global_gen;
    ]

let map_gen gen =
  let* seeds = small_list int in
  return
  @@ Instance.NameMap.create
       ~produce_value:(fun key ->
         let rand = Random.State.make @@ Array.of_list (key @ seeds) in
         Lwt.return @@ generate1 ~rand gen)
       ()

let elems_gen =
  let+ v = vector_gen ref_gen in
  ref v

let datas_gen =
  let+ lbl = data_label_gen in
  ref @@ lbl

let blocks_table_gen = vector_gen (vector_gen instr_gen)

let datas_table_gen = vector_gen chunked_byte_vector_gen

let allocations_gen =
  let* blocks = blocks_table_gen in
  let+ datas = datas_table_gen in
  Ast.{blocks; datas}

let module_ref_and_instance_gen ?module_reg () =
  let module_reg =
    match module_reg with
    | None -> Instance.ModuleMap.create ()
    | Some module_reg -> module_reg
  in
  let* module_name = string_printable in
  let module_ref =
    Instance.(alloc_module_ref (Module_key module_name) module_reg)
  in
  let* types = vector_gen func_type_gen in
  let* funcs = vector_gen @@ func_gen module_ref in
  let* tables = vector_gen table_gen in
  let* memories = vector_gen memory_gen in
  let* globals = vector_gen global_gen in
  let* exports = map_gen (extern_gen module_ref) in
  let* elems = vector_gen elems_gen in
  let* datas = vector_gen datas_gen in
  let* allocations = allocations_gen in
  let module_ =
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
    }
  in
  Instance.update_module_ref module_ref module_ ;
  return (module_ref, module_)

let module_gen ?module_reg () =
  map snd (module_ref_and_instance_gen ?module_reg ())

let frame_gen ~module_reg =
  let* inst, _ = module_ref_and_instance_gen ~module_reg () in
  let+ locals = small_list (map ref value_gen) in
  Eval.{inst; locals}

let rec admin_instr'_gen ~module_reg depth =
  let open Eval in
  let from_block_gen =
    let* block = block_label_gen in
    let+ index = int32 in
    From_block (block, index)
  in
  let plain_gen =
    let+ instr = instr_gen in
    Plain instr.it
  in
  let refer_gen =
    let+ ref_ = ref_gen in
    Refer ref_
  in
  let invoke_gen =
    let* inst, _ = module_ref_and_instance_gen ~module_reg () in
    let+ func = func_gen inst in
    Invoke func
  in
  let trapping_gen =
    let+ msg = string_printable in
    Trapping msg
  in
  let returning_gen =
    let+ values = small_list value_gen in
    Returning values
  in
  let breaking_gen =
    let* index = int32 in
    let+ values = small_list value_gen in
    Breaking (index, values)
  in
  let label_gen =
    let* index = int32 in
    let* final_instrs = small_list instr_gen in
    let* values = small_list value_gen in
    let+ instrs = small_list (admin_instr_gen ~module_reg (depth - 1)) in
    Label (index, final_instrs, (values, instrs))
  in
  let frame_gen' =
    let* index = int32 in
    let* frame = frame_gen ~module_reg in
    let* values = small_list value_gen in
    let+ instrs = small_list (admin_instr_gen ~module_reg (depth - 1)) in
    Frame (index, frame, (values, instrs))
  in
  oneof
    ([
       from_block_gen;
       plain_gen;
       refer_gen;
       invoke_gen;
       trapping_gen;
       returning_gen;
       breaking_gen;
     ]
    @ if depth > 0 then [label_gen; frame_gen'] else [])

and admin_instr_gen ~module_reg depth =
  map Source.(at no_region) (admin_instr'_gen ~module_reg depth)

let admin_instr_gen ~module_reg =
  let gen = admin_instr_gen ~module_reg in
  sized_size (int_bound 3) gen

let input_buffer_gen =
  let gen_message =
    let* rtype = int32 in
    let* raw_level = int32 in
    let* message_counter = map Z.of_int small_nat in
    let+ payload = map Bytes.of_string (small_string ~gen:char) in
    Input_buffer.{rtype; raw_level; message_counter; payload}
  in
  let* messages = vector_z_gen gen_message in
  let+ num_elements = small_nat in
  {
    Input_buffer.content = Lazy_vector.Mutable.LwtZVector.of_immutable messages;
    num_elements = Z.of_int num_elements;
  }

let config_gen ~host_funcs ~module_reg =
  let* frame = frame_gen ~module_reg in
  let* input = input_buffer_gen in
  let* instrs = small_list (admin_instr_gen ~module_reg) in
  let* values = small_list value_gen in
  let+ budget = small_int in
  Eval.{frame; input; code = (values, instrs); host_funcs; budget}
