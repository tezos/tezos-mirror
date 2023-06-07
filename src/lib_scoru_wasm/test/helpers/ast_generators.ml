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
open Tezos_lazy_containers
open QCheck2.Gen
module Vector = Tezos_lazy_containers.Lazy_vector.Int32Vector

let small_vector_gen gen = Vector.of_list <$> list_size (int_range 0 10) gen

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
    (Lazy_vector.Int32Vector.create
       ~produce_value:(fun ix ->
         let rand =
           Random.State.make @@ Array.of_list (Int32.to_int ix :: seeds)
         in
         Lwt.return @@ generate1 ~rand gen)
       (Int32.of_int len))

let vector_z_gen gen =
  let* len = int_range 0 10 in
  let* seeds = list_size (int_range 0 10) int in
  return
    (Lazy_vector.ZVector.create
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

let ast_func_gen =
  let* ftype = var_gen in
  let* locals = vector_gen value_type_gen in
  let* body = block_label_gen in
  return @@ no_region {Ast.ftype; locals; body}

let func_gen current_module =
  let ast_func () =
    let* func_type = func_type_gen in
    let* func = ast_func_gen in
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
  return @@ Chunked_byte_vector.of_string bs

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
         let seeds =
           String.fold_left (fun seeds c -> Char.code c :: seeds) seeds key
         in
         let rand = Random.State.make @@ Array.of_list seeds in
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

let limit_gen gen =
  let* min = gen in
  let* max = opt gen in
  return {Types.min; Types.max}

let table_type_gen =
  let* limit = limit_gen int32 in
  let* ref_type = ref_type_gen in
  return @@ Types.TableType (limit, ref_type)

let memory_type_gen =
  let+ limit = limit_gen int32 in
  Types.MemoryType limit

let global_type_gen =
  let* vt = value_type_gen in
  let* mt = oneofl [Types.Immutable; Types.Mutable] in
  return @@ Types.GlobalType (vt, mt)

let import_desc_gen =
  let+ idesc =
    oneof
      [
        map (fun v -> Ast.FuncImport v) var_gen;
        map (fun tt -> Ast.TableImport tt) table_type_gen;
        map (fun mt -> Ast.MemoryImport mt) memory_type_gen;
        map (fun gt -> Ast.GlobalImport gt) global_type_gen;
      ]
  in
  no_region idesc

let export_desc_gen =
  let+ edesc =
    oneof
      [
        map (fun v -> Ast.FuncExport v) var_gen;
        map (fun v -> Ast.TableExport v) var_gen;
        map (fun v -> Ast.MemoryExport v) var_gen;
        map (fun v -> Ast.GlobalExport v) var_gen;
      ]
  in
  no_region edesc

let const_gen = map no_region block_label_gen

let segment_mode_gen =
  let passive = return Ast.Passive in
  let active =
    let* index = var_gen in
    let+ offset = block_label_gen in
    Ast.Active {index; offset = no_region offset}
  in
  let declarative = return Ast.Declarative in
  let+ mode = oneof [passive; active; declarative] in
  no_region mode

let start_gen =
  let+ sfunc = var_gen in
  no_region Ast.{sfunc}

let module_key_gen =
  let+ module_name = string_printable in
  Instance.Module_key module_name

let module_key_and_instance_gen ?module_reg () =
  let module_reg =
    match module_reg with
    | None -> Instance.ModuleMap.create ()
    | Some module_reg -> module_reg
  in
  let* module_key = module_key_gen in
  let* types = vector_gen func_type_gen in
  let* funcs = vector_gen @@ func_gen module_key in
  let* tables = vector_gen table_gen in
  let* memories = vector_gen memory_gen in
  let* globals = vector_gen global_gen in
  let* exports = map_gen (extern_gen module_key) in
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
  Instance.update_module_ref module_reg module_key module_ ;
  return (module_key, module_)

let module_gen ?module_reg () =
  map snd (module_key_and_instance_gen ?module_reg ())

let frame_gen ~module_reg:_ =
  let* inst = module_key_gen in
  let+ locals = small_vector_gen value_gen in
  Eval.{inst; locals}

let rec admin_instr'_gen ~module_reg:_ =
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
    let* inst = module_key_gen in
    let+ func = func_gen inst in
    Invoke func
  in
  let trapping_gen =
    let+ msg = string_printable in
    Trapping msg
  in
  let returning_gen =
    let+ values = small_vector_gen value_gen in
    Returning values
  in
  let breaking_gen =
    let* index = int32 in
    let+ values = small_vector_gen value_gen in
    Breaking (index, values)
  in
  let table_init_meta_gen =
    let* idx = int32 in
    let* value = ref_gen in
    let* d = int32 in
    let* s = int32 in
    let* n = int32 in
    let* x = var_gen in
    let+ y = var_gen in
    Table_init_meta (idx, value, d, s, n, x, y)
  in
  let table_fill_meta_gen =
    let* idx = int32 in
    let* i = int32 in
    let* n = int32 in
    let* r = ref_gen in
    let+ x = var_gen in
    Table_fill_meta (idx, i, n, r, x)
  in
  let table_copy_meta_gen =
    let* idx = int32 in
    let* d = int32 in
    let* s = int32 in
    let* n = int32 in
    let* x = var_gen in
    let* y = var_gen in
    let+ case = bool in
    Table_copy_meta (idx, d, s, n, x, y, case)
  in
  let memory_init_meta_gen =
    let* idx = int32 in
    let* d = int32 in
    let* b = int32 in
    let* n = int32 in
    let* s = int32 in
    let+ x = var_gen in
    Memory_init_meta (idx, d, b, n, s, x)
  in
  let memory_fill_meta_gen =
    let* idx = int32 in
    let* i = int32 in
    let* k = num_gen in
    let+ n = int32 in
    Memory_fill_meta (idx, i, k.it, n)
  in
  let memory_copy_meta_instr =
    let* idx = int32 in
    let* d = int32 in
    let* s = int32 in
    let* n = int32 in
    let+ case = bool in
    Memory_copy_meta (idx, d, s, n, case)
  in
  oneof
    [
      from_block_gen;
      plain_gen;
      refer_gen;
      invoke_gen;
      trapping_gen;
      returning_gen;
      breaking_gen;
      table_init_meta_gen;
      table_fill_meta_gen;
      table_copy_meta_gen;
      memory_init_meta_gen;
      memory_fill_meta_gen;
      memory_copy_meta_instr;
    ]

and admin_instr_gen ~module_reg =
  map Source.(at no_region) (admin_instr'_gen ~module_reg)

let input_buffer_gen =
  let gen_message =
    let* raw_level = int32 in
    let* message_counter = map Z.of_int (int_range 0 10) in
    let+ payload = map Bytes.of_string (small_string ~gen:char) in
    Input_buffer.{raw_level; message_counter; payload}
  in
  let+ messages = vector_z_gen gen_message in
  Lazy_vector.Mutable.ZVector.of_immutable messages

let output_info_gen =
  let* level = small_int in
  let outbox_level = Int32.of_int level in
  let* message_index = map Z.of_int small_nat in
  return Output_buffer.{outbox_level; message_index}

let output_buffer_gen =
  let* l = list_size (int_range 0 10) int in
  let outboxes =
    List.fold_left
      (fun outboxes i ->
        let messages =
          generate1
          @@ map
               (fun a ->
                 Output_buffer.Messages.(of_immutable @@ Vector.of_list a))
               (list (map Bytes.of_string string))
        in
        Output_buffer.Outboxes.Map.Map.add (Int32.of_int i) messages outboxes)
      Output_buffer.Outboxes.Map.Map.empty
      l
  in
  let* last_level = option (map Int32.of_int (int_range 0 10)) in
  let* validity_period = map Int32.of_int (int_range 0 10) in
  let* message_limit = map Z.of_int small_int in
  return
    Output_buffer.
      {
        outboxes = Outboxes.(create ~values:outboxes ());
        last_level;
        validity_period;
        message_limit;
      }

let label_gen ~module_reg =
  let* label_arity = option (Int32.of_int <$> small_nat) in
  let* label_break = option instr_gen in
  let* es = small_vector_gen (admin_instr_gen ~module_reg) in
  let+ vs = small_vector_gen value_gen in
  Eval.{label_arity; label_break; label_code = (vs, es)}

let label_stack_gen ~module_reg =
  let* label = label_gen ~module_reg in
  let+ stack = small_vector_gen (label_gen ~module_reg) in
  Eval.Label_stack (label, stack)

let label_result_gen =
  let+ values = small_vector_gen value_gen in
  Eval.Label_result values

let label_trapped_gen =
  let+ msg = small_string ~gen:char in
  Eval.Label_trapped (no_region @@ msg)

type packed_label_kont = Packed_lk : 'a Eval.label_kont -> packed_label_kont

type packed_frame_stack =
  | Packed_fs : 'a Eval.frame_stack -> packed_frame_stack

let packed_label_kont_gen ~module_reg =
  let pack x = Packed_lk x in
  oneof
    [
      pack <$> label_stack_gen ~module_reg;
      pack <$> label_result_gen;
      pack <$> label_trapped_gen;
    ]

let ongoing_frame_stack_gen ~module_reg =
  let* frame_arity = option (Int32.of_int <$> small_nat) in
  let* frame_specs = frame_gen ~module_reg in
  let+ frame_label_kont = label_stack_gen ~module_reg in
  Eval.{frame_arity; frame_specs; frame_label_kont}

let packed_frame_stack_gen ~module_reg =
  let* frame_arity = option (Int32.of_int <$> small_nat) in
  let* frame_specs = frame_gen ~module_reg in
  let+ (Packed_lk frame_label_kont) = packed_label_kont_gen ~module_reg in
  Packed_fs {frame_arity; frame_specs; frame_label_kont}

let map_kont_gen gen gen' =
  let* origin = small_vector_gen gen in
  let* destination = small_vector_gen gen' in
  let+ offset = Int32.of_int <$> small_nat in
  Eval.{origin; destination; offset}

let concat_kont_gen gen =
  let* lv = small_vector_gen gen in
  let* rv = small_vector_gen gen in
  let* res = small_vector_gen gen in
  let+ offset = Int32.of_int <$> small_nat in
  Eval.{lv; rv; res; offset}

let inv_start_gen ~module_reg =
  let* module_name = string_printable in
  let module_key = Instance.Module_key module_name in
  let* func = func_gen module_key in
  let* es = small_vector_gen (admin_instr_gen ~module_reg) in
  let+ vs = small_vector_gen value_gen in
  Eval.Inv_start {func; code = (vs, es)}

let inv_prepare_locals_gen ~module_reg =
  let* arity = Int32.of_int <$> small_nat in
  let* args = small_vector_gen value_gen in
  let* vs = small_vector_gen value_gen in
  let* instructions = small_vector_gen (admin_instr_gen ~module_reg) in
  let* module_name = string_printable in
  let inst = Instance.Module_key module_name in
  let* func = ast_func_gen in
  let+ locals_kont = map_kont_gen value_type_gen value_gen in
  Eval.Inv_prepare_locals
    {arity; args; vs; instructions; inst; func; locals_kont}

let inv_prepare_args_gen ~module_reg =
  let* arity = Int32.of_int <$> small_nat in
  let* vs = small_vector_gen value_gen in
  let* instructions = small_vector_gen (admin_instr_gen ~module_reg) in
  let* module_name = string_printable in
  let inst = Instance.Module_key module_name in
  let* func = ast_func_gen in
  let* locals = small_vector_gen value_gen in
  let+ args_kont = map_kont_gen value_gen value_gen in
  Eval.Inv_prepare_args {arity; vs; instructions; inst; func; locals; args_kont}

let inv_concat_gen ~module_reg =
  let* arity = Int32.of_int <$> small_nat in
  let* vs = small_vector_gen value_gen in
  let* instructions = small_vector_gen (admin_instr_gen ~module_reg) in
  let* module_name = string_printable in
  let inst = Instance.Module_key module_name in
  let* func = ast_func_gen in
  let+ concat_kont = concat_kont_gen value_gen in
  Eval.Inv_concat {arity; vs; instructions; inst; func; concat_kont}

let inv_reveal_tick ~module_reg =
  let* hash = string_small in
  let* base_destination = Int32.of_int <$> small_nat in
  let* max_bytes = Int32.of_int <$> small_nat in
  let* vs = small_vector_gen value_gen in
  let+ es = small_vector_gen (admin_instr_gen ~module_reg) in
  Eval.Inv_reveal_tick
    {
      reveal = Reveal_raw_data hash;
      base_destination;
      max_bytes;
      code = (vs, es);
    }

let inv_stop_gen ~module_reg =
  let* vs = small_vector_gen value_gen in
  let* es = small_vector_gen (admin_instr_gen ~module_reg) in
  let* fresh_frame = option (ongoing_frame_stack_gen ~module_reg) in
  let+ remaining_ticks = map Z.of_int small_nat in
  Eval.Inv_stop {code = (vs, es); fresh_frame; remaining_ticks}

let invoke_step_gen ~module_reg =
  oneof
    [
      inv_start_gen ~module_reg;
      inv_prepare_locals_gen ~module_reg;
      inv_prepare_args_gen ~module_reg;
      inv_concat_gen ~module_reg;
      inv_reveal_tick ~module_reg;
      inv_stop_gen ~module_reg;
    ]

let ls_start_gen ~module_reg =
  let+ label = label_stack_gen ~module_reg in
  Eval.LS_Start label

let ls_craft_frame_gen ~module_reg =
  let* kont = label_stack_gen ~module_reg in
  let+ invoke_step = invoke_step_gen ~module_reg in
  Eval.LS_Craft_frame (kont, invoke_step)

let ls_push_frame_gen ~module_reg =
  let* kont = label_stack_gen ~module_reg in
  let+ frame = ongoing_frame_stack_gen ~module_reg in
  Eval.LS_Push_frame (kont, frame)

let ls_consolidate_top_gen ~module_reg =
  let* label = label_gen ~module_reg in
  let* kont = concat_kont_gen value_gen in
  let* es = small_vector_gen (admin_instr_gen ~module_reg) in
  let+ stack = small_vector_gen (label_gen ~module_reg) in
  Eval.LS_Consolidate_top (label, kont, es, stack)

let ls_modify_top_gen ~module_reg =
  let+ (Packed_lk kont) = packed_label_kont_gen ~module_reg in
  Eval.LS_Modify_top kont

let label_step_kont_gen ~module_reg =
  oneof
    [
      ls_start_gen ~module_reg;
      ls_craft_frame_gen ~module_reg;
      ls_push_frame_gen ~module_reg;
      ls_consolidate_top_gen ~module_reg;
      ls_modify_top_gen ~module_reg;
    ]

let sk_start_gen ~module_reg =
  let* (Packed_fs top) = packed_frame_stack_gen ~module_reg in
  let+ stack = small_vector_gen (ongoing_frame_stack_gen ~module_reg) in
  Eval.SK_Start (top, stack)

let sk_next_gen ~module_reg =
  let* (Packed_fs top) = packed_frame_stack_gen ~module_reg in
  let* stack = small_vector_gen (ongoing_frame_stack_gen ~module_reg) in
  let+ label_kont = label_step_kont_gen ~module_reg in
  Eval.SK_Next (top, stack, label_kont)

let sk_consolidate_label_result_gen ~module_reg =
  let* frame = ongoing_frame_stack_gen ~module_reg in
  let* stack = small_vector_gen (ongoing_frame_stack_gen ~module_reg) in
  let* label = label_gen ~module_reg in
  let* kont = concat_kont_gen value_gen in
  let* es = small_vector_gen (admin_instr_gen ~module_reg) in
  let+ lstack = small_vector_gen (label_gen ~module_reg) in
  Eval.SK_Consolidate_label_result (frame, stack, label, kont, es, lstack)

let sk_result_gen =
  let+ values = small_vector_gen value_gen in
  Eval.SK_Result values

let sk_trapped_gen =
  let+ msg = small_string ~gen:char in
  Eval.SK_Trapped (no_region @@ msg)

let step_kont_gen ~module_reg =
  oneof
    [
      sk_start_gen ~module_reg;
      sk_next_gen ~module_reg;
      sk_result_gen;
      sk_trapped_gen;
    ]

let buffers_gen =
  let* input = input_buffer_gen in
  let _input_list =
    Lwt_main.run @@ Lazy_vector.ZVector.to_list
    @@ Lazy_vector.Mutable.ZVector.snapshot input
  in
  let+ output = output_buffer_gen in
  Eval.{input; output}

let config_gen ~module_reg =
  let* stack_size_limit = small_int in
  let+ step_kont = step_kont_gen ~module_reg in
  Eval.{step_kont; stack_size_limit}
