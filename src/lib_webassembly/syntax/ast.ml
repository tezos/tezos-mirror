(*
 * Throughout the implementation we use consistent naming conventions for
 * syntactic elements, associated with the types defined here and in a few
 * other places:
 *
 *   x : var
 *   v : value
 *   e : instr
 *   f : func
 *   m : module_
 *
 *   t : value_type
 *   s : func_type
 *   c : context / config
 *
 * These conventions mostly follow standard practice in language semantics.
 *)

open Types
module TzStdLib = Tezos_lwt_result_stdlib.Lwtreslib.Bare
module Vector = Lazy_vector.Int32Vector

type void = Lib.void

let pp_void _ _ = Stdlib.failwith "You are not supposed to be able to call this"

(* Operators *)

module IntOp = struct
  type unop = Clz | Ctz | Popcnt | ExtendS of pack_size [@@deriving show]

  type binop =
    | Add
    | Sub
    | Mul
    | DivS
    | DivU
    | RemS
    | RemU
    | And
    | Or
    | Xor
    | Shl
    | ShrS
    | ShrU
    | Rotl
    | Rotr
  [@@deriving show]

  type testop = Eqz [@@deriving show]

  type relop = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  [@@deriving show]

  type cvtop =
    | ExtendSI32
    | ExtendUI32
    | WrapI64
    | TruncSF32
    | TruncUF32
    | TruncSF64
    | TruncUF64
    | TruncSatSF32
    | TruncSatUF32
    | TruncSatSF64
    | TruncSatUF64
    | ReinterpretFloat
  [@@deriving show]
end

module FloatOp = struct
  type unop = Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt
  [@@deriving show]

  type binop = Add | Sub | Mul | Div | Min | Max | CopySign [@@deriving show]

  type testop = | [@@deriving show]

  type relop = Eq | Ne | Lt | Gt | Le | Ge [@@deriving show]

  type cvtop =
    | ConvertSI32
    | ConvertUI32
    | ConvertSI64
    | ConvertUI64
    | PromoteF32
    | DemoteF64
    | ReinterpretInt
  [@@deriving show]
end

module I32Op = IntOp
module I64Op = IntOp
module F32Op = FloatOp
module F64Op = FloatOp

module V128Op = struct
  type itestop = AllTrue [@@deriving show]

  type iunop = Abs | Neg | Popcnt [@@deriving show]

  type funop = Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest
  [@@deriving show]

  type ibinop =
    | Add
    | Sub
    | Mul
    | MinS
    | MinU
    | MaxS
    | MaxU
    | AvgrU
    | AddSatS
    | AddSatU
    | SubSatS
    | SubSatU
    | DotS
    | Q15MulRSatS
    | ExtMulLowS
    | ExtMulHighS
    | ExtMulLowU
    | ExtMulHighU
    | Swizzle
    | Shuffle of int list
    | NarrowS
    | NarrowU
  [@@deriving show]

  type fbinop = Add | Sub | Mul | Div | Min | Max | Pmin | Pmax
  [@@deriving show]

  type irelop = Eq | Ne | LtS | LtU | LeS | LeU | GtS | GtU | GeS | GeU
  [@@deriving show]

  type frelop = Eq | Ne | Lt | Le | Gt | Ge [@@deriving show]

  type icvtop =
    | ExtendLowS
    | ExtendLowU
    | ExtendHighS
    | ExtendHighU
    | ExtAddPairwiseS
    | ExtAddPairwiseU
    | TruncSatSF32x4
    | TruncSatUF32x4
    | TruncSatSZeroF64x2
    | TruncSatUZeroF64x2
  [@@deriving show]

  type fcvtop =
    | DemoteZeroF64x2
    | PromoteLowF32x4
    | ConvertSI32x4
    | ConvertUI32x4
  [@@deriving show]

  type ishiftop = Shl | ShrS | ShrU [@@deriving show]

  type ibitmaskop = Bitmask [@@deriving show]

  type vtestop = AnyTrue [@@deriving show]

  type vunop = Not [@@deriving show]

  type vbinop = And | Or | Xor | AndNot [@@deriving show]

  type vternop = Bitselect [@@deriving show]

  type testop = (itestop, itestop, itestop, itestop, void, void) V128.laneop
  [@@deriving show]

  type unop = (iunop, iunop, iunop, iunop, funop, funop) V128.laneop
  [@@deriving show]

  type binop = (ibinop, ibinop, ibinop, ibinop, fbinop, fbinop) V128.laneop
  [@@deriving show]

  type relop = (irelop, irelop, irelop, irelop, frelop, frelop) V128.laneop
  [@@deriving show]

  type cvtop = (icvtop, icvtop, icvtop, icvtop, fcvtop, fcvtop) V128.laneop
  [@@deriving show]

  type shiftop =
    (ishiftop, ishiftop, ishiftop, ishiftop, void, void) V128.laneop
  [@@deriving show]

  type bitmaskop =
    (ibitmaskop, ibitmaskop, ibitmaskop, ibitmaskop, void, void) V128.laneop
  [@@deriving show]

  type nsplatop = Splat [@@deriving show]

  type 'a nextractop = Extract of int * 'a [@@deriving show]

  type nreplaceop = Replace of int [@@deriving show]

  type splatop =
    (nsplatop, nsplatop, nsplatop, nsplatop, nsplatop, nsplatop) V128.laneop
  [@@deriving show]

  type extractop =
    ( extension nextractop,
      extension nextractop,
      unit nextractop,
      unit nextractop,
      unit nextractop,
      unit nextractop )
    V128.laneop
  [@@deriving show]

  type replaceop =
    ( nreplaceop,
      nreplaceop,
      nreplaceop,
      nreplaceop,
      nreplaceop,
      nreplaceop )
    V128.laneop
  [@@deriving show]
end

type testop = (I32Op.testop, I64Op.testop, F32Op.testop, F64Op.testop) Values.op
[@@deriving show]

type unop = (I32Op.unop, I64Op.unop, F32Op.unop, F64Op.unop) Values.op
[@@deriving show]

type binop = (I32Op.binop, I64Op.binop, F32Op.binop, F64Op.binop) Values.op
[@@deriving show]

type relop = (I32Op.relop, I64Op.relop, F32Op.relop, F64Op.relop) Values.op
[@@deriving show]

type cvtop = (I32Op.cvtop, I64Op.cvtop, F32Op.cvtop, F64Op.cvtop) Values.op
[@@deriving show]

type vec_testop = V128Op.testop Values.vecop [@@deriving show]

type vec_relop = V128Op.relop Values.vecop [@@deriving show]

type vec_unop = V128Op.unop Values.vecop [@@deriving show]

type vec_binop = V128Op.binop Values.vecop [@@deriving show]

type vec_cvtop = V128Op.cvtop Values.vecop [@@deriving show]

type vec_shiftop = V128Op.shiftop Values.vecop [@@deriving show]

type vec_bitmaskop = V128Op.bitmaskop Values.vecop [@@deriving show]

type vec_vtestop = V128Op.vtestop Values.vecop [@@deriving show]

type vec_vunop = V128Op.vunop Values.vecop [@@deriving show]

type vec_vbinop = V128Op.vbinop Values.vecop [@@deriving show]

type vec_vternop = V128Op.vternop Values.vecop [@@deriving show]

type vec_splatop = V128Op.splatop Values.vecop [@@deriving show]

type vec_extractop = V128Op.extractop Values.vecop [@@deriving show]

type vec_replaceop = V128Op.replaceop Values.vecop [@@deriving show]

type ('t, 'p) memop = {ty : 't; align : int; offset : int32; pack : 'p}
[@@deriving show]

type loadop = (num_type, (pack_size * extension) option) memop [@@deriving show]

type storeop = (num_type, pack_size option) memop [@@deriving show]

type vec_loadop = (vec_type, (pack_size * vec_extension) option) memop
[@@deriving show]

type vec_storeop = (vec_type, unit) memop [@@deriving show]

type vec_laneop = (vec_type, pack_size) memop * int [@@deriving show]

(* Expressions *)

type var = int32 Source.phrase [@@deriving show]

type num = Values.num Source.phrase [@@deriving show]

type vec = Values.vec Source.phrase [@@deriving show]

type name = string

type name_list = int list

type block_type = VarBlockType of var | ValBlockType of value_type option
[@@deriving show]

type block_label = Block_label of int32 [@@deriving show]

type instr' =
  | Unreachable (* trap unconditionally *)
  | Nop (* do nothing *)
  | Drop (* forget a value *)
  | Select of value_type list option (* branchless conditional *)
  | Block of block_type * block_label (* execute in sequence *)
  | Loop of block_type * block_label (* loop header *)
  | If of block_type * block_label * block_label (* conditional *)
  | Br of var (* break to n-th surrounding label *)
  | BrIf of var (* conditional break *)
  | BrTable of var list * var (* indexed break *)
  | Return (* break from function body *)
  | Call of var (* call function *)
  | CallIndirect of var * var (* call function through table *)
  | LocalGet of var (* read local variable *)
  | LocalSet of var (* write local variable *)
  | LocalTee of var (* write local variable and keep value *)
  | GlobalGet of var (* read global variable *)
  | GlobalSet of var (* write global variable *)
  | TableGet of var (* read table element *)
  | TableSet of var (* write table element *)
  | TableSize of var (* size of table *)
  | TableGrow of var (* grow table *)
  | TableFill of var (* fill table range with value *)
  | TableCopy of var * var (* copy table range *)
  | TableInit of var * var (* initialize table range from segment *)
  | ElemDrop of var (* drop passive element segment *)
  | Load of loadop (* read memory at address *)
  | Store of storeop (* write memory at address *)
  | VecLoad of vec_loadop (* read memory at address *)
  | VecStore of vec_storeop (* write memory at address *)
  | VecLoadLane of vec_laneop (* read single lane at address *)
  | VecStoreLane of vec_laneop (* write single lane to address *)
  | MemorySize (* size of memory *)
  | MemoryGrow (* grow memory *)
  | MemoryFill (* fill memory range with value *)
  | MemoryCopy (* copy memory ranges *)
  | MemoryInit of var (* initialize memory range from segment *)
  | DataDrop of var (* drop passive data segment *)
  | RefNull of ref_type (* null reference *)
  | RefFunc of var (* function reference *)
  | RefIsNull (* null test *)
  | Const of num (* constant *)
  | Test of testop (* numeric test *)
  | Compare of relop (* numeric comparison *)
  | Unary of unop (* unary numeric operator *)
  | Binary of binop (* binary numeric operator *)
  | Convert of cvtop (* conversion *)
  | VecConst of vec (* constant *)
  | VecTest of vec_testop (* vector test *)
  | VecCompare of vec_relop (* vector comparison *)
  | VecUnary of vec_unop (* unary vector operator *)
  | VecBinary of vec_binop (* binary vector operator *)
  | VecConvert of vec_cvtop (* vector conversion *)
  | VecShift of vec_shiftop (* vector shifts *)
  | VecBitmask of vec_bitmaskop (* vector masking *)
  | VecTestBits of vec_vtestop (* vector bit test *)
  | VecUnaryBits of vec_vunop (* unary bit vector operator *)
  | VecBinaryBits of vec_vbinop (* binary bit vector operator *)
  | VecTernaryBits of vec_vternop (* ternary bit vector operator *)
  | VecSplat of vec_splatop (* number to vector conversion *)
  | VecExtract of vec_extractop (* extract lane from vector *)
  | VecReplace of vec_replaceop (* replace lane in vector *)
[@@deriving show]

type instr = instr' Source.phrase [@@deriving show]

(* Globals & Functions *)

type const = block_label Source.phrase

type global = global' Source.phrase

and global' = {gtype : global_type; ginit : const}

type func = func' Source.phrase

and func' = {ftype : var; locals : value_type Vector.t; body : block_label}

(* Tables & Memories *)

type table = table' Source.phrase

and table' = {ttype : table_type}

type memory = memory' Source.phrase

and memory' = {mtype : memory_type}

type segment_mode = segment_mode' Source.phrase

and segment_mode' =
  | Passive
  | Active of {index : var; offset : const}
  | Declarative

type elem_segment = elem_segment' Source.phrase

and elem_segment' = {
  etype : ref_type;
  einit : const Vector.t;
  emode : segment_mode;
}

type data_label = Data_label of int32 [@@deriving show]

type data_segment = data_segment' Source.phrase

and data_segment' = {dinit : data_label; dmode : segment_mode}

(* Modules *)

type type_ = func_type Source.phrase

type export_desc = export_desc' Source.phrase

and export_desc' =
  | FuncExport of var
  | TableExport of var
  | MemoryExport of var
  | GlobalExport of var

type export = export' Source.phrase

and export' = {name : name; edesc : export_desc}

type import_desc = import_desc' Source.phrase

and import_desc' =
  | FuncImport of var
  | TableImport of table_type
  | MemoryImport of memory_type
  | GlobalImport of global_type

type import = import' Source.phrase

and import' = {module_name : name; item_name : name; idesc : import_desc}

type start = start' Source.phrase

and start' = {sfunc : var}

type block_table = instr Vector.t Vector.t

type datas_table = Chunked_byte_vector.t Vector.t

type allocations = {mutable blocks : block_table; mutable datas : datas_table}

type module_ = module_' Source.phrase

and module_' = {
  types : type_ Vector.t;
  globals : global Vector.t;
  tables : table Vector.t;
  memories : memory Vector.t;
  funcs : func Vector.t;
  start : start option;
  elems : elem_segment Vector.t;
  datas : data_segment Vector.t;
  imports : import Vector.t;
  exports : export Vector.t;
  allocations : allocations;
}

(* Auxiliary functions *)

let empty_allocations () =
  {
    blocks = Vector.(singleton (empty ()));
    datas = Vector.(singleton (Chunked_byte_vector.create 0L));
  }

let make_allocation_state blocks datas = {blocks; datas}

let alloc_block allocs =
  let blocks, b = Vector.(append (empty ()) allocs.blocks) in
  allocs.blocks <- blocks ;
  Block_label b

let add_to_block allocs (Block_label b) instr =
  let open Lwt.Syntax in
  let+ block = Vector.get b allocs.blocks in
  let block, _ = Vector.(append instr block) in
  allocs.blocks <- Vector.set b block allocs.blocks

let alloc_data (allocs : allocations) len =
  let datas, d =
    Vector.append (Chunked_byte_vector.allocate len) allocs.datas
  in
  allocs.datas <- datas ;
  Data_label d

let add_to_data (allocs : allocations) (Data_label d) index byte =
  let open Lwt.Syntax in
  let* data = Vector.get d allocs.datas in
  Chunked_byte_vector.store_byte data index byte

let empty_module () =
  {
    types = Vector.create 0l;
    globals = Vector.create 0l;
    tables = Vector.create 0l;
    memories = Vector.create 0l;
    funcs = Vector.create 0l;
    start = None;
    elems = Vector.create 0l;
    datas = Vector.create 0l;
    imports = Vector.create 0l;
    exports = Vector.create 0l;
    allocations = empty_allocations ();
  }

open Source

let get_data (Data_label d) datas = Vector.get d datas

let func_type_for (m : module_) (x : var) : func_type Lwt.t =
  let open Lwt.Syntax in
  let+ ty = Vector.get x.it m.it.types in
  ty.it

let import_type (m : module_) (im : import) : extern_type Lwt.t =
  let open Lwt.Syntax in
  let {idesc; _} = im.it in
  match idesc.it with
  | FuncImport x ->
      let+ ty = func_type_for m x in
      ExternFuncType ty
  | TableImport t -> Lwt.return (ExternTableType t)
  | MemoryImport t -> Lwt.return (ExternMemoryType t)
  | GlobalImport t -> Lwt.return (ExternGlobalType t)

(* This function is only used to printing types for debugging purpose, as such
   it is safe to use conversions to lists. *)
let export_type (m : module_) (ex : export) : extern_type Lwt.t =
  let open Lwt.Syntax in
  let {edesc; _} = ex.it in
  let* its =
    TzStdLib.List.filter_map_s
      (fun (_, i) -> TzStdLib.Option.map_s (import_type m) i)
      (Vector.loaded_bindings m.it.imports)
  in
  let open Lib.List32 in
  match edesc.it with
  | FuncExport x ->
      let+ fts' =
        TzStdLib.List.filter_map_s
          (fun (_, f) ->
            TzStdLib.Option.map_s (fun f -> func_type_for m f.it.ftype) f)
          (Vector.loaded_bindings m.it.funcs)
      in
      let fts = funcs its @ fts' in
      ExternFuncType (nth fts x.it)
  | TableExport x ->
      let+ tts' =
        TzStdLib.List.filter_map_s
          (fun (_, t) -> Lwt.return (Option.map (fun t -> t.it.ttype) t))
          (Vector.loaded_bindings m.it.tables)
      in
      let tts = tables its @ tts' in
      ExternTableType (nth tts x.it)
  | MemoryExport x ->
      let+ mts' =
        TzStdLib.List.filter_map_s
          (fun (_, m) -> Lwt.return (Option.map (fun m -> m.it.mtype) m))
          (Vector.loaded_bindings m.it.memories)
      in
      let mts = memories its @ mts' in
      ExternMemoryType (nth mts x.it)
  | GlobalExport x ->
      let+ gts' =
        TzStdLib.List.filter_map_s
          (fun (_, g) -> Lwt.return (Option.map (fun g -> g.it.gtype) g))
          (Vector.loaded_bindings m.it.globals)
      in
      let gts = globals its @ gts' in
      ExternGlobalType (nth gts x.it)

let string_of_name n =
  let b = Buffer.create 16 in
  let escape uc =
    let uc = Char.code uc in
    if uc < 0x20 || uc >= 0x7f then
      Buffer.add_string b (Format.sprintf "\\u{%02x}" uc)
    else
      let c = Char.chr uc in
      if c = '\"' || c = '\\' then Buffer.add_char b '\\' ;
      Buffer.add_char b c
  in
  String.iter escape n ;
  Buffer.contents b
