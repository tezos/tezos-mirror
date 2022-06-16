open Source
open Ast
module Set = Set.Make (Int32)

type t = {
  types : Set.t;
  globals : Set.t;
  tables : Set.t;
  memories : Set.t;
  funcs : Set.t;
  elems : Set.t;
  datas : Set.t;
  locals : Set.t;
  labels : Set.t;
}

let empty : t =
  {
    types = Set.empty;
    globals = Set.empty;
    tables = Set.empty;
    memories = Set.empty;
    funcs = Set.empty;
    elems = Set.empty;
    datas = Set.empty;
    locals = Set.empty;
    labels = Set.empty;
  }

let union (s1 : t) (s2 : t) : t =
  {
    types = Set.union s1.types s2.types;
    globals = Set.union s1.globals s2.globals;
    tables = Set.union s1.tables s2.tables;
    memories = Set.union s1.memories s2.memories;
    funcs = Set.union s1.funcs s2.funcs;
    elems = Set.union s1.elems s2.elems;
    datas = Set.union s1.datas s2.datas;
    locals = Set.union s1.locals s2.locals;
    labels = Set.union s1.labels s2.labels;
  }

let types s = {empty with types = s}

let globals s = {empty with globals = s}

let tables s = {empty with tables = s}

let memories s = {empty with memories = s}

let funcs s = {empty with funcs = s}

let elems s = {empty with elems = s}

let datas s = {empty with datas = s}

let locals s = {empty with locals = s}

let labels s = {empty with labels = s}

let var x = Set.singleton x.it

let zero = Set.singleton 0l

let shift s = Set.map (Int32.add (-1l)) (Set.remove 0l s)

let ( ++ ) = union

let ( ++* ) x y =
  let open Lwt.Syntax in
  let* x' = x in
  let+ y' = y in
  union x' y'

let list free xs = List.fold_left union empty (List.map free xs)

let opt free xo = Lib.Option.get (Lib.Option.map free xo) empty

let lazy_vector_s free xs =
  let open Lwt.Syntax in
  let open Tezos_lwt_result_stdlib.Lwtreslib.Bare in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3378 &
     https://gitlab.com/tezos/tezos/-/issues/3387

     [Free] module is used only during the validation of the AST, no mutation
     can happen at this time, and is only used during tests. It is then safe to
     simply use the list, it doesn't need to be tickified. *)
  let* xs = Vector.to_list xs in
  List.fold_left_s
    (fun acc s ->
      let+ f = free s in
      union acc f)
    empty
    xs

let lazy_vector free xs = lazy_vector_s (fun x -> Lwt.return (free x)) xs

let block_type = function
  | VarBlockType x -> types (var x)
  | ValBlockType _ -> empty

let rec instr lookup_block (e : instr) =
  match e.it with
  | Unreachable | Nop | Drop | Select _ -> empty
  | RefNull _ | RefIsNull -> empty
  | RefFunc x -> funcs (var x)
  | Const _ | Test _ | Compare _ | Unary _ | Binary _ | Convert _ -> empty
  | Block (bt, es) | Loop (bt, es) -> block_type bt ++ block lookup_block es
  | If (bt, es1, es2) ->
      block_type bt ++ block lookup_block es1 ++ block lookup_block es2
  | Br x | BrIf x -> labels (var x)
  | BrTable (xs, x) -> list (fun x -> labels (var x)) (x :: xs)
  | Return -> empty
  | Call x -> funcs (var x)
  | CallIndirect (x, y) -> tables (var x) ++ types (var y)
  | LocalGet x | LocalSet x | LocalTee x -> locals (var x)
  | GlobalGet x | GlobalSet x -> globals (var x)
  | TableGet x | TableSet x | TableSize x | TableGrow x | TableFill x ->
      tables (var x)
  | TableCopy (x, y) -> tables (var x) ++ tables (var y)
  | TableInit (x, y) -> tables (var x) ++ elems (var y)
  | ElemDrop x -> elems (var x)
  | Load _ | Store _ | VecLoad _ | VecStore _ | VecLoadLane _ | VecStoreLane _
  | MemorySize | MemoryGrow | MemoryCopy | MemoryFill ->
      memories zero
  | VecConst _ | VecTest _ | VecUnary _ | VecBinary _ | VecCompare _
  | VecConvert _ | VecShift _ | VecBitmask _ | VecTestBits _ | VecUnaryBits _
  | VecBinaryBits _ | VecTernaryBits _ | VecSplat _ | VecExtract _
  | VecReplace _ ->
      memories zero
  | MemoryInit x -> memories zero ++ datas (var x)
  | DataDrop x -> datas (var x)

and block lookup_block (es : block_label) =
  let free = list (instr lookup_block) (lookup_block es) in
  {free with labels = shift free.labels}

let const lookup_block (c : const) = block lookup_block c.it

let global lookup_block (g : global) = const lookup_block g.it.ginit

let func lookup_block (f : func) =
  {(block lookup_block f.it.body) with locals = Set.empty}

let table (t : table) = empty

let memory (m : memory) = empty

let segment_mode lookup_block f (m : segment_mode) =
  match m.it with
  | Passive | Declarative -> empty
  | Active {index; offset} -> f (var index) ++ const lookup_block offset

let elem lookup_block (s : elem_segment) =
  lazy_vector (const lookup_block) s.it.einit
  ++* (segment_mode lookup_block tables s.it.emode |> Lwt.return)

let data lookup_block (s : data_segment) =
  segment_mode lookup_block memories s.it.dmode

let type_ (t : type_) = empty

let export_desc (d : export_desc) =
  match d.it with
  | FuncExport x -> funcs (var x)
  | TableExport x -> tables (var x)
  | MemoryExport x -> memories (var x)
  | GlobalExport x -> globals (var x)

let import_desc (d : import_desc) =
  match d.it with
  | FuncImport x -> types (var x)
  | TableImport tt -> empty
  | MemoryImport mt -> empty
  | GlobalImport gt -> empty

let export (e : export) = export_desc e.it.edesc

let import (i : import) = import_desc i.it.idesc

let start (s : start) = funcs (var s.it.sfunc)

let module_ (m : module_) =
  let lookup_block (Block_label b) = Array.to_list m.it.blocks.(b) in
  lazy_vector type_ m.it.types
  ++* lazy_vector (global lookup_block) m.it.globals
  ++* lazy_vector table m.it.tables
  ++* lazy_vector memory m.it.memories
  ++* lazy_vector (func lookup_block) m.it.funcs
  ++* Lwt.return (opt start m.it.start)
  ++* lazy_vector_s (elem lookup_block) m.it.elems
  ++* lazy_vector (data lookup_block) m.it.datas
  ++* lazy_vector import m.it.imports
  ++* lazy_vector export m.it.exports
