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

let types s = {empty with types = s} |> Lwt.return

let globals s = {empty with globals = s} |> Lwt.return

let tables s = {empty with tables = s} |> Lwt.return

let memories s = {empty with memories = s} |> Lwt.return

let funcs s = {empty with funcs = s} |> Lwt.return

let elems s = {empty with elems = s} |> Lwt.return

let datas s = {empty with datas = s} |> Lwt.return

let locals s = {empty with locals = s} |> Lwt.return

let labels s = {empty with labels = s} |> Lwt.return

let var x = Set.singleton x.it

let zero = Set.singleton 0l

let shift s = Set.map (Int32.add (-1l)) (Set.remove 0l s)

let ( ++* ) x y =
  let open Lwt.Syntax in
  let* x' = x in
  let+ y' = y in
  union x' y'

let list free xs = List.fold_left union empty (List.map free xs)

let list_s free xs =
  let open Lwt.Syntax in
  let open Tezos_lwt_result_stdlib.Lwtreslib.Bare in
  List.fold_left_s
    (fun acc s ->
      let+ f = free s in
      union acc f)
    empty
    xs

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3387

   This function is used during parsing (`MKElaborateFunc`), hence it will
   load entire vectors at once. *)
let vector_s free v =
  let open Lwt.Syntax in
  let rec fold acc n =
    if n < 0l then Lwt.return acc
    else
      let* s = Vector.get n v in
      let* f = free s in
      fold (union acc f) (Int32.pred n)
  in
  fold empty (Int32.pred (Vector.num_elements v))

let vector free v = vector_s (fun x -> Lwt.return (free x)) v

let opt free xo = match xo with None -> Lwt.return empty | Some v -> free v

let block_type = function
  | VarBlockType x -> types (var x)
  | ValBlockType _ -> empty |> Lwt.return

let rec instr blocks (e : instr) : t Lwt.t =
  let empty = Lwt.return empty in
  match e.it with
  | Unreachable | Nop | Drop | Select _ -> empty
  | RefNull _ | RefIsNull -> empty
  | RefFunc x -> funcs (var x)
  | Const _ | Test _ | Compare _ | Unary _ | Binary _ | Convert _ -> empty
  | Block (bt, es) | Loop (bt, es) -> block_type bt ++* block blocks es
  | If (bt, es1, es2) -> block_type bt ++* block blocks es1 ++* block blocks es2
  | Br x | BrIf x -> labels (var x)
  | BrTable (xs, x) -> list_s (fun x -> labels (var x)) (x :: xs)
  | Return -> empty
  | Call x -> funcs (var x)
  | CallIndirect (x, y) -> tables (var x) ++* types (var y)
  | LocalGet x | LocalSet x | LocalTee x -> locals (var x)
  | GlobalGet x | GlobalSet x -> globals (var x)
  | TableGet x | TableSet x | TableSize x | TableGrow x | TableFill x ->
      tables (var x)
  | TableCopy (x, y) -> tables (var x) ++* tables (var y)
  | TableInit (x, y) -> tables (var x) ++* elems (var y)
  | ElemDrop x -> elems (var x)
  | Load _ | Store _ | VecLoad _ | VecStore _ | VecLoadLane _ | VecStoreLane _
  | MemorySize | MemoryGrow | MemoryCopy | MemoryFill ->
      memories zero
  | VecConst _ | VecTest _ | VecUnary _ | VecBinary _ | VecCompare _
  | VecConvert _ | VecShift _ | VecBitmask _ | VecTestBits _ | VecUnaryBits _
  | VecBinaryBits _ | VecTernaryBits _ | VecSplat _ | VecExtract _
  | VecReplace _ ->
      memories zero
  | MemoryInit x -> memories zero ++* datas (var x)
  | DataDrop x -> datas (var x)

and block blocks (Block_label es : block_label) =
  let open Lwt.Syntax in
  let* bl = Vector.get es blocks in
  let+ free = vector_s (instr blocks) bl in
  {free with labels = shift free.labels}

let const blocks (c : const) = block blocks c.it

let global blocks (g : global) = const blocks g.it.ginit

let func blocks (f : func) =
  let open Lwt.Syntax in
  let+ body = block blocks f.it.body in
  {body with locals = Set.empty}

let table (_ : table) = empty

let memory (_ : memory) = empty

let segment_mode blocks f (m : segment_mode) =
  match m.it with
  | Passive | Declarative -> empty |> Lwt.return
  | Active {index; offset} -> f (var index) ++* const blocks offset

let elem blocks (s : elem_segment) =
  vector_s (const blocks) s.it.einit ++* segment_mode blocks tables s.it.emode

let data blocks _data (s : data_segment) =
  segment_mode blocks memories s.it.dmode

let type_ (_ : type_) = empty

let export_desc (d : export_desc) =
  match d.it with
  | FuncExport x -> funcs (var x)
  | TableExport x -> tables (var x)
  | MemoryExport x -> memories (var x)
  | GlobalExport x -> globals (var x)

let import_desc (d : import_desc) =
  match d.it with
  | FuncImport x -> types (var x)
  | TableImport _ | MemoryImport _ | GlobalImport _ -> empty |> Lwt.return

let export (e : export) = export_desc e.it.edesc

let import (i : import) = import_desc i.it.idesc

let start (s : start) = funcs (var s.it.sfunc)

let module_ (m : module_) =
  vector type_ m.it.types
  ++* vector_s (global m.it.allocations.blocks) m.it.globals
  ++* vector table m.it.tables
  ++* vector memory m.it.memories
  ++* vector_s (func m.it.allocations.blocks) m.it.funcs
  ++* opt start m.it.start
  ++* vector_s (elem m.it.allocations.blocks) m.it.elems
  ++* vector_s (data m.it.allocations.blocks m.it.allocations.datas) m.it.datas
  ++* vector_s import m.it.imports
  ++* vector_s export m.it.exports
