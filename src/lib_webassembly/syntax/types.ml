(* Types *)

type num_type = I32Type | I64Type | F32Type | F64Type [@@deriving show]

type vec_type = V128Type [@@deriving show]

type ref_type = FuncRefType | ExternRefType [@@deriving show]

type value_type =
  | NumType of num_type
  | VecType of vec_type
  | RefType of ref_type
[@@deriving show]

type result_type = value_type Lazy_vector.Int32Vector.t

type func_type = FuncType of result_type * result_type

type 'a limits = {min : 'a; max : 'a option} [@@deriving show]

type mutability = Immutable | Mutable [@@deriving show]

type table_type = TableType of Int32.t limits * ref_type [@@deriving show]

type memory_type = MemoryType of Int32.t limits [@@deriving show]

type global_type = GlobalType of value_type * mutability [@@deriving show]

type extern_type =
  | ExternFuncType of func_type
  | ExternTableType of table_type
  | ExternMemoryType of memory_type
  | ExternGlobalType of global_type

(* Reference-interpreter-todo: these types should move somewhere else *)
type pack_size = Pack8 | Pack16 | Pack32 | Pack64 [@@deriving show]

type extension = SX | ZX [@@deriving show]

type pack_shape = Pack8x8 | Pack16x4 | Pack32x2 [@@deriving show]

type vec_extension = ExtLane of pack_shape * extension | ExtSplat | ExtZero
[@@deriving show]

(* Attributes *)

let num_size = function I32Type | F32Type -> 4 | I64Type | F64Type -> 8

let vec_size = function V128Type -> 16

let packed_size = function
  | Pack8 -> 1
  | Pack16 -> 2
  | Pack32 -> 4
  | Pack64 -> 8

let packed_shape_size = function Pack8x8 | Pack16x4 | Pack32x2 -> 8

let is_num_type = function NumType _ -> true | _ -> false

let is_vec_type = function VecType _ -> true | _ -> false

let is_ref_type = function RefType _ -> true | _ -> false

(* Filters *)

let funcs =
  Lib.List.map_filter (function ExternFuncType t -> Some t | _ -> None)

let tables =
  Lib.List.map_filter (function ExternTableType t -> Some t | _ -> None)

let memories =
  Lib.List.map_filter (function ExternMemoryType t -> Some t | _ -> None)

let globals =
  Lib.List.map_filter (function ExternGlobalType t -> Some t | _ -> None)

(* Subtyping *)

let match_limits lim1 lim2 =
  I32.ge_u lim1.min lim2.min
  &&
  match (lim1.max, lim2.max) with
  | _, None -> true
  | None, Some _ -> false
  | Some i, Some j -> I32.le_u i j

let func_type_empty (FuncType (ins, out)) =
  Lazy_vector.Int32Vector.num_elements ins = 0l
  && Lazy_vector.Int32Vector.num_elements out = 0l

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3387

   This function should probably be tickified, or the check removed from the
   evaluation. *)
let func_types_equal (FuncType (ins, out)) (FuncType (ins', out')) =
  let open Lwt.Syntax in
  let ins_len = Lazy_vector.Int32Vector.num_elements ins in
  let out_len = Lazy_vector.Int32Vector.num_elements out in
  if
    ins_len <> Lazy_vector.Int32Vector.num_elements ins'
    || out_len <> Lazy_vector.Int32Vector.num_elements out'
  then Lwt.return_false
  else
    let rec for_all acc i len vec vec' =
      if i >= len || not acc then Lwt.return acc
      else
        let* t = Lazy_vector.Int32Vector.get i vec in
        let* t' = Lazy_vector.Int32Vector.get i vec' in
        for_all (t = t') (Int32.succ i) len vec vec'
    in
    let* ins_eq = for_all true 0l ins_len ins ins' in
    let+ out_eq = for_all true 0l out_len out out' in
    ins_eq && out_eq

let match_func_type ft1 ft2 = func_types_equal ft1 ft2

let match_table_type (TableType (lim1, et1)) (TableType (lim2, et2)) =
  et1 = et2 && match_limits lim1 lim2

let match_memory_type (MemoryType lim1) (MemoryType lim2) =
  match_limits lim1 lim2

let match_global_type gt1 gt2 = gt1 = gt2

let match_extern_type et1 et2 =
  match (et1, et2) with
  | ExternFuncType ft1, ExternFuncType ft2 -> match_func_type ft1 ft2
  | ExternTableType tt1, ExternTableType tt2 ->
      Lwt.return (match_table_type tt1 tt2)
  | ExternMemoryType mt1, ExternMemoryType mt2 ->
      Lwt.return (match_memory_type mt1 mt2)
  | ExternGlobalType gt1, ExternGlobalType gt2 ->
      Lwt.return (match_global_type gt1 gt2)
  | _, _ -> Lwt.return_false

(* String conversion *)

let string_of_num_type = function
  | I32Type -> "i32"
  | I64Type -> "i64"
  | F32Type -> "f32"
  | F64Type -> "f64"

let string_of_vec_type = function V128Type -> "v128"

let string_of_ref_type = function
  | FuncRefType -> "funcref"
  | ExternRefType -> "externref"

let string_of_refed_type = function
  | FuncRefType -> "func"
  | ExternRefType -> "extern"

let string_of_value_type = function
  | NumType t -> string_of_num_type t
  | VecType t -> string_of_vec_type t
  | RefType t -> string_of_ref_type t

let string_of_value_types = function
  | [t] -> string_of_value_type t
  | ts -> "[" ^ String.concat " " (List.map string_of_value_type ts) ^ "]"

let string_of_limits {min; max} =
  I32.to_string_u min
  ^ match max with None -> "" | Some n -> " " ^ I32.to_string_u n

let string_of_memory_type = function MemoryType lim -> string_of_limits lim

let string_of_table_type = function
  | TableType (lim, t) -> string_of_limits lim ^ " " ^ string_of_ref_type t

let string_of_global_type = function
  | GlobalType (t, Immutable) -> string_of_value_type t
  | GlobalType (t, Mutable) -> "(mut " ^ string_of_value_type t ^ ")"

let string_of_result_type ts =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3378

     Ensure `string_of_*` functions are never used in the PVM, since it will be
     wrong on partial values. It can only be used during the execution of the
     testsuite. *)
  let ts = List.filter_map snd (Lazy_vector.Int32Vector.loaded_bindings ts) in
  "[" ^ String.concat " " (List.map string_of_value_type ts) ^ "]"

let string_of_func_type (FuncType (ins, out)) =
  string_of_result_type ins ^ " -> " ^ string_of_result_type out

let string_of_extern_type = function
  | ExternFuncType ft -> "func " ^ string_of_func_type ft
  | ExternTableType tt -> "table " ^ string_of_table_type tt
  | ExternMemoryType mt -> "memory " ^ string_of_memory_type mt
  | ExternGlobalType gt -> "global " ^ string_of_global_type gt
