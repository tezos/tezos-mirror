(*
 * Simple collection of functions useful for writing test cases.
 *)

open Types
open Values
open Instance

let global (GlobalType (t, _) as gt) =
  let v =
    match t with
    | NumType I32Type -> Num (I32 666l)
    | NumType I64Type -> Num (I64 666L)
    | NumType F32Type -> Num (F32 (F32.of_float 666.6))
    | NumType F64Type -> Num (F64 (F64.of_float 666.6))
    | VecType V128Type ->
        Vec (V128 (V128.I32x4.of_lanes [666l; 666l; 666l; 666l]))
    | RefType t -> Ref (NullRef t)
  in
  Global.alloc gt v

let table =
  Table.alloc
    (TableType ({min = 10l; max = Some 20l}, FuncRefType))
    (NullRef FuncRefType)

let memory = Memory.alloc (MemoryType {min = 1l; max = Some 2l})

let func module_name func_name f t =
  Host_funcs.register ~module_name ~func_name (f t) ;
  Func.alloc_host ~module_name ~func_name t

let print_value v =
  Printf.printf
    "%s : %s\n"
    (Values.string_of_value v)
    (Types.string_of_value_type (Values.type_of_value v))

let print (FuncType (_, out)) _m _v vs =
  List.iter print_value vs ;
  flush_all () ;
  List.map
    (fun (_, t) -> default_value t)
    (Lazy_vector.LwtInt32Vector.loaded_bindings out)
  |> Lwt.return

let lookup module_name name t =
  let open Lwt.Syntax in
  let+ name = Utf8.encode name in
  let empty () = Lazy_vector.LwtInt32Vector.create 0l in
  let singleton i = Lazy_vector.LwtInt32Vector.(create 1l |> set 0l i) in
  let two i j =
    Lazy_vector.LwtInt32Vector.(create 2l |> set 0l i |> set 1l j)
  in
  let func = func module_name name in
  match (name, t) with
  | "print", _ -> ExternFunc (func print (FuncType (empty (), empty ())))
  | "print_i32", _ ->
      ExternFunc (func print (FuncType (singleton (NumType I32Type), empty ())))
  | "print_i64", _ ->
      ExternFunc (func print (FuncType (singleton (NumType I64Type), empty ())))
  | "print_f32", _ ->
      ExternFunc (func print (FuncType (singleton (NumType F32Type), empty ())))
  | "print_f64", _ ->
      ExternFunc (func print (FuncType (singleton (NumType F64Type), empty ())))
  | "print_i32_f32", _ ->
      ExternFunc
        (func
           print
           (FuncType (two (NumType I32Type) (NumType F32Type), empty ())))
  | "print_f64_f64", _ ->
      ExternFunc
        (func
           print
           (FuncType (two (NumType F64Type) (NumType F64Type), empty ())))
  | "global_i32", _ ->
      ExternGlobal (global (GlobalType (NumType I32Type, Immutable)))
  | "global_i64", _ ->
      ExternGlobal (global (GlobalType (NumType I64Type, Immutable)))
  | "global_f32", _ ->
      ExternGlobal (global (GlobalType (NumType F32Type, Immutable)))
  | "global_f64", _ ->
      ExternGlobal (global (GlobalType (NumType F64Type, Immutable)))
  | "table", _ -> ExternTable table
  | "memory", _ -> ExternMemory memory
  | _ -> raise Not_found
