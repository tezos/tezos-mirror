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

let print_value v =
  Format.printf
    "%s : %s\n"
    (Values.string_of_value v)
    (Types.string_of_value_type (Values.type_of_value v))

let print =
  Host_funcs.Host_func
    (fun _i _o d _m vs ->
      List.iter print_value vs ;
      flush_all () ;
      Lwt.return (d, [], Z.zero))

let register_host_funcs registry =
  Host_funcs.register ~global_name:"spectest_print" print registry ;
  Host_funcs.register ~global_name:"spectest_print_i32" print registry ;
  Host_funcs.register ~global_name:"spectest_print_i64" print registry ;
  Host_funcs.register ~global_name:"spectest_print_f32" print registry ;
  Host_funcs.register ~global_name:"spectest_print_f64" print registry ;
  Host_funcs.register ~global_name:"spectest_print_i32_f32" print registry ;
  Host_funcs.register ~global_name:"spectest_print_f64_f64" print registry

let lookup name =
  let empty () = Lazy_vector.Int32Vector.create 0l in
  let singleton i = Lazy_vector.Int32Vector.(create 1l |> set 0l i) in
  let two i j = Lazy_vector.Int32Vector.(create 2l |> set 0l i |> set 1l j) in
  match name with
  | "print" ->
      ExternFunc
        (Func.alloc_host
           ~global_name:"spectest_print"
           (FuncType (empty (), empty ())))
  | "print_i32" ->
      ExternFunc
        (Func.alloc_host
           ~global_name:"spectest_print_i32"
           (FuncType (singleton (NumType I32Type), empty ())))
  | "print_i64" ->
      ExternFunc
        (Func.alloc_host
           ~global_name:"spectest_print_i64"
           (FuncType (singleton (NumType I64Type), empty ())))
  | "print_f32" ->
      ExternFunc
        (Func.alloc_host
           ~global_name:"spectest_print_f32"
           (FuncType (singleton (NumType F32Type), empty ())))
  | "print_f64" ->
      ExternFunc
        (Func.alloc_host
           ~global_name:"spectest_print_f64"
           (FuncType (singleton (NumType F64Type), empty ())))
  | "print_i32_f32" ->
      ExternFunc
        (Func.alloc_host
           ~global_name:"spectest_print_i32_f32"
           (FuncType (two (NumType I32Type) (NumType F32Type), empty ())))
  | "print_f64_f64" ->
      ExternFunc
        (Func.alloc_host
           ~global_name:"spectest_print_f64_f64"
           (FuncType (two (NumType F64Type) (NumType F64Type), empty ())))
  | "global_i32" ->
      ExternGlobal (global (GlobalType (NumType I32Type, Immutable)))
  | "global_i64" ->
      ExternGlobal (global (GlobalType (NumType I64Type, Immutable)))
  | "global_f32" ->
      ExternGlobal (global (GlobalType (NumType F32Type, Immutable)))
  | "global_f64" ->
      ExternGlobal (global (GlobalType (NumType F64Type, Immutable)))
  | "table" -> ExternTable table
  | "memory" -> ExternMemory memory
  | _ -> raise Not_found
