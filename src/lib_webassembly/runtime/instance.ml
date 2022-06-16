open Types
module Vector = Lazy_vector.LwtInt32Vector

module NameMap =
  Lazy_map.Make
    (Lazy_map.Effect.Lwt)
    (struct
      type t = Ast.name_list

      let compare = List.compare Int.compare

      let to_string = Utf8.encode_list
    end)

type module_inst = {
  types : func_type Vector.t;
  funcs : func_inst Vector.t;
  tables : table_inst Vector.t;
  memories : memory_inst Vector.t;
  globals : global_inst Vector.t;
  exports : extern NameMap.t;
  elems : elem_inst Vector.t;
  datas : data_inst Vector.t;
  blocks : Ast.instr array array;
}

and func_inst = (input_inst, module_inst ref) Func.t

and table_inst = Table.t

and memory_inst = Memory.t

and global_inst = Global.t

and input_inst = Input_buffer.t

and export_inst = Ast.name * extern

and elem_inst = Values.ref_ Vector.t ref

and data_inst = Chunked_byte_vector.Lwt.t ref

and extern =
  | ExternFunc of func_inst
  | ExternTable of table_inst
  | ExternMemory of memory_inst
  | ExternGlobal of global_inst

(* Reference types *)

type Values.ref_ += FuncRef of func_inst

let () =
  let type_of_ref' = !Values.type_of_ref' in
  Values.type_of_ref' :=
    function FuncRef _ -> FuncRefType | r -> type_of_ref' r

let () =
  let string_of_ref' = !Values.string_of_ref' in
  Values.string_of_ref' :=
    function FuncRef _ -> "func" | r -> string_of_ref' r

let () =
  let eq_ref' = !Values.eq_ref' in
  Values.eq_ref' :=
    fun r1 r2 ->
      match (r1, r2) with
      | FuncRef f1, FuncRef f2 -> f1 == f2
      | _, _ -> eq_ref' r1 r2

(* Auxiliary functions *)

let empty_module_inst =
  {
    types = Vector.create 0l;
    funcs = Vector.create 0l;
    tables = Vector.create 0l;
    memories = Vector.create 0l;
    globals = Vector.create 0l;
    exports = NameMap.create ~produce_value:(fun _ -> Lwt.fail Not_found) ();
    elems = Vector.create 0l;
    datas = Vector.create 0l;
    blocks = [||];
  }

let extern_type_of = function
  | ExternFunc func -> ExternFuncType (Func.type_of func)
  | ExternTable tab -> ExternTableType (Table.type_of tab)
  | ExternMemory mem -> ExternMemoryType (Memory.type_of mem)
  | ExternGlobal glob -> ExternGlobalType (Global.type_of glob)

let export inst name =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let+ export = NameMap.get name inst.exports in
      Some export)
    (function
      | Not_found | Lazy_map.UnexpectedAccess -> Lwt.return_none
      | exn -> Lwt.fail exn)
