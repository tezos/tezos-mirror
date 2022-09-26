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
open Instance

exception Bad_input

exception Key_too_large of int

let retrieve_memory memories =
  let crash_with msg = raise (Eval.Crash (Source.no_region, msg)) in
  match memories with
  | Host_funcs.No_memories_during_init ->
      crash_with "host functions must not access memory during initialisation"
  | Host_funcs.Available_memories memories
    when Vector.num_elements memories = 1l ->
      Vector.get 0l memories
  | Host_funcs.Available_memories _ ->
      crash_with "caller module must have exactly 1 memory instance"

let aux_write_input_in_memory ~input_buffer ~output_buffer ~memory ~rtype_offset
    ~level_offset ~id_offset ~dst ~max_bytes =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* {rtype; raw_level; message_counter; payload} =
        Input_buffer.dequeue input_buffer
      in
      Output_buffer.set_level output_buffer raw_level ;
      let input_size = Bytes.length payload in
      if Int64.of_int input_size > 4096L then
        raise (Eval.Crash (Source.no_region, "input too large"))
      else
        let payload =
          Bytes.sub payload 0 @@ min input_size (Int32.to_int max_bytes)
        in
        let _ = Memory.store_bytes memory dst (Bytes.to_string payload) in
        let _ = Memory.store_num memory rtype_offset 0l (I32 rtype) in
        let _ = Memory.store_num memory level_offset 0l (I32 raw_level) in
        let _ =
          Memory.store_num
            memory
            id_offset
            0l
            (I64 (Z.to_int64 message_counter))
        in
        Lwt.return input_size)
    (fun exn ->
      match exn with
      | Input_buffer.Dequeue_from_empty_queue -> Lwt.return 0
      | _ -> raise exn)

let aux_write_output ~input_buffer:_ ~output_buffer ~memory ~src ~num_bytes =
  let open Lwt.Syntax in
  if num_bytes > 4096l then Lwt.return 1l
  else
    let num_bytes = Int32.to_int num_bytes in
    let* payload = Memory.load_bytes memory src num_bytes in
    let* () = Output_buffer.set_value output_buffer (Bytes.of_string payload) in
    Lwt.return 0l

let read_input_type =
  let input_types =
    Types.
      [
        NumType I32Type;
        NumType I32Type;
        NumType I32Type;
        NumType I32Type;
        NumType I32Type;
      ]
    |> Vector.of_list
  in
  let output_types = Types.[NumType I32Type] |> Vector.of_list in
  Types.FuncType (input_types, output_types)

let read_input_name = "tezos_read_input"

let read_input =
  Host_funcs.Host_func
    (fun input_buffer output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | [
       Values.(Num (I32 rtype_offset));
       Values.(Num (I32 level_offset));
       Values.(Num (I32 id_offset));
       Values.(Num (I32 dst));
       Values.(Num (I32 max_bytes));
      ] ->
          let* memory = retrieve_memory memories in
          let* x =
            aux_write_input_in_memory
              ~input_buffer
              ~output_buffer
              ~memory
              ~rtype_offset
              ~level_offset
              ~id_offset
              ~dst
              ~max_bytes
          in
          Lwt.return (durable, [Values.(Num (I32 (I32.of_int_s x)))])
      | _ -> raise Bad_input)

let write_output_name = "tezos_write_output"

let write_output_type =
  let input_types =
    Types.[NumType I32Type; NumType I32Type] |> Vector.of_list
  in
  let output_types = Types.[NumType I32Type] |> Vector.of_list in
  Types.FuncType (input_types, output_types)

let write_output =
  Host_funcs.Host_func
    (fun input_buffer output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | [Values.(Num (I32 src)); Values.(Num (I32 num_bytes))] ->
          let* memory = retrieve_memory memories in
          let* x =
            aux_write_output
              ~input_buffer
              ~output_buffer
              ~memory
              ~src
              ~num_bytes
          in
          Lwt.return (durable, [Values.(Num (I32 x))])
      | _ -> raise Bad_input)

let write_debug_name = "tezos_write_debug"

let write_debug_type =
  let input_types =
    Types.[NumType I32Type; NumType I32Type] |> Vector.of_list
  in
  let output_types = Vector.empty () in
  Types.FuncType (input_types, output_types)

(* [write_debug] accepts a pointer to the start of a sequence of
   bytes, and a length.

   The PVM, however, does not check that these are valid: from its
   point of view, [write_debug] is a no-op. *)
let write_debug =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable _memories inputs ->
      match inputs with
      | [Values.(Num (I32 _src)); Values.(Num (I32 _num_bytes))] ->
          Lwt.return (durable, [])
      | _ -> raise Bad_input)

(* [alternate_write_debug] may be used to replace the default
   [write_debug] implementation when debugging the PVM/kernel, and
   prints the debug message.

   NB this does slightly change the semantics of 'write_debug', as it may
   load the memory pointed to by [src] & [num_bytes].
*)
let _alternate_write_debug =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      match inputs with
      | [Values.(Num (I32 src)); Values.(Num (I32 num_bytes))] ->
          let open Lwt_syntax in
          let* memory = retrieve_memory memories in
          let mem_size = I32.of_int_s @@ I64.to_int_s @@ Memory.bound memory in
          let* s =
            if src < mem_size then
              let truncated =
                Int32.(src |> sub (min (add src num_bytes) mem_size) |> to_int)
              in
              Memory.load_bytes memory src truncated
            else
              Lwt.return
              @@ Printf.sprintf
                   "DEBUG FAILED: address %s out of bounds (maximum %s)\n"
                   (Int32.to_string src)
                   (Int32.to_string mem_size)
          in
          Printf.printf "DEBUG: %s\n" s ;
          Lwt.return (durable, [])
      | _ -> raise Bad_input)

let store_has_name = "tezos_store_has"

let store_has_type =
  let input_types =
    Types.[NumType I32Type; NumType I32Type] |> Vector.of_list
  in
  let output_types = Types.[NumType I32Type] |> Vector.of_list in
  Types.FuncType (input_types, output_types)

let store_has_unknown_key = 0

let store_has_value_only = 1

let store_has_subtrees_only = 2

let store_has_value_and_subtrees = 3

let store_has =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | [Values.(Num (I32 key_offset)); Values.(Num (I32 key_length))] ->
          let key_length = Int32.to_int key_length in
          if key_length > Durable.max_key_length then
            raise (Key_too_large key_length) ;
          let* memory = retrieve_memory memories in
          let* key = Memory.load_bytes memory key_offset key_length in
          let tree = Durable.of_storage_exn durable in
          let key = Durable.key_of_string_exn key in
          let* value_opt = Durable.find_value tree key in
          let+ num_subtrees = Durable.count_subtrees tree key in
          let r =
            match (value_opt, num_subtrees) with
            | None, 0 -> store_has_unknown_key
            | Some _, 0 -> store_has_value_only
            | None, _ -> store_has_subtrees_only
            | _ -> store_has_value_and_subtrees
          in
          (durable, [Values.(Num (I32 (I32.of_int_s r)))])
      | _ -> raise Bad_input)

let store_delete_name = "tezos_store_delete"

let store_delete_type =
  let input_types =
    Types.[NumType I32Type; NumType I32Type] |> Vector.of_list
  in
  let output_types = Vector.of_list [] in
  Types.FuncType (input_types, output_types)

let store_delete =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | [Values.(Num (I32 key_offset)); Values.(Num (I32 key_length))] ->
          let key_length = Int32.to_int key_length in
          if key_length > Durable.max_key_length then
            raise (Key_too_large key_length) ;
          let* memory = retrieve_memory memories in
          let* key = Memory.load_bytes memory key_offset key_length in
          let tree = Durable.of_storage_exn durable in
          let key = Durable.key_of_string_exn key in
          let+ tree = Durable.delete tree key in
          (Durable.to_storage tree, [])
      | _ -> raise Bad_input)

let store_list_size_name = "tezos_store_list_size"

let store_list_size_type =
  let input_types =
    Types.[NumType I32Type; NumType I32Type] |> Vector.of_list
  in
  let output_types = Types.[NumType I64Type] |> Vector.of_list in
  Types.FuncType (input_types, output_types)

let store_list_size =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | [Values.(Num (I32 key_offset)); Values.(Num (I32 key_length))] ->
          let key_length = Int32.to_int key_length in
          if key_length > Durable.max_key_length then
            raise (Key_too_large key_length) ;
          let* memory = retrieve_memory memories in
          let* key = Memory.load_bytes memory key_offset key_length in
          let tree = Durable.of_storage_exn durable in
          let key = Durable.key_of_string_exn key in
          let+ num_subtrees = Durable.count_subtrees tree key in
          (durable, [Values.(Num (I64 (I64.of_int_s num_subtrees)))])
      | _ -> raise Bad_input)

let store_copy_name = "tezos_store_copy"

let store_copy_type =
  let input_types =
    Types.[NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type]
    |> Vector.of_list
  in
  let output_types = Vector.of_list [] in
  Types.FuncType (input_types, output_types)

let store_copy =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | [
       Values.(Num (I32 from_key_offset));
       Values.(Num (I32 from_key_length));
       Values.(Num (I32 to_key_offset));
       Values.(Num (I32 to_key_length));
      ] ->
          let from_key_length = Int32.to_int from_key_length in
          let to_key_length = Int32.to_int to_key_length in

          if from_key_length > Durable.max_key_length then
            raise (Key_too_large from_key_length) ;
          if to_key_length > Durable.max_key_length then
            raise (Key_too_large to_key_length) ;
          let* memory = retrieve_memory memories in
          let* from_key =
            Memory.load_bytes memory from_key_offset from_key_length
          in
          let* to_key = Memory.load_bytes memory to_key_offset to_key_length in
          let tree = Durable.of_storage_exn durable in
          let from_key = Durable.key_of_string_exn from_key in
          let to_key = Durable.key_of_string_exn to_key in
          let* move_tree = Durable.find_tree_exn tree from_key in
          let+ tree = Durable.add_tree tree to_key move_tree in
          (Durable.to_storage tree, [])
      | _ -> raise Bad_input)

let lookup_opt name =
  match name with
  | "read_input" ->
      Some (ExternFunc (HostFunc (read_input_type, read_input_name)))
  | "write_output" ->
      Some (ExternFunc (HostFunc (write_output_type, write_output_name)))
  | "write_debug" ->
      Some (ExternFunc (HostFunc (write_debug_type, write_debug_name)))
  | "store_has" -> Some (ExternFunc (HostFunc (store_has_type, store_has_name)))
  | "store_list_size" ->
      Some (ExternFunc (HostFunc (store_list_size_type, store_list_size_name)))
  | "store_delete" ->
      Some (ExternFunc (HostFunc (store_delete_type, store_delete_name)))
  | "store_copy" ->
      Some (ExternFunc (HostFunc (store_copy_type, store_copy_name)))
  | _ -> None

let lookup name =
  match lookup_opt name with Some f -> f | None -> raise Not_found

let register_host_funcs registry =
  List.fold_left
    (fun _acc (global_name, host_function) ->
      Host_funcs.register ~global_name host_function registry)
    ()
    [
      (read_input_name, read_input);
      (write_output_name, write_output);
      (write_debug_name, write_debug);
      (store_has_name, store_has);
      (store_list_size_name, store_list_size);
      (store_delete_name, store_delete);
      (store_copy_name, store_copy);
    ]

module Internal_for_tests = struct
  let aux_write_output = aux_write_output

  let write_output = Func.HostFunc (write_output_type, write_output_name)

  let aux_write_input_in_memory = aux_write_input_in_memory

  let read_input = Func.HostFunc (read_input_type, read_input_name)

  let store_has = Func.HostFunc (store_has_type, store_has_name)

  let store_delete = Func.HostFunc (store_delete_type, store_delete_name)

  let store_list_size =
    Func.HostFunc (store_list_size_type, store_list_size_name)
end
