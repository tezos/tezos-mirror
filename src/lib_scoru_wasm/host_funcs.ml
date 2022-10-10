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

let check_key_length key_length =
  if key_length > Durable.max_key_length then raise (Key_too_large key_length)

module Error = struct
  let store_key_too_large = -1l

  let store_invalid_key = -2l

  let store_not_a_value = -3l

  let memory_invalid_access = -4l
end

let safe_load_byte memory ofs len =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let+ bytes = Memory.load_bytes memory ofs len in
      Some bytes)
    (fun _ -> return None)

module Aux = struct
  let read_input ~input_buffer ~output_buffer ~memory ~rtype_offset
      ~level_offset ~id_offset ~dst ~max_bytes =
    let open Lwt.Syntax in
    Lwt.catch
      (fun () ->
        let* {rtype; raw_level; message_counter; payload} =
          Input_buffer.dequeue input_buffer
        in
        let input_size = Bytes.length payload in
        if Int64.of_int input_size > 4096L then
          raise (Eval.Crash (Source.no_region, "input too large"))
        else
          let payload =
            Bytes.sub payload 0 @@ min input_size (Int32.to_int max_bytes)
          in
          let* () = Memory.store_bytes memory dst (Bytes.to_string payload) in
          let* () = Memory.store_num memory rtype_offset 0l (I32 rtype) in
          let* () = Memory.store_num memory level_offset 0l (I32 raw_level) in
          let* () =
            Memory.store_num
              memory
              id_offset
              0l
              (I64 (Z.to_int64 message_counter))
          in
          Output_buffer.set_level output_buffer raw_level ;
          Lwt.return input_size)
      (fun exn ->
        match exn with
        | Input_buffer.Dequeue_from_empty_queue -> Lwt.return 0
        | _ -> raise exn)

  let write_output ~output_buffer ~memory ~src ~num_bytes =
    let open Lwt.Syntax in
    if num_bytes > 4096l then Lwt.return 1l
    else
      let num_bytes = Int32.to_int num_bytes in
      let* payload = Memory.load_bytes memory src num_bytes in
      let* () =
        Output_buffer.set_value output_buffer (Bytes.of_string payload)
      in
      Lwt.return 0l

  let store_has_unknown_key = 0l

  let store_has_value_only = 1l

  let store_has_subtrees_only = 2l

  let store_has_value_and_subtrees = 3l

  let store_has ~durable ~memory ~key_offset ~key_length =
    let open Lwt.Syntax in
    let key_length = Int32.to_int key_length in
    check_key_length key_length ;
    let* key = Memory.load_bytes memory key_offset key_length in
    let key = Durable.key_of_string_exn key in
    let* value_opt = Durable.find_value durable key in
    let+ num_subtrees = Durable.count_subtrees durable key in
    match (value_opt, num_subtrees) with
    | None, 0 -> store_has_unknown_key
    | Some _, 1 -> store_has_value_only
    | None, _ -> store_has_subtrees_only
    | _ -> store_has_value_and_subtrees

  let store_delete ~durable ~memory ~key_offset ~key_length =
    let open Lwt.Syntax in
    let key_length = Int32.to_int key_length in
    check_key_length key_length ;
    let* key = Memory.load_bytes memory key_offset key_length in
    let key = Durable.key_of_string_exn key in
    Durable.delete durable key

  let store_list_size ~durable ~memory ~key_offset ~key_length =
    let open Lwt.Syntax in
    let key_length = Int32.to_int key_length in
    check_key_length key_length ;
    let* key = Memory.load_bytes memory key_offset key_length in
    let key = Durable.key_of_string_exn key in
    let+ num_subtrees = Durable.count_subtrees durable key in
    (durable, I64.of_int_s num_subtrees)

  let store_copy ~durable ~memory ~from_key_offset ~from_key_length
      ~to_key_offset ~to_key_length =
    let open Lwt_syntax in
    let from_key_length = Int32.to_int from_key_length in
    let to_key_length = Int32.to_int to_key_length in
    check_key_length from_key_length ;
    check_key_length to_key_length ;
    let* from_key = Memory.load_bytes memory from_key_offset from_key_length in
    let* to_key = Memory.load_bytes memory to_key_offset to_key_length in
    let from_key = Durable.key_of_string_exn from_key in
    let to_key = Durable.key_of_string_exn to_key in
    Durable.copy_tree_exn durable from_key to_key

  let store_move ~durable ~memory ~from_key_offset ~from_key_length
      ~to_key_offset ~to_key_length =
    let open Lwt.Syntax in
    let from_key_length = Int32.to_int from_key_length in
    let to_key_length = Int32.to_int to_key_length in
    check_key_length from_key_length ;
    check_key_length to_key_length ;
    let* from_key = Memory.load_bytes memory from_key_offset from_key_length in
    let* to_key = Memory.load_bytes memory to_key_offset to_key_length in
    let from_key = Durable.key_of_string_exn from_key in
    let to_key = Durable.key_of_string_exn to_key in
    Durable.move_tree_exn durable from_key to_key

  let store_value_size ~durable ~memory ~key_offset ~key_length =
    let open Lwt_syntax in
    let key_len = Int32.to_int key_length in
    if key_len > Durable.max_key_length then return Error.store_key_too_large
    else
      let* key = safe_load_byte memory key_offset key_len in
      match key with
      | Some key -> (
          match Durable.key_of_string_opt key with
          | Some key -> (
              let* bytes = Durable.find_value durable key in
              match bytes with
              | Some bytes ->
                  let size =
                    Tezos_lazy_containers.Chunked_byte_vector.length bytes
                  in
                  return (Int64.to_int32 size)
              | None -> return Error.store_not_a_value)
          | None -> return Error.store_invalid_key)
      | None -> return Error.memory_invalid_access

  let store_read ~durable ~memory ~key_offset ~key_length ~value_offset ~dest
      ~max_bytes =
    let open Lwt_syntax in
    let value_offset = Int64.of_int32 value_offset in
    let max_bytes = Int64.of_int32 max_bytes in
    let key_length = Int32.to_int key_length in
    check_key_length key_length ;
    let* key = Memory.load_bytes memory key_offset key_length in
    let key = Durable.key_of_string_exn key in
    let* value = Durable.read_value_exn durable key value_offset max_bytes in
    let+ () = Memory.store_bytes memory dest value in
    Int32.of_int (String.length value)

  let store_write ~durable ~memory ~key_offset ~key_length ~value_offset ~src
      ~num_bytes =
    let open Lwt_syntax in
    if num_bytes > 4096l then Lwt.return (durable, 1l)
    else
      let value_offset = Int64.of_int32 value_offset in
      let num_bytes = Int32.to_int num_bytes in
      let key_length = Int32.to_int key_length in
      check_key_length key_length ;
      let* key = Memory.load_bytes memory key_offset key_length in
      let* payload = Memory.load_bytes memory src num_bytes in
      let key = Durable.key_of_string_exn key in
      let+ durable = Durable.write_value_exn durable key value_offset payload in
      (durable, 0l)

  let store_get_nth_key ~durable ~memory ~key_offset ~key_length ~index ~dst
      ~max_size =
    let open Lwt.Syntax in
    let index = Int64.to_int index in
    let key_length = Int32.to_int key_length in
    check_key_length key_length ;
    let* key = Memory.load_bytes memory key_offset key_length in
    let key = Durable.key_of_string_exn key in
    let* result = Durable.subtree_name_at durable key index in

    let result_size = String.length result in
    let max_size = Int32.to_int max_size in
    let result =
      if max_size < result_size then String.sub result 0 max_size else result
    in
    let* _ =
      if result <> "" then Memory.store_bytes memory dst result
      else Lwt.return_unit
    in
    Lwt.return (Int32.of_int @@ String.length result)
end

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
            Aux.read_input
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
    (fun _input_buffer output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | [Values.(Num (I32 src)); Values.(Num (I32 num_bytes))] ->
          let* memory = retrieve_memory memories in
          let* x = Aux.write_output ~output_buffer ~memory ~src ~num_bytes in
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

let store_has =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | [Values.(Num (I32 key_offset)); Values.(Num (I32 key_length))] ->
          let* memory = retrieve_memory memories in
          let+ r =
            Aux.store_has
              ~durable:(Durable.of_storage_exn durable)
              ~memory
              ~key_offset
              ~key_length
          in
          (durable, [Values.(Num (I32 r))])
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
      match inputs with
      | [Values.(Num (I32 key_offset)); Values.(Num (I32 key_length))] ->
          let open Lwt.Syntax in
          let* memory = retrieve_memory memories in
          let+ durable =
            Aux.store_delete
              ~durable:(Durable.of_storage_exn durable)
              ~memory
              ~key_offset
              ~key_length
          in
          (Durable.to_storage durable, [])
      | _ -> raise Bad_input)

let store_value_size_name = "tezos_store_value_size"

let store_value_size_type =
  let input_types =
    Types.[NumType I32Type; NumType I32Type] |> Vector.of_list
  in
  let output_types = Vector.of_list Types.[NumType I32Type] in
  Types.FuncType (input_types, output_types)

let store_value_size =
  let open Lwt_syntax in
  let open Values in
  Host_funcs.Host_func
    (fun _input _output durable memories inputs ->
      match inputs with
      | [Num (I32 key_offset); Num (I32 key_length)] ->
          let* memory = retrieve_memory memories in
          let+ res =
            Aux.store_value_size
              ~durable:(Durable.of_storage_exn durable)
              ~memory
              ~key_offset
              ~key_length
          in
          (durable, Values.[Num (I32 res)])
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
          let* memory = retrieve_memory memories in
          let+ durable, result =
            Aux.store_list_size
              ~durable:(Durable.of_storage_exn durable)
              ~memory
              ~key_offset
              ~key_length
          in
          (Durable.to_storage durable, [Values.(Num (I64 result))])
      | _ -> raise Bad_input)

let store_get_nth_key_name = "tezos_store_get_nth_key_list"

let store_get_nth_key_type =
  let input_types =
    Types.
      [
        NumType I32Type;
        NumType I32Type;
        NumType I64Type;
        NumType I32Type;
        NumType I32Type;
      ]
    |> Vector.of_list
  in
  let output_types = Types.[NumType I32Type] |> Vector.of_list in
  Types.FuncType (input_types, output_types)

let store_get_nth_key =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      let open Lwt.Syntax in
      match inputs with
      | Values.
          [
            Num (I32 key_offset);
            Num (I32 key_length);
            Num (I64 index);
            Num (I32 dst);
            Num (I32 max_size);
          ] ->
          let* memory = retrieve_memory memories in
          let+ result =
            Aux.store_get_nth_key
              ~durable:(Durable.of_storage_exn durable)
              ~memory
              ~key_offset
              ~key_length
              ~index
              ~dst
              ~max_size
          in
          (durable, [Values.(Num (I32 result))])
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
      match inputs with
      | [
       Values.(Num (I32 from_key_offset));
       Values.(Num (I32 from_key_length));
       Values.(Num (I32 to_key_offset));
       Values.(Num (I32 to_key_length));
      ] ->
          let open Lwt.Syntax in
          let* memory = retrieve_memory memories in
          let durable = Durable.of_storage_exn durable in
          let+ durable =
            Aux.store_copy
              ~durable
              ~memory
              ~from_key_offset
              ~from_key_length
              ~to_key_offset
              ~to_key_length
          in
          (Durable.to_storage durable, [])
      | _ -> raise Bad_input)

let store_move_name = "tezos_store_move"

let store_move_type =
  let input_types =
    Types.[NumType I32Type; NumType I32Type; NumType I32Type; NumType I32Type]
    |> Vector.of_list
  in
  let output_types = Vector.of_list [] in
  Types.FuncType (input_types, output_types)

let store_move =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      match inputs with
      | [
       Values.(Num (I32 from_key_offset));
       Values.(Num (I32 from_key_length));
       Values.(Num (I32 to_key_offset));
       Values.(Num (I32 to_key_length));
      ] ->
          let open Lwt.Syntax in
          let* memory = retrieve_memory memories in
          let durable = Durable.of_storage_exn durable in
          let+ durable =
            Aux.store_move
              ~durable
              ~memory
              ~from_key_offset
              ~from_key_length
              ~to_key_offset
              ~to_key_length
          in
          (Durable.to_storage durable, [])
      | _ -> raise Bad_input)

let store_read_name = "tezos_store_read"

let store_read_type =
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
  let output_types = Vector.of_list Types.[NumType I32Type] in
  Types.FuncType (input_types, output_types)

let store_read =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      match inputs with
      | [
       Values.(Num (I32 key_offset));
       Values.(Num (I32 key_length));
       Values.(Num (I32 value_offset));
       Values.(Num (I32 dest));
       Values.(Num (I32 max_bytes));
      ] ->
          let open Lwt.Syntax in
          let* memory = retrieve_memory memories in
          let+ len =
            Aux.store_read
              ~durable:(Durable.of_storage_exn durable)
              ~memory
              ~key_offset
              ~key_length
              ~value_offset
              ~dest
              ~max_bytes
          in
          (durable, [Values.(Num (I32 len))])
      | _ -> raise Bad_input)

(** [reveal_args args] compute the list of arguments type of a host
    functions resulting in a reveal tick. [args] is the list of types
    specific to a given host functions, and [reveal_args] appends two
    int32 which specify the output buffer (base, and length) where the
    result of the function is to be stored. *)
let reveal_args args = args @ Types.[NumType I32Type; NumType I32Type]

let reveal_preimage_name = "reveal_preimage"

let reveal_preimage_type =
  let input_types = reveal_args Types.[NumType I32Type] |> Vector.of_list in
  let output_types = Types.[NumType I32Type] |> Vector.of_list in
  Types.FuncType (input_types, output_types)

let reveal_preimage_parse_args memories args =
  let open Lwt_syntax in
  match args with
  | Values.[Num (I32 hash_addr); Num (I32 base); Num (I32 max_bytes)] ->
      let* memory = retrieve_memory memories in
      let+ hash = Memory.load_bytes memory hash_addr 32 in
      ( Reveal.(Reveal_raw_data (input_hash_from_string_exn hash)),
        Host_funcs.{base; max_bytes} )
  | _ -> raise Bad_input

let reveal_preimage = Host_funcs.Reveal_func reveal_preimage_parse_args

let store_write_name = "tezos_write_read"

let store_write_type =
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
  let output_types = Vector.of_list Types.[NumType I32Type] in
  Types.FuncType (input_types, output_types)

let store_write =
  Host_funcs.Host_func
    (fun _input_buffer _output_buffer durable memories inputs ->
      match inputs with
      | [
       Values.(Num (I32 key_offset));
       Values.(Num (I32 key_length));
       Values.(Num (I32 value_offset));
       Values.(Num (I32 src));
       Values.(Num (I32 num_bytes));
      ] ->
          let open Lwt.Syntax in
          let* memory = retrieve_memory memories in
          let durable = Durable.of_storage_exn durable in
          let+ durable, ret =
            Aux.store_write
              ~durable
              ~memory
              ~key_offset
              ~key_length
              ~value_offset
              ~src
              ~num_bytes
          in
          (Durable.to_storage durable, [Values.(Num (I32 ret))])
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
  | "store_get_nth_key" ->
      Some
        (ExternFunc (HostFunc (store_get_nth_key_type, store_get_nth_key_name)))
  | "store_delete" ->
      Some (ExternFunc (HostFunc (store_delete_type, store_delete_name)))
  | "store_copy" ->
      Some (ExternFunc (HostFunc (store_copy_type, store_copy_name)))
  | "store_move" ->
      Some (ExternFunc (HostFunc (store_move_type, store_move_name)))
  | "store_value_size" ->
      Some
        (ExternFunc (HostFunc (store_value_size_type, store_value_size_name)))
  | "reveal_preimage" ->
      Some (ExternFunc (HostFunc (reveal_preimage_type, reveal_preimage_name)))
  | "store_read" ->
      Some (ExternFunc (HostFunc (store_read_type, store_read_name)))
  | "store_write" ->
      Some (ExternFunc (HostFunc (store_write_type, store_write_name)))
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
      (store_get_nth_key_name, store_get_nth_key);
      (store_delete_name, store_delete);
      (store_copy_name, store_copy);
      (store_move_name, store_move);
      (store_value_size_name, store_value_size);
      (reveal_preimage_name, reveal_preimage);
      (store_read_name, store_read);
      (store_write_name, store_write);
    ]

module Internal_for_tests = struct
  let write_output = Func.HostFunc (write_output_type, write_output_name)

  let read_input = Func.HostFunc (read_input_type, read_input_name)

  let store_has = Func.HostFunc (store_has_type, store_has_name)

  let store_delete = Func.HostFunc (store_delete_type, store_delete_name)

  let store_copy = Func.HostFunc (store_copy_type, store_copy_name)

  let store_move = Func.HostFunc (store_move_type, store_move_name)

  let store_read = Func.HostFunc (store_read_type, store_read_name)

  let store_write = Func.HostFunc (store_write_type, store_write_name)

  let store_value_size =
    Func.HostFunc (store_value_size_type, store_value_size_name)

  let store_list_size =
    Func.HostFunc (store_list_size_type, store_list_size_name)

  let store_get_nth_key =
    Func.HostFunc (store_get_nth_key_type, store_get_nth_key_name)
end
