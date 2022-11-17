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

open Tezos_scoru_wasm
module Wasmer = Tezos_wasmer
module Lazy_containers = Tezos_lazy_containers

let store =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4121
     Making this global is potentially not a great idea. Maybe lazy? *)
  let engine = Wasmer.Engine.create Wasmer.Config.{compiler = SINGLEPASS} in
  Wasmer.Store.create engine

let load_kernel durable =
  let open Lwt.Syntax in
  let* kernel = Durable.find_value_exn durable Constants.kernel_key in
  let+ kernel = Lazy_containers.Chunked_byte_vector.to_string kernel in
  Wasmer.Module.(create store Binary kernel)

let to_ref_memory mem =
  let open Wasmer.Memory in
  let get_chunk page_id =
    let start_address =
      Int64.mul page_id Lazy_containers.Chunked_byte_vector.Chunk.size
    in
    let body =
      Bytes.init
        (Int64.to_int Lazy_containers.Chunked_byte_vector.Chunk.size)
        (fun i ->
          Wasmer.Memory.get mem Int64.(add start_address (of_int i) |> to_int)
          |> Unsigned.UInt8.to_int |> Char.chr)
    in
    Lwt.return (Lazy_containers.Chunked_byte_vector.Chunk.of_bytes body)
  in
  let length = Wasmer.Memory.length mem |> Int64.of_int in
  let chunked = Lazy_containers.Chunked_byte_vector.create ~get_chunk length in
  let min = Unsigned.UInt32.to_int32 mem.min in
  let max = Option.map Unsigned.UInt32.to_int32 mem.max in
  Tezos_webassembly_interpreter.(
    Partial_memory.of_chunks (MemoryType {min; max}) chunked)

let commit_memory mem partial_memory =
  let chunks =
    Lazy_containers.Chunked_byte_vector.loaded_chunks
      (Tezos_webassembly_interpreter.Partial_memory.content partial_memory)
  in
  List.iter
    (function
      | chunk_id, Some chunk ->
          let start =
            Int64.mul chunk_id Lazy_containers.Chunked_byte_vector.Chunk.size
          in
          let body = Lazy_containers.Chunked_byte_vector.Chunk.to_bytes chunk in
          Bytes.iteri
            (fun i char ->
              let addr = Int64.(add start (of_int i) |> to_int) in
              Wasmer.Memory.set
                mem
                addr
                (Char.code char |> Unsigned.UInt8.of_int))
            body
      | _ -> ())
    chunks

let compute durable (buffers : Tezos_webassembly_interpreter.Eval.buffers) =
  let open Lwt.Syntax in
  let* module_ = load_kernel durable in

  let main_mem = ref None in
  let retrieve_mem () =
    match !main_mem with Some x -> x () | None -> assert false
  in
  let with_mem f =
    let mem = retrieve_mem () in
    let ref_mem = to_ref_memory mem in
    let+ value = f ref_mem in
    commit_memory mem ref_mem ;
    value
  in

  let durable_ref = ref durable in
  let with_durable f =
    let+ durable = f !durable_ref in
    durable_ref := durable
  in

  let host_funcs =
    let open Wasmer in
    let read_input =
      fn
        (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
        (fun level_offset id_offset dst max_bytes ->
          with_mem @@ fun memory ->
          Host_funcs.Aux.read_input
            ~input_buffer:buffers.input
            ~output_buffer:buffers.output
            ~memory
            ~level_offset
            ~id_offset
            ~dst
            ~max_bytes)
    in
    let write_output =
      fn
        (i32 @-> i32 @-> returning1 i32)
        (fun src num_bytes ->
          with_mem @@ fun memory ->
          Host_funcs.Aux.write_output
            ~output_buffer:buffers.output
            ~memory
            ~src
            ~num_bytes)
    in
    let store_has =
      fn
        (i32 @-> i32 @-> returning1 i32)
        (fun key_offset key_length ->
          with_mem @@ fun memory ->
          Host_funcs.Aux.store_has
            ~durable:!durable_ref
            ~memory
            ~key_offset
            ~key_length)
    in
    let store_list_size =
      fn
        (i32 @-> i32 @-> returning1 i64)
        (fun key_offset key_length ->
          with_mem @@ fun memory ->
          let+ durable, result =
            Host_funcs.Aux.store_list_size
              ~durable:!durable_ref
              ~memory
              ~key_offset
              ~key_length
          in
          durable_ref := durable ;
          result)
    in
    let store_delete =
      fn
        (i32 @-> i32 @-> returning1 i32)
        (fun key_offset key_length ->
          with_mem @@ fun memory ->
          let+ durable, result =
            Host_funcs.Aux.store_delete
              ~durable:!durable_ref
              ~memory
              ~key_offset
              ~key_length
          in
          durable_ref := durable ;
          result)
    in
    let write_debug =
      fn
        (i32 @-> i32 @-> returning nothing)
        (fun key_offset key_length ->
          let mem = retrieve_mem () in
          let len = Wasmer.Memory.length mem |> Int32.of_int in
          let key_offset, key_length =
            match () with
            | () when key_offset >= len ->
                (* Start of key is out of bounds *)
                (0l, 0l)
            | () when key_length > Int32.sub len key_offset ->
                (* End of key would exceeds bounds *)
                (key_offset, Int32.sub len key_offset)
            | () ->
                (* Everything is ok *)
                (key_offset, key_length)
          in
          let key_offset = Int32.to_int key_offset in
          let str =
            String.init (Int32.to_int key_length) (fun i ->
                Wasmer.Memory.get mem (key_offset + i)
                |> Unsigned.UInt8.to_int |> Char.chr)
          in
          Printf.printf "DEBUG: %s\n" str ;
          Lwt.return ())
    in
    let store_copy =
      fn
        (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
        (fun from_key_offset from_key_length to_key_offset to_key_length ->
          with_mem @@ fun memory ->
          let+ durable, result =
            Host_funcs.Aux.store_copy
              ~durable:!durable_ref
              ~memory
              ~from_key_offset
              ~from_key_length
              ~to_key_offset
              ~to_key_length
          in
          durable_ref := durable ;
          result)
    in
    let store_move =
      fn
        (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
        (fun from_key_offset from_key_length to_key_offset to_key_length ->
          with_mem @@ fun memory ->
          let+ durable, result =
            Host_funcs.Aux.store_move
              ~durable:!durable_ref
              ~memory
              ~from_key_offset
              ~from_key_length
              ~to_key_offset
              ~to_key_length
          in
          durable_ref := durable ;
          result)
    in
    let store_read =
      fn
        (i32 @-> i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
        (fun key_offset key_length value_offset dest max_bytes ->
          with_mem @@ fun memory ->
          Host_funcs.Aux.store_read
            ~durable:!durable_ref
            ~memory
            ~key_offset
            ~key_length
            ~value_offset
            ~dest
            ~max_bytes)
    in
    let store_write =
      fn
        (i32 @-> i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
        (fun key_offset key_length value_offset src num_bytes ->
          with_mem @@ fun memory ->
          let+ durable, ret =
            Host_funcs.Aux.store_write
              ~durable:!durable_ref
              ~memory
              ~key_offset
              ~key_length
              ~value_offset
              ~src
              ~num_bytes
          in
          durable_ref := durable ;
          ret)
    in
    let store_get_nth_key =
      fn
        (i32 @-> i32 @-> i64 @-> i32 @-> i32 @-> returning1 i32)
        (fun key_offset key_length index dst max_size ->
          with_mem @@ fun memory ->
          Host_funcs.Aux.store_get_nth_key
            ~durable:!durable_ref
            ~memory
            ~key_offset
            ~key_length
            ~index
            ~dst
            ~max_size)
    in
    let store_value_size =
      fn
        (i32 @-> i32 @-> returning1 i32)
        (fun key_offset key_length ->
          with_mem @@ fun memory ->
          Host_funcs.Aux.store_value_size
            ~durable:!durable_ref
            ~memory
            ~key_offset
            ~key_length)
    in
    [
      ("rollup_safe_core", "read_input", read_input);
      ("rollup_safe_core", "write_output", write_output);
      ("rollup_safe_core", "write_debug", write_debug);
      ("rollup_safe_core", "store_has", store_has);
      ("rollup_safe_core", "store_list_size", store_list_size);
      ("rollup_safe_core", "store_value_size", store_value_size);
      ("rollup_safe_core", "store_delete", store_delete);
      ("rollup_safe_core", "store_copy", store_copy);
      ("rollup_safe_core", "store_move", store_move);
      ("rollup_safe_core", "store_read", store_read);
      ("rollup_safe_core", "store_write", store_write);
      ("rollup_safe_core", "store_get_nth_key", store_get_nth_key);
    ]
  in

  let* instance = Wasmer.Instance.create store module_ host_funcs in

  let* () =
    (* At this point we know that the kernel is valid because we parsed and
       instantiated it. It is now safe to set it as the fallback kernel. *)
    with_durable Wasm_vm.save_fallback_kernel
  in

  let exports = Wasmer.Exports.from_instance instance in
  let kernel_next =
    Wasmer.(Exports.fn exports "kernel_next" (producer nothing))
  in

  main_mem := Some (fun () -> Wasmer.Exports.mem0 exports) ;

  let* () = kernel_next () in

  Wasmer.Instance.delete instance ;
  Wasmer.Module.delete module_ ;
  Lwt.return !durable_ref
