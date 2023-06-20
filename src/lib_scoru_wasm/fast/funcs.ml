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

module Host_funcs = struct
  module Aux : Host_funcs.Aux.S with type memory = Wasmer.Memory.t =
    Host_funcs.Aux.Make (Memory_access.Wasmer)
end

type host_state = {
  retrieve_mem : unit -> Wasmer.Memory.t;
  buffers : Tezos_webassembly_interpreter.Eval.buffers;
  mutable durable : Durable.t;
}

let make ~version ~reveal_builtins ~write_debug state =
  let open Wasmer in
  let open Lwt.Syntax in
  let with_mem f =
    let mem = state.retrieve_mem () in
    f mem
  in

  let read_input =
    fn
      (i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun info_addr dst max_bytes ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.read_input
          ~input_buffer:state.buffers.input
          ~memory
          ~info_addr
          ~dst
          ~max_bytes)
  in
  let write_output =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun src num_bytes ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.write_output
          ~output_buffer:state.buffers.output
          ~memory
          ~src
          ~num_bytes)
  in
  let store_exists =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_exists
          ~durable:state.durable
          ~memory
          ~key_offset
          ~key_length)
  in
  let store_has =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_has
          ~durable:state.durable
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
            ~durable:state.durable
            ~memory
            ~key_offset
            ~key_length
        in
        state.durable <- durable ;
        result)
  in
  let store_delete_generic ~kind =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length ->
        with_mem @@ fun memory ->
        let+ durable, result =
          Host_funcs.Aux.generic_store_delete
            ~kind
            ~durable:state.durable
            ~memory
            ~key_offset
            ~key_length
        in
        state.durable <- durable ;
        result)
  in
  let store_delete = store_delete_generic ~kind:Directory in
  let store_delete_value = store_delete_generic ~kind:Value in
  let write_debug =
    fn
      (i32 @-> i32 @-> returning nothing)
      (fun src num_bytes ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.write_debug ~implem:write_debug ~memory ~src ~num_bytes)
  in
  let store_copy =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun from_key_offset from_key_length to_key_offset to_key_length ->
        with_mem @@ fun memory ->
        let+ durable, result =
          Host_funcs.Aux.store_copy
            ~durable:state.durable
            ~memory
            ~from_key_offset
            ~from_key_length
            ~to_key_offset
            ~to_key_length
        in
        state.durable <- durable ;
        result)
  in
  let store_move =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun from_key_offset from_key_length to_key_offset to_key_length ->
        with_mem @@ fun memory ->
        let+ durable, result =
          Host_funcs.Aux.store_move
            ~durable:state.durable
            ~memory
            ~from_key_offset
            ~from_key_length
            ~to_key_offset
            ~to_key_length
        in
        state.durable <- durable ;
        result)
  in
  let store_create =
    fn
      (i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length size ->
        with_mem @@ fun memory ->
        let+ durable, result =
          Host_funcs.Aux.store_create
            ~durable:state.durable
            ~memory
            ~key_offset
            ~key_length
            ~size
        in
        state.durable <- durable ;
        result)
  in
  let store_read =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length value_offset dest max_bytes ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_read
          ~durable:state.durable
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
            ~durable:state.durable
            ~memory
            ~key_offset
            ~key_length
            ~value_offset
            ~src
            ~num_bytes
        in
        state.durable <- durable ;
        ret)
  in
  let store_get_nth_key =
    fn
      (i32 @-> i32 @-> i64 @-> i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length index dst max_size ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_get_nth_key
          ~durable:state.durable
          ~memory
          ~key_offset
          ~key_length
          ~index
          ~dst
          ~max_size)
  in
  let store_get_hash =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length dst max_size ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_get_hash
          ~durable:state.durable
          ~memory
          ~key_offset
          ~key_length
          ~dst
          ~max_size)
  in
  let store_value_size =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_value_size
          ~durable:state.durable
          ~memory
          ~key_offset
          ~key_length)
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4369
     Align failure mode of reveal_* functions in Fast Execution. *)
  let reveal_preimage =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun hash_addr hash_size dst max_bytes ->
        Lwt.map (Result.fold ~ok:Fun.id ~error:Fun.id)
        @@ with_mem
        @@ fun memory ->
        let open Lwt_result_syntax in
        let* hash =
          Host_funcs.Aux.load_bytes ~memory ~addr:hash_addr ~size:hash_size
        in
        let*! payload = reveal_builtins.Builtins.reveal_preimage hash in
        let*! result =
          Host_funcs.Aux.reveal
            ~memory
            ~dst
            ~max_bytes
            ~payload:(Bytes.of_string payload)
        in
        return result)
  in
  let reveal_metadata =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun dst max_bytes ->
        let mem = state.retrieve_mem () in
        let* payload = reveal_builtins.reveal_metadata () in
        Host_funcs.Aux.reveal
          ~memory:mem
          ~dst
          ~max_bytes
          ~payload:(Bytes.of_string payload))
  in

  let base =
    [
      ("read_input", read_input);
      ("write_output", write_output);
      ("write_debug", write_debug);
      ("store_has", store_has);
      ("store_list_size", store_list_size);
      ("store_value_size", store_value_size);
      ("store_delete", store_delete);
      ("store_copy", store_copy);
      ("store_move", store_move);
      ("store_read", store_read);
      ("store_write", store_write);
      ("store_get_nth_key", store_get_nth_key);
      ("reveal_preimage", reveal_preimage);
      ("reveal_metadata", reveal_metadata);
    ]
  in
  let v1 =
    [
      ("__internal_store_get_hash", store_get_hash);
      ("store_delete_value", store_delete_value);
      ("store_create", store_create);
    ]
  in
  let extra =
    match version with
    | Wasm_pvm_state.V0 -> []
    | V1 -> v1
    | V2 -> v1 @ [("store_exists", store_exists)]
  in
  List.map
    (fun (name, impl) -> (Constants.wasm_host_funcs_virual_module, name, impl))
    (base @ extra)
