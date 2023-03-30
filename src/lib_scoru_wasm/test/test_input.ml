(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(** Testing
    -------
    Component:    Lib_scoru_wasm input
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Input$"
    Subject:      Input tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm

let write_input () =
  let open Lwt.Syntax in
  let input = Input_buffer.alloc () in
  let* () =
    Input_buffer.enqueue
      input
      {
        raw_level = 2l;
        message_counter = Z.of_int 2;
        payload = Bytes.of_string "hello";
      }
  in
  let* () =
    Input_buffer.enqueue
      input
      {
        raw_level = 2l;
        message_counter = Z.of_int 3;
        payload = Bytes.of_string "hello";
      }
  in
  assert (Input_buffer.num_elements input = Z.of_int 2) ;
  let* () =
    Lwt.try_bind
      (fun () ->
        Input_buffer.enqueue
          input
          {
            raw_level = 2l;
            message_counter = Z.of_int 2;
            payload = Bytes.of_string "hello";
          })
      (fun _ -> assert false)
      (function
        | Input_buffer.Cannot_store_an_earlier_message -> Lwt.return ()
        | _ -> assert false)
  in
  Lwt.return Result.return_unit

let read_input () =
  let open Lwt.Syntax in
  let lim = Types.(MemoryType {min = 100l; max = Some 1000l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  let* () =
    Input_buffer.enqueue
      input_buffer
      {
        raw_level = 2l;
        message_counter = Z.of_int 2;
        payload = Bytes.of_string "hello";
      }
  in
  assert (Input_buffer.num_elements input_buffer = Z.one) ;
  let* result =
    Host_funcs.Aux.read_input
      ~input_buffer
      ~memory
      ~info_addr:4l
      ~dst:50l
      ~max_bytes:3600l
  in
  assert (Input_buffer.num_elements input_buffer = Z.zero) ;
  assert (result = 5l) ;
  let* m = Memory.load_bytes memory 4l 4 in
  assert (m = "\002\000\000\000") ;
  let* m = Memory.load_bytes memory 8l 4 in
  assert (m = "\002\000\000\000") ;
  let* m = Memory.load_bytes memory 50l 5 in
  assert (m = "hello") ;
  Lwt.return @@ Result.return_unit

let read_input_no_messages () =
  let open Lwt.Syntax in
  let lim = Types.(MemoryType {min = 100l; max = Some 1000l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  assert (Input_buffer.num_elements input_buffer = Z.zero) ;
  let* result =
    Host_funcs.Aux.read_input
      ~input_buffer
      ~memory
      ~info_addr:4l
      ~dst:50l
      ~max_bytes:3600l
  in
  assert (Input_buffer.num_elements input_buffer = Z.zero) ;
  assert (result = 0l) ;
  Lwt.return @@ Result.return_unit

let test_host_fun ~version () =
  let open Lwt.Syntax in
  let input = Input_buffer.alloc () in
  let* () =
    Input_buffer.enqueue
      input
      {
        raw_level = 2l;
        message_counter = Z.of_int 2;
        payload = Bytes.of_string "hello";
      }
  in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memories =
    Lazy_vector.Int32Vector.cons
      (Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}))
      module_inst.memories
  in
  let module_inst = {module_inst with memories} in
  let values = Values.[Num (I32 4l); Num (I32 50l); Num (I32 3600l)] in

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;

  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      (Host_funcs.registry ~write_debug:Noop ~version)
      ~input
      Host_funcs.Internal_for_tests.read_input
      values
  in
  let* module_inst = Instance.resolve_module_ref module_reg module_key in
  let* memory = Lazy_vector.Int32Vector.get 0l module_inst.memories in
  assert (Input_buffer.num_elements input = Z.zero) ;
  let* m = Memory.load_bytes memory 4l 4 in
  assert (m = "\002\000\000\000") ;
  let* m = Memory.load_bytes memory 8l 4 in
  assert (m = "\002\000\000\000") ;
  let* m = Memory.load_bytes memory 50l 5 in
  assert (m = "hello") ;
  assert (result = Values.[Num (I32 5l)]) ;
  Lwt.return @@ Result.return_unit

let write_outside_bounds_doesnt_dequeue_gen info_addr input_addr input_max_bytes
    =
  let open Lwt_syntax in
  let lim = Types.(MemoryType {min = 1l; max = Some 2l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  let message =
    {
      Input_buffer.raw_level = 0l;
      message_counter = Z.of_int 0;
      payload = Bytes.of_string "hello";
    }
  in
  let* () = Input_buffer.enqueue input_buffer message in
  let input_buffer_size = Input_buffer.num_elements input_buffer in
  let* result =
    Host_funcs.Aux.read_input
      ~input_buffer
      ~memory
      ~info_addr
      ~dst:input_addr
      ~max_bytes:input_max_bytes
  in
  assert (result = Host_funcs.Error.code Memory_invalid_access) ;
  assert (Input_buffer.num_elements input_buffer = input_buffer_size) ;
  let* dequeued_message = Input_buffer.dequeue input_buffer in
  assert (message = dequeued_message) ;
  return_ok_unit

let test_write_input_info_invalid_address () =
  (* The memory is allocated with only one page, this address would be on the
     second page. *)
  let address = Int64.(to_int32 (add Memory.page_size 100L)) in
  write_outside_bounds_doesnt_dequeue_gen address 0l 100l

let test_write_input_info_beyond_memory_bounds () =
  (* The memory is allocated with only one page, this address would be the exact
     one at the end of this page. The input would be written on the second page
     too. *)
  let address = Int64.(to_int32 (sub Memory.page_size 1L)) in
  write_outside_bounds_doesnt_dequeue_gen address 0l 100l

let test_write_input_invalid_address () =
  (* The memory is allocated with only one page, this address would be on the
     second page. *)
  let address = Int64.(to_int32 (add Memory.page_size 100L)) in
  write_outside_bounds_doesnt_dequeue_gen 0l address 100l

let test_write_input_beyond_memory_bounds () =
  (* The memory is allocated with only one page, this address would be at the
     end of this page. *)
  let address = Int64.(to_int32 (sub Memory.page_size 1L)) in
  let length = 100l in
  write_outside_bounds_doesnt_dequeue_gen 0l address length

(* Note that this case should never happen, the size of messages in enforced by
   the protocol. *)
let test_read_input_too_big_payload () =
  let open Lwt_syntax in
  let lim = Types.(MemoryType {min = 1l; max = Some 2l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  let message_size = Host_funcs.Aux.input_output_max_size * 2 in
  let original_message = Bytes.make message_size 'a' in
  let message =
    {
      Input_buffer.raw_level = 0l;
      message_counter = Z.of_int 0;
      payload = original_message;
    }
  in
  let* () = Input_buffer.enqueue input_buffer message in
  let* result =
    Host_funcs.Aux.read_input
      ~input_buffer
      ~memory
      ~info_addr:0l
      ~dst:10l
      ~max_bytes:(Int32.of_int Host_funcs.Aux.input_output_max_size)
  in
  assert (result = Int32.of_int Host_funcs.Aux.input_output_max_size) ;
  let* message_in_memory = Partial_memory.load_bytes memory 10l message_size in
  assert (not (Bytes.equal (Bytes.of_string message_in_memory) original_message)) ;
  let expected_message = String.make Host_funcs.Aux.input_output_max_size 'a' in
  assert (
    String.equal
      (String.sub message_in_memory 0 (Int32.to_int result))
      expected_message) ;
  return_ok_unit

let test_read_input_max_size_above_limit () =
  let open Lwt_syntax in
  let lim = Types.(MemoryType {min = 1l; max = Some 2l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  let message_size = Host_funcs.Aux.input_output_max_size * 2 in
  let original_message = Bytes.make message_size 'a' in
  let message =
    {
      Input_buffer.raw_level = 0l;
      message_counter = Z.of_int 0;
      payload = original_message;
    }
  in
  let* () = Input_buffer.enqueue input_buffer message in
  let* result =
    Host_funcs.Aux.read_input
      ~input_buffer
      ~memory
      ~info_addr:0l
      ~dst:10l
      ~max_bytes:(Int32.of_int message_size)
  in
  assert (result = Int32.of_int Host_funcs.Aux.input_output_max_size) ;
  let* message_in_memory = Partial_memory.load_bytes memory 10l message_size in
  assert (not (Bytes.equal (Bytes.of_string message_in_memory) original_message)) ;
  let expected_message = String.make Host_funcs.Aux.input_output_max_size 'a' in
  assert (
    String.equal
      (String.sub message_in_memory 0 (Int32.to_int result))
      expected_message) ;
  return_ok_unit

let tests =
  Tztest_helper.tztests_with_pvm
    ~versions:[V0; V1]
    [("Host read input", `Quick, test_host_fun)]
  @ [
      tztest "Write input" `Quick write_input;
      tztest "Read input" `Quick read_input;
      tztest "Read input no messages" `Quick read_input_no_messages;
      tztest
        "Write input info at invalid address doesn't dequeue the input"
        `Quick
        test_write_input_info_invalid_address;
      tztest
        "Write input info beyond memory bounds doesn't dequeue the input"
        `Quick
        test_write_input_info_beyond_memory_bounds;
      tztest
        "Write input at invalid address doesn't dequeue the input"
        `Quick
        test_write_input_invalid_address;
      tztest
        "Write input beyond memory bounds doesn't dequeue the input"
        `Quick
        test_write_input_beyond_memory_bounds;
      tztest
        "Payload bigger than max size constant are truncated"
        `Quick
        test_read_input_too_big_payload;
      tztest
        "Payload bigger and expected max size constant are truncated"
        `Quick
        test_read_input_max_size_above_limit;
    ]
