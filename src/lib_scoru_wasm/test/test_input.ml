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
  let output_buffer = Output_buffer.alloc () in
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
      ~output_buffer
      ~memory
      ~level_offset:4l
      ~id_offset:10l
      ~dst:50l
      ~max_bytes:36000l
  in
  let* output_level, output_id = Output_buffer.get_id output_buffer in
  assert (output_level = 2l) ;
  assert (output_id = Z.of_int (-1)) ;
  assert (Input_buffer.num_elements input_buffer = Z.zero) ;
  assert (result = 5l) ;
  let* m = Memory.load_bytes memory 4l 1 in
  assert (m = "\002") ;
  let* m = Memory.load_bytes memory 10l 1 in
  assert (m = "\002") ;
  let* m = Memory.load_bytes memory 50l 5 in
  assert (m = "hello") ;
  Lwt.return @@ Result.return_unit

let read_input_no_messages () =
  let open Lwt.Syntax in
  let lim = Types.(MemoryType {min = 100l; max = Some 1000l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  let output_buffer = Output_buffer.alloc () in
  assert (Input_buffer.num_elements input_buffer = Z.zero) ;
  let* result =
    Host_funcs.Aux.read_input
      ~input_buffer
      ~output_buffer
      ~memory
      ~level_offset:4l
      ~id_offset:10l
      ~dst:50l
      ~max_bytes:36000l
  in
  assert (Input_buffer.num_elements input_buffer = Z.zero) ;
  assert (result = 0l) ;
  Lwt.return @@ Result.return_unit

let read_input_too_large () =
  let open Lwt.Syntax in
  let lim = Types.(MemoryType {min = 100l; max = Some 1000l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  let output_buffer = Output_buffer.alloc () in
  let* () =
    Input_buffer.enqueue
      input_buffer
      {
        raw_level = 2l;
        message_counter = Z.of_int 2;
        payload = Bytes.make 5000 '\000';
      }
  in
  assert (Input_buffer.num_elements input_buffer = Z.one) ;
  let* result =
    Host_funcs.Aux.read_input
      ~input_buffer
      ~output_buffer
      ~memory
      ~level_offset:4l
      ~id_offset:10l
      ~dst:50l
      ~max_bytes:36000l
  in
  assert (Input_buffer.num_elements input_buffer = Z.zero) ;
  assert (result = Host_funcs.Error.(code Input_output_too_large)) ;
  Lwt.return @@ Result.return_unit

let test_host_fun () =
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
  let values =
    Values.[Num (I32 4l); Num (I32 10l); Num (I32 50l); Num (I32 3600l)]
  in

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;

  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      Host_funcs.all
      ~input
      Host_funcs.Internal_for_tests.read_input
      values
  in
  let* module_inst = Instance.resolve_module_ref module_reg module_key in
  let* memory = Lazy_vector.Int32Vector.get 0l module_inst.memories in
  assert (Input_buffer.num_elements input = Z.zero) ;
  let* m = Memory.load_bytes memory 4l 1 in
  assert (m = "\002") ;
  let* m = Memory.load_bytes memory 10l 1 in
  assert (m = "\002") ;
  let* m = Memory.load_bytes memory 50l 5 in
  assert (m = "hello") ;
  assert (result = Values.[Num (I32 5l)]) ;
  Lwt.return @@ Result.return_unit

let tests =
  [
    tztest "Write input" `Quick write_input;
    tztest "Read input" `Quick read_input;
    tztest "Read input no messages" `Quick read_input_no_messages;
    tztest "Read input too large" `Quick read_input_too_large;
    tztest "Host read input" `Quick test_host_fun;
  ]
