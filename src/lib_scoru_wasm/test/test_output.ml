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
                    -- test "Output"
    Subject:      Input tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm

let test_output_buffer () =
  let open Lwt_result_syntax in
  let test (level, output_buffer) =
    match Output_buffer.level_range output_buffer with
    | None -> true
    | Some (first_level, max_level) ->
        if level <= max_level && level >= first_level then
          Output_buffer.is_outbox_available output_buffer level
        else true
  in
  let test =
    QCheck2.Test.make
      QCheck2.Gen.(
        tup2 (map Int32.of_int small_int) Ast_generators.output_buffer_gen)
      test
  in
  let result = QCheck_base_runner.run_tests [test] in
  if result = 0 then return_unit else failwith "QCheck tests failed"

let test_aux_write_output () =
  let open Lwt.Syntax in
  let lim = Types.(MemoryType {min = 100l; max = Some 1000l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  let output_buffer = Eval.default_output_buffer () in
  let* () =
    Input_buffer.enqueue
      input_buffer
      {
        raw_level = 0l;
        message_counter = Z.of_int 2;
        payload = Bytes.of_string "hello";
      }
  in
  assert (Input_buffer.num_elements input_buffer = Z.one) ;
  let* _ =
    Host_funcs.Aux.read_input
      ~input_buffer
      ~memory
      ~level_offset:4l
      ~id_offset:10l
      ~dst:50l
      ~max_bytes:36000l
  in
  let output_level = output_buffer.Output_buffer.last_level in
  assert (output_level = Some 0l) ;
  let* result =
    Host_funcs.Aux.write_output ~output_buffer ~memory ~src:50l ~num_bytes:5l
  in

  let last_outbox_level = output_buffer.Output_buffer.last_level in
  let* last_outbox =
    match last_outbox_level with
    | Some level -> Output_buffer.get_outbox output_buffer level
    | None -> Stdlib.failwith "No outbox exists"
  in
  let last_message_in_last_outbox =
    Output_buffer.get_outbox_last_message_index last_outbox
  in
  assert (last_outbox_level = Some 0l) ;
  assert (last_message_in_last_outbox = Some Z.zero) ;

  let* z =
    Output_buffer.get_message
      output_buffer
      {outbox_level = 0l; message_index = Z.zero}
  in
  assert (result = 0l) ;
  assert (z = Bytes.of_string "hello") ;

  Lwt.return_ok ()

let test_write_host_fun () =
  let open Lwt.Syntax in
  let input = Input_buffer.alloc () in
  let output = Eval.default_output_buffer () in
  let* () =
    Input_buffer.enqueue
      input
      {
        raw_level = 0l;
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

  let* _ =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      Host_funcs.all
      ~input
      ~output
      Host_funcs.Internal_for_tests.read_input
      values
  in
  let values = Values.[Num (I32 50l); Num (I32 5l)] in

  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      Host_funcs.all
      ~input
      ~output
      Host_funcs.Internal_for_tests.write_output
      values
  in
  let last_outbox_level = output.Output_buffer.last_level in
  let* last_outbox =
    match last_outbox_level with
    | Some level -> Output_buffer.get_outbox output level
    | None -> Stdlib.failwith "No outbox exists"
  in
  let last_message_in_last_outbox =
    Output_buffer.get_outbox_last_message_index last_outbox
  in
  assert (last_outbox_level = Some 0l) ;
  assert (last_message_in_last_outbox = Some Z.zero) ;

  let* z =
    Output_buffer.get_message output {outbox_level = 0l; message_index = Z.zero}
  in
  assert (result = Values.[Num (I32 0l)]) ;
  assert (z = Bytes.of_string "hello") ;

  let values = Values.[Num (I32 50l); Num (I32 5000l)] in
  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      Host_funcs.all
      ~input
      ~output
      Host_funcs.Internal_for_tests.write_output
      values
  in
  let last_outbox_level = output.Output_buffer.last_level in
  let* last_outbox =
    match last_outbox_level with
    | Some level -> Output_buffer.get_outbox output level
    | None ->
        Stdlib.failwith "The PVM output buffer does not contain any outbox."
  in
  let last_message_in_last_outbox =
    Output_buffer.get_outbox_last_message_index last_outbox
  in
  assert (
    result = Values.[Num (I32 Host_funcs.Error.(code Input_output_too_large))]) ;
  assert (last_outbox_level = Some 0l) ;
  assert (last_message_in_last_outbox = Some Z.zero) ;
  Lwt.return @@ Result.return_unit

let tests =
  [
    tztest "Output buffer" `Quick test_output_buffer;
    tztest "Aux_write_output" `Quick test_aux_write_output;
    tztest "Host write" `Quick test_write_host_fun;
  ]
