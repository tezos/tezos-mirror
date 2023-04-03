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
    match Output_buffer.Internal_for_tests.level_range output_buffer with
    | None -> true
    | Some (first_level, max_level) ->
        if level <= max_level && level >= first_level then
          Output_buffer.Internal_for_tests.is_outbox_available
            output_buffer
            level
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
      ~info_addr:4l
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
    | Some level ->
        Output_buffer.Internal_for_tests.get_outbox output_buffer level
    | None -> Stdlib.failwith "No outbox exists"
  in
  let last_message_in_last_outbox =
    Output_buffer.Internal_for_tests.get_outbox_last_message_index last_outbox
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

let test_write_host_fun ~version () =
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
  let values = Values.[Num (I32 4l); Num (I32 50l); Num (I32 3600l)] in

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;

  let* _ =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      (Host_funcs.registry ~write_debug:Noop ~version)
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
      (Host_funcs.registry ~write_debug:Noop ~version)
      ~input
      ~output
      Host_funcs.Internal_for_tests.write_output
      values
  in
  let last_outbox_level = output.Output_buffer.last_level in
  let* last_outbox =
    match last_outbox_level with
    | Some level -> Output_buffer.Internal_for_tests.get_outbox output level
    | None -> Stdlib.failwith "No outbox exists"
  in
  let last_message_in_last_outbox =
    Output_buffer.Internal_for_tests.get_outbox_last_message_index last_outbox
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
      (Host_funcs.registry ~write_debug:Noop ~version)
      ~input
      ~output
      Host_funcs.Internal_for_tests.write_output
      values
  in
  let last_outbox_level = output.Output_buffer.last_level in
  let* last_outbox =
    match last_outbox_level with
    | Some level -> Output_buffer.Internal_for_tests.get_outbox output level
    | None ->
        Stdlib.failwith "The PVM output buffer does not contain any outbox."
  in
  let last_message_in_last_outbox =
    Output_buffer.Internal_for_tests.get_outbox_last_message_index last_outbox
  in
  assert (
    result = Values.[Num (I32 Host_funcs.Error.(code Input_output_too_large))]) ;
  assert (last_outbox_level = Some 0l) ;
  assert (last_message_in_last_outbox = Some Z.zero) ;
  Lwt.return @@ Result.return_unit

(* This test takes a [limit] for the outbox and a number [n] of message to push
   in it, and tries to push [n] messages in the outbox. If the push succeeds, it
   check the message is indeed below the limit, otherwise we expect an error
   that the outbox is full. *)
let test_output_limit_gen ~limit ~message_number =
  let open Lwt_syntax in
  let message_limit = Z.of_int limit in
  let buffer =
    Output_buffer.alloc
      ~message_limit
      ~validity_period:Wasm_utils.default_outbox_validity_period
      ~last_level:(Some 0l)
  in
  let push_message expected_message_index =
    Lwt.catch
      (fun () ->
        let+ Output_buffer.{outbox_level = _; message_index} =
          Output_buffer.push_message buffer (Bytes.of_string "message")
        in
        assert (Z.equal expected_message_index message_index) ;
        assert (Z.Compare.(message_index < message_limit)))
      (fun exn ->
        assert (exn = Output_buffer.Full_outbox) ;
        return_unit)
  in

  let messages =
    Result.value
      ~default:[]
      (List.init ~when_negative_length:[] message_number Z.of_int)
  in
  let* () = List.iter_s push_message messages in
  let* outbox = Output_buffer.Internal_for_tests.get_outbox buffer 0l in
  if message_number > limit then
    assert (Output_buffer.Messages.num_elements outbox = message_limit)
  else
    assert (Output_buffer.Messages.num_elements outbox = Z.of_int message_number) ;
  return_ok_unit

let test_messages_below_limit () =
  test_output_limit_gen ~limit:10 ~message_number:5

let test_messages_at_limit () =
  test_output_limit_gen ~limit:10 ~message_number:10

let test_messages_above_limit () =
  test_output_limit_gen ~limit:10 ~message_number:15

(* Same as {test_outbox_limit_gen} but uses directly the host function and tests
   the error code. *)
let test_write_output_above_limit ~version () =
  let open Lwt_syntax in
  let input = Input_buffer.alloc () in
  let message_limit = Z.of_int 5 in
  let output =
    Output_buffer.alloc
      ~message_limit
      ~validity_period:Wasm_utils.default_outbox_validity_period
      ~last_level:(Some 0l)
  in

  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memories =
    Lazy_vector.Int32Vector.cons
      (Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}))
      module_inst.memories
  in
  let module_inst = {module_inst with memories} in
  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;

  let push_message expected_message_index =
    let values = Values.[Num (I32 50l); Num (I32 5l)] in
    let* _, result =
      Eval.invoke
        ~module_reg
        ~caller:module_key
        (Host_funcs.registry ~write_debug:Noop ~version)
        ~input
        ~output
        Host_funcs.Internal_for_tests.write_output
        values
    in
    let last_outbox_level = output.Output_buffer.last_level in
    let* last_outbox =
      match last_outbox_level with
      | Some level -> Output_buffer.Internal_for_tests.get_outbox output level
      | None ->
          Stdlib.failwith "The PVM output buffer does not contain any outbox."
    in
    let last_message_in_last_outbox =
      Output_buffer.Internal_for_tests.get_outbox_last_message_index last_outbox
    in

    if expected_message_index >= message_limit then (
      assert (result = Values.[Num (I32 Host_funcs.Error.(code Full_outbox))]) ;
      assert (last_message_in_last_outbox = Some (Z.pred message_limit)))
    else (
      assert (result = Values.[Num (I32 0l)]) ;
      assert (last_message_in_last_outbox = Some expected_message_index)) ;
    return_ok_unit
  in
  let l =
    Result.value ~default:[] (List.init ~when_negative_length:[] 10 Z.of_int)
  in
  List.iter_es push_message l

let test_push_output_bigger_than_max_size () =
  let open Lwt_syntax in
  let lim = Types.(MemoryType {min = 1l; max = Some 2l}) in
  let memory = Memory.alloc lim in
  let output_buffer = Eval.default_output_buffer () in
  let message_size = Host_funcs.Aux.input_output_max_size * 2 in
  assert (output_buffer.Output_buffer.last_level = Some 0l) ;
  let* outbox_level_0 =
    Output_buffer.Internal_for_tests.get_outbox output_buffer 0l
  in
  let num_messages_before =
    Output_buffer.Messages.num_elements outbox_level_0
  in
  let* result =
    Host_funcs.Aux.write_output
      ~output_buffer
      ~memory
      ~src:0l
      ~num_bytes:(Int32.of_int message_size)
  in
  assert (result = Host_funcs.Error.(code Input_output_too_large)) ;
  assert (output_buffer.Output_buffer.last_level = Some 0l) ;
  let* outbox_level_0 =
    Output_buffer.Internal_for_tests.get_outbox output_buffer 0l
  in
  assert (
    Z.equal
      num_messages_before
      (Output_buffer.Messages.num_elements outbox_level_0)) ;
  return_ok_unit

let tests =
  Tztest_helper.tztests_with_pvm
    ~versions:[V0; V1]
    [
      ("Host write", `Quick, test_write_host_fun);
      ( "Write_output: Push message above the limit",
        `Quick,
        test_write_output_above_limit );
    ]
  @ [
      tztest "Output buffer" `Quick test_output_buffer;
      tztest "Aux_write_output" `Quick test_aux_write_output;
      tztest "Push message below the limit" `Quick test_messages_below_limit;
      tztest "Push message at the limit" `Quick test_messages_at_limit;
      tztest "Push message above the limit" `Quick test_messages_above_limit;
      tztest
        "Write_output: push messages bigger than the protocol limit"
        `Quick
        test_push_output_bigger_than_max_size;
    ]
