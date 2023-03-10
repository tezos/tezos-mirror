(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Testing
    -------
    Component:    Tree_encoding_decoding
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_wasm_pvm_encodings.ml
    Subject:      WASM PVM encoding tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_scoru_wasm
open Tezos_webassembly_interpreter
module Contest = Tezos_context_memory.Context_binary

let decode_state_gen =
  let open QCheck2.Gen in
  oneofl
    [
      Decode.Byte_vector_step;
      Instr_step;
      Instr_block_step;
      Block_step;
      Name_step;
      Func_type_step;
      Import_step;
      Export_step;
      Code_step;
      Elem_step;
      Data_step;
      Module_step;
    ]

let init_state_gen =
  let open QCheck2.Gen in
  oneofl
    [
      Eval.Init_step;
      Map_step;
      Map_concat_step;
      Join_step;
      Section_step;
      Eval_const;
      Create_global_step;
      Run_data_step;
    ]

let eval_state_gen =
  let open QCheck2.Gen in
  let invoke_step =
    let+ msg = string in
    Eval.Invoke_step msg
  in
  oneof
    [
      invoke_step;
      return Eval.Label_step;
      return Eval.Frame_step;
      return Eval.Eval_step;
    ]

let reveal_error_gen =
  let open QCheck2.Gen in
  let reveal_step = return Eval.Reveal_step in
  let reveal_hash_decoding =
    let+ raw = string in
    Eval.Reveal_hash_decoding raw
  in
  let reveal_payload_decoding =
    let+ raw = string in
    Eval.Reveal_payload_decoding raw
  in
  oneof [reveal_step; reveal_hash_decoding; reveal_payload_decoding]

let exn_gen =
  let open QCheck2.Gen in
  let open Tezos_lazy_containers in
  let value_type_error =
    let* i = int in
    let* num = Ast_generators.(value_op_gen int32 int64) in
    let+ num_type = Ast_generators.num_type_gen in
    Values.TypeError (i, num, num_type)
  in
  let decode_error =
    let+ msg = string in
    Binary_exn.Decode_error.Error (Source.no_region, msg)
  in
  let encode_error =
    let+ msg = string in
    Binary_exn.Encode_error.Error (Source.no_region, msg)
  in
  let decode_step_error =
    let+ state = decode_state_gen in
    Decode.Step_error state
  in
  let init_step_error =
    let+ state = init_state_gen in
    Eval.Init_step_error state
  in
  let eval_step_error =
    let+ state = eval_state_gen in
    Eval.Evaluation_step_error state
  in
  let reveal_error =
    let+ err = reveal_error_gen in
    Eval.Reveal_error err
  in
  let invalid =
    let+ msg = string in
    Valid.Invalid (Source.no_region, msg)
  in
  let link =
    let+ msg = string in
    Eval.Link (Source.no_region, msg)
  in
  let trap =
    let+ msg = string in
    Eval.Trap (Source.no_region, msg)
  in
  let crash =
    let+ msg = string in
    Eval.Crash (Source.no_region, msg)
  in
  let exhaustion =
    let+ msg = string in
    Eval.Exhaustion (Source.no_region, msg)
  in
  let import_unknown =
    let+ msg = string in
    Import.Unknown (Source.no_region, msg)
  in
  oneof
    [
      value_type_error;
      return Binary_exn.EOS;
      return Binary_exn.Utf8;
      decode_error;
      encode_error;
      decode_step_error;
      init_step_error;
      eval_step_error;
      reveal_error;
      return Lazy_map.UnexpectedAccess;
      return Lazy_vector.Bounds;
      return Lazy_vector.SizeOverflow;
      return Chunked_byte_vector.Bounds;
      return Chunked_byte_vector.SizeOverflow;
      invalid;
      return Table.Type;
      return Table.SizeLimit;
      return Table.OutOfMemory;
      return Table.Bounds;
      return Table.SizeOverflow;
      return Memory.Type;
      return Memory.SizeLimit;
      return Memory.OutOfMemory;
      return Memory.Bounds;
      return Memory.SizeOverflow;
      return Global.Type;
      return Global.NotMutable;
      link;
      trap;
      crash;
      exhaustion;
      return Ixx.Overflow;
      return Ixx.DivideByZero;
      return Ixx.InvalidConversion;
      return Input_buffer.Bounds;
      return Input_buffer.SizeOverflow;
      return Input_buffer.Cannot_store_an_earlier_message;
      return Input_buffer.Dequeue_from_empty_queue;
      import_unknown;
    ]

let error_state_gen =
  (* The generator produces only known interpreter errors. *)
  let extract_error = function
    | `Interpreter e -> e
    | `Unknown _ -> assert false
  in
  let open QCheck2.Gen in
  let truncated_string =
    let+ s = string in
    Wasm_pvm_errors.truncate_message s
  in
  let decode_error =
    let+ exn = exn_gen in
    let error =
      Wasm_pvm_errors.extract_interpreter_error exn |> extract_error
    in
    Wasm_pvm_errors.Decode_error error
  in
  let link_error =
    let+ error = truncated_string in
    Wasm_pvm_errors.Link_error error
  in
  let init_error =
    let+ exn = exn_gen in
    let error =
      Wasm_pvm_errors.extract_interpreter_error exn |> extract_error
    in
    Wasm_pvm_errors.Init_error error
  in
  let eval_error =
    let+ exn = exn_gen in
    let error =
      Wasm_pvm_errors.extract_interpreter_error exn |> extract_error
    in
    Wasm_pvm_errors.Eval_error error
  in
  let unknown_error =
    let+ err = truncated_string in
    Wasm_pvm_errors.Unknown_error err
  in
  let fallback_error =
    let decode_cause =
      let+ exn = exn_gen in
      let error =
        Wasm_pvm_errors.extract_interpreter_error exn |> extract_error
      in
      Wasm_pvm_errors.Decode_cause error
    in
    let link_cause =
      let+ error = truncated_string in
      Wasm_pvm_errors.Link_cause error
    in
    let init_cause =
      let+ exn = exn_gen in
      let error =
        Wasm_pvm_errors.extract_interpreter_error exn |> extract_error
      in
      Wasm_pvm_errors.Init_cause error
    in
    let+ cause = oneof [decode_cause; link_cause; init_cause] in
    Wasm_pvm_errors.No_fallback_kernel cause
  in

  let invalid_state =
    let+ err = truncated_string in
    Wasm_pvm_errors.Invalid_state err
  in
  oneof
    [
      decode_error;
      link_error;
      init_error;
      eval_error;
      invalid_state;
      unknown_error;
      fallback_error;
    ]

let check_interpreter_error error error' =
  error.Wasm_pvm_errors.raw_exception = error'.Wasm_pvm_errors.raw_exception
  && error.explanation = error'.explanation

let fallback_cause_check cause cause' =
  match (cause, cause') with
  | Wasm_pvm_errors.Decode_cause error, Wasm_pvm_errors.Decode_cause error'
  | Init_cause error, Init_cause error' ->
      Lwt.return_ok (check_interpreter_error error error')
  | Link_cause error, Link_cause error' -> Lwt.return_ok (error = error')
  | _, _ -> Lwt.return_ok false

let error_state_check state state' =
  match (state, state') with
  | Wasm_pvm_errors.Decode_error error, Wasm_pvm_errors.Decode_error error'
  | Init_error error, Init_error error'
  | Eval_error error, Eval_error error' ->
      Lwt.return_ok (check_interpreter_error error error')
  | Link_error err, Link_error err'
  | Invalid_state err, Invalid_state err'
  | Unknown_error err, Unknown_error err' ->
      Lwt.return_ok (err = err')
  | Too_many_ticks, Too_many_ticks -> Lwt.return_ok true
  | No_fallback_kernel cause, No_fallback_kernel cause' ->
      fallback_cause_check cause cause'
  | _, _ -> Lwt.return_ok false

let tests =
  [
    tztest
      "Wasm_pvm_errors"
      `Quick
      (Test_encodings_util.make_test
         ~print:Wasm_utils.print_error_state
         (Tezos_tree_encoding.value [] Wasm_pvm_errors.encoding)
         error_state_gen
         error_state_check);
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru wasm"
    [("WASM PVM Encodings", tests)]
  |> Lwt_main.run
