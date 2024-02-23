(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs  <contact@nomadic-labs.com>               *)
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
    Component:    Lib_scoru_wasm protocol migration internal message
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_protocol_migration.ml
    Subject:      Protocol migration tests for the tezos-scoru-wasm library
*)

open Tztest
open Wasm_utils

let noop_module =
  {|
  (module
    (memory 1)
    (export "mem"(memory 0))
    (func (export "kernel_run")
      nop))
|}

let test_protocol_migration_message ~from_version ~to_version
    ~after_protocol_activation:protocol () =
  let open Lwt_syntax in
  let* tree = initial_tree ~version:from_version noop_module in
  let* tree = eval_until_input_requested tree in
  let* version = Wasm.get_wasm_version tree in
  assert (version = from_version) ;
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = eval_until_input_requested tree in
  let* version = Wasm.get_wasm_version tree in
  assert (version = from_version) ;
  let* tree = set_empty_inbox_step ~migrate_to:protocol 0l tree in
  let* tree = eval_until_input_requested tree in
  let* version = Wasm.get_wasm_version tree in
  assert (version = to_version) ;
  Lwt_result_syntax.return_unit

let proto_name : Tezos_scoru_wasm.Pvm_input_kind.protocol -> string = function
  | Paris -> "Paris"
  | Oxford -> "Oxford"
  | Proto_alpha -> "Proto_alpha"

let tests =
  List.map
    (fun (from_version, to_version, protocol) ->
      (* This pattern match is introduced in order to not typecheck if
         a new version is added.
         If you end up here because of this, please add a protocol migration
         test for you new version. *)
      (match from_version with
      | Tezos_scoru_wasm.Wasm_pvm_state.V0 | V1 | V2 | V3 | V4 -> ()) ;
      tztest
        (sf
           "protocol migration message handling by the WASM PVM (%s)"
           (proto_name protocol))
        `Quick
        (test_protocol_migration_message
           ~from_version
           ~to_version
           ~after_protocol_activation:protocol))
    [(V3, V4, Paris); (V1, V2, Oxford); (V3, V4, Proto_alpha)]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru wasm"
    [("Protocol migration", tests)]
  |> Lwt_main.run
