(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_scoru_wasm NDS host function dispatch
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- \
                  --file test_nds_host_func_crash.ml
    Subject:      Verify that dispatching an NDS host function without NDS
                  storage raises [Eval.Nds_host_func_without_nds_storage]
                  rather than silently degrading to a stuck PVM.
*)

open Tezos_scoru_wasm
open Wasm_utils
open Tztest_helper

(** Replace [store_has] in the registry with a dummy [Nds_host_func].
    Returns [(install, revert)] functions to swap and restore the
    registry entry, following the pattern from
    {!Test_host_functions_ticks}. *)
let hijack_store_has ~version =
  let global_name =
    match Host_funcs.Internal_for_tests.store_has with
    | Tezos_webassembly_interpreter.Func.HostFunc (_, name) -> name
    | _ -> Stdlib.failwith "store_has is not a host function"
  in
  let original =
    Tezos_webassembly_interpreter.Host_funcs.lookup
      ~global_name
      (Host_funcs.registry ~version ~write_debug:Noop)
  in
  let nds_implem =
    Tezos_webassembly_interpreter.Host_funcs.Nds_host_func
      (fun _input _output nds _memories _args ->
        Lwt.return
          (nds, [Tezos_webassembly_interpreter.Values.(Num (I32 0l))], Z.one))
  in
  let register impl () =
    Tezos_webassembly_interpreter.Host_funcs.register
      ~global_name
      impl
      (Host_funcs.registry ~version ~write_debug:Noop)
  in
  (register nds_implem, register original)

(** A kernel that imports and calls [store_has].  When the registry
    entry for [store_has] has been replaced by an [Nds_host_func],
    the eval dispatch in [eval.ml] hits the [Durable_only] branch and
    raises [Eval.Nds_host_func_without_nds_storage]. *)
let kernel =
  {|
(module
 (import "smart_rollup_core" "store_has"
         (func $store_has (param i32 i32) (result i32)))
 (data (i32.const 100) "/test/key")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
       (drop (call $store_has (i32.const 100) (i32.const 9)))
       (nop))
 )
|}

(** Dispatching an [Nds_host_func] with [Durable_only] storage raises
    [Eval.Nds_host_func_without_nds_storage].  The exception is
    re-raised by [next_tick_state] so it crashes the process instead of
    being silently converted to a stuck PVM. *)
let test_nds_host_func_crashes_process ~version () =
  let open Lwt_syntax in
  let install, revert = hijack_store_has ~version in
  install () ;
  let* tree = initial_state ~version kernel in
  let* tree = eval_until_input_requested tree in
  let* tree = set_empty_inbox_step 0l tree in
  let* raised =
    Lwt.catch
      (fun () ->
        let* _tree = eval_until_input_or_reveal_requested tree in
        Lwt.return_false)
      (function
        | Tezos_webassembly_interpreter.Eval.Nds_host_func_without_nds_storage
          ->
            Lwt.return_true
        | exn -> Lwt.reraise exn)
  in
  revert () ;
  assert raised ;
  Lwt_result_syntax.return_unit

let tests =
  tztests_with_all_pvms
    [
      ( "NDS host function crashes process without NDS storage",
        `Quick,
        test_nds_host_func_crashes_process );
    ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru wasm"
    [("NDS host function dispatch", tests)]
  |> Lwt_main.run
