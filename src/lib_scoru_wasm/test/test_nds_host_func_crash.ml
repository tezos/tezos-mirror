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
      (* [Wasm_utils.Config.config] is [Wasm_pvm_config.empty], which
         leaves [nds_host_functions_enabled] [false] at runtime. We
         match that here so the install/revert pattern mutates the
         same cached singleton the kernel evaluation will later
         dispatch through. *)
      (Host_funcs.registry
         ~version
         ~nds_host_functions_enabled:false
         ~write_debug:Noop)
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
      (Host_funcs.registry
         ~version
         ~nds_host_functions_enabled:false
         ~write_debug:Noop)
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

(** A kernel that imports and calls the real [nds_registry_resize]
    host function (available from V6). Unlike {!kernel}, nothing is
    hijacked in the registry: this exercises the genuine flag-off NDS
    implementation as dispatched by a single-state PVM. *)
let nds_flag_off_kernel =
  {|
(module
 (import "smart_rollup_core" "nds_registry_resize"
         (func $nds_registry_resize (param i64) (result i32)))
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
       (drop (call $nds_registry_resize (i64.const 2)))
       (nop))
 )
|}

(** Regression test for making the flag-off NDS path a plain
    [Host_func].  The default PVM runs with single-state
    ([Durable_only]) storage and the [Nds_host_functions] feature flag
    off.  Prior to the fix a disabled NDS host function was still
    registered as an [Nds_host_func], so calling it hit the
    [Durable_only] branch in [eval.ml] and raised
    [Eval.Nds_host_func_without_nds_storage] — which [next_tick_state]
    re-raises, crashing the PVM.  With the flag off the call must now
    degrade to a no-op (returning the disabled code) and leave the PVM
    running normally. *)
let test_nds_flag_off_does_not_crash_on_single_state ~version () =
  let open Lwt_syntax in
  let* tree = initial_state ~version nds_flag_off_kernel in
  let* tree = eval_until_input_requested tree in
  let* tree = set_empty_inbox_step 0l tree in
  let* crashed, final_tree =
    Lwt.catch
      (fun () ->
        let* tree = eval_until_input_or_reveal_requested tree in
        Lwt.return (false, Some tree))
      (function
        | Tezos_webassembly_interpreter.Eval.Nds_host_func_without_nds_storage
          ->
            Lwt.return (true, None)
        | exn -> Lwt.reraise exn)
  in
  assert (not crashed) ;
  (* The flag-off call degraded to a no-op, so the PVM must keep running
     normally rather than having been marked stuck. *)
  let* stuck_flag =
    match final_tree with
    | Some tree -> has_stuck_flag tree
    | None -> Lwt.return_true
  in
  assert (not stuck_flag) ;
  Lwt_result_syntax.return_unit

let tests =
  tztests_with_all_pvms
    [
      ( "NDS host function crashes process without NDS storage",
        `Quick,
        test_nds_host_func_crashes_process );
    ]

(* NDS host functions only exist from V6 onwards; on earlier versions
   the import would fail to link, so the flag-off regression is gated
   to V6+. *)
let v6_tests =
  tztests_with_pvm
    ~versions:Wasm_pvm_state.[V6; VExperimental]
    [
      ( "flag-off NDS host function does not crash single-state PVM",
        `Quick,
        test_nds_flag_off_does_not_crash_on_single_state );
    ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru wasm"
    [("NDS host function dispatch", tests @ v6_tests)]
  |> Lwt_main.run
