(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_scoru_wasm NDS activation
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_nds_activation.ml
    Subject:      Unit tests for the kernel-driven NDS activation
                  trigger (sentinel at /kernel/env/activate_nds, observed
                  at the reboot boundary).
*)

open Tztest
open Tezos_scoru_wasm
open Wasm_utils

(* -------------------------------------------------------------------- *)
(* Helpers                                                              *)
(* -------------------------------------------------------------------- *)

(** A {!Wasm_vm.Make_vm} instantiation with the [Nds_host_functions]
    feature enabled at activation level [0l] — so any
    [current_level > 0l] sees the gate open and exercises the
    activation path.  The factory is the canonical in-memory NDS
    builder. *)
module Vm = Tezos_scoru_wasm.Wasm_vm.Make_vm (struct
  let config = Wasm_pvm_config.of_signals [("nds_host_functions", 0l)]

  let make_empty_nds = Some Octez_riscv_nds_memory.make_empty_normal_nds
end)

(** A {!Wasm_vm.Make_vm} instantiation with the [Nds_host_functions]
    gate open but no factory provided. Used to exercise the failure
    path of {!Wasm_vm.maybe_activate_nds} when an instantiation
    statically promised the gate would stay closed but the kernel
    nevertheless reaches the activation boundary. *)
module Vm_no_factory = Tezos_scoru_wasm.Wasm_vm.Make_vm (struct
  let config = Wasm_pvm_config.of_signals [("nds_host_functions", 0l)]

  let make_empty_nds = None
end)

(** Default [current_level] for tests that want the gate open. *)
let current_level_open = 1l

(** A [current_level] equal to the activation level: the gate is
    {b closed} at this level (activation is for strictly later levels). *)
let current_level_closed = 0l

(** A PVM version at which the NDS host functions exist (V6+). *)
let version_supported = Wasm_pvm_state.V6

(** A PVM version predating the NDS host functions: activation must not
    fire there even with the feature flag on and the sentinel set. *)
let version_unsupported = Wasm_pvm_state.V5

(** Build a {!Durable_only} storage backed by the given key-value
    pairs in the durable tree. *)
let make_durable_only kvs =
  let open Lwt.Syntax in
  let+ durable = make_durable kvs in
  Wasm_pvm_state.Internal_state.Durable_only
    {durable = Durable.of_storage_exn durable}

(** Set the NDS activation sentinel at [/kernel/env/activate_nds]. *)
let set_activation_sentinel storage =
  let open Lwt.Syntax in
  let durable = Wasm_pvm_state.Internal_state.durable_of storage in
  let+ durable =
    Durable.write_value_exn durable Constants.activate_nds_flag_key 0L ""
  in
  Wasm_pvm_state.Internal_state.update_durable storage durable

let has_activation_sentinel storage =
  let durable = Wasm_pvm_state.Internal_state.durable_of storage in
  let open Lwt.Syntax in
  let+ flag = Durable.find_value durable Constants.activate_nds_flag_key in
  Option.is_some flag

let is_dual = function
  | Wasm_pvm_state.Internal_state.Dual _ -> true
  | Durable_only _ -> false

(* -------------------------------------------------------------------- *)
(* Tests                                                                *)
(* -------------------------------------------------------------------- *)

(* Sentinel set + Durable_only + gate open ⇒ activate to Dual; the
   sentinel is left intact (never read again once Dual). *)
let test_activation_happy_path () =
  let open Lwt_result_syntax in
  let*! storage = make_durable_only [] in
  let*! storage = set_activation_sentinel storage in
  let*! storage =
    Vm.maybe_activate_nds
      ~version:version_supported
      ~current_level:current_level_open
      storage
  in
  assert (is_dual storage) ;
  (* Sentinel preserved: activation does not consume it. *)
  let*! still_set = has_activation_sentinel storage in
  assert still_set ;
  return_unit

(* Sentinel absent ⇒ no-op (storage remains Durable_only). *)
let test_no_sentinel_no_activation () =
  let open Lwt_result_syntax in
  let*! storage = make_durable_only [] in
  let*! storage =
    Vm.maybe_activate_nds
      ~version:version_supported
      ~current_level:current_level_open
      storage
  in
  assert (not (is_dual storage)) ;
  return_unit

(* Sentinel set + gate closed ⇒ no-op; sentinel preserved so a future
   PVM with the gate open can still observe the kernel's request. *)
let test_gate_closed_preserves_sentinel () =
  let open Lwt_result_syntax in
  let*! storage = make_durable_only [] in
  let*! storage = set_activation_sentinel storage in
  let*! storage =
    Vm.maybe_activate_nds
      ~version:version_supported
      ~current_level:current_level_closed
      storage
  in
  assert (not (is_dual storage)) ;
  let*! still_set = has_activation_sentinel storage in
  assert still_set ;
  return_unit

(* Sentinel set + storage already Dual ⇒ pure no-op: the existing NDS
   handle is preserved and the sentinel is left intact. *)
let test_noop_when_already_dual () =
  let open Lwt_result_syntax in
  let*! storage = make_durable_only [] in
  let storage =
    let open Wasm_pvm_state.Internal_state in
    match storage with
    | Durable_only {durable} ->
        let registry = Octez_riscv_nds_memory.Normal.Registry.create () in
        (match Octez_riscv_nds_memory.Normal.Registry.resize registry 1L with
        | Ok () -> ()
        | Error _ -> Stdlib.failwith "resize failed") ;
        let nds =
          Octez_riscv_nds_common.Nds.wrap
            Octez_riscv_nds_memory.Normal_tag
            (module Octez_riscv_nds_memory.Normal)
            registry
        in
        Dual {durable; nds}
    | Dual _ -> storage
  in
  let pre_size =
    match storage with
    | Wasm_pvm_state.Internal_state.Dual {nds; _} ->
        Octez_riscv_nds_common.Nds.size nds
    | Durable_only _ -> Stdlib.failwith "expected Dual"
  in
  assert (Int64.equal pre_size 1L) ;
  let*! storage = set_activation_sentinel storage in
  let*! storage =
    Vm.maybe_activate_nds
      ~version:version_supported
      ~current_level:current_level_open
      storage
  in
  assert (is_dual storage) ;
  (* Sentinel intact: PVM does not consume it when storage is Dual. *)
  let*! still_set = has_activation_sentinel storage in
  assert still_set ;
  let post_size =
    match storage with
    | Wasm_pvm_state.Internal_state.Dual {nds; _} ->
        Octez_riscv_nds_common.Nds.size nds
    | Durable_only _ -> Stdlib.failwith "expected Dual after activation"
  in
  assert (Int64.equal post_size 1L) ;
  return_unit

(* Sentinel set + gate open + [make_empty_nds = None] ⇒ raises.
   Guards against silently dropping a kernel activation request when
   an instantiation promised the gate would stay closed. *)
let test_missing_factory_when_gate_open () =
  let open Lwt_result_syntax in
  let*! storage = make_durable_only [] in
  let*! storage = set_activation_sentinel storage in
  let*! caught =
    Lwt.catch
      (fun () ->
        let*! _ =
          Vm_no_factory.maybe_activate_nds
            ~version:version_supported
            ~current_level:current_level_open
            storage
        in
        Lwt.return_false)
      (function Failure _ -> Lwt.return_true | exn -> Lwt.reraise exn)
  in
  assert caught ;
  return_unit

(* The activated NDS starts empty: registry size is 0. *)
let test_activated_nds_is_empty () =
  let open Lwt_result_syntax in
  let*! storage = make_durable_only [] in
  let*! storage = set_activation_sentinel storage in
  let*! storage =
    Vm.maybe_activate_nds
      ~version:version_supported
      ~current_level:current_level_open
      storage
  in
  (match storage with
  | Wasm_pvm_state.Internal_state.Dual {nds; _} ->
      assert (Int64.equal (Octez_riscv_nds_common.Nds.size nds) 0L)
  | Durable_only _ -> Stdlib.failwith "expected Dual storage after activation") ;
  return_unit

(* Sentinel set + feature flag on, but PVM version predates the NDS
   host functions (< V6) ⇒ no-op; sentinel preserved so a later PVM at
   V6+ can still observe the request.  The NDS host functions are not
   importable below V6, so activating there would build an NDS that
   nothing can use. *)
let test_version_too_low_no_activation () =
  let open Lwt_result_syntax in
  let*! storage = make_durable_only [] in
  let*! storage = set_activation_sentinel storage in
  let*! storage =
    Vm.maybe_activate_nds
      ~version:version_unsupported
      ~current_level:current_level_open
      storage
  in
  assert (not (is_dual storage)) ;
  let*! still_set = has_activation_sentinel storage in
  assert still_set ;
  return_unit

let tests =
  [
    tztest
      "activation: sentinel + Durable_only + gate open -> Dual + sentinel \
       preserved"
      `Quick
      test_activation_happy_path;
    tztest
      "version < V6: sentinel preserved, storage unchanged"
      `Quick
      test_version_too_low_no_activation;
    tztest
      "no-op: sentinel absent leaves storage Durable_only"
      `Quick
      test_no_sentinel_no_activation;
    tztest
      "gate closed: sentinel preserved, storage unchanged"
      `Quick
      test_gate_closed_preserves_sentinel;
    tztest
      "no-op when already Dual: sentinel and NDS both intact"
      `Quick
      test_noop_when_already_dual;
    tztest
      "missing factory + sentinel + gate open -> raises"
      `Quick
      test_missing_factory_when_gate_open;
    tztest
      "activated NDS is empty (registry size = 0)"
      `Quick
      test_activated_nds_is_empty;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru wasm"
    [("NDS activation trigger", tests)]
  |> Lwt_main.run
