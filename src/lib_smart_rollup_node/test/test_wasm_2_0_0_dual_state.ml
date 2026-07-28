(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Smart rollup node, dual-storage WASM PVM context backend
    Invocation:   dune exec src/lib_smart_rollup_node/test/main.exe \
                  -- --file test_wasm_2_0_0_dual_state.ml
    Subject:      Safety properties of {!Wasm_2_0_0_dual_state.Dual_context}
                  (Irmin + rocksdb NDS): Active-state commit/checkout
                  roundtrips, the persist-before-commit ordering (including
                  across a restart), snapshot independence at the context
                  boundary, checkout-handle independence, and the Inactive
                  lifecycle. *)

module D = Wasm_2_0_0_dual_state.Dual_state
module DC = Wasm_2_0_0_dual_state.Dual_context
module Nds = Octez_riscv_nds_common.Nds

let load_ctxt dir =
  DC.load
    ~cache_size:Configuration.default_irmin_cache_size
    Read_write
    (Configuration.default_context_dir dir)

let nds_handle_exn ~__LOC__:loc (s : D.state) =
  match s with
  | {nds = D.Active nds; _} -> nds
  | {nds = D.Inactive; _} ->
      Stdlib.failwith (loc ^ ": expected an Active dual state")

let resize_exn nds n =
  match Nds.resize nds n with
  | Ok () -> ()
  | Error _ -> Stdlib.failwith "failed to resize the NDS registry"

let check_hash ~__LOC__:loc msg expected actual =
  if not (Bytes.equal expected actual) then
    Format.kasprintf
      Stdlib.failwith
      "%s: %s: expected %a, got %a"
      loc
      msg
      Hex.pp
      (Hex.of_bytes expected)
      Hex.pp
      (Hex.of_bytes actual)

(** Build a mutable PVM state whose tree carries the [/pvm/nds_hash] marker
    of a fresh empty handle backed by [index]'s rocksdb repository — the
    shape the dual-state encoder produces right after NDS activation. *)
let make_active_pvm_state index =
  let open Lwt_syntax in
  let nds = Wasm_2_0_0_dual_state.make_empty_nds index () in
  let m = DC.PVMState.empty () in
  let ({irmin = tree; _} : D.state) = D.read m in
  let* tree =
    Irmin_context.Tree.add tree D.nds_hash_path (Nds.registry_hash nds)
  in
  D.write m {irmin = tree; nds = D.Active nds} ;
  return (m, nds)

(** Active-state lifecycle: [PVMState.set] then [commit] persists both
    halves; [checkout] reconstructs an [Active] handle whose registry hash
    equals the marker.  The restored handle is independent (a fresh rocksdb
    checkout), and — persist-before-commit — the registry survives closing
    and reloading the context (restart simulation), even after the restored
    handle was mutated and abandoned in between. *)
let test_active_roundtrip_and_restart data_dir =
  let open Lwt_result_syntax in
  let* index = load_ctxt data_dir in
  let ctxt = DC.empty index in
  let*! m, nds0 = make_active_pvm_state index in
  let h0 = Nds.registry_hash nds0 in
  let*! () = DC.PVMState.set ctxt m in
  let*! hash = DC.commit ctxt in
  (* Same-process checkout. *)
  let*! ctxt' = DC.checkout index hash in
  let ctxt' =
    match ctxt' with
    | Some c -> c
    | None -> Stdlib.failwith "checkout of the committed hash failed"
  in
  let nds' = nds_handle_exn ~__LOC__ (D.read ctxt'.Context_sigs.state) in
  check_hash
    ~__LOC__
    "restored handle disagrees with the committed registry"
    h0
    (Nds.registry_hash nds') ;
  (* The restored handle is a fresh checkout: mutating and abandoning it
     must not damage what later checkouts restore. *)
  resize_exn nds' 1L ;
  (* Restart simulation: close everything, reload from disk. *)
  let*! () = DC.close index in
  let* index2 = load_ctxt data_dir in
  let*! ctxt2 = DC.checkout index2 hash in
  let ctxt2 =
    match ctxt2 with
    | Some c -> c
    | None -> Stdlib.failwith "checkout after restart failed"
  in
  let nds2 = nds_handle_exn ~__LOC__ (D.read ctxt2.Context_sigs.state) in
  check_hash
    ~__LOC__
    "registry restored after restart disagrees with the marker"
    h0
    (Nds.registry_hash nds2) ;
  let*! () = DC.close index2 in
  return_unit

(** Inactive lifecycle: an empty context stays pre-activation through
    commit and checkout — no marker, no handle, no PVM state. *)
let test_inactive_lifecycle data_dir =
  let open Lwt_result_syntax in
  let* index = load_ctxt data_dir in
  let ctxt = DC.empty index in
  (* Seed a non-empty pre-activation PVM state: irmin-pack refuses to
     persist a completely empty tree. *)
  let m = DC.PVMState.empty () in
  let ({irmin = tree; _} : D.state) = D.read m in
  let*! tree = Irmin_context.Tree.add tree ["seed"] (Bytes.of_string "1") in
  D.write m {irmin = tree; nds = D.Inactive} ;
  let*! () = DC.PVMState.set ctxt m in
  let*! hash = DC.commit ctxt in
  let*! ctxt' = DC.checkout index hash in
  let ctxt' =
    match ctxt' with
    | Some c -> c
    | None -> Stdlib.failwith "checkout of the empty commit failed"
  in
  (match D.read ctxt'.Context_sigs.state with
  | {nds = D.Inactive; _} -> ()
  | {nds = D.Active _; _} ->
      Stdlib.failwith "pre-activation context restored an Active NDS handle") ;
  let*! pvm_state = DC.PVMState.find ctxt' in
  (match pvm_state with
  | Some m' -> (
      match D.read m' with
      | {nds = D.Inactive; _} -> ()
      | {nds = D.Active _; _} ->
          Stdlib.failwith "pre-activation PVM state carries an Active handle")
  | None -> Stdlib.failwith "the seeded PVM state was not restored") ;
  let*! () = DC.close index in
  return_unit

(** Context-boundary snapshot independence: [Dual_context.to_imm] (the
    conversion the generic [PVMState.imm_copy]/[copy] machinery uses for the
    interpreter's dissection cache) severs the NDS handle — mutating the
    live handle afterwards leaves the snapshot at its capture-time hash.
    Guards the wiring: [Dual_context.to_imm] regressing to an aliasing
    dereference would pass every other test until a refutation game. *)
let test_to_imm_severs_at_context_boundary data_dir =
  let open Lwt_result_syntax in
  let* index = load_ctxt data_dir in
  let*! m, nds0 = make_active_pvm_state index in
  let h0 = Nds.registry_hash nds0 in
  let snapshot = DC.to_imm m in
  resize_exn (nds_handle_exn ~__LOC__ (D.read m)) 1L ;
  check_hash
    ~__LOC__
    "mutating the live handle changed the to_imm snapshot"
    h0
    (Nds.registry_hash (nds_handle_exn ~__LOC__ snapshot)) ;
  (* Sanity: the live handle did change. *)
  if Bytes.equal (Nds.registry_hash nds0) h0 then
    Stdlib.failwith "sanity: resize should have changed the live hash" ;
  let*! () = DC.close index in
  return_unit

(** Checkout-handle independence ([PVMState.find]): every [find] restores a
    fresh handle from rocksdb, so mutating and abandoning one (a failed
    evaluation attempt) leaves both the context's own state and later
    [find]s untouched — the daemon can always retry from a clean handle. *)
let test_find_yields_independent_handles data_dir =
  let open Lwt_result_syntax in
  let* index = load_ctxt data_dir in
  let ctxt = DC.empty index in
  let*! m, nds0 = make_active_pvm_state index in
  let h0 = Nds.registry_hash nds0 in
  let*! () = DC.PVMState.set ctxt m in
  let*! _hash = DC.commit ctxt in
  let find_exn () =
    let open Lwt_syntax in
    let+ found = DC.PVMState.find ctxt in
    match found with
    | Some m -> m
    | None -> Stdlib.failwith "PVMState.find returned None"
  in
  let*! m1 = find_exn () in
  (* Failed-evaluation simulation: mutate and abandon. *)
  resize_exn (nds_handle_exn ~__LOC__ (D.read m1)) 1L ;
  let*! m2 = find_exn () in
  check_hash
    ~__LOC__
    "a fresh find was polluted by a previously mutated find"
    h0
    (Nds.registry_hash (nds_handle_exn ~__LOC__ (D.read m2))) ;
  check_hash
    ~__LOC__
    "the context's own handle was polluted by a mutated find"
    h0
    (Nds.registry_hash
       (nds_handle_exn ~__LOC__ (D.read ctxt.Context_sigs.state))) ;
  let*! () = DC.close index in
  return_unit

(** Full evaluation cycle: mutate the live handle (as kernel execution
    does), re-stamp the marker (as the encoder does), [set], [commit],
    [checkout] — the restored handle carries the mutation.  The earlier
    commit still restores the pre-mutation registry: rocksdb keeps both. *)
let test_eval_cycle_roundtrip data_dir =
  let open Lwt_result_syntax in
  let* index = load_ctxt data_dir in
  let ctxt = DC.empty index in
  let*! m, nds0 = make_active_pvm_state index in
  let h0 = Nds.registry_hash nds0 in
  let*! () = DC.PVMState.set ctxt m in
  let*! hash1 = DC.commit ctxt in
  (* One evaluation cycle on the live handle. *)
  resize_exn (nds_handle_exn ~__LOC__ (D.read m)) 1L ;
  let h1 = Nds.registry_hash (nds_handle_exn ~__LOC__ (D.read m)) in
  if Bytes.equal h1 h0 then
    Stdlib.failwith "sanity: the evaluation should have changed the registry" ;
  (* Re-stamp the marker, as the dual-state encoder does on encode. *)
  let ({irmin = tree; nds = tag} : D.state) = D.read m in
  let*! tree = Irmin_context.Tree.add tree D.nds_hash_path h1 in
  D.write m {irmin = tree; nds = tag} ;
  let*! () = DC.PVMState.set ctxt m in
  let*! hash2 = DC.commit ctxt in
  let checkout_registry_hash hash =
    let open Lwt_syntax in
    let+ ctxt = DC.checkout index hash in
    match ctxt with
    | Some c ->
        Nds.registry_hash
          (nds_handle_exn ~__LOC__ (D.read c.Context_sigs.state))
    | None -> Stdlib.failwith "checkout failed"
  in
  let*! restored2 = checkout_registry_hash hash2 in
  check_hash
    ~__LOC__
    "the post-evaluation commit does not restore the mutated registry"
    h1
    restored2 ;
  let*! restored1 = checkout_registry_hash hash1 in
  check_hash
    ~__LOC__
    "the pre-evaluation commit no longer restores the original registry"
    h0
    restored1 ;
  let*! () = DC.close index in
  return_unit

(* Adapted from test_context_gc.ml. *)
let wrap n f =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      Lwt_utils_unix.with_tempdir "dual_context_test_" (fun dir ->
          let open Lwt_syntax in
          let* r = f dir in
          match r with
          | Ok () -> Lwt.return_unit
          | Error error ->
              Format.kasprintf Stdlib.failwith "%a" pp_print_trace error))

let tests =
  [
    wrap
      "Active state commit/checkout roundtrip + restart"
      test_active_roundtrip_and_restart;
    wrap "Inactive lifecycle stays pre-activation" test_inactive_lifecycle;
    wrap
      "to_imm severs the NDS handle at the context boundary"
      test_to_imm_severs_at_context_boundary;
    wrap
      "PVMState.find yields independent handles"
      test_find_yields_independent_handles;
    wrap
      "evaluation cycle roundtrip preserves both commits"
      test_eval_cycle_roundtrip;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "lib_smart_rollup_node" [("Dual_context", tests)]
  |> Lwt_main.run
