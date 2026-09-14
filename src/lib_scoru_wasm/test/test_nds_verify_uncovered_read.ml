(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_scoru_wasm NDS Verify-mode tick
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- \
                  --file test_nds_verify_uncovered_read.ml
    Subject:      A Verify-mode NDS read the proof leaves uncovered aborts
                  the tick instead of committing a [Stuck] state.
*)

open Tezos_scoru_wasm
open Tztest_helper
module Nds = Octez_riscv_nds_common.Nds
module Nds_errors = Octez_riscv_nds_common.Nds_errors
module Memory_nds = Octez_riscv_nds_memory

(** A Verify-mode handle over a proof that records a populated
    one-database registry but not the path to the key the kernel reads.

    Every part of that shape is load-bearing. The registry has to exist,
    or [nds_store_read] answers [Nds_database_out_of_bounds] from the
    index check alone and never consults the proof. The key has to be
    present in the database, or its absence is provable from the root the
    proof does carry and the read is an honest "not found" rather than a
    divergence. And the proving session must not touch it, so its path
    stays out of the proof. *)
let uncovered_verify_nds () =
  let normal = Memory_nds.Normal.Registry.create () in
  (match Memory_nds.Normal.Registry.resize normal 1L with
  | Ok () -> ()
  | Error _ -> Stdlib.failwith "resize failed") ;
  let set key value =
    match
      Memory_nds.Normal.Database.set
        normal
        ~db_index:0L
        ~key:(Bytes.of_string key)
        ~value:(Bytes.of_string value)
    with
    | Ok () -> ()
    | Error _ -> Stdlib.failwith ("set failed for " ^ key)
  in
  set "/k0" "v0" ;
  set "/k1" "v1" ;
  (* The proving session reads the sibling only. That records the path
     to [/k1] and leaves the subtree covering [/k0] blinded, which is
     what a later read of [/k0] diverges against. A session that
     recorded nothing would not produce a blinded view at all. *)
  let prove = Memory_nds.Prove.start_proof normal in
  (match
     Memory_nds.Prove.Database.read
       prove
       ~db_index:0L
       ~key:(Bytes.of_string "/k1")
       ~offset:0L
       ~len:2L
   with
  | Ok _ -> ()
  | Error _ -> Stdlib.failwith "prove-mode read of the sibling failed") ;
  let proof = Memory_nds.Prove.produce_proof prove in
  Nds.wrap
    Memory_nds.Verify_tag
    (module Memory_nds.Verify)
    (Memory_nds.Verify.start_verify proof)

module Vm = Wasm_vm.Make_vm (struct
  (* Spelled out rather than taken from [Wasm_pvm_config]: the constant
     is not exposed by its interface here, as {!Test_nds_activation}
     also has to do. *)
  let config = Wasm_pvm_config.of_signals [("nds_host_functions", 0l)]

  (* The storage this test steps is built by hand, so the activation
     boundary is never reached and no factory is needed. *)
  let make_empty_nds = None
end)

module Ctx = Wasm_utils.In_memory_context
module State = Tree_state.Make (Ctx.Tree)
module Machine = Wasm_pvm.Make_machine (Vm) (State)

(* The fast machine is only used by the [eval_fast] helpers, which this
   test does not call, so the slow one stands in for both. *)
module Utils = Wasm_utils_functor.Make (Ctx) (Machine) (Machine)

(** A kernel whose [kernel_run] performs one NDS read. *)
let kernel =
  {|
(module
 (import "smart_rollup_core" "nds_store_read"
         (func $nds_store_read (param i64 i32 i32 i64 i32 i32) (result i32)))
 (data (i32.const 300) "/k0")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
   (drop (call $nds_store_read (i64.const 0) (i32.const 300) (i32.const 3)
                               (i64.const 0) (i32.const 600)
                               (i32.const 2)))))
|}

(** Replaying an NDS read the proof does not cover must abort the tick.

    Committing it as a [Stuck] state would make an uncovered read a
    valid transition: a refutation opponent could then prove an
    arbitrary [Stuck] by blinding the subtree the kernel reads, so the
    exception has to leave [next_tick_state] for the proof-replay
    boundary to reject the proof.

    The dual storage is installed on a decoded [pvm_state] and the level
    is stepped in memory rather than through the tree helpers. A
    [Verify] handle is a live proof-replay session, which is how
    production uses it too: {!Octez_riscv_nds_memory.Verify} exists only
    for the duration of a [verify_proof] replay. Driving the tree
    instead would run [kernel_run] inside the input step, before any
    handle could be installed on the state it reads. *)
let test_uncovered_read_aborts_the_tick ~version () =
  let open Lwt_syntax in
  let open Wasm_pvm_state.Internal_state in
  (* A short snapshot period keeps the [Padding] stretch ahead of
     [kernel_run] inside the step bound below. *)
  let* tree = Utils.initial_state ~version ~ticks_per_snapshot:5_000L kernel in
  (* Boot only: stop with the kernel linked and an input pending, before
     any [kernel_run]. *)
  let* tree = Utils.eval_until_input_requested tree in
  let* pvm_state = State.Encoding_runner.decode tree in
  let pvm_state =
    {
      pvm_state with
      storage =
        Dual
          {
            durable = durable_of pvm_state.storage;
            nds = uncovered_verify_nds ();
          };
    }
  in
  (* The level is driven on the [pvm_state] itself, not on the tree: the
     storage has to already carry the Verify handle when [kernel_run]
     executes, and a tree-level input step runs the kernel before any
     patched storage could be installed. Level 1, since the feature is
     recorded at level 0 and the gate opens strictly above it. *)
  let input_info level message_counter =
    Wasm_pvm_state.
      {
        inbox_level =
          Option.value_f ~default:(fun () -> assert false)
          @@ Tezos_base.Bounded.Non_negative_int32.of_value level;
        message_counter;
      }
  in
  let internal kind =
    Pvm_input_kind.(Internal_for_tests.to_binary_input (Internal kind) None)
  in
  (* Step until the read answers, one way or another. The bound only
     stops a kernel that never reaches the read from hanging. *)
  let rec step pvm_state remaining =
    if remaining <= 0 then Lwt.return `Never_read
    else
      match pvm_state.tick_state with
      | Stuck error -> Lwt.return (`Stuck error)
      (* [Collect] is the next level asking for input, so the whole
         [kernel_run] is behind us. [Padding] and [Snapshot] are on the
         way to it, not the end of it. *)
      | Collect -> Lwt.return `Ran_to_completion
      | Padding | Snapshot | Decode _ | Link _ | Init _ | Eval _ ->
          let* pvm_state =
            Vm.compute_step
              ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
              pvm_state
          in
          step pvm_state (remaining - 1)
  in
  let* outcome =
    Lwt.catch
      (fun () ->
        let* pvm_state =
          Vm.set_input_step
            (input_info 1l Z.zero)
            (internal Start_of_level)
            pvm_state
        in
        let* pvm_state =
          Vm.set_input_step
            (input_info 1l Z.one)
            (internal End_of_level)
            pvm_state
        in
        step pvm_state 500_000)
      (function
        | Nds_errors.Verification_failed _ -> Lwt.return `Aborted
        | exn -> Lwt.reraise exn)
  in
  (match outcome with
  | `Aborted -> ()
  | `Stuck error ->
      Test.fail
        ~__LOC__
        "The uncovered NDS read was committed as a [Stuck] state (%s). A proof \
         blinding the subtree the kernel reads would then verify, letting an \
         opponent prove an arbitrary [Stuck]."
        (Utils.print_error_state error)
  | `Ran_to_completion ->
      Test.fail
        ~__LOC__
        "The kernel ran to completion, so the read answered normally instead \
         of diverging: the handle under it is not replaying the proof this \
         test installs."
  | `Never_read ->
      Test.fail
        ~__LOC__
        "The kernel did not reach the read within the step bound.") ;
  Lwt_result_syntax.return_unit

(* NDS host functions exist from V6 onwards; below that the import does
   not link. *)
let tests =
  tztests_with_pvm
    ~versions:Wasm_pvm_state.[V6; VExperimental]
    [
      ( "an uncovered Verify-mode NDS read aborts the tick",
        `Quick,
        test_uncovered_read_aborts_the_tick );
    ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("NDS verify mode", tests)]
  |> Lwt_main.run
