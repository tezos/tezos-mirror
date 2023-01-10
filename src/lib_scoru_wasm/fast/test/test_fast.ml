(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Tztest
module Preimage_map = Map.Make (String)
open Wasm_utils

let apply_fast ?(images = Preimage_map.empty) counter tree =
  let open Lwt.Syntax in
  let run_counter = ref 0l in
  let reveal_builtins =
    Some
      Tezos_scoru_wasm.Builtins.
        {
          reveal_preimage =
            (fun hash ->
              match Preimage_map.find hash images with
              | None -> Stdlib.failwith "Failed to find preimage"
              | Some preimage -> Lwt.return preimage);
          reveal_metadata =
            (fun () -> Stdlib.failwith "reveal_preimage is not available");
        }
  in
  let+ tree =
    Wasm_utils.eval_until_input_requested
      ~reveal_builtins
        (* We override the builtins to provide our own [reveal_preimage]
           implementation. This allows us to run Fast Exec with kernels that
           want to reveal stuff. *)
      ~after_fast_exec:(fun () -> run_counter := Int32.succ !run_counter)
      ~fast_exec:true
      ~max_steps:Int64.max_int
      tree
  in
  if counter > 0l then
    (* Assert that the FE actual ran. We must consider that before the first
       message is inserted, FE is unlikely to run. *)
    if !run_counter <> 1l then
      Stdlib.failwith "Fast Execution was expected to run!" ;
  tree

let rec apply_slow ?images tree =
  let open Lwt.Syntax in
  let* tree =
    Wasm_utils.eval_until_input_or_reveal_requested
      ~fast_exec:false
      ~max_steps:Int64.max_int
      tree
  in
  (* Sometimes the evaluation will stop when a revelation is required.
     We don't want to expose this intermediate state because the Fast Execution
     doesn't either. The following step takes care of that by trying to
     satisfy the revelation request. *)
  (check_reveal [@tailcall]) ?images tree

and check_reveal ?(images = Preimage_map.empty) tree =
  let open Lwt.Syntax in
  let* info = Wasm_utils.Wasm.get_info tree in
  match info.input_request with
  | Reveal_required (Reveal_raw_data hash) -> (
      match Preimage_map.find hash images with
      | None -> Lwt.return tree
      | Some preimage ->
          let* tree =
            Wasm_utils.Wasm.reveal_step (Bytes.of_string preimage) tree
          in
          (apply_slow [@tailcall]) ~images tree)
  | _ -> Lwt.return tree

let test_against_both ?images ~from_binary ~kernel ~messages () =
  let open Lwt.Syntax in
  let make_folder apply (tree, counter, hashes) message =
    let* tree = apply counter tree in
    let hash1 = Tezos_context_memory.Context_binary.Tree.hash tree in

    let* stuck = Wasm_utils.Wasm.Internal_for_tests.is_stuck tree in
    assert (Option.is_none stuck) ;

    let* info = Wasm_utils.Wasm.get_info tree in
    assert (info.input_request = Input_required) ;

    let+ tree = Wasm_utils.set_full_input_step [message] counter tree in
    let hash2 = Tezos_context_memory.Context_binary.Tree.hash tree in

    (tree, Int32.succ counter, hash1 :: hash2 :: hashes)
  in

  let* initial_tree =
    Wasm_utils.initial_tree ~max_tick:Int64.max_int ~from_binary kernel
  in

  let run_with apply =
    let* tree, counter, hashes =
      List.fold_left_s (make_folder apply) (initial_tree, 0l, []) messages
    in

    let+ tree = apply counter tree in
    let hash = Tezos_context_memory.Context_binary.Tree.hash tree in

    hash :: hashes
  in

  let* fast_hashes = run_with (apply_fast ?images) in
  let* slow_hashes = run_with (fun _ -> apply_slow ?images) in

  assert (List.equal Tezos_crypto.Context_hash.equal slow_hashes fast_hashes) ;

  Lwt_result_syntax.return_unit

let test_computation =
  Wasm_utils.test_with_kernel "computation" (fun kernel ->
      (* Providing 4 empty messages basically means calling the kernel
         entrypoint 4 times. *)
      test_against_both ~from_binary:true ~kernel ~messages:[""; ""; ""; ""] ())

let test_store_read_write =
  let kernel =
    {|
(module
  (import
    "smart_rollup_core"
    "write_debug"
    (func $write_debug (param i32 i32))
  )

  (import
    "smart_rollup_core"
    "store_write"
    (func $store_write (param i32 i32 i32 i32 i32) (result i32))
  )

  (import
    "smart_rollup_core"
    "store_read"
    (func $store_read (param i32 i32 i32 i32 i32) (result i32))
  )

  (memory 1)
  (export "memory" (memory 0))

  (data (i32.const 0) "Hello World")
  (data (i32.const 100) "/foo")

  (func (export "kernel_run") (local $n i64)
    (call $write_debug
      ;; Memory address and length of the string to log
      ;; Should be "Hello" as defined above
      (i32.const 0) (i32.const 5)
    )

    (call $write_debug
      ;; Memory address and length of the string to log
      ;; Should be "World" as defined above
      (i32.const 6) (i32.const 5)
    )

    (call $write_debug
      ;; Memory address and length of the string to log
      ;; Should be "Hello World" as defined above
      (i32.const 0) (i32.const 11)
    )

    ;; Write the initial value to the durable store
    (call $store_write
      ;; Memory address and length of the store path
      (i32.const 100) (i32.const 4)
      ;; Offset into the store value
      (i32.const 0)
      ;; Memory address and length of the payload to write
      (i32.const 1000) (i32.const 4)
    )
    (drop)

    ;; Set the loop counter
    (local.set $n (i64.const 100))

    (loop
      ;; Read from store to memory
      (call $store_read
        ;; Memory address and length of the store path
        (i32.const 100) (i32.const 4)
        ;; Offset into the store value
        (i32.const 0)
        ;; Memory address and length of the read buffer
        (i32.const 1000) (i32.const 4) ;; Bytes to write
      )
      (drop)

      ;; Increment the value in memory
      (i32.store
        (i32.const 1000)
        (i32.add
          (i32.load (i32.const 1000))
          (i32.const 1)))

      ;; Write value from memory to durable store
      (call $store_write
        ;; Memory address and length of the store path
        (i32.const 100) (i32.const 4)
        ;; Offset into the store value
        (i32.const 0)
        ;; Memory address and length of the payload to write
        (i32.const 1000) (i32.const 4)
      )
      (drop)

      ;; Decrement loop counter
      (local.set $n (i64.sub (local.get $n) (i64.const 1)))

      ;; Check if we ought to loop again
      (i32.eq (i32.const 0) (i64.eq (local.get $n) (i64.const 0)))
      (br_if 0)
    )
  )
)
  |}
  in
  test_against_both ~from_binary:false ~kernel ~messages:(List.repeat 100 "")

let test_read_input =
  let kernel =
    {|
  (module
    (import
      "smart_rollup_core"
      "read_input"
      (func $read_input (param i32 i32 i32) (result i32))
    )

    (import
      "smart_rollup_core"
      "write_output"
      (func $write_output (param i32 i32) (result i32))
    )

    (import
      "smart_rollup_core"
      "write_debug"
      (func $write_debug (param i32 i32))
    )


    (memory 1)
    (export "memory" (memory 0))

    (data (i32.const 1000) "kernel_run called")

    (func (export "kernel_run")
      (call $write_debug
        (i32.const 1000) ;; Memory address
        (i32.const 17) ;; length of the string to log
      ) ;; Should print "kernel_run called"

      (call $read_input
        (i32.const 100) ;; address to write the level (4b)
        (i32.const 104) ;; address to write the message counter (8b)
        (i32.const 112) ;; address to write the read input
        (i32.const 4) ;; length of the input
      ) ;; should fill the bytes 100-116 with level, msg counter and the data "abcd"

      (drop) ;; we don't care about the result

      (call $write_output
        (i32.const 100) ;; source address
        (i32.const 16) ;; number of bytes
      ) ;; writes the read level, msg counter and the data "abcd"

      (drop) ;; we don't care about the result of write_output
      (drop) ;; we don't care about the result of write_output
    )
  )
    |}
  in
  let msg = "abcd" in
  (* We want to produce more than 255 levels such that, when the level is written
     in the memory, its multi-byte representation is tested for correctness.
     If the number were a single byte the test would have been less relevant*)
  let no_of_levels = 257 in

  test_against_both
    ~from_binary:false
    ~kernel
    ~messages:(List.repeat no_of_levels msg)

let test_big_address =
  let kernel =
    {|
        (module
          (type $read_t (func (param i32 i32 i32) (result i32)))
          (import "smart_rollup_core" "read_input" (func $read_input (type $read_t)))
          (memory 65536)
          (export "mem" (memory 0))

          (func (export "kernel_run")
            (call $read_input
                  (i32.const 4_294_967_100) ;; addr_info
                  (i32.const 4_294_967_196) ;; (-100)
                  (i32.const 4))
            (drop)
          )
        )
      |}
  in
  let msg = "abcd" in
  test_against_both ~from_binary:false ~kernel ~messages:[msg]

let test_reveal_preimage =
  let example_hash = "this represents the 33-byte hash " in
  let example_preimage = "This is the expected preimage" in
  let images = Preimage_map.singleton example_hash example_preimage in

  let kernel =
    Format.asprintf
      {|
(module
  (import
    "smart_rollup_core"
    "store_write"
    (func $store_write (param i32 i32 i32 i32 i32) (result i32))
  )

  (import
    "smart_rollup_core"
    "reveal_preimage"
    (func $reveal_preimage (param i32 i32 i32 i32) (result i32))
  )

  (memory 1)

  (export "memory" (memory 0))
  (data (i32.const 0) "%s") ;; The hash we want to reveal
  (data (i32.const 100) "/foo")

  (func (export "kernel_run") (local $len i32)
    ;; Reveal something
    (call $reveal_preimage
      ;; Address of the hash
      (i32.const 0)
      ;; size of the hash
      (i32.const 33)
      ;; Destination address and length
      (i32.const 1000) (i32.const 1000)
    )
    (local.set $len)

    ;; Write the preimage to the output buffer
    (call $store_write
      ;; Key address and length
      (i32.const 100) (i32.const 4)
      ;; Offset into the target value
      (i32.const 0)
      ;; Memory address and length of the payload to write
      (i32.const 1000) (local.get $len)
    )
    (drop)
  )
)
  |}
      example_hash
  in
  test_against_both ~images ~from_binary:false ~kernel ~messages:[""; ""]

let test_tx =
  let open Lwt.Syntax in
  Wasm_utils.test_with_kernel "tx-kernel-no-verif" (fun kernel ->
      let* messages =
        Wasm_utils.read_test_messages ["deposit.out"; "withdrawal.out"]
      in
      test_against_both ~from_binary:true ~kernel ~messages ())

let test_compute_step_many_pauses_at_snapshot_when_flag_set =
  Wasm_utils.test_with_kernel "computation" (fun kernel ->
      let open Lwt_result_syntax in
      let reveal_builtins =
        Tezos_scoru_wasm.Builtins.
          {
            reveal_preimage =
              (fun _ -> Stdlib.failwith "reveal_preimage is not available");
            reveal_metadata =
              (fun () -> Stdlib.failwith "reveal_metadata is not available");
          }
      in
      let*! initial_tree =
        Wasm_utils.initial_tree ~max_tick:Int64.max_int ~from_binary:true kernel
      in
      (* Supply input messages manually in order to have full control over tick_state we are in *)
      let*! tree = Wasm_utils.set_sol_input 0l initial_tree in
      let*! tree = Wasm_utils.set_internal_message 0l Z.one "message" tree in
      let*! tree = Wasm_utils.set_eol_input 0l (Z.of_int 2) tree in

      let*! tick_state = Wasm.Internal_for_tests.get_tick_state tree in
      (* Check precondition that we are not in Snapshot *)
      assert (tick_state == Padding) ;
      let*! fast_tree, _ =
        Wasm_utils.Wasm_fast.compute_step_many
          ~reveal_builtins
          ~write_debug:Noop
          ~stop_at_snapshot:true
          ~max_steps:Int64.max_int
          tree
      in
      let*! slow_tree, _ =
        Wasm_utils.Wasm.compute_step_many
          ~reveal_builtins
          ~stop_at_snapshot:true
          ~max_steps:Int64.max_int
          tree
      in
      let hash_fast = Tezos_context_memory.Context_binary.Tree.hash fast_tree in
      let hash_slow = Tezos_context_memory.Context_binary.Tree.hash slow_tree in
      assert (Tezos_crypto.Context_hash.equal hash_fast hash_slow) ;
      return ())

let tests =
  [
    tztest "Big addresses are working correctly" `Quick test_big_address;
    tztest "Computation kernel" `Quick test_computation;
    tztest "Store read/write kernel" `Quick test_store_read_write;
    tztest "read_input" `Quick test_read_input;
    tztest "Reveal_preimage kernel" `Quick test_reveal_preimage;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3729
       Enable back this test when the transaction kernel has been
       updated. *)
    (* tztest "TX kernel" `Quick test_tx; *)
    tztest
      "compute_step_many pauses at snapshot"
      `Quick
      test_compute_step_many_pauses_at_snapshot_when_flag_set;
  ]
