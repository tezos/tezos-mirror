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

let apply_fast counter tree =
  let open Lwt.Syntax in
  let run_counter = ref 0l in
  let+ tree =
    Wasm_utils.eval_until_input_requested
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

let apply_slow =
  Wasm_utils.eval_until_input_requested
    ~fast_exec:false
    ~max_steps:Int64.max_int

let test_against_both ~from_binary ~kernel ~messages =
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

  let* fast_hashes = run_with apply_fast in
  let* slow_hashes = run_with (fun _ tree -> apply_slow tree) in

  assert (List.equal Tezos_crypto.Context_hash.equal slow_hashes fast_hashes) ;

  Lwt_result_syntax.return_unit

let test_computation =
  Wasm_utils.test_with_kernel "computation" (fun kernel ->
      (* Providing 4 empty messages basically means calling the kernel
         entrypoint 4 times. *)
      test_against_both ~from_binary:true ~kernel ~messages:[""; ""; ""; ""])

let test_store_read_write () =
  let kernel =
    {|
(module
  (import
    "rollup_safe_core"
    "write_debug"
    (func $write_debug (param i32 i32))
  )

  (import
    "rollup_safe_core"
    "store_write"
    (func $store_write (param i32 i32 i32 i32 i32) (result i32))
  )

  (import
    "rollup_safe_core"
    "store_read"
    (func $store_read (param i32 i32 i32 i32 i32) (result i32))
  )

  (memory 1)
  (export "memory" (memory 0))

  (data (i32.const 0) "Hello World")
  (data (i32.const 100) "/foo")

  (func (export "kernel_next") (local $n i64)
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

let test_tx =
  let open Lwt.Syntax in
  Wasm_utils.test_with_kernel "tx-kernel-no-verif" (fun kernel ->
      let* messages =
        Wasm_utils.read_test_messages ["deposit.out"; "withdrawal.out"]
      in
      test_against_both ~from_binary:true ~kernel ~messages)

let tests =
  [
    tztest "Computation kernel" `Quick test_computation;
    tztest "Store read/write kernel" `Quick test_store_read_write;
    tztest "TX kernel" `Quick test_tx;
  ]
