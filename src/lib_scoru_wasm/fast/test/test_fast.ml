(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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
    Component:    Lib_scoru_wasm_fast
    Invocation:   dune exec src/lib_scoru_wasm/fast/test/main.exe -- --file test_fast.ml
    Subject:      Tests for the tezos-scoru-wasm library
*)

module Preimage_map = Map.Make (String)
open Wasm_utils
open Tztest_helper

let run_fast =
  Wasm_fast.compute_step_many
    ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint

let run_slow =
  Wasm.compute_step_many
    ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint

let apply_fast ?write_debug ?(fast_should_run = true)
    ?(images = Preimage_map.empty) ?metadata ?(stop_at_snapshot = false)
    ?(max_steps = Int64.max_int) counter tree =
  let open Lwt.Syntax in
  let run_counter = ref 0l in
  let reveal_builtins req =
    match Tezos_scoru_wasm.Wasm_pvm_state.Compatibility.of_current_opt req with
    | Some (Reveal_raw_data hash) -> (
        match Preimage_map.find hash images with
        | None -> Stdlib.failwith "Failed to find preimage"
        | Some preimage -> Lwt.return preimage)
    | _ -> (
        match metadata with
        | Some reveal -> Lwt.return reveal
        | None -> Stdlib.failwith "reveal_metadata is not available")
  in
  let hooks =
    Tezos_scoru_wasm.Hooks.(
      no_hooks
      |> on_fast_exec_completed (fun () ->
             run_counter := Int32.succ !run_counter ;
             Lwt_syntax.return_unit))
  in
  let+ tree, ticks =
    run_fast
      ?write_debug
      ~reveal_builtins
        (* We override the builtins to provide our own [reveal_preimage]
           implementation. This allows us to run Fast Exec with kernels that
           want to reveal stuff. *)
      ~stop_at_snapshot
      ~hooks
      ~max_steps
      tree
  in
  if counter > 0l then
    (* Assert that the FE actual ran. We must consider that before the first
       message is inserted, FE is unlikely to run. *)
    if fast_should_run && !run_counter <= 0l then
      Stdlib.failwith "Fast Execution was expected to run!"
    else if (not fast_should_run) && !run_counter > 0l then
      (* If we didn't expect FE to run, and e.g. expected it to fallback to PVM *)
      Stdlib.failwith "Fast Execution was NOT expected to run!" ;
  (tree, ticks)

let rec apply_slow ?write_debug ?images ?metadata ?(max_steps = Int64.max_int)
    tree =
  let open Lwt.Syntax in
  let* tree, _ = run_slow ?write_debug ~max_steps tree in
  (* Sometimes the evaluation will stop when a revelation is required.
     We don't want to expose this intermediate state because the Fast Execution
     doesn't either. The following step takes care of that by trying to
     satisfy the revelation request. *)
  (check_reveal [@tailcall]) ?write_debug ?images ?metadata ~max_steps tree

and check_reveal ?write_debug ?(images = Preimage_map.empty) ?metadata
    ~max_steps tree =
  let open Lwt.Syntax in
  let* info = Wasm_utils.Wasm.get_info tree in
  match info.input_request with
  | Reveal_required req -> (
      match
        Tezos_scoru_wasm.Wasm_pvm_state.Compatibility.of_current_opt req
      with
      | Some (Reveal_raw_data hash) -> (
          match Preimage_map.find hash images with
          | None -> Lwt.return tree
          | Some preimage ->
              let* tree =
                Wasm_utils.Wasm.reveal_step (Bytes.of_string preimage) tree
              in
              (apply_slow [@tailcall])
                ?write_debug
                ~images
                ?metadata
                ~max_steps
                tree)
      | Some Reveal_metadata -> (
          match metadata with
          | None -> Stdlib.failwith "Unable to reveal metadata"
          | Some metadata ->
              let* tree =
                Wasm_utils.Wasm.reveal_step (Bytes.of_string metadata) tree
              in
              (apply_slow [@tailcall]) ~images ~metadata ~max_steps tree)
      | None -> Stdlib.failwith "unsupported reveal request")
  | _ -> Lwt.return tree

(** [test_against_both ~from_binary ~kernel ~messages] test [kernel] against
    [messages] in slow mode vs fast mode, comparing state hashes after each
    message.
    A number of options are available:
    - [max_steps] (int64) is the number of steps to execute after each message.
    Only makes sense if just one message was provided.
    - [set_input] (see [Wasm_utils.set_full_input_step]) is used to read
    messages.
    - [write_debug] is the implementation of the host function to use.
    - [metadata] is used in case of Reveal_metadata step.
    - [images] is a hashmap used in case of reveal steps. *)
let test_against_both ~version ?write_debug ?(fast_should_run = true)
    ?(set_input = Wasm_utils.set_full_input_step) ?images ?metadata ~from_binary
    ~kernel ~messages ?(max_steps = Int64.max_int) () =
  let open Lwt.Syntax in
  let make_folder apply (tree, counter, hashes) message =
    let* info = Wasm_utils.Wasm.get_info tree in
    assert (info.input_request = Input_required) ;

    let* tree = set_input [message] counter tree in
    let hash2 = Tezos_context_memory.Context_binary.Tree.hash tree in

    (* kernel_run (including reboots) *)
    let* tree = apply counter tree in
    let hash3 = Tezos_context_memory.Context_binary.Tree.hash tree in

    let+ stuck = Wasm_utils.Wasm.Internal_for_tests.is_stuck tree in
    assert (Option.is_none stuck) ;

    (tree, Int32.succ counter, hash2 :: hash3 :: hashes)
  in

  let* initial_state =
    Wasm_utils.initial_state
      ~version
      ~ticks_per_snapshot:Wasm_utils.production_max_tick
      ~from_binary
      kernel
  in

  (* make sure to start in correct state *)
  let* initial_state =
    Wasm_utils.eval_until_input_or_reveal_requested
      ~fast_exec:false
      ~max_steps:Int64.max_int
      initial_state
  in

  let* stuck = Wasm_utils.Wasm.Internal_for_tests.is_stuck initial_state in
  assert (Option.is_none stuck) ;

  let run_with apply =
    let+ _, _, hashes =
      List.fold_left_s (make_folder apply) (initial_state, 0l, []) messages
    in
    hashes
  in

  let* slow_hashes =
    run_with (fun _ -> apply_slow ?write_debug ?images ?metadata ~max_steps)
  in
  let* fast_hashes =
    run_with (fun i t ->
        let+ tree, _ =
          apply_fast
            ~fast_should_run
            ?write_debug
            ?images
            ?metadata
            ~max_steps
            i
            t
        in
        tree)
  in

  assert (List.equal Context_hash.equal slow_hashes fast_hashes) ;

  Lwt_result_syntax.return_unit

let test_computation ~version =
  Wasm_utils.test_with_kernel "computation" (fun kernel ->
      (* Providing 4 empty messages basically means calling the kernel
         entrypoint 4 times. *)
      test_against_both
        ~version
        ~from_binary:true
        ~kernel
        ~messages:[""; ""; ""; ""]
        ())

(* kernel that does a fixed number of reboot
   by storing a counter in durable storage
*)
let kernel_bounded_reboot =
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
    "store_has"
    (func $store_has (param i32 i32) (result i32))
  )
  (import
    "smart_rollup_core"
    "store_read"
    (func $store_read (param i32 i32 i32 i32 i32) (result i32))
  )

  (memory 1)
  (export "memory" (memory 0))


  (data (i32.const 0) "HelloRebootInitStop")
  (data (i32.const 100) "/kernel/env/reboot")
  (data (i32.const 1000) "/test")

  (func $set_reboot_flag
    (call $write_debug
      ;; Memory address and length of the string to log
      ;; Should be "Reboot" as defined above
      (i32.const 5) (i32.const 6)
    )
    (call $store_write
          (i32.const 100) ;; offset
          (i32.const 18) ;; key size
          (i32.const 0) ;; offset in the durable storage page
          (i32.const 100) ;; offset in memory for the value (placeholder here)
          (i32.const 0)) ;; size of the value in memory (placeholder here)
    (drop)
  )

  (func $init
    ;; Is key defined
    (call $store_has
      (i32.const 1000)
      (i32.const 5))
    ;; returns 0 if no key defined
    (i32.const 0)
    (i32.eq)
    (if
      (then
        ;; init value
        (call $write_debug
          ;; Memory address and length of the string to log
          ;; Should be "Init" as defined above
          (i32.const 11) (i32.const 4)
        )
        (i32.store (i32.const 20) (i32.const 6))
        (call $store_write
          (i32.const 1000) ;; offset
          (i32.const 5) ;; key size
          (i32.const 0) ;; offset in the durable storage page
          (i32.const 20) ;; offset in memory for the value
          (i32.const 4)) ;; size of the value in memory
        (drop)
      )
    )
  )

  (func (export "kernel_run")
    (local $counter i32)
    (local $counter_value i32)
    (local.set $counter (i32.const 10000))
    (call $write_debug
      ;; Memory address and length of the string to log
      ;; Should be "Hello" as defined above
      (i32.const 0) (i32.const 5)
    )
    (call $init)

    ;; read counter
    (call $store_read
      ;; Memory address and length of the store path
      (i32.const 1000) (i32.const 5)
      ;; Offset into the store value
      (i32.const 0)
      ;; Memory address and length of the read buffer
      (local.get $counter) (i32.const 4) ;; Bytes to write
    )
    (drop) ;; drop nb of bytes read
    (call $write_debug
      ;; Memory address and length of the string to log
      (local.get $counter) (i32.const 4)
    )

    ;; has counter reached 0 ?
    (i32.load (local.get $counter))
    (i32.const 0)
    (i32.ne)
    (if
      (then
        ;; substract 1 to counter value
        (local.set $counter_value
          (i32.sub
            (i32.load (local.get $counter))
            (i32.const 1)
          )
        )

        ;; store in counter
        (i32.store
          (local.get $counter)
          (local.get $counter_value)
        )

        (call $write_debug
          ;; Memory address and length of the string to log
          (local.get $counter) (i32.const 4)
        )

        ;; store in durable storage
        (call $store_write
          (i32.const 1000) ;; offset
          (i32.const 5) ;; key size
          (i32.const 0) ;; offset in the durable storage page
          (local.get $counter) ;; offset in memory for the value
          (i32.const 4)) ;; size of the value in memory
        (drop) ;; drop nb of bytes written
        ;; set for reboot
        (call $set_reboot_flag)
      )
      (else
        (call $write_debug
          ;; Memory address and length of the string to log
          (i32.const 15)(i32.const 4)
        ))
    )
  )
)
  |}

let test_reboot ~version () =
  test_against_both
    ~version
    ~from_binary:false
    ~kernel:kernel_bounded_reboot
    ~messages:[""]
    ()

let test_max_steps ~version () =
  test_against_both
    ~version
    ~max_steps:23_001_001_001L
    ~from_binary:false
    ~kernel:kernel_bounded_reboot
    ~messages:[""]
    ()

let test_too_many_reboots ~version () =
  (* kernel that
     - increment counter in durable storage
     - reboots without stopping condition
  *)
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
    "store_has"
    (func $store_has (param i32 i32) (result i32))
  )
  (import
    "smart_rollup_core"
    "store_read"
    (func $store_read (param i32 i32 i32 i32 i32) (result i32))
  )

  (memory 1)
  (export "memory" (memory 0))

  (data (i32.const 0) "HelloRebootInitStop")
  (data (i32.const 100) "/kernel/env/reboot")
  (data (i32.const 1000) "/test")

  (func $set_reboot_flag ;; function asking for a reboot
    (call $write_debug
      ;; Memory address and length of the string to log
      ;; Should be "Reboot" as defined above
      (i32.const 5) (i32.const 6)
    )
    (call $store_write
          (i32.const 100) ;; offset
          (i32.const 18) ;; key size
          (i32.const 0) ;; offset in the durable storage page
          (i32.const 100) ;; offset in memory for the value (placeholder here)
          (i32.const 0)) ;; size of the value in memory (placeholder here)
    (drop)
  )

  (func $init ;; initialisation of a value in durable storage
    ;; Is key defined
    (call $store_has
      (i32.const 1000)
      (i32.const 5))
    ;; returns 0 if no key defined
    (i32.const 0)
    (i32.eq)
    (if
      (then
        ;; init value
        (call $write_debug
          ;; Memory address and length of the string to log
          ;; Should be "Init" as defined above
          (i32.const 11) (i32.const 4)
        )
        (i32.store (i32.const 20) (i32.const 0))
        (call $store_write
          (i32.const 1000) ;; offset
          (i32.const 5) ;; key size
          (i32.const 0) ;; offset in the durable storage page
          (i32.const 20) ;; offset in memory for the value
          (i32.const 4)) ;; size of the value in memory
        (drop)
      )
    )
  )

  (func $increment ;; increment counter
    (local $counter i32)
    (local $counter_value i32)
    (local.set $counter (i32.const 10000))
    (call $store_read
      ;; Memory address and length of the store path
      (i32.const 1000) (i32.const 5)
      ;; Offset into the store value
      (i32.const 0)
      ;; Memory address and length of the read buffer
      (local.get $counter) (i32.const 4) ;; Bytes to write
    )
    (drop) ;; drop nb of bytes read

    ;; increment counter value
    (local.set $counter_value
      (i32.add
        (i32.load (local.get $counter))
        (i32.const 1)
      )
    )

    ;; store in counter
    (i32.store
      (local.get $counter)
      (local.get $counter_value)
    )

    ;; store new value in durable storage
    (call $store_write
      (i32.const 1000) ;; offset
      (i32.const 5) ;; key size
      (i32.const 0) ;; offset in the durable storage page
      (local.get $counter) ;; offset in memory for the value
      (i32.const 4)) ;; size of the value in memory
    (drop) ;; drop nb of bytes written
  )

  (func (export "kernel_run")
    (call $init) ;; init value if not defined
    (call $increment) ;; increment counter
    (call $set_reboot_flag) ;; ask for reboot
  )
)
  |}
  in
  test_against_both ~version ~from_binary:false ~kernel ~messages:[""] ()

let test_store_read_write ~version () =
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
  test_against_both
    ~version
    ~from_binary:false
    ~kernel
    ~messages:(List.repeat 100 "")
    ()

let test_read_input ~version () =
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
        (i32.const 100) ;; address to write the level (4b) + message counter (8b)
        (i32.const 112) ;; address to write the read input
        (i32.const 4) ;; length of the input
      ) ;; should fill the bytes 100-116 with level, msg counter and the data "abcd"

      (drop) ;; we don't care about the result

      (call $write_output
        (i32.const 100) ;; source address
        (i32.const 16) ;; number of bytes
      ) ;; writes the read level, msg counter and the data "abcd"

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
    ~version
    ~from_binary:false
    ~kernel
    ~messages:(List.repeat no_of_levels msg)
    ()

let test_big_address ~version () =
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
  test_against_both ~version ~from_binary:false ~kernel ~messages:[msg] ()

let test_reveal_preimage ~version () =
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
  test_against_both
    ~version
    ~images
    ~from_binary:false
    ~kernel
    ~messages:[""; ""]
    ()

let test_tx ~version () =
  let open Lwt.Syntax in
  Wasm_utils.test_with_kernel
    "tx-kernel-no-verif"
    (fun kernel ->
      let* messages =
        Wasm_utils.read_test_messages ["deposit.out"; "withdrawal.out"]
      in
      let expected_rollup_address =
        (* Rollup address set in deposit.out *)
        `Hex "4da51c5de7a1cdd494c15b53815e1faa4c1a96ca" |> Hex.to_string
      in
      let expected_rollup_address =
        match expected_rollup_address with
        | Some a -> a
        | None -> Stdlib.failwith "invalid rollup address hash"
      in
      (* TX Kernel ignores origination level *)
      let metadata = expected_rollup_address ^ "\000\000\000\000" in
      test_against_both
        ~version
        ~set_input:Wasm_utils.set_full_raw_input_step
        ~from_binary:true
        ~metadata
        ~kernel
        ~messages
        ())
    ()

let test_compute_step_many_pauses_at_snapshot_when_flag_set ~version () =
  let open Lwt_result_syntax in
  let reveal_builtins = Wasm_utils.reveal_builtins in
  let*! initial_state =
    Wasm_utils.initial_state
      ~version
      ~ticks_per_snapshot:Wasm_utils.production_max_tick
      ~from_binary:false
      kernel_bounded_reboot
  in
  (* Supply input messages manually in order to have full control over tick_state we are in *)
  let*! tree = Wasm_utils.set_sol_input 0l initial_state in
  let*! tree = Wasm_utils.set_internal_message 0l Z.one "message" tree in
  let*! tree = Wasm_utils.set_eol_input 0l (Z.of_int 2) tree in

  let*! tick_state = Wasm.Internal_for_tests.get_tick_state tree in
  (* Check precondition that we are not in Snapshot *)
  assert (tick_state <> Snapshot) ;

  let*! fast_tree, fast_ticks = apply_fast ~stop_at_snapshot:true 0l tree in
  let*! slow_tree, slow_ticks =
    Wasm_utils.Wasm.compute_step_many
      ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
      ~reveal_builtins
      ~stop_at_snapshot:true
      ~max_steps:Int64.max_int
      tree
  in
  (* getting to first snapshot is ok (FE probably uses slow mode for that) *)
  let hash_fast = Tezos_context_memory.Context_binary.Tree.hash fast_tree in
  let hash_slow = Tezos_context_memory.Context_binary.Tree.hash slow_tree in
  assert (Context_hash.equal hash_fast hash_slow) ;
  assert (Int64.equal fast_ticks slow_ticks) ;

  (* check that we are in Snapshot *)
  let*! fast_tick_state = Wasm.Internal_for_tests.get_tick_state fast_tree in
  assert (fast_tick_state = Snapshot) ;
  let*! slow_tick_state = Wasm.Internal_for_tests.get_tick_state slow_tree in
  assert (slow_tick_state = Snapshot) ;

  let*! fast_tree, fast_ticks =
    apply_fast ~stop_at_snapshot:true 1l fast_tree
  in
  let*! slow_tree, slow_ticks =
    Wasm_utils.Wasm.compute_step_many
      ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
      ~reveal_builtins
      ~stop_at_snapshot:true
      ~max_steps:Int64.max_int
      slow_tree
  in
  (* getting to second snapshot is ok too (FE should have run) *)
  let hash_fast = Tezos_context_memory.Context_binary.Tree.hash fast_tree in
  let hash_slow = Tezos_context_memory.Context_binary.Tree.hash slow_tree in
  assert (Context_hash.equal hash_fast hash_slow) ;
  assert (Int64.equal fast_ticks slow_ticks) ;

  (* check that we are in Snapshot *)
  let*! fast_tick_state = Wasm.Internal_for_tests.get_tick_state fast_tree in
  assert (fast_tick_state = Snapshot) ;
  let*! slow_tick_state = Wasm.Internal_for_tests.get_tick_state slow_tree in
  assert (slow_tick_state = Snapshot) ;

  return_unit

let test_check_nb_ticks ~version () =
  let open Lwt_result_syntax in
  let reveal_builtins = Wasm_utils.reveal_builtins in
  let*! initial_state =
    Wasm_utils.initial_state
      ~version
      ~ticks_per_snapshot:Wasm_utils.production_max_tick
      ~from_binary:false
      kernel_bounded_reboot
  in
  (* Supply input messages manually in order to have full control over tick_state we are in *)
  let*! tree = Wasm_utils.set_sol_input 0l initial_state in
  let*! tree = Wasm_utils.set_internal_message 0l Z.one "message" tree in
  let*! tree = Wasm_utils.set_eol_input 0l (Z.of_int 2) tree in

  let*! tick_state = Wasm.Internal_for_tests.get_tick_state tree in
  (* Check precondition that we are not in Snapshot *)
  assert (tick_state <> Snapshot) ;

  let*! _, fast_ticks = apply_fast ~stop_at_snapshot:false 0l tree in
  let*! _, slow_ticks =
    Wasm_utils.Wasm.compute_step_many
      ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
      ~reveal_builtins
      ~stop_at_snapshot:false
      ~max_steps:Int64.max_int
      tree
  in
  assert (Int64.equal fast_ticks slow_ticks) ;

  return_unit

let test_read_input_write_output_failing ~version () =
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
          (i32.const 100) ;; address to write the level (4b) + message counter (8b)
          (i32.const 112) ;; address to write the read input
          (i32.const 4) ;; length of the input
        ) ;; should fill the bytes 100-116 with level, msg counter and the 4 bytes data
        (drop) ;; we don't care about the result

        (call $read_input
          (i32.const 100) ;; address to write the level (4b) + message counter (8b)
          (i32.const 112) ;; address to write the read input
          (i32.const 4) ;; length of the input
        ) ;; should fill the bytes 100-116 with level, msg counter and the 4 bytes data
        (drop) ;; we don't care about the result

        (call $write_output
          (i32.const 100) ;; source address
          (i32.const 16) ;; number of bytes
        )
        (drop) ;; we don't care about the result of write_output

        (call $write_output
          (i32.const 100) ;; source address
          (i32.const 16) ;; number of bytes
        ) ;; writes the read level, msg counter and the data "abcd"
        (drop) ;; we don't care about the result of write_output

        (unreachable)
      )
    )
      |}
  in
  let messages = ["abc"; "bonjour"; "привет"; "guten Tag"] in
  test_against_both
    ~fast_should_run:false
    ~version
    ~from_binary:false
    ~kernel
    ~messages
    ()

let tests =
  tztests_with_all_pvms
    [
      ("Big addresses are working correctly", `Quick, test_big_address);
      ("Computation kernel", `Quick, test_computation);
      ("Store read/write kernel", `Quick, test_store_read_write);
      ("read_input", `Quick, test_read_input);
      ("reboot", `Quick, test_reboot);
      ("max_steps flag", `Quick, test_max_steps);
      ("too many reboots", `Quick, test_too_many_reboots);
      ("compare nb ticks", `Quick, test_check_nb_ticks);
      ("Reveal_preimage kernel", `Quick, test_reveal_preimage);
      ("TX kernel", `Quick, test_tx);
      ( "compute_step_many pauses at snapshot",
        `Quick,
        test_compute_step_many_pauses_at_snapshot_when_flag_set );
      ( "compute_step_many recovers correctly after kernel panics",
        `Quick,
        test_read_input_write_output_failing );
    ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru-wasm-fast"
    [("Fast Execution", tests)]
  |> Lwt_main.run
