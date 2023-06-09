(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Lib_scoru_wasm reveal
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_reveal.ml
    Subject:      Reveal tests for the tezos-scoru-wasm library
*)

open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
open Wasm_utils
open Tztest_helper

let reveal_preimage_module hash hash_addr hash_size preimage_addr max_bytes =
  Format.sprintf
    {|
      (module
        (import "smart_rollup_core" "reveal_preimage"
          (func $reveal_preimage (param i32 i32 i32 i32) (result i32))
        )
        (memory 1)
        (data (i32.const %ld) "%s")
        (export "mem" (memory 0))
        (func (export "kernel_run")
          (call $reveal_preimage (i32.const %ld) (i32.const %ld) (i32.const %ld) (i32.const %ld))
        )
      )
    |}
    hash_addr
    hash
    hash_addr
    hash_size
    preimage_addr
    max_bytes

let reveal_metadata_module metadata_addr =
  Format.sprintf
    {|
      (module
        (import "smart_rollup_core" "reveal_metadata"
          (func $reveal_metadata (param i32 i32) (result i32))
        )
        (memory 1)
        (export "mem" (memory 0))
        (func (export "kernel_run")
          (call $reveal_metadata (i32.const %ld) (i32.const %d))
        )
      )
    |}
    metadata_addr
    Tezos_scoru_wasm.Host_funcs.Internal_for_tests.metadata_size

let reveal_returned_size tree =
  let open Lwt_syntax in
  let* tick_state = Wasm.Internal_for_tests.get_tick_state tree in
  match tick_state with
  | Eval
      Tezos_webassembly_interpreter.Eval.
        {
          config =
            {
              step_kont =
                SK_Next (_, _, LS_Craft_frame (_, Inv_stop {code = vs, _; _}));
              _;
            };
          _;
        } -> (
      let* hd = Tezos_lazy_containers.Lazy_vector.Int32Vector.get 0l vs in
      match hd with
      | Num (I32 size) -> return size
      | _ -> Stdlib.failwith "Incorrect stack")
  | _ -> Stdlib.failwith "The tick after reveal_builtins is not consistent"

let to_hex_string ?(tag = "\\00") s =
  let open Format in
  asprintf
    "%s%a"
    tag
    (pp_print_seq
       ~pp_sep:(fun _ _ -> ())
       (fun ppf e -> fprintf ppf "\\%02x" (Char.code e)))
    (String.to_seq s)

let test_reveal_preimage_gen ~version preimage max_bytes =
  let open Lwt_result_syntax in
  let hash_addr = 120l in
  let preimage_addr = 200l in
  (* The first byte corresponds to a tag which in the case of Blake2B hashes
     is set to zero. *)
  let hash_size = Int32.of_int (1 + Tezos_crypto.Blake2B.size) in
  let hash = Tezos_crypto.Blake2B.(to_string (hash_string [preimage])) in
  let modl =
    reveal_preimage_module
      (to_hex_string hash)
      hash_addr
      hash_size
      preimage_addr
      max_bytes
  in
  let*! state = initial_tree ~version modl in
  let*! state_snapshotted = eval_until_input_or_reveal_requested state in
  let*! state_with_dummy_input = set_empty_inbox_step 0l state_snapshotted in
  (* Let’s go *)
  let*! state = eval_until_input_or_reveal_requested state_with_dummy_input in
  (* Let's check the hash in memory. *)
  let*! module_instance =
    Wasm.Internal_for_tests.get_module_instance_exn state
  in
  let*! memory = Instance.Vector.get 0l module_instance.memories in
  let*! hash_in_memory =
    Memory.load_bytes memory hash_addr (Int32.to_int hash_size)
  in
  assert (
    String.equal (String.sub hash_in_memory 1 Tezos_crypto.Blake2B.size) hash) ;
  let*! info = Wasm.get_info state in
  let* () =
    let open Wasm_pvm_state in
    match info.Wasm_pvm_state.input_request with
    | Wasm_pvm_state.Reveal_required (Reveal_raw_data reveal_hash) ->
        (* The PVM has reached a point where it’s asking for some preimage. *)
        assert (
          String.equal (String.sub reveal_hash 1 Tezos_crypto.Blake2B.size) hash) ;

        return_unit
    | No_input_required | Input_required | Reveal_required _ -> assert false
  in
  let*! state = Wasm.reveal_step (Bytes.of_string preimage) state in
  let*! info = Wasm.get_info state in
  let* () =
    let open Wasm_pvm_state in
    match info.input_request with
    | No_input_required -> return_unit
    | Input_required ->
        failwith "should be running, but expect input from the L1"
    | Reveal_required _ -> failwith "should be running, but expect reveal tick"
  in
  (* The revelation step should contain the number of bytes effectively wrote in
     memory for the preimage. *)
  let*! returned_size = reveal_returned_size state in
  (* Let's check the preimage in memory. *)
  let*! module_instance =
    Wasm.Internal_for_tests.get_module_instance_exn state
  in
  let*! memory = Instance.Vector.get 0l module_instance.memories in
  let expected_length = min (String.length preimage) (Int32.to_int max_bytes) in
  assert (returned_size = Int32.of_int expected_length) ;
  let*! preimage_in_memory =
    Memory.load_bytes memory preimage_addr expected_length
  in
  assert (
    preimage_in_memory = String.sub preimage 0 (Int32.to_int returned_size)) ;
  return_unit

(* Test the best conditions for the preimage reveal: its size is below the
   maximum bytes for the preimage, it will be . *)
let test_reveal_preimage_classic ~version () =
  let preimage =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse \
     elementum nec ex sed porttitor."
    (* 100 bytes *)
  in
  let max_bytes = 200l in
  test_reveal_preimage_gen ~version preimage max_bytes

let test_reveal_preimage_above_max ~version () =
  let preimage =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse \
     elementum nec ex sed porttitor."
    (* 100 bytes *)
  in
  let max_bytes = 50l in
  test_reveal_preimage_gen ~version preimage max_bytes

let test_reveal_metadata ~version () =
  let open Lwt_result_syntax in
  let metadata_addr = 200l in
  let modl = reveal_metadata_module metadata_addr in
  let*! state = initial_tree ~version modl in
  let*! state_snapshotted = eval_until_input_or_reveal_requested state in
  let*! state_with_dummy_input = set_empty_inbox_step 0l state_snapshotted in
  (* Let’s go *)
  let*! state = eval_until_input_or_reveal_requested state_with_dummy_input in
  let*! info = Wasm.get_info state in
  let* () =
    match info.Wasm_pvm_state.input_request with
    | Wasm_pvm_state.Reveal_required Reveal_metadata -> return_unit
    | No_input_required | Input_required | Reveal_required _ -> assert false
  in
  (* These are dummy metadata since we do not have access to the
     metadata definition in this compilation unit. *)
  let metadata =
    Bytes.init
      Tezos_scoru_wasm.Host_funcs.Internal_for_tests.metadata_size
      Char.chr
  in
  let*! state = Wasm.reveal_step metadata state in
  let*! info = Wasm.get_info state in
  let* () =
    match info.Wasm_pvm_state.input_request with
    | Wasm_pvm_state.No_input_required -> return_unit
    | Input_required ->
        failwith "should be running, but expect input from the L1"
    | Reveal_required _ -> failwith "should be running, but expect reveal tick"
  in
  (* The revelation step should contain the number of bytes effectively wrote in
     memory for the preimage. *)
  let*! returned_size = reveal_returned_size state in
  (* Let's check the preimage in memory. *)
  let*! module_instance =
    Wasm.Internal_for_tests.get_module_instance_exn state
  in
  let*! memory = Instance.Vector.get 0l module_instance.memories in
  assert (
    Int32.to_int returned_size
    = Tezos_scoru_wasm.Host_funcs.Internal_for_tests.metadata_size) ;
  let*! preimage_in_memory =
    Memory.load_bytes memory metadata_addr (Int32.to_int returned_size)
  in
  assert (preimage_in_memory = Bytes.to_string metadata) ;
  return_unit

module Preimage_map = Map.Make (String)

let apply_fast ?(images = Preimage_map.empty) tree =
  let open Lwt.Syntax in
  let run_counter = ref 0l in
  let reveal_builtins =
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
      ~reveal_builtins:(Some reveal_builtins)
        (* We override the builtins to provide our own [reveal_preimage]
           implementation. This allows us to rune Fast Exec with
           kernels that want to reveal stuff. *)
      ~after_fast_exec:(fun () -> run_counter := Int32.succ !run_counter)
      ~fast_exec:true
      ~max_steps:Int64.max_int
      tree
  in
  (* Assert that the FE actual ran. *)
  if !run_counter <> 1l then
    Stdlib.failwith "Fast Execution was expected to run!" ;
  tree

let test_fast_exec_reveal ~version () =
  let open Lwt.Syntax in
  let example_hash = "this represents the 33-byte hash!" in
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
      ;; Size of the hash
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

  let* tree = initial_tree ~version kernel in
  let* tree = eval_until_input_or_reveal_requested tree in
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = apply_fast ~images tree in

  let* durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in

  let* written_value =
    Durable.(find_value_exn durable (key_of_string_exn "/foo"))
  in
  let* written_value =
    Tezos_lazy_containers.Chunked_byte_vector.to_string written_value
  in
  assert (String.equal written_value example_preimage) ;

  Lwt_result_syntax.return_unit

let tests =
  tztests_with_all_pvms
    [
      ( "Test reveal_preimage with preimage length below max_bytes",
        `Quick,
        test_reveal_preimage_classic );
      ( "Test reveal_preimage with preimage length above max_bytes",
        `Quick,
        test_reveal_preimage_above_max );
      ("Test reveal_metadata", `Quick, test_reveal_metadata);
      ("Test reveal_preimage with Fast Exec", `Quick, test_fast_exec_reveal);
    ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("Reveal", tests)]
  |> Lwt_main.run
