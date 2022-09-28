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
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "Reveal"
    Subject:      Reveal tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
open Wasm_utils

let module_ hash_addr preimage_addr max_bytes =
  Format.sprintf
    {|
      (module
        (import "rollup_safe_core" "reveal_preimage"
          (func $reveal_preimage (param i32 i32 i32) (result i32))
        )
        (memory 1)
        (export "mem" (memory 0))
        (func (export "kernel_next")
          (call $reveal_preimage (i32.const %ld) (i32.const %ld) (i32.const %ld))
        )
      )
    |}
    hash_addr
    preimage_addr
    max_bytes

let test_reveal_preimage_gen preimage max_bytes =
  let open Lwt_result_syntax in
  let hash_addr = 120l in
  let preimage_addr = 200l in
  let modl = module_ hash_addr preimage_addr max_bytes in
  let*! state = initial_tree modl in
  (* Let’s go *)
  let*! state = eval_until_input_requested state in
  let*! info = Wasm.get_info state in
  let* () =
    match info.Wasm_pvm_sig.input_request with
    | Wasm_pvm_sig.No_input_required | Input_required -> assert false
    | Reveal_required (Reveal_raw_data hash) ->
        (* The PVM has reached a point where it’s asking for some
           preimage. Since the memory is left blank, we are looking
           for the zero hash *)
        let zero_hash =
          Reveal.input_hash_from_string_exn (String.make 32 '\000')
        in
        assert (hash = zero_hash) ;
        return_unit
  in
  let*! state = Wasm.reveal_step (Bytes.of_string preimage) state in
  let*! info = Wasm.get_info state in
  let* () =
    match info.Wasm_pvm_sig.input_request with
    | Wasm_pvm_sig.No_input_required -> return_unit
    | Input_required ->
        failwith "should be running, but expect input from the L1"
    | Reveal_required _ -> failwith "should be running, but expect reveal tick"
  in
  let*! tick_state = Wasm.Internal_for_tests.get_tick_state state in
  (* The revelation step should contain the number of bytes effectively wrote in
     memory for the preimage. *)
  let* returned_size =
    match tick_state with
    | Eval
        Tezos_webassembly_interpreter.Eval.
          {
            step_kont =
              SK_Next (_, _, LS_Craft_frame (_, Inv_stop {code = vs, _; _}));
            _;
          } -> (
        let*! hd = Lazy_containers.Lazy_vector.Int32Vector.get 0l vs in
        match hd with
        | Num (I32 size) -> return size
        | _ -> failwith "Incorrect stack")
    | _ -> failwith "The tick after reveal_step is not consistent"
  in
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
let test_reveal_preimage_classic () =
  let preimage =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse \
     elementum nec ex sed porttitor."
    (* 100 bytes *)
  in
  let max_bytes = 200l in
  test_reveal_preimage_gen preimage max_bytes

let test_reveal_preimage_above_max () =
  let preimage =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse \
     elementum nec ex sed porttitor."
    (* 100 bytes *)
  in
  let max_bytes = 50l in
  test_reveal_preimage_gen preimage max_bytes

let tests =
  [
    tztest
      "Test reveal_preimage with preimage length below max_bytes"
      `Quick
      test_reveal_preimage_classic;
    tztest
      "Test reveal_preimage with preimage length above max_bytes"
      `Quick
      test_reveal_preimage_above_max;
  ]
