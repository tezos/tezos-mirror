(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Testing
    -------
    Component:    Gather_floppies
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Gather floppies$"
    Subject:      Gather floppies tests
*)

open Tztest
open Tezos_scoru_wasm
open Test_encodings_util
open Wasm_utils

(* [account ()] returns a new account usable to sign floppies. *)
let account () =
  let _pkh, pk, sk = Tezos_crypto.Signature.generate_key () in
  (pk, sk)

(* Corresponds to `Constants_repr.sc_rollup_message_size_limit` in the
   protocol. *)
let sc_rollup_message_size_limit = 4096

(* [fake_inbox_message_encoding] simulates an external message from the rollup
   inbox. *)
let fake_inbox_message_encoding =
  let open Data_encoding in
  check_size
    sc_rollup_message_size_limit
    (union
       [
         case
           (Tag 1)
           ~title:"External"
           Variable.string
           (function `External msg -> Some msg)
           (fun msg -> `External msg);
       ])

(* [set_floppy_input_step chunk sk counter tree] encodes a [chunk] as a valid
   input from the rollup perspective, by signing it with [sk], and setting it as
   next input in [tree] with the given message [counter], at inbox level [0]. *)
let set_floppy_input_step chunk sk counter tree =
  let signature = Tezos_crypto.Signature.sign sk chunk in
  let input =
    Data_encoding.Binary.to_string_exn Gather_floppies.floppy_encoding
    @@ Gather_floppies.{chunk; signature}
  in
  let encoded_input =
    Data_encoding.Binary.to_string_exn fake_inbox_message_encoding
    @@ `External input
  in
  let input_info =
    Wasm_pvm_sig.
      {
        inbox_level =
          Option.value_f ~default:(fun () -> assert false)
          @@ Tezos_base.Bounded.Non_negative_int32.of_value 0l;
        message_counter = Z.of_int counter;
      }
  in
  Wasm.set_input_step input_info encoded_input tree

(* Extracted from
   src/proto_alpha/lib_protocol/test/integration/test_sc_rollup_wasm.ml *)
let make_chunks kernel pk =
  let chunk_size = Gather_floppies.chunk_size in
  let initial_chunk, rem_chunks =
    let split_chunk s =
      let len = String.length s in
      let size = min len chunk_size in
      let chunk = String.sub s 0 size in
      let rest =
        if len > chunk_size then Some (String.sub s size @@ (len - size))
        else None
      in
      (chunk, rest)
    in
    let rec do_chunks chunks left =
      match left with
      | None -> chunks
      | Some left ->
          let chunk, rest = split_chunk left in
          (do_chunks [@tailcall]) (chunk :: chunks) rest
    in
    let initial, rest = split_chunk kernel in
    (initial, List.rev @@ do_chunks [] rest)
  in
  let initial_chunk =
    Data_encoding.Binary.to_string_exn
      Gather_floppies.origination_message_encoding
    @@ Incomplete_kernel (Bytes.of_string initial_chunk, pk)
  in
  let chunks =
    rem_chunks
    |> List.take_n (List.length rem_chunks - 1)
    |> List.map Bytes.of_string
  in
  let final_chunk = Bytes.of_string @@ List.last "" rem_chunks in
  let final_chunk_size = Bytes.length final_chunk in
  (initial_chunk, chunks, final_chunk, final_chunk_size)

(* [initial_tree ~max_tick initial_chunck] builds a tree out of an encoded
   initial chunk. *)
let initial_tree ?(max_tick = 100000L) initial_chunk =
  let open Lwt.Syntax in
  let max_tick_Z = Z.of_int64 max_tick in
  let* empty_tree = empty_tree () in
  let* tree =
    Wasm.Internal_for_tests.initial_tree_from_boot_sector
      ~empty_tree
      initial_chunk
  in
  Wasm.Internal_for_tests.set_max_nb_ticks max_tick_Z tree

let pp_internal_status ppf = function
  | Gather_floppies.Gathering_floppies pk ->
      Format.fprintf ppf "Gathering_floppies %a" Signature.Public_key.pp pk
  | Not_gathering_floppies -> Format.fprintf ppf "Not_gathering_floppies"

(* [check_gathering_status expected_status tree] checks the current status of `Gather_floppies`
   against the given [expected_status]. *)
let check_gathering_status expected_status tree =
  let open Lwt_result_syntax in
  let*! internal_status = Wasm.Internal_for_tests.get_internal_status tree in
  match (internal_status, expected_status) with
  | None, _ -> failwith "Gathering floppies has failed"
  | Some (Gathering_floppies pk), Gather_floppies.Gathering_floppies pk' ->
      if not (Signature.Public_key.equal pk pk') then failwith ""
      else return_unit
  | Some Not_gathering_floppies, Not_gathering_floppies -> return_unit
  | Some status, _ ->
      failwith
        "Unexpected gathering status %a, expected %a"
        pp_internal_status
        status
        pp_internal_status
        expected_status

(* [init_tree_with_floppies kernel] builds a tree out of a kernel by chunkifying
   it, and install each of them one by one as would the rollup do. *)
let init_tree_with_floppies ?(max_steps = Wasm_utils.default_max_tick) kernel =
  let open Lwt_result_syntax in
  let pk, sk = account () in
  let initial_chunk, chunks, final_chunk, _final_chunk_size =
    make_chunks kernel pk
  in
  let*! tree = initial_tree initial_chunk in
  (* This forces it to go through `Gather_floppies.compute_step_many if need be,
     but it actually shouldn't. *)
  let*! tree = eval_until_input_requested ~max_steps tree in
  let set_input (tree, counter) chunk =
    let*! info = Wasm.get_info tree in
    if info.input_request = Wasm_pvm_sig.Input_required then
      let*! tree = set_floppy_input_step chunk sk counter tree in
      let+ () = check_gathering_status (Gathering_floppies pk) tree in
      (tree, succ counter)
    else return (tree, counter)
  in
  let* tree, counter = List.fold_left_es set_input (tree, 0) chunks in
  let*! tree = set_floppy_input_step final_chunk sk counter tree in
  return tree

let test_gather_floppies_until_first_PVM_input kernel =
  let open Lwt_result_syntax in
  let* tree = init_tree_with_floppies kernel in
  check_gathering_status Not_gathering_floppies tree

let test_gather_floppies_and_compute ~max_steps kernel =
  let open Lwt_result_syntax in
  let* tree = init_tree_with_floppies ~max_steps kernel in
  (* Make the first ticks of the WASM PVM (parsing of origination
     message, snapshot,n parsing and init of the kernel), to switch it to
     “Input_requested” mode. *)
  let*! tree_snapshotted = eval_until_input_requested tree in
  (* Feeding it with one input *)
  let*! tree_with_input = set_input_step "test" 0 tree_snapshotted in
  (* running until waiting for next input *)
  let*! tree = eval_until_input_requested tree_with_input in
  let*! state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is expected to fail, then ths PVM should be in stuck state, and
     have failed during the evaluation when evaluating a `Unreachable`
     instruction. *)
  assert (
    is_stuck
      ~step:`Eval
      ~reason:"unreachable executed"
      state_after_first_message) ;
  return_unit

(* Tests are using `unreachable.wasm`, which is 9842 Bytes long, hence contained
   in 3 chunks. *)
let tests =
  [
    tztest
      "Test unreachable kernel until first PVM input"
      `Quick
      (test_with_kernel
         Kernels.unreachable_kernel
         test_gather_floppies_until_first_PVM_input);
    tztest
      "Test unreachable kernel with a PVM input, one step at a time"
      `Quick
      (test_with_kernel
         Kernels.unreachable_kernel
         (test_gather_floppies_and_compute ~max_steps:1L));
    tztest
      "Test unreachable kernel with a PVM input, 10_000 steps at a time"
      `Quick
      (test_with_kernel
         Kernels.unreachable_kernel
         (test_gather_floppies_and_compute ~max_steps:10_000L));
  ]
