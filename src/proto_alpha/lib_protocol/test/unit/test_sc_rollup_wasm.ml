(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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
    Component:    Rollup layer 1 logic
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- test "^\[Unit\] sc rollup wasm$"
    Subject:      Unit test for the Wasm PVM
*)

open Protocol

let test_initial_state_hash_wasm_pvm () =
  let open Alpha_context in
  let open Lwt_result_syntax in
  let context = Tezos_context_memory.make_empty_context () in
  let*! state = Sc_rollup_helpers.Wasm_pvm.initial_state context in
  let*! hash = Sc_rollup_helpers.Wasm_pvm.state_hash state in
  let expected = Sc_rollup.Wasm_2_0_0PVM.reference_initial_state_hash in
  if Sc_rollup.State_hash.(hash = expected) then return_unit
  else
    failwith
      "incorrect hash, expected %a, got %a"
      Sc_rollup.State_hash.pp
      expected
      Sc_rollup.State_hash.pp
      hash

let test_incomplete_kernel_chunk_limit () =
  let open Lwt_result_syntax in
  let operator =
    match Account.generate_accounts 1 with
    | [(account, _, _)] -> account
    | _ -> assert false
  in
  let chunk_size = Tezos_scoru_wasm.Gather_floppies.chunk_size in
  let chunk_too_big = Bytes.make (chunk_size + 10) 'a' in
  let signature = Signature.sign operator.Account.sk chunk_too_big in
  let floppy =
    Tezos_scoru_wasm.Gather_floppies.{chunk = chunk_too_big; signature}
  in
  match
    Data_encoding.Binary.to_string_opt
      Tezos_scoru_wasm.Gather_floppies.floppy_encoding
      floppy
  with
  | None -> return_unit
  | Some _ -> failwith "encoding of a floppy with a chunk too large should fail"

let test_metadata_size () =
  let address = Sc_rollup_repr.Address.of_bytes_exn (Bytes.make 20 '\000') in
  let metadata =
    Sc_rollup_metadata_repr.{address; origination_level = Raw_level_repr.root}
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn Sc_rollup_metadata_repr.encoding metadata
  in
  assert (
    Bytes.length bytes
    = Tezos_scoru_wasm.Host_funcs.Internal_for_tests.metadata_size) ;
  Lwt_result_syntax.return_unit

let tests =
  [
    Tztest.tztest
      "initial state hash for Wasm"
      `Quick
      test_initial_state_hash_wasm_pvm;
    Tztest.tztest
      "encoding of a floppy with a chunk too large should fail"
      `Quick
      test_incomplete_kernel_chunk_limit;
    Tztest.tztest "size of a rollup metadata" `Quick test_metadata_size;
  ]
