(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol (Tezos_crypto.Timelock)
    Invocation: cd src/proto_alpha/lib_protocol/test/integration/michelson && \
                dune exec ./main.exe -- test "^timelock$"
    Subject:    On timelock
*)

open Protocol

let wrap e = Lwt.return (Environment.wrap_tzresult e)

let simple_test () =
  let public, secret = Tezos_crypto.Timelock.gen_rsa_keys () in
  let locked_value = Tezos_crypto.Timelock.gen_locked_value public in
  let time = 1000 in
  let unlocked_value =
    Tezos_crypto.Timelock.unlock_with_secret secret ~time locked_value
  in
  let same_unlocked, proof =
    Tezos_crypto.Timelock.unlock_and_prove_without_secret
      public
      ~time
      locked_value
  in
  assert (unlocked_value = same_unlocked) ;
  let sym_key =
    Tezos_crypto.Timelock.unlocked_value_to_symmetric_key unlocked_value
  in
  let message = Bytes.create 12 in
  let c = Tezos_crypto.Timelock.encrypt sym_key message in
  let expected_result = Environment.Timelock.Correct message in
  let chest_key = Tezos_crypto.Timelock.{unlocked_value; proof} in
  let chest =
    Tezos_crypto.Timelock.{locked_value; rsa_public = public; ciphertext = c}
  in
  let result = Environment.Timelock.open_chest chest chest_key ~time in
  assert (result = expected_result) ;
  return_unit

let timelock_path =
  project_root // "michelson_test_scripts/ill_typed/timelock.tz"

let deprecated_chest_open () =
  let open Lwt_result_syntax in
  (* Verify contract fails origination as OPEN_CHEST is marked as legacy (deprecated )*)
  let* block, baker, source_contract, _src2 = Contract_helpers.init () in
  let storage = "0xdeadbeef" in
  let script = Contract_helpers.read_file timelock_path in
  Contract_helpers.originate_contract_from_string_hash
    ~script
    ~storage
    ~source_contract
    ~baker
    block
  >>= function
  | Ok _ -> Alcotest.fail "script originated successfully, expected an error"
  | Error lst
    when List.mem
           ~equal:( = )
           (Environment.Ecoproto_error
              (Script_tc_errors.Deprecated_instruction T_chest_key))
           lst ->
      return ()
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_trace errs

(* Test to verify open_chest correctness
   DISABLED as open_chest is deprecated, but is expected to return.
*)
let contract_test () =
  let open Lwt_result_syntax in
  let* block, baker, source_contract, _src2 = Contract_helpers.init () in
  let storage = "0xdeadbeef" in
  let script = Contract_helpers.read_file timelock_path in
  let* dst, _script, block =
    Contract_helpers.originate_contract_from_string_hash
      ~script
      ~storage
      ~source_contract
      ~baker
      block
  in
  let public, secret = Tezos_crypto.Timelock.gen_rsa_keys () in
  let locked_value = Tezos_crypto.Timelock.gen_locked_value public in
  let time = 1000 in
  let unlocked_value =
    Tezos_crypto.Timelock.unlock_with_secret secret ~time locked_value
  in
  let _same_unlocked, proof =
    Tezos_crypto.Timelock.unlock_and_prove_without_secret
      public
      ~time
      locked_value
  in
  let sym_key =
    Tezos_crypto.Timelock.unlocked_value_to_symmetric_key unlocked_value
  in
  let message = Bytes.of_string "this is my message" in
  let c = Tezos_crypto.Timelock.encrypt sym_key message in
  let check_storage chest chest_key expected_storage_hexa =
    let chest_key_bytes =
      "0x"
      ^ Hex.show
          (Hex.of_bytes
             (Data_encoding.Binary.to_bytes_exn
                Tezos_crypto.Timelock.chest_key_encoding
                chest_key))
    in
    let chest_bytes =
      "0x"
      ^ Hex.show
          (Hex.of_bytes
             (Data_encoding.Binary.to_bytes_exn
                Tezos_crypto.Timelock.chest_encoding
                chest))
    in
    let michelson_string =
      Format.sprintf "(Pair %s %s )" chest_key_bytes chest_bytes
    in
    let parameters =
      Alpha_context.Script.(lazy_expr (Expr.from_string michelson_string))
    in
    let fee = Test_tez.of_int 10 in
    Op.transaction
      ~fee
      (B block)
      source_contract
      (Originated dst)
      (Test_tez.of_int 3)
      ~parameters
    >>=? fun operation ->
    Incremental.begin_construction block >>=? fun incr ->
    Incremental.add_operation incr operation >>=? fun incr ->
    Incremental.finalize_block incr >>=? fun block ->
    Incremental.begin_construction block >>=? fun incr ->
    let ctxt = Incremental.alpha_ctxt incr in
    Alpha_context.Contract.get_storage ctxt dst >>= wrap
    >>=? fun (_, expr_option) ->
    let expr = expr_option |> WithExceptions.Option.get ~loc:__LOC__ in
    let bytes =
      match Micheline.root expr with
      | Micheline.Bytes (_, b) -> b
      | _ -> assert false
    in
    let to_check = Hex.show (Hex.of_bytes bytes) in
    assert (to_check = expected_storage_hexa) ;
    return_unit
  in
  let chest_key_correct = Tezos_crypto.Timelock.{unlocked_value; proof} in
  let chest_correct =
    Tezos_crypto.Timelock.{locked_value; rsa_public = public; ciphertext = c}
  in
  check_storage
    chest_correct
    chest_key_correct
    (Hex.show (Hex.of_bytes message))
  >>=? fun () ->
  (* We redo an RSA parameters generation to create incorrect cipher and proof *)
  let public_bogus, secret_bogus = Tezos_crypto.Timelock.gen_rsa_keys () in
  let locked_value_bogus =
    Tezos_crypto.Timelock.gen_locked_value public_bogus
  in
  let time = 1000 in
  let unlocked_value_bogus =
    Tezos_crypto.Timelock.unlock_with_secret
      secret_bogus
      ~time
      locked_value_bogus
  in
  let _same_unlocked, proof_bogus =
    Tezos_crypto.Timelock.unlock_and_prove_without_secret
      public
      ~time
      locked_value_bogus
  in
  let sym_key_bogus =
    Tezos_crypto.Timelock.unlocked_value_to_symmetric_key unlocked_value_bogus
  in
  let c_bogus = Tezos_crypto.Timelock.encrypt sym_key_bogus message in

  let chest_incorrect =
    Tezos_crypto.Timelock.
      {locked_value; rsa_public = public; ciphertext = c_bogus}
  in
  check_storage chest_incorrect chest_key_correct "00" >>=? fun () ->
  let chest_key_incorrect =
    Tezos_crypto.Timelock.{unlocked_value; proof = proof_bogus}
  in
  check_storage chest_correct chest_key_incorrect "01" >>=? fun () ->
  return_unit

(**
   Expect fail wrapper for tests that you expect to return Error or throw an exception.
   Useful to keep tests enabled even if they fail, but still run them.
   @param test_f test function that is expected to fail.
   *)
let expect_fail_result_lwt test_f () =
  let open Lwt_syntax in
  try
    let* res = test_f () in
    match res with
    | Ok _ -> Alcotest.fail "Expect failure"
    | Error _ -> return_ok_unit
  with _ -> return_ok_unit

let tests =
  [
    Tztest.tztest "simple test" `Quick simple_test;
    Tztest.tztest
      "verify chest_open fails origination"
      `Quick
      deprecated_chest_open;
    Tztest.tztest
      "contract test with chest_open (OK when it fails)"
      `Quick
      (expect_fail_result_lwt contract_test);
  ]
