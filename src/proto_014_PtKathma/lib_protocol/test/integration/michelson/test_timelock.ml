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
    Component:  Protocol (Timelock)
    Invocation: cd src/proto_alpha/lib_protocol/test/integration/michelson && \
                dune exec ./main.exe -- test "^timelock$"
    Subject:    On timelock
*)

open Protocol

let wrap e = Lwt.return (Environment.wrap_tzresult e)

let simple_test () =
  let public, secret = Timelock.gen_rsa_keys () in
  let locked_value = Timelock.gen_locked_value public in
  let time = 1000 in
  let unlocked_value = Timelock.unlock_with_secret secret ~time locked_value in
  let same_unlocked, proof =
    Timelock.unlock_and_prove_without_secret public ~time locked_value
  in
  assert (unlocked_value = same_unlocked) ;
  let sym_key = Timelock.unlocked_value_to_symmetric_key unlocked_value in
  let message = Bytes.create 12 in
  let c = Timelock.encrypt sym_key message in
  let expected_result = Environment.Timelock.Correct message in
  let chest_key = Timelock.{unlocked_value; proof} in
  let chest = Timelock.{locked_value; rsa_public = public; ciphertext = c} in
  let result = Environment.Timelock.open_chest chest chest_key ~time in
  assert (result = expected_result) ;
  return_unit

let contract_test () =
  (* Parse a Michelson contract from string. *)
  let originate_contract file storage src b =
    let load_file f =
      let ic = open_in f in
      let res = really_input_string ic (in_channel_length ic) in
      close_in ic ;
      res
    in
    let contract_string = load_file file in
    let code = Expr.toplevel_from_string contract_string in
    let storage = Expr.from_string storage in
    let script =
      Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}
    in
    Op.contract_origination (B b) src ~fee:(Test_tez.of_int 10) ~script
    >>=? fun (operation, dst) ->
    Incremental.begin_construction b >>=? fun incr ->
    Incremental.add_operation incr operation >>=? fun incr ->
    Incremental.finalize_block incr >|=? fun b -> (dst, b)
  in
  Context.init3 ~consensus_threshold:0 () >>=? fun (b, (src, _c2, _c3)) ->
  originate_contract "contracts/timelock.tz" "0xaa" src b >>=? fun (dst, b) ->
  let public, secret = Timelock.gen_rsa_keys () in
  let locked_value = Timelock.gen_locked_value public in
  let time = 1000 in
  let unlocked_value = Timelock.unlock_with_secret secret ~time locked_value in
  let _same_unlocked, proof =
    Timelock.unlock_and_prove_without_secret public ~time locked_value
  in
  let sym_key = Timelock.unlocked_value_to_symmetric_key unlocked_value in
  let message = Bytes.of_string "this is my message" in
  let c = Timelock.encrypt sym_key message in
  let check_storage chest chest_key expected_storage_hexa =
    let chest_key_bytes =
      "0x"
      ^ Hex.show
          (Hex.of_bytes
             (Data_encoding.Binary.to_bytes_exn
                Timelock.chest_key_encoding
                chest_key))
    in
    let chest_bytes =
      "0x"
      ^ Hex.show
          (Hex.of_bytes
             (Data_encoding.Binary.to_bytes_exn Timelock.chest_encoding chest))
    in
    let michelson_string =
      Format.sprintf "(Pair %s %s )" chest_key_bytes chest_bytes
    in
    let parameters =
      Alpha_context.Script.(lazy_expr (Expr.from_string michelson_string))
    in
    let fee = Test_tez.of_int 10 in
    Op.transaction ~fee (B b) src dst (Test_tez.of_int 3) ~parameters
    >>=? fun operation ->
    Incremental.begin_construction b >>=? fun incr ->
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
  let chest_key_correct = Timelock.{unlocked_value; proof} in
  let chest_correct =
    Timelock.{locked_value; rsa_public = public; ciphertext = c}
  in
  check_storage
    chest_correct
    chest_key_correct
    (Hex.show (Hex.of_bytes message))
  >>=? fun () ->
  (* We redo an RSA parameters generation to create incorrect cipher and proof *)
  let public_bogus, secret_bogus = Timelock.gen_rsa_keys () in
  let locked_value_bogus = Timelock.gen_locked_value public_bogus in
  let time = 1000 in
  let unlocked_value_bogus =
    Timelock.unlock_with_secret secret_bogus ~time locked_value_bogus
  in
  let _same_unlocked, proof_bogus =
    Timelock.unlock_and_prove_without_secret public ~time locked_value_bogus
  in
  let sym_key_bogus =
    Timelock.unlocked_value_to_symmetric_key unlocked_value_bogus
  in
  let c_bogus = Timelock.encrypt sym_key_bogus message in

  let chest_incorrect =
    Timelock.{locked_value; rsa_public = public; ciphertext = c_bogus}
  in
  check_storage chest_incorrect chest_key_correct "00" >>=? fun () ->
  let chest_key_incorrect = Timelock.{unlocked_value; proof = proof_bogus} in
  check_storage chest_correct chest_key_incorrect "01" >>=? fun () ->
  return_unit

let tests =
  [
    Tztest.tztest "simple test" `Quick simple_test;
    Tztest.tztest "contract test" `Quick contract_test;
  ]
