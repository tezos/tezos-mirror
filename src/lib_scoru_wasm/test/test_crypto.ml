(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Trili Tech  <contact@trili.tech>                       *)
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
    Component:    Cryptographic_host_functions
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_crypto.ml
    Subject:      Cryptographic host functions tests for the tezos-scoru-wasm library
*)

open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
open Wasm_utils

let test_ec_pairing_check ~version () =
  let open Lwt_syntax in
  let paring_check_with_point point1_x point1_y point2_x point2_y =
    let* durable = make_durable [] in
    let addr1 = 10l in
    let addr2 = Int32.add addr1 48l in
    let addr3 = Int32.add addr2 96l in
    let addr4 = Int32.add addr3 48l in

    let src = 10l in
    let module_reg, module_key, host_funcs_registry =
      make_module_inst
        ~version
        (List.map
           Bytes.to_string
           [
             Bls12_381.G1.to_compressed_bytes point1_x;
             Bls12_381.G2.to_compressed_bytes point1_y;
             Bls12_381.G1.to_compressed_bytes point2_x;
             Bls12_381.G2.to_compressed_bytes point2_y;
           ])
        src
    in

    let values =
      Values.
        [Num (I32 addr1); Num (I32 addr2); Num (I32 addr3); Num (I32 addr4)]
    in

    let* _, res =
      Eval.invoke
        ~module_reg
        ~caller:module_key
        ~durable
        host_funcs_registry
        Host_funcs.Internal_for_tests.ec_pairing_check_bls12_381
        values
    in

    let expected_result =
      if
        Bls12_381.Pairing.pairing_check
          [(point1_x, point1_y); (point2_x, point2_y)]
      then 1l
      else 0l
    in

    assert (res = [Num (I32 expected_result)]) ;

    Lwt.return_ok ()
  in

  let* _ =
    paring_check_with_point
      Bls12_381.G1.zero
      Bls12_381.G2.zero
      Bls12_381.G1.zero
      Bls12_381.G2.zero
  in

  let* _ =
    paring_check_with_point
      Bls12_381.G1.one
      Bls12_381.G2.one
      Bls12_381.G1.one
      Bls12_381.G2.one
  in

  Lwt.return_ok ()

let tests =
  Tztest_helper.tztests_with_all_pvms
    [("ec_pairing_check", `Quick, test_ec_pairing_check)]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("Crypto", tests)]
  |> Lwt_main.run
