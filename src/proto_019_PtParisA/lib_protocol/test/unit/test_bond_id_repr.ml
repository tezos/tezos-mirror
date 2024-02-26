(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trilitech <contact@trili.tech>                         *)
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
    Component:    Bond_id_repr
    Invocation:   dune exec src/proto_019_PtParisA/lib_protocol/test/unit/main.exe \
                  -- --file test_bond_id_repr.ml
    Dependencies: --
    Subject:      Test bond id representations for RPC definitions.
*)

open Protocol

let assert_bond_id_result_equal ~loc =
  Assert.equal_result
    ~loc
    ~pp_ok:Bond_id_repr.pp
    ~pp_error:Format.pp_print_string
    Bond_id_repr.( = )
    ( = )

let test_destruct_sc_bond_id_repr () =
  let open Lwt_result_syntax in
  let sc_rollup_address1 = "sr1JPVatbbPoGp4vb6VfQ1jzEPMrYFcKq6VG" in
  let sc_rollup_address2 = "sr1JtMTWShgi1jLrqeHohMwLYiGizpsyWzXJ" in
  let invalid_sc_rollup_address = "sr1RWAV26caoU7oVMvetUPMt8CqvGmKtA8BO" in
  let destruct = Bond_id_repr.Internal_for_test.destruct in
  let sc_bond id =
    match Sc_rollup_repr.Address.of_b58check_opt id with
    | Some id -> Ok (Bond_id_repr.Sc_rollup_bond_id id)
    | None -> Error "Not an sc address"
  in
  let* () =
    assert_bond_id_result_equal
      ~loc:__LOC__
      (destruct sc_rollup_address1)
      (sc_bond sc_rollup_address1)
  in
  let* () =
    assert_bond_id_result_equal
      ~loc:__LOC__
      (destruct sc_rollup_address2)
      (sc_bond sc_rollup_address2)
  in
  Assert.is_error
    ~loc:__LOC__
    ~pp:Bond_id_repr.pp
    (destruct invalid_sc_rollup_address)

let test_destruct_invalid_bond_id_repr () =
  let open Lwt_result_syntax in
  let invalid_address = "asdfasdfasdf" in
  let empty_address = "" in
  let destruct = Bond_id_repr.Internal_for_test.destruct in
  let* () =
    Assert.is_error ~loc:__LOC__ ~pp:Bond_id_repr.pp (destruct invalid_address)
  in
  Assert.is_error ~loc:__LOC__ ~pp:Bond_id_repr.pp (destruct empty_address)

let test_roundtrip () =
  let open Lwt_result_syntax in
  let destruct_for_rountrip v =
    let r =
      match Bond_id_repr.Internal_for_test.destruct v with
      | Ok r -> return r
      | _ -> failwith "Destruct failed for %s" v
    in
    r
  in
  let rountrip_test loc s =
    let* r = destruct_for_rountrip s in
    let s2 = Bond_id_repr.Internal_for_test.construct r in
    Assert.equal_string ~loc s s2
  in
  let sc_rollup_address1 = "sr1JPVatbbPoGp4vb6VfQ1jzEPMrYFcKq6VG" in
  let sc_rollup_address2 = "sr1JtMTWShgi1jLrqeHohMwLYiGizpsyWzXJ" in
  let* () = rountrip_test __LOC__ sc_rollup_address1 in
  rountrip_test __LOC__ sc_rollup_address2

let tests =
  [
    Tztest.tztest
      "Deserializing sc bond ids succeeds only when id is valid"
      `Quick
      test_destruct_sc_bond_id_repr;
    Tztest.tztest
      "Deserializing invalid bond ids fails"
      `Quick
      test_destruct_invalid_bond_id_repr;
    Tztest.tztest "Deserialize/serialize roundtrip" `Quick test_roundtrip;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("Bond_id_repr.ml", tests)]
  |> Lwt_main.run
