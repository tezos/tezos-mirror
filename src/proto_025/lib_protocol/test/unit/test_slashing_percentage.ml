(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (quantities)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_slashing_percentage.ml
    Subject:    On slashing double attestations.
*)

open Protocol

let assert_equal ~loc pct1 pct2 =
  let open Lwt_result_syntax in
  let* pct1 in
  let* pct2 in
  let pct1_q = Percentage.to_q pct1 in
  let pct2_q = Percentage.to_q pct2 in
  Assert.equal_q ~loc pct1_q pct2_q

let assert_equal_int ~loc n (pct : Percentage.t tzresult Lwt.t) =
  let open Lwt_result_syntax in
  let* pct in
  let pct_q = Percentage.to_q pct in
  Assert.equal_q ~loc Q.(n // 100) pct_q

let assert_not_equal_int ~loc n (pct : Percentage.t tzresult Lwt.t) =
  let open Lwt_result_syntax in
  let* pct in
  let pct_q = Percentage.to_q pct in
  Assert.not_equal ~loc Q.equal "Values are equal" Q.pp_print Q.(n // 100) pct_q

let raw_context ~max_slashing_threshold ~max_slashing_per_block () =
  let open Constants_helpers in
  let constants =
    Default_parameters.constants_test
    |> Set.consensus_committee_size
         Default_parameters.constants_mainnet.consensus_committee_size
    |> Set.max_slashing_threshold max_slashing_threshold
    |> Set.max_slashing_per_block max_slashing_per_block
  in
  Context.raw_context_from_constants constants

let make_fake_culprits_with_rights_from_int_list il =
  let open Result_syntax in
  let n = List.length il in
  let* accounts_list = Account.generate_accounts n in
  let pkh_list = List.map (fun x -> x.Account.pkh) accounts_list in
  let pkh_rights_list = Stdlib.List.combine pkh_list il in
  let map =
    List.fold_left
      (fun map (pkh, rights) ->
        Environment.Signature.Public_key_hash.Map.add
          pkh
          (Int64.of_int rights)
          map)
      Environment.Signature.Public_key_hash.Map.empty
      pkh_rights_list
  in
  return (map, pkh_list)

let get_pct ~max_slashing_threshold ~max_slashing_per_block int_list =
  let open Lwt_result_syntax in
  (* TODO ABAAB: test with and without flag *)
  let* ctxt = raw_context ~max_slashing_threshold ~max_slashing_per_block () in
  let*? map, pkh_list = make_fake_culprits_with_rights_from_int_list int_list in
  return
  @@ Protocol.Slash_percentage.Internal_for_tests.for_double_attestation
       ctxt
       ~committee_size:
         (Int64.of_int
            Default_parameters.constants_mainnet.consensus_committee_size)
       map
       pkh_list

(** Tests that the slashing amount for several delegates is the same as long
    as the sum of their rights is the same *)
let test_list_and_sum () =
  let open Lwt_result_syntax in
  (* A max slashing threshold of 100 ensures that x -> f [x] is injective *)
  let f x =
    get_pct
      ~max_slashing_threshold:{numerator = 100; denominator = 7000}
      ~max_slashing_per_block:Percentage.p100
      x
  in
  let* () = assert_equal ~loc:__LOC__ (f [0; 0]) (f [0]) in
  let* () = assert_equal ~loc:__LOC__ (f [1; 2; 3]) (f [3; 3]) in
  let* () = assert_equal ~loc:__LOC__ (f [120]) (f [60; 60; 0]) in
  let* () = assert_equal ~loc:__LOC__ (f []) (f [0]) in
  return_unit

(** We test only one slashed delegate from now on *)
let get_pct i = get_pct [i]

(** Tests the max_slashing_per_block parameter *)
let test_max_slashing_per_block () =
  let open Lwt_result_syntax in
  let f max_slash x =
    let max_slashing_per_block =
      Percentage.of_q_bounded ~round:`Up Q.(max_slash // 100)
    in
    get_pct
      ~max_slashing_threshold:{numerator = 100; denominator = 7000}
      ~max_slashing_per_block
      x
  in
  let* () = assert_equal_int ~loc:__LOC__ 100 (f 100 200) in
  let* () = assert_equal_int ~loc:__LOC__ 1 (f 1 200) in
  let* () = assert_equal_int ~loc:__LOC__ 49 (f 49 200) in
  let* () = assert_equal_int ~loc:__LOC__ 100 (f 100 100) in
  let* () = assert_not_equal_int ~loc:__LOC__ 100 (f 100 99) in
  return_unit

(** We now test with max slashing to 100% (mainnet value) *)
let get_pct =
  get_pct
    ~max_slashing_per_block:
      Default_parameters.constants_mainnet.max_slashing_per_block

(** Tests the max_slashing_threshold parameter *)
let test_max_slashing_threshold () =
  let open Lwt_result_syntax in
  let f max_slashing_threshold x =
    get_pct
      ~max_slashing_threshold:
        {numerator = max_slashing_threshold; denominator = 7000}
      x
  in
  let* () = assert_equal_int ~loc:__LOC__ 100 (f 100 20000) in
  let* () = assert_equal_int ~loc:__LOC__ 100 (f 1000 1001) in
  let* () = assert_equal_int ~loc:__LOC__ 100 (f 1000 1000) in
  let* () = assert_not_equal_int ~loc:__LOC__ 100 (f 1000 999) in
  return_unit

(** We now test with max slashing threshold to 1/3 (mainnet value) *)
let get_pct =
  get_pct
    ~max_slashing_threshold:
      Default_parameters.constants_mainnet.max_slashing_threshold

(** Test slashing values for mainnet constants *)
let test_mainnet_values () =
  let open Lwt_result_syntax in
  let f x = get_pct x in
  (* percentage with two decimals *)
  let assert_equal_precise_int ~loc n (pct : Percentage.t tzresult Lwt.t) =
    let open Lwt_result_syntax in
    let* pct in
    let pct_q = Percentage.to_q pct in
    Assert.equal_q ~loc Q.(n // 10000) pct_q
  in
  let* () = assert_equal_precise_int ~loc:__LOC__ 0 (f 0) in
  (* For 1 right, up to 23, slash is 0.01% *)
  let* () = assert_equal_precise_int ~loc:__LOC__ 1 (f 1) in
  let* () = assert_equal_precise_int ~loc:__LOC__ 1 (f 23) in
  let* () = assert_equal_precise_int ~loc:__LOC__ 2 (f 24) in
  (* Some random value points *)
  let* () = assert_equal_precise_int ~loc:__LOC__ 19 (f 100) in
  let* () = assert_equal_precise_int ~loc:__LOC__ 459 (f 500) in
  let* () = assert_equal_precise_int ~loc:__LOC__ 1836 (f 1000) in
  (* Highest non-saturated slash is 99.92% *)
  let* () = assert_equal_precise_int ~loc:__LOC__ 9983 (f 2332) in
  let* () = assert_equal_precise_int ~loc:__LOC__ 9992 (f 2333) in
  let* () = assert_equal_precise_int ~loc:__LOC__ 10000 (f 2334) in
  let* () = assert_equal_precise_int ~loc:__LOC__ 10000 (f 7000) in
  let* () = assert_equal_precise_int ~loc:__LOC__ 10000 (f 70000) in
  return_unit

let tests =
  Tztest.
    [
      tztest "Test only sum of rights counts" `Quick test_list_and_sum;
      tztest "Test max_slashing_per_block" `Quick test_max_slashing_per_block;
      tztest "Test max_slashing_threshold" `Quick test_max_slashing_threshold;
      tztest "Test exact slashing values" `Quick test_mainnet_values;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("slashing_percentage", tests)]
  |> Lwt_main.run
