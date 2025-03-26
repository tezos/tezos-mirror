(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Lib_crypto_dal Test_dal_cryptobox
    Invocation: dune exec src/lib_crypto_dal/test/main.exe -- --file test_trap.ml
    Subject:    Tests the Trap module

*)

(* The [test_share_is_trap_stats] test checks that the fraction f=10% of "traps"
   among N=10,000 randomly generated samples is within p=±10% of the expected
   value (i.e., between 900 and 1,100 traps, assuming a true trap fraction f).

   Even if the trap detection function is "perfect" — meaning it correctly
   classifies each data sample as a "trap" or "non-trap" according to the
   intended probability f — there is still some natural variation due to
   randomness in the sampling process.

   The probability of observing exactly `k` traps in the N samples follows the
   binomial distribution:

       P(k traps) = C(N, k) * f^k * (1 - f)^(N - k)

   Therefore, the probability that the number of traps falls in the acceptable
   range — i.e. with low = (1 - p) * N = 900 and high = (1 + p) * N = 1100 is:

       P(pass) = sum_{k=low}^{high} [P(k traps)]

   This probability is approximately 99.92%. So even if the trap function is
   "perfect", the test has a 0.08% chance of failing due to the variation in the
   randomly generated samples. *)

let get_commitment_and_shards_with_proofs cryptobox ~slot =
  let res =
    Cryptobox.Internal_for_tests.get_commitment_and_shards_with_proofs
      cryptobox
      ~slot
  in
  match res with
  | Error err ->
      Test.fail ~__LOC__ "Unexpected error:@.%a@." Cryptobox.pp_error err
  | Ok v -> v

(* The Rio parameters; see src/proto_022_PsRiotum/lib_parameters/default_parameters.ml *)
let slot_size = 126_944

let number_of_shards = 512

let parameters =
  {
    Cryptobox.page_size = 3967;
    slot_size;
    redundancy_factor = 8;
    number_of_shards;
  }

(* This value is much bigger than the one used in production. However, to check
   for smaller values, we would need more samples, and that would make the test
   slower. Testing for this values should already provide some assurance that
   the trap checking function behaves correctly. *)
let traps_fraction = Q.(1 // 10)

let setup () =
  let open Lwt_result_syntax in
  Log.info "Loading SRS..." ;
  let* () =
    Cryptobox.init_prover_dal
      ~find_srs_files:Tezos_base.Dal_srs.find_trusted_setup_files
      ~fetch_trusted_setup:false
      ()
  in
  assert (Cryptobox.Internal_for_tests.ensure_validity parameters) ;
  let*? cryptobox =
    let res = Cryptobox.make parameters in
    match res with Ok v -> Ok v | Error (`Fail str) -> error_with "%s" str
  in
  Log.info "The cryptobox is available" ;
  return cryptobox

let test_share_is_trap_stats () =
  let open Lwt_result_syntax in
  let* cryptobox = setup () in

  (* some real public key hashes *)
  let pkhs =
    [
      "tz1irJKkXS2DBWkU1NnmFQx1c1L7pbGg4yhk";
      "tz1aRoaRhSpRYvFdyvgWLL6TGyRoGF51wDjM";
      "tz3cqThj23Feu55KDynm7Vg81mCMpWDgzQZq";
      "tz3Zhs2dygr55yHyQyKjntAtY9bgfhLZ4Xj1";
      "tz1N7fL6KvRzD5Umy7yeX5uBa5wFsC5veuN2";
    ]
  in
  let*? pkhs =
    List.map_e Tezos_crypto.Signature.Public_key_hash.of_b58check pkhs
  in

  (* The number of shards to be generated and used is such that there should be
     1000 traps in total. *)
  let num_samples = Q.(of_int 1000 * (one / traps_fraction) |> to_int) in
  let number_of_slots = 1 + (num_samples / number_of_shards) in
  let total_shards = number_of_slots * number_of_shards in

  Log.info
    "Check for traps_fraction %s and %d shards (%d slots) "
    (Q.to_string traps_fraction)
    total_shards
    number_of_slots ;

  (* Generate random slots and count traps per pkh. *)
  let num_traps = Array.make (List.length pkhs) 0 in
  let count_traps pkh shards ~traps_fraction =
    Seq.filter
      (fun (Cryptobox.{share; _}, _proof) ->
        match Trap.share_is_trap pkh share ~traps_fraction with
        | Ok true -> true
        | _ -> false)
      shards
    |> Seq.length
  in
  let rec iter_on_slots slot_index =
    let () = Log.debug "  slot %d" slot_index in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let _commitment, _proof, shards =
      get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    List.iteri
      (fun pkh_index pkh ->
        num_traps.(pkh_index) <-
          num_traps.(pkh_index) + count_traps pkh shards ~traps_fraction)
      pkhs ;
    if slot_index >= number_of_slots then
      (* Return the last slot's shards, for checking the edge cases. *)
      shards
    else iter_on_slots (slot_index + 1)
  in
  let shards = iter_on_slots 1 in

  (* First check for the edge cases when the [traps_fraction] is 0 or 1. *)
  let num_traps_0 =
    count_traps (Stdlib.List.hd pkhs) shards ~traps_fraction:Q.zero
  in
  Check.(
    (num_traps_0 = 0) int ~__LOC__ ~error_msg:"Expected zero traps, got %L") ;
  let num_traps_1 =
    count_traps (Stdlib.List.hd pkhs) shards ~traps_fraction:Q.one
  in
  Check.(
    (num_traps_1 = number_of_shards)
      int
      ~__LOC__
      ~error_msg:"Expected %R traps, got %L") ;

  (* Now do the checks for the real [traps_fraction]. *)
  let expected_traps = Q.(traps_fraction * of_int total_shards |> to_int) in
  (* We allow a deviation of 10% from the expected number of traps. *)
  let tolerance =
    Q.(traps_fraction * of_int total_shards * (10 // 100) |> to_int)
  in
  Log.info
    "expected number of traps = %d, tolerated difference = %d"
    expected_traps
    tolerance ;
  List.iteri
    (fun pkh_index pkh ->
      Log.info
        "number of traps for %a: %d"
        Tezos_crypto.Signature.Public_key_hash.pp
        pkh
        num_traps.(pkh_index) ;
      if abs (expected_traps - num_traps.(pkh_index)) > tolerance then
        Test.fail
          "Unexpected traps count for %a and traps fraction %s: expected %d \
           but got %d (tolerance: %d)"
          Tezos_crypto.Signature.Public_key_hash.pp
          pkh
          (Q.to_string traps_fraction)
          expected_traps
          num_traps.(pkh_index)
          tolerance)
    pkhs ;
  return_unit

let tests =
  [Tztest.tztest "share_is_trap statistics" `Slow test_share_is_trap_stats]

let () =
  (* Seed for deterministic pseudo-randomness:
      If the environment variable RANDOM_SEED is set, then its value is used as
      as seed. Otherwise, a random seed is used.
     WARNING: using [Random.self_init] elsewhere in the tests breaks the determinism.
  *)
  let seed =
    match Sys.getenv_opt "RANDOM_SEED" with
    | None ->
        Random.self_init () ;
        Random.int 1073741823
    | Some v -> int_of_string v
  in
  Random.init seed ;
  Alcotest_lwt.run ~__FILE__ "DAL.Trap" [("share_is_trap", tests)]
  |> Lwt_main.run
