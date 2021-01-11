(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol.Alpha_context

let constants_mainnet =
  Constants.
    {
      preserved_cycles = 5;
      blocks_per_cycle = 4096l;
      blocks_per_commitment = 32l;
      blocks_per_roll_snapshot = 256l;
      blocks_per_voting_period = 20480l;
      time_between_blocks = List.map Period.of_seconds_exn [60L; 40L];
      minimal_block_delay = Period.of_seconds_exn 30L;
      endorsers_per_block = 256;
      hard_gas_limit_per_operation = Gas.Arith.(integral_of_int_exn 1_040_000);
      hard_gas_limit_per_block = Gas.Arith.(integral_of_int_exn 10_400_000);
      proof_of_work_threshold = Int64.(sub (shift_left 1L 46) 1L);
      tokens_per_roll = Tez.(mul_exn one 8_000);
      michelson_maximum_type_size = 1000;
      seed_nonce_revelation_tip =
        (match Tez.(one /? 8L) with Ok c -> c | Error _ -> assert false);
      origination_size = 257;
      block_security_deposit = Tez.(mul_exn one 640);
      endorsement_security_deposit = Tez.(mul_exn one_cent 250);
      baking_reward_per_endorsement =
        Tez.[of_mutez_exn 78_125L; of_mutez_exn 11_719L];
      endorsement_reward = Tez.[of_mutez_exn 78_125L; of_mutez_exn 52_083L];
      hard_storage_limit_per_operation = Z.of_int 60_000;
      cost_per_byte = Tez.of_mutez_exn 250L;
      quorum_min = 20_00l;
      (* quorum is in centile of a percentage *)
      quorum_max = 70_00l;
      min_proposal_quorum = 5_00l;
      initial_endorsers = 192;
      delay_per_missing_endorsement = Period.of_seconds_exn 4L;
    }

let constants_sandbox =
  Constants.
    {
      constants_mainnet with
      preserved_cycles = 2;
      blocks_per_cycle = 8l;
      blocks_per_commitment = 4l;
      blocks_per_roll_snapshot = 4l;
      blocks_per_voting_period = 64l;
      time_between_blocks = List.map Period.of_seconds_exn [1L; 0L];
      minimal_block_delay = Period.of_seconds_exn 1L;
      proof_of_work_threshold = Int64.of_int (-1);
      initial_endorsers = 1;
      delay_per_missing_endorsement = Period.of_seconds_exn 1L;
    }

let constants_test =
  Constants.
    {
      constants_mainnet with
      blocks_per_cycle = 128l;
      blocks_per_commitment = 4l;
      blocks_per_roll_snapshot = 32l;
      blocks_per_voting_period = 256l;
      time_between_blocks = List.map Period.of_seconds_exn [1L; 0L];
      minimal_block_delay = Period.of_seconds_exn 1L;
      proof_of_work_threshold = Int64.of_int (-1);
      initial_endorsers = 1;
      delay_per_missing_endorsement = Period.of_seconds_exn 1L;
    }

let bootstrap_accounts_strings =
  [ "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
    "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9";
    "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV";
    "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU";
    "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n" ]

let bootstrap_balance = Tez.of_mutez_exn 4_000_000_000_000L

let bootstrap_accounts =
  List.map
    (fun s ->
      let public_key = Signature.Public_key.of_b58check_exn s in
      let public_key_hash = Signature.Public_key.hash public_key in
      Parameters.
        {
          public_key_hash;
          public_key = Some public_key;
          amount = bootstrap_balance;
        })
    bootstrap_accounts_strings

(* TODO this could be generated from OCaml together with the faucet
   for now these are hardcoded values in the tests *)
let commitments =
  let json_result =
    Data_encoding.Json.from_string
      {json|
  [
    [ "btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", "23932454669343" ],
    [ "btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", "72954577464032" ],
    [ "btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", "217487035428348" ],
    [ "btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", "4092742372031" ],
    [ "btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", "17590039016550" ],
    [ "btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", "26322312350555" ],
    [ "btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", "244951387881443" ],
    [ "btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", "80065050465525" ],
    [ "btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", "3569618927693" ],
    [ "btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", "9034781424478" ]
  ]|json}
  in
  match json_result with
  | Error err ->
      raise (Failure err)
  | Ok json ->
      Data_encoding.Json.destruct (Data_encoding.list Commitment.encoding) json

let make_bootstrap_account (pkh, pk, amount) =
  Parameters.{public_key_hash = pkh; public_key = Some pk; amount}

let parameters_of_constants ?(bootstrap_accounts = bootstrap_accounts)
    ?(bootstrap_contracts = []) ?(with_commitments = false) constants =
  let commitments = if with_commitments then commitments else [] in
  Parameters.
    {
      bootstrap_accounts;
      bootstrap_contracts;
      commitments;
      constants;
      security_deposit_ramp_up_cycles = None;
      no_reward_cycles = None;
    }

let json_of_parameters parameters =
  Data_encoding.Json.construct Parameters.encoding parameters
