(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** These generators aims at generating operations which are not
    necessary correct. The goal is to tests functions such as {!
    Operation.compare} with as much as possible parameters that play a
    role in operation [weight] computation.

    When adding a new operation, one should also add its weight
    computation, hence knows which kind of generator should be provided
    for this new operation.*)

open Protocol
open Alpha_context

(** {2 Operations kind labelling.} *)

let consensus_pass = `PConsensus

let anonymous_pass = `PAnonymous

let vote_pass = `PVote

let manager_pass = `PManager

let all_passes = [`PConsensus; `PAnonymous; `PVote; `PManager]

let consensus_kinds = [`KPreendorsement; `KEndorsement; `KDal_slot]

let anonymous_kinds =
  [
    `KSeed_nonce_revelation;
    `KVdf_revelation;
    `KDouble_endorsement;
    `KDouble_preendorsement;
    `KDouble_baking;
    `KActivate_account;
  ]

let vote_kinds = [`KProposals; `KBallot]

(* N.b. we do not consider Failing_noop as those will never be valid. *)
let manager_kinds =
  [
    `KReveal;
    `KTransaction;
    `KOrigination;
    `KDelegation;
    `KSet_deposits_limit;
    `KIncrease_paid_storage;
    `KRegister_global_constant;
    `KTx_rollup_origination;
    `KTx_rollup_submit_batch;
    `KTx_rollup_commit;
    `KTx_rollup_return_bond;
    `KTx_rollup_finalize_commitment;
    `KTx_rollup_remove_commitment;
    `KTx_rollup_rejection;
    `KTx_rollup_dispatch_tickets;
    `KTransfer_ticket;
    `KDal_publish_slot_header;
    `KSc_rollup_originate;
    `KSc_rollup_add_messages;
    `KSc_rollup_cement;
    `KSc_rollup_publish;
    `KSc_rollup_refute;
    `KSc_rollup_timeout;
    `KSc_rollup_execute_outbox_message;
    `KSc_rollup_recover_bond;
    `KSc_rollup_dal_slot_subscribe;
  ]

let pass_to_operation_kinds = function
  | `PConsensus -> consensus_kinds
  | `PVote -> vote_kinds
  | `PAnonymous -> anonymous_kinds
  | `PManager -> [`KManager]

let pp_kind fmt k =
  Format.fprintf
    fmt
    "%s"
    (match k with
    | `KPreendorsement -> "KPreendorsement"
    | `KEndorsement -> "KEndorsement"
    | `KDal_slot -> "KDal_slot"
    | `KSeed_nonce_revelation -> "KSeed_nonce_revelation"
    | `KVdf_revelation -> "KVdf_revelation"
    | `KDouble_endorsement -> "KDouble_endorsement"
    | `KDouble_preendorsement -> "KDouble_preendorsement"
    | `KDouble_baking -> "KDouble_baking"
    | `KActivate_account -> "KActivate_account"
    | `KProposals -> "KProposals"
    | `KBallot -> "KBallot"
    | `KManager -> "KManager")

(** {2 Generators} *)

(** {3 Selection in hashes list} *)

let block_hashes =
  List.map
    Block_hash.of_b58check_exn
    [
      "BLbcVY1kYiKQy2MJJfoHJMN2xRk5QPG1PEKWMDSyW2JMxBsMmiL";
      "BLFhLKqQQn32Cc9QXqtEqysYqWNCowNKaypVHP5zEyZcywbXcHo";
      "BLuurCvGmNPTzXSnGCpcFPy5h8A49PwH2LnfAWBnp5R1qv5czwe";
    ]

let payload_hashes =
  List.map
    Block_payload_hash.of_b58check_exn
    [
      "vh2gWcSUUhJBwvjx4vS7JN5ioMVWpHCSK6W2MKNPr5dn6NUdfFDQ";
      "vh1p1VzeYjZLEW6WDqdTwVy354KEmGCDgPmagEKcLN4NT4X58mNk";
      "vh2TyrWeZ2dydEy9ZjmvrjQvyCs5sdHZPypcZrXDUSM1tNuPermf";
    ]

let random_payload_hash = QCheck2.Gen.oneofl payload_hashes

let signatures =
  List.map
    Signature.of_b58check_exn
    [
      "sigaNsiye7D8dJHKSQZBwDbS2aQNXipDP7bw8uQnMgnaXi5pcnoPZRKXrDeFRx4FjWJD2xfyUA9CuBXhwPHhVs7LxkL4vT32";
      "sigvtPBMQvk2DgNtu3AKFU1ZRsagGxsoiZVQyQhJNEojReBY2vE5sDwt3H7Mh8RMe27QHBjemxqhMVVszZqpNsdDux6KAELX";
      "sighje7pEbUUwGtJ4GTP7uzMZe5SFz6dRRC3BvZBHnrRHnc47WHGnVdfiscHPMek7esmj7saTuj54QBWy3SezyA2EGbHkmW5";
    ]

let random_signature = QCheck2.Gen.oneofl signatures

let pkhs =
  List.map
    Signature.Public_key_hash.of_b58check_exn
    [
      "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
      "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv";
      "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU";
    ]

let random_pkh = QCheck2.Gen.oneofl pkhs

let pks =
  List.map
    Signature.Public_key.of_b58check_exn
    [
      "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2";
      "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n";
      "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU";
    ]

let random_pk = QCheck2.Gen.oneofl pks

let contract_hashes =
  List.map
    Contract_hash.of_b58check_exn
    [
      "KT1WvzYHCNBvDSdwafTHv7nJ1dWmZ8GCYuuC";
      "KT1NkWx47WzJeHCSyB62WjLtFn4tRf3uXBur";
      "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
    ]

let random_contract_hash = QCheck2.Gen.oneofl contract_hashes

let block_headers =
  let bh1 =
    {json|{ "level": 2, "proto": 1, "predecessor": "BLbcVY1kYiKQy2MJJfoHJMN2xRk5QPG1PEKWMDSyW2JMxBsMmiL", "timestamp": "2022-08-08T11:16:30Z", "validation_pass": 4, "operations_hash": "LLoa7bxRTKaQN2bLYoitYB6bU2DvLnBAqrVjZcvJ364cTcX2PZYKU", "fitness": [ "02", "00000002", "", "ffffffff", "00000001" ], "context": "CoUvpF8XBUfz3w9CJumt4ZKGZkrcdcfs1Qdrrd1ZeFij64E1QCud", "payload_hash": "vh2TyrWeZ2dydEy9ZjmvrjQvyCs5sdHZPypcZrXDUSM1tNuPermf", "payload_round": 1, "proof_of_work_nonce": "62de1e0d00000000", "liquidity_baking_toggle_vote": "pass", "signature": "sigaXGo4DWsZwo1SvbKCp2hLgE5jcwd61Ufkc3iMt3sXy3NBj9jticuJKJnRhyH2ZPJQMwEuDqQTgZgoK5xRH6HeF7YxLb4u" }|json}
  in
  let bh2 =
    {json|{ "level": 3, "proto": 1, "predecessor": "BLAUNUbzKHgA4DYQEXCbxY73wdE2roGAzvJJbFp8dQe62Ekpada", "timestamp": "2022-08-08T11:16:32Z", "validation_pass": 4, "operations_hash": "LLoaWjBX8Cm8DVpoLNtm7FPNnxUdL6Dakq122pVfNHYaf2rE9GQXi", "fitness": [ "02", "00000003", "", "fffffffe", "00000000" ], "context": "CoUtWowJUqXwMm4pbR1jjyFfVRHqRHGs6bYVDaaByvbmULoAND2x", "payload_hash": "vh1p1VzeYjZLEW6WDqdTwVy354KEmGCDgPmagEKcLN4NT4X58mNk", "payload_round": 0, "proof_of_work_nonce": "62de1e0d00000000", "liquidity_baking_toggle_vote": "pass", "signature": "sigVqWWE7BPuxHqPWiVRmzQ1eMZZAPAxGJ94ytY2sjV8Y1Z4QH1F2bPGZS1ZeWDbqmcppPPFobRpi7wNasQ17Mm9CFGKag2t" }|json}
  in
  let bh3 =
    {json|{ "level": 4, "proto": 1, "predecessor": "BLuurCvGmNPTzXSnGCpcFPy5h8A49PwH2LnfAWBnp5R1qv5czwe", "timestamp": "2022-08-08T11:16:33Z", "validation_pass": 4, "operations_hash": "LLoaf8AANzyNxhk715zykDrwG5Bpqw6FsZLWWNp2Dcm3ewFrcc3Wc", "fitness": [ "02", "00000004", "", "ffffffff", "00000000" ], "context": "CoVzxEBMDhxpGVxrguik6r5qVogJBFyhuvwm2KZBcsmvqhekPiwL", "payload_hash": "vh2gWcSUUhJBwvjx4vS7JN5ioMVWpHCSK6W2MKNPr5dn6NUdfFDQ", "payload_round": 0, "proof_of_work_nonce": "62de1e0d00000000", "seed_nonce_hash": "nceV3VjdHp1yk6uqcQicQBxLJY1AfWvLSabQpqnpiqkC1q2tS35EN", "liquidity_baking_toggle_vote": "pass", "signature": "sigijumaDLSQwjh2AKK7af1VcEDsZsRwbweL8hF176puhHy3ySVocNCbrwPqJLiQP8EbqY5YL6z6b1vDaw12h8MQU2Rh4SW1" }|json}
  in
  List.map
    (fun s ->
      let open Data_encoding.Json in
      from_string s |> function
      | Ok json -> destruct Alpha_context.Block_header.encoding json
      | Error _ -> assert false)
    [bh1; bh2; bh3]

let random_block_header = QCheck2.Gen.oneofl block_headers

let tx_rollups =
  List.filter_map
    Tx_rollup.of_b58check_opt
    [
      "txr1hFmPcr5y1P2xTm7W2y1sfjLLCUdzCZGvg";
      "txr1jux4nZWf8ToGZc4ojLBbT538BBTTSiJUD";
      "txr1TAFTENC2YACvoMDrpJHCbdvdfSSjcjEjc";
    ]

let random_tx_rollup = QCheck2.Gen.oneofl tx_rollups

let sc_rollups =
  List.map
    Sc_rollup.Address.of_b58check_exn
    [
      "scr1FPSu51gGtyv9S5HqDqXeH16DJviJ9qpr6";
      "scr1U39BVdpVQun1QjjiXfd3XgoBKcenWt5sb";
      "scr1Kqqbvust2adJMtSu2V4fcd49oQHug4BLb";
    ]

let random_sc_rollup = QCheck2.Gen.oneofl sc_rollups

let protos =
  List.map
    (fun s -> Protocol_hash.of_b58check_exn s)
    [
      "ProtoALphaALphaALphaALphaALphaALphaALpha61322gcLUGH";
      "ProtoALphaALphaALphaALphaALphaALphaALphabc2a7ebx6WB";
      "ProtoALphaALphaALphaALphaALphaALphaALpha84efbeiF6cm";
      "ProtoALphaALphaALphaALphaALphaALphaALpha91249Z65tWS";
      "ProtoALphaALphaALphaALphaALphaALphaALpha537f5h25LnN";
      "ProtoALphaALphaALphaALphaALphaALphaALpha5c8fefgDYkr";
      "ProtoALphaALphaALphaALphaALphaALphaALpha3f31feSSarC";
      "ProtoALphaALphaALphaALphaALphaALphaALphabe31ahnkxSC";
      "ProtoALphaALphaALphaALphaALphaALphaALphabab3bgRb7zQ";
      "ProtoALphaALphaALphaALphaALphaALphaALphaf8d39cctbpk";
      "ProtoALphaALphaALphaALphaALphaALphaALpha3b981byuYxD";
      "ProtoALphaALphaALphaALphaALphaALphaALphaa116bccYowi";
      "ProtoALphaALphaALphaALphaALphaALphaALphacce68eHqboj";
      "ProtoALphaALphaALphaALphaALphaALphaALpha225c7YrWwR7";
      "ProtoALphaALphaALphaALphaALphaALphaALpha58743cJL6FG";
      "ProtoALphaALphaALphaALphaALphaALphaALphac91bcdvmJFR";
      "ProtoALphaALphaALphaALphaALphaALphaALpha1faaadhV7oW";
      "ProtoALphaALphaALphaALphaALphaALphaALpha98232gD94QJ";
      "ProtoALphaALphaALphaALphaALphaALphaALpha9d1d8cijvAh";
      "ProtoALphaALphaALphaALphaALphaALphaALphaeec52dKF6Gx";
      "ProtoALphaALphaALphaALphaALphaALphaALpha841f2cQqajX";
    ]

let random_proto = QCheck2.Gen.oneofl protos

let codes =
  List.filter_map
    Blinded_public_key_hash.activation_code_of_hex
    [
      "41f98b15efc63fa893d61d7d6eee4a2ce9427ac4";
      "411dfef031eeecc506de71c9df9f8e44297cf5ba";
      "08d7d355bc3391d12d140780b39717d9f46fcf87";
    ]

let random_code = QCheck2.Gen.oneofl codes

(** {2 Operations parameters generators} *)

let random_shell : Tezos_base.Operation.shell_header QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ branch = oneofl block_hashes in
  Tezos_base.Operation.{branch}

let gen_slot =
  let open QCheck2.Gen in
  let+ i = small_nat in
  match Slot.Internal_for_tests.of_int i with
  | Ok slot -> slot
  | Error _ -> assert false

let gen_level =
  let open QCheck2.Gen in
  let+ i = ui32 in
  match Raw_level.of_int32 i with Ok v -> v | Error _ -> assert false

let gen_round =
  let open QCheck2.Gen in
  let+ i = ui32 in
  match Round.of_int32 i with Ok v -> v | Error _ -> assert false

let generate_consensus_content : consensus_content QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* slot = gen_slot in
  let* level = gen_level in
  let* round = gen_round in
  let+ block_payload_hash = random_payload_hash in
  {slot; level; round; block_payload_hash}

let gen_tez =
  let open QCheck2.Gen in
  let+ i = ui64 in
  match Tez.of_mutez i with None -> Tez.zero | Some v -> v

let gen_fee = gen_tez

let gen_amount = gen_tez

let gen_amount_in_bytes =
  let open QCheck2.Gen in
  let+ i = nat in
  Z.of_int i

let random_contract =
  let open QCheck2.Gen in
  let* b = bool in
  if b then
    let+ pkh = random_pkh in
    Contract.Implicit pkh
  else
    let+ contract_hash = random_contract_hash in
    Contract.Originated contract_hash

let random_contract_hash = QCheck2.Gen.oneofl contract_hashes

let gen_counters =
  let open QCheck2.Gen in
  let+ i = nat in
  Z.of_int i

let gen_gas_limit =
  let open QCheck2.Gen in
  let+ i = nat in
  Gas.Arith.integral_of_int_exn i

let gen_storage_limit =
  let open QCheck2.Gen in
  let+ i = nat in
  Z.of_int i

let nonces =
  List.map
    (fun i ->
      let b = Bytes.create 32 in
      Bytes.set_int8 b 0 i ;
      Alpha_context.Nonce.of_bytes b |> function
      | Ok v -> v
      | Error _ -> assert false)
    [1; 2; 3]

let random_nonce = QCheck2.Gen.oneofl nonces

let vdf_solutions =
  let open Environment.Vdf in
  let opt_assert = function Some v -> v | None -> assert false in
  List.map
    (fun i ->
      let b = Bytes.create form_size_bytes in
      Bytes.set_int8 b 0 i ;
      let result = result_of_bytes_opt b |> opt_assert in
      let proof = proof_of_bytes_opt b |> opt_assert in
      (result, proof))
    [1; 2; 3]

(** {2 Generators for each Operation Kind} *)

let wrap_operation sh (pdata : 'kind protocol_data) : 'kind operation =
  {shell = sh; protocol_data = pdata}

let generate_op (gen_op : 'kind contents QCheck2.Gen.t) :
    'kind operation QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* op = gen_op in
  let* signature = random_signature in
  let+ shell = random_shell in
  let contents = Single op in
  let signature = Some signature in
  let protocol_data = {contents; signature} in
  wrap_operation shell protocol_data

let generate_operation gen_op =
  let open QCheck2.Gen in
  let+ op = generate_op gen_op in
  Operation.pack op

let generate_preendorsement =
  let open QCheck2.Gen in
  let+ cc = generate_consensus_content in
  Preendorsement cc

let generate_endorsement =
  let open QCheck2.Gen in
  let+ cc = generate_consensus_content in
  Endorsement cc

let generate_dal_slot =
  let open QCheck2.Gen in
  let+ pkh = random_pkh in
  Dal_slot_availability (pkh, Dal.Endorsement.empty)

let generate_vdf_revelation =
  let open QCheck2.Gen in
  let+ solution = oneofl vdf_solutions in
  Vdf_revelation {solution}

let generate_seed_nonce_revelation =
  let open QCheck2.Gen in
  let* level = gen_level in
  let+ nonce = random_nonce in
  Seed_nonce_revelation {level; nonce}

let generate_double_preendorsement =
  let open QCheck2.Gen in
  let* op1 = generate_op generate_preendorsement in
  let+ op2 = generate_op generate_preendorsement in
  Double_preendorsement_evidence {op1; op2}

let generate_double_endorsement =
  let open QCheck2.Gen in
  let* op1 = generate_op generate_endorsement in
  let+ op2 = generate_op generate_endorsement in
  Double_endorsement_evidence {op1; op2}

let generate_double_baking =
  let open QCheck2.Gen in
  let* bh1 = random_block_header in
  let+ bh2 = random_block_header in
  Double_baking_evidence {bh1; bh2}

let generate_activate_account =
  let open QCheck2.Gen in
  let* activation_code = random_code in
  let+ id = random_pkh in
  let id = match id with Signature.Ed25519 pkh -> pkh | _ -> assert false in
  Activate_account {id; activation_code}

let random_period =
  let open QCheck2.Gen in
  let+ i = ui32 in
  i

let generate_proposals =
  let open QCheck2.Gen in
  let* source = random_pkh in
  let+ period = random_period in
  let proposals = [] in
  Proposals {source; period; proposals}

let generate_ballot =
  let open QCheck2.Gen in
  let* source = random_pkh in
  let* period = random_period in
  let+ proposal = random_proto in
  let ballot = Vote.Pass in
  Ballot {source; period; proposal; ballot}

let generate_manager_aux ?source gen_manop =
  let open QCheck2.Gen in
  let* source =
    match source with None -> random_pkh | Some source -> return source
  in
  let* fee = gen_fee in
  let* counter = gen_counters in
  let* gas_limit = gen_gas_limit in
  let* storage_limit = gen_storage_limit in
  let+ operation = gen_manop in
  Manager_operation {source; fee; counter; operation; gas_limit; storage_limit}

let generate_manager ?source gen_manop =
  generate_op (generate_manager_aux ?source gen_manop)

let generate_manager_operation ?source gen_manop =
  let open QCheck2.Gen in
  let+ manop = generate_manager ?source gen_manop in
  Operation.pack manop

let generate_reveal =
  let open QCheck2.Gen in
  let+ pk = random_pk in
  Reveal pk

let generate_transaction =
  let open QCheck2.Gen in
  let* amount = gen_amount in
  let+ destination = random_contract in
  let parameters = Script.unit_parameter in
  let entrypoint = Entrypoint.default in
  Transaction {amount; parameters; entrypoint; destination}

let generate_origination =
  let open QCheck2.Gen in
  let+ credit = gen_amount in
  let delegate = None in
  let script = Script.{code = unit_parameter; storage = unit_parameter} in
  Origination {delegate; script; credit}

let generate_delegation =
  let open QCheck2.Gen in
  let+ delegate = option random_pkh in
  Delegation delegate

let generate_increase_paid_storage =
  let open QCheck2.Gen in
  let* amount_in_bytes = gen_amount_in_bytes in
  let+ destination = random_contract_hash in
  Increase_paid_storage {amount_in_bytes; destination}

let generate_set_deposits_limit =
  let open QCheck2.Gen in
  let+ amount_opt = option gen_amount in
  Set_deposits_limit amount_opt

let generate_register_global_constant =
  let value = Script_repr.lazy_expr (Expr.from_string "Pair 1 2") in
  QCheck2.Gen.pure (Register_global_constant {value})

let generate_tx_rollup_origination = QCheck2.Gen.pure Tx_rollup_origination

let generate_tx_rollup_submit_batch =
  let open QCheck2.Gen in
  let+ tx_rollup = random_tx_rollup in
  let content = "batch" in
  let burn_limit = None in
  Tx_rollup_submit_batch {tx_rollup; content; burn_limit}

let generate_tx_rollup_commit =
  let open QCheck2.Gen in
  let+ tx_rollup = random_tx_rollup in
  let commitment : Tx_rollup_commitment.Full.t =
    {
      level = Tx_rollup_level.root;
      messages = [];
      predecessor = None;
      inbox_merkle_root = Tx_rollup_inbox.Merkle.merklize_list [];
    }
  in
  Tx_rollup_commit {tx_rollup; commitment}

let generate_tx_rollup_return_bond =
  let open QCheck2.Gen in
  let+ tx_rollup = random_tx_rollup in
  Tx_rollup_return_bond {tx_rollup}

let generate_tx_finalize_commitment =
  let open QCheck2.Gen in
  let+ tx_rollup = random_tx_rollup in
  Tx_rollup_finalize_commitment {tx_rollup}

let generate_tx_rollup_remove_commitment =
  let open QCheck2.Gen in
  let+ tx_rollup = random_tx_rollup in
  Tx_rollup_remove_commitment {tx_rollup}

let generate_tx_rollup_rejection =
  let open QCheck2.Gen in
  let+ tx_rollup = random_tx_rollup in
  let message, _ = Tx_rollup_message.make_batch "" in
  let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
  let message_path =
    match Tx_rollup_inbox.Merkle.compute_path [message_hash] 0 with
    | Ok message_path -> message_path
    | _ -> raise (Invalid_argument "Single_message_inbox.message_path")
  in
  let proof : Tx_rollup_l2_proof.t =
    {
      version = 1;
      before = `Value Tx_rollup_message_result.empty_l2_context_hash;
      after = `Value Context_hash.zero;
      state = Seq.empty;
    }
  in
  let previous_message_result : Tx_rollup_message_result.t =
    {
      context_hash = Tx_rollup_message_result.empty_l2_context_hash;
      withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
    }
  in
  let level = Tx_rollup_level.root in
  let message_result_hash = Tx_rollup_message_result_hash.zero in
  let message_result_path = Tx_rollup_commitment.Merkle.dummy_path in
  let previous_message_result_path = Tx_rollup_commitment.Merkle.dummy_path in
  let message_position = 0 in
  let proof = Tx_rollup_l2_proof.serialize_proof_exn proof in
  Tx_rollup_rejection
    {
      tx_rollup;
      level;
      message;
      message_position;
      message_path;
      message_result_hash;
      message_result_path;
      previous_message_result;
      previous_message_result_path;
      proof;
    }

let generate_tx_dispatch_tickets =
  let open QCheck2.Gen in
  let* tx_rollup = random_tx_rollup in
  let* source = random_pkh in
  let+ contract = random_contract in
  let level = Tx_rollup_level.root in
  let message_index = 0 in
  let message_result_path = Tx_rollup_commitment.Merkle.dummy_path in
  let context_hash = Context_hash.zero in
  let reveal =
    Tx_rollup_reveal.
      {
        contents = Script.lazy_expr (Expr.from_string "1");
        ty = Script.lazy_expr (Expr.from_string "nat");
        ticketer = contract;
        amount = Tx_rollup_l2_qty.of_int64_exn 10L;
        claimer = source;
      }
  in
  let tickets_info = [reveal] in
  Tx_rollup_dispatch_tickets
    {
      tx_rollup;
      level;
      context_hash;
      message_index;
      message_result_path;
      tickets_info;
    }

let generate_transfer_ticket =
  let open QCheck2.Gen in
  let* ticketer = random_contract in
  let* destination = random_contract in
  let+ amount = gen_counters in
  let amount =
    Option.value (Ticket_amount.of_zint amount) ~default:Ticket_amount.one
  in
  let contents = Script.lazy_expr (Expr.from_string "1") in
  let ty = Script.lazy_expr (Expr.from_string "nat") in
  let entrypoint = Entrypoint.default in
  Transfer_ticket {contents; ty; ticketer; amount; destination; entrypoint}

let generate_dal_publish_slot_header =
  let published_level = Alpha_context.Raw_level.of_int32_exn Int32.zero in
  let index = Alpha_context.Dal.Slot_index.zero in
  let commitment = Alpha_context.Dal.Slot.Commitment.zero in
  let slot_header =
    Alpha_context.Dal.Slot.Header.{id = {published_level; index}; commitment}
  in
  QCheck2.Gen.pure (Dal_publish_slot_header {slot_header})

let generate_sc_rollup_originate =
  let kind = Sc_rollup.Kind.Example_arith in
  let boot_sector = "" in
  let parameters_ty = Script.lazy_expr (Expr.from_string "1") in
  let origination_proof =
    Lwt_main.run (Sc_rollup_helpers.origination_proof ~boot_sector kind)
  in
  let (module PVM) = Sc_rollup.wrapped_proof_module origination_proof in
  let origination_proof =
    Data_encoding.Binary.to_string_exn PVM.proof_encoding PVM.proof
  in
  QCheck2.Gen.pure
    (Sc_rollup_originate {kind; boot_sector; origination_proof; parameters_ty})

let generate_sc_rollup_add_messages =
  let open QCheck2.Gen in
  return (Sc_rollup_add_messages {messages = []})

let sc_dummy_commitment =
  let number_of_ticks =
    match Sc_rollup.Number_of_ticks.of_value 3000L with
    | None -> assert false
    | Some x -> x
  in
  Sc_rollup.Commitment.
    {
      predecessor = Sc_rollup.Commitment.Hash.zero;
      inbox_level = Raw_level.of_int32_exn Int32.zero;
      number_of_ticks;
      compressed_state = Sc_rollup.State_hash.zero;
    }

let generate_sc_rollup_cement =
  let open QCheck2.Gen in
  let+ rollup = random_sc_rollup in
  let commitment = Sc_rollup.Commitment.hash_uncarbonated sc_dummy_commitment in
  Sc_rollup_cement {rollup; commitment}

let generate_sc_rollup_publish =
  let open QCheck2.Gen in
  let+ rollup = random_sc_rollup in
  let commitment = sc_dummy_commitment in
  Sc_rollup_publish {rollup; commitment}

let generate_sc_rollup_refute =
  let open QCheck2.Gen in
  let* opponent = random_pkh in
  let+ rollup = random_sc_rollup in
  let refutation : Sc_rollup.Game.refutation option =
    Some {choice = Sc_rollup.Tick.initial; step = Dissection []}
  in
  Sc_rollup_refute {rollup; opponent; refutation}

let generate_sc_rollup_timeout =
  let open QCheck2.Gen in
  let* source = random_pkh in
  let* rollup = random_sc_rollup in
  let+ staker = random_pkh in
  let stakers = Sc_rollup.Game.Index.make source staker in
  Sc_rollup_timeout {rollup; stakers}

let generate_sc_rollup_execute_outbox_message =
  let open QCheck2.Gen in
  let+ rollup = random_sc_rollup in
  let cemented_commitment =
    Sc_rollup.Commitment.hash_uncarbonated sc_dummy_commitment
  in
  let output_proof = "" in
  Sc_rollup_execute_outbox_message {rollup; cemented_commitment; output_proof}

let generate_sc_rollup_recover_bond =
  let open QCheck2.Gen in
  let+ sc_rollup = random_sc_rollup in
  Sc_rollup_recover_bond {sc_rollup}

let generate_sc_rollup_dal_slot_subscribe =
  let open QCheck2.Gen in
  let+ rollup = random_sc_rollup in
  let slot_index = Alpha_context.Dal.Slot_index.zero in
  Sc_rollup_dal_slot_subscribe {rollup; slot_index}

(** {By Kind Operation Generator} *)

let generator_of ?source = function
  | `KReveal -> generate_manager_operation ?source generate_reveal
  | `KTransaction -> generate_manager_operation ?source generate_transaction
  | `KOrigination -> generate_manager_operation ?source generate_origination
  | `KSet_deposits_limit ->
      generate_manager_operation ?source generate_set_deposits_limit
  | `KIncrease_paid_storage ->
      generate_manager_operation ?source generate_increase_paid_storage
  | `KDelegation -> generate_manager_operation ?source generate_delegation
  | `KRegister_global_constant ->
      generate_manager_operation ?source generate_register_global_constant
  | `KTx_rollup_origination ->
      generate_manager_operation ?source generate_tx_rollup_origination
  | `KTransfer_ticket ->
      generate_manager_operation ?source generate_transfer_ticket
  | `KDal_publish_slot_header ->
      generate_manager_operation ?source generate_dal_publish_slot_header
  | `KTx_rollup_submit_batch ->
      generate_manager_operation ?source generate_tx_rollup_submit_batch
  | `KTx_rollup_commit ->
      generate_manager_operation ?source generate_tx_rollup_commit
  | `KTx_rollup_return_bond ->
      generate_manager_operation ?source generate_tx_rollup_return_bond
  | `KTx_rollup_finalize_commitment ->
      generate_manager_operation ?source generate_tx_finalize_commitment
  | `KTx_rollup_remove_commitment ->
      generate_manager_operation ?source generate_tx_rollup_remove_commitment
  | `KTx_rollup_rejection ->
      generate_manager_operation ?source generate_tx_rollup_rejection
  | `KTx_rollup_dispatch_tickets ->
      generate_manager_operation ?source generate_tx_dispatch_tickets
  | `KSc_rollup_originate ->
      generate_manager_operation ?source generate_sc_rollup_originate
  | `KSc_rollup_add_messages ->
      generate_manager_operation ?source generate_sc_rollup_add_messages
  | `KSc_rollup_cement ->
      generate_manager_operation ?source generate_sc_rollup_cement
  | `KSc_rollup_publish ->
      generate_manager_operation ?source generate_sc_rollup_publish
  | `KSc_rollup_refute ->
      generate_manager_operation ?source generate_sc_rollup_refute
  | `KSc_rollup_timeout ->
      generate_manager_operation ?source generate_sc_rollup_timeout
  | `KSc_rollup_execute_outbox_message ->
      generate_manager_operation
        ?source
        generate_sc_rollup_execute_outbox_message
  | `KSc_rollup_recover_bond ->
      generate_manager_operation ?source generate_sc_rollup_recover_bond
  | `KSc_rollup_dal_slot_subscribe ->
      generate_manager_operation ?source generate_sc_rollup_dal_slot_subscribe

let generate_manager_operation batch_size =
  let open QCheck2.Gen in
  let* source = random_pkh in
  let source = Some source in
  let* l =
    flatten_l (Stdlib.List.init batch_size (fun _ -> oneofl manager_kinds))
  in
  let* packed_manager_ops = flatten_l (List.map (generator_of ?source) l) in
  let first_op = Stdlib.List.hd packed_manager_ops in
  let unpacked_operations =
    List.map
      (function
        | {Alpha_context.protocol_data = Operation_data {contents; _}; _} -> (
            match Contents_list contents with
            | Contents_list (Single o) -> Contents o
            | Contents_list
                (Cons (Manager_operation {operation = Reveal _; _}, Single o))
              ->
                Contents o
            | _ -> assert false))
      packed_manager_ops
  in
  let contents_list =
    List.fold_left
      (fun acc -> function
        | Contents (Manager_operation m) ->
            Contents (Manager_operation m) :: acc
        | x -> x :: acc)
      []
      unpacked_operations
    |> List.rev
  in
  let (Contents_list contents_list) =
    match Operation.of_list contents_list with Ok v -> v | _ -> assert false
  in
  let signature =
    match first_op.protocol_data with
    | Operation_data {signature; _} -> signature
  in
  let protocol_data = {contents = contents_list; signature} in
  return (Operation.pack {shell = first_op.shell; protocol_data})

let generate_operation =
  let open QCheck2.Gen in
  let* pass = oneofl all_passes in
  let* kind = oneofl (pass_to_operation_kinds pass) in
  let+ packed_operation =
    match kind with
    | `KPreendorsement -> generate_operation generate_preendorsement
    | `KEndorsement -> generate_operation generate_endorsement
    | `KDal_slot -> generate_operation generate_dal_slot
    | `KSeed_nonce_revelation ->
        generate_operation generate_seed_nonce_revelation
    | `KVdf_revelation -> generate_operation generate_vdf_revelation
    | `KDouble_endorsement -> generate_operation generate_double_endorsement
    | `KDouble_preendorsement ->
        generate_operation generate_double_preendorsement
    | `KDouble_baking -> generate_operation generate_double_baking
    | `KActivate_account -> generate_operation generate_activate_account
    | `KProposals -> generate_operation generate_proposals
    | `KBallot -> generate_operation generate_ballot
    | `KManager ->
        let* batch_size = int_range 1 49 in
        generate_manager_operation batch_size
  in
  (kind, (Operation.hash_packed packed_operation, packed_operation))
