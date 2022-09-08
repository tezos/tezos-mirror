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

open Protocol
open Alpha_context
module Random = Random.State

type random_state = {seed : int; rnd_state : Random.t}

let choose_list_element random_state l =
  Stdlib.List.nth l (Random.int random_state.rnd_state (List.length l))

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

let block_hashes =
  List.map
    Block_hash.of_b58check_exn
    [
      "BLbcVY1kYiKQy2MJJfoHJMN2xRk5QPG1PEKWMDSyW2JMxBsMmiL";
      "BLFhLKqQQn32Cc9QXqtEqysYqWNCowNKaypVHP5zEyZcywbXcHo";
      "BLuurCvGmNPTzXSnGCpcFPy5h8A49PwH2LnfAWBnp5R1qv5czwe";
    ]

let random_shell random_state : Tezos_base.Operation.shell_header =
  {branch = choose_list_element random_state block_hashes}

let random_slot random_state =
  choose_list_element random_state [100; 200; 300]
  |> Slot.of_int_do_not_use_except_for_parameters

let random_level random_state =
  choose_list_element random_state [10l; 20l; 30l] |> Raw_level.of_int32
  |> function
  | Ok v -> v
  | Error _ -> assert false

let random_round random_state =
  choose_list_element random_state [0l; 1l; 2l] |> Round.of_int32 |> function
  | Ok v -> v
  | Error _ -> assert false

let payload_hashes =
  List.map
    Block_payload_hash.of_b58check_exn
    [
      "vh2gWcSUUhJBwvjx4vS7JN5ioMVWpHCSK6W2MKNPr5dn6NUdfFDQ";
      "vh1p1VzeYjZLEW6WDqdTwVy354KEmGCDgPmagEKcLN4NT4X58mNk";
      "vh2TyrWeZ2dydEy9ZjmvrjQvyCs5sdHZPypcZrXDUSM1tNuPermf";
    ]

let random_payload_hash random_state =
  choose_list_element random_state payload_hashes

let generate_consensus_content random_state : consensus_content =
  let slot = random_slot random_state in
  let level = random_level random_state in
  let round = random_round random_state in
  let block_payload_hash = random_payload_hash random_state in
  {slot; level; round; block_payload_hash}

let signatures =
  List.map
    Signature.of_b58check_exn
    [
      "sigaNsiye7D8dJHKSQZBwDbS2aQNXipDP7bw8uQnMgnaXi5pcnoPZRKXrDeFRx4FjWJD2xfyUA9CuBXhwPHhVs7LxkL4vT32";
      "sigvtPBMQvk2DgNtu3AKFU1ZRsagGxsoiZVQyQhJNEojReBY2vE5sDwt3H7Mh8RMe27QHBjemxqhMVVszZqpNsdDux6KAELX";
      "sighje7pEbUUwGtJ4GTP7uzMZe5SFz6dRRC3BvZBHnrRHnc47WHGnVdfiscHPMek7esmj7saTuj54QBWy3SezyA2EGbHkmW5";
    ]

let random_signature random_state =
  Some (choose_list_element random_state signatures)

let pkhs =
  List.map
    Signature.Public_key_hash.of_b58check_exn
    [
      "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
      "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv";
      "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU";
    ]

let random_pkh random_state = choose_list_element random_state pkhs

let pks =
  List.map
    Signature.Public_key.of_b58check_exn
    [
      "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2";
      "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n";
      "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU";
    ]

let random_pk random_state = choose_list_element random_state pks

let random_fee random_state =
  choose_list_element random_state [Tez.zero; Tez.one_cent; Tez.one]

let random_amount random_state =
  choose_list_element random_state [Tez.zero; Tez.one_cent; Tez.one]

let random_amount_in_bytes random_state =
  choose_list_element random_state [Z.zero; Z.one; Z.of_int 100]

let contract_hashes =
  List.map
    Contract_hash.of_b58check_exn
    [
      "KT1WvzYHCNBvDSdwafTHv7nJ1dWmZ8GCYuuC";
      "KT1NkWx47WzJeHCSyB62WjLtFn4tRf3uXBur";
      "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
    ]

let random_contract_hash random_state =
  choose_list_element random_state contract_hashes

let random_contract random_state =
  if Random.bool random_state.rnd_state then
    Contract.Implicit (random_pkh random_state)
  else
    let contract_hash = random_contract_hash random_state in
    Contract.Originated contract_hash

let random_contract_hash random_state =
  choose_list_element random_state contract_hashes

let counters = List.map Z.of_int [123; 456; 789]

let random_counter random_state = choose_list_element random_state counters

let random_gas_limit random_state =
  choose_list_element
    random_state
    Gas.Arith.[zero; integral_of_int_exn 1_000; integral_of_int_exn 10_000]

let random_storage_limit random_state =
  choose_list_element random_state Z.[zero; of_int 1_000; of_int 10_000]

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

let random_block_header random_state =
  choose_list_element random_state block_headers

let nonces =
  List.map
    (fun i ->
      let b = Bytes.create 32 in
      Bytes.set_int8 b 0 i ;
      Alpha_context.Nonce.of_bytes b |> function
      | Ok v -> v
      | Error _ -> assert false)
    [1; 2; 3]

let random_nonce random_state = choose_list_element random_state nonces

let random_option f random_state =
  if Random.bool random_state.rnd_state then Some (f random_state) else None

let tx_rollups =
  List.filter_map
    Tx_rollup.of_b58check_opt
    [
      "txr1hFmPcr5y1P2xTm7W2y1sfjLLCUdzCZGvg";
      "txr1jux4nZWf8ToGZc4ojLBbT538BBTTSiJUD";
      "txr1TAFTENC2YACvoMDrpJHCbdvdfSSjcjEjc";
    ]

let random_tx_rollup random_state = choose_list_element random_state tx_rollups

let sc_rollups =
  List.map
    Sc_rollup.Address.of_b58check_exn
    [
      "scr1FPSu51gGtyv9S5HqDqXeH16DJviJ9qpr6";
      "scr1U39BVdpVQun1QjjiXfd3XgoBKcenWt5sb";
      "scr1Kqqbvust2adJMtSu2V4fcd49oQHug4BLb";
    ]

let random_sc_rollup random_state = choose_list_element random_state sc_rollups

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

let random_proto random_state = choose_list_element random_state protos

let generate_op random_state (gen_op : random_state -> 'kind contents_list) :
    'kind operation =
  let shell = random_shell random_state in
  let signature = random_signature random_state in
  let contents = gen_op random_state in
  let protocol_data = {contents; signature} in
  {shell; protocol_data}

let generate_preendorsement random_state =
  let gen random_state =
    Single (Preendorsement (generate_consensus_content random_state))
  in
  generate_op random_state gen

let generate_endorsement random_state : Kind.endorsement Operation.t =
  let gen random_state =
    Single (Endorsement (generate_consensus_content random_state))
  in
  generate_op random_state gen

let generate_dal_slot random_state : Kind.dal_slot_availability Operation.t =
  let gen random_state =
    let pkh = random_pkh random_state in
    let dal_endorsement = Dal.Endorsement.empty in
    Single (Dal_slot_availability (pkh, dal_endorsement))
  in
  generate_op random_state gen

let generate_seed_nonce_revelation random_state :
    Kind.seed_nonce_revelation Operation.t =
  let gen random_state =
    let level = random_level random_state in
    let nonce = random_nonce random_state in
    Single (Seed_nonce_revelation {level; nonce})
  in
  generate_op random_state gen

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

let random_vdf_solution random_state =
  choose_list_element random_state vdf_solutions

let generate_vdf_revelation random_state : Kind.vdf_revelation Operation.t =
  let gen random_state =
    let solution = random_vdf_solution random_state in
    Single (Vdf_revelation {solution})
  in
  generate_op random_state gen

let generate_double_preendorsement random_state :
    Kind.double_preendorsement_evidence Operation.t =
  let gen random_state =
    let op1 = generate_preendorsement random_state in
    let op2 = generate_preendorsement random_state in
    Single (Double_preendorsement_evidence {op1; op2})
  in
  generate_op random_state gen

let generate_double_endorsement random_state :
    Kind.double_endorsement_evidence Operation.t =
  let gen random_state =
    let op1 = generate_endorsement random_state in
    let op2 = generate_endorsement random_state in
    Single (Double_endorsement_evidence {op1; op2})
  in
  generate_op random_state gen

let generate_double_baking random_state :
    Kind.double_baking_evidence Operation.t =
  let gen random_state =
    let bh1 = random_block_header random_state in
    let bh2 = random_block_header random_state in
    Single (Double_baking_evidence {bh1; bh2})
  in
  generate_op random_state gen

let generate_manager_aux :
    type kind.
    public_key_hash option ->
    random_state ->
    (random_state -> kind manager_operation) ->
    kind Kind.manager contents =
 fun opt_source random_state gen_op ->
  let source =
    match opt_source with
    | None -> random_pkh random_state
    | Some source -> source
  in
  let fee = random_fee random_state in
  let counter = random_counter random_state in
  let operation = gen_op random_state in
  let gas_limit = random_gas_limit random_state in
  let storage_limit = random_storage_limit random_state in
  Manager_operation {source; fee; counter; operation; gas_limit; storage_limit}

let generate_manager random_state
    (gen_op : random_state -> 'kind manager_operation) :
    'kind Kind.manager Operation.t =
  let source = Some (random_pkh random_state) in
  let shell = random_shell random_state in
  let signature = random_signature random_state in
  let contents = Single (generate_manager_aux source random_state gen_op) in
  let protocol_data = {contents; signature} in
  {shell; protocol_data}

let generate_managers random_state gen_ops =
  let source = Some (random_pkh random_state) in
  let ops_as_single =
    List.map
      (fun gen_op -> Contents (generate_manager_aux source random_state gen_op))
      gen_ops
  in
  Operation.of_list ops_as_single

let generate_reveal random_state : Kind.reveal Kind.manager Operation.t =
  let gen random_state = Reveal (random_pk random_state) in
  generate_manager random_state gen

let generate_transaction random_state =
  let gen_trans random_state =
    let amount = random_amount random_state in
    let parameters = Script.unit_parameter in
    let entrypoint = Entrypoint.default in
    let destination = random_contract random_state in
    Transaction {amount; parameters; entrypoint; destination}
  in
  generate_manager random_state gen_trans

let generate_origination random_state :
    Kind.origination Kind.manager Operation.t =
  let gen_origination random_state =
    let delegate = None in
    let script = Script.{code = unit_parameter; storage = unit_parameter} in
    let credit = random_amount random_state in
    Origination {delegate; script; credit}
  in
  generate_manager random_state gen_origination

let generate_delegation random_state : Kind.delegation Kind.manager Operation.t
    =
  let gen_delegation random_state =
    let delegate = random_option random_pkh random_state in
    Delegation delegate
  in
  generate_manager random_state gen_delegation

let generate_set_deposits_limit random_state :
    Kind.set_deposits_limit Kind.manager Operation.t =
  let gen_set_deposits_limit random_state =
    let amount_opt = random_option random_amount random_state in
    Set_deposits_limit amount_opt
  in
  generate_manager random_state gen_set_deposits_limit

let generate_increase_paid_storage random_state :
    Kind.increase_paid_storage Kind.manager Operation.t =
  let gen_increase_paid_storage random_state =
    let amount_in_bytes = random_amount_in_bytes random_state in
    let destination = random_contract_hash random_state in
    Increase_paid_storage {amount_in_bytes; destination}
  in
  generate_manager random_state gen_increase_paid_storage

let generate_register_global_constant random_state :
    Kind.register_global_constant Kind.manager Operation.t =
  let gen_register_global_constant _ =
    let value = Script_repr.lazy_expr (Expr.from_string "Pair 1 2") in
    Register_global_constant {value}
  in
  generate_manager random_state gen_register_global_constant

let generate_tx_rollup_origination random_state :
    Kind.tx_rollup_origination Kind.manager Operation.t =
  let gen_tx_orig _ = Tx_rollup_origination in
  generate_manager random_state gen_tx_orig

let generate_tx_rollup_submit_batch random_state :
    Kind.tx_rollup_submit_batch Kind.manager Operation.t =
  let gen_tx_submit random_state =
    let tx_rollup = random_tx_rollup random_state in
    let content = "batch" in
    let burn_limit = None in
    Tx_rollup_submit_batch {tx_rollup; content; burn_limit}
  in
  generate_manager random_state gen_tx_submit

let generate_tx_rollup_commit random_state :
    Kind.tx_rollup_commit Kind.manager Operation.t =
  let gen_tx_commit random_state =
    let tx_rollup = random_tx_rollup random_state in
    let commitment : Tx_rollup_commitment.Full.t =
      {
        level = Tx_rollup_level.root;
        messages = [];
        predecessor = None;
        inbox_merkle_root = Tx_rollup_inbox.Merkle.merklize_list [];
      }
    in
    Tx_rollup_commit {tx_rollup; commitment}
  in
  generate_manager random_state gen_tx_commit

let generate_tx_rollup_return_bond random_state :
    Kind.tx_rollup_return_bond Kind.manager Operation.t =
  let gen_tx_return_bd random_state =
    let tx_rollup = random_tx_rollup random_state in
    Tx_rollup_return_bond {tx_rollup}
  in
  generate_manager random_state gen_tx_return_bd

let generate_tx_finalize_commitment random_state :
    Kind.tx_rollup_finalize_commitment Kind.manager Operation.t =
  let gen_tx_finalize random_state =
    let tx_rollup = random_tx_rollup random_state in
    Tx_rollup_finalize_commitment {tx_rollup}
  in
  generate_manager random_state gen_tx_finalize

let generate_tx_rollup_remove_commitment random_state :
    Kind.tx_rollup_remove_commitment Kind.manager Operation.t =
  let gen_tx_remove random_state =
    let tx_rollup = random_tx_rollup random_state in
    Tx_rollup_remove_commitment {tx_rollup}
  in
  generate_manager random_state gen_tx_remove

let generate_tx_rollup_rejection random_state :
    Kind.tx_rollup_rejection Kind.manager Operation.t =
  let gen_tx_rejection random_state =
    let tx_rollup = random_tx_rollup random_state in
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
  in
  generate_manager random_state gen_tx_rejection

let generate_tx_dispatch_tickets random_state :
    Kind.tx_rollup_dispatch_tickets Kind.manager Operation.t =
  let gen_tx_dispatch random_state =
    let tx_rollup = random_tx_rollup random_state in
    let source = random_pkh random_state in
    let contract = random_contract random_state in
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
  in
  generate_manager random_state gen_tx_dispatch

let generate_transfer_ticket random_state :
    Kind.transfer_ticket Kind.manager Operation.t =
  let gen_transfer_ticket random_state =
    let contents = Script.lazy_expr (Expr.from_string "1") in
    let ty = Script.lazy_expr (Expr.from_string "nat") in
    let ticketer = random_contract random_state in
    let destination = random_contract random_state in
    let amount = random_counter random_state in
    let entrypoint = Entrypoint.default in
    Transfer_ticket {contents; ty; ticketer; amount; destination; entrypoint}
  in
  generate_manager random_state gen_transfer_ticket

let generate_dal_publish_slot_header random_state :
    Kind.dal_publish_slot_header Kind.manager Operation.t =
  let gen_dal_publish _ =
    let published_level = Alpha_context.Raw_level.of_int32_exn Int32.zero in
    let index = Alpha_context.Dal.Slot_index.zero in
    let header = Alpha_context.Dal.Slot.zero in
    let slot = Alpha_context.Dal.Slot.{published_level; index; header} in
    Dal_publish_slot_header {slot}
  in
  generate_manager random_state gen_dal_publish

let generate_sc_rollup_originate random_state :
    Kind.sc_rollup_originate Kind.manager Operation.t =
  let gen_sc_originate _ =
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
    Sc_rollup_originate {kind; boot_sector; origination_proof; parameters_ty}
  in
  generate_manager random_state gen_sc_originate

let generate_sc_rollup_add_messages random_state :
    Kind.sc_rollup_add_messages Kind.manager Operation.t =
  let gen_sc_add_messages random_state =
    let rollup = random_sc_rollup random_state in
    let messages = [] in
    Sc_rollup_add_messages {rollup; messages}
  in
  generate_manager random_state gen_sc_add_messages

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

let generate_sc_rollup_cement random_state :
    Kind.sc_rollup_cement Kind.manager Operation.t =
  let gen_sc_cement random_state =
    let rollup = random_sc_rollup random_state in
    let commitment =
      Sc_rollup.Commitment.hash_uncarbonated sc_dummy_commitment
    in
    Sc_rollup_cement {rollup; commitment}
  in
  generate_manager random_state gen_sc_cement

let generate_sc_rollup_publish random_state :
    Kind.sc_rollup_publish Kind.manager Operation.t =
  let gen_sc_publish random_state =
    let rollup = random_sc_rollup random_state in
    let commitment = sc_dummy_commitment in
    Sc_rollup_publish {rollup; commitment}
  in
  generate_manager random_state gen_sc_publish

let generate_sc_rollup_refute random_state :
    Kind.sc_rollup_refute Kind.manager Operation.t =
  let gen random_state =
    let opponent = random_pkh random_state in
    let rollup = random_sc_rollup random_state in
    let refutation : Sc_rollup.Game.refutation option =
      Some {choice = Sc_rollup.Tick.initial; step = Dissection []}
    in
    Sc_rollup_refute {rollup; opponent; refutation}
  in
  generate_manager random_state gen

let generate_sc_rollup_timeout random_state :
    Kind.sc_rollup_timeout Kind.manager Operation.t =
  let gen random_state =
    let source = random_pkh random_state in
    let rollup = random_sc_rollup random_state in
    let staker = random_pkh random_state in
    let stakers = Sc_rollup.Game.Index.make source staker in
    Sc_rollup_timeout {rollup; stakers}
  in
  generate_manager random_state gen

let generate_sc_rollup_execute_outbox_message random_state :
    Kind.sc_rollup_execute_outbox_message Kind.manager Operation.t =
  let gen random_state =
    let rollup = random_sc_rollup random_state in
    let cemented_commitment =
      Sc_rollup.Commitment.hash_uncarbonated sc_dummy_commitment
    in
    let output_proof = "" in
    Sc_rollup_execute_outbox_message {rollup; cemented_commitment; output_proof}
  in
  generate_manager random_state gen

let generate_sc_rollup_recover_bond random_state :
    Kind.sc_rollup_recover_bond Kind.manager Operation.t =
  let gen random_state =
    let sc_rollup = random_sc_rollup random_state in
    Sc_rollup_recover_bond {sc_rollup}
  in
  generate_manager random_state gen

let generate_sc_rollup_dal_slot_subscribe random_state :
    Kind.sc_rollup_dal_slot_subscribe Kind.manager Operation.t =
  let gen random_state =
    let rollup = random_sc_rollup random_state in
    let slot_index = Alpha_context.Dal.Slot_index.zero in
    Sc_rollup_dal_slot_subscribe {rollup; slot_index}
  in
  generate_manager random_state gen

let codes =
  List.filter_map
    Blinded_public_key_hash.activation_code_of_hex
    [
      "41f98b15efc63fa893d61d7d6eee4a2ce9427ac4";
      "411dfef031eeecc506de71c9df9f8e44297cf5ba";
      "08d7d355bc3391d12d140780b39717d9f46fcf87";
    ]

let random_code random_state = choose_list_element random_state codes

let generate_activate_account random_state : Kind.activate_account Operation.t =
  let gen random_state =
    let id =
      random_pkh random_state |> function
      | Ed25519 pkh -> pkh
      | _ -> assert false
    in
    let activation_code = random_code random_state in
    Single (Activate_account {id; activation_code})
  in
  generate_op random_state gen

let random_period random_state = choose_list_element random_state [0l; 1l; 2l]

let generate_proposals random_state : Kind.proposals Operation.t =
  let gen random_state =
    let source = random_pkh random_state in
    let period = random_period random_state in
    let proposals = [] in
    Single (Proposals {source; period; proposals})
  in
  generate_op random_state gen

let generate_ballot random_state : Kind.ballot Operation.t =
  let gen random_state =
    let source = random_pkh random_state in
    let period = random_period random_state in
    let proposal = random_proto random_state in
    let ballot = Vote.Pass in
    Single (Ballot {source; period; proposal; ballot})
  in
  generate_op random_state gen

let generate_manager_operation batch_size random_state =
  let l =
    Stdlib.List.init batch_size (fun _ ->
        choose_list_element random_state manager_kinds)
  in
  let packed_manager_ops =
    List.map
      (function
        | `KReveal -> generate_reveal random_state |> Operation.pack
        | `KTransaction -> generate_transaction random_state |> Operation.pack
        | `KOrigination -> generate_origination random_state |> Operation.pack
        | `KSet_deposits_limit ->
            generate_set_deposits_limit random_state |> Operation.pack
        | `KIncrease_paid_storage ->
            generate_increase_paid_storage random_state |> Operation.pack
        | `KDelegation -> generate_delegation random_state |> Operation.pack
        | `KRegister_global_constant ->
            generate_register_global_constant random_state |> Operation.pack
        | `KTx_rollup_origination ->
            generate_tx_rollup_origination random_state |> Operation.pack
        | `KTransfer_ticket ->
            generate_transfer_ticket random_state |> Operation.pack
        | `KDal_publish_slot_header ->
            generate_dal_publish_slot_header random_state |> Operation.pack
        | `KTx_rollup_submit_batch ->
            generate_tx_rollup_submit_batch random_state |> Operation.pack
        | `KTx_rollup_commit ->
            generate_tx_rollup_commit random_state |> Operation.pack
        | `KTx_rollup_return_bond ->
            generate_tx_rollup_return_bond random_state |> Operation.pack
        | `KTx_rollup_finalize_commitment ->
            generate_tx_finalize_commitment random_state |> Operation.pack
        | `KTx_rollup_remove_commitment ->
            generate_tx_rollup_remove_commitment random_state |> Operation.pack
        | `KTx_rollup_rejection ->
            generate_tx_rollup_rejection random_state |> Operation.pack
        | `KTx_rollup_dispatch_tickets ->
            generate_tx_dispatch_tickets random_state |> Operation.pack
        | `KSc_rollup_originate ->
            generate_sc_rollup_originate random_state |> Operation.pack
        | `KSc_rollup_add_messages ->
            generate_sc_rollup_add_messages random_state |> Operation.pack
        | `KSc_rollup_cement ->
            generate_sc_rollup_cement random_state |> Operation.pack
        | `KSc_rollup_publish ->
            generate_sc_rollup_publish random_state |> Operation.pack
        | `KSc_rollup_refute ->
            generate_sc_rollup_refute random_state |> Operation.pack
        | `KSc_rollup_timeout ->
            generate_sc_rollup_timeout random_state |> Operation.pack
        | `KSc_rollup_execute_outbox_message ->
            generate_sc_rollup_execute_outbox_message random_state
            |> Operation.pack
        | `KSc_rollup_recover_bond ->
            generate_sc_rollup_recover_bond random_state |> Operation.pack
        | `KSc_rollup_dal_slot_subscribe ->
            generate_sc_rollup_dal_slot_subscribe random_state |> Operation.pack)
      l
  in
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
  Operation.pack {shell = first_op.shell; protocol_data}

let generate_operation random_state =
  let pass = choose_list_element random_state all_passes in
  let kind = choose_list_element random_state (pass_to_operation_kinds pass) in
  let packed_operation =
    match kind with
    | `KPreendorsement -> generate_preendorsement random_state |> Operation.pack
    | `KEndorsement -> generate_endorsement random_state |> Operation.pack
    | `KDal_slot -> generate_dal_slot random_state |> Operation.pack
    | `KSeed_nonce_revelation ->
        generate_seed_nonce_revelation random_state |> Operation.pack
    | `KVdf_revelation -> generate_vdf_revelation random_state |> Operation.pack
    | `KDouble_endorsement ->
        generate_double_endorsement random_state |> Operation.pack
    | `KDouble_preendorsement ->
        generate_double_preendorsement random_state |> Operation.pack
    | `KDouble_baking -> generate_double_baking random_state |> Operation.pack
    | `KActivate_account ->
        generate_activate_account random_state |> Operation.pack
    | `KProposals -> generate_proposals random_state |> Operation.pack
    | `KBallot -> generate_ballot random_state |> Operation.pack
    | `KManager ->
        let batch_size = 1 + Random.int random_state.rnd_state 3 in
        generate_manager_operation batch_size random_state
  in
  (kind, (Operation.hash_packed packed_operation, packed_operation))
