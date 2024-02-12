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

let all_non_manager_passes = [`PConsensus; `PAnonymous; `PVote]

let consensus_kinds = [`KPreattestation; `KAttestation]

let anonymous_kinds =
  [
    `KSeed_nonce_revelation;
    `KVdf_revelation;
    `KDouble_attestation;
    `KDouble_preattestation;
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
    | `KPreattestation -> "KPreattestation"
    | `KAttestation -> "KAttestation"
    | `KDal_attestation -> "KDal_attestation"
    | `KSeed_nonce_revelation -> "KSeed_nonce_revelation"
    | `KVdf_revelation -> "KVdf_revelation"
    | `KDouble_attestation -> "KDouble_attestation"
    | `KDouble_preattestation -> "KDouble_preattestation"
    | `KDouble_baking -> "KDouble_baking"
    | `KActivate_account -> "KActivate_account"
    | `KProposals -> "KProposals"
    | `KBallot -> "KBallot"
    | `KManager -> "KManager")

(** {2 Generators} *)

module Gen_hash (H : sig
  type t

  val size : int

  val of_bytes_exn : bytes -> t
end) =
struct
  let gen =
    let open QCheck2.Gen in
    let+ str = string_size (pure H.size) in
    H.of_bytes_exn (Bytes.unsafe_of_string str)
end

(** {3 Selection in hashes list} *)

let gen_block_hash =
  let module G = Gen_hash (Block_hash) in
  G.gen

let random_payload_hash =
  let module G = Gen_hash (Block_payload_hash) in
  G.gen

let gen_algo = QCheck2.Gen.oneofl Signature.algos

let random_seed =
  let open QCheck2.Gen in
  let+ str = string_size (pure Tezos_crypto.Hacl.Ed25519.sk_size) in
  Bytes.unsafe_of_string str

let random_keys =
  let open QCheck2.Gen in
  let* algo = gen_algo in
  let+ seed = random_seed in
  Signature.generate_key ~algo ~seed ()

let random_tz1 =
  let open QCheck2.Gen in
  let+ str = string_size (pure Signature.Ed25519.Public_key_hash.size) in
  (Ed25519 (Signature.Ed25519.Public_key_hash.of_string_exn str)
    : public_key_hash)

let random_tz2 =
  let open QCheck2.Gen in
  let+ str = string_size (pure Signature.Secp256k1.Public_key_hash.size) in
  (Secp256k1 (Signature.Secp256k1.Public_key_hash.of_string_exn str)
    : public_key_hash)

let random_tz3 =
  let open QCheck2.Gen in
  let+ str = string_size (pure Signature.P256.Public_key_hash.size) in
  (P256 (Signature.P256.Public_key_hash.of_string_exn str) : public_key_hash)

let random_tz4 =
  let open QCheck2.Gen in
  let+ str = string_size (pure Signature.Bls.Public_key_hash.size) in
  (Bls (Signature.Bls.Public_key_hash.of_string_exn str) : public_key_hash)

let random_pkh =
  let open QCheck2.Gen in
  let* algo = gen_algo in
  match algo with
  | Ed25519 -> random_tz1
  | Secp256k1 -> random_tz2
  | P256 -> random_tz3
  | Bls -> random_tz4

let random_pk =
  let open QCheck2.Gen in
  let+ _, pk, _ = random_keys in
  pk

let random_signature =
  let open QCheck2.Gen in
  let* algo = option ~ratio:0.8 gen_algo in
  match algo with
  | None ->
      let+ str = string_size (pure Signature.Ed25519.size) in
      (Unknown (Bytes.unsafe_of_string str) : Signature.t)
  | Some Ed25519 ->
      let+ str = string_size (pure Signature.Ed25519.size) in
      (Ed25519 (Signature.Ed25519.of_string_exn str) : Signature.t)
  | Some Secp256k1 ->
      let+ str = string_size (pure Signature.Secp256k1.size) in
      (Secp256k1 (Signature.Secp256k1.of_string_exn str) : Signature.t)
  | Some P256 ->
      let+ str = string_size (pure Signature.P256.size) in
      (P256 (Signature.P256.of_string_exn str) : Signature.t)
  | Some Bls ->
      let+ seed = random_seed in
      let _, _, sk = Signature.generate_key ~algo:Bls ~seed () in
      Signature.sign sk Bytes.empty

let random_signature =
  let open QCheck2.Gen in
  graft_corners
    random_signature
    Signature.
      [
        of_ed25519 Signature.Ed25519.zero;
        of_secp256k1 Signature.Secp256k1.zero;
        of_p256 Signature.P256.zero;
        of_bls Signature.Bls.zero;
        Unknown (Bytes.make 64 '\000');
      ]
    ()

let random_contract_hash =
  let module G = Gen_hash (Contract_hash) in
  G.gen

let block_headers =
  let bh1 =
    {json|{ "level": 2, "proto": 1, "predecessor": "BLbcVY1kYiKQy2MJJfoHJMN2xRk5QPG1PEKWMDSyW2JMxBsMmiL", "timestamp": "2022-08-08T11:16:30Z", "validation_pass": 4, "operations_hash": "LLoa7bxRTKaQN2bLYoitYB6bU2DvLnBAqrVjZcvJ364cTcX2PZYKU", "fitness": [ "02", "00000002", "", "ffffffff", "00000001" ], "context": "CoUvpF8XBUfz3w9CJumt4ZKGZkrcdcfs1Qdrrd1ZeFij64E1QCud", "payload_hash": "vh2TyrWeZ2dydEy9ZjmvrjQvyCs5sdHZPypcZrXDUSM1tNuPermf", "payload_round": 1, "proof_of_work_nonce": "62de1e0d00000000", "liquidity_baking_toggle_vote": "pass", "adaptive_issuance_vote": "pass", "signature": "sigaXGo4DWsZwo1SvbKCp2hLgE5jcwd61Ufkc3iMt3sXy3NBj9jticuJKJnRhyH2ZPJQMwEuDqQTgZgoK5xRH6HeF7YxLb4u" }|json}
  in
  let bh2 =
    {json|{ "level": 3, "proto": 1, "predecessor": "BLAUNUbzKHgA4DYQEXCbxY73wdE2roGAzvJJbFp8dQe62Ekpada", "timestamp": "2022-08-08T11:16:32Z", "validation_pass": 4, "operations_hash": "LLoaWjBX8Cm8DVpoLNtm7FPNnxUdL6Dakq122pVfNHYaf2rE9GQXi", "fitness": [ "02", "00000003", "", "fffffffe", "00000000" ], "context": "CoUtWowJUqXwMm4pbR1jjyFfVRHqRHGs6bYVDaaByvbmULoAND2x", "payload_hash": "vh1p1VzeYjZLEW6WDqdTwVy354KEmGCDgPmagEKcLN4NT4X58mNk", "payload_round": 0, "proof_of_work_nonce": "62de1e0d00000000", "liquidity_baking_toggle_vote": "pass", "adaptive_issuance_vote": "pass", "signature": "sigVqWWE7BPuxHqPWiVRmzQ1eMZZAPAxGJ94ytY2sjV8Y1Z4QH1F2bPGZS1ZeWDbqmcppPPFobRpi7wNasQ17Mm9CFGKag2t" }|json}
  in
  let bh3 =
    {json|{ "level": 4, "proto": 1, "predecessor": "BLuurCvGmNPTzXSnGCpcFPy5h8A49PwH2LnfAWBnp5R1qv5czwe", "timestamp": "2022-08-08T11:16:33Z", "validation_pass": 4, "operations_hash": "LLoaf8AANzyNxhk715zykDrwG5Bpqw6FsZLWWNp2Dcm3ewFrcc3Wc", "fitness": [ "02", "00000004", "", "ffffffff", "00000000" ], "context": "CoVzxEBMDhxpGVxrguik6r5qVogJBFyhuvwm2KZBcsmvqhekPiwL", "payload_hash": "vh2gWcSUUhJBwvjx4vS7JN5ioMVWpHCSK6W2MKNPr5dn6NUdfFDQ", "payload_round": 0, "proof_of_work_nonce": "62de1e0d00000000", "seed_nonce_hash": "nceV3VjdHp1yk6uqcQicQBxLJY1AfWvLSabQpqnpiqkC1q2tS35EN", "liquidity_baking_toggle_vote": "pass", "adaptive_issuance_vote": "pass", "signature": "sigijumaDLSQwjh2AKK7af1VcEDsZsRwbweL8hF176puhHy3ySVocNCbrwPqJLiQP8EbqY5YL6z6b1vDaw12h8MQU2Rh4SW1" }|json}
  in
  List.map
    (fun s ->
      let open Data_encoding.Json in
      from_string s |> function
      | Ok json -> destruct Alpha_context.Block_header.encoding json
      | Error _ -> assert false)
    [bh1; bh2; bh3]

let random_block_header = QCheck2.Gen.oneofl block_headers

let random_sc_rollup =
  let module G = Gen_hash (Sc_rollup.Address) in
  G.gen

let random_proto =
  let module G = Gen_hash (Protocol_hash) in
  G.gen

let random_code =
  let open QCheck2.Gen in
  let+ str = string_size (pure Signature.Ed25519.Public_key_hash.size) in
  let (`Hex hex) = Hex.of_string str in
  Blinded_public_key_hash.activation_code_of_hex hex
  |> WithExceptions.Option.get ~loc:__LOC__

(** {2 Operations parameters generators} *)

let random_shell : Tezos_base.Operation.shell_header QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ branch = gen_block_hash in
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

let gen_counters =
  let open QCheck2.Gen in
  let+ i = nat in
  Manager_counter.Internal_for_tests.of_int i

let gen_ticket_amounts =
  let open QCheck2.Gen in
  let+ i = nat in
  Option.value (Ticket_amount.of_zint (Z.of_int i)) ~default:Ticket_amount.one

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
  let* signature = option ~ratio:0.9 random_signature in
  let+ shell = random_shell in
  let contents = Single op in
  let protocol_data = {contents; signature} in
  wrap_operation shell protocol_data

let generate_operation gen_op =
  let open QCheck2.Gen in
  let+ op = generate_op gen_op in
  Operation.pack op

let generate_preattestation =
  let open QCheck2.Gen in
  let+ cc = generate_consensus_content in
  Preattestation cc

let generate_attestation =
  let open QCheck2.Gen in
  let+ cc = generate_consensus_content in
  Attestation {consensus_content = cc; dal_content = None}

let generate_vdf_revelation =
  let open QCheck2.Gen in
  let+ solution = oneofl vdf_solutions in
  Vdf_revelation {solution}

let generate_seed_nonce_revelation =
  let open QCheck2.Gen in
  let* level = gen_level in
  let+ nonce = random_nonce in
  Seed_nonce_revelation {level; nonce}

let generate_double_preattestation =
  let open QCheck2.Gen in
  let* op1 = generate_op generate_preattestation in
  let+ op2 = generate_op generate_preattestation in
  Double_preattestation_evidence {op1; op2}

let generate_double_attestation =
  let open QCheck2.Gen in
  let* op1 = generate_op generate_attestation in
  let+ op2 = generate_op generate_attestation in
  Double_attestation_evidence {op1; op2}

let generate_double_baking =
  let open QCheck2.Gen in
  let* bh1 = random_block_header in
  let+ bh2 = random_block_header in
  Double_baking_evidence {bh1; bh2}

let generate_activate_account =
  let open QCheck2.Gen in
  let* activation_code = random_code in
  let+ id = random_tz1 in
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

let generate_transfer_ticket =
  let open QCheck2.Gen in
  let* ticketer = random_contract in
  let* destination = random_contract in
  let+ amount = gen_ticket_amounts in
  let contents = Script.lazy_expr (Expr.from_string "1") in
  let ty = Script.lazy_expr (Expr.from_string "nat") in
  let entrypoint = Entrypoint.default in
  Transfer_ticket {contents; ty; ticketer; amount; destination; entrypoint}

let generate_dal_publish_slot_header =
  let slot_index = Alpha_context.Dal.Slot_index.zero in
  let commitment = Alpha_context.Dal.Slot.Commitment.zero in
  let commitment_proof = Alpha_context.Dal.Slot.Commitment_proof.zero in
  let slot_header =
    Alpha_context.Dal.Operations.Publish_slot_header.
      {slot_index; commitment; commitment_proof}
  in
  QCheck2.Gen.pure (Dal_publish_slot_header slot_header)

let generate_sc_rollup_originate =
  let kind = Sc_rollup.Kind.Example_arith in
  let boot_sector = "" in
  let parameters_ty = Script.lazy_expr (Expr.from_string "1") in
  let whitelist = None in
  QCheck2.Gen.pure
    (Sc_rollup_originate {kind; boot_sector; parameters_ty; whitelist})

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
  Sc_rollup_cement {rollup}

let generate_sc_rollup_publish =
  let open QCheck2.Gen in
  let+ rollup = random_sc_rollup in
  let commitment = sc_dummy_commitment in
  Sc_rollup_publish {rollup; commitment}

let generate_sc_rollup_refute =
  let open QCheck2.Gen in
  let* opponent = random_pkh in
  let+ rollup = random_sc_rollup in
  let refutation : Sc_rollup.Game.refutation =
    Sc_rollup.Game.Move {choice = Sc_rollup.Tick.initial; step = Dissection []}
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
  let* staker = random_pkh in
  let+ sc_rollup = random_sc_rollup in
  Sc_rollup_recover_bond {sc_rollup; staker}

(** {2 By Kind Operation Generator} *)

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
  | `KTransfer_ticket ->
      generate_manager_operation ?source generate_transfer_ticket
  | `KDal_publish_slot_header ->
      generate_manager_operation ?source generate_dal_publish_slot_header
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

let generate_non_manager_operation =
  let open QCheck2.Gen in
  let* pass = oneofl all_non_manager_passes in
  let* kind = oneofl (pass_to_operation_kinds pass) in
  match kind with
  | `KPreattestation -> generate_operation generate_preattestation
  | `KAttestation -> generate_operation generate_attestation
  | `KSeed_nonce_revelation -> generate_operation generate_seed_nonce_revelation
  | `KVdf_revelation -> generate_operation generate_vdf_revelation
  | `KDouble_attestation -> generate_operation generate_double_attestation
  | `KDouble_preattestation -> generate_operation generate_double_preattestation
  | `KDouble_baking -> generate_operation generate_double_baking
  | `KActivate_account -> generate_operation generate_activate_account
  | `KProposals -> generate_operation generate_proposals
  | `KBallot -> generate_operation generate_ballot
  | `KManager -> assert false

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

(** The default upper bound on the number of manager operations in a batch.

    As of December 2022, there is no batch maximal size enforced
    anywhere in the protocol. However, the Octez Shell only accepts
    batches of at most [operations_batch_size] operations, which has a
    default value of [50] in [src/lib_shell_services/shell_limits.ml].
    The protocol tests do not necessarily have to align with this
    value, but there is no reason either to choose a different
    one. Therefore, they use the same bound, but decremented once to
    account for some tests adding a reveal at the front of the batch as
    needed. *)
let max_batch_size = 49

let generate_operation =
  let open QCheck2.Gen in
  let* pass = oneofl all_passes in
  let* kind = oneofl (pass_to_operation_kinds pass) in
  let+ packed_operation =
    match kind with
    | `KPreattestation -> generate_operation generate_preattestation
    | `KAttestation -> generate_operation generate_attestation
    | `KSeed_nonce_revelation ->
        generate_operation generate_seed_nonce_revelation
    | `KVdf_revelation -> generate_operation generate_vdf_revelation
    | `KDouble_attestation -> generate_operation generate_double_attestation
    | `KDouble_preattestation ->
        generate_operation generate_double_preattestation
    | `KDouble_baking -> generate_operation generate_double_baking
    | `KActivate_account -> generate_operation generate_activate_account
    | `KProposals -> generate_operation generate_proposals
    | `KBallot -> generate_operation generate_ballot
    | `KManager ->
        let* batch_size = int_range 1 max_batch_size in
        generate_manager_operation batch_size
  in
  (kind, (Operation.hash_packed packed_operation, packed_operation))
