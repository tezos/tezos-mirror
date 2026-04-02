(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL denunciation tests.  Covers accusation injection (single, multi-lag,
  aggregated), duplicate denunciation handling, cross-cycle denunciations,
  faulty node entrapment, and All Bakers Attest scenarios. *)

open Dal_helpers
module Dal = Dal_common

let test_inject_accusation protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let slot_index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let lag = dal_parameters.attestation_lag in
  let lags = dal_parameters.attestation_lags in
  let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
  let commitment, proof, shards_with_proofs =
    Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
  in
  let* _op_hash =
    Helpers.publish_commitment
      ~source:Constant.bootstrap2
      ~index:slot_index
      ~commitment
      ~proof
      client
  in
  (* We need to bake exactly [lag] blocks so that we can inject an attestation
     at the right level (that attests the published slot). *)
  let* () = bake_for ~count:lag client in
  Log.info "Inject an attestation" ;
  let availability = Slots [slot_index] in
  let signer = Constant.bootstrap2 in
  let* attestation, _op_hash =
    inject_dal_attestation_exn
      ~protocol
      ~signer
      availability
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* signature = Operation.sign attestation client in
  let attestation = (attestation, signature) in
  let* shard_assignments =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_dal_shards
         ~delegates:[signer.public_key_hash]
         ()
  in
  let shard_index =
    JSON.(
      shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list
      |> List.hd |> as_int)
  in
  Log.info "First shard index of the attester is %d" shard_index ;
  let shard, proof =
    Seq.find
      (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
      shards_with_proofs
    |> Option.get
  in
  let accusation =
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      ~lag_index:(List.length lags - 1)
      shard
      proof
  in
  Log.info "Inject an accusation" ;
  let* _op_hash = Operation.Anonymous.inject accusation client in
  let* () = bake_for client in
  let* ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass
         ~block:"head"
         ~validation_pass:2
         ()
  in
  Check.(List.length (JSON.as_list ops) = 1)
    ~__LOC__
    Check.(int)
    ~error_msg:"Expected exactly one anonymous op. Got: %L" ;
  unit

(* This test exercises DAL entrapment evidence validation with dynamic,
   multi-lag DAL attestations.

   Context:
   - In dynamic-lag mode, a single attestation can carry DAL slot availability
     information for multiple lags.
   - An entrapment accusation includes:
       (attestation, lag_index, slot_index, shard, proof)
     and must be checked against the publication level derived from the
     attested level and the selected lag index.
   - The goal is to ensure accusation validation is performed against the
     correct (lag, slot, level) tuple, including shard ownership checks.

   Test structure:
   - Scenario A (single-lag attestation content, cross-lag accusations):
     - Publish a on slot 0, then b on slot 0 later.
     - Build Alice's attestation at a level where:
         lag_index=0 points to b, and lag_index=1 points to a.
     - Inject four accusations:
       A.1/A.2: accuse with lag_index=1 while Alice only attested lag_index=0
                -> must fail with "delegate did not attest the DAL slot".
       A.3: accuse with lag_index=0 with a shard assigned to Alice at lag_index=1
            not lag_index=0 -> must fail with "wrong shard owner".
       A.4: accuse with lag_index=0 with an Alice-assigned shard -> must succeed.

   - Scenario B (true multi-lag attestation payload):
     - Publish four commitments:
         a-slot2, a-slot3 and later b-slot2, b-slot3.
     - Craft one attestation where:
         lag_index=0 attests slot 2, and lag_index=1 attests slot 3.
     - Inject five accusations:
       B.1: lag_index=0, slot 3 (not attested at this lag)
            -> must fail with "delegate did not attest the DAL slot".
       B.2: Same as B.1 with lag_index=1, slot 2.
       B.3: lag_index=1, slot 3 with a valid Alice shard -> must succeed.
       B.4: lag_index=0, slot 2 with a shard assigned to Alice at lag_index=1
            not lag_index=0 -> must fail with "wrong shard owner".
       B.5: lag_index=0, slot 2 with an Alice-assigned shard
            -> must succeed.

   - Scenario C (max lag sanity/parity):
     - Publish one slot and craft an attestation targeting the largest lag
       (i.e. lag_index = number_of_lags - 1).
     - Inject one accusation with matching lag/slot/shard
       -> must succeed.

   Each scenario bakes after successful accusations and verifies that a
   dal_entrapment_evidence operation is present in validation pass 2. *)
let test_inject_accusation_dynamic_multi_lag protocol dal_parameters cryptobox
    node client _bootstrap_key =
  let attestation_lags = dal_parameters.Dal.Parameters.attestation_lags in
  let number_of_lags = List.length attestation_lags in
  let* () =
    if number_of_lags < 3 then
      Test.fail
        "Unexpected attestation_lags. Expected at least 3 lags, got [%s]"
        (dal_parameters.attestation_lags |> List.map string_of_int
       |> String.concat "; ")
    else unit
  in
  let lag_with_index_0 = List.nth attestation_lags 0 in
  let lag_with_index_1 = List.nth attestation_lags 1 in
  (* We have the invariant that [dal_parameters.Dal.Parameters.attestation_lag]
     is the same as [List.nth attestation_lags (number_of_lags - 1)]. *)
  let max_lag = dal_parameters.Dal.Parameters.attestation_lag in
  let slot_size = dal_parameters.cryptobox.slot_size in
  let alice = Constant.bootstrap2 in
  let publish_commitment ?(source = Constant.bootstrap1) ~index ~message () =
    let slot = Helpers.(bytes_of_slot (make_slot ~slot_size message)) in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    let* _op_hash =
      Helpers.publish_commitment ~source ~index ~commitment ~proof client
    in
    return (List.of_seq shards_with_proofs)
  in
  let publish_commitment_and_bake ?source ~index ~message () =
    let* shards_with_proofs = publish_commitment ?source ~index ~message () in
    Log.info "Bake after publication for slot index=%d" index ;
    let* () = bake_for client in
    let* published_level = Node.get_level node in
    Log.info
      "Publication included at level=%d for slot index=%d (message=%s)"
      published_level
      index
      message ;
    return (published_level, shards_with_proofs)
  in
  let ensure_level target_level =
    let* current_level = Node.get_level node in
    if current_level > target_level then
      Test.fail
        "Cannot go back in time: current level=%d, target level=%d"
        current_level
        target_level
    else if current_level < target_level then
      bake_for ~count:(target_level - current_level) client
    else unit
  in
  let shard_with_proof_exn shards shard_index =
    match
      List.find_opt
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards
    with
    | Some x -> x
    | None ->
        Test.fail
          "Could not find shard index %d in locally computed shards"
          shard_index
  in
  let get_alice_indexes ~lag_index ~attestation_level =
    let committee_level =
      attestation_level + max_lag - List.nth attestation_lags lag_index
    in
    let* shard_assignments =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards
           ~level:committee_level
           ~delegates:[alice.public_key_hash]
           ()
    in
    match JSON.as_list shard_assignments with
    | [json] ->
        let indexes = JSON.(json |-> "indexes" |> as_list |> List.map as_int) in
        if List.is_empty indexes then
          Test.fail
            "Alice has no shard index at level %d, cannot build accusation \
             scenario"
            attestation_level
        else return indexes
    | _ ->
        Test.fail
          "No DAL shard assignment found for Alice (%s) at level %d"
          alice.public_key_hash
          attestation_level
  in
  (* [first_among_not_in from forbidden] returns the first element from [from]
     not in [forbidden]. It fails if no such element can be found.
     This function assumes both lists are sorted. *)
  let rec first_among_not_in from forbidden =
    match (from, forbidden) with
    | [], _ ->
        Test.fail
          ~__LOC__
          "Cannot find a shard assigned to Alice for lag 1 but not for lag 0."
    | from_hd :: _, [] -> from_hd
    | from_hd :: from_tl, forbidden_hd :: forbidden_tl ->
        if from_hd < forbidden_hd then from_hd
        else if from_hd = forbidden_hd then
          first_among_not_in from_tl forbidden_tl
        else first_among_not_in from forbidden_tl
  in
  let inject_accusation ~label ~error ~attestation ~lag_index ~slot_index
      ~shard_index ~shard ~proof =
    Log.info
      "[%s] Inject accusation: slot_index=%d shard_index=%d expected_error=%s"
      label
      slot_index
      shard_index
      (match error with
      | None -> "none (success expected)"
      | Some _ -> "some (failure expected)") ;
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol
        ~attestation
        ~slot_index
        ~lag_index
        shard
        proof
    in
    let* _op_hash = Operation.Anonymous.inject ?error accusation client in
    Log.info "[%s] Accusation injected as expected" label ;
    unit
  in
  let assert_dal_entrapment_in_head () =
    let* anonymous_ops =
      Node.RPC.call node
      @@ RPC.get_chain_block_operations_validation_pass
           ~block:"head"
           ~validation_pass:2
           ()
    in
    let has_dal_entrapment =
      List.exists
        (fun op ->
          let kind = JSON.(op |-> "contents" |=> 0 |-> "kind" |> as_string) in
          String.equal kind "dal_entrapment_evidence")
        (JSON.as_list anonymous_ops)
    in
    Check.is_true
      has_dal_entrapment
      ~__LOC__
      ~error_msg:"Expected a dal_entrapment_evidence operation in pass 2" ;
    unit
  in

  (* Scenario A *)
  Log.info "=== Scenario A start ===" ;
  let* _lvl_a, shards_a =
    publish_commitment_and_bake ~index:0 ~message:"a-slot0" ()
  in
  let* () =
    if lag_with_index_1 > lag_with_index_0 + 1 then
      bake_for ~count:(lag_with_index_1 - lag_with_index_0 - 1) client
    else unit
  in
  let* lvl_b, shards_b =
    publish_commitment_and_bake ~index:0 ~message:"b-slot0" ()
  in
  (* For accusations, published_level + attestation_lag = attestation_level + 1.
     To target lvl_b with lag=lag_with_index_0 use lvl_b + lag_with_index_0 - 1.
     It is the same as lvl_a + lag_with_index_1 - 1. *)
  let attestation_level_A = lvl_b + (lag_with_index_0 - 1) in
  let* () = ensure_level attestation_level_A in
  Log.info
    "Scenario A: craft attestation at level=%d lag_index=%d slots=[0]"
    attestation_level_A
    0 ;
  let* attestation_a =
    craft_dal_attestation_exn
      ~protocol
      ~level:attestation_level_A
      ~lag_index:0
      ~signer:alice
      (Slots [0])
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* signature_a = Operation.sign attestation_a client in
  let attestation_a = (attestation_a, signature_a) in
  (* To make all this final, one bakes 2 blocks. *)
  let* () = bake_for ~count:2 client in
  let* alice_indexes_lag_0 =
    get_alice_indexes ~lag_index:0 ~attestation_level:attestation_level_A
  in
  let alice_index_lag_0 = List.hd alice_indexes_lag_0 in
  let* alice_indexes_lag_1 =
    get_alice_indexes ~lag_index:1 ~attestation_level:attestation_level_A
  in
  let alice_index_lag_1 =
    first_among_not_in alice_indexes_lag_1 alice_indexes_lag_0
  in
  let shard_a_1, proof_a_1 = shard_with_proof_exn shards_a alice_index_lag_0 in
  let shard_a_2, proof_a_2 = shard_with_proof_exn shards_a alice_index_lag_1 in
  let shard_b_wrong, proof_b_wrong =
    shard_with_proof_exn shards_b alice_index_lag_1
  in
  let shard_b_ok, proof_b_ok =
    shard_with_proof_exn shards_b alice_index_lag_0
  in
  let* () =
    inject_accusation
      ~label:"A.1 not-attested(a, getting shard index with lag index 0)"
      ~error:(Some Operation.dal_entrapment_slot_not_attested)
      ~attestation:attestation_a
      ~lag_index:1
      ~slot_index:0
      ~shard_index:alice_index_lag_0
      ~shard:shard_a_1
      ~proof:proof_a_1
  in
  let* () =
    inject_accusation
      ~label:"A.2 not-attested(a, getting shard index with lag index 1)"
      ~error:(Some Operation.dal_entrapment_slot_not_attested)
      ~attestation:attestation_a
      ~lag_index:1
      ~slot_index:0
      ~shard_index:alice_index_lag_1
      ~shard:shard_a_2
      ~proof:proof_a_2
  in
  let* () =
    inject_accusation
      ~label:"A.3 wrong-owner(b)"
      ~error:(Some Operation.dal_entrapment_wrong_shard_owner)
      ~attestation:attestation_a
      ~lag_index:0
      ~slot_index:0
      ~shard_index:alice_index_lag_1
      ~shard:shard_b_wrong
      ~proof:proof_b_wrong
  in
  let* () =
    inject_accusation
      ~label:"A.4 success(b)"
      ~error:None
      ~attestation:attestation_a
      ~lag_index:0
      ~slot_index:0
      ~shard_index:alice_index_lag_0
      ~shard:shard_b_ok
      ~proof:proof_b_ok
  in
  Log.info "Scenario A: bake inclusion block for successful accusation" ;
  let* () = bake_for client in
  let* () = assert_dal_entrapment_in_head () in
  Log.info "=== Scenario A done ===" ;

  (* Scenario B (+ ownership check) *)
  Log.info "=== Scenario B start ===" ;
  let* shards_a2 =
    publish_commitment
      ~source:Constant.bootstrap1
      ~index:2
      ~message:"a-slot2"
      ()
  in
  let* lvl_a, shards_a3 =
    publish_commitment_and_bake
      ~source:Constant.bootstrap2
      ~index:3
      ~message:"a-slot3"
      ()
  in
  let* () =
    if lag_with_index_1 > lag_with_index_0 + 1 then
      bake_for ~count:(lag_with_index_1 - lag_with_index_0 - 1) client
    else unit
  in
  let* shards_b2 =
    publish_commitment
      ~source:Constant.bootstrap1
      ~index:2
      ~message:"b-slot2"
      ()
  in
  let* lvl_b, shards_b3 =
    publish_commitment_and_bake
      ~source:Constant.bootstrap2
      ~index:3
      ~message:"b-slot3"
      ()
  in
  (* For accusations, published_level + attestation_lag = attestation_level + 1.
     To target B@lvl_b with lag=lag_with_index_0 use lvl_b + lag_with_index_0 - 1. *)
  let attestation_level_B = lvl_b + (lag_with_index_0 - 1) in
  let* () = ensure_level attestation_level_B in
  Log.info
    "Scenario B: collect round/payload/slot at level=%d"
    attestation_level_B ;
  let* round =
    Client.RPC.call_via_endpoint client
    @@ RPC.get_chain_block_helper_round
         ~block:(string_of_int attestation_level_B)
         ()
  in
  let* block_payload_hash =
    Operation.Consensus.get_block_payload_hash
      ~block:(string_of_int attestation_level_B)
      client
  in
  let* slot =
    Operation.Consensus.get_attestation_slot
      ~level:attestation_level_B
      ~delegate:alice
      ~protocol
      client
  in
  let attestation_per_lag =
    Array.init number_of_lags (fun _ ->
        Array.make dal_parameters.number_of_slots false)
  in
  (* We attest A-slot3 and B-slot2.
     We positioned ourselves at a level such that lag_index_0 points to the
     publication level of B and lag_index_1 points to the publication level of A. *)
  attestation_per_lag.(0).(2) <- true ;
  attestation_per_lag.(1).(3) <- true ;
  let* dal_attestation =
    Dal.Attestations.encode
      protocol
      (Node.as_rpc_endpoint node)
      attestation_per_lag
  in
  let* attestation_B =
    Operation.Consensus.operation
      ~signer:alice
      (Operation.Consensus.attestation
         ~level:attestation_level_B
         ~round
         ~dal_attestation
         ~slot
         ~block_payload_hash
         ())
      client
  in
  let* signature_b = Operation.sign attestation_B client in
  let attestation_B = (attestation_B, signature_b) in
  let* alice_indexes_lag_0 =
    get_alice_indexes ~lag_index:0 ~attestation_level:attestation_level_B
  in
  let alice_index_lag_0 = List.hd alice_indexes_lag_0 in
  let* alice_indexes_lag_1 =
    get_alice_indexes ~lag_index:1 ~attestation_level:attestation_level_B
  in
  let alice_index_lag_1 =
    first_among_not_in alice_indexes_lag_1 alice_indexes_lag_0
  in
  let shard_a2, proof_a2 = shard_with_proof_exn shards_a2 alice_index_lag_1 in
  let shard_a3, proof_a3 = shard_with_proof_exn shards_a3 alice_index_lag_1 in
  let shard_b3, proof_b3 = shard_with_proof_exn shards_b3 alice_index_lag_0 in
  let shard_b2_wrong, proof_b2_wrong =
    shard_with_proof_exn shards_b2 alice_index_lag_1
  in
  let shard_b2_ok, proof_b2_ok =
    shard_with_proof_exn shards_b2 alice_index_lag_0
  in
  Check.(
    (attestation_level_B + 1 - lag_with_index_0 = lvl_b)
      int
      ~__LOC__
      ~error_msg:
        "Scenario B lag2 targets unexpected level: actual %L, expected %R") ;
  Check.(
    (attestation_level_B + 1 - lag_with_index_1 = lvl_a)
      int
      ~__LOC__
      ~error_msg:
        "Scenario B lag3 targets unexpected level: actual %L, expected %R") ;
  let* () =
    inject_accusation
      ~label:"B.1 slot-not-attested(b-slot3)"
      ~error:(Some Operation.dal_entrapment_slot_not_attested)
      ~attestation:attestation_B
      ~lag_index:0
      ~slot_index:3
      ~shard_index:alice_index_lag_0
      ~shard:shard_b3
      ~proof:proof_b3
  in
  let* () =
    inject_accusation
      ~label:"B.2 slot-not-attested(a-slot2)"
      ~error:(Some Operation.dal_entrapment_slot_not_attested)
      ~attestation:attestation_B
      ~lag_index:1
      ~slot_index:2
      ~shard_index:alice_index_lag_1
      ~shard:shard_a2
      ~proof:proof_a2
  in
  let* () =
    inject_accusation
      ~label:"B.3 success(a-slot3)"
      ~error:None
      ~attestation:attestation_B
      ~lag_index:1
      ~slot_index:3
      ~shard_index:alice_index_lag_1
      ~shard:shard_a3
      ~proof:proof_a3
  in
  let* () = bake_for client in
  let* () = assert_dal_entrapment_in_head () in
  let* () =
    inject_accusation
      ~label:"B.4 wrong-owner(b-slot2)"
      ~error:(Some Operation.dal_entrapment_wrong_shard_owner)
      ~attestation:attestation_B
      ~lag_index:0
      ~slot_index:2
      ~shard_index:alice_index_lag_1
      ~shard:shard_b2_wrong
      ~proof:proof_b2_wrong
  in
  let* () =
    inject_accusation
      ~label:"B.5 success(b-slot2)"
      ~error:None
      ~attestation:attestation_B
      ~lag_index:0
      ~slot_index:2
      ~shard_index:alice_index_lag_0
      ~shard:shard_b2_ok
      ~proof:proof_b2_ok
  in
  Log.info "Scenario B: bake inclusion block for successful accusation" ;
  let* () = bake_for client in
  let* () = assert_dal_entrapment_in_head () in
  Log.info "=== Scenario B done ===" ;

  (* Scenario C (lag max parity) *)
  Log.info "=== Scenario C start ===" ;
  let* lvl_c, shards_c =
    publish_commitment_and_bake ~index:0 ~message:"c-slot0" ()
  in
  (* [published_level + lag = attestation_level + 1], since attestations included
     in level N attests for block N-1.
     So for lag=5 use +4. *)
  let attestation_level_C = lvl_c + max_lag - 1 in
  let max_index = number_of_lags - 1 in
  let* () = ensure_level attestation_level_C in
  Log.info
    "Scenario C: craft attestation at level=%d lag_index=%d slots=[0]"
    attestation_level_C
    max_index ;
  let* attestation_c =
    craft_dal_attestation_exn
      ~protocol
      ~level:attestation_level_C
      ~lag_index:max_index
      ~signer:alice
      (Slots [0])
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* signature_c = Operation.sign attestation_c client in
  let attestation_c = (attestation_c, signature_c) in
  let* alice_indexes =
    get_alice_indexes ~lag_index:0 ~attestation_level:attestation_level_C
  in
  let alice_index_lag_0 = List.hd alice_indexes in
  let shard_c_ok, proof_c_ok =
    shard_with_proof_exn shards_c alice_index_lag_0
  in
  let* () =
    inject_accusation
      ~label:"C.1 success(slot0 lag5)"
      ~error:None
      ~attestation:attestation_c
      ~lag_index:max_index
      ~slot_index:0
      ~shard_index:alice_index_lag_0
      ~shard:shard_c_ok
      ~proof:proof_c_ok
  in
  Log.info "Scenario C: bake inclusion block for successful accusation" ;
  let* () = bake_for client in
  let* () = assert_dal_entrapment_in_head () in
  Log.info "=== Scenario C done ===" ;
  unit

type tz4_account = {delegate_key : Account.key; companion_key : Account.key}

(* [round_robin n l] distributes the values of [l] in an array of [n] elements.
   so [round_robin 3 [a1; ..; a10] = [|[a1;a4;a7;a10]; [a2;a5;a8]; [a3;a6;a9]|]].
*)
let round_robin n l =
  let rec bis acc k = function
    | [] -> Array.map List.rev acc
    | hd :: tl ->
        let acc_local = acc.(k) in
        let () = acc.(k) <- hd :: acc_local in
        bis acc ((k + 1) mod n) tl
  in
  bis (Array.init n (fun _ -> [])) 0 l

let create_tz4_accounts_stake_and_wait ~funders ~client ~node nb_to_create =
  Log.info "Generate tz4 keys" ;
  let* new_tz4_accounts =
    Lwt_list.map_s
      (fun index ->
        Client.gen_and_show_keys
          ~sig_alg:"bls"
          ~alias:("bls_account_" ^ string_of_int (1 + index))
          client)
      (List.init nb_to_create Fun.id)
  in
  let* () = bake_for client in
  Log.info "We have the accounts, let's fund them" ;
  let* _ophs =
    Lwt.all
    @@ List.map2
         (fun accounds_to_fund source ->
           (* We give 500k tez to each account. *)
           let amount = 500_000 * 1_000_000 in
           let transfers =
             List.map
               (fun dest -> Operation.Manager.transfer ~dest ~amount ())
               accounds_to_fund
           in
           let* counter = Operation.Manager.get_next_counter ~source client in
           let batch =
             Operation.Manager.make_batch ~source ~counter transfers
           in
           Operation.Manager.inject batch client)
         (Array.to_list @@ round_robin (List.length funders) new_tz4_accounts)
         funders
  in
  let* () = bake_for client in
  Log.info "All keys funded, let's set them as delegates" ;
  let* () =
    Lwt_list.iter_s
      (fun account ->
        Client.set_delegate
          ~src:account.Account.alias
          ~delegate:account.Account.alias
          client)
      new_tz4_accounts
  in
  let* () = bake_for client in
  Log.info "All keys are delegate, let's attach a companion key to them" ;
  let* companions =
    Lwt_list.map_s
      (fun account ->
        Client.update_fresh_companion_key ~algo:"bls" account client)
      new_tz4_accounts
  in
  let* () = bake_for client in
  Log.info "All keys have a companion, let's stake" ;
  let* () =
    Lwt_list.iter_s
      (fun source ->
        Client.stake ~staker:source.Account.alias Tez.(of_int 100_000) client)
      new_tz4_accounts
  in
  let* RPC.{cycle; _} =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  Log.info "All keys have staked, let's wait for them to have rights" ;
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let* () =
    Client.bake_until_cycle
      ~target_cycle:(cycle + consensus_rights_delay + 1)
      client
  in
  return
  @@ List.map2
       (fun delegate_key companion_key -> {delegate_key; companion_key})
       new_tz4_accounts
       companions

let test_aggregation_required_to_pass_quorum protocol dal_parameters _cryptobox
    node client dal_node =
  let slot_index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator slot_index])) in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let lag = dal_parameters.attestation_lag in
  let* _tz4_accounts =
    create_tz4_accounts_stake_and_wait
      ~funders:(Account.Bootstrap.keys |> Array.to_list)
      ~node
      ~client
      25
  in
  Log.info "Enough waiting, let's publish a commitment" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      message
  in
  let* () = bake_for client in
  let* published_level = Node.get_level node in
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint dal_node |> Endpoint.as_string
  in
  Log.info "Let's wait for the attestation of this publication to be sent." ;
  let* () = bake_for ~count:lag client ~dal_node_endpoint in
  let* attestations =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
  in
  let aggregated_attestation =
    JSON.(
      List.find
        (fun attestation ->
          attestation |-> "contents" |> as_list |> List.hd |-> "kind"
          |> as_string = "attestations_aggregate")
        (as_list attestations))
  in
  let aggregated_attestation_consensus_power =
    JSON.(
      aggregated_attestation |-> "contents" |> as_list |> List.hd |-> "metadata"
      |-> "total_consensus_power"
      |> fun x ->
      if Protocol.number protocol >= 024 then x |-> "slots" |> as_int
      else x |> as_int)
  in
  let* constants =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_committee_size =
    JSON.(constants |-> "consensus_committee_size" |> as_int)
  in
  Check.(
    aggregated_attestation_consensus_power
    >= consensus_committee_size
       * (100 - dal_parameters.attestation_threshold)
       / 100)
    ~__LOC__
    Check.int
    ~error_msg:
      "The consensus power of the tz4 accounts is not sufficient to ensure \
       that the ability to read inside the aggregated attestations is required \
       for the slot to be protocol attested." ;
  let check_at_level ~level ~lag ~lag_index =
    let* metadata =
      Node.RPC.(
        call node @@ get_chain_block_metadata ~block:(string_of_int level) ())
    in
    match metadata.dal_attestation with
    | None ->
        Test.fail
          "Field dal_attestation in block headers is mandatory when DAL is \
           activated"
    | Some str ->
        let* decoded =
          Dal.Slot_availability.decode
            protocol
            (Node.as_rpc_endpoint node)
            dal_parameters
            str
        in
        let bitset = decoded.(lag_index) in
        let attested_count =
          Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 bitset
        in
        Log.info
          "Check dal_attestation at level=%d (lag=%d, lag_index=%d): \
           attested_count=%d slot[%d]=%b"
          level
          lag
          lag_index
          attested_count
          slot_index
          bitset.(slot_index) ;
        return (attested_count = 1 && bitset.(slot_index))
  in
  let lags = dal_parameters.attestation_lags in
  let* results =
    Lwt_list.mapi_s
      (fun lag_index lag ->
        let attested_level = published_level + lag in
        check_at_level ~level:attested_level ~lag ~lag_index)
      lags
  in
  if List.exists Fun.id results then unit
  else
    Test.fail
      "Slot was NOT protocol attested at any expected attested level \
       (published_level=%d, lags=%s)"
      published_level
      (String.concat "," (List.map string_of_int lags))
      unit

let test_inject_accusation_aggregated_attestation nb_attesting_tz4 protocol
    dal_parameters _cryptobox node client dal_node =
  let slot_index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator slot_index])) in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.number_of_slots in
  let lag = dal_parameters.attestation_lag in
  let* tz4_accounts =
    create_tz4_accounts_stake_and_wait
      ~funders:[Constant.bootstrap1]
      ~node
      ~client
      5
  in
  Log.info "Enough waiting, let's publish a commitment" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      message
  in
  (* We need to bake exactly [lag] blocks so that we can inject an attestation
     at the right level (that attests the published slot). *)
  let* publication_level = Client.bake_for_and_wait_level client in
  Log.info "Attestation lag should pass before sending an attestation" ;
  let* () = bake_for ~count:(lag - 1) client in
  Log.info "Inject an attestation" ;
  let level = publication_level + lag - 1 in
  (* BLS consensus keys are now activated *)
  let* round = Baker_test.fetch_round client in
  let* branch = Operation.Consensus.get_branch ~attested_level:level client in
  let* block_payload_hash =
    Operation.Consensus.get_block_payload_hash
      ~block:(string_of_int level)
      client
  in
  let* dal_attestation =
    Dal.Attestations.encode_for_one_lag
      protocol
      (Node.as_rpc_endpoint node)
      dal_parameters
    @@ Array.init number_of_slots (fun i -> i = slot_index)
  in
  let faulty_bakers, other_tz4 =
    Tezos_stdlib.TzList.split_n nb_attesting_tz4 tz4_accounts
  in
  let* () =
    Lwt_list.iter_s
      (fun {delegate_key; companion_key} ->
        let* first_slot =
          Operation.Consensus.get_attestation_slot
            ~level
            ~protocol
            ~delegate:delegate_key
            client
        in
        let attestation =
          Operation.Consensus.attestation
            ~dal_attestation
            ~slot:first_slot
            ~level
            ~round
            ~block_payload_hash
            ()
        in
        let* _op_hash =
          Operation.Consensus.inject
            ~protocol
            ~branch
            ~signer_companion:companion_key
            ~signer:delegate_key
            attestation
            client
        in
        unit)
      faulty_bakers
  in
  Log.info "Attestation injected" ;
  (* Since we manually injected the attestation for the faulty baker,
     we have to prevent [bake_for] to inject another one in the mempool.
  *)
  let all_other_delegates =
    List.map (fun {delegate_key; _} -> delegate_key) other_tz4
    @ Constant.all_secret_keys
  in
  let* () =
    bake_for
      ~delegates:
        (`For
           (List.map
              (fun account -> account.Account.public_key_hash)
              all_other_delegates))
      client
  in
  (* Let's wait 2 levels for the block to be finalized. *)
  let* () = bake_for ~count:2 client in
  (* Now we expect the DAL node to be crafting the denunciation,
     hence it should be included in next block.
  *)
  let* () = bake_for client in
  let* anonymous_ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass
         ~block:"head"
         ~validation_pass:2
         ()
  in
  Check.(List.length (JSON.as_list anonymous_ops) = nb_attesting_tz4)
    ~__LOC__
    Check.int
    ~error_msg:"Expected exactly %R anonymous op. Got: %L" ;
  let contents = JSON.(anonymous_ops |=> 0 |-> "contents" |=> 0) in
  let kind = JSON.(contents |-> "kind" |> as_string) in
  let attestation_kind =
    JSON.(contents |-> "attestation" |-> "operations" |-> "kind" |> as_string)
  in
  Check.(
    ((kind, attestation_kind)
    = ("dal_entrapment_evidence", "attestations_aggregate"))
      ~__LOC__
      (tuple2 string string))
    ~error_msg:
      "Expected the anonymous operation to be the denunciation of attestation \
       aggregation. Got: %L" ;
  unit

(* [test_duplicate_denunciations] publishes a commitment for slot [0].  After
   baking [attestation_lag] blocks, the test then injects a DAL trapped
   attestation. Then it attempts to:

   - inject a first denunciation that is expected to succeed,

   - inject a second denunciation built using a different shard that
     is a trap as well, but using the same slot and the same l1 level,
     where each try is expected to fail. The injection temptatives are
     done:
     1. at the next block level (expecting delegate already denounced)
     2. at the next cycle (expecting delegate already denounced)
     3. at the next-next cycle (expecting outdated evidence)
*)
let test_duplicate_denunciations protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let slot_index = 0 in
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let attestation_lag = dal_parameters.Dal.Parameters.attestation_lag in
  let lags = dal_parameters.Dal.Parameters.attestation_lags in
  assert (attestation_lag <= blocks_per_cycle) ;
  let blocks_to_bake = 2 + blocks_per_cycle - attestation_lag in
  Log.info
    "Bake %d blocks so that the attestation level is in cycle 1"
    blocks_to_bake ;
  let* () = bake_for ~count:blocks_to_bake client in
  let accused = Constant.bootstrap2 in
  let* current_level = Node.get_level node in
  let* shards_with_proofs =
    let slot_size = dal_parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    Log.info "Publish commitment at level %d" current_level ;
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:attestation_lag client in
  let* current_level = Node.get_level node in
  Log.info "Injecting an attestation at level %d" current_level ;
  let availability = Slots [slot_index] in
  let signer = Constant.bootstrap2 in
  let* attestation, _op_hash =
    inject_dal_attestation_exn
      ~protocol
      ~signer
      availability
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* signature = Operation.sign attestation client in
  let attestation = (attestation, signature) in
  let* shard_assignments =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_dal_shards
         ~delegates:[accused.public_key_hash]
         ()
  in
  let shard_indexes =
    JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
  in
  let[@ocaml.warning "-8"] (shard_index_1 :: shard_index_2 :: _) =
    List.map JSON.as_int shard_indexes
  in
  let accusation =
    let shard1, proof1 =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index_1)
        shards_with_proofs
      |> Option.get
    in
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      ~lag_index:(List.length lags - 1)
      shard1
      proof1
  in
  let accusation_dup =
    let shard2, proof2 =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index_2)
        shards_with_proofs
      |> Option.get
    in
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      ~lag_index:(List.length lags - 1)
      shard2
      proof2
  in
  let* level_accusation = Node.get_level node in
  Log.info
    "Injecting a first accusation (level %d) expected to succeed"
    level_accusation ;
  let* (`OpHash _) = Operation.Anonymous.inject accusation client in
  let* () = bake_for client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting the second accusation at the next level (level %d), expected to \
     fail because the faulty delegate is already denounced"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation.already_dal_denounced
  in
  let* () = bake_for ~count:blocks_per_cycle client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting at the next cycle (level %d), expected to fail because the \
     delegate is already denounced for this level"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation.already_dal_denounced
  in
  let* () = bake_for ~count:blocks_per_cycle client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting at the next cycle (level %d), expected to fail because the DAL \
     denunciation is outdated"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation.outdated_dal_denunciation
  in
  unit

(* [test_denunciation_next_cycle] injects two trapped attestations and
   inject an accusation for the first one at cycle `c` and another at
   for the second one at cycle `c + 1`, both injections are expected
   to succeed. *)
let test_denunciation_next_cycle protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let slot_index = 0 in
  let lags = dal_parameters.Dal.Parameters.attestation_lags in
  let accused = Constant.bootstrap2 in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* () = bake_for ~count:2 client in
  let* shards_with_proofs1 =
    let* current_level = Node.get_level node in
    Log.info "Publish first commitment at level %d" current_level ;
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:1 client in
  let* shards_with_proofs2 =
    let* current_level = Node.get_level node in
    Log.info "Publish second commitment at level %d" current_level ;
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:(dal_parameters.attestation_lag - 1) client in
  let node_endpoint = Node.as_rpc_endpoint node in
  let* attestation =
    let* current_level = Node.get_level node in
    Log.info "Injecting a first attestation at level %d" current_level ;
    let availability = Slots [slot_index] in
    let* attestation, _op_hash =
      inject_dal_attestation_exn
        ~protocol
        ~signer:Constant.bootstrap2
        availability
        client
        dal_parameters
        node_endpoint
    in
    let* signature = Operation.sign attestation client in
    return (attestation, signature)
  in
  let* () =
    let* shard_assignments =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards
           ~delegates:[accused.public_key_hash]
           ()
    in
    let shard_indexes =
      JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
    in
    let[@ocaml.warning "-8"] (shard_index :: _) =
      List.map JSON.as_int shard_indexes
    in
    let shard, proof =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards_with_proofs1
      |> Option.get
    in
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol
        ~attestation
        ~slot_index
        ~lag_index:(List.length lags - 1)
        shard
        proof
    in
    let* level_accusation = Node.get_level node in
    Log.info
      "Injecting a first accusation (level %d) expected to succeed"
      level_accusation ;
    let* (`OpHash _) = Operation.Anonymous.inject accusation client in
    unit
  in
  let* () = bake_for ~count:1 client in
  let* attestation =
    let* current_level = Node.get_level node in
    Log.info "Injecting a second attestation at level %d" current_level ;
    let availability = Slots [slot_index] in
    let signer = Constant.bootstrap2 in
    let* attestation, _op_hash =
      inject_dal_attestation_exn
        ~protocol
        ~signer
        availability
        client
        dal_parameters
        node_endpoint
    in
    let* signature = Operation.sign attestation client in
    return (attestation, signature)
  in
  let* () =
    let* shard_assignments =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards
           ~delegates:[accused.public_key_hash]
           ()
    in
    let shard_indexes =
      JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
    in
    let[@ocaml.warning "-8"] (shard_index :: _) =
      List.map JSON.as_int shard_indexes
    in
    let shard, proof =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards_with_proofs2
      |> Option.get
    in
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol
        ~attestation
        ~slot_index
        ~lag_index:(List.length lags - 1)
        shard
        proof
    in
    let* () = bake_for ~count:blocks_per_cycle client in
    let* level = Node.get_level node in
    Log.info
      "Injecting the second accusation (level %d) expected to succeed"
      level ;
    let* (`OpHash _) = Operation.Anonymous.inject accusation client in
    unit
  in
  unit

(**  [test_e2e_trap_faulty_dal_node] verifies a scenario where a
     [faulty_delegate] misbehaves. Specifically, it:

     - Creates a DAL node [proxy] whose role is to mimick the honest
       [dal_node] by forwarding to the honest DAL node each RPC call,
       except for the attestable-slots endpoints for the [faulty_delegate]:
         - the streamed RPC [/profiles/<tz>/monitor/attestable_slots], where
           the proxy rewrites the JSON stream so that, at
           [<n> = 2 * blocks_per_cycle + 1] (the target attested level),
           the delegate appears to have all slots attestable (by adding
           the corresponding slot_ids in the backfill and/or live events);
         - the legacy per-level RPC [/profiles/<tz>/attested_levels/<n>/attestable_slots],
           which is also overridden for compatibility by flipping the returned
           ['attestable_slots_set'] array to all ['true'].

     The test proceeds as follows:

     1. Bakes [blocks_per_cycle] blocks to avoid the period in which
        the DAL node is not able to inject an accusation because of the
        accusation delay introduced by the migration.

     2. Bakes [blocks_per_cycle] blocks while publishing on slot index
        [0] and checks that [faulty_delegate] is not denounced at the end.

     3. Bakes again [blocks_per_cycle] blocks to finally reach [3 *
        blocks_per_cycle] blocks.

     - Finally, the test

        - a) retrieves the balance updates for the last block of
             the third cycle and verifies that:
             - Multiple DAL attesting rewards were minted.
             - The delegates that lost the DAL rewards are the [faulty_delegate]
               and the ones that lost the consensus attesting rewards because they
               haven't revealed nonces.

        - b) retrieves the [faulty_delegate] DAL participation at the
             end of the third cycle, minus one block (i.e "head~1")
             because DAL participation are reset at the end of a block
             preceding a new cycle, and checks that:

             - it has a sufficient DAL participation to get rewards,
             - it is denounced,
             - its expected DAL rewards are [0].
*)
let test_e2e_trap_faulty_dal_node protocol dal_parameters _cryptobox node client
    dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let target_attested_level = (2 * blocks_per_cycle) + 1 in
  let faulty_delegate = Constant.bootstrap1.Account.public_key_hash in
  let proxy =
    Dal_node.Proxy.make
      ~name:"proxy-dal-node"
      ~attestation_lag:dal_parameters.Dal.Parameters.attestation_lag
      ~number_of_slots:dal_parameters.Dal.Parameters.number_of_slots
      ~faulty_delegate
      ~target_attested_level
  in
  let faulty_dal_node =
    Dal_node.create
      ~name:"faulty-dal-node"
      ~listen_addr:(Dal_node.listen_addr dal_node)
      ~node
      ()
  in
  let () =
    Dal_node.Proxy.run ~honest_dal_node:dal_node ~faulty_dal_node proxy
  in
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint faulty_dal_node |> Endpoint.as_string
  in
  let* level =
    let* first_level = Node.get_level node in
    let block = string_of_int first_level in
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ~block ()
  in
  let* () =
    bake_for
      ~count:(blocks_per_cycle - 1 - level.cycle_position)
      ~dal_node_endpoint
      client
  in
  let* _ = Node.wait_for_level node blocks_per_cycle in
  let* () =
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    repeat blocks_per_cycle (fun () ->
        let wait_slot ~published_level ~slot_index:_ =
          wait_for_layer1_final_block dal_node (published_level - 2)
        in
        let* _res =
          publish_store_and_wait_slot
            node
            client
            dal_node
            Constant.bootstrap2
            ~index:0
            ~wait_slot
            ~number_of_extra_blocks_to_bake:0
          @@ Helpers.make_slot ~slot_size "content"
        in
        unit)
  in
  let level = ref (2 * blocks_per_cycle) in
  let* _ = Node.wait_for_level node !level in
  let* faulty_delegate_dal_participation =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         faulty_delegate
  in
  Check.is_false
    faulty_delegate_dal_participation.denounced
    ~__LOC__
    ~error_msg:"Expected the faulty delegate to not be denounced" ;
  let wait_for_trap_injection =
    Dal_node.wait_for dal_node "dal_trap_injection.v0" (fun e ->
        let open JSON in
        let delegate = e |-> "delegate" |> as_string in
        let attested_level = e |-> "attested_level" |> as_int in
        if delegate = faulty_delegate && attested_level = target_attested_level
        then Some ()
        else None)
  in
  let* () =
    repeat blocks_per_cycle (fun () ->
        let wait = wait_for_layer1_final_block dal_node (!level - 1) in
        let* () = bake_for ~dal_node_endpoint client in
        incr level ;
        let* () = wait in
        unit)
  in
  let* () = wait_for_trap_injection in
  let* _ = Node.wait_for_level node (3 * blocks_per_cycle) in
  let* metadata = Node.RPC.call node @@ RPC.get_chain_block_metadata_raw () in
  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  let dal_rewards =
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "minted"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "DAL attesting rewards")
      balance_updates
  in
  Check.(List.length dal_rewards > 1)
    ~__LOC__
    Check.int
    ~error_msg:"Expected %R minted DAL-related balance updates, got %L" ;
  let lost_dal_rewards =
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "burned"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "lost DAL attesting rewards")
      balance_updates
  in
  let lost_consensus_rewards =
    (* filter those that lost consensus attesting rewards because they haven't
       revealed their nonces *)
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "burned"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "lost attesting rewards"
        && (not JSON.(json |-> "participation" |> as_bool))
        && JSON.(json |-> "revelation" |> as_bool))
      balance_updates
  in
  let get_delegates =
    List.map (fun json -> JSON.(json |-> "delegate" |> as_string))
  in
  let sort_unique = List.sort_uniq String.compare in
  let losing_delegates = sort_unique @@ get_delegates lost_dal_rewards in
  let expected_to_lose_delegates =
    if Protocol.number protocol >= 023 then
      sort_unique (faulty_delegate :: get_delegates lost_consensus_rewards)
    else [faulty_delegate]
  in
  Check.(expected_to_lose_delegates = losing_delegates)
    ~__LOC__
    Check.(list string)
    ~error_msg:"Unexpected delegates to lose DAL rewards (got %R expected %L)" ;
  let* faulty_delegate_dal_participation =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         faulty_delegate
  in
  Check.is_true
    faulty_delegate_dal_participation.denounced
    ~__LOC__
    ~error_msg:"Expected the faulty delegate to be denounced" ;
  Check.is_true
    faulty_delegate_dal_participation.sufficient_dal_participation
    ~__LOC__
    ~error_msg:"Expected sufficiant participation for the faulty delegate" ;
  Check.(
    Tez.to_mutez faulty_delegate_dal_participation.expected_dal_rewards = 0)
    ~__LOC__
    Check.int
    ~error_msg:
      "Expected DAL rewards for for the faulty delegate expected to be %R, but \
       got %L" ;
  unit

(* [test_denunciation_when_all_bakers_attest] checks that a delegate who
   wrongly attests is denounced by the DAL node, and that this denunciation is
   included in a block by the protocol.

   The test consists of:
   - enabling the "all bakers attest" feature,
   - publishing commitments,
   - forcing a baker to attest to those commitments, despite the traps_fraction
     being set to 1 (so all shards are traps),
   - letting the publisher DAL node identify that those attestations are wrong
     and denounce them,
   - and checking that the denunciations are indeed included in a block by the
     protocol.

   The rationale for this test is that before the "all bakers attest" feature,
   the slot index contained in an attestation was the same as the smallest shard
   index assigned to a baker.

   This property was used in the `get_committees` function of the protocol
   plugin. Hence, before https://gitlab.com/tezos/tezos/-/merge_requests/19698,
   the DAL accuser would associate attestations with the wrong bakers when
   "all bakers attest" was enabled. This test exposes that issue.

   It should be noted that it may still happen that the slot index in an
   attestation coincides with the smallest shard index. To prevent such a
   collision from making the test pass by chance, three publications are
   attested and denounced at three consecutive slots, making the probability
   of a false positive (a test passing by luck) very low.
*)
let test_denunciation_when_all_bakers_attest protocol dal_parameters _cryptobox
    l1_node client dal_node =
  let slot_index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator slot_index])) in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let attestation_lag = dal_parameters.attestation_lag in
  (* To trigger the "all bakers attest" feature, one has to create enough tz4
     and stake with them. *)
  let* tz4_accounts =
    create_tz4_accounts_stake_and_wait
      ~funders:[Constant.bootstrap1; Constant.bootstrap2]
      ~node:l1_node
      ~client
      10
  in
  let all_delegates =
    Array.to_list Account.Bootstrap.keys
    @ List.map (fun {delegate_key; _} -> delegate_key) tz4_accounts
  in
  let delegate_wrongly_attestating = List.hd all_delegates in
  let rest_delegates_pkhs =
    List.map (fun key -> key.Account.public_key_hash) (List.tl all_delegates)
  in
  Log.info "Enough waiting, let's publish 3 commitments" ;
  let* () =
    repeat 3 (fun () ->
        let message = Helpers.make_slot ~slot_size "Hello world!" in
        let* _pid =
          Helpers.publish_and_store_slot
            client
            dal_node
            delegate_wrongly_attestating
            ~index:slot_index
            message
        in
        bake_for client)
  in
  let* () = bake_for ~count:(attestation_lag - 3) client in
  Log.info "Our byzantine baker to DAL attest" ;
  let* () =
    repeat 3 (fun () ->
        let* _attestation, _op_hash =
          inject_dal_attestation_exn
            ~protocol
            ~signer:delegate_wrongly_attestating
            (Slots [slot_index])
            client
            dal_parameters
            (Node.as_rpc_endpoint l1_node)
        in
        let* () = bake_for ~delegates:(`For rest_delegates_pkhs) client in
        let* _ops =
          Node.RPC.call l1_node @@ RPC.get_chain_block_operations ()
        in
        unit)
  in
  (* As soon as the attestations are final, the DAL node should see that they
     are denounceable and publish the denunciation. *)
  Log.info "Denunciations should start to arrive." ;
  let* () =
    repeat 3 (fun () ->
        let* () = bake_for client in
        let* _mempool =
          Client.RPC.call client @@ RPC.get_chain_mempool_pending_operations ()
        in
        let* ops =
          Node.RPC.call l1_node
          @@ RPC.get_chain_block_operations_validation_pass
               ~block:"head"
               ~validation_pass:2
               ()
        in
        Check.(List.length (JSON.as_list ops) = 1)
          ~__LOC__
          Check.(int)
          ~error_msg:"Expected exactly one anonymous op. Got: %L" ;
        unit)
  in
  unit

let register ~__FILE__ ~protocols =
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    "slot is protocol attested even if attestations are aggregated"
    test_aggregation_required_to_pass_quorum
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~traps_fraction:Q.one
    "inject accusation"
    test_inject_accusation
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~traps_fraction:Q.one
    "inject accusation with dynamic multi-lag attestations"
    ~tags:["traps"; "denunciation"; "multi_lag"]
    test_inject_accusation_dynamic_multi_lag
    (List.filter (fun p -> Protocol.number p >= 025) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~traps_fraction:Q.one
    ~operator_profiles:[0]
    "inject accusation of aggregated attestation"
    (test_inject_accusation_aggregated_attestation 1)
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~traps_fraction:Q.one
    ~operator_profiles:[0]
    "inject several accusations for the same aggregated attestation"
    (test_inject_accusation_aggregated_attestation 2)
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~traps_fraction:Q.one
    "inject a duplicated denunciation at different steps"
    test_duplicate_denunciations
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~traps_fraction:Q.one
    "inject a denunciation at the next cycle"
    test_denunciation_next_cycle
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  (* Tests with layer1 and dal nodes *)
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~number_of_slots:1
    ~operator_profiles:[0]
    ~traps_fraction:Q.one
    "faulty DAL node entrapment"
    test_e2e_trap_faulty_dal_node
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["traps"]
    ~operator_profiles:[0]
    ~traps_fraction:Q.one
    ~all_bakers_attest_activation_threshold:Q.(Z.one /// Z.of_int 2)
    "Trap is denounced when all bakers attest"
    test_denunciation_when_all_bakers_attest
    protocols
