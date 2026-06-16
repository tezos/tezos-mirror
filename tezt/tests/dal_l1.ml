(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL tests that run without a DAL node (slot management, attestation
   operations, committee checks). These tests exercise DAL functionality
   at the protocol/L1 level only. *)

open Dal_helpers
module Dal = Dal_common

let test_one_committee_per_level _protocol _parameters _cryptobox node _client
    _bootstrap_key =
  let* current_level =
    Node.RPC.(call node @@ get_chain_block_helper_current_level ())
  in
  (* The test assumes we are at a level when an epoch starts. And
     that is indeed the case. *)
  assert (current_level.cycle_position = 0) ;
  let* current_committee =
    Dal.Committee.at_level node ~level:current_level.level ()
  in
  let* next_committee =
    Dal.Committee.at_level node ~level:(current_level.level + 1) ()
  in
  Check.((current_committee <> next_committee) Dal.Committee.typ)
    ~error_msg:"Unexpected equal DAL committees at subsequent levels: %L and %R" ;
  unit

(* We check that publishing a slot header with a proof for a different
   slot leads to a proof-checking error. *)
let publish_dummy_slot_with_wrong_proof_for_same_content ~source ?fee ~index
    cryptobox =
  let commitment, _proof = Dal.(Commitment.dummy_commitment cryptobox "a") in
  let _commitment, proof = Dal.(Commitment.dummy_commitment cryptobox "b") in
  Helpers.publish_commitment ~source ?fee ~index ~commitment ~proof

(* We check that publishing a slot header with a proof for the "same"
   slot contents but represented using a different [slot_size] leads
   to a proof-checking error. *)
let publish_dummy_slot_with_wrong_proof_for_different_slot_size ~source ?fee
    ~index parameters cryptobox ?counter ?force ?error client =
  let cryptobox_params =
    {
      parameters.Dal.Parameters.cryptobox with
      slot_size = 2 * parameters.cryptobox.slot_size;
    }
  in
  let* cryptobox' = Helpers.make_cryptobox cryptobox_params in
  let msg = "a" in
  let commitment, _proof = Dal.(Commitment.dummy_commitment cryptobox msg) in
  let _commitment, proof = Dal.(Commitment.dummy_commitment cryptobox' msg) in
  Helpers.publish_commitment
    ~source
    ?fee
    ~index
    ~commitment
    ~proof
    ?counter
    ?force
    ?error
    client

type status = Applied | Failed of {error_id : string}

let pp fmt = function
  | Applied -> Format.fprintf fmt "applied"
  | Failed {error_id} -> Format.fprintf fmt "failed: %s" error_id

let status_typ = Check.equalable pp ( = )

let check_manager_operation_status result expected_status oph =
  let manager_operations = JSON.(result |=> 3 |> as_list) in
  let op =
    try
      List.find
        (fun op -> JSON.(op |-> "hash" |> as_string) = oph)
        manager_operations
    with Not_found ->
      Test.fail
        "Test expecting operation %s to be included into the last block."
        oph
  in
  let op_result =
    JSON.(op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result")
  in
  let status_kind = JSON.(op_result |-> "status" |> as_string) in
  let status =
    match status_kind with
    | "applied" -> Applied
    | "failed" ->
        let error_id =
          JSON.(op_result |-> "errors" |=> 0 |-> "id" |> as_string)
        in
        Failed {error_id}
    | s -> Test.fail "Unexpected status: %s" s
  in
  let prefix_msg = sf "Unexpected operation result for %s." oph in
  Check.(expected_status = status)
    status_typ
    ~error_msg:(prefix_msg ^ " Expected: %L. Got: %R.")

let check_dal_raw_context node =
  let* dal_raw_json =
    Node.RPC.(call node @@ get_chain_block_context_raw_json ~path:["dal"] ())
  in
  if JSON.is_null dal_raw_json then
    Test.fail "Expected the context to contain information under /dal key."
  else
    let json_to_string j =
      JSON.unannotate j |> Ezjsonm.wrap |> Ezjsonm.to_string
    in
    let* confirmed_slots_opt =
      Node.RPC.(call node @@ get_chain_block_context_dal_commitments_history ())
    in
    if JSON.is_null confirmed_slots_opt then
      Test.fail
        "confirmed_slots_history RPC is not expected to return None if DAL is \
         enabled" ;
    let confirmed_slots = json_to_string confirmed_slots_opt in
    let confirmed_slots_from_ctxt =
      json_to_string @@ JSON.(dal_raw_json |-> "slot_headers_history")
    in
    if not (String.equal confirmed_slots confirmed_slots_from_ctxt) then
      Test.fail "Confirmed slots history mismatch." ;
    unit

let test_slot_management_logic protocol parameters cryptobox node client
    _bootstrap_key =
  let*! () = Client.reveal ~src:"bootstrap6" client in
  let* () = bake_for client in
  Log.info "Inject some valid slot headers" ;
  let* (`OpHash oph1) =
    publish_dummy_slot
      ~source:Constant.bootstrap1
      ~fee:10_000
      ~index:0
      ~message:"a"
      cryptobox
      client
  in
  let* (`OpHash oph2) =
    publish_dummy_slot
      ~source:Constant.bootstrap2
      ~fee:15_000
      ~index:1
      ~message:"b"
      cryptobox
      client
  in
  let* (`OpHash oph3) =
    publish_dummy_slot
      ~source:Constant.bootstrap3
      ~fee:20_000
      ~index:0
      ~message:"c"
      cryptobox
      client
  in
  let* (`OpHash oph4) =
    publish_dummy_slot
      ~source:Constant.bootstrap4
      ~fee:12_000
      ~index:1
      ~message:"d"
      cryptobox
      client
  in
  let* (`OpHash oph5) =
    publish_dummy_slot_with_wrong_proof_for_same_content
      ~source:Constant.bootstrap5
      ~fee:30_000
      ~index:2
      cryptobox
      client
  in
  (* Check another operation now because we are lacking of bootstrap accounts. *)
  let* bootstrap6 = Client.show_address ~alias:"bootstrap6" client in
  let* (`OpHash oph6) =
    publish_dummy_slot_with_wrong_proof_for_different_slot_size
      ~source:bootstrap6
      ~fee:30_000
      ~index:2
      parameters
      cryptobox
      client
  in
  let* mempool = Mempool.get_mempool client in
  let expected_mempool =
    Mempool.{empty with validated = [oph1; oph2; oph3; oph4; oph5; oph6]}
  in
  Check.(
    (mempool = expected_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected all the operations to be applied. Got %L") ;
  let* () = bake_for client in
  let* published_level = Node.get_level node in
  let* bytes = Node.RPC.call node @@ RPC.get_chain_block_context_raw_bytes () in
  if JSON.(bytes |-> "dal" |> is_null) then
    Test.fail "Expected the context to contain some information about the DAL" ;
  let* operations_result =
    Node.RPC.call node @@ RPC.get_chain_block_operations ()
  in
  let fees_error =
    Failed
      {
        error_id =
          sf
            "proto.%s.dal_publish_commitment_duplicate"
            (Protocol.encoding_prefix protocol);
      }
  in
  let proof_error =
    Failed
      {
        error_id =
          sf
            "proto.%s.dal_publish_commitment_invalid_proof"
            (Protocol.encoding_prefix protocol);
      }
  in
  (* The baker sorts operations fee wise. Consequently order of
     application for the operations will be: oph3 > oph2 > oph4 > oph1

     For slot 0, oph3 is applied first.

     Flor slot1, oph2 is applied first. *)
  check_manager_operation_status operations_result fees_error oph1 ;
  check_manager_operation_status operations_result fees_error oph4 ;
  check_manager_operation_status operations_result proof_error oph5 ;
  check_manager_operation_status operations_result proof_error oph6 ;
  check_manager_operation_status operations_result Applied oph3 ;
  check_manager_operation_status operations_result Applied oph2 ;
  let attestation_lag = parameters.attestation_lag in
  let* () = repeat (attestation_lag - 1) (fun () -> bake_for client) in
  let node_endpoint = Node.as_rpc_endpoint node in

  let* _ =
    inject_dal_attestations
      ~protocol
      ~signers:[Constant.bootstrap1; Constant.bootstrap2]
      (Slots [1; 0])
      client
      parameters
      node_endpoint
  in
  let* _ =
    inject_dal_attestations
      ~protocol
      ~signers:[Constant.bootstrap3; Constant.bootstrap4; Constant.bootstrap5]
      (Slots [1])
      client
      parameters
      node_endpoint
  in
  let* baker =
    let* level = Node.get_level node in
    baker_for_round_zero node ~level:(level + 1)
  in
  let* () = bake_for ~delegates:(`For [baker]) client in
  let* current_level = Node.get_level node in
  let min_lag = List.hd parameters.attestation_lags in
  let max_lag = parameters.attestation_lag in
  let attested_levels =
    published_level + min_lag --> min (published_level + max_lag) current_level
  in
  let* all_slot_availabilities =
    Dal.collect_slot_availabilities node ~attested_levels
  in

  let* slot_0_not_attested =
    Dal.is_slot_attested
      ~endpoint:(Node.as_rpc_endpoint node)
      ~published_level
      ~slot_index:0
      ~to_attested_levels:
        (Dal.to_attested_levels ~protocol ~dal_parameters:parameters)
      all_slot_availabilities
  in
  Check.is_false
    slot_0_not_attested
    ~error_msg:"Expected slot 0 to not be attested" ;

  let* slot_1_attested =
    Dal.is_slot_attested
      ~endpoint:(Node.as_rpc_endpoint node)
      ~published_level
      ~slot_index:1
      ~to_attested_levels:
        (Dal.to_attested_levels ~protocol ~dal_parameters:parameters)
      all_slot_availabilities
  in
  Check.is_true slot_1_attested ~error_msg:"Expected slot 1 to be attested" ;
  check_dal_raw_context node

(** This test tests various situations related to DAL slots attestation.
    See the steps inside the test.
*)
let test_slots_attestation_operation_behavior protocol parameters _cryptobox
    node client _bootstrap_key =
  (* Some helpers *)
  let attestation_lag = parameters.Dal.Parameters.attestation_lag in
  assert (attestation_lag > 1) ;
  let attest ?payload_level ?(signer = Constant.bootstrap2) ~level () =
    let* _op, op_hash =
      inject_dal_attestation_exn
        ~protocol
        ?payload_level
        ~force:true
        ~level
        ~signer
        (Slots [0])
        client
        parameters
        (Node.as_rpc_endpoint node)
    in
    return op_hash
  in
  let mempool_is ~__LOC__ expected_mempool =
    let* mempool = Mempool.get_mempool client in
    Check.(
      (mempool = expected_mempool)
        Mempool.classified_typ
        ~error_msg:(__LOC__ ^ " : Bad mempool !!!. Got %L")) ;
    unit
  in
  (* Just bake some blocks before starting attesting. *)
  let* () = bake_for ~count:4 client in

  (* No header published, we just play with attestations with various levels;
     - Initially, only [h3] is applied, [h1; h2] are outdated, and [h4] is
       branch_delayed. After baking a block, [h3] is included in a block and
       [h4] becomes applied.
     - No slot is confirmed as no slot header is published.
  *)
  let* now = Node.wait_for_level node 5 in
  let* (`OpHash h1) = attest ~level:2 () in
  let outdated = [h1] in
  Log.info "expected mempool: outdated: h1 = %s" h1 ;
  let* () = mempool_is ~__LOC__ Mempool.{empty with outdated} in
  let* (`OpHash h2) = attest ~level:(now - 1) () in
  (* level [now-1] is allowed in the mempool for attestations *)
  let* (`OpHash h2') = attest ~level:(now - 2) () in
  let outdated = [h1; h2'] in
  Log.info "expected mempool: outdated: h1, h2' = %s, validated: h2 = %s" h2' h2 ;
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated = [h2]}
  in
  let* (`OpHash h3) = attest ~level:now () in
  Log.info "expected mempool: outdated: h1, h2', validated: h2, h3 = %s" h3 ;
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated = [h2; h3]}
  in
  (* Level [now+1] is allowed in the mempool for attestations, so we inject for
     level [now+2]. We also take care that the attester is not the same one as
     the baker (who will also inject an attestation, and the two ops will
     conflict). *)
  let* baker1 = baker_for_round_zero node ~level:(now + 1) in
  let* baker2 = baker_for_round_zero node ~level:(now + 2) in
  let signer1 = different_delegate baker1 in
  let signer2 = different_delegate baker1 in
  let* (`OpHash h4) =
    attest ~payload_level:now ~level:(now + 1) ~signer:signer1 ()
  in
  let* (`OpHash h4') =
    attest ~payload_level:now ~level:(now + 2) ~signer:signer2 ()
  in
  Log.info
    "expected mempool: outdated: h1, h2', validated: h2, h3, h4 = %s, \
     branch_delayed: h4' = %s"
    h4
    h4' ;
  let* () =
    mempool_is
      ~__LOC__
      Mempool.
        {empty with outdated; validated = [h2; h3; h4]; branch_delayed = [h4']}
  in
  let* () = bake_for ~delegates:(`For [baker1]) client in
  let outdated = [h1; h2; h2'] in
  (* [h4] and [h4'] cannot be included because their payload hash is wrong. *)
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated = [h4; h4']}
  in
  let* () = bake_for ~delegates:(`For [baker2]) client in
  let* _json =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
  in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated = [h4; h4']}
  in
  let* metadata = Node.RPC.(call node @@ get_chain_block_metadata ()) in
  match metadata.dal_attestation with
  | None -> unit
  | Some dal_attestation_bitset ->
      let* attestation_array =
        Dal.Slot_availability.decode
          protocol
          (Node.as_rpc_endpoint node)
          parameters
          dal_attestation_bitset
      in
      (* Check that no slots are attested at any lag *)
      let some_slot_attested =
        Array.exists
          (fun lag_attestations ->
            Array.exists (fun attested -> attested) lag_attestations)
          attestation_array
      in
      Check.is_false
        some_slot_attested
        ~error_msg:"%s: Expected no slots to be attested, but some were"
        ~__LOC__ ;
      unit

let test_all_available_slots _protocol parameters cryptobox node client
    _bootstrap_key =
  let number_of_slots = parameters.Dal.Parameters.number_of_slots in
  (* We ensure there is at least one account per manager operation to
     be included. This is because of the 1M restrction. Another way
     could use batched operations, but the current DAL helpers are
     difficult to use for batched operations. *)
  let* accounts =
    Seq.ints 0 |> Seq.take number_of_slots |> List.of_seq
    |> Lwt_list.map_p (fun index ->
           Client.show_address
             ~alias:("bootstrap" ^ string_of_int (1 + index))
             client)
  in
  let* () =
    Lwt_list.iter_p
      (fun source ->
        let*! () = Client.reveal ~src:source.Account.alias client in
        unit)
      (List.filteri
         (fun i _ -> i >= Array.length Account.Bootstrap.keys)
         accounts)
  in
  let* () = bake_for client in
  let* () =
    Lwt_list.iteri_p
      (fun index source ->
        let* (`OpHash _oph1) =
          publish_dummy_slot
            ~source
            ~fee:13_000
            ~index
            ~message:"a"
            cryptobox
            client
        in
        unit)
      accounts
  in
  let* () = bake_for client in
  let* result =
    Node.RPC.(call ~rpc_hooks node @@ get_chain_block_metadata_raw ())
  in
  JSON.encode result |> hooks.on_log ;
  let* operations = Node.RPC.(call node @@ get_chain_block_operations ()) in
  let () =
    (* Check validity of operations *)
    let manager_operations = JSON.(operations |=> 3 |> as_list) in
    Check.(List.length manager_operations = number_of_slots)
      ~__LOC__
      ~error_msg:"Expected %R manager operations.Got %L"
      Check.int ;
    List.iter
      (fun operation ->
        let status =
          JSON.(
            operation |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
            |-> "status" |> as_string)
        in
        Check.(status = "applied")
          ~__LOC__
          ~error_msg:
            "Expected all slots to be included. At least one operation failed \
             with status: %L"
          Check.string)
      manager_operations
  in
  unit

(* Tests that DAL attestation payloads are only attached if the attestation is
   from a DAL-committee member. This test creates a new account and registers it
   as a baker, and bakes blocks until it reaches a level where the new account
   is in the TB committee but not in the DAL committee).*)
let test_slots_attestation_operation_dal_committee_membership_check protocol
    parameters _cryptobox node client dal_node =
  (* The attestation from the bootstrap account should succeed as the bootstrap
     node has sufficient stake to be in the DAL committee. *)
  let client = Client.with_dal_node client ~dal_node in
  let number_of_shards = parameters.Dal.Parameters.cryptobox.number_of_shards in
  Log.info "number_of_shards = %d" number_of_shards ;
  let* () = bake_for client in
  let* level = Client.level client in
  let* _op, `OpHash _oph =
    inject_dal_attestation_exn
      ~protocol
      ~level
      ~signer:Constant.bootstrap1
      (Slots [])
      client
      parameters
      (Node.as_rpc_endpoint node)
  in
  (* Set up a new account that holds the right amount of tez and make sure it
     can be an attester. *)
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  (* With [consensus_committee_size = 1024] slots in total, the new baker should
     get roughly 1024 / 64 = 16 TB slots on average. So the probability that it
     is on TB committee is high. With [number_of_shards = 256] (which is the
     current default), the new baker should be assigned roughly 256 / 64 = 4
     shards on average. We should encounter relatively quickly a level where it
     is assigned to no shard. *)
  let stake =
    Tez.of_mutez_int64 (Int64.div Protocol.default_bootstrap_balance 64L)
  in
  let amount = Tez.(stake + of_int 10) in
  let* new_account = Client.gen_and_show_keys client in
  let* () =
    Client.transfer
      ~giver:Constant.bootstrap1.alias
      ~receiver:new_account.alias
      ~amount
      ~burn_cap:Tez.one
      client
  in
  let* () = bake_for client in
  let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client in
  let* () = bake_for client in
  let* () = Client.register_key new_account.alias client in
  let* () = bake_for client in
  let* () = Client.stake ~staker:new_account.alias Tez.(amount /! 2L) client in
  let num_cycles = 2 + consensus_rights_delay in
  Log.info
    "Bake for %d cycles for %s to be a baker"
    num_cycles
    new_account.alias ;
  let* () = bake_for ~count:(num_cycles * blocks_per_cycle) client in
  (* We iterate until we find a level for which the new account has no assigned
     shard. *)
  let rec iter () =
    let* level = Client.level client in
    let* committee = Dal.Committee.at_level node ~level () in
    if
      List.exists
        (fun member ->
          String.equal member.Dal.Committee.attester new_account.public_key_hash)
        committee
    then (
      Log.info
        "The new account is in the DAL committee. Bake another block to change \
         the DAL committee" ;
      let* () = bake_for client in
      iter ())
    else (
      Log.info "The new account is NOT in the DAL committee" ;
      Log.info "We check that the new account is in the Tenderbake committee" ;
      let* () =
        check_in_TB_committee
          ~__LOC__
          ~protocol
          node
          new_account.public_key_hash
          ~level
      in
      let attested_slots =
        (* Starting with 025, an empty DAL payload is allowed. *)
        if Protocol.number protocol >= 025 then Slots [0] else Slots []
      in
      let* _op, `OpHash _oph =
        inject_dal_attestation_exn
          ~protocol
          ~error:
            (Operation.dal_data_availibility_attester_not_in_committee protocol)
          ~level
          ~signer:new_account
          attested_slots
          client
          parameters
          (Node.as_rpc_endpoint node)
      in
      Log.info "Bake a block and check the DAL attestation of the new baker." ;
      let* () = bake_for client in
      let* json =
        Node.RPC.call node
        @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
      in
      let ops = JSON.as_list json in
      Check.(
        (List.length ops = 1 + Array.length Account.Bootstrap.keys)
          int
          ~error_msg:"Expected %R operations, found %L") ;
      let ops_contents =
        List.map (fun op -> JSON.(op |-> "contents" |> as_list) |> List.hd) ops
      in
      let content =
        List.find_opt
          (fun contents ->
            let delegate =
              JSON.(contents |-> "metadata" |-> "delegate" |> as_string)
            in
            delegate = new_account.public_key_hash)
          ops_contents
      in
      match content with
      | None ->
          Test.fail "Did not find an attestation operation for the new delegate"
      | Some contents ->
          let kind = JSON.(contents |-> "kind" |> as_string) in
          let expected_kind =
            if Protocol.number protocol <= 024 then "attestation"
            else "attestation_with_dal"
          in
          Check.(
            (kind = expected_kind) string ~error_msg:"Expected %R, found %L") ;
          unit)
  in
  iter ()

let register ~__FILE__ ~protocols =
  scenario_with_layer1_node
    ~__FILE__
    ~additional_bootstrap_accounts:1
    ~slot_size:190_416
    "dal basic logic"
    test_slot_management_logic
    protocols ;
  scenario_with_layer1_node
    ~__FILE__
    "slots attestation operation behavior"
    test_slots_attestation_operation_behavior
    protocols ;
  (* We want to test that the number of slots following mainnet
     parameters can be included into one block. We hard-code the
     mainnet value. It could be extended to higher values if
     desired. *)
  scenario_with_layer1_node
    ~__FILE__
    ~regression:true
    ~number_of_slots:32
    ~additional_bootstrap_accounts:(32 - Array.length Account.Bootstrap.keys)
    "Use all available slots"
    test_all_available_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    "slots attestation operation dal committee membership check"
    test_slots_attestation_operation_dal_committee_membership_check
    (* We need to set the prevalidator's event level to [`Debug]
       in order to capture the errors thrown in the validation phase. *)
    ~event_sections_levels:[("prevalidator", `Debug)]
    ~consensus_committee_size:1024
    protocols ;
  scenario_with_layer1_node
    ~__FILE__
    "one_committee_per_level"
    test_one_committee_per_level
    protocols
