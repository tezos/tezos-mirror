(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL attestation, rewards, and participation tests. Covers new attester
   onboarding, reward distribution, attestation encoding, low-stake attester
   behavior, reorg scenarios, and mockup-based attestable slot verification. *)

open Dal_helpers
module Dal = Dal_common

(* We have a bootstrap node, a producer node and an attester node for a new
   attester. We check that as soon as the attester is in the DAL committee it
   attests. *)
let test_new_attester_attests protocol dal_parameters _cryptobox node client
    dal_bootstrap =
  let peer_id dal_node = Dal_node.read_identity dal_node in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let slot_index = 0 in
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let peers = [Dal_node.listen_addr dal_bootstrap] in

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () =
    Dal_node.init_config ~operator_profiles:[slot_index] ~peers producer
  in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator slot_index])
  in
  Log.info "Slot producer DAL node is running" ;

  (* Set up a new account that holds a big amount of tez and make sure it can be
     an attester. *)
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* balance =
    Client.get_balance_for ~account:Constant.bootstrap1.alias client
  in
  let amount = Tez.(balance - one) in
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

  let attester = Dal_node.create ~name:"attester" ~node () in
  let* () =
    Dal_node.init_config
      ~attester_profiles:[new_account.public_key_hash]
      ~peers
      attester
  in
  let* () = Dal_node.run ~event_level:`Debug attester in
  let client = Client.with_dal_node client ~dal_node:attester in

  let num_cycles = 1 + consensus_rights_delay in
  let* level = Client.level client in

  let lag = dal_parameters.attestation_lag in
  (* We need to publish at the level [n - lag], where [n] is the first level
     where we should see the attestation of the new attester. Level [n] is
     [first_level_in_committee + 1] (because the attestation is included in the
     following block). *)
  let first_level_in_committee =
    (* plus 1 because the first cycle starts at level 1 *)
    (num_cycles * blocks_per_cycle) + 1
  in
  let published_level = first_level_in_committee + 1 - lag in
  Log.info
    "first_level_in_committee = %d; published_level = %d"
    first_level_in_committee
    published_level ;

  let check_graft_promises =
    let* id_attester = peer_id attester in
    let* id_producer = peer_id producer in
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index
         (attester, id_attester)
         (producer, id_producer)
         new_account.public_key_hash
  in

  Log.info "Bake blocks up to level %d" (published_level - 1) ;
  let* () = bake_for ~count:(published_level - 1 - level) client in

  let wait_for_shards_promises =
    let* assigned_shard_indexes =
      Dal_RPC.(
        call attester
        @@ get_assigned_shard_indices
             ~level:first_level_in_committee
             ~pkh:new_account.public_key_hash)
    in
    wait_for_shards_promises
      ~dal_node:attester
      ~storage_profile:`Cache_only
      ~shards:assigned_shard_indexes
      ~published_level
      ~slot_index
  in
  let* level = Client.level client in
  Log.info
    "Current level is %d, publish a slot for level %d"
    level
    published_level ;
  let* _ =
    Helpers.publish_and_store_slot
      client
      producer
      Constant.bootstrap2
      ~index:slot_index
    @@ Helpers.make_slot ~slot_size "SLOTDATA"
  in
  let* () = bake_for client in
  (* At this point the attester node fetches the DAL committee for level
     [first_level_in_committee] and changes topics. *)
  let* manager_ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:3 ()
  in
  Check.(
    (JSON.as_list manager_ops |> List.length <> 0)
      int
      ~error_msg:
        "Expected the commitment to be published, but no manager operation was \
         included.") ;

  Log.info "Waiting for grafting of the attester - producer connection" ;
  (* This is important because the attester and the producer should connect and
     join the relevant mesh, before the producer attempts to send its
     shards. Normally there the time between two levels to do that, but since
     we're baking in the past, this time is very short, so without this explicit
     wait the test would be flaky. *)
  let* () = check_graft_promises in

  Log.info "Bake another block, so that the producer sends the shards" ;
  let* () = bake_for client in
  let () = Log.info "Waiting for the attester to receive its shards" in
  let* () = wait_for_shards_promises in

  Log.info "Bake blocks up to level %d" (first_level_in_committee - 1) ;
  let* () = bake_for ~count:(lag - 3) client in
  let* level = Client.level client in
  Log.info "Current level is %d" level ;
  Check.(
    (level = first_level_in_committee - 1)
      int
      ~error_msg:"Expected current level to be %R, got %L") ;
  Log.info
    "At level %d the new attester is not yet in the committee, at the next \
     level it will be"
    level ;
  let* () =
    check_in_TB_committee
      ~__LOC__
      ~protocol
      node
      new_account.public_key_hash
      ~inside:false
      ~level
  in
  let* () =
    Dal.Committee.check_is_in
      ~__LOC__
      node
      new_account.public_key_hash
      ~inside:false
      ~level
  in
  Log.info "Bake one more block for %s to be in the committee" new_account.alias ;
  let* () = bake_for client in
  let* () =
    check_in_TB_committee
      ~__LOC__
      ~protocol
      node
      new_account.public_key_hash
      ~level:(level + 1)
  in
  let* () =
    Dal.Committee.check_is_in
      ~__LOC__
      node
      new_account.public_key_hash
      ~level:(level + 1)
  in

  Log.info "Bake a block with all accounts, including the new account" ;
  let* () =
    let bootstrap_accounts =
      Array.to_list Account.Bootstrap.keys
      |> List.map (fun a -> a.Account.public_key_hash)
    in
    bake_for
      ~delegates:(`For (new_account.public_key_hash :: bootstrap_accounts))
      client
  in
  let* json =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
  in
  let* attested_level = Client.level client in
  let dal_attestation_opt =
    List.find_map
      (fun json ->
        let contents = JSON.(json |-> "contents" |> as_list) |> List.hd in
        let delegate =
          JSON.(contents |-> "metadata" |-> "delegate" |> as_string)
        in
        let kind = JSON.(contents |-> "kind" |> as_string) in
        if
          delegate = new_account.public_key_hash
          && kind = "attestation_with_dal"
        then Some JSON.(contents |-> "dal_attestation" |> as_string)
        else None)
      (JSON.as_list json)
  in
  let* new_attester_attested_slot =
    match dal_attestation_opt with
    | None -> return false
    | Some dal_attestation ->
        Dal.is_slot_attested_in_bitset
          ~endpoint:(Node.as_rpc_endpoint node)
          ~protocol
          ~dal_parameters
          ~attested_level
          ~published_level
          ~slot_index
          ~dal_attestation
  in
  Check.is_true
    ~__LOC__
    new_attester_attested_slot
    ~error_msg:
      (Format.sprintf
         "Expected new attester to attest slot %d from published_level %d"
         slot_index
         published_level) ;
  unit

let pair_up ~error_msg =
  let rec pair_up = function
    | [] -> []
    | [_] -> Test.fail ~__LOC__ error_msg
    | x :: y :: rest -> (x, y) :: pair_up rest
  in
  pair_up

let extract_dal_balance_updates balance_updates =
  List.filter_map
    (fun (json1, json2) ->
      let change1 = JSON.(json1 |-> "change" |> as_int) in
      let change2 = JSON.(json2 |-> "change" |> as_int) in
      Check.(
        (-change1 = change2)
          ~__LOC__
          int
          ~error_msg:"Expected 'change' to match; got %L and %R") ;
      let kind = JSON.(json1 |-> "kind" |> as_string) in
      if
        (not (String.equal kind "minted"))
        && not (String.equal kind "accumulator")
      then
        Test.fail
          ~__LOC__
          "Expected a 'minted' or 'accumulator' kind, got '%s'"
          kind ;
      let category1 = JSON.(json1 |-> "category" |> as_string) in
      if String.equal category1 "DAL attesting rewards" then
        let kind2 = JSON.(json2 |-> "kind" |> as_string) in
        match kind2 with
        | "burned" ->
            let category2 = JSON.(json2 |-> "category" |> as_string) in
            Check.(
              (category2 = "lost DAL attesting rewards")
                ~__LOC__
                string
                ~error_msg:
                  "Expected a 'lost DAL attesting rewards' category, got %L") ;
            let delegate = JSON.(json2 |-> "delegate" |> as_string) in
            Some (`Lost (delegate, change2, json2))
        | "freezer" ->
            let delegate =
              JSON.(json2 |-> "staker" |-> "baker_own_stake" |> as_string)
            in
            Some (`Got (delegate, change2, json2))
        | "contract" ->
            let delegate = JSON.(json2 |-> "contract" |> as_string) in
            Some (`Got (delegate, change2, json2))
        | _ -> Test.fail ~__LOC__ "Unexpected balance update kind %s" kind2
      else None)
    balance_updates

(* This function checks the fields of the [dal_participation] RPC result. It is
   supposed to be used only by the test scenario below, as it assumes
   [expected_attested_shards] is either 0 or [expected_attestable_shards],
   [denounced] is [false], and there are only two kinds of transfers, both to
   the delegate itself, as there are no co-stakers in the scenario below. *)
let check_participation_and_rewards participation ~expected_assigned_shards
    ~expected_attestable_slots ~attesting_reward_per_shard dal_balance_updates
    delegate ~sufficient_participation =
  Check.is_false
    participation.RPC.denounced
    ~__LOC__
    ~error_msg:"The delegate was unexpectedly denounced" ;
  Check.(
    (participation.expected_assigned_shards_per_slot = expected_assigned_shards)
      ~__LOC__
      int
      ~error_msg:"Unexpected number of assigned shards. Expected %R, got %L") ;
  let expected_attested_slots =
    if sufficient_participation then expected_attestable_slots else 0
  in
  Check.(
    (participation.delegate_attested_dal_slots = expected_attested_slots)
      ~__LOC__
      int
      ~error_msg:"Expected that the delegate has attested %R slots, got %L") ;
  Check.(
    (participation.delegate_attestable_dal_slots = expected_attestable_slots)
      ~__LOC__
      int
      ~error_msg:"Expected that there are %R attestable slots, got %L") ;
  Check.(
    (participation.sufficient_dal_participation = sufficient_participation)
      ~__LOC__
      bool
      ~error_msg:"Expected sufficient_dal_participation to be %R, got %L") ;
  let dal_rewards = expected_assigned_shards * attesting_reward_per_shard in
  Check.(
    (Tez.to_mutez participation.expected_dal_rewards = dal_rewards)
      ~__LOC__
      int
      ~error_msg:
        ("Unexpected rewards for delegate " ^ delegate ^ ": expected %L, got %R")) ;
  let get_delegate = function
    | `Got (delegate, _amount, _json) | `Lost (delegate, _amount, _json) ->
        delegate
  in
  let dal_rewards =
    List.filter_map
      (fun item ->
        let item_delegate = get_delegate item in
        if String.equal item_delegate delegate then Some item else None)
      dal_balance_updates
  in
  let get_json_list =
    List.map (function `Got (_, _, json) | `Lost (_, _, json) ->
        JSON.encode json)
  in
  let rewards =
    if sufficient_participation then
      match dal_rewards with
      | [`Got (_, amount1, _); `Got (_, amount2, _)] ->
          (* one corresponds to the liquid rewards and one to the frozen ones (but
             we do not care here) *)
          amount1 + amount2
      | _ ->
          Test.fail
            ~__LOC__
            "Unexpected balance updates for the delegate %s: %a"
            delegate
            Format.(pp_print_list pp_print_string)
            (get_json_list dal_rewards)
    else
      match dal_rewards with
      | [`Lost (_, amount, _)] -> amount
      | _ ->
          Test.fail
            ~__LOC__
            "Unexpected balance updates for the delegate %s: %a"
            delegate
            Format.(pp_print_list pp_print_string)
            (get_json_list dal_rewards)
  in
  Check.(
    (Tez.to_mutez participation.expected_dal_rewards = rewards)
      ~__LOC__
      int
      ~error_msg:
        ("Unexpected rewards for delegate " ^ delegate ^ ": expected %L, got %R"))

let check_participation_and_rewards node ~expected_assigned_shards
    ~expected_attestable_slots =
  let* metadata = Node.RPC.call node @@ RPC.get_chain_block_metadata_raw () in
  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  let balance_updates =
    pair_up
      balance_updates
      ~error_msg:"The list of balance updates has an odd number of elements"
  in
  let dal_balance_updates =
    extract_dal_balance_updates balance_updates
    |>
    (* sort them for proper regression output *)
    List.sort (fun e1 e2 ->
        match (e1, e2) with
        | `Got (d1, a1, _), `Got (d2, a2, _)
        | `Lost (d1, a1, _), `Lost (d2, a2, _) ->
            let c = String.compare d1 d2 in
            if c = 0 then a1 - a2 else c
        | `Got _, `Lost _ -> 1
        | `Lost _, `Got _ -> -1)
  in
  List.iter
    (function
      | `Got (_, _, json) | `Lost (_, _, json) ->
          Regression.capture @@ JSON.encode json)
    dal_balance_updates ;
  let* attesting_reward_per_shard =
    let* json =
      Node.RPC.call ~rpc_hooks node
      @@ RPC.get_chain_block_context_issuance_expected_issuance ()
    in
    return @@ JSON.(json |=> 0 |-> "dal_attesting_reward_per_shard" |> as_int)
  in
  return @@ fun delegate ~sufficient_participation ->
  (* Note that at the last level in the cycle we lose information about the
     total_dal_attested_slots *)
  let* participation =
    Node.RPC.call ~rpc_hooks node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         delegate
  in
  return
  @@ check_participation_and_rewards
       participation
       ~expected_assigned_shards
       ~expected_attestable_slots
       ~attesting_reward_per_shard
       dal_balance_updates
       delegate
       ~sufficient_participation

(* We have one DAL attester node. During the second cycle, one of the bakers
   does not DAL attest sufficiently. We check that the attesters receive or not
   the DAL rewards depending on their participation. *)
let test_attesters_receive_dal_rewards _protocol dal_parameters _cryptobox node
    client dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let attestation_lag = dal_parameters.Dal.Parameters.attestation_lag in
  (* This constraint makes the test simpler; it could be lifted. *)
  assert (attestation_lag <= blocks_per_cycle) ;

  let expected_assigned_shards =
    let number_of_shards =
      dal_parameters.Dal.Parameters.cryptobox.number_of_shards
    in
    let num_delegates = Array.length Account.Bootstrap.keys in
    number_of_shards * blocks_per_cycle / num_delegates
  in
  let all_delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun key -> key.Account.public_key_hash)
  in
  let delegate_without_dal = List.hd all_delegates in
  let rest_delegates = List.tl all_delegates in
  let delegate_with_dal = List.hd rest_delegates in
  Log.info
    "delegate not running DAL: %s, delegate running DAL: %s"
    delegate_without_dal
    delegate_with_dal ;
  let* level =
    let* first_level = Node.get_level node in
    let block = string_of_int first_level in
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ~block ()
  in
  let blocks_to_bake = blocks_per_cycle - 1 - level.cycle_position in
  Log.info "Publish a slot for %d levels, up to the end of cycle" blocks_to_bake ;
  let* () =
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    repeat blocks_to_bake (fun () ->
        let wait_slot ~published_level:_ ~slot_index:_ = unit in
        let* _res =
          publish_store_and_wait_slot
            node
            client
            dal_node
            Constant.bootstrap1
            ~index:0
            ~wait_slot
            ~number_of_extra_blocks_to_bake:0
          @@ Helpers.make_slot ~slot_size "content"
        in
        unit)
  in
  let* level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  Log.info "Current level (and last published level) is %d" level.level ;
  assert (level.cycle_position = blocks_per_cycle - 1) ;

  let* check =
    let expected_attestable_slots = 0 in
    check_participation_and_rewards
      node
      ~expected_assigned_shards
      ~expected_attestable_slots
  in
  Log.info "Check that [delegate_without_dal] sufficiently participated" ;
  let* () = check delegate_without_dal ~sufficient_participation:true in

  Log.info "Bake for one more cycle" ;
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint dal_node |> Endpoint.as_string
  in
  let* () =
    bake_for
      ~delegates:(`For rest_delegates)
      ~count:blocks_per_cycle
      ~dal_node_endpoint
      client
  in

  let* check =
    (* [-1] because the participation is obtained one level before the cycle end;
       and another [-1] because the slot for the first level in the cycle was not
       published. *)
    let expected_attestable_slots =
      min attestation_lag (blocks_per_cycle - 2)
    in
    check_participation_and_rewards
      node
      ~expected_assigned_shards
      ~expected_attestable_slots
  in
  Log.info "Check that [delegate_without_dal] did not sufficiently participate" ;
  let* () = check delegate_without_dal ~sufficient_participation:false in
  Log.info "Check that [delegate_with_dal] sufficiently participated" ;
  let* () = check delegate_with_dal ~sufficient_participation:true in
  unit

(* Publishing and attesting should work.
   A producer DAL node publishes "Hello world!" (produced with key [bootstrap1]) on a slot.
   An attestation, which attests the block is emitted with key [bootstrap2].
   The published commitment is attested.*)
let test_producer_attester (protocol : Protocol.t)
    (dal_parameters : Dal_common.Parameters.t) (_cryptobox : Cryptobox.t)
    (node : Node.t) (client : Client.t) (_dal_node : Dal_node.t) : unit Lwt.t =
  let {log_step} = init_logger () in
  let index = 3 in
  let slot_size = dal_parameters.cryptobox.slot_size in
  log_step "Declaration of a producer" ;
  let producer_node = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] producer_node in
  let* () = Dal_node.run ~wait_ready:true producer_node in
  log_step "Two blocks are baked, because why not" ;
  let* () = bake_for ~count:2 client in
  log_step "The producer crafts a commitment and publish it" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      producer_node
      Constant.bootstrap1
      ~index
      message
  in
  let* lvl_publish = Client.level client in
  let lag = dal_parameters.attestation_lag in
  Log.info "We are at level %d and the lag is %d" lvl_publish lag ;
  (* We bake [lag] blocks, one which will include the publication of the commitment
     and [lag-1] just to wait. *)
  log_step "Let's wait for the attestation lag while baking" ;
  let* () = bake_for ~count:lag client in
  log_step "It is now time to attest" ;
  (* We want the attestation to be included in the block [lag] after the one containing the shards,
     so we have to inject them now. *)
  let* _ =
    inject_dal_attestations
      ~protocol
        (* Since the attestation threshold of the test is set to 1%,
           having only [bootstrap2] signing is sufficient. *)
      ~signers:[Constant.bootstrap2]
      (Slots [index])
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* lvl_attest = Client.level client in
  Check.(
    (lvl_publish + lag = lvl_attest)
      int
      ~error_msg:"Current level of the client is unexpected") ;
  log_step "Bake a block containing this attestation" ;
  let map_alias = List.map (fun x -> x.Account.alias) in
  (* Since the function [bake_for] crafts the attestation for delegates,
     we do not want it to craft a concurrent attestation for [bootstrap2].
     One which attests a DAL slot has already been injected. *)
  let* () =
    bake_for
      ~delegates:
        (`For
           (map_alias Constant.[bootstrap1; bootstrap3; bootstrap4; bootstrap5]))
      client
  in
  log_step "Bake a few more blocks before calling RPC" ;
  let final_block_promise =
    wait_for_layer1_final_block producer_node (lvl_attest + 2)
  in
  let* () = bake_for ~count:3 client in
  let* () = final_block_promise in
  log_step "Call to the RPC of the DAL node to check that the slot is attested." ;
  let* status =
    Dal_RPC.(
      call producer_node
      (* The level the commitment is included in a block is the first one crafted after publication. *)
      @@ get_level_slot_status ~slot_level:(lvl_publish + 1) ~slot_index:index)
  in
  Log.info "Status is %a" Dal_RPC.pp_slot_id_status status ;
  log_step "Final check." ;
  let* () =
    check_slot_status
      ~__LOC__
      producer_node
      ~expected_status:(Dal_RPC.Attested lag)
      ~check_attested_lag:`At_most
      ~slot_level:(lvl_publish + 1)
      ~slot_index:index
  in
  unit

let wait_for_classified oph node =
  let filter json = if JSON.as_string json = oph then Some () else None in
  Node.wait_for node "operation_classified.v0" filter

(* Check if the [attester_did_not_attest] warning is correctly emitted.
   This test is a variation of [test_producer_attester] where an attestation
   not attesting the published DAL slot is injected. *)
let test_attester_did_not_attest (protocol : Protocol.t)
    (dal_parameters : Dal_common.Parameters.t) (_cryptobox : Cryptobox.t)
    (node : Node.t) (client : Client.t) (_dal_node : Dal_node.t) : unit Lwt.t =
  let {log_step} = init_logger () in
  let index = 3 in
  let slot_size = dal_parameters.cryptobox.slot_size in
  log_step "Declaration of a producer" ;
  let producer_node = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] producer_node in
  let* () = Dal_node.run ~wait_ready:true producer_node in
  log_step "Declaration of an attester" ;
  let attester_node = Dal_node.create ~name:"attester" ~node () in
  let* () =
    Dal_node.init_config
      ~peers:[Dal_node.listen_addr producer_node]
      ~attester_profiles:[Constant.bootstrap2.public_key_hash]
      attester_node
  in
  let* () = Dal_node.run ~wait_ready:true attester_node in
  log_step
    "We promise the attester_did_not_attest_slot will be emitted by the \
     [attester node]" ;
  let not_attested_by_bootstrap2_promise =
    Dal_node.wait_for attester_node "dal_attester_did_not_attest.v0" (fun _ ->
        Some ())
  in
  log_step "The producer crafts a commitment and publish it" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      producer_node
      Constant.bootstrap1
      ~index
      message
  in
  let* () = bake_for client in

  let* published_level = Client.level client in
  let lag = dal_parameters.attestation_lag in
  Log.info "We are at level %d and the lag is %d" published_level lag ;
  (* If one wants to see all the node events, they should use [Node.log_events node] *)
  (* We bake [lag] blocks, one which will include the publication of the commitment
     and [lag-1] just to wait. *)
  log_step "Let's wait for the attestation lag while baking" ;
  let* () = bake_for ~count:(lag - 1) client in
  log_step
    "Crafting attestation for [bootstrap3] (with expected DAL attestation)." ;
  let node_endpoint = Node.as_rpc_endpoint node in
  let* op1 =
    craft_dal_attestation_exn
      ~protocol
      ~signer:Constant.bootstrap3
      (Slots [index])
      client
      dal_parameters
      node_endpoint
  in
  let* (`OpHash oph1) = Operation.hash op1 client in
  let op1_promise = wait_for_classified oph1 node in
  log_step "Crafting attestation for [bootstrap2] (with empty DAL attestation)." ;
  let* op2 =
    craft_dal_attestation_exn
      ~protocol
      ~signer:Constant.bootstrap2
      (Slots [])
      client
      dal_parameters
      node_endpoint
  in
  let* (`OpHash oph2) = Operation.hash op2 client in
  let op2_promise = wait_for_classified oph2 node in
  log_step "Injecting the attestations" ;
  let* _ = Operation.inject op1 client in
  let* _ = Operation.inject op2 client in
  log_step "Waiting for the attestations to be classified." ;
  let* () = Lwt.join [op1_promise; op2_promise] in
  let* lvl_attest = Node.get_level node in
  log_step
    "Attestations should be injected in the validated field of the mempool" ;
  let is_operation_in_validated_mempool mempool oph =
    let open JSON in
    let applied_list = as_list (mempool |-> "validated") in
    List.exists (fun e -> e |-> "hash" |> as_string = oph) applied_list
  in
  let* mempool =
    Node.RPC.(call node @@ get_chain_mempool_pending_operations ())
  in
  Check.is_true
    (List.for_all (is_operation_in_validated_mempool mempool) [oph1; oph2])
    ~error_msg:
      "After injection of the attestations, no operations should remain in the \
       mempool." ;
  log_step "Bake a block containing this attestation" ;
  let map_alias = List.map (fun x -> x.Account.alias) in
  (* Since the function [bake_for] crafts the attestation for delegates,
     we do not want it to craft a concurrent attestation for [bootstrap2].
     One which attests a DAL slot has already been injected. *)
  let* () =
    bake_for
      ~delegates:
        (`For (map_alias Constant.[bootstrap1; bootstrap4; bootstrap5]))
      client
  in
  let* current_level = Node.get_level node in
  log_step "Attestation should be included in the head block." ;
  let attested_levels = published_level --> current_level in
  let* all_slot_availabilities =
    Dal.collect_slot_availabilities node ~attested_levels
  in
  let* attestation_is_in_block =
    Dal.is_slot_attested
      ~endpoint:(Node.as_rpc_endpoint node)
      ~published_level
      ~slot_index:index
      ~to_attested_levels:(Dal.to_attested_levels ~protocol ~dal_parameters)
      all_slot_availabilities
  in
  Check.is_true
    attestation_is_in_block
    ~error_msg:"Slot from published level should be attested" ;
  log_step "Mempool should now be empty" ;
  let validated_mempool_is_empty mempool =
    let open JSON in
    let validated_list = as_list (mempool |-> "validated") in
    List.length validated_list = 0
  in
  let* mempool =
    Node.RPC.(call node @@ get_chain_mempool_pending_operations ())
  in
  Check.is_true
    (validated_mempool_is_empty mempool)
    ~error_msg:
      "After injection of the attestations, no operations should remain in the \
       mempool." ;
  log_step "Bake a few more blocks before calling RPC." ;
  let final_block_promise =
    wait_for_layer1_final_block producer_node (lvl_attest + 2)
  in
  let* () = bake_for ~count:3 client in
  let* () = final_block_promise in
  log_step "Call to the RPC of the DAL node to check that the slot is attested." ;
  let* status =
    Dal_RPC.(
      call producer_node
      (* The level the commitment is included in a block is the first one crafted
         after publication. *)
      @@ get_level_slot_status ~slot_level:published_level ~slot_index:index)
  in
  Log.info "Status is %a" Dal_RPC.pp_slot_id_status status ;
  log_step "Final checks." ;
  let* () =
    check_slot_status
      ~__LOC__
      producer_node
      ~expected_status:(Dal_RPC.Attested lag)
      ~check_attested_lag:`At_most
      ~slot_level:published_level
      ~slot_index:index
  in
  let not_attested_by_bootstrap2 =
    match Lwt.state not_attested_by_bootstrap2_promise with
    | Sleep -> false
    | Return () -> true
    | Fail exn ->
        Test.fail "Unexpected exception: '%s'" (Printexc.to_string exn)
  in
  Check.is_true
    ~error_msg:
      "We expected the slot to be protocol attested, but not by this attester \
       node."
    not_attested_by_bootstrap2 ;
  unit

let create_account ?(source = Constant.bootstrap2) ~amount ~alias client =
  Log.info
    "Create a [%s] account: generate a key, inject a transaction that funds \
     it, and bake a block to apply the transaction."
    alias ;
  let* fresh_account = Client.gen_and_show_keys ~alias client in
  let* _oph =
    Operation.Manager.inject_single_transfer
      client
      ~source
      ~dest:fresh_account
      ~amount
  in
  let* () = bake_for client in
  return fresh_account

let create_account_and_reveal ?source ~amount ~alias client =
  let* fresh_account = create_account ?source ~amount ~alias client in
  Log.info "Reveal pkh of [%s] account." alias ;
  let op_reveal =
    Operation.Manager.(make ~source:fresh_account (reveal fresh_account ()))
  in
  let* _oph = Operation.Manager.inject [op_reveal] client in
  let* () = bake_for client in
  return fresh_account

let create_low_stake node dal_parameters proto_params blocks_per_cycle =
  let minimal_stake = JSON.(proto_params |-> "minimal_stake" |> as_int) in
  let number_of_shards =
    dal_parameters.Dal.Parameters.cryptobox.number_of_shards
  in
  let* bootstrap_info =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_delegate Constant.bootstrap1.public_key_hash
  in
  let bootstrap_baking_power =
    JSON.(bootstrap_info |-> "baking_power" |> as_string) |> int_of_string
  in
  let total_baking_power =
    bootstrap_baking_power * Array.length Account.Bootstrap.keys
  in
  (* The amount is such that this baker has only one assigned shard per cycles
     (on average). Note that if the baker has no assigned shard, the test
     should still pass, because we just check that it gets its DAL rewards.

     Let [t] be total_baking_power, [s] be [small_baker_stake], and [1/n]
     desired shards fraction for the small baker. We want [s / (t + s) = 1 /
     n], ie [t + s = n * s], ie [t = (n-1) * s] ie, [s = t / (n-1)] *)
  let desired_stake =
    total_baking_power / ((blocks_per_cycle * number_of_shards) - 1)
  in
  let small_baker_stake = max desired_stake minimal_stake in
  Log.info
    "total_baking_power = %d, small_baker_stake = %d"
    total_baking_power
    small_baker_stake ;
  return small_baker_stake

(** [test_dal_rewards_distribution _protocol dal_parameters cryptobox node
    client _dal_node] verifies the correct distribution of DAL rewards among
    delegates based on their participation in DAL attestations activity.

    The test uses the 5 bootstrap accounts and a new account with a small stake
    that has on average one assigned shard per level.

    The main steps of the test are:

    1. Initialize delegates: we assign six accounts to specific roles:
    - **Baker:** Always attests both TenderBake (TB) and all DAL slots.
    - **Attesting DAL Slot 10:** Always attests TB and specifically DAL slot 10.
    - **Not Attesting at All:** Does not attest either TB or DAL slots.
    - **Not Attesting DAL:** Does not attest DAL slots, either by not sending
    any DAL attestation or by sending an empty bitset.
    - **Not Sufficiently Attesting DAL Slot 10:** Attests DAL slot 10 only 25%
    of the time.
    - **Small Baker:** It has very few assigned shards, but does the same as the Baker.

    2. Initial balances snapshot: we capture the initial balances of all
    delegates to compare against post-test balances.

    3. Publish DAL slots and inject DAL attestations: we define a helper
    function to inject DAL attestations based on each delegate's role:
    - we publish a dummy DAL slot at index 10.
    - we inject attestations from each delegate according to their assigned
    behavior.

    4. Blocks production until end of cycle: we bake blocks up to the last
    block of the current cycle, ensuring DAL slot publications and attestations
    are appropriately injected. Each time a new block is produced, we check the
    value of the "dal_attestation" bitset in the block's metadata and count the
    number of times the DAL slot at index 10 is attested.

    5. Second balances snapshot: this is done before TB and DAL rewards are
    distributed at the next block, which marks the end of the current cycle.

    6. Bake the final block of the cycle to trigger the distribution of DAL and
    TB rewards.

    7. Final snapshot of delegates' balances to assess the impact of rewards
    distribution.

    8. Balance Checks:
    - Ensure that delegates who did not attest at all (for both TB & DAL) have
    unchanged balances.
    - Verify that delegates with sufficient DAL participation received the
    expected DAL and TB rewards.
    - Confirm that delegates with insufficient or no DAL participation received
    only the expected TB rewards.

    9. Some extra checks related to /dal_participation RPC:
    - DAL participation status of each delegate matches the expected outcome
    based on their attestation behavior.
    - No delegates are denounced (there is no accuser running actually).
    - The attestable slots field of /dal_participation's result is equal to the
    number of attested slots counted in blocks metadata. *)
let test_dal_rewards_distribution protocol dal_parameters cryptobox node client
    _dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  assert (blocks_per_cycle >= dal_parameters.Dal.Parameters.attestation_lag) ;
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let all_slots = List.init number_of_slots Fun.id in

  (* Compute the stake of the small baker. *)
  let* small_baker_stake =
    create_low_stake node dal_parameters proto_params blocks_per_cycle
  in

  (* Each of the 5 bootstrap accounts contributes equally to the new baker's
     account. We give it a bit more tez, for it to be able to pay the fees for
     the pk reveal and stake operations. *)
  let to_transfer = (small_baker_stake + 3_000) / 5 in

  let* level = Node.get_level node in
  assert (level < blocks_per_cycle) ;
  let level = ref level in
  let* small_baker =
    create_account_and_reveal ~amount:to_transfer ~alias:"small_baker" client
  in
  incr level ;

  let* () =
    Lwt_list.iter_s
      (fun bootstrap ->
        if bootstrap <> Constant.bootstrap2 then
          let* _oph =
            Operation.Manager.inject_single_transfer
              client
              ~source:bootstrap
              ~dest:small_baker
              ~amount:to_transfer
          in
          unit
        else unit)
      (Array.to_list Account.Bootstrap.keys)
  in
  let* () = bake_for client in
  incr level ;

  Log.info "Register small_baker as a delegate" ;
  let* _small_baker =
    Client.register_delegate ~delegate:small_baker.alias client
  in
  let* () = bake_for client in
  incr level ;

  Log.info "Stake for small_baker" ;
  let* () =
    Client.stake
      (Tez.of_mutez_int small_baker_stake)
      ~staker:small_baker.public_key_hash
      client
  in

  Log.info
    "Bake (almost) %d cycles to activate the delegate"
    consensus_rights_delay ;
  let* () =
    bake_for
      ~count:((blocks_per_cycle * (1 + consensus_rights_delay)) - !level - 1)
      client
  in
  let* current_level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  assert (current_level.cycle_position = blocks_per_cycle - 1) ;
  level := blocks_per_cycle * (1 + consensus_rights_delay) ;
  assert (!level = current_level.level) ;

  (* We get our available delegates and assign them different roles they'll play
     during the test. *)
  let ( accounts_list,
        ( baker,
          attesting_dal_slot_10,
          not_attesting_at_all,
          not_attesting_dal,
          not_sufficiently_attesting_dal_slot_10,
          small_baker ) ) =
    match Account.Bootstrap.keys with
    | [|d1; d2; d3; d4; d5|] ->
        ( [
            (d1, "baker");
            (d2, "attesting_dal_slot_10");
            (d3, "not_attesting_at_all");
            (d4, "not_attesting_dal");
            (d5, "not_sufficiently_attesting_dal_slot_10");
            (small_baker, "small_baker");
          ],
          (d1, d2, d3, d4, d5, small_baker) )
    | _ -> Test.fail "Expected exactly 5 bootstrap accounts."
  in

  (* A helper function to get balances of the delegates above. *)
  let snapshot_full_balances_helper () =
    let bal account =
      let* b =
        Client.get_full_balance_for client ~account:account.Account.alias
      in
      return (Tez.mutez_int64 b)
    in
    let* l =
      Lwt.all
        [
          bal baker;
          bal attesting_dal_slot_10;
          bal not_attesting_at_all;
          bal not_attesting_dal;
          bal not_sufficiently_attesting_dal_slot_10;
          bal small_baker;
        ]
    in
    match l with
    | [b1; b2; b3; b4; b5; b6] -> return (b1, b2, b3, b4, b5, b6)
    | _ -> Test.fail "Not reachable."
  in

  Log.info "Snapshot the balances of the accounts at startup." ;
  let* ( _baker_bal0,
         attesting_dal_slot_10_bal0,
         not_attesting_at_all_bal0,
         not_attesting_dal_bal0,
         not_sufficiently_attesting_dal_slot_10_bal0,
         small_baker_bal0 ) =
    snapshot_full_balances_helper ()
  in

  let node_endpoint = Node.as_rpc_endpoint node in

  (* This is the main helper function which injects (DAL) attestations for
     delegates depending on their profiles. *)
  let inject_attestations () =
    let count_dal_attesting_bakers = ref 0 in
    (* 1. Baker always attests TB and all DAL slots *)
    let* (_ : Operation.t * [`OpHash of peer_id]) =
      (* The baker delegate will miss 1/10 of its DAL attestations and will send
         [No_dal_attestation] in this case. *)
      let baker_attestation =
        if !level mod 10 = 0 then No_dal_attestation
        else (
          incr count_dal_attesting_bakers ;
          Slots all_slots)
      in
      inject_dal_attestation_exn
        ~protocol
        ~signer:baker
        baker_attestation
        client
        dal_parameters
        node_endpoint
    in
    (* 2. attesting_dal_slot_10 always attests TB and DAL slot 10 *)
    let* (_ : Operation.t * [`OpHash of peer_id]) =
      (* The attesting_dal_slot_10 delegate misses 1/11th of its DAL
         attestations and will send Slots [], but it should be fine for its
         rewards. *)
      let attestation =
        if !level mod 11 = 0 then Slots []
        else (
          incr count_dal_attesting_bakers ;
          Slots [10])
      in
      inject_dal_attestation_exn
        ~protocol
        ~signer:attesting_dal_slot_10
        attestation
        client
        dal_parameters
        node_endpoint
    in
    (* 3. not_attesting_at_all is not attesting neither TB nor DAL slots *)
    (* 4. not_attesting_dal either sends no DAL content or sends bitset 0 *)
    let* (_ : Operation.t * [`OpHash of peer_id]) =
      let dal_attestation =
        if !level mod 2 = 0 then No_dal_attestation else Slots []
      in
      inject_dal_attestation_exn
        ~protocol
        ~signer:not_attesting_dal
        dal_attestation
        client
        dal_parameters
        node_endpoint
    in
    (* 5. not_sufficiently_attesting_dal_slot_10: is attesting DAL slot 10, but only 25%
       of the time. *)
    let* (_ : Operation.t * [`OpHash of peer_id]) =
      let slots_to_attest =
        if !level mod 4 = 0 then (
          incr count_dal_attesting_bakers ;
          Slots [10])
        else No_dal_attestation
      in
      inject_dal_attestation_exn
        ~protocol
        ~signer:not_sufficiently_attesting_dal_slot_10
        slots_to_attest
        client
        dal_parameters
        node_endpoint
    in
    (* 6. small_baker: is always attesting DAL slot 10. *)
    let* () =
      let slots_to_attest = Slots [10] in
      let* res =
        inject_dal_attestation
          ~protocol
          ~signer:small_baker
          slots_to_attest
          client
          dal_parameters
          node_endpoint
      in
      (match res with
      | None ->
          Log.info
            "At level %d, %s could not TB attest"
            !level
            small_baker.alias
      | Some _ -> incr count_dal_attesting_bakers) ;
      unit
    in
    Log.info
      "At level %d, there are %d bakers that DAL attested"
      !level
      !count_dal_attesting_bakers ;
    unit
  in

  (* We'll count the number of times we see that slot 10 is attested in blocks
     metadata. We'll then check the final value against the
     delegate_attestable_dal_slots field of /dal_participation for each
     delegate. *)
  let count_set_dal_attestation_bitset = ref 0 in

  let count_slot_10_if_attested () =
    let* block_json = Node.RPC.(call node @@ get_chain_block_metadata_raw ()) in
    let block_dal_attestation_bitset =
      JSON.(block_json |-> "dal_attestation" |> as_int)
    in
    Log.info
      "At level %d, dal_attestation = %d"
      !level
      block_dal_attestation_bitset ;
    (* count when slot 10 is attested *)
    let expected_bitset =
      if Protocol.number protocol <= 024 then 1024
      else
        (* For the compact multi-lag encoding with 1 lag and slot 10:
           - prefix bit 0 = 1
           - chunks of 8 bits (is_last + 7 slots), slot 10 is in chunk 1
           - is_last bit for chunk 1 at position 1 + 8 = 9
           - slot 10 bit at position 1 + 8 + (1 + (10 mod 7)) = 13
           Result: 2^0 + 2^9 + 2^13 = 1 + 512 + 8192 = 8705 *)
        8705
    in
    if block_dal_attestation_bitset = expected_bitset then
      incr count_set_dal_attestation_bitset ;
    unit
  in

  (* This is the main entry of the test: we start by baking a number of blocks
     in which we inject DAL slots publications at slot index 10 and TB/DAL
     attestations. We stop before baking the last block of the current cycle,
     where DAL rewards are distributed. *)
  Log.info
    "Bake and publish for one cycle minus one block (%d blocks)"
    (blocks_per_cycle - 1) ;
  let* () =
    repeat (blocks_per_cycle - 1) (fun () ->
        let* () = count_slot_10_if_attested () in
        let* (`OpHash _oph1) =
          publish_dummy_slot
            ~source:baker
            ~index:10
            ~message:"hi!"
            cryptobox
            client
        in
        let* () = inject_attestations () in
        let* () =
          (* Do not use [bake_for] because it also injects an attestation
             operation for [baker]. *)
          Client.propose_for_and_wait
            ~key:[baker.Account.public_key_hash]
            client
        in
        incr level ;
        let* manager_ops =
          Node.RPC.call node
          @@ RPC.get_chain_block_operations_validation_pass
               ~validation_pass:3
               ()
        in
        Check.(
          (List.length @@ JSON.as_list manager_ops = 1)
            int
            ~__LOC__
            ~error_msg:"expected 1 manager operation in block, got %L") ;
        let* attestations =
          Node.RPC.call node
          @@ RPC.get_chain_block_operations_validation_pass
               ~validation_pass:0
               ()
        in
        let num_attestations = List.length @@ JSON.as_list attestations in
        (* The small baker may not have rights to inject its attestation *)
        let check_num = num_attestations = 4 || num_attestations = 5 in
        Check.is_true
          check_num
          ~__LOC__
          ~error_msg:"expected 4 or 5 attestations in block" ;
        unit)
  in

  Log.info
    "After this first round of blocks, we snapshot the balances of our \
     delegates again." ;
  let* ( _baker_bal1,
         attesting_dal_slot_10_bal1,
         not_attesting_at_all_bal1,
         not_attesting_dal_bal1,
         not_sufficiently_attesting_dal_slot_10_bal1,
         small_baker_bal1 ) =
    snapshot_full_balances_helper ()
  in

  let* () = count_slot_10_if_attested () in

  let* bootstrap_accounts_participation =
    Lwt_list.map_s
      (fun (account, _account_role) ->
        let* dal_participation =
          Node.RPC.call node
          @@ RPC.get_chain_block_context_delegate_dal_participation
               account.Account.public_key_hash
        in
        return (account, dal_participation))
      accounts_list
  in

  (* We use the 'participation' RPC to get the expected Tenderbake rewards of delegate
     who TB-attested sufficiently. *)
  let snapshot_tb_participation () =
    let participation account =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_delegate_participation
           account.Account.public_key_hash
    in
    let* l =
      Lwt_list.map_s
        participation
        [
          baker;
          attesting_dal_slot_10;
          not_attesting_at_all;
          not_attesting_dal;
          not_sufficiently_attesting_dal_slot_10;
          small_baker;
        ]
    in
    match l with
    | [p1; p2; p3; p4; p5; p6] -> return (p1, p2, p3, p4, p5, p6)
    | _ -> Test.fail "Not reachable."
  in
  let* ( _baker_tb_participation,
         attesting_dal_slot_10_tb_participation,
         _not_attesting_at_all_tb_participation,
         not_attesting_dal_tb_participation,
         not_sufficiently_attesting_dal_slot_10_tb_participation,
         small_baker_tb_participation ) =
    snapshot_tb_participation ()
  in

  Log.info
    "We now bake the last block of the cycle, which should trigger (TB and) \
     DAL rewards distribution." ;
  (* TB rewards are actually set to 0. *)
  let* () = bake_for ~delegates:(`For [baker.Account.public_key_hash]) client in
  let* metadata = Node.RPC.(call node @@ get_chain_block_metadata_raw ()) in
  incr level ;
  let* current_level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  assert (current_level.cycle_position = blocks_per_cycle - 1) ;

  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  let expected_to_lose_attesting_rewards =
    if Protocol.number protocol >= 023 then
      (* keep those that lost consensus attesting rewards because they haven't
         revealed their nonces *)
      List.filter_map
        (fun json ->
          let check json =
            JSON.(json |-> "kind" |> as_string) |> String.equal "burned"
            && JSON.(json |-> "category" |> as_string)
               |> String.equal "lost attesting rewards"
            && (not JSON.(json |-> "participation" |> as_bool))
            && JSON.(json |-> "revelation" |> as_bool)
          in
          if check json then Some JSON.(json |-> "delegate" |> as_string)
          else None)
        balance_updates
    else []
  in
  (* We snapshot the balances of the delegates at the end of the cycle. *)
  let* ( _baker_bal2,
         attesting_dal_slot_10_bal2,
         not_attesting_at_all_bal2,
         not_attesting_dal_bal2,
         not_sufficiently_attesting_dal_slot_10_bal2,
         small_baker_bal2 ) =
    snapshot_full_balances_helper ()
  in

  (* We're now ready to do some checks, both on balances and on
     dal_participation RPC's result. *)
  (* First, we define a function to check expected balances of delegates. *)
  let check_bal_incr ~__LOC__ account bal_before bal_after ~delta =
    let error_msg =
      if delta = 0 then
        "account " ^ account.Account.public_key_hash
        ^ ", expected balance to be unchanged. Got %R, expecting %L"
      else
        "account " ^ account.Account.public_key_hash
        ^ ", unexpected balance: got %R, expecting %L = "
        ^ Int64.to_string bal_before ^ " + " ^ string_of_int delta
    in
    Check.(Int64.(add bal_before (of_int delta)) = bal_after)
      ~__LOC__
      Check.int64
      ~error_msg
  in

  (* Except for the baker, we don't expect the balances of the delegates to
     change before baking the last block of the current cycle. *)
  List.iter
    (fun (account, bal0, bal1) ->
      check_bal_incr ~__LOC__ account bal0 bal1 ~delta:0)
    [
      ( attesting_dal_slot_10,
        attesting_dal_slot_10_bal0,
        attesting_dal_slot_10_bal1 );
      ( not_attesting_at_all,
        not_attesting_at_all_bal0,
        not_attesting_at_all_bal1 );
      (not_attesting_dal, not_attesting_dal_bal0, not_attesting_dal_bal1);
      ( not_sufficiently_attesting_dal_slot_10,
        not_sufficiently_attesting_dal_slot_10_bal0,
        not_sufficiently_attesting_dal_slot_10_bal1 );
      (small_baker, small_baker_bal0, small_baker_bal1);
    ] ;

  (* As all delegates except [small_baker] have the same stake distribution,
     they're expected to have the same number of the assigned shards, to have
     shards at the same level, to have the same rewards allocated, ... *)
  let () =
    match List.rev bootstrap_accounts_participation with
    | (_small_baker, _) :: (_account, dal_part) :: rest ->
        List.iter
          (fun (_account, dal_participation) ->
            (* We transferred funds from [bootstrap2] to [small_baker], so
               [bootstrap_2] has a smaller baking power than the other bootstrap
               delegates. *)
            Check.(
              dal_part.expected_assigned_shards_per_slot
              = dal_participation.RPC.expected_assigned_shards_per_slot)
              ~__LOC__
              Check.int
              ~error_msg:"expected_assigned_shards_per_slot mismatch" ;
            Check.(
              Tez.to_mutez dal_part.expected_dal_rewards
              = Tez.to_mutez dal_participation.expected_dal_rewards)
              ~__LOC__
              Check.int
              ~error_msg:"expected_dal_rewards mismatch" ;
            Check.(
              dal_part.delegate_attestable_dal_slots
              = dal_participation.delegate_attestable_dal_slots)
              ~__LOC__
              Check.int
              ~error_msg:"delegate_attestable_dal_slots mismatch")
          rest
    | _ -> Test.fail "Not reachable."
  in

  (* After baking the last block of the cycle, we check that:
     - the balances of the delegates who didn't attest at all didn't change.
     - the participation RPC's result is aligned with the first check. *)
  List.iter
    (fun (account, bal1, bal2) ->
      check_bal_incr ~__LOC__ account bal1 bal2 ~delta:0 ;
      let dal_participation =
        List.assoc account bootstrap_accounts_participation
      in
      Check.is_false
        dal_participation.sufficient_dal_participation
        ~__LOC__
        ~error_msg:
          ("account " ^ account.Account.public_key_hash
         ^ ", expected to have insufficient DAL participation."))
    [
      ( not_attesting_at_all,
        not_attesting_at_all_bal1,
        not_attesting_at_all_bal2 );
    ] ;

  (* After baking the last block of the cycle, we check that:
     - the balances of the delegates who didn't attest DAL sufficiently or
       didn't attest DAL at all only increased by a delta equal to the expected
       Tenderbake attestation rewards.
     - the participation RPC's result is aligned with the first check. *)
  List.iter
    (fun (account, bal1, bal2, tb_participation, sufficient_dal_participation)
       ->
      let dal_participation =
        List.assoc account bootstrap_accounts_participation
      in
      Check.(
        (dal_participation.sufficient_dal_participation
       = sufficient_dal_participation)
          ~__LOC__
          bool
          ~error_msg:
            ("account " ^ account.Account.public_key_hash
           ^ ", expected to have sufficient DAL participation? %R, but got %L")) ;
      let expecting_attesting_rewards =
        not
        @@ List.mem account.public_key_hash expected_to_lose_attesting_rewards
      in
      let expected_attesting_rewards =
        if expecting_attesting_rewards then
          Tez.to_mutez tb_participation.RPC.expected_attesting_rewards
        else 0
      in
      let expected_dal_rewards =
        if sufficient_dal_participation && expecting_attesting_rewards then
          Tez.to_mutez dal_participation.expected_dal_rewards
        else 0
      in
      let delta = expected_attesting_rewards + expected_dal_rewards in
      Log.info
        "[check] %s %s: %Ld = %Ld + %d + %d"
        account.Account.alias
        account.public_key_hash
        bal1
        bal2
        expected_attesting_rewards
        expected_dal_rewards ;
      check_bal_incr ~__LOC__ account bal1 bal2 ~delta)
    [
      ( not_attesting_dal,
        not_attesting_dal_bal1,
        not_attesting_dal_bal2,
        not_attesting_dal_tb_participation,
        false );
      ( not_sufficiently_attesting_dal_slot_10,
        not_sufficiently_attesting_dal_slot_10_bal1,
        not_sufficiently_attesting_dal_slot_10_bal2,
        not_sufficiently_attesting_dal_slot_10_tb_participation,
        false );
      ( attesting_dal_slot_10,
        attesting_dal_slot_10_bal1,
        attesting_dal_slot_10_bal2,
        attesting_dal_slot_10_tb_participation,
        true );
      ( small_baker,
        small_baker_bal1,
        small_baker_bal2,
        small_baker_tb_participation,
        true );
    ] ;

  (* As a final check, we verify that no delegate is denounced and that we
     report the correct number of attested slots. *)
  List.iter
    (fun (account, dal_participation) ->
      Check.is_false
        dal_participation.RPC.denounced
        ~__LOC__
        ~error_msg:
          ("account " ^ account.Account.public_key_hash
         ^ ", not expected to be denounced.") ;

      (* The number of attestable slots for the small stake baker is smaller
         than for the other bakers and since we did not count them, we don't
         check them. *)
      if not @@ String.equal account.alias small_baker.alias then
        let msg_prefix = sf "For delegate %s: " account.public_key_hash in
        Check.(
          dal_participation.delegate_attestable_dal_slots
          = !count_set_dal_attestation_bitset)
          ~__LOC__
          Check.int
          ~error_msg:
            (msg_prefix
           ^ "expecting %L attestable DAL slots, but %R were reported in \
              blocks metadata"))
    bootstrap_accounts_participation ;
  unit

let use_mockup_node_for_getting_attestable_slots protocol dal_parameters
    cryptobox l1_node client _bootstrap_key =
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let attestation_lag = dal_parameters.attestation_lag in

  Log.info "Start the mocked DAL node" ;
  let dal_node_mockup =
    let attesters =
      Account.Bootstrap.keys |> Array.to_list
      |> List.map (fun b -> b.Account.public_key_hash)
    in
    let attestable_slots ~attester:_ ~attested_level:_ =
      List.init number_of_slots (fun _0 -> true)
    in
    Dal_node.Mockup_for_baker.make
      ~name:"mock-dal-node"
      ~attestation_lag
      ~attesters
      ~attestable_slots
  in
  let port = Port.fresh () in
  let () = Dal_node.Mockup_for_baker.run dal_node_mockup ~port in
  let dal_node_rpc_endpoint =
    Endpoint.make ~host:"localhost" ~scheme:"http" ~port ()
  in
  let baker = Agnostic_baker.create ~dal_node_rpc_endpoint l1_node client in

  Log.info "Publish a slot" ;
  let* (`OpHash _op_hash) =
    publish_dummy_slot
      ~source:Constant.bootstrap1
      ~index:0
      ~message:"a"
      cryptobox
      client
  in
  (* Bake one block manually to include the slot operation, so that
     published_level is deterministic. Without this, the baker might
     bake the next block before the operation reaches the validated
     mempool, pushing it to a later level. *)
  let* () = bake_for client in
  let* published_level = Node.get_level l1_node in

  Log.info "Start the baker" ;
  let* () = Agnostic_baker.run baker in

  (* +2 blocks for the attested block to be final, +1 for some slack *)
  let* _ =
    Node.wait_for_level l1_node (published_level + attestation_lag + 3)
  in
  let* () = Agnostic_baker.terminate baker in
  let () = Dal_node.Mockup_for_baker.stop dal_node_mockup in

  let attested_level = published_level + attestation_lag in
  Log.info
    "Check that the slot published at level %d was attested at level %d"
    published_level
    attested_level ;
  let* {dal_attestation; _} =
    Node.RPC.(
      call l1_node
      @@ get_chain_block_metadata ~block:(string_of_int attested_level) ())
  in
  let* dal_attestation =
    match dal_attestation with
    | None -> return None
    | Some str ->
        let* per_lag =
          Dal.Slot_availability.decode
            protocol
            (Node.as_rpc_endpoint l1_node)
            dal_parameters
            str
        in
        return
        @@ Some
             (Array.init number_of_slots (fun slot_index ->
                  Array.exists (fun lag_array -> lag_array.(slot_index)) per_lag))
  in
  let expected_attestation = expected_attestation dal_parameters [0] in
  Check.((Some expected_attestation = dal_attestation) (option (array bool)))
    ~error_msg:"Unexpected DAL attestation: expected %L, got %R" ;
  unit

let wait_for_branch_switch node level =
  let filter json =
    match JSON.(json |-> "level" |> as_int_opt) with
    | Some l when l = level -> Some ()
    | Some _ -> None
    | None -> None
  in
  Node.wait_for node "branch_switch.v0" filter

let baker_at_round_n ?level round client : string Lwt.t =
  let* json =
    Client.RPC.call client @@ RPC.get_chain_block_helper_baking_rights ?level ()
  in
  match JSON.(json |=> round |-> "delegate" |> as_string_opt) with
  | Some delegate_id -> return delegate_id
  | None ->
      Test.fail
        "Could not find the baker at round %d for level %s"
        round
        (match level with None -> "head" | Some level -> string_of_int level)

(* Simulate a fork at level n where two competing blocks exist:
   - n
     - [node1]: A1 (round 0, no DAL commitment)
     - [node2]: A2 (round 1, with DAL commitment)
   Then, build one more block on top of A2:
   - n+1
     - [node2]: B (on top of A2)
   After reconnecting the nodes, node1 switches to the A2 -> B branch (due to
   higher fitness).
   Then, test that shards are propagated after one level is baked on top of
   the block which included the commitment publication, at (n+1). *)
let test_dal_one_level_reorg protocol dal_parameters _cryptobox node1 client1
    dal_bootstrap =
  (* Helpers / Constants *)
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let slot_index = 0 in

  (* Spin up a second L1 node and connect it *)
  let nodes_args = Node.[Synchronisation_threshold 0; Connections 1] in
  let* node2, client2 =
    Client.init_with_protocol ~protocol ~nodes_args `Client ()
  in
  let* () =
    Client.Admin.connect_address ~endpoint:(Node node1) ~peer:node2 client1
  in

  (* DAL Bootstrap *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;
  let peers = [Dal_node.listen_addr dal_bootstrap] in

  (* DAL Producer on [node1] *)
  let producer = Dal_node.create ~name:"producer" ~node:node1 () in
  let* () =
    Dal_node.init_config ~operator_profiles:[slot_index] ~peers producer
  in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator slot_index])
  in
  Log.info "Slot producer DAL node is running" ;

  (* DAL Attester on [node1] *)
  let* proto_params =
    Node.RPC.call node1 @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* balance =
    Client.get_balance_for ~account:Constant.bootstrap1.alias client1
  in
  let amount = Tez.(balance - one) in
  let* new_account = Client.gen_and_show_keys client1 in
  let* () =
    Client.transfer
      ~giver:Constant.bootstrap1.alias
      ~receiver:new_account.alias
      ~amount
      ~burn_cap:Tez.one
      client1
  in
  let* () = bake_for client1 in
  let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client1 in
  let* () = bake_for client1 in
  let* () = Client.register_key new_account.alias client1 in
  let* () = bake_for client1 in
  let* () = Client.stake ~staker:new_account.alias Tez.(amount /! 2L) client1 in
  let attester = Dal_node.create ~name:"attester" ~node:node1 () in
  let* () =
    Dal_node.init_config
      ~attester_profiles:[new_account.public_key_hash]
      ~peers
      attester
  in
  let* () = Dal_node.run ~event_level:`Debug attester in
  let client1 = Client.with_dal_node client1 ~dal_node:attester in

  (* Compute the DAL attestation level for publication *)
  let num_cycles = 1 + consensus_rights_delay in
  let* level = Client.level client1 in
  let lag = dal_parameters.attestation_lag in
  let attestation_level = (num_cycles * blocks_per_cycle) + 1 in
  let published_level = attestation_level + 1 - lag in
  Log.info "Bake blocks up to level %d" (published_level - 1) ;
  let* () = bake_for ~count:(published_level - 1 - level) client1 in
  let* current_level = Client.level client1 in
  Log.info
    "current_level = %d; published_level = %d; attestation_level = %d"
    current_level
    published_level
    attestation_level ;

  (* Align node2 to current level, then disconnect to fork *)
  let* _ = Node.wait_for_level node2 current_level in
  let* node2_id = Node.wait_for_identity node2 in
  let* () =
    Client.Admin.kick_peer ~endpoint:(Node node1) ~peer:node2_id client1
  in

  (* Branch 1 on node1: bake A1 at round 0 (lower fitness) *)
  let* baker_low_round = baker_at_round_n ~level:published_level 0 client1 in
  let* () = bake_for ~delegates:(`For [baker_low_round]) client1 in
  Log.info "node1 baked A1 at (level = %d, round = 0)" published_level ;

  (* Branch 2 on node2: bake A2 with commitment at round 1, then B *)
  let* _ =
    Helpers.publish_and_store_slot
      client2
      producer
      Constant.bootstrap2
      ~index:slot_index
    @@ Helpers.make_slot ~slot_size "REORG-SLOTDATA"
  in
  let* baker_high_round = baker_at_round_n ~level:published_level 1 client2 in
  let* () = bake_for ~delegates:(`For [baker_high_round]) client2 in
  Log.info "node1 baked A2 at (level = %d, round = 1)" published_level ;
  let* manager_ops =
    Node.RPC.call node2
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:3 ()
  in
  Check.(
    (JSON.as_list manager_ops |> List.length <> 0)
      int
      ~error_msg:
        "Expected the commitment to be published, but no manager operation was \
         included.") ;
  let* assigned_shard_indexes =
    Dal_RPC.(
      call attester
      @@ get_assigned_shard_indices
           ~level:attestation_level
           ~pkh:new_account.public_key_hash)
  in
  let wait_for_shards_promises =
    wait_for_shards_promises
      ~dal_node:attester
      ~storage_profile:`Cache_only
      ~shards:assigned_shard_indexes
      ~published_level
      ~slot_index
  in
  let* () = bake_for client2 in
  Log.info "node2 baked B at level = %d" (published_level + 1) ;

  (* Reconnect & wait for node1 to switch to node2’s higher-fitness branch *)
  let wait_switch = wait_for_branch_switch node1 published_level in
  let* () =
    Client.Admin.connect_address ~endpoint:(Node node1) ~peer:node2 client1
  in
  let* () = wait_switch in
  Log.info "node1 switched to branch with round = 1 at level %d" published_level ;

  let* _ = Node.wait_for_level node1 (published_level + 1) in
  Log.info "node1 synchronised with node2" ;
  Log.info "Waiting for attester to receive its assigned shards" ;
  let* () = wait_for_shards_promises in
  unit

let test_dal_low_stake_attester_attestable_slots protocol dal_parameters
    _cryptobox node client dal_node =
  let {log_step} = init_logger () in
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let slot_index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in

  let* low_stake =
    create_low_stake node dal_parameters proto_params blocks_per_cycle
  in
  let* new_account = Client.gen_and_show_keys client in
  let amount = Tez.of_mutez_int (low_stake + 3_000_000) in
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
  let* _ = Client.register_delegate ~delegate:new_account.alias client in
  let* () = bake_for client in
  let* () =
    Client.stake
      (Tez.of_mutez_int low_stake)
      ~staker:new_account.public_key_hash
      client
  in
  let* () = bake_for client in
  let client = Client.with_dal_node client ~dal_node in
  log_step "Attester DAL node started (operator + attester)." ;

  let* current_level = Node.get_level node in
  log_step
    "Bake (almost) %d cycles to activate the delegate"
    consensus_rights_delay ;
  let* () =
    bake_for
      ~count:
        ((blocks_per_cycle * (1 + consensus_rights_delay)) - current_level - 1)
      client
  in

  (* Find DAL attestation bitset in the last block for the delegate, if an
     [attestation_with_dal] exists. *)
  let get_delegate_dal_attestation_opt () =
    let* json =
      Node.RPC.call node
      @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
    in
    let dal_attestation_opt =
      List.find_map
        (fun json ->
          let contents = JSON.(json |-> "contents" |> as_list) |> List.hd in
          let delegate =
            JSON.(contents |-> "metadata" |-> "delegate" |> as_string)
          in
          let kind = JSON.(contents |-> "kind" |> as_string) in
          if
            String.equal delegate new_account.public_key_hash
            && String.equal kind "attestation_with_dal"
          then Some JSON.(contents |-> "dal_attestation" |> as_string)
          else None)
        (JSON.as_list json)
    in
    return dal_attestation_opt
  in

  let count_not_in_committee = ref 0 in
  let count_attestable_slots = ref 0 in
  let count_traps = ref 0 in

  let* first_level = Client.level client in

  let check_one_level () =
    (* Publish a slot, then bake a block with all delegates. *)
    let* _ =
      Helpers.publish_and_store_slot
        client
        dal_node
        Constant.bootstrap2
        ~index:slot_index
        (Helpers.make_slot ~slot_size "SLOTDATA")
    in
    let* () = bake_for client in
    let* attested_level = Client.level client in
    let published_level = attested_level - dal_parameters.attestation_lag in

    let* expected_dal_attestable_slots =
      Dal_RPC.(
        call dal_node
        @@ get_attestable_slots ~attester:new_account ~attested_level)
    in
    let* actual_dal_attestable_slots_opt =
      get_delegate_dal_attestation_opt ()
    in
    let expected_bit_opt =
      match expected_dal_attestable_slots with
      | Not_in_committee -> None
      | Attestable_slots slots ->
          Some
            (match List.nth_opt slots slot_index with
            | Some b -> b
            | None -> false)
    in
    (match expected_bit_opt with
    | None -> incr count_not_in_committee
    | Some true -> incr count_attestable_slots
    | _ -> ()) ;

    (* Check if delegate actually attested our slot when they included DAL content *)
    let* actual_bit_opt =
      match actual_dal_attestable_slots_opt with
      | None -> return None
      | Some dal_attestation ->
          let* b =
            Dal.is_slot_attested_in_bitset
              ~endpoint:(Node.as_rpc_endpoint node)
              ~protocol
              ~dal_parameters
              ~attested_level
              ~published_level
              ~slot_index
              ~dal_attestation
          in
          return (Some b)
    in

    match (expected_dal_attestable_slots, actual_bit_opt) with
    | Not_in_committee, None ->
        log_step "Level %d: Not_in_committee" attested_level ;
        unit
    | Not_in_committee, Some true ->
        Test.fail
          "Level %d: Not_in_committee but delegate attested slot %d from \
           published_level=%d"
          attested_level
          slot_index
          published_level
    | Not_in_committee, Some false ->
        log_step
          "Level %d: Not_in_committee (attestation for other lags/levels)"
          attested_level ;
        unit
    | Attestable_slots _slots, None ->
        if published_level > first_level then
          Test.fail
            "Level %d: delegate %s is in committee but no DAL attestation found"
            attested_level
            new_account.public_key_hash
        else unit
    | Attestable_slots _slots, Some actual_bit ->
        let expected_bit =
          match expected_bit_opt with Some b -> b | None -> false
        in
        if actual_bit <> expected_bit then
          Test.fail
            "Level %d: DAL node says bit(%d)=%b for published_level=%d, but \
             delegate's attestation shows bit=%b"
            attested_level
            slot_index
            expected_bit
            published_level
            actual_bit
        else
          let* has_traps =
            let* traps =
              Dal_RPC.(
                call dal_node
                @@ get_published_level_known_traps
                     ~published_level
                     ~pkh:new_account.public_key_hash
                     ~slot_index)
            in
            return @@ not @@ List.is_empty traps
          in
          if has_traps then incr count_traps ;
          if published_level <= first_level then (
            Check.(
              (actual_bit = false)
                ~__LOC__
                bool
                ~error_msg:
                  (sf "Level %d: Expected false, got true" attested_level)) ;
            unit)
          else if actual_bit <> not has_traps then
            Test.fail
              "Level %d, slot %d, published_level %d: bit=[%b], has_traps=%b"
              attested_level
              slot_index
              published_level
              actual_bit
              has_traps
          else unit
  in

  (* Run until we've seen all three cases, or give up after a limit. *)
  let max_steps = 10 * blocks_per_cycle in
  log_step
    "Running measurement for at most %d steps (up to %d cycles)."
    max_steps
    (max_steps / blocks_per_cycle) ;

  let rec loop step =
    if
      !count_not_in_committee > 0
      && !count_attestable_slots > 0
      && !count_traps > 0
    then unit
    else if step > max_steps then
      Test.fail
        "Reached max_steps=%d without seeing all event kinds. Summary: \
         Not_in_committee=%d, Attestable_slots=%d, Traps=%d"
        max_steps
        !count_not_in_committee
        !count_attestable_slots
        !count_traps
    else
      let* () = check_one_level () in
      loop (step + 1)
  in
  let* () = loop 1 in

  log_step
    "Final summary: Not_in_committee = %d, Attestable_slots = %d, Traps = %d"
    !count_not_in_committee
    !count_attestable_slots
    !count_traps ;

  unit

(** Test encode/decode round-trip for single lag. *)
let test_attestations_encode_decode_single_lag ~__FILE__ =
  Protocol.register_test
    ~__FILE__
    ~title:"DAL attestations encode/decode single lag"
    ~tags:["dal"; "attestations"; "encode"; "decode"; "single_lag"]
    ~uses_node:true
    ~uses_client:true
  @@ fun protocol ->
  let* node, _client = Client.init_with_protocol `Client ~protocol () in
  let attestation_lag, attestation_lags =
    if Protocol.number protocol < 025 then (8, [8]) else (5, [2; 3; 5])
  in
  let dal_parameters : Dal.Parameters.t =
    {
      feature_enabled = true;
      incentives_enabled = false;
      cryptobox =
        {
          number_of_shards = 2048;
          redundancy_factor = 16;
          slot_size = 65536;
          page_size = 4096;
        };
      number_of_slots = 4;
      attestation_lag;
      attestation_lags;
      attestation_threshold = 50;
    }
  in
  let number_of_lags = List.length dal_parameters.attestation_lags in
  Log.info
    "Parameters: number_of_slots=%d, number_of_lags=%d, attestation_lags=[%s]"
    dal_parameters.number_of_slots
    number_of_lags
    (dal_parameters.attestation_lags |> List.map string_of_int
   |> String.concat "; ") ;

  Log.info "Create a test attestation: first and last slot attested" ;
  let original = Array.make dal_parameters.number_of_slots false in
  original.(0) <- true ;
  original.(dal_parameters.number_of_slots - 1) <- true ;

  let* () =
    Lwt_list.iter_s
      (fun lag_index ->
        Log.info "Testing encoding/decoding at lag_index: %d" lag_index ;
        let* encoded =
          Dal.Attestations.encode_for_one_lag
            protocol
            (Node.as_rpc_endpoint node)
            dal_parameters
            ~lag_index
            original
        in
        Log.info "Encoded (lag_index: %d): %s" lag_index encoded ;

        let* decoded =
          Dal.Attestations.decode
            protocol
            (Node.as_rpc_endpoint node)
            dal_parameters
            encoded
        in
        let decoded_for_lag = decoded.(lag_index) in
        Check.(
          (Array.length decoded_for_lag = dal_parameters.number_of_slots) int)
          ~error_msg:"Expected %R slots, got %L" ;
        Array.iteri
          (fun i expected ->
            Check.(decoded_for_lag.(i) = expected)
              Check.bool
              ~error_msg:(Format.sprintf "Slot %d mismatch after round-trip" i))
          original ;
        unit)
      (List.init number_of_lags Fun.id)
  in

  Log.info "Single-lag encode/decode succeeded" ;
  unit

(** Test encode/decode round-trip for multiple lags. *)
let test_attestations_encode_decode_multiple_lags ~__FILE__ =
  Protocol.register_test
    ~__FILE__
    ~title:"DAL attestations encode/decode multiple lags"
    ~tags:["dal"; "attestations"; "encode"; "decode"; "multiple_lags"]
    ~uses_node:true
    ~uses_client:true
    ~supports:(Protocol.From_protocol 025)
  @@ fun protocol ->
  let* node, _client = Client.init_with_protocol `Client ~protocol () in
  let dal_parameters : Dal.Parameters.t =
    {
      feature_enabled = true;
      incentives_enabled = false;
      cryptobox =
        {
          number_of_shards = 2048;
          redundancy_factor = 16;
          slot_size = 65536;
          page_size = 4096;
        };
      number_of_slots = 4;
      attestation_lag = 5;
      attestation_lags = [2; 3; 5];
      attestation_threshold = 50;
    }
  in
  let number_of_lags = List.length dal_parameters.attestation_lags in
  Log.info
    "Parameters: number_of_slots=%d, number_of_lags=%d, attestation_lags=[%s]"
    dal_parameters.number_of_slots
    number_of_lags
    (dal_parameters.attestation_lags |> List.map string_of_int
   |> String.concat "; ") ;

  (* Create test attestations: different slots attested at different lags *)
  let attestations_per_lag =
    Array.init number_of_lags (fun lag_index ->
        let arr = Array.make dal_parameters.number_of_slots false in
        (* Attest slot [lag_index mod number_of_slots] at each lag *)
        arr.(lag_index mod dal_parameters.number_of_slots) <- true ;
        (* Also attest slot 1 at lag 0 if we have multiple slots *)
        if lag_index = 0 && dal_parameters.number_of_slots > 1 then
          arr.(1) <- true ;
        arr)
  in
  let* encoded =
    Dal.Attestations.encode
      protocol
      (Node.as_rpc_endpoint node)
      attestations_per_lag
  in
  Log.info "Encoded multi-lag: %s" encoded ;

  let* decoded =
    Dal.Attestations.decode
      protocol
      (Node.as_rpc_endpoint node)
      dal_parameters
      encoded
  in
  Check.((Array.length decoded = number_of_lags) int)
    ~error_msg:"Expected %R lags, got %L" ;
  Array.iteri
    (fun lag_index original_arr ->
      let decoded_arr = decoded.(lag_index) in
      Check.((Array.length decoded_arr = dal_parameters.number_of_slots) int)
        ~error_msg:
          (Format.sprintf "Lag %d: expected %%R slots, got %%L" lag_index) ;
      Array.iteri
        (fun slot_index expected ->
          Check.(decoded_arr.(slot_index) = expected)
            Check.bool
            ~error_msg:
              (Format.sprintf
                 "Lag %d, slot %d mismatch: expected %b"
                 lag_index
                 slot_index
                 expected))
        original_arr)
    attestations_per_lag ;

  Log.info "Multi-lag encode/decode succeeded" ;
  unit

let register ~__FILE__ ~protocols =
  scenario_with_layer1_node
    ~__FILE__
    "attesters receive expected DAL rewards depending on participation"
    test_dal_rewards_distribution
    (List.filter (fun p -> Protocol.number p >= 022) protocols)
    (* We set attestation threshold to 30% because we'll have 2 regular bakers
       who attest sufficiently. *)
    ~attestation_threshold:30
    ~attestation_lag:2
    ~blocks_per_cycle:16
    ~blocks_per_commitment:17 (* so that there's no nonce revelation required *) ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~attestation_threshold:1
    ~l1_history_mode:Default_with_refutation
    "Attester attests produced slot"
    test_producer_attester
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~attestation_threshold:1
    ~l1_history_mode:Default_with_refutation
    ~event_sections_levels:[("prevalidator", `Debug)]
    "attester_did_not_attest warning is emitted"
    test_attester_did_not_attest
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~traps_fraction:Q.zero
    ~number_of_slots:1
    "new attester attests"
    test_new_attester_attests
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~l1_history_mode:Default_with_refutation
    ~traps_fraction:(Q.of_float 0.5)
    ~number_of_slots:1
    "low stake attester attests (with traps)"
    test_dal_low_stake_attester_attestable_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~l1_history_mode:Default_with_refutation
    ~traps_fraction:(Q.of_float 0.5)
    ~all_bakers_attest_activation_threshold:Q.zero
    ~number_of_slots:1
    "low stake attester attests (with traps and ABA activated)"
    test_dal_low_stake_attester_attestable_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~number_of_slots:1
    "publish slot in one level reorganisation"
    test_dal_one_level_reorg
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~number_of_slots:1
    ~operator_profiles:[0]
    ~regression:true
    "attesters receive DAL rewards"
    test_attesters_receive_dal_rewards
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~activation_timestamp:Now
    "mockup get_attestable_slots"
    use_mockup_node_for_getting_attestable_slots
    protocols ;
  test_attestations_encode_decode_single_lag ~__FILE__ protocols ;
  test_attestations_encode_decode_multiple_lags ~__FILE__ protocols
