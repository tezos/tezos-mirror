(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Companion key client-baker integration
    Invocation:   dune exec tezt/tests/main.exe -- --file companion_key.ml
    Subject:      Test companion key registration and usage
*)

let team = Tag.layer1

let hooks =
  let replacements =
    ("edsig\\w{94}", "[SIGNATURE]")
    :: ("BLsig\\w{137}", "[BLS_SIGNATURE]")
    :: Tezos_regression.replacements
  in
  Tezos_regression.hooks_custom
    ~replace_variables:(fun s ->
      Tezos_regression.replace_variables ~replacements s)
    ()

let blocks_per_cycle = 4

let consensus_rights_delay = 1

open Consensus_key.Helpers

type companion_key = {
  active_companion_key : string option;
  (* Pending companion keys per cycle *)
  pending_companion_keys : (int * string) list;
}

let companion_key_typ : companion_key Check.typ =
  Check.(
    convert
      (fun {active_companion_key; pending_companion_keys} ->
        (active_companion_key, pending_companion_keys))
      (tuple2 (option string) (list (tuple2 int string))))

let get_validators_companion_key ?level ~protocol (delegate : Account.key)
    client =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_helper_validators
         ?level
         ~delegate:delegate.public_key_hash
         ()
  in
  let companion_key =
    if Protocol.number protocol >= 024 then
      JSON.(
        json |=> 0 |-> "delegates" |=> 0 |-> "companion_key" |> as_string_opt)
    else JSON.(json |=> 0 |-> "companion_key" |> as_string_opt)
  in
  Log.info
    ~color:Log.Color.FG.green
    "companion_key = %s"
    (match companion_key with Some ck -> ck | None -> "None") ;
  return companion_key

let check_validators_companion_key ~__LOC__ ?level ~protocol
    (delegate : Account.key) client ~expected =
  let* companion_key =
    get_validators_companion_key ?level ~protocol delegate client
  in
  Check.(
    (companion_key = expected)
      (option string)
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  unit

let get_companion_key ?block client (delegate : Account.key) :
    companion_key Lwt.t =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_delegate_companion_key
         ?block
         delegate.public_key_hash
  in
  return
    JSON.
      {
        active_companion_key =
          json |-> "active" |> as_opt
          |> Option.map (fun x -> x |-> "pkh" |> as_string);
        pending_companion_keys =
          json |-> "pendings" |> as_list
          |> List.map (fun pending_key ->
                 ( pending_key |-> "cycle" |> as_int,
                   pending_key |-> "pkh" |> as_string ));
      }

let check_companion_key ~__LOC__ delegate ?expected_active
    ?(expected_pending = []) client =
  let* companion_key = get_companion_key client delegate in
  let expected_active_pkh =
    Option.map (fun (x : Account.key) -> x.public_key_hash) expected_active
  in
  let expected_companion_key =
    {
      active_companion_key = expected_active_pkh;
      pending_companion_keys =
        List.map
          (fun (cycle, (account : Account.key)) ->
            (cycle, account.public_key_hash))
          expected_pending;
    }
  in
  Check.(
    (companion_key = expected_companion_key)
      companion_key_typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  unit

let init_node_and_client ~protocol =
  let parameters =
    (* we update parameters for faster testing: no need to wait
       2 cycles for the consensus key to activate. *)
    [
      (["blocks_per_cycle"], `Int blocks_per_cycle);
      (["nonce_revelation_threshold"], `Int 2);
      (["consensus_rights_delay"], `Int consensus_rights_delay);
      (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
      (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
      (["allow_tz4_delegate_enable"], `Bool true);
      (["aggregate_attestation"], `Bool true);
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (protocol, None)) parameters
  in
  let* node, client =
    Client.init_with_protocol ~parameter_file ~protocol `Client ()
  in
  return (node, client)

let check_active_companion_and_consensus_keys ~protocol delegate ~companion_key
    ~consensus_key client =
  Log.info "Checking keys are activated" ;
  let* () =
    Consensus_key.check_consensus_key
      ~__LOC__
      delegate
      ~expected_active:consensus_key
      client
  in
  let* () =
    check_companion_key ~__LOC__ delegate ~expected_active:companion_key client
  in
  let* () =
    check_validators_companion_key
      ~__LOC__
      ~protocol
      delegate
      ~expected:None
      client
  in
  let* current_level = get_current_level client in
  let* () =
    check_validators_companion_key
      ~__LOC__
      ~protocol
      ~level:(current_level.level + 1)
      delegate
      ~expected:(Some companion_key.public_key_hash)
      client
  in
  unit

let test_update_companion_key =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"update companion key"
    ~tags:[team; "companion_key"]
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _node, client = init_node_and_client ~protocol in
  let delegate = Constant.bootstrap1 in
  let* consensus_key_bls =
    Client.gen_and_show_keys ~alias:"consensus_key" ~sig_alg:"bls" client
  in
  let* companion_key_bls =
    Client.gen_and_show_keys ~alias:"companion_key" ~sig_alg:"bls" client
  in

  Log.info "Updating consensus key and companion key." ;
  let* () =
    Client.update_consensus_key
      ~hooks
      ~src:delegate.alias
      ~pk:consensus_key_bls.alias
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.update_companion_key
      ~hooks
      ~src:delegate.alias
      ~pk:companion_key_bls.alias
      client
  in
  let* () = Client.bake_for_and_wait client in

  Log.info "Waiting for consensus and companion keys activation" ;
  let* () = bake_n_cycles (consensus_rights_delay + 1) client in
  let* () =
    check_active_companion_and_consensus_keys
      ~protocol
      delegate
      ~companion_key:companion_key_bls
      ~consensus_key:consensus_key_bls
      client
  in
  unit

let test_update_companion_key_for_non_tz4_delegate =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"update companion key for non tz4 delegate"
    ~tags:[team; "companion_key"]
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _node, client = init_node_and_client ~protocol in
  let delegate = Constant.bootstrap1 in
  let* companion_key_bls =
    Client.gen_and_show_keys ~alias:"companion_key" ~sig_alg:"bls" client
  in
  Log.info "Updating companion key" ;
  let* () =
    Client.update_companion_key
      ~hooks
      ~src:delegate.alias
      ~pk:companion_key_bls.alias
      client
  in
  let* () = Client.bake_for_and_wait client in

  Log.info "Waiting for companion key activation" ;
  let* () = bake_n_cycles (consensus_rights_delay + 1) client in

  Log.info "Checking key is activated" ;
  let* () =
    check_companion_key
      ~__LOC__
      delegate
      ~expected_active:companion_key_bls
      client
  in
  let* () =
    check_validators_companion_key
      ~__LOC__
      ~protocol
      delegate
      ~expected:None
      client
  in
  let* current_level = get_current_level client in
  let* () =
    check_validators_companion_key
      ~__LOC__
      ~protocol
      ~level:(current_level.level + 1)
      delegate
      ~expected:None
      client
  in
  unit

let test_update_companion_key_for_tz4_delegate =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"update companion key for tz4 delegate"
    ~tags:[team; "companion_key"]
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _node, client = init_node_and_client ~protocol in
  (* gen keys delegate -s bls *)
  (* transfer 1000000 from bootstrap2 to delegate *)
  let* delegate =
    Client.gen_and_show_keys ~alias:"delegate" ~sig_alg:"bls" client
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1_000_000)
      ~giver:Constant.bootstrap2.alias
      ~receiver:delegate.public_key_hash
      client
  in
  let* () = Client.bake_for_and_wait client in
  (* set delegate for delegate to delegate *)
  let* () =
    Client.set_delegate ~src:delegate.alias ~delegate:delegate.alias client
  in
  let* () = Client.bake_for_and_wait client in
  (* stake 800000 for delegate *)
  let* () = Client.stake ~staker:delegate.alias (Tez.of_int 800_000) client in
  let* () = Client.bake_for_and_wait client in
  (* gen keys companion_key -s bls *)
  (* set companion key for delegate to companion_key *)
  let* companion_key_bls =
    Client.gen_and_show_keys ~alias:"companion_key" ~sig_alg:"bls" client
  in
  Log.info "Updating companion key" ;
  let* () =
    Client.update_companion_key
      ~hooks
      ~src:delegate.alias
      ~pk:companion_key_bls.alias
      client
  in
  let* () = Client.bake_for_and_wait client in

  Log.info "Waiting for companion key activation" ;
  let* () = bake_n_cycles (consensus_rights_delay + 1) client in
  let* () =
    check_active_companion_and_consensus_keys
      ~protocol
      delegate
      ~companion_key:companion_key_bls
      ~consensus_key:delegate
      client
  in
  unit

let test_update_companion_key_with_external_pop =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"update companion key with external pop"
    ~tags:[team; "companion_key"; "pop"]
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* node, client = init_node_and_client ~protocol in
  let delegate = Constant.bootstrap1 in
  (* gen keys companion_key -s bls *)
  (* set companion key for delegate to companion_key *)
  Log.info
    "Init second client, generate a BLS companion key and its proof of \
     possession" ;
  let* client2 = Client.init ~endpoint:(Node node) () in
  let* companion_key_bls =
    Client.gen_and_show_keys ~alias:"companion_key" ~sig_alg:"bls" client2
  in
  let* companion_key_pop =
    Client.create_bls_proof ~signer:companion_key_bls.alias client2
  in

  Log.info "Import the key on the first client" ;
  let* () =
    Client.import_public_key
      ~alias:"companion_key"
      ~public_key:companion_key_bls.public_key
      client
  in
  Log.info "Updating companion key without secret key nor proof possession" ;
  let* () =
    Client.update_companion_key
      ~hooks
      ~src:delegate.alias
      ~pk:companion_key_bls.alias
      ~expect_failure:true
      client
  in
  Log.info "Updating companion key with BLS proof of possession" ;
  let* () =
    Client.update_companion_key
      ~hooks
      ~src:delegate.alias
      ~pk:companion_key_bls.alias
      ~companion_key_pop
      client
  in
  unit

let test_register_keys_and_stake =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "register key as delegate with companion and consensus keys and stake"
    ~tags:[team; "companion_key"; "consensus_key"; "stake"]
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _node, client = init_node_and_client ~protocol in
  let* delegate = Client.gen_and_show_keys ~alias:"delegate" client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1_000_000)
      ~giver:Constant.bootstrap1.alias
      ~receiver:delegate.alias
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* consensus_key_bls =
    Client.gen_and_show_keys ~alias:"consensus_key" ~sig_alg:"bls" client
  in
  let* companion_key_bls =
    Client.gen_and_show_keys ~alias:"companion_key" ~sig_alg:"bls" client
  in
  let amount = Tez.of_int 900_000 in
  let* () =
    Client.register_key
      ~hooks
      ~consensus:consensus_key_bls.alias
      ~companion:companion_key_bls.alias
      ~amount
      delegate.alias
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* staked_balance =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_staked_balance
         delegate.public_key_hash
  in
  Check.((staked_balance = Tez.to_mutez amount) ~__LOC__ int)
    ~error_msg:"Expected staked balance %R to be equal to %L" ;

  Log.info "Waiting for consensus and companion keys activation" ;
  let* () = bake_n_cycles (consensus_rights_delay + 1) client in
  let* () =
    check_active_companion_and_consensus_keys
      ~protocol
      delegate
      ~companion_key:companion_key_bls
      ~consensus_key:consensus_key_bls
      client
  in
  unit

let test_register_keys_with_proofs_and_stake =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"register keys with proofs and stake"
    ~tags:[team; "companion_key"; "consensus_key"; "proof"; "stake"]
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _node, client = init_node_and_client ~protocol in
  let* delegate = Client.gen_and_show_keys ~alias:"delegate" client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1_000_000)
      ~giver:Constant.bootstrap1.alias
      ~receiver:delegate.alias
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* consensus_key_bls =
    Client.gen_and_show_keys ~alias:"consensus_key" ~sig_alg:"bls" client
  in
  let* consensus_pop =
    Client.create_bls_proof ~signer:consensus_key_bls.alias client
  in
  let* companion_key_bls =
    Client.gen_and_show_keys ~alias:"companion_key" ~sig_alg:"bls" client
  in
  let* companion_pop =
    Client.create_bls_proof ~signer:companion_key_bls.alias client
  in
  let amount = Tez.of_int 900_000 in
  Log.info "Registering a companion key with an invalid proof." ;
  let register_key_process =
    Client.spawn_register_key
      ~consensus:consensus_key_bls.alias
      ~consensus_pop
      ~companion:companion_key_bls.alias
      ~companion_pop:consensus_pop
      ~amount
      delegate.alias
      client
  in
  let* () =
    Process.check_error
      ~msg:
        (rex
           "Update to a BLS companion key ([^ ]+) contains an incorrect proof \
            of possession")
      register_key_process
  in
  Log.info "Registering consensus and companion keys with valid proofs." ;
  let* () =
    Client.register_key
      ~hooks
      ~consensus:consensus_key_bls.alias
      ~consensus_pop
      ~companion:companion_key_bls.alias
      ~companion_pop
      ~amount
      delegate.alias
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* staked_balance =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_staked_balance
         delegate.public_key_hash
  in
  Check.((staked_balance = Tez.to_mutez amount) ~__LOC__ int)
    ~error_msg:"Expected staked balance %R to be equal to %L" ;

  Log.info "Waiting for consensus and companion keys activation" ;
  let* () = bake_n_cycles (consensus_rights_delay + 1) client in
  let* () =
    check_active_companion_and_consensus_keys
      ~protocol
      delegate
      ~companion_key:companion_key_bls
      ~consensus_key:consensus_key_bls
      client
  in
  unit

let test_update_keys_with_proofs =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"update keys with proofs"
    ~tags:[team; "companion_key"; "consensus_key"; "proof"]
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _node, client = init_node_and_client ~protocol in
  let delegate = Constant.bootstrap1 in
  let* consensus_key_bls =
    Client.gen_and_show_keys ~alias:"consensus_key" ~sig_alg:"bls" client
  in
  let* consensus_key_pop =
    Client.create_bls_proof ~signer:consensus_key_bls.alias client
  in
  let* companion_key_bls =
    Client.gen_and_show_keys ~alias:"companion_key" ~sig_alg:"bls" client
  in
  let* companion_key_pop =
    Client.create_bls_proof ~signer:companion_key_bls.alias client
  in

  Log.info "Updating consensus and companion keys." ;
  let* () =
    Client.update_consensus_key
      ~hooks
      ~src:delegate.alias
      ~pk:consensus_key_bls.alias
      ~consensus_key_pop
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.update_companion_key
      ~hooks
      ~src:delegate.alias
      ~pk:companion_key_bls.alias
      ~companion_key_pop
      client
  in
  let* () = Client.bake_for_and_wait client in

  Log.info "Waiting for consensus and companion keys activation" ;
  let* () = bake_n_cycles (consensus_rights_delay + 1) client in
  let* () =
    check_active_companion_and_consensus_keys
      ~protocol
      delegate
      ~companion_key:companion_key_bls
      ~consensus_key:consensus_key_bls
      client
  in
  unit

let register ~protocols =
  test_update_companion_key protocols ;
  test_update_companion_key_for_non_tz4_delegate protocols ;
  test_update_companion_key_for_tz4_delegate protocols ;
  test_update_companion_key_with_external_pop protocols ;
  test_register_keys_and_stake protocols ;
  test_register_keys_with_proofs_and_stake protocols ;
  test_update_keys_with_proofs protocols
