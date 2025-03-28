(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    BLS Signature
    Invocation:   dune exec tezt/tests/main.exe -- --file bls_signature.ml
    Subject:      Test BLS Signatures for staking operations
*)

let team = Tag.layer1

(** Tags shared by all tests in this file. *)
let threshold_bls_tags = [team; "bls"; "staking"; "manager"]

module Local_helpers = struct
  let staking_parameters ~(delegate : Account.key) client =
    let pkh = delegate.public_key_hash in
    let* staking_parameters =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_delegate_active_staking_parameters pkh
    in
    let limit =
      JSON.(
        staking_parameters |-> "limit_of_staking_over_baking_millionth"
        |> as_int)
    in
    let edge =
      JSON.(
        staking_parameters |-> "edge_of_baking_over_staking_billionth" |> as_int)
    in
    Log.info
      ~color:Log.Color.FG.green
      "Delegate %s: limit = %d and edge = %d"
      delegate.alias
      limit
      edge ;
    return (limit, edge)

  let bake_for_n_cycles_and_wait ~baker ~blocks_per_cycle ~n client =
    Log.info ~color:Log.Color.FG.green "Wait for %d cycles." n ;
    let* () =
      Client.bake_for_and_wait
        ~count:(blocks_per_cycle * n)
        ~keys:[baker]
        client
    in
    unit

  (* [bake_for_consensus_rights_delay_and_wait] bakes full
     [consensus_rights_delay] cycles. Baking 1 extra cycle can be
     replaced by baking until the end of the current cycle. *)
  let bake_for_consensus_rights_delay_and_wait ~baker ~parameters
      ~(delegate : Account.key) client =
    let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
    let consensus_rights_delay =
      JSON.(get "consensus_rights_delay" parameters |> as_int)
    in
    let* () =
      bake_for_n_cycles_and_wait
        ~baker
        ~blocks_per_cycle
        ~n:(consensus_rights_delay + 1)
        client
    in
    Log.info ~color:Log.Color.FG.green "Bake with %s." delegate.alias ;
    let* () = Client.bake_for_and_wait ~keys:[delegate.alias] client in
    unit

  let accept_external_stakers_and_bake_until_activation ~baker ~parameters
      ~(delegate : Account.key) client =
    Log.info
      ~color:Log.Color.FG.green
      "Update delegate parameters and bake cycles until the delegate accepts \
       external stakers." ;
    let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
    let delegate_parameters_activation_delay =
      JSON.(get "delegate_parameters_activation_delay" parameters |> as_int)
    in
    let* _ = staking_parameters ~delegate client in
    let set_delegate_parameters =
      Client.spawn_set_delegate_parameters
        ~delegate:delegate.alias
        ~limit:"4"
        ~edge:"0.05"
        client
    in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    let* () = set_delegate_parameters |> Process.check in
    let* () =
      bake_for_n_cycles_and_wait
        ~baker
        ~blocks_per_cycle
        ~n:(delegate_parameters_activation_delay + 1)
        client
    in
    let* _ = staking_parameters ~delegate client in
    unit

  let gen_and_show_keys ~alias ?sig_alg client =
    let* account = Client.gen_and_show_keys ~alias ?sig_alg client in
    Log.info
      ~color:Log.Color.FG.green
      "Create an account for %s with pkh = %s."
      alias
      account.public_key_hash ;
    return account

  let transfer ~baker ~amount ~giver ~receiver client =
    Log.info
      ~color:Log.Color.FG.green
      "Transfer %s tez from %s to %s."
      (Tez.to_string amount)
      giver
      receiver ;
    let* () =
      Client.transfer ~burn_cap:Tez.one ~amount ~giver ~receiver client
    in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    unit

  let create_and_fund_account ~baker ~giver ~alias ~amount ?sig_alg client =
    let* account = gen_and_show_keys ~alias ?sig_alg client in
    let* () = transfer ~baker ~amount ~giver ~receiver:account.alias client in
    return account

  let set_delegate ~baker ~src ~(delegate : Account.key) client =
    Log.info
      ~color:Log.Color.FG.green
      "Set delegate for %s to %s."
      src
      delegate.alias ;
    let*! () = Client.set_delegate ~src ~delegate:delegate.alias client in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    unit

  let stake ~baker ~amount ~staker client =
    Log.info
      ~color:Log.Color.FG.green
      "Staker %s stakes %s tez."
      staker
      (Tez.to_string amount) ;
    let* () = Client.stake amount ~staker client in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    unit

  let mk_op_reveal (new_account : Account.key) client =
    Log.info
      ~color:Log.Color.FG.green
      "Reveal a public key of %s with pkh = %s."
      new_account.alias
      new_account.public_key_hash ;
    let module M = Operation.Manager in
    let op_manager =
      M.make ~gas_limit:1800 ~source:new_account @@ M.reveal new_account
    in
    let* op = M.operation [op_manager] client in
    return op

  let mk_op_set_delegate ~(src : Account.key) ~(delegate : Account.key) client =
    Log.info
      ~color:Log.Color.FG.green
      "Set delegate for %s to %s."
      src.alias
      delegate.alias ;
    let module M = Operation.Manager in
    let op_manager =
      M.make ~gas_limit:1800 ~source:src @@ M.delegation ~delegate ()
    in
    let* op = M.operation [op_manager] client in
    return op

  let mk_op_stake ~(staker : Account.key) ~amount client =
    Log.info
      ~color:Log.Color.FG.green
      "Staker %s stakes %s tez."
      staker.alias
      (Tez.to_string amount) ;
    let module M = Operation.Manager in
    let op_manager =
      M.make ~gas_limit:5300 ~source:staker
      @@ M.call
           ~dest:staker.public_key_hash
           ~amount:(Tez.to_mutez amount)
           ~entrypoint:"stake"
           ()
    in
    let* op = M.operation [op_manager] client in
    return op

  let mk_op_update_consensus_key ~(delegate_consensus_key : Account.key)
      ~(delegate : Account.key) ?proof client =
    Log.info
      ~color:Log.Color.FG.green
      "Set consensus key for %s to %s."
      delegate.alias
      delegate_consensus_key.alias ;
    let module M = Operation.Manager in
    let op_manager =
      M.make ~gas_limit:5300 ~source:delegate
      @@ M.update_consensus_key
           ~public_key:delegate_consensus_key.public_key
           ?proof
           ()
    in
    let* op = M.operation [op_manager] client in
    return op

  let create_proof ~(signer : Account.key) =
    let public_key =
      Tezos_crypto.Signature.Public_key.of_b58check_exn signer.public_key
    in
    let msg =
      Data_encoding.Binary.to_bytes_exn
        Tezos_crypto.Signature.Public_key.encoding
        public_key
    in
    Account.sign_bytes ~signer msg |> Tezos_crypto.Signature.to_b58check

  let inject_bls_sign_op ~baker ~(signer : Account.key) (op : Operation.t)
      client =
    let* op_hex = Operation.hex op client in
    let signature =
      Account.sign_bytes
        ~watermark:Generic_operation
        ~signer
        (Hex.to_bytes op_hex)
    in
    let* (`OpHash op_hash) = Operation.inject ~signature op client in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    let* receipt = Client.get_receipt_for ~operation:op_hash client in
    Log.info ~color:Log.Color.FG.gray "receipt for %s:\n%s" op_hash receipt ;
    return op_hash

  let print_parameters parameters =
    let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
    let consensus_rights_delay =
      JSON.(get "consensus_rights_delay" parameters |> as_int)
    in
    Log.info
      ~color:Log.Color.FG.green
      "blocks_per_cycle = %d, consensus_rights_delay = %d"
      blocks_per_cycle
      consensus_rights_delay

  let init_node_and_client ~protocol =
    let parameter_file = Protocol.parameter_file protocol in
    let parameters = JSON.parse_file parameter_file in
    let () = print_parameters parameters in
    Log.info ~color:Log.Color.FG.green "Initialize a node and a client." ;
    let* _node, client =
      Client.init_with_protocol ~parameter_file ~protocol `Client ()
    in
    (* [default_baker] never gets deactivated *)
    let default_baker = Constant.bootstrap1.alias in
    (* [funder] is used to fund all new accounts *)
    let funder = Constant.bootstrap2.alias in
    return (parameters, client, default_baker, funder)

  let init_node_and_client_with_external_delegate ~protocol =
    let* parameters, client, default_baker, funder =
      init_node_and_client ~protocol
    in
    (* Update delegate parameters and wait for their activation *)
    let delegate = Constant.bootstrap3 in
    let* () =
      accept_external_stakers_and_bake_until_activation
        ~baker:default_baker
        ~parameters
        ~delegate
        client
    in
    return (parameters, client, default_baker, funder, delegate)

  let get_staked_balance (contract : Account.key) client =
    let* staked_balance =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_staked_balance
           contract.public_key_hash
    in
    Log.info
      ~color:Log.Color.FG.green
      "Contract %s: staked_balance = %d"
      contract.alias
      staked_balance ;
    return staked_balance

  (* The baking rewards for staked tez are automatically shared
     between a baker and its stakers. So, [staker]'s staked balance is
     increased when its [baker] bakes a block, if the following is
     valid:
     - [staker]'s [staked_balance] <> 0
     - for external delegate:
       - [limit_of_staking_over_baking] <> 0
       - [edge_of_baking_over_staking_billionth] <> 1. *)
  let check_staked_balance_increase_when_baking ~(baker : Account.key) ~staker
      client =
    let* staked_balance_before = get_staked_balance staker client in
    Log.info ~color:Log.Color.FG.green "Bake with %s." baker.alias ;
    let* () = Client.bake_for_and_wait ~keys:[baker.alias] client in
    let* staked_balance_after = get_staked_balance staker client in
    Check.((staked_balance_before < staked_balance_after) ~__LOC__ int)
      ~error_msg:"Expected staked balance %R to be greater than %L" ;
    unit
end

let test_single_staker_sign_staking_operation_self_delegate =
  Protocol.register_test
    ~__FILE__
    ~title:"single staker signs a staking operation with a self delegate"
    ~tags:(threshold_bls_tags @ ["single"; "self_delegate"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* parameters, client, default_baker, funder =
    Local_helpers.init_node_and_client ~protocol
  in
  (* gen keys delegate -s bls *)
  (* transfer 150000 from bootstrap2 to delegate *)
  let* delegate =
    Local_helpers.create_and_fund_account
      ~baker:default_baker
      ~giver:funder
      ~alias:"delegate"
      ~amount:(Tez.of_int 150_000)
      ~sig_alg:"bls"
      client
  in
  (* set delegate for delegate to delegate *)
  (* [delegate] registers as a self-delegate/baker. *)
  let* () =
    Local_helpers.set_delegate
      ~baker:default_baker
      ~src:delegate.alias
      ~delegate
      client
  in
  (* stake 140000 for delegate *)
  (* To receive baking rights, [delegate] must stake at least
     [minimal_frozen_stake] and its baking power must be at least
     [minimal_stake]. *)
  let* () =
    Local_helpers.stake
      ~baker:default_baker
      ~staker:delegate.alias
      ~amount:(Tez.of_int 140_000)
      client
  in
  (* [delegate] cannot bake during the current and next full
     [consensus_rights_delay] cycles, as it has no baking rights. So,
     we wait for the cycle when we can bake with [delegate]. *)
  let* () =
    Local_helpers.bake_for_consensus_rights_delay_and_wait
      ~baker:default_baker
      ~parameters
      ~delegate
      client
  in
  (* [delegate] bakes a next block, so its staked balance is increased
     due to the baking rewards. *)
  let* () =
    Local_helpers.check_staked_balance_increase_when_baking
      ~baker:delegate
      ~staker:delegate
      client
  in
  unit

let test_single_staker_sign_staking_operation_external_delegate =
  Protocol.register_test
    ~__FILE__
    ~title:"single staker signs a staking operation with an external delegate"
    ~tags:(threshold_bls_tags @ ["single"; "external_delegate"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _parameters, client, default_baker, funder, delegate =
    Local_helpers.init_node_and_client_with_external_delegate ~protocol
  in
  (* gen keys staker -s bls *)
  (* transfer 150000 from bootstrap2 to staker *)
  let* staker =
    Local_helpers.create_and_fund_account
      ~baker:default_baker
      ~giver:funder
      ~alias:"staker"
      ~amount:(Tez.of_int 150_000)
      ~sig_alg:"bls"
      client
  in
  (* set delegate for staker to delegate *)
  (* [staker] chooses an external delegate/baker. *)
  let* () =
    Local_helpers.set_delegate
      ~baker:default_baker
      ~src:staker.alias
      ~delegate
      client
  in
  (* stake 140000 for staker *)
  (* [staker] stakes with a baker to get staking rewards, for example,
     when its baker bakes a block. *)
  let* () =
    Local_helpers.stake
      ~baker:default_baker
      ~staker:staker.alias
      ~amount:(Tez.of_int 140_000)
      client
  in
  (* [staker]'s baker bakes a next block, so [staker]'s staked balance
     is increased due to the baking rewards. The change happens
     immediately without waiting for [consensus_rights_delay]
     cycles. *)
  let* () =
    Local_helpers.check_staked_balance_increase_when_baking
      ~baker:delegate
      ~staker
      client
  in
  unit

let test_single_staker_sign_staking_operation_consensus_key =
  Protocol.register_test
    ~__FILE__
    ~title:"single staker signs a staking operation with a consensus key"
    ~tags:(threshold_bls_tags @ ["single"; "consensus_key"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* parameters, client, default_baker, funder =
    Local_helpers.init_node_and_client ~protocol
  in
  (* gen keys delegate -s bls *)
  (* transfer 150000 from bootstrap2 to delegate *)
  let* delegate =
    Local_helpers.create_and_fund_account
      ~baker:default_baker
      ~giver:funder
      ~alias:"delegate"
      ~amount:(Tez.of_int 150_000)
      ~sig_alg:"bls"
      client
  in
  (* gen keys delegate_consensus_key -s bls *)
  let* delegate_consensus_key =
    Local_helpers.gen_and_show_keys
      ~alias:"delegate_consensus_key"
      ~sig_alg:"bls"
      client
  in
  (* register key delegate as delegate with consensus key delegate_consensus_key *)
  (* [delegate] registers as a self-delegate/baker with a consensus
     key [delegate_consensus_key] for consensus operations. *)
  Log.info
    ~color:Log.Color.FG.green
    "Register key %s as delegate with consensus key %s."
    delegate.alias
    delegate_consensus_key.alias ;
  let* () =
    Client.register_key
      ~consensus:delegate_consensus_key.alias
      delegate.alias
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[default_baker] client in
  (* stake 140000 for delegate *)
  (* To receive baking rights, [delegate] must stake at least
     [minimal_frozen_stake] and its baking power must be at least
     [minimal_stake]. *)
  let* () =
    Local_helpers.stake
      ~baker:default_baker
      ~staker:delegate.alias
      ~amount:(Tez.of_int 140_000)
      client
  in
  (* a consensus key [delegate_consensus_key] becomes active in
     [consensus_rights_delay] cycles. *)
  let* () =
    Local_helpers.bake_for_consensus_rights_delay_and_wait
      ~baker:default_baker
      ~parameters
      ~delegate:delegate_consensus_key
      client
  in
  (* [delegate] bakes a next block with a consensus key
     [delegate_consensus_key], so its staked balance is increased due
     to the baking rewards. *)
  let* () =
    Local_helpers.check_staked_balance_increase_when_baking
      ~baker:delegate_consensus_key
      ~staker:delegate
      client
  in
  unit

let test_single_staker_sign_staking_operation_consensus_key_op_core =
  Protocol.register_test
    ~__FILE__
    ~title:
      "single staker signs a staking operation with a consensus key \
       (operation_core)"
    ~tags:(threshold_bls_tags @ ["single"; "consensus_key"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* parameters, client, default_baker, funder =
    Local_helpers.init_node_and_client ~protocol
  in
  (* gen keys delegate -s bls *)
  let* delegate =
    Local_helpers.gen_and_show_keys ~alias:"delegate" ~sig_alg:"bls" client
  in
  (* transfer 150000 from bootstrap2 to delegate *)
  let* () =
    Local_helpers.transfer
      ~baker:default_baker
      ~amount:(Tez.of_int 150_000)
      ~giver:funder
      ~receiver:delegate.alias
      client
  in
  (* reveal key for delegate *)
  let* op_reveal = Local_helpers.mk_op_reveal delegate client in
  let* _op_hash =
    Local_helpers.inject_bls_sign_op
      ~baker:default_baker
      ~signer:delegate
      op_reveal
      client
  in
  (* set delegate for delegate to delegate *)
  let* op_set_delegate =
    Local_helpers.mk_op_set_delegate ~src:delegate ~delegate client
  in
  let* _op_hash =
    Local_helpers.inject_bls_sign_op
      ~baker:default_baker
      ~signer:delegate
      op_set_delegate
      client
  in
  (* stake 140000 for delegate *)
  let* op_stake =
    Local_helpers.mk_op_stake
      ~staker:delegate
      ~amount:(Tez.of_int 140_000)
      client
  in
  let* _op_hash =
    Local_helpers.inject_bls_sign_op
      ~baker:default_baker
      ~signer:delegate
      op_stake
      client
  in
  (* gen keys delegate_consensus_key -s bls *)
  let* delegate_consensus_key =
    Local_helpers.gen_and_show_keys
      ~alias:"delegate_consensus_key"
      ~sig_alg:"bls"
      client
  in
  (* create a proof for a consensus key *)
  let proof = Local_helpers.create_proof ~signer:delegate_consensus_key in
  (* set consensus key for delegate to delegate_consensus_key *)
  let* op_update_consensus_key =
    Local_helpers.mk_op_update_consensus_key
      ~delegate
      ~delegate_consensus_key
      ~proof
      client
  in
  let* _op_hash =
    Local_helpers.inject_bls_sign_op
      ~baker:default_baker
      ~signer:delegate
      op_update_consensus_key
      client
  in
  let* () =
    Local_helpers.bake_for_consensus_rights_delay_and_wait
      ~baker:default_baker
      ~parameters
      ~delegate:delegate_consensus_key
      client
  in
  let* () =
    Local_helpers.check_staked_balance_increase_when_baking
      ~baker:delegate_consensus_key
      ~staker:delegate
      client
  in
  unit

let register ~protocols =
  test_single_staker_sign_staking_operation_self_delegate protocols ;
  test_single_staker_sign_staking_operation_external_delegate protocols ;
  test_single_staker_sign_staking_operation_consensus_key protocols ;
  test_single_staker_sign_staking_operation_consensus_key_op_core protocols
