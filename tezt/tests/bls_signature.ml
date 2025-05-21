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

type kind = Client | RPC

let kind_to_string x = match x with Client -> "Client" | RPC -> "RPC"

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

  let mk_reveal_delegation_stake_update_ck_in_batch ~(delegate : Account.key)
      ~amount ?(delegate_consensus_key : Account.key option) ?proof client =
    let module M = Operation.Manager in
    let* counter = M.get_next_counter client ~source:delegate in
    let reveal = M.reveal delegate in
    let delegation = M.delegation ~delegate () in
    let stake =
      M.call
        ~dest:delegate.public_key_hash
        ~amount:(Tez.to_mutez amount)
        ~entrypoint:"stake"
        ()
    in
    let update_ck =
      match delegate_consensus_key with
      | Some delegate_ck ->
          let update_ck =
            M.update_consensus_key ~public_key:delegate_ck.public_key ?proof ()
          in
          [update_ck]
      | None -> []
    in
    let ops =
      M.make_batch
        ~source:delegate
        ~gas_limit:5300
        ~counter
        ([reveal; delegation; stake] @ update_ck)
    in
    let* op = M.operation ops client in
    return op

  let create_proof ~(signer : Account.key) =
    let b58_secret_key =
      Account.require_unencrypted_secret_key ~__LOC__ signer.secret_key
    in
    let secret_key =
      Tezos_crypto.Signature.Secret_key.of_b58check_exn b58_secret_key
    in
    match secret_key with
    | Bls sk ->
        let proof = Tezos_crypto.Signature.Bls.pop_prove sk in
        Tezos_crypto.Signature.of_bytes_exn proof
        |> Tezos_crypto.Signature.to_b58check
    | _ -> Test.fail "Proof-of-Possession is only required for BLS keys"

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

  let create_bls_proofs ~(signers : Account.key list) client =
    Lwt_list.map_s
      (fun (signer : Account.key) ->
        let* proof = Client.create_bls_proof ~signer:signer.alias client in
        return (signer.public_key, proof))
      signers

  let check_bls_proofs ~kind client pk_with_proofs =
    match kind with
    | Client ->
        Lwt_list.iter_s
          (fun (pk, proof) -> Client.check_bls_proof ~pk ~proof client)
          pk_with_proofs
    | RPC ->
        Lwt_list.iter_s
          (fun (pk, proof) ->
            let* is_valid =
              Client.RPC.call client @@ RPC.post_bls_check_proof ~pk ~proof ()
            in
            let () = Assert.is_true is_valid in
            unit)
          pk_with_proofs

  let aggregate_bls_public_keys ~kind client pk_with_proofs =
    (* Just to test the [check_proof] client command/RPC *)
    let* () = check_bls_proofs ~kind client pk_with_proofs in
    match kind with
    | Client -> Client.aggregate_bls_public_keys client pk_with_proofs
    | RPC ->
        Client.RPC.call client
        @@ RPC.post_bls_aggregate_public_keys pk_with_proofs

  let mk_fake_account_from_bls_pk ~bls_pk ~bls_pkh ~alias : Account.key =
    Log.info
      ~color:Log.Color.FG.green
      "Create a fake account for %s with pkh = %s."
      alias
      bls_pkh ;
    Account.
      {
        alias;
        public_key_hash = bls_pkh;
        public_key = bls_pk;
        secret_key = Encrypted "";
      }

  let mk_account_from_bls_sk ~bls_sk ~alias =
    let sk = Tezos_crypto.Signature.Bls.Secret_key.of_b58check_exn bls_sk in
    let pk = Tezos_crypto.Signature.Bls.Secret_key.to_public_key sk in
    let pkh =
      Tezos_crypto.Signature.Bls.Public_key.hash pk
      |> Tezos_crypto.Signature.Bls.Public_key_hash.to_b58check
    in
    let public_key = Tezos_crypto.Signature.Bls.Public_key.to_b58check pk in
    let secret_key = Account.Unencrypted bls_sk in
    Log.info
      ~color:Log.Color.FG.green
      "Create an account for %s with pkh = %s."
      alias
      pkh ;
    Account.{alias; public_key_hash = pkh; public_key; secret_key}

  let bls_sk_to_b58_string (sk : Bls12_381_signature.sk) =
    Tezos_crypto.Signature.Bls sk
    |> Tezos_crypto.Signature.Secret_key.to_b58check

  let sign_and_aggregate_signatures ~kind ~watermark
      ~(signers : Account.key list) (msg : bytes) client =
    let signatures =
      List.map
        (fun signer ->
          Account.sign_bytes ~watermark ~signer msg
          |> Tezos_crypto.Signature.to_b58check)
        signers
    in
    match kind with
    | Client -> Client.aggregate_bls_signatures client signatures
    | RPC ->
        Client.RPC.call client @@ RPC.post_bls_aggregate_signatures signatures

  let sign_and_recover_threshold_signature ~kind ~watermark
      ~(signers : (int * Account.key) list) (msg : bytes) client =
    let signatures =
      List.map
        (fun (id, signer) ->
          let signature =
            Account.sign_bytes ~watermark ~signer msg
            |> Tezos_crypto.Signature.to_b58check
          in
          (id, signature))
        signers
    in
    match kind with
    | Client -> Client.threshold_bls_signatures client signatures
    | RPC ->
        Client.RPC.call client @@ RPC.post_bls_threshold_signatures signatures

  let inject_bls_group_sign_op ~baker ~group_signature (op : Operation.t) client
      =
    let group_signature =
      Tezos_crypto.Signature.of_b58check_exn group_signature
    in
    (* inject an operation signed by a group *)
    let* (`OpHash op_hash) =
      Operation.inject ~signature:group_signature op client
    in
    let* () = Client.bake_for_and_wait ~keys:[baker] client in
    let* receipt = Client.get_receipt_for ~operation:op_hash client in
    Log.info ~color:Log.Color.FG.gray "receipt for %s:\n%s" op_hash receipt ;
    return op_hash

  let inject_aggregate_bls_sign_op ~kind ~baker ~(signers : Account.key list)
      (op : Operation.t) client =
    let* op_hex = Operation.hex op client in
    let manager_op = Hex.to_bytes op_hex in
    let* group_signature =
      sign_and_aggregate_signatures
        ~kind
        ~watermark:Generic_operation
        ~signers
        manager_op
        client
    in
    inject_bls_group_sign_op ~baker ~group_signature op client

  let inject_threshold_bls_sign_op ~kind ~baker
      ~(signers : (int * Account.key) list) (op : Operation.t) client =
    let* op_hex = Operation.hex op client in
    let manager_op = Hex.to_bytes op_hex in
    let* group_signature =
      sign_and_recover_threshold_signature
        ~kind
        ~watermark:Generic_operation
        ~signers
        manager_op
        client
    in
    inject_bls_group_sign_op ~baker ~group_signature op client

  let create_accounts_from_master_sk ~sk ~m ~n client =
    if not (1 < m && m <= n) then
      Test.fail "Invalid parameters for N = %d and M = %d" n m ;
    let* group_pk, group_pkh, secret_shares =
      Client.share_bls_secret_key ~sk ~n:5 ~m:3 client
    in
    let stakers =
      List.map
        (fun (id, bls_sk) ->
          ( id,
            mk_account_from_bls_sk ~bls_sk ~alias:("staker_" ^ string_of_int id)
          ))
        secret_shares
    in
    return (group_pk, group_pkh, stakers)

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

(* N = group size and M = threshold *)

(** Simple case: N = M = 3 *)
let test_all_stakers_sign_staking_operation_external_delegate ~kind =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "all stakers sign a staking operation with an external delegate (%s)"
         (kind_to_string kind))
    ~tags:(threshold_bls_tags @ ["multiple"; "external_delegate"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _parameters, client, default_baker, funder, delegate =
    Local_helpers.init_node_and_client_with_external_delegate ~protocol
  in
  (* gen keys staker_i -s bls *)
  let stakers = List.init 3 (fun i -> "staker_" ^ Int.to_string (i + 1)) in
  let* accounts =
    Lwt_list.map_s
      (fun alias ->
        Local_helpers.gen_and_show_keys ~alias ~sig_alg:"bls" client)
      stakers
  in
  (* Create bls proofs *)
  let* pks_with_proofs =
    Local_helpers.create_bls_proofs ~signers:accounts client
  in
  (* aggregate bls public keys *)
  let* group_pk_bls, group_pkh_bls =
    Local_helpers.aggregate_bls_public_keys ~kind client pks_with_proofs
  in
  let group_staker =
    Local_helpers.mk_fake_account_from_bls_pk
      ~bls_pk:group_pk_bls
      ~bls_pkh:group_pkh_bls
      ~alias:"group_staker"
  in
  (* transfer 150000 from bootstrap2 to group_staker *)
  let* () =
    Local_helpers.transfer
      ~baker:default_baker
      ~amount:(Tez.of_int 150_000)
      ~giver:funder
      ~receiver:group_staker.public_key_hash
      client
  in
  (* reveal key for group_staker *)
  let* op_reveal = Local_helpers.mk_op_reveal group_staker client in
  let* _op_hash =
    Local_helpers.inject_aggregate_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers:accounts
      op_reveal
      client
  in
  (* set delegate for group_staker to delegate *)
  let* op_set_delegate =
    Local_helpers.mk_op_set_delegate ~src:group_staker ~delegate client
  in
  let* _op_hash =
    Local_helpers.inject_aggregate_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers:accounts
      op_set_delegate
      client
  in
  (* stake 140000 for group_staker *)
  let* op_stake =
    Local_helpers.mk_op_stake
      ~staker:group_staker
      ~amount:(Tez.of_int 140_000)
      client
  in
  let* _op_hash =
    Local_helpers.inject_aggregate_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers:accounts
      op_stake
      client
  in
  let* () =
    Local_helpers.check_staked_balance_increase_when_baking
      ~baker:delegate
      ~staker:group_staker
      client
  in
  unit

(** Simple case: N = M = 3 *)
let test_all_stakers_sign_staking_operation_consensus_key ~kind =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "all stakers sign a staking operation with a consensus key (%s)"
         (kind_to_string kind))
    ~tags:(threshold_bls_tags @ ["multiple"; "consensus_key"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* parameters, client, default_baker, funder =
    Local_helpers.init_node_and_client ~protocol
  in
  (* gen keys staker_i -s bls *)
  let stakers = List.init 3 (fun i -> "staker_" ^ Int.to_string (i + 1)) in
  let* accounts =
    Lwt_list.map_s
      (fun alias ->
        Local_helpers.gen_and_show_keys ~alias ~sig_alg:"bls" client)
      stakers
  in
  (* Create bls proofs *)
  let* pks_with_proofs =
    Local_helpers.create_bls_proofs ~signers:accounts client
  in
  (* aggregate bls public keys *)
  let* group_pk_bls, group_pkh_bls =
    Local_helpers.aggregate_bls_public_keys ~kind client pks_with_proofs
  in
  let group_staker =
    Local_helpers.mk_fake_account_from_bls_pk
      ~bls_pk:group_pk_bls
      ~bls_pkh:group_pkh_bls
      ~alias:"group_staker"
  in
  (* transfer 150000 from bootstrap2 to group_staker *)
  let* () =
    Local_helpers.transfer
      ~baker:default_baker
      ~amount:(Tez.of_int 150_000)
      ~giver:funder
      ~receiver:group_staker.public_key_hash
      client
  in
  (* reveal key for group_staker *)
  let* op_reveal = Local_helpers.mk_op_reveal group_staker client in
  let* _op_hash =
    Local_helpers.inject_aggregate_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers:accounts
      op_reveal
      client
  in
  (* set delegate for group_staker to group_staker *)
  let* op_set_delegate =
    Local_helpers.mk_op_set_delegate
      ~src:group_staker
      ~delegate:group_staker
      client
  in
  let* _op_hash =
    Local_helpers.inject_aggregate_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers:accounts
      op_set_delegate
      client
  in
  (* stake 140000 for group_staker *)
  let* op_stake =
    Local_helpers.mk_op_stake
      ~staker:group_staker
      ~amount:(Tez.of_int 140_000)
      client
  in
  let* _op_hash =
    Local_helpers.inject_aggregate_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers:accounts
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
  let* proof =
    Client.create_bls_proof ~signer:delegate_consensus_key.alias client
  in
  (* set consensus key for group_staker to delegate_consensus_key *)
  let* op_update_consensus_key =
    Local_helpers.mk_op_update_consensus_key
      ~delegate:group_staker
      ~delegate_consensus_key
      ~proof
      client
  in
  let* _op_hash =
    Local_helpers.inject_aggregate_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers:accounts
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
      ~staker:group_staker
      client
  in
  unit

(** Simple case: N = M = 3 *)
let test_all_stakers_sign_staking_operation_consensus_key_batch ~kind =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "all stakers sign a staking operation with a consensus key in batch \
          (%s)"
         (kind_to_string kind))
    ~tags:(threshold_bls_tags @ ["multiple"; "consensus_key"; "batch"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* parameters, client, default_baker, funder =
    Local_helpers.init_node_and_client ~protocol
  in
  (* gen keys staker_i -s bls *)
  let stakers = List.init 3 (fun i -> "staker_" ^ Int.to_string (i + 1)) in
  let* accounts =
    Lwt_list.map_s
      (fun alias ->
        Local_helpers.gen_and_show_keys ~alias ~sig_alg:"bls" client)
      stakers
  in
  (* Create bls proofs *)
  let* pks_with_proofs =
    Local_helpers.create_bls_proofs ~signers:accounts client
  in
  (* aggregate bls public keys *)
  let* group_pk_bls, group_pkh_bls =
    Local_helpers.aggregate_bls_public_keys ~kind client pks_with_proofs
  in
  let group_staker =
    Local_helpers.mk_fake_account_from_bls_pk
      ~bls_pk:group_pk_bls
      ~bls_pkh:group_pkh_bls
      ~alias:"group_staker"
  in
  (* transfer 150000 from bootstrap2 to group_staker *)
  let* () =
    Local_helpers.transfer
      ~baker:default_baker
      ~amount:(Tez.of_int 150_000)
      ~giver:funder
      ~receiver:group_staker.public_key_hash
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
  let* proof =
    Client.create_bls_proof ~signer:delegate_consensus_key.alias client
  in

  (* reveal key for group_staker *)
  (* set delegate for group_staker to group_staker *)
  (* stake 140000 for group_staker *)
  (* set consensus key for group_staker to delegate_consensus_key *)
  let* op_batch =
    Local_helpers.mk_reveal_delegation_stake_update_ck_in_batch
      ~delegate:group_staker
      ~delegate_consensus_key
      ~proof
      ~amount:(Tez.of_int 140_000)
      client
  in
  let* _op_hash =
    Local_helpers.inject_aggregate_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers:accounts
      op_batch
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
      ~staker:group_staker
      client
  in
  unit

(** N = 5 and M = 3 *)
let test_threshold_number_stakers_sign_staking_operation_external_delegate ~kind
    =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "threshold number of stakers sign a staking operation with an \
          external delegate (%s)"
         (kind_to_string kind))
    ~tags:(threshold_bls_tags @ ["threshold"; "external_delegate"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* _parameters, client, default_baker, funder, delegate =
    Local_helpers.init_node_and_client_with_external_delegate ~protocol
  in
  let master_sk =
    Bls12_381_signature.generate_sk @@ Tezos_hacl.Hacl.Rand.gen 32
    |> Local_helpers.bls_sk_to_b58_string
  in
  (* Shamir's Secret Sharing *)
  let* group_pk_bls, group_pkh_bls, stakers =
    Local_helpers.create_accounts_from_master_sk ~sk:master_sk ~m:3 ~n:5 client
  in
  let group_staker =
    Local_helpers.mk_fake_account_from_bls_pk
      ~bls_pk:group_pk_bls
      ~bls_pkh:group_pkh_bls
      ~alias:"group_staker"
  in
  (* transfer 150000 from bootstrap2 to group_staker *)
  let* () =
    Local_helpers.transfer
      ~baker:default_baker
      ~amount:(Tez.of_int 150_000)
      ~giver:funder
      ~receiver:group_staker.public_key_hash
      client
  in

  let signers = List.filteri (fun i _s -> i < 3) stakers in
  (* reveal key for group_staker *)
  let* op_reveal = Local_helpers.mk_op_reveal group_staker client in
  let* _op_hash =
    Local_helpers.inject_threshold_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers
      op_reveal
      client
  in
  (* set delegate for group_staker to delegate *)
  let* op_set_delegate =
    Local_helpers.mk_op_set_delegate ~src:group_staker ~delegate client
  in
  let* _op_hash =
    Local_helpers.inject_threshold_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers
      op_set_delegate
      client
  in
  (* stake 140000 for group_staker *)
  let* op_stake =
    Local_helpers.mk_op_stake
      ~staker:group_staker
      ~amount:(Tez.of_int 140_000)
      client
  in
  let* _op_hash =
    Local_helpers.inject_threshold_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers
      op_stake
      client
  in
  let* () =
    Local_helpers.check_staked_balance_increase_when_baking
      ~baker:delegate
      ~staker:group_staker
      client
  in
  unit

(** N = 5 and M = 3 *)
let test_threshold_number_stakers_sign_staking_operation_consensus_key ~kind =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "threshold number of stakers sign a staking operation with a \
          consensus key (%s)"
         (kind_to_string kind))
    ~tags:(threshold_bls_tags @ ["threshold"; "consensus_key"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* parameters, client, default_baker, funder =
    Local_helpers.init_node_and_client ~protocol
  in
  let master_sk =
    Bls12_381_signature.generate_sk @@ Tezos_hacl.Hacl.Rand.gen 32
    |> Local_helpers.bls_sk_to_b58_string
  in
  (* Shamir's Secret Sharing *)
  let* group_pk_bls, group_pkh_bls, stakers =
    Local_helpers.create_accounts_from_master_sk ~sk:master_sk ~m:3 ~n:5 client
  in
  let group_staker =
    Local_helpers.mk_fake_account_from_bls_pk
      ~bls_pk:group_pk_bls
      ~bls_pkh:group_pkh_bls
      ~alias:"group_staker"
  in
  (* transfer 150000 from bootstrap2 to group_staker *)
  let* () =
    Local_helpers.transfer
      ~baker:default_baker
      ~amount:(Tez.of_int 150_000)
      ~giver:funder
      ~receiver:group_staker.public_key_hash
      client
  in

  let signers = List.filteri (fun i _s -> i < 3) stakers in
  (* reveal key for group_staker *)
  let* op_reveal = Local_helpers.mk_op_reveal group_staker client in
  let* _op_hash =
    Local_helpers.inject_threshold_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers
      op_reveal
      client
  in
  (* set delegate for group_staker to group_staker *)
  let* op_set_delegate =
    Local_helpers.mk_op_set_delegate
      ~src:group_staker
      ~delegate:group_staker
      client
  in
  let* _op_hash =
    Local_helpers.inject_threshold_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers
      op_set_delegate
      client
  in
  (* stake 140000 for group_staker *)
  let* op_stake =
    Local_helpers.mk_op_stake
      ~staker:group_staker
      ~amount:(Tez.of_int 140_000)
      client
  in
  let* _op_hash =
    Local_helpers.inject_threshold_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers
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
  let* proof =
    Client.create_bls_proof ~signer:delegate_consensus_key.alias client
  in
  (* set consensus key for group_staker to delegate_consensus_key *)
  let* op_update_consensus_key =
    Local_helpers.mk_op_update_consensus_key
      ~delegate:group_staker
      ~delegate_consensus_key
      ~proof
      client
  in
  let* _op_hash =
    Local_helpers.inject_threshold_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers
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
      ~staker:group_staker
      client
  in
  unit

(** N = 5 and M = 3 *)
let test_threshold_number_stakers_sign_staking_operation_consensus_key_batch
    ~kind =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "threshold number of stakers sign a staking operation with a \
          consensus key in batch (%s)"
         (kind_to_string kind))
    ~tags:(threshold_bls_tags @ ["threshold"; "consensus_key"; "batch"])
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let* parameters, client, default_baker, funder =
    Local_helpers.init_node_and_client ~protocol
  in
  let master_sk =
    Bls12_381_signature.generate_sk @@ Tezos_hacl.Hacl.Rand.gen 32
    |> Local_helpers.bls_sk_to_b58_string
  in
  (* Shamir's Secret Sharing *)
  let* group_pk_bls, group_pkh_bls, stakers =
    Local_helpers.create_accounts_from_master_sk ~sk:master_sk ~m:3 ~n:5 client
  in
  let group_staker =
    Local_helpers.mk_fake_account_from_bls_pk
      ~bls_pk:group_pk_bls
      ~bls_pkh:group_pkh_bls
      ~alias:"group_staker"
  in
  (* transfer 150000 from bootstrap2 to group_staker *)
  let* () =
    Local_helpers.transfer
      ~baker:default_baker
      ~amount:(Tez.of_int 150_000)
      ~giver:funder
      ~receiver:group_staker.public_key_hash
      client
  in

  let signers = List.filteri (fun i _s -> i < 3) stakers in

  (* gen keys delegate_consensus_key -s bls *)
  let* delegate_consensus_key =
    Local_helpers.gen_and_show_keys
      ~alias:"delegate_consensus_key"
      ~sig_alg:"bls"
      client
  in
  (* create a proof for a consensus key *)
  let* proof =
    Client.create_bls_proof ~signer:delegate_consensus_key.alias client
  in

  (* reveal key for group_staker *)
  (* set delegate for group_staker to group_staker *)
  (* stake 140000 for group_staker *)
  (* set consensus key for group_staker to delegate_consensus_key *)
  let* op_batch =
    Local_helpers.mk_reveal_delegation_stake_update_ck_in_batch
      ~delegate:group_staker
      ~delegate_consensus_key
      ~proof
      ~amount:(Tez.of_int 140_000)
      client
  in
  let* _op_hash =
    Local_helpers.inject_threshold_bls_sign_op
      ~kind
      ~baker:default_baker
      ~signers
      op_batch
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
      ~staker:group_staker
      client
  in
  unit

let register ~protocols =
  test_single_staker_sign_staking_operation_self_delegate protocols ;
  test_single_staker_sign_staking_operation_external_delegate protocols ;
  test_single_staker_sign_staking_operation_consensus_key protocols ;
  test_single_staker_sign_staking_operation_consensus_key_op_core protocols ;
  test_all_stakers_sign_staking_operation_external_delegate
    ~kind:Client
    protocols ;
  test_all_stakers_sign_staking_operation_external_delegate ~kind:RPC protocols ;
  test_all_stakers_sign_staking_operation_consensus_key ~kind:Client protocols ;
  test_all_stakers_sign_staking_operation_consensus_key ~kind:RPC protocols ;
  test_all_stakers_sign_staking_operation_consensus_key_batch
    ~kind:Client
    protocols ;
  test_threshold_number_stakers_sign_staking_operation_external_delegate
    ~kind:Client
    protocols ;
  test_threshold_number_stakers_sign_staking_operation_external_delegate
    ~kind:RPC
    protocols ;
  test_threshold_number_stakers_sign_staking_operation_consensus_key
    ~kind:Client
    protocols ;
  test_threshold_number_stakers_sign_staking_operation_consensus_key
    ~kind:RPC
    protocols ;
  test_threshold_number_stakers_sign_staking_operation_consensus_key_batch
    ~kind:Client
    protocols
