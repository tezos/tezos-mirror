(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Requirement:  For dal tests: ./scripts/install_dal_trusted_setup.sh
    Component:    Operation size and gas
    Invocation:   dune exec tezt/tests/main.exe -- --file operation_size_and_gas.ml
    Subject:      Tests size and gas consumption of manager operations.
*)

let team = Tag.layer1

(** Tags shared by all tests in this file. *)
let operation_size_and_gas_tags =
  [team; "operation"; "size"; "gas"; "estimation"; "manager"]

let print_gas_consumed gas_consumed =
  let gas_limit = Float.(to_int (ceil gas_consumed)) in
  Log.info
    ~color:Log.Color.FG.gray
    "Estimated gas consumption is: %f. The gas_limit must be at least of %d \
     gas units for the operation to succeed."
    gas_consumed
    gas_limit

let get_consumed_milligas operation_result =
  Log.info
    ~color:Log.Color.FG.gray
    "operation_result: %s"
    (JSON.encode operation_result) ;
  JSON.(operation_result |> get "consumed_milligas" |> as_int)

(* Different options to compute gas consumption by operation *)
(* Option 1. [operation_process] is the output of [Client.spawn_*] *)
let estimated_gas_consumption operation_process =
  let* stdout = Process.check_and_read_stdout operation_process in
  Log.info ~color:Log.Color.FG.blue "stdout = %s" stdout ;
  let gas_consumed =
    let re = Re.Str.regexp "\\(.\\|[ \\\n]\\)*Consumed gas: \\([0-9.]+\\).*" in
    if Re.Str.string_match re stdout 0 then
      float_of_string (Re.Str.matched_group 2 stdout)
    else
      Test.fail
        "Failed to parse the consumed gas in the following output of the dry \
         run:\n\
         %s"
        stdout
  in
  let () = print_gas_consumed gas_consumed in
  return gas_consumed

(* Option 2. Requires the hash of an operation that has been baked
   into a block. *)
(* Example:
   let* (`OpHash op_hash) = Operation.inject op client in
   let* () = Client.bake_for_and_wait ~node client in
   let* op_gas = operation_gas_hash op_hash client in
*)
let operation_gas_hash operation_hash client =
  let* receipt =
    Operation_receipt.get_result_for ~check_previous:1 operation_hash client
  in
  assert (List.length receipt = 1) ;
  let operation_result = List.hd receipt in
  let milligas_consumed = get_consumed_milligas operation_result in
  let gas_consumed = Float.of_int milligas_consumed /. 1000. in
  let () = print_gas_consumed gas_consumed in
  return gas_consumed

(* Option 3. Uses the run_operation RPC *)
let operation_gas ~node operation client =
  let* op_json = Operation.make_run_operation_input operation client in
  (* Log.info "%s" (Ezjsonm.value_to_string ~minify:false op_json) ; *)
  let* output =
    Node.RPC.(
      call node (post_chain_block_helpers_scripts_run_operation (Data op_json)))
  in
  let operation_result =
    JSON.(output |-> "contents" |=> 0 |-> "metadata" |-> "operation_result")
  in
  let milligas_consumed = get_consumed_milligas operation_result in
  let gas_consumed = Float.of_int milligas_consumed /. 1000. in
  let () = print_gas_consumed gas_consumed in
  return gas_consumed

let operation_size operation client =
  let* size = Operation.byte_size operation client in
  Log.info ~color:Log.Color.FG.gray "Operation size in bytes is %d." size ;
  return size

let operation_kind operation client =
  let* op_json_u = Operation.make_run_operation_input operation client in
  let op_json = JSON.annotate ~origin:__LOC__ op_json_u in
  (*   Log.info ~color:Log.Color.FG.red "operation kind: %s" (JSON.encode op_json) ; *)
  let op_kind =
    JSON.(op_json |-> "operation" |-> "contents" |=> 0 |-> "kind" |> as_string)
  in
  return op_kind

let print_op_size_and_gas_in_file ~name ?op_size ~op_gas () =
  let op_size = Option.value ~default:0 op_size in
  let str = sf "%s, %d, %f" name op_size op_gas in
  Regression.capture ~eol:true str ;
  Log.info ~color:Log.Color.FG.red "%s" str

let operation_size_and_gas ?name ?gas_limit ?storage_limit ~node ~source
    operation_payload client =
  let module M = Operation.Manager in
  let op_manager =
    M.make ?gas_limit ?storage_limit ~source @@ operation_payload
  in
  let* op = M.operation [op_manager] client in
  let* op_kind = operation_kind op client in
  let* op_size = operation_size op client in
  let* op_gas = operation_gas ~node op client in
  let name =
    match name with Some name -> op_kind ^ "; " ^ name | None -> op_kind
  in
  print_op_size_and_gas_in_file ~name ~op_size ~op_gas () ;
  unit

let create_account ?(source = Constant.bootstrap2) ~node ~amount ?sig_alg ~alias
    client =
  Log.info
    ~color:Log.Color.FG.green
    "Create a [%s] account: generate a key, inject a transaction that funds \
     it, and bake a block to apply the transaction."
    alias ;
  let* fresh_account = Client.gen_and_show_keys ?sig_alg ~alias client in
  let* _oph =
    Operation.Manager.inject_single_transfer
      client
      ~source
      ~dest:fresh_account
      ~amount
  in
  let* () = Client.bake_for_and_wait ~node client in
  return fresh_account

let create_account_and_reveal ?source ~node ~amount ~alias client =
  let* fresh_account = create_account ?source ~node ~amount ~alias client in
  Log.info ~color:Log.Color.FG.green "Reveal pkh of [%s] account." alias ;
  let op_reveal =
    Operation.Manager.(make ~source:fresh_account (reveal fresh_account ()))
  in
  let* _oph = Operation.Manager.inject [op_reveal] client in
  let* () = Client.bake_for_and_wait ~node client in
  return fresh_account

let originate_contract ?init ~node protocol script_name client =
  Log.info
    ~color:Log.Color.FG.green
    "Originate contract %s."
    Michelson_script.(find script_name protocol |> name_s) ;
  let* _alias_contract, hash_contract =
    Client.originate_contract_at
      ?init
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~burn_cap:(Tez.of_int 10)
      client
      script_name
      protocol
  in
  let* () = Client.bake_for_and_wait ~node client in
  Log.info ~color:Log.Color.FG.gray "Contract address is %s." hash_contract ;
  return hash_contract

let test_reveal =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for reveal operation"
    ~tags:(operation_size_and_gas_tags @ ["reveal"])
  @@ fun protocol ->
  Log.info ~color:Log.Color.FG.green "Initialize a node and a client." ;
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let* fresh_account =
    create_account ~node ~amount:10_000_000 ~alias:"fresh_account" client
  in
  Log.info ~color:Log.Color.FG.green "Reveal pkh of [fresh_account]." ;
  let op_reveal = Operation.Manager.reveal fresh_account () in
  operation_size_and_gas ~source:fresh_account ~node op_reveal client

let test_reveal_tz4 =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for tz4 reveal operation"
    ~tags:(operation_size_and_gas_tags @ ["reveal"; "tz4"])
  @@ fun protocol ->
  Log.info ~color:Log.Color.FG.green "Initialize a node and a client." ;
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let* fresh_account =
    create_account
      ~node
      ~amount:10_000_000
      ~sig_alg:"bls"
      ~alias:"fresh_account"
      client
  in
  let proof =
    if Protocol.(number protocol > 022) then
      Operation.Manager.create_proof_of_possession ~signer:fresh_account
    else None
  in
  Log.info ~color:Log.Color.FG.green "Reveal pkh of [fresh_account]." ;
  let op_reveal = Operation.Manager.reveal ?proof fresh_account () in
  operation_size_and_gas
    ~gas_limit:3500
    ~source:fresh_account
    ~node
    op_reveal
    client

let test_simple_transfer =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for simple transfer operation"
    ~tags:(operation_size_and_gas_tags @ ["transfer"])
  @@ fun protocol ->
  Log.info ~color:Log.Color.FG.green "Initialize a node and a client." ;
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  Log.info
    ~color:Log.Color.FG.green
    "Transfer 10_000_000 mutez from [bootstrap1] to [bootstrap2]." ;
  let op_transfer =
    Operation.Manager.transfer ~dest:Constant.bootstrap2 ~amount:10_000_000 ()
  in
  operation_size_and_gas ~source:Constant.bootstrap1 ~node op_transfer client

let test_delegation =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for delegation operation"
    ~tags:(operation_size_and_gas_tags @ ["delegation"])
  @@ fun protocol ->
  Log.info ~color:Log.Color.FG.green "Initialize a node and a client." ;
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let* delegator =
    create_account_and_reveal ~node ~amount:10_000_000 ~alias:"delegator" client
  in
  Log.info
    ~color:Log.Color.FG.green
    "Set delegate for [delegator] to [bootstrap1]." ;
  let op_delegate =
    Operation.Manager.delegation ~delegate:Constant.bootstrap1 ()
  in
  operation_size_and_gas ~source:delegator ~node op_delegate client

let name_concat name = String.concat "/" name

let test_contract_call =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for contract call operation"
    ~tags:(operation_size_and_gas_tags @ ["contract"; "call"])
  @@ fun protocol ->
  let test ?gas_limit ?storage_limit ~script_name ?init ~arg
      ~(source : Account.key) ~node client =
    let* contract =
      originate_contract ?init ~node protocol script_name client
    in
    Log.info
      ~color:Log.Color.FG.green
      "[%s] calls the [%s] contract."
      source.alias
      contract ;
    let* arg = Client.convert_data_to_json ~data:arg client in
    let op_contract_call = Operation.Manager.call ~arg ~dest:contract () in
    operation_size_and_gas
      ~name:(name_concat script_name)
      ?gas_limit
      ?storage_limit
      ~source
      ~node
      op_contract_call
      client
  in
  Log.info ~color:Log.Color.FG.green "Initialize a node and a client." ;
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let account = Constant.bootstrap1 in

  let script_name = ["mini_scenarios"; "check_signature"] in
  let msg = "0x" ^ Hex.show (Hex.of_string "Message to sign") in
  let* signature = Client.sign_bytes ~signer:account.alias ~data:msg client in
  let data = sf "Pair %S %S %s" account.public_key signature msg in
  test
    ~gas_limit:1470
    ~storage_limit:161
    ~script_name
    ~arg:data
    ~source:account
    ~node
    client

let test_origination =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for contract origination operation"
    ~tags:(operation_size_and_gas_tags @ ["contract"; "origination"])
  @@ fun protocol ->
  let test ?gas_limit ?storage_limit ~script_name ~init_storage
      ~(source : Account.key) ~node client =
    let* init_storage = Client.convert_data_to_json ~data:init_storage client in
    let script_path = Michelson_script.(find script_name protocol |> path) in
    let* code = Client.convert_script_to_json ~script:script_path client in
    let name = name_concat script_name in
    Log.info ~color:Log.Color.FG.green "Originate contract %s." name ;
    let op_origination = Operation.Manager.origination ~init_storage ~code () in
    operation_size_and_gas
      ~name
      ?gas_limit
      ?storage_limit
      ~source
      ~node
      op_origination
      client
  in
  Log.info ~color:Log.Color.FG.green "Initialize a node and a client." ;
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let account = Constant.bootstrap1 in

  let script_name = ["mini_scenarios"; "check_signature"] in
  let* () =
    test
      ~gas_limit:700
      ~storage_limit:500
      ~script_name
      ~init_storage:"Unit"
      ~source:account
      ~node
      client
  in

  let script_name = ["mini_scenarios"; "big_map_all"] in
  let all_values = List.init 10 Fun.id in
  let entries : (string * int) list =
    List.map (fun i -> (Format.sprintf "\"%04i\"" i, i)) all_values
  in
  let entries_s =
    List.map (fun (k, v) -> sf "Elt %s %s " k @@ Int.to_string v) entries
  in
  let init_storage = "{" ^ String.concat ";" entries_s ^ "}" in
  test
    ~gas_limit:3009
    ~storage_limit:1032
    ~script_name
    ~init_storage
    ~source:account
    ~node
    client

(* inspired by test_dal_node_import_snapshot from tests/dal.ml *)
let test_dal_publish_commitment =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for dal publish commitment operation"
    ~tags:(operation_size_and_gas_tags @ ["dal"])
    ~uses:(fun _protocol -> [Constant.octez_dal_node])
  @@ fun protocol ->
  let test _protocol dal_parameters _cryptobox node client dal_node =
    let* commitment, proof =
      Dal.Helpers.(
        store_slot dal_node ~slot_index:0
        @@ make_slot
             ~slot_size:dal_parameters.Dal_common.Parameters.cryptobox.slot_size
             "content1")
    in
    let commitment = Dal_common.Commitment.of_string commitment in
    let proof = Dal_common.Commitment.proof_of_string proof in
    let op_dal_publish_commitment =
      Operation.Manager.dal_publish_commitment ~index:0 ~commitment ~proof
    in
    operation_size_and_gas
      ~source:Constant.bootstrap1
      ~node
      op_dal_publish_commitment
      client
  in
  Dal.with_layer1
    ~l1_history_mode:Default_with_refutation
    ~protocol
    ~dal_enable:true
  @@ fun parameters cryptobox node client ->
  Dal.with_dal_node ~operator_profiles:[0] node @@ fun _key dal_node ->
  test protocol parameters cryptobox node client dal_node

(* inspired by test_cont_refute_pre_migration from tests/sc_rollup_migration.ml *)
let test_sc_rollup_refute =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for SC rollup refutation operation"
    ~tags:(operation_size_and_gas_tags @ ["rollup"; "refutation"])
  @@ fun protocol ->
  let test ~kind =
    let* tezos_node, tezos_client =
      Sc_rollup_helpers.setup_l1
        ~commitment_period:10
        ~challenge_window:10
        ~timeout:10
        protocol
    in
    let* sc_rollup =
      Sc_rollup_helpers.originate_sc_rollup
        ~kind
        ~src:Constant.bootstrap1.alias
        tezos_client
    in
    let* commitment1, player_commitment_hash =
      Sc_rollup_helpers.bake_period_then_publish_commitment
        ~sc_rollup
        ~number_of_ticks:1
        ~src:Constant.bootstrap1.public_key_hash
        tezos_client
    in
    let* _commitment2, opponent_commitment_hash =
      Sc_rollup_helpers.forge_and_publish_commitment
        ~inbox_level:commitment1.inbox_level
        ~predecessor:commitment1.predecessor
        ~sc_rollup
        ~number_of_ticks:2
        ~src:Constant.bootstrap2.public_key_hash
        tezos_client
    in
    let* () =
      (* Before starting refutations, we must wait for one commitment period as
         required by the protocol. The test setup uses commitment_period=10. *)
      let commitment_period_in_blocks = 10 in
      Client.bake_for_and_wait
        tezos_client
        ~count:(commitment_period_in_blocks - 1)
    in
    let refutation =
      Operation.Manager.Start {player_commitment_hash; opponent_commitment_hash}
    in
    let op_sc_rollup_refute =
      Operation.Manager.sc_rollup_refute
        ~sc_rollup
        ~opponent:Constant.bootstrap2.public_key_hash
        ~refutation
        ()
    in
    operation_size_and_gas
      ~source:Constant.bootstrap1
      ~node:tezos_node
      op_sc_rollup_refute
      tezos_client
  in
  let* () = test ~kind:"arith" in
  (* it seems there is no diff in gas & size *)
  (*   let* () = test ~kind:"wasm_2_0_0" in *)
  unit

(* with self-delegate staker *)
let test_staking_operations =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"operation size and gas for staking operations"
    ~tags:
      (operation_size_and_gas_tags @ ["stake"; "unstake"; "finalize_unstake"])
  @@ fun protocol ->
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Left (Protocol.parameter_file protocol))
      []
  in
  Log.info ~color:Log.Color.FG.green "Initialize a node and a client." ;
  let* node, client =
    Client.init_with_protocol ~parameter_file ~protocol `Client ()
  in
  Log.info ~color:Log.Color.FG.green "Create and reveal pkh of [baker]." ;
  let* baker =
    create_account_and_reveal ~node ~amount:10_000_000 ~alias:"baker" client
  in
  Log.info ~color:Log.Color.FG.green "Register [baker] as a delegate." ;
  let* _baker = Client.register_delegate ~delegate:baker.alias client in
  let* () = Client.bake_for_and_wait client in

  Log.info ~color:Log.Color.FG.green "Set delegate parameters for [baker]." ;
  let set_delegate_parameters =
    Client.spawn_set_delegate_parameters
      ~delegate:baker.alias
      ~limit:"5"
      ~edge:"0.5"
      client
  in
  let* op_gas = estimated_gas_consumption set_delegate_parameters in
  print_op_size_and_gas_in_file ~name:"set_delegate_parameters" ~op_gas () ;
  let* () = Client.bake_for_and_wait client in
  let* () = Process.check set_delegate_parameters in

  Log.info ~color:Log.Color.FG.green "[baker] stakes 1 tez." ;
  let stake = Client.spawn_stake (Tez.of_int 1) ~staker:baker.alias client in
  let* op_gas = estimated_gas_consumption stake in
  print_op_size_and_gas_in_file ~name:"stake" ~op_gas () ;
  let* () = Client.bake_for_and_wait client in
  let* () = Process.check stake in

  Log.info ~color:Log.Color.FG.green "[baker] unstakes 1 tez." ;
  let unstake =
    Client.spawn_unstake (Tez.of_int 1) ~staker:baker.alias client
  in
  let* op_gas = estimated_gas_consumption unstake in
  print_op_size_and_gas_in_file ~name:"unstake" ~op_gas () ;
  let* () = Client.bake_for_and_wait client in
  let* () = Process.check unstake in
  (* Bake consensus_rights_delay + max_slashing_period for
     finalize_unstake, max_slashing_period = 2. *)
  let parameters = JSON.parse_file parameter_file in
  let consensus_rights_delay =
    JSON.(get "consensus_rights_delay" parameters |> as_int)
  in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  let* () =
    repeat
      ((consensus_rights_delay + 2) * blocks_per_cycle)
      (fun () -> Client.bake_for_and_wait client)
  in

  Log.info ~color:Log.Color.FG.green "[baker] finalizes unstake of 1 tez." ;
  let finalize_unstake =
    Client.spawn_finalize_unstake ~staker:baker.alias client
  in
  let* op_gas = estimated_gas_consumption finalize_unstake in
  print_op_size_and_gas_in_file ~name:"finalize_unstake" ~op_gas () ;
  let* () = Client.bake_for_and_wait client in
  let* () = Process.check finalize_unstake in
  unit

let register ~protocols =
  test_reveal protocols ;
  test_reveal_tz4 protocols ;
  test_simple_transfer protocols ;
  test_delegation protocols ;
  test_contract_call protocols ;
  test_origination protocols ;
  test_dal_publish_commitment protocols ;
  test_sc_rollup_refute protocols ;
  test_staking_operations protocols
