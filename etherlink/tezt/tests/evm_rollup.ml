(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: EVM Kernel
   Requirement:  make -f kernels.mk build
                 npm install eth-cli solc@0.8.26

                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup

                 ./scripts/install_dal_trusted_setup.sh

   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file evm_rollup.ml
*)
open Sc_rollup_helpers
open Helpers
open Rpc.Syntax
open Contract_path
open Solidity_contracts

module Protocol = struct
  include Protocol

  let register_test =
    Protocol.register_test ~additional_tags:(function
        | Alpha -> []
        | _ -> [Tag.slow])

  let register_regression_test =
    Protocol.register_regression_test ~additional_tags:(function
        | Alpha -> []
        | _ -> [Tag.slow])
end

let pvm_kind = "wasm_2_0_0"

let base_fee_for_hardcoded_tx = Wei.to_wei_z @@ Z.of_int 21000

type l1_contracts = {
  exchanger : string;
  bridge : string;
  admin : string;
  kernel_governance : string;
  kernel_security_governance : string;
  sequencer_governance : string option;
}

type full_evm_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  sc_rollup_address : string;
  originator_key : string;
  rollup_operator_key : string;
  evm_node : Evm_node.t;
  produce_block : unit -> (int, Rpc.error) result Lwt.t;
  endpoint : string;
  l1_contracts : l1_contracts option;
  kernel : string;
  kernel_root_hash : string;
}

let hex_256_of_address acc =
  let s = acc.Eth_account.address in
  (* strip 0x and convert to lowercase *)
  let n = String.length s in
  let s = String.lowercase_ascii @@ String.sub s 2 (n - 2) in
  (* prepend 24 leading zeros *)
  String.("0x" ^ make 24 '0' ^ s)

let expected_gas_fees ~gas_price ~gas_used =
  let open Wei in
  let gas_price = gas_price |> Z.of_int64 |> Wei.to_wei_z in
  let gas_used = gas_used |> Z.of_int64 in
  gas_price * gas_used

let evm_node_version evm_node =
  let endpoint = Evm_node.endpoint evm_node in
  let get_version_url = endpoint ^ "/version" in
  Curl.get get_version_url

let get_transaction_status ~endpoint ~tx =
  let* receipt = Eth_cli.get_receipt ~endpoint ~tx in
  match receipt with
  | None ->
      failwith "no transaction receipt, probably it hasn't been mined yet."
  | Some r -> return r.status

let check_tx_succeeded ~endpoint ~tx =
  let* status = get_transaction_status ~endpoint ~tx in
  Check.(is_true status) ~error_msg:"Expected transaction to succeed." ;
  unit

let check_tx_failed ~endpoint ~tx =
  (* Eth-cli sometimes wraps receipt of failed transaction in its own error
     message. This means that the [tx] could be an empty string.
     Additionally, the output of eth-cli might contain some extra characters,
     so we use a regular expression to make sure we get a valid hash.
  *)
  let tx = tx =~* rex "(0x[0-9a-fA-F]{64})" in
  match tx with
  | Some tx ->
      let* status = get_transaction_status ~endpoint ~tx in
      Check.(is_false status) ~error_msg:"Expected transaction to fail." ;
      unit
  | None -> unit

(* Check simple transfer fee is correct

   We apply a da_fee to every tx - which is paid through an
   increase in in either/both gas_used, gas_price.

   We prefer to keep [gas_used == execution_gas_used] where possible, but
   when this results in [gas_price > tx.max_price_per_gas], we set gas_price to
   tx.max_price_per_gas, and increase the gas_used in the receipt.
*)
let check_tx_gas_for_fee ~da_fee_per_byte ~expected_execution_gas ~gas_price
    ~gas_used ~base_fee_per_gas ~data_size =
  (* execution gas fee *)
  let expected_execution_gas = Z.of_int expected_execution_gas in
  let expected_base_fee_per_gas = Z.of_int32 base_fee_per_gas in
  let execution_gas_fee =
    Z.mul expected_execution_gas expected_base_fee_per_gas
  in
  (* Data availability fee *)
  let assumed_encoded_size = 150 in
  let size = Z.of_int (assumed_encoded_size + data_size) in
  let da_fee = Wei.(da_fee_per_byte * size) in
  (* total fee 'in gas' *)
  let expected_total_fee =
    Z.add (Wei.of_wei_z da_fee) execution_gas_fee |> Z.to_int64
  in
  let total_fee_receipt =
    Z.(mul (of_int64 gas_price) (of_int64 gas_used)) |> Z.to_int64
  in
  Check.((total_fee_receipt >= expected_total_fee) int64)
    ~error_msg:"total fee in receipt %L did not cover expected fees of %R"

let check_status_n_logs ~endpoint ~status ~logs ~tx =
  let* receipt = Eth_cli.get_receipt ~endpoint ~tx in
  match receipt with
  | None ->
      failwith "no transaction receipt, probably it hasn't been mined yet."
  | Some r ->
      Check.(
        (r.status = status)
          bool
          ~__LOC__
          ~error_msg:"Unexpected transaction status, expected: %R but got: %L") ;
      let received_logs = List.map Transaction.extract_log_body r.logs in
      Check.(
        (received_logs = logs)
          (list (tuple3 string (list string) string))
          ~__LOC__
          ~error_msg:"Unexpected transaction logs, expected:\n%R but got:\n%L") ;
      unit

(** [get_value_in_storage client addr nth] fetch the [nth] value in the storage
    of account [addr]  *)
let get_value_in_storage sc_rollup_node address nth =
  Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
  @@ Sc_rollup_rpc.get_global_block_durable_state_value
       ~pvm_kind
       ~operation:Sc_rollup_rpc.Value
       ~key:
         (Durable_storage_path.storage
            address
            ~key:(Helpers.hex_256_of_int nth)
            ())
       ()

let check_str_in_storage ~evm_setup ~address ~nth ~expected =
  let* value = get_value_in_storage evm_setup.sc_rollup_node address nth in
  Check.((value = Some expected) (option string))
    ~error_msg:"Unexpected value in storage, should be %R, but got %L" ;
  unit

let check_nb_in_storage ~evm_setup ~address ~nth ~expected =
  check_str_in_storage
    ~evm_setup
    ~address
    ~nth
    ~expected:(Helpers.hex_256_of_int expected)

let get_storage_size sc_rollup_node ~address =
  let* storage =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:(Durable_storage_path.storage address ())
         ()
  in
  return (List.length storage)

let check_storage_size sc_rollup_node ~address size =
  (* check storage size *)
  let* storage_size = get_storage_size sc_rollup_node ~address in
  Check.((storage_size = size) int)
    ~error_msg:"Unexpected storage size, should be %R, but is %L" ;
  unit

let setup_l1_contracts ~admin ?sequencer_admin client =
  (* Originates the exchanger. *)
  let* exchanger =
    Client.originate_contract
      ~alias:"exchanger"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:"Unit"
      ~prg:(exchanger_path ())
      ~burn_cap:Tez.one
      client
  in
  (* Originates the bridge. *)
  let* bridge =
    Client.originate_contract
      ~alias:"evm-bridge"
      ~amount:Tez.zero
      ~src:Constant.bootstrap2.public_key_hash
      ~init:(sf "Pair %S None" exchanger)
      ~prg:(bridge_path ())
      ~burn_cap:Tez.one
      client
  (* Originates the administrator contract. *)
  and* admin_contract =
    Client.originate_contract
      ~alias:"evm-admin"
      ~amount:Tez.zero
      ~src:Constant.bootstrap3.public_key_hash
      ~init:(sf "%S" admin.Account.public_key_hash)
      ~prg:(admin_path ())
      ~burn_cap:Tez.one
      client
  (* Originates the governance contract (using the administrator contract). *)
  and* kernel_governance =
    Client.originate_contract
      ~alias:"kernel-governance"
      ~amount:Tez.zero
      ~src:Constant.bootstrap4.public_key_hash
      ~init:(sf "%S" admin.Account.public_key_hash)
      ~prg:(admin_path ())
      ~burn_cap:Tez.one
      client
  (* Originates the governance contract (using the administrator contract). *)
  and* kernel_security_governance =
    Client.originate_contract
      ~alias:"security-governance"
      ~amount:Tez.zero
      ~src:Constant.bootstrap5.public_key_hash
      ~init:(sf "%S" admin.Account.public_key_hash)
      ~prg:(admin_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in

  (* Originates the sequencer administrator contract. *)
  let* sequencer_governance =
    match sequencer_admin with
    | Some sequencer_admin ->
        let* sequencer_admin =
          Client.originate_contract
            ~alias:"evm-sequencer-admin"
            ~amount:Tez.zero
            ~src:Constant.bootstrap1.public_key_hash
            ~init:(sf "%S" sequencer_admin.Account.public_key_hash)
            ~prg:(admin_path ())
            ~burn_cap:Tez.one
            client
        in
        let* () = Client.bake_for_and_wait ~keys:[] client in
        return (Some sequencer_admin)
    | None -> return None
  in
  return
    {
      exchanger;
      bridge;
      admin = admin_contract;
      kernel_governance;
      sequencer_governance;
      kernel_security_governance;
    }

type setup_mode =
  | Setup_sequencer of {
      return_sequencer : bool;
      time_between_blocks : Evm_node.time_between_blocks option;
      sequencer : Account.key;
      max_blueprints_ahead : int option;
      block_storage_sqlite3 : bool;
    }
  | Setup_proxy

let setup_evm_kernel ?additional_config ?(setup_kernel_root_hash = true)
    ?(kernel = Kernel.Latest)
    ?(originator_key = Constant.bootstrap1.public_key_hash)
    ?(rollup_operator_key = Constant.bootstrap1.public_key_hash) ?chain_id
    ?(bootstrap_accounts =
      List.map
        (fun account -> account.Eth_account.address)
        (Array.to_list Eth_account.bootstrap_accounts))
    ?(with_administrator = true) ?da_fee_per_byte ?minimum_base_fee_per_gas
    ~admin ?sequencer_admin ?commitment_period ?challenge_window ?timestamp
    ?tx_pool_timeout_limit ?tx_pool_addr_limit ?tx_pool_tx_per_addr_limit
    ?max_number_of_chunks ?(setup_mode = Setup_proxy)
    ?(force_install_kernel = true) ?whitelist ?maximum_allowed_ticks
    ?restricted_rpcs ?(enable_dal = false) ?dal_slots protocol =
  let _, kernel_installee = Kernel.to_uses_and_tags kernel in
  let* node, client =
    setup_l1 ?commitment_period ?challenge_window ?timestamp protocol
  in
  let* dal_node =
    if enable_dal then
      let dal_node = Dal_node.create ~node () in
      let* () = Dal_node.init_config ?producer_profiles:dal_slots dal_node in
      let* () = Dal_node.run ~wait_ready:true dal_node in
      some dal_node
    else none
  in
  let client = Client.with_dal_node client ?dal_node in
  let* l1_contracts =
    match admin with
    | Some admin ->
        let* res = setup_l1_contracts ~admin ?sequencer_admin client in
        return (Some res)
    | None -> return None
  in
  let* kernel_root_hash =
    if setup_kernel_root_hash then
      let* {root_hash; _} =
        prepare_installer_kernel
          ~preimages_dir:(Temp.dir "ignored_preimages")
          kernel_installee
      in
      return (Some root_hash)
    else return None
  in
  (* If a L1 bridge was set up, we make the kernel aware of the address. *)
  let* base_config =
    let ticketer = Option.map (fun {exchanger; _} -> exchanger) l1_contracts in
    let administrator =
      if with_administrator then
        Option.map (fun {admin; _} -> admin) l1_contracts
      else None
    in
    let kernel_governance =
      Option.map (fun {kernel_governance; _} -> kernel_governance) l1_contracts
    in
    let kernel_security_governance =
      Option.map
        (fun {kernel_security_governance; _} -> kernel_security_governance)
        l1_contracts
    in
    let sequencer =
      match setup_mode with
      | Setup_proxy -> None
      | Setup_sequencer {sequencer; _} -> Some sequencer.public_key
    in
    let output_config = Temp.file "config.yaml" in
    let*! () =
      Evm_node.make_kernel_installer_config
        ?chain_id
        ~mainnet_compat:false
        ~remove_whitelist:Option.(is_some whitelist)
        ?kernel_root_hash
        ~bootstrap_accounts
        ?da_fee_per_byte
        ?minimum_base_fee_per_gas
        ?ticketer
        ?administrator
        ?kernel_governance
        ?kernel_security_governance
        ?sequencer
        ?sequencer_governance:
          (Option.bind l1_contracts (fun {sequencer_governance; _} ->
               sequencer_governance))
        ?maximum_allowed_ticks
        ~output:output_config
        ~enable_dal
        ?dal_slots
        ()
    in
    match additional_config with
    | Some config -> return @@ `Both (config, output_config)
    | None -> return @@ `Path output_config
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:rollup_operator_key
      ?dal_node
  in
  (* Start a rollup node *)
  let preimages_dir =
    Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0"
  in
  let* {output; root_hash; _} =
    prepare_installer_kernel ~preimages_dir ~config:base_config kernel_installee
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ?whitelist
      ~keys:[]
      ~kind:pvm_kind
      ~boot_sector:("file:" ^ output)
      ~parameters_ty:evm_type
      ~src:originator_key
      client
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  (* EVM Kernel installation level. *)
  let* () =
    if force_install_kernel then
      let* () = Client.bake_for_and_wait ~keys:[] client in
      let* level = Node.get_level node in
      let* _ =
        Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level
      in
      unit
    else unit
  in
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~node_transaction_validation:true
      ()
  in
  let* produce_block, evm_node =
    match setup_mode with
    | Setup_proxy ->
        let mode = Evm_node.Proxy in
        let* evm_node =
          Evm_node.init
            ~patch_config
            ~mode
            ?restricted_rpcs
            (Sc_rollup_node.endpoint sc_rollup_node)
        in
        return
          ( (fun () ->
              let* l = next_rollup_node_level ~sc_rollup_node ~client in
              return (Ok l)),
            evm_node )
    | Setup_sequencer
        {
          return_sequencer;
          time_between_blocks;
          sequencer;
          max_blueprints_ahead;
          block_storage_sqlite3;
        } ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature
            ~node_transaction_validation:true
            ~block_storage_sqlite3
            ()
        in
        let private_rpc_port = Some (Port.fresh ()) in
        let sequencer_mode =
          Evm_node.Sequencer
            {
              initial_kernel = output;
              preimage_dir = Some preimages_dir;
              private_rpc_port;
              time_between_blocks;
              sequencer = sequencer.alias;
              genesis_timestamp = None;
              max_blueprints_lag = None;
              max_blueprints_ahead;
              max_blueprints_catchup = None;
              catchup_cooldown = None;
              max_number_of_chunks;
              wallet_dir = Some (Client.base_dir client);
              tx_pool_timeout_limit;
              tx_pool_addr_limit;
              tx_pool_tx_per_addr_limit;
              dal_slots;
            }
        in
        let* sequencer =
          Evm_node.init
            ~patch_config
            ~mode:sequencer_mode
            ?restricted_rpcs
            (Sc_rollup_node.endpoint sc_rollup_node)
        in
        let produce_block () = Rpc.produce_block sequencer in
        if return_sequencer then return (produce_block, sequencer)
        else
          let evm_node =
            Evm_node.create
              ~data_dir:(Evm_node.data_dir sequencer)
              ~mode:(Rpc Evm_node.(mode sequencer))
              (Evm_node.endpoint sequencer)
          in
          let* () = Evm_node.run evm_node in
          return (produce_block, evm_node)
  in
  let endpoint = Evm_node.endpoint evm_node in
  return
    {
      node;
      client;
      sc_rollup_node;
      sc_rollup_address;
      originator_key;
      rollup_operator_key;
      evm_node;
      produce_block;
      endpoint;
      l1_contracts;
      kernel = output;
      kernel_root_hash = root_hash;
    }

let register_test ~title ~tags ?(kernels = Kernel.all) ?additional_config ?admin
    ?(additional_uses = []) ?commitment_period ?challenge_window
    ?bootstrap_accounts ?whitelist ?da_fee_per_byte ?minimum_base_fee_per_gas
    ?rollup_operator_key ?maximum_allowed_ticks ?restricted_rpcs ~setup_mode
    ~enable_dal ?(dal_slots = if enable_dal then Some [4] else None) f protocols
    =
  let extra_tag =
    match setup_mode with
    | Setup_proxy -> "proxy"
    | Setup_sequencer _ -> "sequencer"
  in
  List.iter
    (fun kernel ->
      let kernel_tag, kernel_use = Kernel.to_uses_and_tags kernel in
      let uses _protocol =
        [
          kernel_use;
          Constant.octez_smart_rollup_node;
          Constant.octez_evm_node;
          Constant.smart_rollup_installer;
        ]
        @ (if enable_dal then [Constant.octez_dal_node] else [])
        @ additional_uses
      in
      Protocol.register_test
        ~__FILE__
        ~tags:
          ((if enable_dal then ["dal"; Tag.ci_disabled] else [])
          @ (kernel_tag :: extra_tag :: tags))
        ~uses
        ~title:
          (sf
             "%s (%s, %s, %s)"
             title
             extra_tag
             kernel_tag
             (if enable_dal then "with dal" else "without dal"))
        (fun protocol ->
          let* evm_setup =
            setup_evm_kernel
              ~kernel
              ?additional_config
              ?whitelist
              ?commitment_period
              ?challenge_window
              ?bootstrap_accounts
              ?da_fee_per_byte
              ?minimum_base_fee_per_gas
              ?rollup_operator_key
              ?maximum_allowed_ticks
              ?restricted_rpcs
              ~admin
              ~setup_mode
              ~enable_dal
              ?dal_slots
              protocol
          in
          f ~protocol ~evm_setup)
        protocols)
    kernels

let register_proxy ~title ~tags ?kernels ?additional_uses ?additional_config
    ?admin ?commitment_period ?challenge_window ?bootstrap_accounts
    ?da_fee_per_byte ?minimum_base_fee_per_gas ?whitelist ?rollup_operator_key
    ?maximum_allowed_ticks ?restricted_rpcs f protocols =
  let register ~enable_dal : unit =
    register_test
      ~title
      ~tags
      ?kernels
      ?additional_uses
      ?additional_config
      ?admin
      ?commitment_period
      ?challenge_window
      ?bootstrap_accounts
      ?da_fee_per_byte
      ?minimum_base_fee_per_gas
      ?whitelist
      ?rollup_operator_key
      ?maximum_allowed_ticks
      ?restricted_rpcs
      f
      protocols
      ~enable_dal
      ~setup_mode:Setup_proxy
  in
  register ~enable_dal:false ;
  register ~enable_dal:true

let register_sequencer ?(return_sequencer = false) ~title ~tags ?kernels
    ?additional_uses ?additional_config ?admin ?commitment_period
    ?challenge_window ?bootstrap_accounts ?da_fee_per_byte
    ?minimum_base_fee_per_gas ?time_between_blocks ?whitelist
    ?rollup_operator_key ?maximum_allowed_ticks ?restricted_rpcs
    ?max_blueprints_ahead ?(block_storage_sqlite3 = false) f protocols =
  let register ~enable_dal : unit =
    register_test
      ~title
      ~tags
      ?kernels
      ?additional_uses
      ?additional_config
      ?admin
      ?commitment_period
      ?challenge_window
      ?bootstrap_accounts
      ?da_fee_per_byte
      ?minimum_base_fee_per_gas
      ?whitelist
      ?rollup_operator_key
      ?maximum_allowed_ticks
      ?restricted_rpcs
      f
      protocols
      ~enable_dal
      ~setup_mode:
        (Setup_sequencer
           {
             return_sequencer;
             time_between_blocks;
             sequencer = Constant.bootstrap1;
             max_blueprints_ahead;
             block_storage_sqlite3;
           })
  in
  register ~enable_dal:false ;
  register ~enable_dal:true

let register_both ~title ~tags ?kernels ?additional_uses ?additional_config
    ?admin ?commitment_period ?challenge_window ?bootstrap_accounts
    ?da_fee_per_byte ?minimum_base_fee_per_gas ?time_between_blocks ?whitelist
    ?rollup_operator_key ?maximum_allowed_ticks ?restricted_rpcs
    ?max_blueprints_ahead ?block_storage_sqlite3 f protocols : unit =
  register_proxy
    ~title
    ~tags
    ?kernels
    ?additional_uses
    ?additional_config
    ?admin
    ?commitment_period
    ?challenge_window
    ?bootstrap_accounts
    ?da_fee_per_byte
    ?minimum_base_fee_per_gas
    ?whitelist
    ?rollup_operator_key
    ?maximum_allowed_ticks
    ?restricted_rpcs
    f
    protocols ;
  register_sequencer
    ~title
    ~tags
    ?kernels
    ?additional_uses
    ?additional_config
    ?admin
    ?commitment_period
    ?challenge_window
    ?bootstrap_accounts
    ?da_fee_per_byte
    ?minimum_base_fee_per_gas
    ?time_between_blocks
    ?whitelist
    ?rollup_operator_key
    ?maximum_allowed_ticks
    ?restricted_rpcs
    ?max_blueprints_ahead
    ?block_storage_sqlite3
    f
    protocols

let deploy ~contract ~sender full_evm_setup =
  let {evm_node; produce_block; _} = full_evm_setup in
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  (* if contract label is already among them, then update, otherwise add *)
  let* already_registered = Eth_cli.check_abi ~label:contract.label () in
  let* () =
    if already_registered then
      Eth_cli.update_abi ~label:contract.label ~abi:contract.abi ()
    else Eth_cli.add_abi ~label:contract.label ~abi:contract.abi ()
  in
  let send_deploy () =
    Eth_cli.deploy
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint:evm_node_endpoint
      ~abi:contract.label
      ~bin:contract.bin
  in
  Helpers.wait_for_application ~produce_block send_deploy

type deploy_checks = {
  contract : contract;
  expected_address : string;
  expected_code : string;
}

let deploy_with_base_checks {contract; expected_address; expected_code}
    full_evm_setup =
  let {sc_rollup_node; evm_node; _} = full_evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address, tx = deploy ~contract ~sender full_evm_setup in
  let address = String.lowercase_ascii contract_address in
  Check.(
    (address = expected_address)
      string
      ~error_msg:"Expected address to be %R but was %L.") ;
  let* code_in_kernel =
    Evm_node.fetch_contract_code evm_node contract_address
  in
  Check.((code_in_kernel = expected_code) string)
    ~error_msg:"Unexpected code %L, it should be %R" ;
  (* The transaction was a contract creation, the transaction object
     must not contain the [to] field. *)
  let* tx_object = Eth_cli.transaction_get ~endpoint ~tx_hash:tx in
  (match tx_object with
  | Some tx_object ->
      Check.((tx_object.to_ = None) (option string))
        ~error_msg:
          "The transaction object of a contract creation should not have the \
           [to] field present"
  | None -> Test.fail "The transaction object of %s should be available" tx) ;
  let* accounts =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:Durable_storage_path.eth_accounts
         ()
  in
  (* check tx status*)
  let* () = check_tx_succeeded ~endpoint ~tx in
  (* check contract account was created *)
  Check.(
    list_mem
      string
      (Durable_storage_path.normalize contract_address)
      (List.map String.lowercase_ascii accounts)
      ~error_msg:"Expected %L account to be initialized by contract creation.") ;
  unit

let send ~sender ~receiver ~value ?data full_evm_setup =
  let {produce_block; evm_node; _} = full_evm_setup in
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let send =
    Eth_cli.transaction_send
      ~source_private_key:sender.Eth_account.private_key
      ~to_public_key:receiver.Eth_account.address
      ~value
      ~endpoint:evm_node_endpoint
      ?data
  in
  wait_for_application ~produce_block send

let check_block_progression ~produce_block ~endpoint ~expected_block_level =
  let*@ _level = produce_block () in
  let* block_number = Eth_cli.block_number ~endpoint in
  return
  @@ Check.((block_number = expected_block_level) int)
       ~error_msg:"Unexpected block number, should be %%R, but got %%L"

let test_evm_node_connection =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"]
    ~uses:(fun _protocol -> Constant.[octez_smart_rollup_node; octez_evm_node])
    ~title:"EVM node server connection"
  @@ fun protocol ->
  let* tezos_node, tezos_client = setup_l1 protocol in
  let* sc_rollup =
    originate_sc_rollup
      ~kind:"wasm_2_0_0"
      ~parameters_ty:"string"
      ~src:Constant.bootstrap1.alias
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Observer
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:Constant.bootstrap1.alias
  in
  let evm_node = Evm_node.create (Sc_rollup_node.endpoint sc_rollup_node) in
  let* () = Process.check @@ Evm_node.spawn_init_config evm_node in
  (* Tries to start the EVM node server without a listening rollup node. *)
  let* () = Process.check ~expect_failure:true @@ Evm_node.spawn_run evm_node in
  (* Starts the rollup node. *)
  let* _ = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  (* Starts the EVM node server and asks its version. *)
  let* () = Evm_node.run evm_node in
  let*? process = evm_node_version evm_node in
  let* () = Process.check process in
  unit

let test_originate_evm_kernel =
  register_both ~tags:["evm"] ~title:"Originate EVM kernel with installer"
  @@ fun ~protocol:_ ~evm_setup:{client; node; sc_rollup_node; _} ->
  (* First run of the installed EVM kernel, it will initialize the directory
     "eth_accounts". *)
  let* () = Client.bake_for_and_wait ~keys:[] client in
  let* first_evm_run_level = Node.get_level node in
  let* level =
    Sc_rollup_node.wait_for_level
      ~timeout:30.
      sc_rollup_node
      first_evm_run_level
  in
  Check.(level = first_evm_run_level)
    Check.int
    ~error_msg:"Current level has moved past first EVM run (%L = %R)" ;
  let evm_key = "evm" in
  let* storage_root_keys =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:""
         ()
  in
  Check.(
    list_mem
      string
      evm_key
      storage_root_keys
      ~error_msg:"Expected %L to be initialized by the EVM kernel.") ;
  unit

let test_rpc_getBalance =
  register_both
    ~tags:["evm"; "rpc"; "get_balance"]
    ~title:"RPC method eth_getBalance"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* balance =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(0).address
      ~endpoint:evm_node_endpoint
  in
  Check.((balance = Helpers.default_bootstrap_account_balance) Wei.typ)
    ~error_msg:
      (sf
         "Expected balance of %s should be %%R, but got %%L"
         Eth_account.bootstrap_accounts.(0).address) ;
  unit

let test_rpc_getBlockByNumber =
  register_both
    ~tags:["evm"; "rpc"; "get_block_by_number"]
    ~title:"RPC method eth_getBlockByNumber"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* block = Eth_cli.get_block ~block_id:"0" ~endpoint:evm_node_endpoint in
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  unit

let get_block_by_hash ?(full_tx_objects = false) evm_setup block_hash =
  let* block =
    Evm_node.(
      call_evm_rpc
        evm_setup.evm_node
        {
          method_ = "eth_getBlockByHash";
          parameters = `A [`String block_hash; `Bool full_tx_objects];
        })
  in
  return @@ (block |> Evm_node.extract_result |> Block.of_json)

let test_rpc_getBlockByHash =
  register_both
    ~time_between_blocks:Nothing
    ~tags:["evm"; "rpc"; "get_block_by_hash"]
    ~title:"RPC method eth_getBlockByHash"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup ->
  let evm_node_endpoint = Evm_node.endpoint evm_setup.evm_node in
  let* block = Eth_cli.get_block ~block_id:"0" ~endpoint:evm_node_endpoint in
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  let* block' = get_block_by_hash evm_setup block.hash in
  assert (block = block') ;
  unit

let test_rpc_getBlockReceipts =
  register_both
    ~time_between_blocks:Nothing
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["evm"; "rpc"; "get_block_receipts"]
    ~title:"RPC method eth_getBlockReceipts"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup:{evm_node; produce_block; _} ->
  let txs =
    read_tx_from_file ()
    |> List.filteri (fun i _ -> i < 5)
    |> List.map (fun (tx, _hash) -> tx)
  in
  let* _requests, receipt, _hashes =
    send_n_transactions ~evm_node ~produce_block txs
  in
  let* receipts =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_getBlockReceipts";
          parameters = `A [`String (Format.sprintf "%#lx" receipt.blockNumber)];
        })
  in
  let txs =
    List.map
      (fun receipt ->
        JSON.
          ( receipt |-> "transactionHash" |> as_string,
            receipt |-> "transactionIndex" |> as_int ))
      JSON.(receipts |-> "result" |> as_list)
  in
  let expected_txs =
    read_tx_from_file ()
    |> List.filteri (fun i _ -> i < 5)
    |> List.mapi (fun i (_tx, hash) -> (hash, i))
  in
  assert (List.equal ( = ) txs expected_txs) ;
  unit

let test_rpc_getBlockBy_return_base_fee_per_gas_and_mix_hash =
  register_both
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
     Replace by [Any] after the next upgrade *)
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "get_block_by_hash"]
    ~title:"getBlockBy returns base fee per gas and previous random number"
    ~minimum_base_fee_per_gas:(Wei.to_wei_z @@ Z.of_int 100)
  @@ fun ~protocol:_ ~evm_setup ->
  let evm_node_endpoint = Evm_node.endpoint evm_setup.evm_node in

  let* _ =
    send
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~receiver:Eth_account.bootstrap_accounts.(1)
      ~value:Wei.one
      evm_setup
  in
  let* block_by_number =
    Eth_cli.get_block ~block_id:"1" ~endpoint:evm_node_endpoint
  in
  Check.((block_by_number.baseFeePerGas = 100L) int64)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  Check.(
    (block_by_number.prevRandao
   = "0x0000000000000000000000000000000000000000000000000000000000000000")
      string)
    ~error_msg:"Unexpected previous random number, should be %%R, but got %%L" ;

  let* block_by_hash = get_block_by_hash evm_setup block_by_number.hash in
  Check.((block_by_hash.baseFeePerGas = 100L) int64)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  Check.(
    (block_by_hash.prevRandao
   = "0x0000000000000000000000000000000000000000000000000000000000000000")
      string)
    ~error_msg:"Unexpected previous random number, should be %%R, but got %%L" ;
  unit

let test_l2_block_size_non_zero =
  register_both
    ~tags:["evm"; "block"; "size"]
    ~title:"Block size is greater than zero"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* block = Eth_cli.get_block ~block_id:"0" ~endpoint:evm_node_endpoint in
  Check.((block.size > 0l) int32)
    ~error_msg:"Unexpected block size, should be > 0, but got %%L" ;
  unit

let test_rpc_getTransactionCount =
  register_both
    ~tags:["evm"; "rpc"; "get_transaction_count"]
    ~title:"RPC method eth_getTransactionCount"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let*@ transaction_count =
    Rpc.get_transaction_count
      evm_node
      ~address:Eth_account.bootstrap_accounts.(0).address
  in
  Check.((transaction_count = 0L) int64)
    ~error_msg:"Expected a nonce of %R, but got %L" ;
  unit

let test_rpc_blockNumber =
  register_both
    ~time_between_blocks:Nothing
    ~tags:["evm"; "rpc"; "block_number"]
    ~title:"RPC method eth_blockNumber"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; produce_block; _} ->
  let* () =
    repeat 2 (fun () ->
        let*@ _ = produce_block () in
        unit)
  in
  let*@ block_number = Rpc.block_number evm_node in
  Check.((block_number = 2l) int32)
    ~error_msg:"Expected a block number of %R, but got %L" ;
  unit

let test_rpc_net_version =
  register_both
    ~tags:["evm"; "rpc"; "net_version"]
    ~title:"RPC method net_version"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let*@ net_version = Rpc.net_version evm_node in
  Check.((net_version = "1337") string)
    ~error_msg:"Expected net_version is %R, but got %L" ;
  unit

let test_rpc_getTransactionCountBatch =
  register_both
    ~tags:["evm"; "rpc"; "get_transaction_count_as_batch"]
    ~title:"RPC method eth_getTransactionCount in batch"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let*@ transaction_count =
    Rpc.get_transaction_count
      evm_node
      ~address:Eth_account.bootstrap_accounts.(0).address
  in
  let* transaction_count_batch =
    let* transaction_count =
      Evm_node.batch_evm_rpc
        evm_node
        [
          Rpc.Request.eth_getTransactionCount
            ~address:Eth_account.bootstrap_accounts.(0).address
            ~block:"latest";
        ]
    in
    match JSON.as_list transaction_count with
    | [transaction_count] ->
        return JSON.(transaction_count |-> "result" |> as_int64)
    | _ -> Test.fail "Unexpected result from batching one request"
  in
  Check.((transaction_count = transaction_count_batch) int64)
    ~error_msg:"Nonce from a single request is %L, but got %R from batching it" ;
  unit

let test_rpc_batch =
  register_both ~tags:["evm"; "rpc"; "batch"] ~title:"RPC batch requests"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* transaction_count, chain_id =
    let transaction_count =
      Rpc.Request.eth_getTransactionCount
        ~address:Eth_account.bootstrap_accounts.(0).address
        ~block:"latest"
    in
    let chain_id = Evm_node.{method_ = "eth_chainId"; parameters = `Null} in
    let* results =
      Evm_node.batch_evm_rpc evm_node [transaction_count; chain_id]
    in
    match JSON.as_list results with
    | [transaction_count; chain_id] ->
        return
          ( JSON.(transaction_count |-> "result" |> as_int64),
            JSON.(chain_id |-> "result" |> as_int64) )
    | _ -> Test.fail "Unexpected result from batching two requests"
  in
  Check.((transaction_count = 0L) int64)
    ~error_msg:"Expected a nonce of %R, but got %L" ;
  (* Default chain id for Ethereum custom networks, not chosen randomly. *)
  let default_chain_id = 1337L in
  Check.((chain_id = default_chain_id) int64)
    ~error_msg:"Expected a chain_id of %R, but got %L" ;
  unit

let test_rpc_eth_coinbase =
  register_both ~tags:["evm"; "rpc"; "coinbase"] ~title:"RPC eth_coinbase"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let*@ coinbase = Rpc.coinbase evm_node in
  (* If there's no sequencer pool address, coinbase returns 0x00..00. *)
  Check.((coinbase = "0x0000000000000000000000000000000000000000") string)
    ~error_msg:"eth_coinbase returned %L, expected %R" ;
  unit

let test_l2_blocks_progression =
  register_proxy
    ~tags:["evm"; "l2_blocks_progression"]
    ~title:"Check L2 blocks progression"
  @@ fun ~protocol:_ ~evm_setup:{produce_block; endpoint; _} ->
  let* () =
    check_block_progression ~produce_block ~endpoint ~expected_block_level:1
  in
  let* () =
    check_block_progression ~produce_block ~endpoint ~expected_block_level:2
  in
  unit

let test_consistent_block_hashes =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_blocks"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Check L2 blocks consistency of hashes"
  @@ fun protocol ->
  let* {endpoint; produce_block; _} = setup_evm_kernel ~admin:None protocol in
  let new_block () =
    let*@ _level = produce_block () in
    let* number = Eth_cli.block_number ~endpoint in
    Eth_cli.get_block ~block_id:(string_of_int number) ~endpoint
  in

  let* block0 = Eth_cli.get_block ~block_id:(string_of_int 0) ~endpoint in
  let* block1 = new_block () in
  let* block2 = new_block () in
  let* block3 = new_block () in
  let* block4 = new_block () in

  let check_parent_hash parent block =
    Check.((block.Block.parent = parent.Block.hash) string)
      ~error_msg:"Unexpected parent hash, should be %%R, but got %%L"
  in

  (* Check consistency accross blocks. *)
  check_parent_hash block0 block1 ;
  check_parent_hash block1 block2 ;
  check_parent_hash block2 block3 ;
  check_parent_hash block3 block4 ;

  let block_hashes, parent_hashes =
    List.map
      (fun Block.{hash; parent; _} -> (hash, parent))
      [block0; block1; block2; block3; block4]
    |> List.split
  in
  let block_hashes_uniq = List.sort_uniq compare block_hashes in
  let parent_hashes_uniq = List.sort_uniq compare parent_hashes in

  (* Check unicity of hashes and parent hashes. *)
  Check.(List.(length block_hashes = length block_hashes_uniq) int)
    ~error_msg:"The list of block hashes must be unique" ;
  Check.(List.(length parent_hashes = length parent_hashes_uniq) int)
    ~error_msg:"The list of block parent hashes must be unique" ;

  unit

(** Test that the contract creation works.  *)
let test_l2_deploy_simple_storage =
  register_proxy
    ~tags:["evm"; "l2_deploy"; "simple_storage"]
    ~title:"Check L2 contract deployment"
  @@ fun ~protocol:_ ~evm_setup ->
  let* simple_storage_resolved = simple_storage () in
  deploy_with_base_checks
    {
      contract = simple_storage_resolved;
      expected_address = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344";
      (* The same deployment has been reproduced on the Sepolia testnet, resulting
         on this specific code. *)
      expected_code = simple_storage_code;
    }
    evm_setup

let send_call_set_storage_simple contract_address sender n
    {produce_block; endpoint; _} =
  let* simple_storage_resolved = simple_storage () in
  let call_set (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:simple_storage_resolved.label
      ~address:contract_address
      ~method_call:(Printf.sprintf "set(%d)" n)
  in
  wait_for_application ~produce_block (call_set sender n)

let send_call_get_storage_simple contract_address {endpoint; _} =
  let* simple_storage_resolved = simple_storage () in
  let* nb =
    Eth_cli.contract_call
      ~endpoint
      ~abi_label:simple_storage_resolved.label
      ~address:contract_address
      ~method_call:"get()"
      ()
  in
  return @@ int_of_string (String.trim nb)

let set_and_get_simple_storage_check ~sender ~number ~address ~error_prefix
    evm_setup =
  let*@ code = Rpc.get_code ~address evm_setup.evm_node in
  Check.((code = simple_storage_code) string)
    ~error_msg:(sf "%s, expected code is %%R, but got %%L" error_prefix) ;
  let* tx = send_call_set_storage_simple address sender number evm_setup in
  let* () = check_tx_succeeded ~endpoint:evm_setup.endpoint ~tx in
  let* found_nb = send_call_get_storage_simple address evm_setup in
  Check.((number = found_nb) int)
    ~error_msg:
      (sf
         "%s, storage of simple storage contract: Expected %%L, found %%R"
         error_prefix) ;
  unit

(** Test that a contract can be called,
    and that the call can modify the storage.  *)
let test_l2_call_simple_storage =
  register_proxy
    ~tags:["evm"; "l2_deploy"; "l2_call"; "simple_storage"]
    ~title:"Check L2 contract call"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved = simple_storage () in

  (* deploy contract *)
  let* address, _tx =
    deploy ~contract:simple_storage_resolved ~sender evm_setup
  in

  (* set 42 *)
  let* tx = send_call_set_storage_simple address sender 42 evm_setup in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address 1 in
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:42 in

  (* set 24 by another user *)
  let* tx =
    send_call_set_storage_simple
      address
      Eth_account.bootstrap_accounts.(1)
      24
      evm_setup
  in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address 1 in
  (* value stored has changed *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:24 in

  (* set -1 *)
  (* some environments prevent sending a negative value, as the value is
     unsigned (eg remix) but it is actually the expected result *)
  let* tx = send_call_set_storage_simple address sender (-1) evm_setup in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address 1 in
  (* value stored has changed *)
  let* () =
    check_str_in_storage
      ~evm_setup
      ~address
      ~nth:0
      ~expected:
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  in
  unit

let test_l2_deploy_erc20 =
  register_proxy
    ~tags:["evm"; "l2_deploy"; "erc20"; "l2_call"]
    ~title:"Check L2 erc20 contract deployment"
  @@ fun ~protocol:_ ~evm_setup ->
  (* setup *)
  let {evm_node; sc_rollup_node; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let player = Eth_account.bootstrap_accounts.(1) in
  let* erc20_resolved = erc20 () in

  (* deploy the contract *)
  let* address, tx = deploy ~contract:erc20_resolved ~sender evm_setup in
  Check.(
    (String.lowercase_ascii address
    = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")
      string
      ~error_msg:"Expected address to be %R but was %L.") ;

  (* check tx status *)
  let* () = check_tx_succeeded ~endpoint ~tx in

  (* check account was created *)
  let* accounts =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:Durable_storage_path.eth_accounts
         ()
  in
  Check.(
    list_mem
      string
      (Durable_storage_path.normalize address)
      (List.map String.lowercase_ascii accounts)
      ~error_msg:"Expected %L account to be initialized by contract creation.") ;

  (* minting / burning *)
  let call_mint (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:erc20_resolved.label
      ~address
      ~method_call:(Printf.sprintf "mint(%d)" n)
  in
  let call_burn ?(expect_failure = false) (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:erc20_resolved.label
      ~address
      ~method_call:(Printf.sprintf "burn(%d)" n)
  in
  let transfer_event_topic =
    let h =
      Tezos_crypto.Hacl.Hash.Keccak_256.digest
        (Bytes.of_string "Transfer(address,address,uint256)")
    in
    "0x" ^ Hex.show (Hex.of_bytes h)
  in
  let zero_address = "0x" ^ String.make 64 '0' in
  let mint_logs sender amount =
    [
      ( address,
        [transfer_event_topic; zero_address; hex_256_of_address sender],
        "0x" ^ Helpers.hex_256_of_int amount );
    ]
  in
  let burn_logs sender amount =
    [
      ( address,
        [transfer_event_topic; hex_256_of_address sender; zero_address],
        "0x" ^ Helpers.hex_256_of_int amount );
    ]
  in
  (* sender mints 42 *)
  let* tx =
    wait_for_application
      ~produce_block:evm_setup.produce_block
      (call_mint sender 42)
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(mint_logs sender 42) ~tx
  in

  (* totalSupply is the first value in storage *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:42 in

  (* player mints 100 *)
  let* tx =
    wait_for_application
      ~produce_block:evm_setup.produce_block
      (call_mint player 100)
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(mint_logs player 100) ~tx
  in
  (* totalSupply is the first value in storage *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:142 in

  (* sender tries to burn 100, should fail *)
  let* _tx =
    wait_for_application
      ~produce_block:evm_setup.produce_block
      (call_burn ~expect_failure:true sender 100)
  in
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:142 in

  (* sender tries to burn 42, should succeed *)
  let* tx =
    wait_for_application
      ~produce_block:evm_setup.produce_block
      (call_burn sender 42)
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(burn_logs sender 42) ~tx
  in
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:100 in
  unit

let test_deploy_contract_for_shanghai =
  register_proxy
    ~tags:["evm"; "deploy"; "shanghai"]
    ~title:
      "Check that a contract containing PUSH0 can successfully be deployed."
  @@ fun ~protocol:_ ~evm_setup ->
  let* shanghai_storage_resolved = shanghai_storage () in
  deploy_with_base_checks
    {
      contract = shanghai_storage_resolved;
      expected_address = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344";
      expected_code =
        "0x6080604052348015600e575f80fd5b5060043610603a575f3560e01c80634e70b1dc14603e57806360fe47b11460575780636d4ce63c146068575b5f80fd5b60455f5481565b60405190815260200160405180910390f35b60666062366004606e565b5f55565b005b5f546045565b5f60208284031215607d575f80fd5b503591905056fea26469706673582212205ac3c8853fa911acef1949bb82ac7e4c424679606388e035123d9a6f1120b37a64736f6c634300081a0033";
    }
    evm_setup

let check_log_indices ~endpoint ~status ~tx indices =
  let* receipt = Eth_cli.get_receipt ~endpoint ~tx in
  match receipt with
  | None ->
      failwith "no transaction receipt, probably it hasn't been mined yet."
  | Some r ->
      Check.(
        (r.status = status)
          bool
          ~__LOC__
          ~error_msg:"Unexpected transaction status, expected: %R but got: %L") ;
      let received_indices =
        List.map (fun tx -> tx.Transaction.logIndex) r.logs
      in
      Check.(
        (received_indices = indices)
          (list int32)
          ~__LOC__
          ~error_msg:
            "Unexpected transaction logs indices, expected:\n%R but got:\n%L") ;
      unit

let test_log_index =
  register_both
    ~tags:["evm"; "log_index"; "events"]
    ~title:"Check that log index is correctly computed"
  @@ fun ~protocol:_ ~evm_setup ->
  (* setup *)
  let {evm_node; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let _player = Eth_account.bootstrap_accounts.(1) in
  let* events_resolved = events () in
  (* deploy the events contract *)
  let* _address, _tx = deploy ~contract:events_resolved ~sender evm_setup in
  (* Emits two events: EventA and EventB *)
  let* raw_emitBoth =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:27_638
      ~value:Wei.zero
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~signature:"emitBoth(uint256)"
      ~arguments:["100"]
      ()
  in
  let* raw_emitA =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:25_664
      ~value:Wei.zero
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~signature:"emitA(uint256)"
      ~arguments:["10"]
      ()
  in
  let* _requests, _receipt, hashes =
    send_n_transactions
      ~produce_block:evm_setup.produce_block
      ~evm_node
      [raw_emitBoth; raw_emitA]
  in
  let* () =
    check_log_indices ~endpoint ~status:true ~tx:(List.hd hashes) [0l; 1l]
  in
  check_log_indices ~endpoint ~status:true ~tx:(List.nth hashes 1) [2l]

(* TODO: add internal parameters here (e.g the kernel version) *)
type config_result = {chain_id : int64}

let config_setup evm_setup =
  let web3_clientVersion =
    Evm_node.{method_ = "web3_clientVersion"; parameters = `A []}
  in
  let chain_id = Evm_node.{method_ = "eth_chainId"; parameters = `Null} in
  let* results =
    Evm_node.batch_evm_rpc evm_setup.evm_node [web3_clientVersion; chain_id]
  in
  match JSON.as_list results with
  | [web3_clientVersion; chain_id] ->
      (* We don't need to return the web3_clientVersion because,
         it might change after the upgrade.
         The only thing that we need to look out for is,
         are we able to retrieve it and deserialize it. *)
      let _sanity_check = JSON.(web3_clientVersion |-> "result" |> as_string) in
      return {chain_id = JSON.(chain_id |-> "result" |> as_int64)}
  | _ -> Test.fail "Unexpected result from batching two requests"

let ensure_config_setup_integrity ~config_result evm_setup =
  let* upcoming_config_setup = config_setup evm_setup in
  assert (config_result = upcoming_config_setup) ;
  unit

let ensure_block_integrity ~block_result evm_setup =
  let* block =
    Eth_cli.get_block
      ~block_id:(Int32.to_string block_result.Block.number)
      ~endpoint:evm_setup.endpoint
  in
  (* only the relevant fields *)
  assert (block.number = block_result.number) ;
  assert (block.hash = block_result.hash) ;
  assert (block.timestamp = block_result.timestamp) ;
  assert (block.transactions = block_result.transactions) ;
  unit

let latest_block ?(full_tx_objects = false) evm_node =
  Rpc.get_block_by_number ~full_tx_objects ~block:"latest" evm_node

type transfer_result = {
  sender_balance_before : Wei.t;
  sender_balance_after : Wei.t;
  sender_nonce_before : int64;
  sender_nonce_after : int64;
  value : Wei.t;
  tx_hash : string;
  tx_object : Transaction.transaction_object;
  tx_receipt : Transaction.transaction_receipt;
  receiver_balance_before : Wei.t;
  receiver_balance_after : Wei.t;
}

let get_tx_object ~endpoint ~tx_hash =
  let* tx_object = Eth_cli.transaction_get ~endpoint ~tx_hash in
  match tx_object with
  | Some tx_object -> return tx_object
  | None -> Test.fail "The transaction object of %s should be available" tx_hash

let ensure_transfer_result_integrity ~transfer_result ~sender ~receiver
    full_evm_setup =
  let endpoint = Evm_node.endpoint full_evm_setup.evm_node in
  let balance account = Eth_cli.balance ~account ~endpoint in
  let* sender_balance = balance sender.Eth_account.address in
  assert (sender_balance = transfer_result.sender_balance_after) ;
  let* receiver_balance = balance receiver.Eth_account.address in
  assert (receiver_balance = transfer_result.receiver_balance_after) ;
  let*@ sender_nonce =
    Rpc.get_transaction_count full_evm_setup.evm_node ~address:sender.address
  in
  assert (sender_nonce = transfer_result.sender_nonce_after) ;
  let* tx_object = get_tx_object ~endpoint ~tx_hash:transfer_result.tx_hash in
  (* The prod and dev modes are desynchronized, as the encoding for `v` has
     changed. It should be fixed after the next freeze. *)
  let tx_object = {tx_object with v = transfer_result.tx_object.v} in
  assert (tx_object = transfer_result.tx_object) ;
  let*@! tx_receipt =
    Rpc.get_transaction_receipt
      ~tx_hash:transfer_result.tx_hash
      full_evm_setup.evm_node
  in
  assert (tx_receipt = transfer_result.tx_receipt) ;
  unit

let make_transfer ?data ~value ~sender ~receiver full_evm_setup =
  let endpoint = Evm_node.endpoint full_evm_setup.evm_node in
  let balance account = Eth_cli.balance ~account ~endpoint in
  let* sender_balance_before = balance sender.Eth_account.address in
  let* receiver_balance_before = balance receiver.Eth_account.address in
  let*@ sender_nonce_before =
    Rpc.get_transaction_count full_evm_setup.evm_node ~address:sender.address
  in
  let* tx_hash = send ~sender ~receiver ~value ?data full_evm_setup in
  let* () = check_tx_succeeded ~endpoint ~tx:tx_hash in
  let* sender_balance_after = balance sender.address in
  let* receiver_balance_after = balance receiver.address in
  let*@ sender_nonce_after =
    Rpc.get_transaction_count full_evm_setup.evm_node ~address:sender.address
  in
  let* tx_object = get_tx_object ~endpoint ~tx_hash in
  let*@! tx_receipt =
    Rpc.get_transaction_receipt ~tx_hash full_evm_setup.evm_node
  in
  return
    {
      sender_balance_before;
      sender_balance_after;
      sender_nonce_before;
      sender_nonce_after;
      value;
      tx_hash;
      tx_object;
      tx_receipt;
      receiver_balance_before;
      receiver_balance_after;
    }

let transfer ?data ~da_fee_per_byte ~expected_execution_gas ~evm_setup () =
  let* base_fee_per_gas = Rpc.get_gas_price evm_setup.evm_node in
  let sender, receiver =
    (Eth_account.bootstrap_accounts.(0), Eth_account.bootstrap_accounts.(1))
  in
  let* {
         sender_balance_before;
         sender_balance_after;
         sender_nonce_before;
         sender_nonce_after;
         value;
         tx_object;
         receiver_balance_before;
         receiver_balance_after;
         _;
       } =
    make_transfer
      ?data
      ~value:Wei.(Helpers.default_bootstrap_account_balance - one_eth)
      ~sender
      ~receiver
      evm_setup
  in
  let* receipt =
    Eth_cli.get_receipt ~endpoint:evm_setup.endpoint ~tx:tx_object.hash
  in
  let gas_used, gas_price =
    match receipt with
    | Some Transaction.{status = true; gasUsed; effectiveGasPrice; _} ->
        (gasUsed, effectiveGasPrice)
    | _ -> Test.fail "Transaction didn't succeed"
  in
  let fees = expected_gas_fees ~gas_price ~gas_used in
  Check.(
    Wei.(sender_balance_after = sender_balance_before - value - fees) Wei.typ)
    ~error_msg:
      "Unexpected sender balance after transfer, should be %R, but got %L" ;
  Check.(Wei.(receiver_balance_after = receiver_balance_before + value) Wei.typ)
    ~error_msg:
      "Unexpected receiver balance after transfer, should be %R, but got %L" ;
  Check.((sender_nonce_after = Int64.succ sender_nonce_before) int64)
    ~error_msg:
      "Unexpected sender nonce after transfer, should be %R, but got %L" ;
  (* Perform some sanity checks on the transaction object produced by the
     kernel. *)
  Check.((tx_object.from = sender.address) string)
    ~error_msg:"Unexpected transaction's sender" ;
  Check.((tx_object.to_ = Some receiver.address) (option string))
    ~error_msg:"Unexpected transaction's receiver" ;
  Check.((tx_object.value = value) Wei.typ)
    ~error_msg:"Unexpected transaction's value" ;
  let data = Option.value ~default:"0x" data in
  let data = String.sub data 2 (String.length data - 2) in
  let data_size = Bytes.length @@ Hex.to_bytes @@ `Hex data in
  check_tx_gas_for_fee
    ~da_fee_per_byte
    ~expected_execution_gas
    ~gas_used
    ~gas_price
    ~base_fee_per_gas
    ~data_size ;
  unit

let test_l2_transfer =
  let da_fee_per_byte = Wei.of_eth_string "0.000002" in
  let expected_execution_gas = 21000 in
  let test_f ~protocol:_ ~evm_setup =
    transfer ~evm_setup ~da_fee_per_byte ~expected_execution_gas ()
  in
  let title = "Check L2 transfers are applied" in
  let tags = ["evm"; "l2_transfer"] in
  register_both ~title ~tags ~da_fee_per_byte test_f

let test_chunked_transaction =
  let da_fee_per_byte = Wei.of_eth_string "0.000002" in
  let expected_execution_gas = 117000 in
  let test_f ~protocol:_ ~evm_setup =
    transfer
      ~data:("0x" ^ String.make 12_000 'a')
      ~da_fee_per_byte
      ~evm_setup
      ~expected_execution_gas
      ()
  in
  let title = "Check L2 chunked transfers are applied" in
  let tags = ["evm"; "l2_transfer"; "chunked"] in
  register_both ~title ~tags ~da_fee_per_byte test_f

let test_rpc_txpool_content =
  register_sequencer
    ~return_sequencer:true (* See {Note: TX Pool RPC mode} *)
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "txpool_content"]
    ~title:"Check RPC txpool_content is available"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~time_between_blocks:Nothing
  @@ fun ~protocol:_ ~evm_setup:{evm_node; produce_block; _} ->
  let get_transaction_field transaction_content field_name =
    transaction_content |> JSON.get field_name |> JSON.as_string_opt
    |> Option.value ~default:"null"
  in
  let check_transaction_content ~transaction_content ~blockHash ~blockNumber
      ~from ~gas ~gasPrice ~hash ~input ~nonce ~to_ ~transactionIndex ~value ~v
      ~r ~s =
    Check.(
      (get_transaction_field transaction_content "blockHash" = blockHash) string)
      ~error_msg:"Expected block hash to be %%R, got %%L." ;
    Check.(
      (get_transaction_field transaction_content "blockNumber" = blockNumber)
        string)
      ~error_msg:"Expected block number to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "from" = from) string)
      ~error_msg:"Expected caller to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "gas" = gas) string)
      ~error_msg:"Expected gas to be %%R, got %%L." ;
    Check.(
      (get_transaction_field transaction_content "gasPrice" = gasPrice) string)
      ~error_msg:"Expected gas price to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "hash" = hash) string)
      ~error_msg:"Expected hash to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "input" = input) string)
      ~error_msg:"Expected input to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "nonce" = nonce) string)
      ~error_msg:"Expected nonce to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "to" = to_) string)
      ~error_msg:"Expected callee to be %%R, got %%L." ;
    Check.(
      (get_transaction_field transaction_content "transactionIndex"
      = transactionIndex)
        string)
      ~error_msg:"Expected transaction index to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "value" = value) string)
      ~error_msg:"Expected value to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "v" = v) string)
      ~error_msg:"Expected v to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "r" = r) string)
      ~error_msg:"Expected r to be %%R, got %%L." ;
    Check.((get_transaction_field transaction_content "s" = s) string)
      ~error_msg:"Expected s to be %%R, got %%L."
  in
  let* tx1 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:100_000
      ~gas:23_300
      ~value:(Wei.of_string "100")
      ~address:"0x11d3c9168db9d12a3c591061d555870969b43dc9"
      ()
  in
  let* tx2 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:100_000
      ~gas:23_300
      ~value:(Wei.of_string "100")
      ~address:"0x11d3c9168db9d12a3c591061d555870969b43dc9"
      ()
  in
  let*@ tx_hash1 = Rpc.send_raw_transaction ~raw_tx:tx1 evm_node in

  let*@! transaction_object =
    Rpc.get_transaction_by_hash ~transaction_hash:tx_hash1 evm_node
  in

  Check.(
    ((transaction_object.hash = tx_hash1) string)
      ~error_msg:"Incorrect transaction hash, should be %R, but got %L.") ;
  Check.(
    ((transaction_object.blockHash = None) (option string))
      ~error_msg:"Incorrect block hash, should be %R, but got %L.") ;
  Check.(
    ((transaction_object.blockNumber = None) (option int32))
      ~error_msg:"Incorrect block number, should be %R, but got %L.") ;
  Check.(
    ((transaction_object.transactionIndex = None) (option int32))
      ~error_msg:"Incorrect transaction index, should be %R, but got %L.") ;
  let*@ _tx_hash2 = Rpc.send_raw_transaction ~raw_tx:tx2 evm_node in
  let*@ txpool_pending, txpool_queued = Rpc.txpool_content evm_node in
  Check.((List.length txpool_pending = 1) int)
    ~error_msg:
      "Expected number of addresses with pending transaction to be %R, got %L." ;
  Check.((List.length txpool_queued = 1) int)
    ~error_msg:
      "Expected number of addresses with queued transaction to be %R, got %L." ;
  let transaction_addr_pending = List.nth txpool_pending 0 in
  let transaction_addr_queued = List.nth txpool_queued 0 in
  Check.(
    (transaction_addr_pending.address
   = "0x6ce4d79d4e77402e1ef3417fdda433aa744c6e1c")
      string)
    ~error_msg:"Expected caller of transaction_1 to be %R, got %L." ;
  Check.(
    (transaction_addr_queued.address
   = "0x6ce4d79d4e77402e1ef3417fdda433aa744c6e1c")
      string)
    ~error_msg:"Expected caller of transaction_2 to be %R, got %L." ;
  let num_pending_transaction_addr_1 =
    List.length transaction_addr_pending.transactions
  in
  let num_queued_transaction_addr_1 =
    List.length transaction_addr_queued.transactions
  in
  Check.((num_pending_transaction_addr_1 = 1) int)
    ~error_msg:"Expected number of pending transaction to be %R, got %L." ;
  Check.((num_queued_transaction_addr_1 = 1) int)
    ~error_msg:"Expected number of queued transaction to be %R, got %L." ;
  let pending_transaction_addr_1_nonce, pending_transaction_addr_1_content =
    List.nth transaction_addr_pending.transactions 0
  in
  let queued_transaction_addr_1_nonce, queued_transaction_addr_1_content =
    List.nth transaction_addr_queued.transactions 0
  in
  Check.((pending_transaction_addr_1_nonce = 0L) int64)
    ~error_msg:"Expected nonce pending transaction to be %R, got %L." ;

  Check.((queued_transaction_addr_1_nonce = 1L) int64)
    ~error_msg:"Expected nonce queued transaction to be %R, got %L." ;

  let () =
    check_transaction_content
      ~transaction_content:pending_transaction_addr_1_content
      ~blockHash:"null"
      ~blockNumber:"null"
      ~from:"0x6ce4d79d4e77402e1ef3417fdda433aa744c6e1c"
      ~gas:"0x5b04"
      ~gasPrice:"0x186a0"
      ~hash:"0xed148f664807dfcb3c7095de22a6c63e72ae4f9d503549c525f5014327b51693"
      ~input:"0x"
      ~nonce:"0x0"
      ~to_:"0x11d3c9168db9d12a3c591061d555870969b43dc9"
      ~transactionIndex:"null"
      ~value:"0x64"
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7194
           v is currently incorrectly encoded as big-endian by the kernel,
           causing the decoded value to be incorrect. Should be 0xa96 here *)
      ~v:"0xa96"
      ~r:"0x4217494c4c98d5f8015399c004e088d094fcee43bcb9a4a6b29bdff27d6f1079"
      ~s:"0x23ca4eeac30b72e7582f2fcd9a151a855ae943ffb40f4a3ef616f5ae5483a592"
  in

  let () =
    check_transaction_content
      ~transaction_content:queued_transaction_addr_1_content
      ~blockHash:"null"
      ~blockNumber:"null"
      ~from:"0x6ce4d79d4e77402e1ef3417fdda433aa744c6e1c"
      ~gas:"0x5b04"
      ~gasPrice:"0x186a0"
      ~hash:"0x38b2831803a0f9a82bcc68f79bf167311b0adfe9e3d111a7f9579cdfcbae0f0f"
      ~input:"0x"
      ~nonce:"0x1"
      ~to_:"0x11d3c9168db9d12a3c591061d555870969b43dc9"
      ~transactionIndex:"null"
      ~value:"0x64"
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7194
           v is currently incorrectly encoded as big-endian by the kernel,
           causing the decoded value to be incorrect. Should be 0xa95 here *)
      ~v:"0xa95"
      ~r:"0x30d35547c7d39738a85fd6e96d9c9308070b83f334d64f51a94404d20902f970"
      ~s:"0x45ccee6d401d77df59f6831b7d73d1e3df7a9584070f45c117f55a9b81fa997c"
  in
  let*@ _level = produce_block () in
  let*@ txpool_pending, txpool_queued = Rpc.txpool_content evm_node in

  Check.((List.length txpool_pending = 1) int)
    ~error_msg:
      "Expected number of addresses with pending transaction to be %%R, got \
       %%L." ;
  Check.((List.length txpool_queued = 0) int)
    ~error_msg:
      "Expected number of addresses with queued transaction to be %%R, got %%L." ;
  let transaction_addr_pending = List.nth txpool_pending 0 in
  let num_pending_transaction_addr_1 =
    List.length transaction_addr_pending.transactions
  in
  Check.((num_pending_transaction_addr_1 = 1) int)
    ~error_msg:"Expected number of pending transaction to be %%R, got %%L." ;
  unit

let test_rpc_web3_clientVersion =
  register_both
    ~tags:["evm"; "rpc"; "client_version"]
    ~title:"Check RPC web3_clientVersion"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* web3_clientVersion =
    Evm_node.(
      call_evm_rpc evm_node {method_ = "web3_clientVersion"; parameters = `A []})
  in
  let* server_version = evm_node_version evm_node |> Runnable.run in
  Check.(
    (JSON.(web3_clientVersion |-> "result" |> as_string)
    = JSON.as_string server_version)
      string)
    ~error_msg:"Expected version %%R, got %%L." ;
  unit

let test_rpc_web3_sha3 =
  register_both ~tags:["evm"; "rpc"; "sha3"] ~title:"Check RPC web3_sha3"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  (* From the example provided in
     https://ethereum.org/en/developers/docs/apis/json-rpc/#web3_sha3 *)
  let input_data = "0x68656c6c6f20776f726c64" in
  let expected_reply =
    "0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
  in
  let* web3_sha3 =
    Evm_node.(
      call_evm_rpc
        evm_node
        {method_ = "web3_sha3"; parameters = `A [`String input_data]})
  in
  Check.((JSON.(web3_sha3 |-> "result" |> as_string) = expected_reply) string)
    ~error_msg:"Expected hash %%R, got %%L." ;
  unit

let test_simulate =
  register_proxy
    ~tags:["evm"; "simulate"]
    ~title:"A block can be simulated in the rollup node"
    (fun ~protocol:_ ~evm_setup:{evm_node; sc_rollup_node; _} ->
      let*@ block_number = Rpc.block_number evm_node in
      let* simulation_result =
        Sc_rollup_node.RPC.call sc_rollup_node
        @@ Sc_rollup_rpc.post_global_block_simulate
             ~insight_requests:
               [
                 `Durable_storage_key
                   ["evm"; "world_state"; "blocks"; "current"; "number"];
               ]
             []
      in
      let simulated_block_number =
        match simulation_result.insights with
        | [insight] -> Option.map Helpers.hex_string_to_int insight
        | _ -> None
      in
      Check.(
        (simulated_block_number = Some (Int32.to_int block_number + 1))
          (option int))
        ~error_msg:"The simulation should advance one L2 block" ;
      unit)

let test_full_blocks =
  register_proxy
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["evm"; "full_blocks"]
    ~title:
      "Check `eth_getBlockByNumber` with full blocks returns the correct \
       informations"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup:{evm_node; produce_block; _} ->
  let txs =
    read_tx_from_file ()
    |> List.filteri (fun i _ -> i < 5)
    |> List.map (fun (tx, _hash) -> tx)
  in
  let* _requests, receipt, _hashes =
    send_n_transactions ~evm_node ~produce_block txs
  in
  let* block =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_getBlockByNumber";
          parameters =
            `A [`String (Format.sprintf "%#lx" receipt.blockNumber); `Bool true];
        })
  in
  let block = block |> Evm_node.extract_result |> Block.of_json in
  let block_number = block.number in
  (match block.Block.transactions with
  | Block.Empty -> Test.fail "Expected a non empty block"
  | Block.Full transactions ->
      List.iteri
        (fun index
             ({blockHash; blockNumber; transactionIndex; _} :
               Transaction.transaction_object) ->
          Check.((Some block.hash = blockHash) (option string))
            ~error_msg:
              (sf "The transaction should be in block %%L but found %%R") ;
          Check.((Some block_number = blockNumber) (option int32))
            ~error_msg:
              (sf "The transaction should be in block %%L but found %%R") ;
          Check.((Some (Int32.of_int index) = transactionIndex) (option int32))
            ~error_msg:
              (sf "The transaction should be at index %%L but found %%R"))
        transactions
  | Block.Hash _ -> Test.fail "Block is supposed to contain transaction objects") ;
  unit

let test_latest_block =
  register_proxy
    ~tags:["evm"; "blocks"; "latest"]
    ~title:
      "Check `eth_getBlockByNumber` works correctly when asking for the \
       `latest`"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; produce_block; _} ->
  let*@ _ = produce_block () in
  (* The first execution of the kernel actually builds two blocks: the genesis
     block and the block for the current inbox. As such, the latest block is
     always of level 1. *)
  let* latest_block =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_getBlockByNumber";
          parameters = `A [`String "latest"; `Bool false];
        })
  in
  let latest_block = latest_block |> Evm_node.extract_result |> Block.of_json in
  Check.((latest_block.Block.number = 1l) int32)
    ~error_msg:"Expected latest being block number %R, but got %L" ;
  unit

let test_eth_call_nullable_recipient =
  register_both
    ~tags:["evm"; "eth_call"; "null"]
    ~title:"Check `eth_call.to` input can be null"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* call_result =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_call";
          parameters = `A [`O [("to", `Null)]; `String "latest"];
        })
  in
  (* Check the RPC returns a `result`. *)
  let _result = call_result |> Evm_node.extract_result in
  unit

let test_eth_call_contract_create =
  (* See https://github.com/safe-global/safe-singleton-factory/issues/545 *)
  register_both
    ~tags:["evm"; "eth_call"; "contract_create"]
    ~title:"Check eth_call with contract creation"
    ~kernels:[Latest]
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* call_result =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_call";
          parameters =
            `A
              [
                `O
                  [
                    ( "data",
                      `String
                        "0x604580600e600039806000f350fe7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe03601600081602082378035828234f58015156039578182fd5b8082525050506014600cf3"
                    );
                  ];
                `String "latest";
              ];
        })
  in
  let call_result = Evm_node.extract_result call_result |> JSON.as_string in
  Check.(
    (call_result
   = "0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe03601600081602082378035828234f58015156039578182fd5b8082525050506014600cf3"
    )
      string)
    ~error_msg:"test" ;
  unit

let test_inject_100_transactions =
  register_proxy
    ~tags:["evm"; "bigger_blocks"]
    ~title:"Check blocks can contain more than 64 transactions"
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup:{evm_node; produce_block; _} ->
  (* Retrieves all the messages and prepare them for the current rollup. *)
  let txs = read_tx_from_file () |> List.map (fun (tx, _hash) -> tx) in
  let* requests, receipt, _hashes =
    send_n_transactions ~produce_block ~evm_node txs
  in
  let* block_with_100tx =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_getBlockByNumber";
          parameters =
            `A
              [`String (Format.sprintf "%#lx" receipt.blockNumber); `Bool false];
        })
  in
  let block_with_100tx =
    block_with_100tx |> Evm_node.extract_result |> Block.of_json
  in
  (match block_with_100tx.Block.transactions with
  | Block.Empty -> Test.fail "Expected a non empty block"
  | Block.Full _ ->
      Test.fail "Block is supposed to contain only transaction hashes"
  | Block.Hash hashes ->
      Check.((List.length hashes = List.length requests) int)
        ~error_msg:"Expected %R transactions in the latest block, got %L") ;

  let* _level = produce_block () in
  let*@ latest_evm_level = Rpc.block_number evm_node in
  (* At each loop, the kernel reads the previous block. Until the patch, the
     kernel failed to read the previous block if there was more than 64 hash,
     this test ensures it works by assessing new blocks are produced. *)
  Check.((latest_evm_level >= Int32.succ block_with_100tx.Block.number) int32)
    ~error_msg:
      "Expected a new block after the one with 100 transactions, but level \
       hasn't changed" ;
  unit

let check_estimate_gas {evm_node; _} eth_call expected_gas =
  (* Make the call to the EVM node. *)
  let*@ r = Rpc.estimate_gas eth_call evm_node in
  (* Check the RPC result. *)
  Check.((r >= expected_gas) int64)
    ~error_msg:"Expected result greater than %R, but got %L" ;
  unit

let check_eth_call {evm_node; _} eth_call expected_result =
  (* Make the call to the EVM node. *)
  let* call_result =
    Evm_node.(
      call_evm_rpc
        evm_node
        {method_ = "eth_call"; parameters = `A [`O eth_call; `String "latest"]})
  in
  (* Check the RPC result. *)
  let r = call_result |> Evm_node.extract_result in
  Check.((JSON.as_string r = expected_result) string)
    ~error_msg:"Expected result %R, but got %L" ;
  unit

let test_eth_call_large =
  let test_f ~protocol:_ ~evm_setup =
    let sender = Eth_account.bootstrap_accounts.(0) in
    (* large request *)
    let eth_call =
      [
        ("to", Ezjsonm.encode_string sender.address);
        ("data", Ezjsonm.encode_string ("0x" ^ String.make 12_000 'a'));
      ]
    in

    check_eth_call evm_setup eth_call "0x"
  in
  let title = "eth_call with a large amount of data" in
  let tags = ["evm"; "eth_call"; "simulate"; "large"; "simple_storage"] in
  register_both ~title ~tags test_f

let test_eth_call_input =
  let test_f ~protocol:_ ~evm_setup =
    let sender = Eth_account.bootstrap_accounts.(0) in
    let eth_call =
      [
        ("to", Ezjsonm.encode_string sender.address);
        ("input", Ezjsonm.encode_string "0xcafe");
      ]
    in

    check_eth_call evm_setup eth_call "0x"
  in
  let title = "eth_call with input instead of data" in
  let tags = ["evm"; "eth_call"; "simulate"; "input"] in
  register_both ~title ~tags test_f

let test_estimate_gas =
  let test_f ~protocol:_ ~evm_setup =
    (* large request *)
    let* simple_storage_resolved = simple_storage () in
    let data = read_file simple_storage_resolved.bin in
    let eth_call = [("data", Ezjsonm.encode_string @@ "0x" ^ data)] in

    check_estimate_gas evm_setup eth_call 23423L
  in

  let title = "eth_estimateGas for contract creation" in
  let tags = ["evm"; "eth_estimategas"; "simulate"; "simple_storage"] in
  register_both ~title ~tags test_f

let test_estimate_gas_additionnal_field =
  let test_f ~protocol:_ ~evm_setup =
    (* large request *)
    let* simple_storage_resolved = simple_storage () in
    let data = read_file simple_storage_resolved.bin in
    let eth_call =
      [
        ( "from",
          Ezjsonm.encode_string @@ "0x6ce4d79d4e77402e1ef3417fdda433aa744c6e1c"
        );
        ("data", Ezjsonm.encode_string @@ "0x" ^ data);
        ("value", Ezjsonm.encode_string @@ "0x0");
        (* for some reason remix adds the "type" field *)
        ("type", Ezjsonm.encode_string @@ "0x1");
      ]
    in

    check_estimate_gas evm_setup eth_call 23423L
  in
  let title = "eth_estimateGas allows additional fields" in
  let tags =
    ["evm"; "eth_estimategas"; "simulate"; "remix"; "simple_storage"]
  in
  register_both ~title ~tags test_f

let test_eth_call_storage_contract =
  let test_f ~protocol:_ ~evm_setup:({evm_node; endpoint; _} as evm_setup) =
    let sender = Eth_account.bootstrap_accounts.(0) in
    let* simple_storage_resolved = simple_storage () in

    (* deploy contract *)
    let* address, tx =
      deploy ~contract:simple_storage_resolved ~sender evm_setup
    in
    let* () = check_tx_succeeded ~endpoint ~tx in
    Check.(
      (String.lowercase_ascii address
      = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")
        string
        ~error_msg:"Expected address to be %R but was %L.") ;

    (* craft request *)
    let data = "0x4e70b1dc" in
    let eth_call =
      [
        ("to", Ezjsonm.encode_string address);
        ("data", Ezjsonm.encode_string data);
      ]
    in

    (* make call to proxy *)
    let* call_result =
      Evm_node.(
        call_evm_rpc
          evm_node
          {
            method_ = "eth_call";
            parameters = `A [`O eth_call; `String "latest"];
          })
    in

    let r = call_result |> Evm_node.extract_result in
    Check.(
      (JSON.as_string r
     = "0x0000000000000000000000000000000000000000000000000000000000000000")
        string)
      ~error_msg:"Expected result %R, but got %L" ;

    let* tx = send_call_set_storage_simple address sender 42 evm_setup in
    let* () = check_tx_succeeded ~endpoint ~tx in

    (* make call to proxy *)
    let* call_result =
      Evm_node.(
        call_evm_rpc
          evm_node
          {
            method_ = "eth_call";
            parameters = `A [`O eth_call; `String "latest"];
          })
    in
    let r = call_result |> Evm_node.extract_result in
    Check.(
      (JSON.as_string r
     = "0x000000000000000000000000000000000000000000000000000000000000002a")
        string)
      ~error_msg:"Expected result %R, but got %L" ;
    unit
  in
  let title = "Call a view" in
  let tags = ["evm"; "eth_call"; "simulate"; "simple_storage"] in
  register_both ~title ~tags test_f

let test_eth_call_storage_contract_eth_cli =
  let test_f ~protocol:_
      ~evm_setup:({evm_node; endpoint; produce_block; _} as evm_setup) =
    (* sanity *)
    let* call_result =
      Evm_node.(
        call_evm_rpc
          evm_node
          {
            method_ = "eth_call";
            parameters = `A [`O [("to", `Null)]; `String "latest"];
          })
    in
    (* Check the RPC returns a `result`. *)
    let _result = call_result |> Evm_node.extract_result in

    let sender = Eth_account.bootstrap_accounts.(0) in

    let* simple_storage_resolved = simple_storage () in

    (* deploy contract send send 42 *)
    let* address, _tx =
      deploy ~contract:simple_storage_resolved ~sender evm_setup
    in
    let* tx = send_call_set_storage_simple address sender 42 evm_setup in
    let* () = check_tx_succeeded ~endpoint ~tx in

    (* make a call to proxy through eth-cli *)
    let call_num =
      Eth_cli.contract_call
        ~endpoint
        ~abi_label:simple_storage_resolved.label
        ~address
        ~method_call:"num()"
    in
    let* res = wait_for_application ~produce_block call_num in

    Check.((String.trim res = "42") string)
      ~error_msg:"Expected result %R, but got %L" ;
    unit
  in
  let title = "Call a view through an ethereum client" in
  let tags = ["evm"; "eth_call"; "simulate"; "simple_storage"] in

  register_both ~title ~tags test_f

let test_preinitialized_evm_kernel =
  let admin = Constant.bootstrap1 in
  register_proxy
    ~tags:["evm"; "administrator"; "config"]
    ~title:"Creates a kernel with an initialized administrator key"
    ~admin
  @@ fun ~protocol:_ ~evm_setup:{sc_rollup_node; l1_contracts; _} ->
  let* found_administrator_key_hex =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:Durable_storage_path.admin
         ()
  in
  let found_administrator_key =
    Option.map
      (fun administrator -> Hex.to_string (`Hex administrator))
      found_administrator_key_hex
  in
  Check.(
    (Option.map (fun l -> l.admin) l1_contracts = found_administrator_key)
      (option string))
    ~error_msg:
      (sf "Expected to read %%L as administrator key, but found %%R instead") ;
  unit

let deposit ~amount_mutez ~bridge ~depositor ~receiver ~produce_block
    ~sc_rollup_address client =
  let* () =
    Client.transfer
      ~entrypoint:"deposit"
      ~arg:(sf "Pair %S %s" sc_rollup_address receiver)
      ~amount:amount_mutez
      ~giver:depositor.Account.public_key_hash
      ~receiver:bridge
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in

  let* _ = produce_block () in
  unit

let call_withdraw ?expect_failure ~sender ~endpoint ~value ~produce_block
    ~receiver () =
  let* () =
    Eth_cli.add_abi ~label:"withdraw" ~abi:(withdrawal_abi_path ()) ()
  in
  let call_withdraw =
    Eth_cli.contract_send
      ?expect_failure
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint
      ~abi_label:"withdraw"
      ~address:"0xff00000000000000000000000000000000000001"
      ~method_call:(sf {|withdraw_base58("%s")|} receiver)
      ~value
      ~gas:16_000_000
  in
  wait_for_application ~produce_block call_withdraw

let withdraw ~commitment_period ~challenge_window ~amount_wei ~sender ~receiver
    ~produce_block ~evm_node ~sc_rollup_node ~sc_rollup_address ~client
    ~endpoint =
  let* withdrawal_level = Client.level client in
  (* Call the withdrawal precompiled contract. *)
  let* _tx =
    call_withdraw
      ~produce_block
      ~sender
      ~endpoint
      ~value:amount_wei
      ~receiver
      ()
  in
  let* _ =
    find_and_execute_withdrawal
      ~withdrawal_level
      ~commitment_period
      ~challenge_window
      ~evm_node
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ()
  in
  unit

let check_balance ~receiver ~endpoint expected_balance =
  let* balance = Eth_cli.balance ~account:receiver ~endpoint in
  let balance = Wei.truncate_to_mutez balance in
  Check.((balance = Tez.to_mutez expected_balance) int)
    ~error_msg:(sf "Expected balance of %s should be %%R, but got %%L" receiver) ;
  unit

let test_deposit_and_withdraw =
  let admin = Constant.bootstrap5 in
  let commitment_period = 5 and challenge_window = 5 in
  register_proxy
    ~tags:["evm"; "deposit"; "withdraw"]
    ~title:"Deposit and withdraw tez"
    ~admin
    ~commitment_period
    ~challenge_window
  @@ fun ~protocol:_
             ~evm_setup:
               {
                 client;
                 sc_rollup_address;
                 l1_contracts;
                 sc_rollup_node;
                 endpoint;
                 evm_node;
                 produce_block;
                 _;
               } ->
  let {
    bridge;
    admin = _;
    kernel_governance = _;
    exchanger = _;
    sequencer_governance = _;
    kernel_security_governance = _;
  } =
    match l1_contracts with
    | Some x -> x
    | None -> Test.fail ~__LOC__ "The test needs the L1 bridge"
  in

  let amount_mutez = Tez.of_mutez_int 100_000_000 in
  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
      }
  in

  let* () =
    deposit
      ~amount_mutez
      ~sc_rollup_address
      ~bridge
      ~depositor:admin
      ~receiver:receiver.address
      ~produce_block
      client
  in
  let* () = check_balance ~receiver:receiver.address ~endpoint amount_mutez in

  let amount_wei = Wei.of_tez amount_mutez in
  (* Keep a small amount to pay for the gas. *)
  let amount_wei = Wei.(amount_wei - one_eth) in

  let withdraw_receiver = "tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX" in
  let* _tx =
    withdraw
      ~produce_block
      ~evm_node
      ~sc_rollup_address
      ~commitment_period
      ~challenge_window
      ~amount_wei
      ~sender:receiver
      ~receiver:withdraw_receiver
      ~sc_rollup_node
      ~client
      ~endpoint
  in

  let* balance = Client.get_balance_for ~account:withdraw_receiver client in
  let expected_balance = Tez.(amount_mutez - one) in
  Check.((balance = expected_balance) Tez.typ)
    ~error_msg:(sf "Expected %%R amount instead of %%L after withdrawal") ;
  return ()

let test_withdraw_amount =
  let admin = Constant.bootstrap5 in
  register_proxy
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
     Replace by [Any] after the next upgrade *)
    ~kernels:[Latest]
    ~tags:["evm"; "withdraw"; "wei"; "mutez"]
    ~title:"Minimum amout to withdraw"
    ~admin
  @@ fun ~protocol:_ ~evm_setup:{endpoint; produce_block; _} ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* Minimal amount of Wei fails with revert. *)
  let* _err =
    call_withdraw
      ~produce_block
      ~expect_failure:true
      ~sender
      ~endpoint
      ~receiver:"tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX"
      ~value:Wei.one
      ()
  in
  (* 1mutez is 10^12wei. The minimal accepted value is then 10^12 *)
  let* _ok =
    call_withdraw
      ~produce_block
      ~sender
      ~endpoint
      ~receiver:"tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX"
      ~value:(Wei.of_string "1000000000000")
      ()
  in
  let* _err =
    call_withdraw
      ~expect_failure:true
      ~sender
      ~endpoint
      ~produce_block
      ~receiver:"tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX"
      ~value:(Wei.of_string "99999999999")
      ()
  in
  (* 1000000000001 must be refused, as 1 wei will be lost during the conversion
     to mutez. *)
  let* _err =
    call_withdraw
      ~expect_failure:true
      ~sender
      ~endpoint
      ~produce_block
      ~receiver:"tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX"
      ~value:(Wei.of_string "1000000000001")
      ()
  in
  unit

let test_withdraw_via_calls =
  let admin = Constant.bootstrap5 in
  register_proxy
    ~kernels:[Kernel.Latest]
    ~tags:["evm"; "withdraw"; "call"; "staticcall"; "delegatecall"; "callcode"]
    ~title:"Withdrawal via different kind of calls"
    ~admin
  @@ fun ~protocol:_ ~evm_setup:({endpoint; produce_block; _} as evm_setup) ->
  let sender = Eth_account.bootstrap_accounts.(0) in

  let* contract, _tx = deploy ~contract:call_withdrawal ~sender evm_setup in

  (* Call works, it transfers funds to the precompiled contract and produce
     a withdrawal. *)
  let call =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:call_withdrawal.label
      ~address:contract
      ~method_call:"testCall()"
      ~gas:16_000_000
      ~value:(Wei.of_eth_int 1)
  in
  let* tx = wait_for_application ~produce_block call in
  let* () = check_tx_succeeded ~endpoint ~tx in

  (* Delegate call does not produce a transfer, the precompiled contract
     reverts. *)
  let delegate_call =
    Eth_cli.contract_send
      ~expect_failure:true
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:call_withdrawal.label
      ~address:contract
      ~method_call:"testDelegatecall()"
      ~gas:16_000_000
      ~value:(Wei.of_eth_int 1)
  in
  let* tx = wait_for_application ~produce_block delegate_call in
  let* () = check_tx_failed ~endpoint ~tx in

  (* Static call does not produce a transfer, the precompiled contract
     reverts. *)
  let static_call =
    Eth_cli.contract_send
      ~expect_failure:true
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:call_withdrawal.label
      ~address:contract
      ~method_call:"testStaticcall()"
      ~gas:16_000_000
      ~value:(Wei.of_eth_int 1)
  in
  let* tx = wait_for_application ~produce_block static_call in
  let* () = check_tx_failed ~endpoint ~tx in

  (* Deploy a new contract that uses CALLCODE, it's in a different contract
     because solidity deprecation blablabla. *)
  let* contract, _tx = deploy ~contract:callcode_withdrawal ~sender evm_setup in

  (* Static call does not produce a transfer, the precompiled contract
     reverts. *)
  let callcode =
    Eth_cli.contract_send
      ~expect_failure:true
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:callcode_withdrawal.label
      ~address:contract
      ~method_call:"testCallcode()"
      ~gas:16_000_000
      ~value:(Wei.of_eth_int 1)
  in
  let* tx = wait_for_application ~produce_block callcode in
  let* () = check_tx_failed ~endpoint ~tx in
  unit

let get_kernel_boot_wasm ~sc_rollup_node =
  let rpc_hooks : RPC_core.rpc_hooks =
    let on_request _verb ~uri:_ _data = Regression.capture "<boot.wasm>" in
    let on_response _status ~body:_ = Regression.capture "<boot.wasm>" in
    {on_request; on_response}
  in
  let* kernel_boot_opt =
    Sc_rollup_node.RPC.call sc_rollup_node ~log_response_body:false ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:Durable_storage_path.kernel_boot_wasm
         ()
  in
  match kernel_boot_opt with
  | Some boot_wasm -> return boot_wasm
  | None -> failwith "Kernel `boot.wasm` should be accessible/readable."

let gen_test_kernel_upgrade ?setup_kernel_root_hash ?admin_contract ?timestamp
    ?(activation_timestamp = "0") ?evm_setup ?rollup_address
    ?(should_fail = false) ~installee ?with_administrator ?expect_l1_failure
    ?(admin = Constant.bootstrap1) ?(upgrador = admin) protocol =
  let* {
         node;
         client;
         sc_rollup_node;
         sc_rollup_address;
         evm_node;
         l1_contracts;
         produce_block;
         _;
       } =
    match evm_setup with
    | Some evm_setup -> return evm_setup
    | None ->
        setup_evm_kernel
          ?setup_kernel_root_hash
          ?timestamp
          ?with_administrator
          ~admin:(Some admin)
          protocol
  in
  let admin_contract =
    match admin_contract with
    | Some x -> x
    | None ->
        let l1_contracts = Option.get l1_contracts in
        l1_contracts.admin
  in
  let sc_rollup_address =
    Option.value ~default:sc_rollup_address rollup_address
  in
  let preimages_dir = Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0" in
  let* {root_hash; _} =
    Sc_rollup_helpers.prepare_installer_kernel ~preimages_dir installee
  in
  let* payload = Evm_node.upgrade_payload ~root_hash ~activation_timestamp in
  let* kernel_boot_wasm_before_upgrade = get_kernel_boot_wasm ~sc_rollup_node in
  let* expected_kernel_boot_wasm =
    if should_fail then return kernel_boot_wasm_before_upgrade
    else return @@ Hex.show @@ Hex.of_string @@ read_file (Uses.path installee)
  in
  let* () =
    let* () =
      Client.transfer
        ?expect_failure:expect_l1_failure
        ~amount:Tez.zero
        ~giver:upgrador.public_key_hash
        ~receiver:admin_contract
        ~arg:(sf {|Pair "%s" 0x%s|} sc_rollup_address payload)
        ~burn_cap:Tez.one
        client
    in
    let*@ _ = produce_block () in
    unit
  in
  let* kernel_boot_wasm_after_upgrade = get_kernel_boot_wasm ~sc_rollup_node in
  Check.((expected_kernel_boot_wasm = kernel_boot_wasm_after_upgrade) string)
    ~error_msg:(sf "Unexpected `boot.wasm`.") ;
  return
    ( sc_rollup_node,
      node,
      client,
      evm_node,
      kernel_boot_wasm_before_upgrade,
      root_hash,
      produce_block )

let test_kernel_upgrade_evm_to_evm =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade integrity to itself"
  @@ fun protocol ->
  let* _sc_rollup_node, _node, _client, evm_node, _, _root_hash, produce_block =
    gen_test_kernel_upgrade ~installee:Constant.WASM.evm_kernel protocol
  in
  (* We ensure the upgrade went well by checking if the kernel still produces
     blocks. *)
  let endpoint = Evm_node.endpoint evm_node in
  check_block_progression ~produce_block ~endpoint ~expected_block_level:2

let test_kernel_upgrade_wrong_key =
  Protocol.register_test
    ~__FILE__
    ~tags:["administrator"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade fails with a wrong administrator key"
  @@ fun protocol ->
  let* _ =
    gen_test_kernel_upgrade
      ~expect_l1_failure:true
      ~should_fail:true
      ~installee:Constant.WASM.debug_kernel
      ~admin:Constant.bootstrap1
      ~upgrador:Constant.bootstrap2
      protocol
  in
  unit

let test_kernel_upgrade_wrong_rollup_address =
  Protocol.register_test
    ~__FILE__
    ~tags:["address"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade fails with a wrong rollup address"
  @@ fun protocol ->
  let* _ =
    gen_test_kernel_upgrade
      ~expect_l1_failure:true
      ~rollup_address:"sr1T13qeVewVm3tudQb8dwn8qRjptNo7KVkj"
      ~should_fail:true
      ~installee:Constant.WASM.debug_kernel
      protocol
  in
  unit

let test_kernel_upgrade_no_administrator =
  Protocol.register_test
    ~__FILE__
    ~tags:["administrator"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade fails if there is no administrator"
  @@ fun protocol ->
  let* _ =
    gen_test_kernel_upgrade
      ~should_fail:true
      ~installee:Constant.WASM.debug_kernel
      ~with_administrator:false
      protocol
  in
  unit

let test_kernel_upgrade_failing_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["migration"; "upgrade"; "failed"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.failed_migration;
      ])
    ~title:"Ensures EVM kernel's upgrade rollback when migration fails"
  @@ fun protocol ->
  let* ( sc_rollup_node,
         _node,
         _client,
         evm_node,
         _original_kernel_boot_wasm,
         root_hash,
         produce_block ) =
    gen_test_kernel_upgrade
      ~setup_kernel_root_hash:false
      ~installee:Constant.WASM.failed_migration
      ~should_fail:true
      protocol
  in
  let*@! found_root_hash = Rpc.tez_kernelRootHash evm_node in
  Check.((root_hash <> found_root_hash) string)
    ~error_msg:"The failed migration should not upgrade the kernel root hash" ;
  Check.(
    ("000000000000000000000000000000000000000000000000000000000000000000"
   = found_root_hash)
      string)
    ~error_msg:"The fallback root hash should be %L, got %R" ;
  (* We make sure that we can't read under the tmp file, after migration failed,
     everything is reverted. *)
  let* tmp_dummy =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:"/tmp/__dummy"
         ()
  in
  (match tmp_dummy with
  | Some _ -> failwith "Nothing should be readable under the temporary dir."
  | None -> ()) ;
  (* We ensure that the fallback mechanism went well by checking if the
     kernel still produces blocks since it has booted back to the previous,
     original kernel. *)
  let*@ _ = produce_block () in
  let endpoint = Evm_node.endpoint evm_node in
  check_block_progression ~produce_block ~endpoint ~expected_block_level:3

let test_kernel_upgrade_via_governance =
  Protocol.register_test
    ~__FILE__
    ~tags:["migration"; "upgrade"; "kernel_governance"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Kernel upgrades using governance contract"
  @@ fun protocol ->
  let admin = Constant.bootstrap1 in
  let* evm_setup =
    setup_evm_kernel ~with_administrator:true ~admin:(Some admin) protocol
  in
  let l1_contracts = Option.get evm_setup.l1_contracts in
  let* _ =
    gen_test_kernel_upgrade
      ~evm_setup
      ~installee:Constant.WASM.debug_kernel
      ~admin_contract:l1_contracts.kernel_governance
      protocol
  in
  unit

let test_kernel_upgrade_via_kernel_security_governance =
  Protocol.register_test
    ~__FILE__
    ~tags:["migration"; "upgrade"; "kernel_security_governance"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Kernel upgrades using security governance contract"
  @@ fun protocol ->
  let admin = Constant.bootstrap1 in
  let* evm_setup =
    setup_evm_kernel ~with_administrator:true ~admin:(Some admin) protocol
  in
  let l1_contracts = Option.get evm_setup.l1_contracts in
  let* _ =
    gen_test_kernel_upgrade
      ~evm_setup
      ~installee:Constant.WASM.debug_kernel
      ~admin_contract:l1_contracts.kernel_security_governance
      protocol
  in
  unit

let test_rpc_sendRawTransaction =
  register_both
    ~tags:["evm"; "rpc"; "tx_hash"; "raw_tx"]
    ~title:
      "Ensure EVM node returns appropriate hash for any given transactions."
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* tx1 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(0).address
      ()
  in
  let* tx2 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let txs = [tx1; tx2] in
  let* hashes =
    Lwt_list.map_p
      (fun raw_tx ->
        let*@ hash = Rpc.send_raw_transaction ~raw_tx evm_node in
        return hash)
      txs
  in
  let expected_hashes =
    [
      "0xb941cbf32821471381b6f003f9013b95c788ad24260d2af54848a5b504c09bb0";
      "0x6ddd857feb6c81405ea50fb12489ea00cd91193ae36cb41fc119999521422e3a";
    ]
  in
  Check.((hashes = expected_hashes) (list string))
    ~error_msg:"Unexpected returned hash, should be %R, but got %L" ;
  unit

let by_block_arg_string by =
  match by with `Hash -> "Hash" | `Number -> "Number"

let get_transaction_by_block_arg_and_index_request ~by arg index =
  let by = by_block_arg_string by in
  Evm_node.
    {
      method_ = "eth_getTransactionByBlock" ^ by ^ "AndIndex";
      parameters = `A [`String arg; `String index];
    }

let get_transaction_by_block_arg_and_index ~by evm_node block_hash index =
  let* transaction_object =
    Evm_node.call_evm_rpc
      evm_node
      (get_transaction_by_block_arg_and_index_request ~by block_hash index)
  in
  return
    JSON.(
      transaction_object |-> "result" |> Transaction.transaction_object_of_json)

let test_rpc_getTransactionByBlockArgAndIndex ~by ~evm_setup =
  let {evm_node; produce_block; _} = evm_setup in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 3) in
  let* _, _, hashes =
    send_n_transactions ~produce_block ~evm_node (List.map fst txs)
  in
  Lwt_list.iter_s
    (fun transaction_hash ->
      let* receipt =
        wait_for_application
          ~produce_block
          (wait_for_transaction_receipt ~evm_node ~transaction_hash)
      in
      let block_arg, index =
        ( (match by with
          | `Hash -> receipt.blockHash
          | `Number -> Int32.to_string receipt.blockNumber),
          receipt.transactionIndex )
      in
      let* transaction_object =
        get_transaction_by_block_arg_and_index
          ~by
          evm_node
          block_arg
          (Int32.to_string index)
      in
      Check.(
        ((transaction_object.hash = transaction_hash) string)
          ~error_msg:"Incorrect transaction hash, should be %R, but got %L.") ;
      unit)
    hashes

let test_rpc_getCode =
  register_both
    ~tags:["evm"; "rpc"; "get_code"; "simple_storage"]
    ~title:"RPC method eth_getCode"
  @@ fun ~protocol:_ ~evm_setup ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved = simple_storage () in
  let* address, _ =
    deploy ~contract:simple_storage_resolved ~sender evm_setup
  in
  let*@ code = Rpc.get_code ~address evm_setup.evm_node in
  let expected_code = simple_storage_code in
  Check.((code = expected_code) string)
    ~error_msg:"Expected code is %R, but got %L" ;
  unit

let test_rpc_getTransactionByHash =
  register_both
    ~tags:["evm"; "rpc"; "get_transaction_by"; "transaction_by_hash"]
    ~title:"RPC method eth_getTransactionByHash"
    ~da_fee_per_byte:(Wei.of_eth_string "0.000004")
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; produce_block; _} = evm_setup in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  let value = Wei.one_eth in
  let estimateGas =
    [
      ("from", `String sender.address);
      ("to", `String receiver.address);
      ("value", `String (Wei.to_string value));
    ]
  in
  let*@ expected_gas = Rpc.estimate_gas estimateGas evm_node in
  let submitted_gas = Int64.mul expected_gas 2L in
  let* expected_gas_price =
    Rpc.get_gas_price evm_node |> Lwt.map Int64.of_int32
  in
  let submitted_gas_price = Int64.mul expected_gas_price 2L in
  let send =
    Eth_cli.transaction_send
      ~source_private_key:sender.Eth_account.private_key
      ~to_public_key:receiver.Eth_account.address
      ~value
      ~endpoint:(Evm_node.endpoint evm_node)
      ~gas_price:(Wei.to_wei_z (Z.of_int64 submitted_gas_price))
      ~gas_limit:(Z.of_int64 submitted_gas)
  in
  let* transaction_hash = wait_for_application ~produce_block send in
  let*@! transaction_object =
    Rpc.get_transaction_by_hash ~transaction_hash evm_node
  in
  Check.(
    ((transaction_object.hash = transaction_hash) string)
      ~error_msg:"Incorrect transaction hash, should be %R, but got %L.") ;
  Check.(
    ((transaction_object.gas = submitted_gas) int64)
      ~error_msg:"Incorrect gas on transaction, should be %R, but got %L.") ;
  Check.(
    ((transaction_object.gasPrice = submitted_gas_price) int64)
      ~error_msg:"Incorrect gasPrice on transaction, should be %R, but got %L.") ;

  unit

let test_rpc_getTransactionByBlockHashAndIndex =
  register_both
    ~tags:["evm"; "rpc"; "get_transaction_by"; "block_hash_and_index"]
    ~title:"RPC method eth_getTransactionByBlockHashAndIndex"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~bootstrap_accounts:Eth_account.lots_of_address
  @@ fun ~protocol:_ -> test_rpc_getTransactionByBlockArgAndIndex ~by:`Hash

let test_rpc_getTransactionByBlockNumberAndIndex =
  register_both
    ~tags:["evm"; "rpc"; "get_transaction_by"; "block_number_and_index"]
    ~title:"RPC method eth_getTransactionByBlockNumberAndIndex"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~bootstrap_accounts:Eth_account.lots_of_address
  @@ fun ~protocol:_ -> test_rpc_getTransactionByBlockArgAndIndex ~by:`Number

type storage_migration_results = {
  transfer_result : transfer_result;
  block_result : Block.t;
  config_result : config_result;
  simple_storage_address : string;
}

(* This is the test generator that will trigger the sanity checks for migration
   tests.
   Note that:
   - it uses the latest version of the ghostnet EVM rollup as a starter kernel.
   - the upgrade of the kernel during the test will always target the latest one
     on master.
   - everytime a new path/rpc/object is stored in the kernel, a new sanity check
     MUST be generated. *)
let gen_kernel_migration_test ~from ~to_ ?bootstrap_accounts ?chain_id
    ?(admin = Constant.bootstrap5) ~scenario_prior ~scenario_after protocol =
  let* evm_setup =
    setup_evm_kernel
      ?chain_id
      ?bootstrap_accounts
      ~da_fee_per_byte:Wei.zero
      ~minimum_base_fee_per_gas:(Wei.of_string "21000")
      ~kernel:from
      ~admin:(Some admin)
      protocol
  in
  (* Load the EVM rollup's storage and sanity check results. *)
  let* evm_node =
    Evm_node.init ~mode:Proxy (Sc_rollup_node.endpoint evm_setup.sc_rollup_node)
  in
  let endpoint = Evm_node.endpoint evm_node in
  let* sanity_check =
    scenario_prior ~evm_setup:{evm_setup with evm_node; endpoint}
  in
  (* Upgrade the kernel. *)
  let _, to_use = Kernel.to_uses_and_tags to_ in
  let* _ =
    gen_test_kernel_upgrade ~evm_setup ~installee:to_use ~admin protocol
  in
  let* _ =
    (* wait for the migration to be processed *)
    next_evm_level
      ~evm_node
      ~sc_rollup_node:evm_setup.sc_rollup_node
      ~client:evm_setup.client
  in
  let* evm_node =
    Evm_node.init ~mode:Proxy (Sc_rollup_node.endpoint evm_setup.sc_rollup_node)
  in
  let evm_setup = {evm_setup with evm_node} in
  (* Check the values after the upgrade with [sanity_check] results. *)
  scenario_after ~evm_setup ~sanity_check

let test_mainnet_ghostnet_kernel_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.ghostnet_evm_kernel;
        Constant.WASM.mainnet_evm_kernel;
      ])
    ~title:
      "Ensures EVM kernel's upgrade succeeds with potential migration(s). \
       (mainnet -> ghostnet)"
  @@ fun protocol ->
  let sender, receiver, deployer =
    ( Eth_account.bootstrap_accounts.(0),
      Eth_account.bootstrap_accounts.(1),
      Eth_account.bootstrap_accounts.(2) )
  in
  let* simple_storage = simple_storage () in
  let scenario_prior ~evm_setup =
    let* transfer_result =
      make_transfer
        ~value:Wei.(Helpers.default_bootstrap_account_balance - one_eth)
        ~sender
        ~receiver
        evm_setup
    in
    let*@ block_result = latest_block evm_setup.evm_node in
    let* config_result = config_setup evm_setup in
    let* simple_storage_address, _ =
      deploy ~contract:simple_storage ~sender:deployer evm_setup
    in
    let* () =
      set_and_get_simple_storage_check
        ~sender:deployer
        ~number:42
        ~address:simple_storage_address
        ~error_prefix:"Prior migration"
        evm_setup
    in
    return
      {transfer_result; block_result; config_result; simple_storage_address}
  in
  let scenario_after ~evm_setup ~sanity_check =
    let* () =
      ensure_transfer_result_integrity
        ~sender
        ~receiver
        ~transfer_result:sanity_check.transfer_result
        evm_setup
    in
    let* () =
      ensure_block_integrity ~block_result:sanity_check.block_result evm_setup
    in
    let* () =
      set_and_get_simple_storage_check
        ~sender:deployer
        ~number:24
        ~address:sanity_check.simple_storage_address
        ~error_prefix:"After migration"
        evm_setup
    in
    let* seconde_simple_storage_address, _ =
      deploy ~contract:simple_storage ~sender:deployer evm_setup
    in
    let* () =
      set_and_get_simple_storage_check
        ~sender:deployer
        ~number:42
        ~address:seconde_simple_storage_address
        ~error_prefix:"After migration"
        evm_setup
    in

    ensure_config_setup_integrity
      ~config_result:sanity_check.config_result
      evm_setup
  in
  gen_kernel_migration_test
    ~from:Mainnet
    ~to_:Ghostnet
    ~scenario_prior
    ~scenario_after
    protocol

let test_latest_kernel_migration protocols =
  let latest_kernel_migration ~from =
    let from_tag, from_use = Kernel.to_uses_and_tags from in
    Protocol.register_test
      ~__FILE__
      ~tags:["evm"; "migration"; "upgrade"; from_tag]
      ~uses:(fun _protocol ->
        [
          Constant.octez_smart_rollup_node;
          Constant.octez_evm_node;
          Constant.smart_rollup_installer;
          Constant.WASM.evm_kernel;
          from_use;
        ])
      ~title:
        Format.(
          sprintf
            "Ensures EVM kernel's upgrade succeeds with potential migration(s) \
             (%s -> latest)."
            from_tag)
    @@ fun protocol ->
    let sender, receiver, deployer =
      ( Eth_account.bootstrap_accounts.(0),
        Eth_account.bootstrap_accounts.(1),
        Eth_account.bootstrap_accounts.(2) )
    in
    let* simple_storage = simple_storage () in
    let scenario_prior ~evm_setup =
      let* transfer_result =
        make_transfer
          ~value:Wei.(Helpers.default_bootstrap_account_balance - one_eth)
          ~sender
          ~receiver
          evm_setup
      in
      let*@ block_result = latest_block evm_setup.evm_node in
      let* config_result = config_setup evm_setup in
      let* indexes =
        Sc_rollup_node.RPC.call
          evm_setup.sc_rollup_node
          ~rpc_hooks:Tezos_regression.rpc_hooks
        @@ Sc_rollup_rpc.get_global_block_durable_state_value
             ~pvm_kind
             ~operation:Sc_rollup_rpc.Subkeys
             ~key:Durable_storage_path.indexes
             ()
      in
      let* simple_storage_address, _ =
        deploy ~contract:simple_storage ~sender:deployer evm_setup
      in
      let* () =
        set_and_get_simple_storage_check
          ~sender:deployer
          ~number:42
          ~address:simple_storage_address
          ~error_prefix:"Prior migration"
          evm_setup
      in
      let indexes = List.sort compare indexes in
      Check.((indexes = ["accounts"; "blocks"; "transactions"]) (list string))
        ~error_msg:"Expected indexes to be %R, got %L" ;
      return
        {transfer_result; block_result; config_result; simple_storage_address}
    in
    let scenario_after ~evm_setup ~sanity_check =
      let* indexes =
        Sc_rollup_node.RPC.call
          evm_setup.sc_rollup_node
          ~rpc_hooks:Tezos_regression.rpc_hooks
        @@ Sc_rollup_rpc.get_global_block_durable_state_value
             ~pvm_kind
             ~operation:Sc_rollup_rpc.Subkeys
             ~key:Durable_storage_path.indexes
             ()
      in
      Check.((indexes = ["blocks"]) (list string))
        ~error_msg:"Expected indexes to be %R, got %L" ;
      let* () =
        ensure_transfer_result_integrity
          ~sender
          ~receiver
          ~transfer_result:sanity_check.transfer_result
          evm_setup
      in
      let* () =
        ensure_block_integrity ~block_result:sanity_check.block_result evm_setup
      in
      let* () =
        set_and_get_simple_storage_check
          ~sender:deployer
          ~number:24
          ~address:sanity_check.simple_storage_address
          ~error_prefix:"After migration"
          evm_setup
      in
      let* seconde_simple_storage_address, _ =
        deploy ~contract:simple_storage ~sender:deployer evm_setup
      in
      let* () =
        set_and_get_simple_storage_check
          ~sender:deployer
          ~number:42
          ~address:seconde_simple_storage_address
          ~error_prefix:"After migration"
          evm_setup
      in
      ensure_config_setup_integrity
        ~config_result:sanity_check.config_result
        evm_setup
    in
    gen_kernel_migration_test
      ~from
      ~to_:Latest
      ~scenario_prior
      ~scenario_after
      protocol
  in
  latest_kernel_migration ~from:Ghostnet protocols ;
  latest_kernel_migration ~from:Mainnet protocols

let test_cannot_prepayed_leads_to_no_inclusion =
  register_both
    ~tags:["evm"; "prepay"; "inclusion"]
    ~title:
      "Not being able to prepay a transaction leads to it not being included."
    ~bootstrap_accounts:[]
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  (* No bootstrap accounts, so no one has funds. *)
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1).  We do not use eth-cli in
     this test because we want the results of the simulation. *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_string "1000000000000000000")
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let*@? error = Rpc.send_raw_transaction ~raw_tx:raw_transfer evm_node in
  Check.(((error.code = -32003) int) ~error_msg:"The transaction should fail") ;
  Check.(
    ((error.message = "Cannot prepay transaction.") string)
      ~error_msg:
        "The transaction should be rejected for not being able to prepay") ;
  unit

let test_cannot_prepayed_with_delay_leads_to_no_injection =
  register_both
    ~tags:["evm"; "prepay"; "injection"]
    ~title:
      "Not being able to prepay a transaction that was included leads to it \
       not being injected."
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup:{evm_node; endpoint; produce_block; _} ->
  let sender, to_public_key =
    ( Eth_account.bootstrap_accounts.(0),
      "0xE7f682c226d7269C7247b878B3F94c7a8d31FEf5" )
  in
  let transaction_included =
    Eth_cli.transaction_send
      ~source_private_key:sender.Eth_account.private_key
      ~to_public_key
      ~value:Wei.one
      ~endpoint
  in
  (* Transaction from previous sender to the same address but with nonce 1 and
     a gas computation that will lead it to not being able to be prepayed hence
     rejected at injection. *)
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce:1
      ~value:Wei.zero
      ~gas:100_000
      ~gas_price:100_000
      ~address:to_public_key
      ()
  in
  let*@ transaction_hash = Rpc.send_raw_transaction ~raw_tx evm_node in
  let* _will_succeed =
    wait_for_application ~produce_block transaction_included
  in
  let*@ _ = produce_block () in
  let wait_for_failure () =
    let* _ =
      wait_for_application
        ~produce_block
        (wait_for_transaction_receipt ~evm_node ~transaction_hash)
    in
    Test.fail "Unreachable state, transaction will never be injected."
  in
  Lwt.catch wait_for_failure (function _ -> unit)

let test_deposit_before_and_after_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "deposit"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Deposit before and after migration"
  @@ fun protocol ->
  let admin = Constant.bootstrap5 in
  let receiver = "0x119811f34EF4491014Fbc3C969C426d37067D6A4" in
  let amount_mutez = Tez.of_mutez_int 50_000_000 in

  let scenario_prior
      ~evm_setup:
        {l1_contracts; sc_rollup_address; client; endpoint; produce_block; _} =
    let {bridge; _} =
      match l1_contracts with Some x -> x | None -> assert false
    in
    let* () =
      deposit
        ~amount_mutez
        ~bridge
        ~depositor:admin
        ~receiver
        ~sc_rollup_address
        ~produce_block
        client
    in
    check_balance ~receiver ~endpoint amount_mutez
  in
  let scenario_after
      ~evm_setup:
        {l1_contracts; sc_rollup_address; client; endpoint; produce_block; _}
      ~sanity_check:() =
    let {bridge; _} =
      match l1_contracts with Some x -> x | None -> assert false
    in
    let* () =
      deposit
        ~amount_mutez
        ~bridge
        ~depositor:admin
        ~receiver
        ~produce_block
        ~sc_rollup_address
        client
    in
    check_balance ~receiver ~endpoint Tez.(amount_mutez + amount_mutez)
  in
  gen_kernel_migration_test
    ~from:Ghostnet
    ~to_:Latest
    ~admin
    ~scenario_prior
    ~scenario_after
    protocol

let test_block_storage_before_and_after_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "block"; "storage"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Block storage before and after migration"
  @@ fun protocol ->
  let block_id = "1" in
  let scenario_prior ~evm_setup:{endpoint; produce_block; _} =
    let*@ _ = produce_block () in
    let* (block : Block.t) = Eth_cli.get_block ~block_id ~endpoint in
    return block
  in
  let scenario_after ~evm_setup:{endpoint; _} ~(sanity_check : Block.t) =
    let* (block : Block.t) = Eth_cli.get_block ~block_id ~endpoint in
    (* Compare fields stored before migration *)
    assert (block.number = sanity_check.number) ;
    assert (block.hash = sanity_check.hash) ;
    assert (block.timestamp = sanity_check.timestamp) ;
    assert (block.transactions = sanity_check.transactions) ;
    unit
  in
  gen_kernel_migration_test
    ~from:Ghostnet
    ~to_:Latest
    ~scenario_prior
    ~scenario_after
    protocol

let test_kernel_upgrade_version_change =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"; "version"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Kernel version changes after an upgrade"
  @@ fun protocol ->
  let scenario_prior ~evm_setup =
    let*@ old_ = Rpc.tez_kernelVersion evm_setup.evm_node in
    return old_
  in
  let scenario_after ~evm_setup ~sanity_check:old =
    let*@ new_ = Rpc.tez_kernelVersion evm_setup.evm_node in
    Check.((old <> new_) string)
      ~error_msg:"The kernel version must change after an upgrade" ;
    unit
  in
  gen_kernel_migration_test
    ~from:Ghostnet
    ~to_:Latest
    ~scenario_prior
    ~scenario_after
    protocol

(** This tests that giving epoch (or any timestamps from the past) as
    the activation timestamp results in a immediate upgrade. *)
let test_kernel_upgrade_epoch =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"; "timestamp"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Upgrade immediatly when activation timestamp is epoch"
  @@ fun protocol ->
  let* _ =
    gen_test_kernel_upgrade
      ~activation_timestamp:"0"
      ~installee:Constant.WASM.debug_kernel
      protocol
  in
  unit

(** This tests that the kernel waits the activation timestamp to apply
    the upgrade.  *)
let test_kernel_upgrade_delay =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"; "timestamp"; "delay"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Upgrade after a delay when activation timestamp is in the future"
  @@ fun protocol ->
  let timestamp = Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z")) in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  (* It shoulnd't be upgrade in a single block, which {!gen_test_kernel_upgrade}
     expect. *)
  let* sc_rollup_node, _node, _client, _evm_node, _, _root_hash, produce_block =
    gen_test_kernel_upgrade
      ~timestamp
      ~activation_timestamp
      ~installee:Constant.WASM.debug_kernel
      ~should_fail:true
      protocol
  in
  let* _ =
    repeat 5 (fun _ ->
        let*@ _ = produce_block () in
        unit)
  in
  let kernel_debug_content = read_file (Uses.path Constant.WASM.debug_kernel) in
  let* kernel = get_kernel_boot_wasm ~sc_rollup_node in
  Check.((kernel <> kernel_debug_content) string)
    ~error_msg:(sf "The kernel hasn't upgraded") ;
  unit

let test_transaction_storage_before_and_after_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "transaction"; "storage"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Transaction storage before and after migration"
  @@ fun protocol ->
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 3) in
  let raw_txs, tx_hashes = List.split txs in
  let check_one evm_setup tx_hash =
    let*@ _receipt = Rpc.get_transaction_receipt ~tx_hash evm_setup.evm_node in
    let* _tx_object = get_tx_object ~endpoint:evm_setup.endpoint ~tx_hash in
    unit
  in
  let scenario_prior ~evm_setup:({produce_block; evm_node; _} as evm_setup) =
    let* _requests, _receipt, _hashes =
      send_n_transactions ~produce_block ~evm_node raw_txs
    in
    Lwt_list.iter_p (check_one evm_setup) tx_hashes
  in
  let scenario_after ~evm_setup ~sanity_check:() =
    Lwt_list.iter_p (check_one evm_setup) tx_hashes
  in
  gen_kernel_migration_test
    ~from:Ghostnet
    ~to_:Latest
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~scenario_prior
    ~scenario_after
    protocol

(* TODO: remove me after Ghostnet upgrade *)
let test_fa_bridge_flag_after_migration_v14 ~kernel_from ~chain_id ~chain_id_hex
    ~flag_expected =
  let chain_name, kernel_wasm_const =
    match kernel_from with
    | Kernel.Mainnet -> ("mainnet", Constant.WASM.mainnet_evm_kernel)
    | _ -> failwith "Unsupported chain"
  in
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "v14"; "fa_bridge"; "flag"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        kernel_wasm_const;
      ])
    ~title:
      (sf
         "FA bridge flag before and after migration v14 [%s -> latest]"
         chain_name)
  @@ fun protocol ->
  let assert_chain_id ~sc_rollup_node ~expected =
    let* chain_id =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:"/evm/chain_id"
           ()
    in
    assert (Option.get chain_id = expected) ;
    unit
  in
  let assert_fa_bridge_flag ~sc_rollup_node ~expected =
    let* flag =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:"/evm/feature_flags/enable_fa_bridge"
           ()
    in
    assert (Option.is_some flag = expected) ;
    unit
  in
  let scenario_prior ~evm_setup:{sc_rollup_node; _} =
    let* () = assert_chain_id ~sc_rollup_node ~expected:chain_id_hex in
    let* () = assert_fa_bridge_flag ~sc_rollup_node ~expected:false in
    unit
  in
  let scenario_after ~evm_setup:{sc_rollup_node; _} ~(sanity_check : unit) =
    let () = sanity_check in
    let* () = assert_chain_id ~sc_rollup_node ~expected:chain_id_hex in
    let* () = assert_fa_bridge_flag ~sc_rollup_node ~expected:flag_expected in
    unit
  in
  gen_kernel_migration_test
    ~from:kernel_from
    ~to_:Latest
    ~scenario_prior
    ~scenario_after
    ~chain_id
    protocol

let test_kernel_root_hash_originate_absent =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "kernel_root_hash"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Kernel root hash is absent at origination if not provided"
  @@ fun protocol ->
  let* {evm_node; _} =
    setup_evm_kernel ~admin:None ~setup_kernel_root_hash:false protocol
  in
  let*@ kernel_root_hash_opt = Rpc.tez_kernelRootHash evm_node in
  Assert.is_none ~loc:__LOC__ kernel_root_hash_opt ;
  unit

let test_kernel_root_hash_originate_present =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "kernel_root_hash"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"tez_kernelRootHash takes root hash provided by the installer"
  @@ fun protocol ->
  let* {evm_node; kernel_root_hash; _} =
    setup_evm_kernel ~admin:None ~setup_kernel_root_hash:true protocol
  in
  let*@! found_kernel_root_hash = Rpc.tez_kernelRootHash evm_node in
  Check.((kernel_root_hash = found_kernel_root_hash) string)
    ~error_msg:
      "tez_kernelRootHash should return root hash set by installer after \
       origination" ;
  unit

let test_kernel_root_hash_after_upgrade =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "kernel_root_hash"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"tez_kernelRootHash is set after upgrade"
  @@ fun protocol ->
  let* _sc_rollup_node, _node, _client, evm_node, _, root_hash, _produce_block =
    gen_test_kernel_upgrade
      ~activation_timestamp:"0"
      ~installee:Constant.WASM.evm_kernel
      protocol
  in
  let*@! found_kernel_root_hash = Rpc.tez_kernelRootHash evm_node in
  Check.((found_kernel_root_hash = root_hash) string)
    ~error_msg:"Found incorrect kernel root hash (expected %L, got %R)" ;
  unit

let test_storage_migration_v18 protocols =
  let storage_migration_v18 ~from ~number_blocks =
    let from_tag, from_use = Kernel.to_uses_and_tags from in
    Protocol.register_test
      ~__FILE__
      ~tags:
        [
          "evm";
          "migration";
          "upgrade";
          from_tag;
          "v18";
          string_of_int number_blocks;
        ]
      ~uses:(fun _protocol ->
        [
          Constant.octez_smart_rollup_node;
          Constant.octez_evm_node;
          Constant.smart_rollup_installer;
          Constant.WASM.evm_kernel;
          from_use;
        ])
      ~title:
        Format.(
          sprintf
            "Test migration V18 with %d blocks (%s -> latest)."
            number_blocks
            from_tag)
    @@ fun protocol ->
    let scenario_prior ~evm_setup:{sc_rollup_node; client; evm_node; _} =
      let* () =
        repeat number_blocks (fun () ->
            let* _ = next_rollup_node_level ~sc_rollup_node ~client in
            unit)
      in
      let*@ head = Rpc.block_number evm_node in
      Check.((head = Int32.of_int number_blocks) int32)
        ~error_msg:"Expected head is %R, got %L" ;
      return ()
    in
    let scenario_after ~evm_setup:{sc_rollup_node; _} ~sanity_check:_ =
      let* blocks_subkeys =
        Sc_rollup_node.RPC.call
          sc_rollup_node
          ~rpc_hooks:Tezos_regression.rpc_hooks
        @@ Sc_rollup_rpc.get_global_block_durable_state_value
             ~pvm_kind
             ~operation:Sc_rollup_rpc.Subkeys
             ~key:"/evm/world_state/blocks"
             ()
      in

      List.iter
        (fun key ->
          if key = "current" || String.length key = 64 then ()
          else
            Test.fail
              "Any key that is not current or a block hash should have been \
               removed, but found %S"
              key)
        blocks_subkeys ;

      unit
    in
    gen_kernel_migration_test
      ~from
      ~to_:Latest
      ~scenario_prior
      ~scenario_after
      protocol
  in
  storage_migration_v18 ~from:Ghostnet ~number_blocks:5 protocols ;
  storage_migration_v18 ~from:Ghostnet ~number_blocks:256 protocols ;
  storage_migration_v18 ~from:Mainnet ~number_blocks:5 protocols ;
  storage_migration_v18 ~from:Mainnet ~number_blocks:256 protocols

let register_evm_migration ~protocols =
  test_latest_kernel_migration protocols ;
  test_mainnet_ghostnet_kernel_migration protocols ;
  test_deposit_before_and_after_migration protocols ;
  test_block_storage_before_and_after_migration protocols ;
  test_transaction_storage_before_and_after_migration protocols ;
  test_storage_migration_v18 protocols

let block_transaction_count_by ~by arg =
  let method_ = "eth_getBlockTransactionCountBy" ^ by_block_arg_string by in
  Evm_node.{method_; parameters = `A [`String arg]}

let get_block_transaction_count_by evm_node ~by arg =
  let* transaction_count =
    Evm_node.call_evm_rpc evm_node (block_transaction_count_by ~by arg)
  in
  return JSON.(transaction_count |-> "result" |> as_int64)

let test_rpc_getBlockTransactionCountBy =
  register_both
    ~tags:["evm"; "rpc"; "get_block_transaction_count_by"]
    ~title:
      "RPC methods eth_getBlockTransactionCountByHash and \
       eth_getBlockTransactionCountByNumber"
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup ->
  let {produce_block; evm_node; _} = evm_setup in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 5) in
  let* _, receipt, _ =
    send_n_transactions ~produce_block ~evm_node (List.map fst txs)
  in
  let* block = get_block_by_hash evm_setup receipt.blockHash in
  let expected_count =
    match block.transactions with
    | Empty -> 0L
    | Hash l -> Int64.of_int @@ List.length l
    | Full l -> Int64.of_int @@ List.length l
  in
  let* transaction_count =
    get_block_transaction_count_by evm_node ~by:`Hash receipt.blockHash
  in
  Check.((transaction_count = expected_count) int64)
    ~error_msg:
      "Expected %R transactions with eth_getBlockTransactionCountByHash, but \
       got %L" ;
  let* transaction_count =
    get_block_transaction_count_by
      evm_node
      ~by:`Number
      (Int32.to_string receipt.blockNumber)
  in
  Check.((transaction_count = expected_count) int64)
    ~error_msg:
      "Expected %R transactions with eth_getBlockTransactionCountByNumber, but \
       got %L" ;
  unit

let uncle_count_by_block_arg_request ~by arg =
  let method_ = "eth_getUncleCountByBlock" ^ by_block_arg_string by in
  Evm_node.{method_; parameters = `A [`String arg]}

let get_uncle_count_by_block_arg evm_node ~by arg =
  let* uncle_count =
    Evm_node.call_evm_rpc evm_node (uncle_count_by_block_arg_request ~by arg)
  in
  return JSON.(uncle_count |-> "result" |> as_int64)

let test_rpc_getUncleCountByBlock =
  register_both
    ~tags:["evm"; "rpc"; "get_uncle_count_by_block"]
    ~title:
      "RPC methods eth_getUncleCountByBlockHash and \
       eth_getUncleCountByBlockNumber"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* block = Eth_cli.get_block ~block_id:"0" ~endpoint:evm_node_endpoint in
  let* uncle_count =
    get_uncle_count_by_block_arg evm_node ~by:`Hash block.hash
  in
  Check.((uncle_count = Int64.zero) int64)
    ~error_msg:
      "Expected %R uncles with eth_getUncleCountByBlockHash, but got %L" ;
  let* uncle_count =
    get_uncle_count_by_block_arg
      evm_node
      ~by:`Number
      (Int32.to_string block.number)
  in
  Check.((uncle_count = Int64.zero) int64)
    ~error_msg:
      "Expected %R uncles with eth_getUncleCountByBlockNumber, but got %L" ;
  unit

let uncle_by_block_arg_and_index_request ~by arg index =
  let by = by_block_arg_string by in
  Evm_node.
    {
      method_ = "eth_getUncleByBlock" ^ by ^ "AndIndex";
      parameters = `A [`String arg; `String index];
    }

let get_uncle_by_block_arg_and_index ~by evm_node arg index =
  let* block =
    Evm_node.call_evm_rpc
      evm_node
      (uncle_by_block_arg_and_index_request ~by arg index)
  in
  let result = JSON.(block |-> "result") in
  if JSON.is_null result then return None
  else return @@ Some (result |> Block.of_json)

let test_rpc_getUncleByBlockArgAndIndex =
  register_both
    ~tags:["evm"; "rpc"; "get_uncle_by_block_arg_and_index"]
    ~title:
      "RPC methods eth_getUncleByBlockHashAndIndex and \
       eth_getUncleByBlockNumberAndIndex"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let block_id = "0" in
  let* block = Eth_cli.get_block ~block_id ~endpoint:evm_node_endpoint in
  let* uncle =
    get_uncle_by_block_arg_and_index ~by:`Hash evm_node block.hash block_id
  in
  assert (Option.is_none uncle) ;
  let* uncle =
    get_uncle_by_block_arg_and_index
      ~by:`Number
      evm_node
      (Int32.to_string block.number)
      block_id
  in
  assert (Option.is_none uncle) ;
  unit

let test_simulation_eip2200 =
  register_both
    ~tags:["evm"; "loop"; "simulation"; "eip2200"]
    ~title:"Simulation is EIP2200 resilient"
  @@ fun ~protocol:_ ~evm_setup ->
  let {produce_block; endpoint; _} = evm_setup in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* loop_resolved = loop () in
  let* loop_address, _tx = deploy ~contract:loop_resolved ~sender evm_setup in
  (* If we support EIP-2200, the simulation gives an amount of gas
     insufficient for the execution. As we do the simulation with an
     enormous gas limit, we never trigger EIP-2200. *)
  let call =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:loop_resolved.label
      ~address:loop_address
      ~method_call:"loop(5)"
  in
  let* _tx = wait_for_application ~produce_block call in
  unit

let test_rpc_sendRawTransaction_with_consecutive_nonce =
  register_both
    ~tags:["evm"; "rpc"; "tx_nonce"]
    ~title:"Can submit many transactions."
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup:{evm_node; produce_block; _} ->
  (* Nonce: 0*)
  let* tx_1 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:"0x0000000000000000000000000000000000000000"
      ()
  in
  let*@ hash_1 = Rpc.send_raw_transaction ~raw_tx:tx_1 evm_node in
  (* Nonce: 1*)
  let* tx_2 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:"0x0000000000000000000000000000000000000000"
      ()
  in
  let*@ hash_2 = Rpc.send_raw_transaction ~raw_tx:tx_2 evm_node in
  let* _ =
    wait_for_application
      ~produce_block
      (wait_for_transaction_receipt ~evm_node ~transaction_hash:hash_1)
  in
  let* _ =
    wait_for_application
      ~produce_block
      (wait_for_transaction_receipt ~evm_node ~transaction_hash:hash_2)
  in
  unit

let test_rpc_sendRawTransaction_not_included =
  register_both
    ~tags:["evm"; "rpc"; "tx_nonce"; "no_inclusion"]
    ~title:
      "Tx with nonce too high are not included without previous transactions."
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup:{evm_node; endpoint; produce_block; _} ->
  (* Nonce: 1 *)
  let* tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(0).address
      ()
  in
  let*@ tx_hash =
    wait_for_application ~produce_block (fun () ->
        Rpc.send_raw_transaction ~raw_tx:tx evm_node)
  in
  let*@ _ = produce_block () in
  (* Check if txs is not included *)
  let* receipt = Eth_cli.get_receipt ~endpoint ~tx:tx_hash in
  Check.((Option.is_none receipt = true) bool)
    ~error_msg:"Receipt should not be present" ;

  unit

let test_rpc_gasPrice =
  register_both
    ~tags:["evm"; "rpc"; "gas_price"]
    ~title:"RPC methods eth_gasPrice"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let expected_gas_price = Wei.of_gwei_string "1" in
  let* gas_price =
    Evm_node.(
      let* price =
        call_evm_rpc evm_node {method_ = "eth_gasPrice"; parameters = `A []}
      in
      return JSON.(price |-> "result" |> as_int64 |> Z.of_int64 |> Wei.to_wei_z))
  in
  Check.((gas_price = expected_gas_price) Wei.typ)
    ~error_msg:"Expected %R, but got %L" ;
  unit

let send_foo_mapping_storage contract_address sender
    {produce_block; endpoint; _} =
  let* mapping_storage_resolved = mapping_storage () in
  let call_foo (sender : Eth_account.t) =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:mapping_storage_resolved.label
      ~address:contract_address
      ~method_call:"foo()"
  in
  wait_for_application ~produce_block (call_foo sender)

let test_rpc_getStorageAt =
  register_both
    ~tags:["evm"; "rpc"; "get_storage_at"; "mapping_storage"]
    ~title:"RPC methods eth_getStorageAt"
  @@ fun ~protocol:_ ~evm_setup ->
  let {endpoint; evm_node; _} = evm_setup in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* mapping_storage_resolved = mapping_storage () in
  (* deploy contract *)
  let* address, _tx =
    deploy ~contract:mapping_storage_resolved ~sender evm_setup
  in
  (* Example from
      https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_getstorageat
  *)
  let expected_value0 = 1234 in
  let expected_value1 = 5678 in

  (* set values *)
  let* tx = send_foo_mapping_storage address sender evm_setup in
  let* () = check_tx_succeeded ~endpoint ~tx in
  let*@ hex_value = Rpc.get_storage_at ~address ~pos:"0x0" evm_node in
  Check.(
    (Durable_storage_path.no_0x hex_value
    = Helpers.hex_256_of_int expected_value0)
      string)
    ~error_msg:"Expected %R, but got %L" ;
  let pos = Helpers.mapping_position sender.address 1 in
  let*@ hex_value = Rpc.get_storage_at ~address ~pos evm_node in
  Check.(
    (Durable_storage_path.no_0x hex_value
    = Helpers.hex_256_of_int expected_value1)
      string)
    ~error_msg:"Expected %R, but got %L" ;
  unit

let test_originate_evm_kernel_and_dump_pvm_state =
  register_proxy
    ~tags:["evm"]
    ~title:"Originate EVM kernel with installer and dump PVM state"
  @@ fun ~protocol:_ ~evm_setup:{sc_rollup_node; produce_block; _} ->
  (* First run of the installed EVM kernel, it will initialize the directory
     "eth_accounts". *)
  let*@ _level = produce_block () in
  let dump = Temp.file "dump.json" in
  let* () = Sc_rollup_node.dump_durable_storage ~sc_rollup_node ~dump () in
  let installer = Installer_kernel_config.of_json dump in

  (* Check the consistency of the PVM state as queried by RPCs and the dumped PVM state. *)
  Lwt_list.iter_s
    (function
      (* We consider only the Set instruction because the dump durable storage
         command of the node produce only this instruction. *)
      | Installer_kernel_config.Set {value; to_} ->
          let* expected_value =
            Sc_rollup_node.RPC.call sc_rollup_node
            @@ Sc_rollup_rpc.get_global_block_durable_state_value
                 ~pvm_kind:"wasm_2_0_0"
                 ~operation:Sc_rollup_rpc.Value
                 ~key:to_
                 ()
          in
          let expected_value =
            match expected_value with
            | Some expected_value -> expected_value
            | None ->
                Test.fail "The key %S doesn't exist in the durable storage" to_
          in
          Check.((expected_value = value) string)
            ~error_msg:
              (sf "Value found in installer is %%R but expected %%L at %S" to_) ;
          unit
      | _ -> assert false)
    installer

(** Test that a contract can be called,
    and that the call can modify the storage.  *)
let test_l2_call_inter_contract =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_deploy"; "l2_call"; "inter_contract"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Check L2 inter contract call"
  @@ fun protocol ->
  (* setup *)
  let* ({produce_block; evm_node; sc_rollup_node; _} as evm_setup) =
    setup_evm_kernel ~admin:None protocol
  in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* callee_resolved = callee () in
  let* caller_resolved = caller () in

  (* deploy Callee contract *)
  let* callee_address, _tx =
    deploy ~contract:callee_resolved ~sender evm_setup
  in

  (* set 20 directly in the Callee *)
  let* tx =
    let call_set_directly (sender : Eth_account.t) n =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:callee_resolved.label
        ~address:callee_address
        ~method_call:(Printf.sprintf "setX(%d)" n)
    in
    wait_for_application ~produce_block (call_set_directly sender 20)
  in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address:callee_address 1 in
  let* () =
    check_nb_in_storage ~evm_setup ~address:callee_address ~nth:0 ~expected:20
  in

  (* deploy caller contract *)
  let* caller_address, _tx =
    deploy ~contract:caller_resolved ~sender evm_setup
  in

  (* set 10 through the caller *)
  let* tx =
    let call_set_from_caller (sender : Eth_account.t) n =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:caller_resolved.label
        ~address:caller_address
        ~method_call:(Printf.sprintf "setX(\"%s\", %d)" callee_address n)
    in
    wait_for_application ~produce_block (call_set_from_caller sender 10)
  in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address:callee_address 1 in
  let* () =
    check_nb_in_storage ~evm_setup ~address:callee_address ~nth:0 ~expected:10
  in
  unit

let get_logs_request ?from_block ?to_block ?address ?topics () =
  let parse_topic = function
    | [] -> `Null
    | [t] -> `String t
    | l -> `A (List.map (fun s -> `String s) l)
  in
  let parse_address = function
    | `Single a -> `String a
    | `List l -> `A (List.map (fun a -> `String a) l)
  in
  let parameters : JSON.u =
    `A
      [
        `O
          (Option.fold
             ~none:[]
             ~some:(fun f -> [("fromBlock", `String f)])
             from_block
          @ Option.fold
              ~none:[]
              ~some:(fun t -> [("toBlock", `String t)])
              to_block
          @ Option.fold
              ~none:[]
              ~some:(fun a -> [("address", parse_address a)])
              address
          @ Option.fold
              ~none:[]
              ~some:(fun t -> [("topics", `A (List.map parse_topic t))])
              topics);
      ]
  in
  Evm_node.{method_ = "eth_getLogs"; parameters}

let get_logs ?from_block ?to_block ?address ?topics evm_node =
  let* response =
    Evm_node.call_evm_rpc
      evm_node
      (get_logs_request ?from_block ?to_block ?address ?topics ())
  in
  return
    JSON.(response |-> "result" |> as_list |> List.map Transaction.logs_of_json)

let test_rpc_getLogs =
  register_both
    ~tags:["evm"; "rpc"; "get_logs"; "erc20"]
    ~title:"Check getLogs RPC"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; produce_block; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let player = Eth_account.bootstrap_accounts.(1) in
  let* erc20_resolved = erc20 () in
  (* deploy the contract *)
  let* address, _tx = deploy ~contract:erc20_resolved ~sender evm_setup in
  let address = String.lowercase_ascii address in
  Check.(
    (address = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")
      string
      ~error_msg:"Expected address to be %R but was %L.") ;
  (* minting / burning *)
  let call_mint (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:erc20_resolved.label
      ~address
      ~method_call:(Printf.sprintf "mint(%d)" n)
  in
  let call_burn ?(expect_failure = false) (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:erc20_resolved.label
      ~address
      ~method_call:(Printf.sprintf "burn(%d)" n)
  in
  let transfer_event_topic =
    let h =
      Tezos_crypto.Hacl.Hash.Keccak_256.digest
        (Bytes.of_string "Transfer(address,address,uint256)")
    in
    "0x" ^ Hex.show (Hex.of_bytes h)
  in
  let zero_address = "0x" ^ String.make 64 '0' in
  let burn_logs sender amount =
    [
      ( address,
        [transfer_event_topic; hex_256_of_address sender; zero_address],
        "0x" ^ Helpers.hex_256_of_int amount );
    ]
  in
  (* sender mints 42 *)
  let* tx1 = wait_for_application ~produce_block (call_mint sender 42) in
  (* player mints 100 *)
  let* _tx = wait_for_application ~produce_block (call_mint player 100) in
  (* sender burns 42 *)
  let* _tx = wait_for_application ~produce_block (call_burn sender 42) in
  (* Check that there have been 3 logs in total *)
  let* all_logs = get_logs ~from_block:"0" evm_node in
  Check.((List.length all_logs = 3) int) ~error_msg:"Expected %R logs, got %L" ;
  (* Check that the [address] contract has produced 3 logs in total *)
  let* contract_logs =
    get_logs ~from_block:"0" ~address:(`Single address) evm_node
  in
  Check.((List.length contract_logs = 3) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Same check also works if [address] is the second in the addresses
     list *)
  let* contract_logs =
    get_logs
      ~from_block:"0"
      ~address:(`List ["0x0000000000000000000000000000000000000000"; address])
      evm_node
  in
  Check.((List.length contract_logs = 3) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Check that there have been 3 logs with the transfer event topic *)
  let* transfer_logs =
    get_logs ~from_block:"0" ~topics:[[transfer_event_topic]] evm_node
  in
  Check.((List.length transfer_logs = 3) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Check that [sender] appears in 2 logs.
     Note: this would also match on a transfer from zero to zero. *)
  let* sender_logs =
    get_logs
      ~from_block:"0"
      ~topics:
        [
          [];
          [hex_256_of_address sender; zero_address];
          [hex_256_of_address sender; zero_address];
        ]
      evm_node
  in
  Check.((List.length sender_logs = 2) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Look for a specific log, for the sender burn. *)
  let* sender_burn_logs =
    get_logs
      ~from_block:"0"
      ~topics:
        [[transfer_event_topic]; [hex_256_of_address sender]; [zero_address]]
      evm_node
  in
  Check.(
    (List.map Transaction.extract_log_body sender_burn_logs
    = burn_logs sender 42)
      (list (tuple3 string (list string) string)))
    ~error_msg:"Expected logs %R, got %L" ;
  (* Check that a specific block has a log *)
  let*@! tx1_receipt = Rpc.get_transaction_receipt ~tx_hash:tx1 evm_node in
  let* tx1_block_logs =
    get_logs
      ~from_block:(Int32.to_string tx1_receipt.blockNumber)
      ~to_block:(Int32.to_string tx1_receipt.blockNumber)
      evm_node
  in
  Check.((List.length tx1_block_logs = 1) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Check no logs after transactions *)
  let*@ _ = produce_block () in
  let*@ no_logs_start = Rpc.block_number evm_node in
  let* new_logs =
    get_logs ~from_block:(Int32.to_string no_logs_start) evm_node
  in
  Check.((List.length new_logs = 0) int) ~error_msg:"Expected %R logs, got %L" ;
  unit

let test_l2_nested_create =
  register_both
    ~tags:["evm"; "l2_deploy"; "l2_create"; "inter_contract"]
    ~title:"Check L2 nested create"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; produce_block; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* nested_create_resolved = nested_create () in
  let* nested_create_address, _tx =
    deploy ~contract:nested_create_resolved ~sender evm_setup
  in
  let* tx1 =
    let call_create (sender : Eth_account.t) n =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:nested_create_resolved.label
        ~address:nested_create_address
        ~method_call:(Printf.sprintf "create(%d)" n)
    in
    wait_for_application ~produce_block (call_create sender 1)
  in
  let* tx2 =
    let call_create (sender : Eth_account.t) n salt =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:nested_create_resolved.label
        ~address:nested_create_address
        ~method_call:(Printf.sprintf "create2(%d, \"%s\")" n salt)
    in
    wait_for_application ~produce_block (call_create sender 1 "0x")
  in
  let* () = check_tx_succeeded ~endpoint ~tx:tx1 in
  let* () = check_tx_succeeded ~endpoint ~tx:tx2 in
  unit

let test_block_hash_regression =
  Protocol.register_regression_test
  (* The test runs only on alpha, as the test with other protocols is marked
     as slow, there is a risk that regression is not updated. *)
    ~supports:Protocol.(From_protocol (number Alpha))
    ~__FILE__
    ~tags:["evm"; "block"; "hash"; "regression"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_evm_node;
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Regression test for L2 block hash"
  @@ fun protocol ->
  (* We use a timestamp equal to the next day after genesis.
     The genesis timestamp can be found in tezt/lib_tezos/client.ml *)
  let* {produce_block; evm_node; _} =
    setup_evm_kernel
      ~bootstrap_accounts:
        (List.map
           (fun account -> account.Eth_account.address)
           (Array.to_list Eth_account.bootstrap_accounts)
        @ Eth_account.lots_of_address)
      ~admin:None
      ~timestamp:(At (Option.get @@ Ptime.of_date (2018, 7, 1)))
      ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
      protocol
  in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 3) in
  let raw_txs, _tx_hashes = List.split txs in
  let* _requests, receipt, _hashes =
    send_n_transactions ~produce_block ~evm_node raw_txs
  in
  Regression.capture @@ sf "Block hash: %s" receipt.blockHash ;
  unit

let test_l2_revert_returns_unused_gas =
  register_both
    ~tags:["evm"; "revert"]
    ~title:"Check L2 revert returns unused gas"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; produce_block; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* revert_resolved = revert () in
  let* _revert_address, _tx =
    deploy ~contract:revert_resolved ~sender evm_setup
  in
  let* tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:65_536
      ~gas:100_000
      ~value:Wei.zero
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~signature:"run()"
      ()
  in
  let* balance_before = Eth_cli.balance ~account:sender.address ~endpoint in
  let*@ transaction_hash = Rpc.send_raw_transaction ~raw_tx:tx evm_node in
  let* transaction_receipt =
    wait_for_application
      ~produce_block
      (wait_for_transaction_receipt ~evm_node ~transaction_hash)
  in
  let gas_used = transaction_receipt.gasUsed in
  let* () = check_tx_failed ~endpoint ~tx:transaction_hash in
  Check.((gas_used < 100000L) int64)
    ~error_msg:"Expected gas usage less than %R logs, got %L" ;
  let* balance_after = Eth_cli.balance ~account:sender.address ~endpoint in
  let gas_fee_paid = Wei.(balance_before - balance_after) in
  let gas_price = transaction_receipt.effectiveGasPrice in
  let expected_gas_fee_paid = expected_gas_fees ~gas_price ~gas_used in
  Check.((expected_gas_fee_paid = gas_fee_paid) Wei.typ)
    ~error_msg:"Expected gas fee paid to be %L, got %R" ;
  unit

let test_l2_create_collision =
  register_both
    ~tags:["evm"; "l2_create"; "collision"]
    ~title:"Check L2 create collision"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; produce_block; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* create2_resolved = create2 () in
  let* create2_address, _tx =
    deploy ~contract:create2_resolved ~sender evm_setup
  in

  let call_create2 (sender : Eth_account.t) ~expect_failure =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:create2_resolved.label
      ~address:create2_address
      ~method_call:(Printf.sprintf "create2()")
  in

  let* tx1 =
    wait_for_application
      ~produce_block
      (call_create2 sender ~expect_failure:false)
  in

  let* tx2 =
    wait_for_application
      ~produce_block
      (call_create2 sender ~expect_failure:true)
  in

  let* () = check_tx_succeeded ~tx:tx1 ~endpoint in
  check_tx_failed ~tx:tx2 ~endpoint

let test_l2_intermediate_OOG_call =
  register_both
    ~tags:["evm"; "out_of_gas"; "call"; "simple_storage"]
    ~title:
      "Check that an L2 call to a smart contract with an intermediate call \
       that runs out of gas still succeeds."
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; produce_block; _} = evm_setup in
  let* oog_call_resolved = oog_call () in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved = simple_storage () in
  let* random_contract_address, _tx =
    deploy ~contract:simple_storage_resolved ~sender evm_setup
  in
  let* oog_call_address, _tx =
    deploy ~contract:oog_call_resolved ~sender evm_setup
  in
  let call_oog (sender : Eth_account.t) ~expect_failure =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:oog_call_resolved.label
      ~address:oog_call_address
      ~method_call:
        (Printf.sprintf "sendViaCall(\"%s\")" random_contract_address)
  in
  let* tx =
    wait_for_application ~produce_block (call_oog sender ~expect_failure:false)
  in
  check_tx_succeeded ~tx ~endpoint

let test_l2_ether_wallet =
  register_both
    ~tags:["evm"; "l2_call"; "wallet"]
    ~title:"Check ether wallet functions correctly"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; produce_block; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* ether_wallet_resolved = ether_wallet () in
  let* ether_wallet_address, _tx =
    deploy ~contract:ether_wallet_resolved ~sender evm_setup
  in
  let* tx1 =
    let transaction =
      Eth_cli.transaction_send
        ~source_private_key:sender.private_key
        ~to_public_key:ether_wallet_address
        ~value:(Wei.of_eth_int 100)
        ~endpoint
    in
    wait_for_application ~produce_block transaction
  in
  let* tx2 =
    let call_withdraw (sender : Eth_account.t) n =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:ether_wallet_resolved.label
        ~address:ether_wallet_address
        ~method_call:(Printf.sprintf "withdraw(%d)" n)
    in
    wait_for_application ~produce_block (call_withdraw sender 100)
  in
  let* () = check_tx_succeeded ~endpoint ~tx:tx1 in
  let* () = check_tx_succeeded ~endpoint ~tx:tx2 in
  unit

let test_keep_alive =
  Protocol.register_test
    ~__FILE__
    ~tags:["keep_alive"; "proxy"]
    ~title:"Proxy mode keep alive argument"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    (fun protocol ->
      let* {sc_rollup_node; sc_rollup_address; evm_node; endpoint = _; _} =
        setup_evm_kernel ~admin:None protocol
      in
      (* Stop the EVM and rollup nodes. *)
      let* () = Evm_node.terminate evm_node in
      let* () = Sc_rollup_node.terminate sc_rollup_node in
      (* Restart the evm node without keep alive, expected to fail. *)
      let process = Evm_node.spawn_run evm_node in
      let* () =
        Process.check_error ~msg:(rex "the communication was lost") process
      in
      (* Restart with keep alive. The EVM node is waiting for the connection. *)
      let* () =
        Evm_node.run ~wait:false ~extra_arguments:["--keep-alive"] evm_node
      in
      let* () = Evm_node.wait_for_retrying_connect evm_node in
      (* Restart the rollup node to restore the connection. *)
      let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
      let* () = Evm_node.wait_for_ready evm_node in
      (* The EVM node should respond to RPCs. *)
      let*@ _block_number = Rpc.block_number evm_node in
      (* Stop the rollup node, the EVM node will loop retrying
         RPCs. *)
      let* () = Sc_rollup_node.terminate sc_rollup_node in
      let block_number = Rpc.block_number evm_node in
      let* () = Evm_node.wait_for_retrying_connect evm_node in
      (* Restart the EVM node, and check RPC. *)
      let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
      let*@ _block_number = block_number in
      let*@ _block_number = Rpc.block_number evm_node in
      unit)

let test_reboot_out_of_ticks =
  register_proxy
    ~tags:["evm"; "reboot"; "loop"; "out_of_ticks"; Tag.flaky]
    ~title:
      "Check that the kernel can handle transactions that take too many ticks \
       for a single run"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~maximum_allowed_ticks:9_000_000_000L
  @@ fun ~protocol:_
             ~evm_setup:{evm_node; produce_block; sc_rollup_node; node; _} ->
  (* Retrieves all the messages and prepare them for the current rollup. *)
  let txs =
    read_file (kernel_inputs_path ^ "/loops-out-of-ticks")
    |> String.trim |> String.split_on_char '\n'
  in
  (* The first three transactions are sent in a separate block, to handle any nonce issue. *)
  let first_block, second_block, third_block, fourth_block =
    match txs with
    | faucet1 :: faucet2 :: create :: rem ->
        ([faucet1], [faucet2], [create], rem)
    | _ ->
        failwith
          "The prepared transactions should contain at least 3 transactions"
  in
  let* () =
    Lwt_list.iter_s
      (fun block ->
        let* _requests, _receipt, _hashes =
          send_n_transactions ~produce_block ~evm_node ~wait_for_blocks:5 block
        in
        unit)
      [first_block; second_block; third_block]
  in
  let* total_tick_number_before_expected_reboots =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let* l1_level_before_out_of_ticks = Node.get_level node in
  let* requests, receipt, _hashes =
    send_n_transactions
      ~produce_block
      ~evm_node
      ~wait_for_blocks:5
      (* By default, it waits for 3 blocks. We need to take into account the
         blocks before the inclusion which is generally 2. The loops can be a
         bit long to execute, as such the inclusion test might fail before the
         execution is over, making it flaky. *)
      fourth_block
  in
  let* total_tick_number_with_expected_reboots =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let*@ block_with_out_of_ticks =
    Rpc.get_block_by_number
      ~block:(Format.sprintf "%#lx" receipt.blockNumber)
      evm_node
  in
  (* Check that all the transactions are actually included in the same block,
     otherwise it wouldn't make sense to continue. *)
  (match block_with_out_of_ticks.Block.transactions with
  | Block.Empty -> Test.fail "Expected a non empty block"
  | Block.Full _ ->
      Test.fail "Block is supposed to contain only transaction hashes"
  | Block.Hash hashes ->
      Check.((List.length hashes = List.length requests) int)
        ~error_msg:"Expected %R transactions in the resulting block, got %L") ;

  (* Check the number of ticks spent during the period when there should have
     been a reboot due to out of ticks. There have been a reboot if the number
     of ticks is not `number of blocks` * `ticks per l1 level`. *)
  let* l1_level_after_out_of_ticks = Node.get_level node in
  let number_of_blocks =
    l1_level_after_out_of_ticks - l1_level_before_out_of_ticks
  in
  let ticks_after_expected_reboot =
    total_tick_number_with_expected_reboots
    - total_tick_number_before_expected_reboots
  in
  let min_ticks_per_l1_level = ticks_per_snapshot * 2 in
  Check.(
    (ticks_after_expected_reboot
    >= (min_ticks_per_l1_level * number_of_blocks) + ticks_per_snapshot)
      int)
    ~error_msg:
      "The number of ticks spent during the period should be higher or equal \
       than %R, but got %L, which implies there have been no reboot, contrary \
       to what was expected." ;
  unit

let test_l2_timestamp_opcode =
  let test ~protocol:_ ~evm_setup =
    let {evm_node; produce_block; _} = evm_setup in
    let endpoint = Evm_node.endpoint evm_node in
    let sender = Eth_account.bootstrap_accounts.(0) in
    let* timestamp_resolved = timestamp () in
    let* timestamp_address, _tx =
      deploy ~contract:timestamp_resolved ~sender evm_setup
    in

    let* set_timestamp_tx =
      let call_create =
        Eth_cli.contract_send
          ~source_private_key:sender.private_key
          ~endpoint
          ~abi_label:timestamp_resolved.label
          ~address:timestamp_address
          ~method_call:(Printf.sprintf "setTimestamp()")
      in
      wait_for_application ~produce_block call_create
    in

    let* saved_timestamp =
      Eth_cli.contract_call
        ~endpoint
        ~abi_label:timestamp_resolved.label
        ~address:timestamp_address
        ~method_call:(Printf.sprintf "getSavedTimestamp()")
        ()
    in
    let saved_timestamp = Int64.of_string (String.trim saved_timestamp) in

    (* This call being done after saving the timestamp, it should be higher. *)
    let* simulated_timestamp =
      Eth_cli.contract_call
        ~endpoint
        ~abi_label:timestamp_resolved.label
        ~address:timestamp_address
        ~method_call:(Printf.sprintf "getTimestamp()")
        ()
    in
    let simulated_timestamp =
      Int64.of_string (String.trim simulated_timestamp)
    in

    let* () = check_tx_succeeded ~endpoint ~tx:set_timestamp_tx in
    Check.(
      (saved_timestamp < simulated_timestamp)
        int64
        ~error_msg:
          "Simulated timestamp (%R) should be higher than the one saved from a \
           previous block (%L)") ;
    unit
  in
  register_both
    ~tags:["evm"; "timestamp"; "opcode"]
    ~title:"Check L2 opcode timestamp"
    ~kernels:[Kernel.Latest]
    test

let test_migrate_proxy_to_sequencer_future =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "rollup_node"; "init"; "migration"; "sequencer"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:
      "migrate from proxy to sequencer using a sequencer admin contract with a \
       future timestamp"
  @@ fun protocol ->
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  (* 1s per block, 10 block. *)
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  let sequencer_admin = Constant.bootstrap5 in
  let sequencer_key = Constant.bootstrap4 in
  let* ({
          evm_node = proxy_node;
          sc_rollup_node;
          client;
          kernel;
          sc_rollup_address;
          l1_contracts;
          _;
        } as full_evm_setup) =
    setup_evm_kernel
      ~timestamp:genesis_timestamp
      ~sequencer_admin
      ~admin:(Some Constant.bootstrap3)
      protocol
  in
  (* Send a transaction in proxy mode. *)
  let* () =
    let sender = Eth_account.bootstrap_accounts.(0) in
    let receiver = Eth_account.bootstrap_accounts.(1) in
    let* tx = send ~sender ~receiver ~value:Wei.one_eth full_evm_setup in
    check_tx_succeeded ~endpoint:(Evm_node.endpoint proxy_node) ~tx
  in
  (* Send the internal message to add a sequencer on the rollup. *)
  let sequencer_governance_contract =
    match
      Option.bind l1_contracts (fun {sequencer_governance; _} ->
          sequencer_governance)
    with
    | Some contract -> contract
    | None -> Test.fail "missing sequencer admin contract"
  in
  let* () =
    sequencer_upgrade
      ~sc_rollup_address
      ~sequencer_admin:sequencer_admin.alias
      ~sequencer_governance_contract
      ~client
      ~upgrade_to:sequencer_key.alias
      ~activation_timestamp
      ~pool_address:Eth_account.bootstrap_accounts.(0).address
  in
  let sequencer_node =
    let mode =
      Evm_node.Sequencer
        {
          initial_kernel = kernel;
          preimage_dir =
            Some (Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0");
          private_rpc_port = Some (Port.fresh ());
          time_between_blocks = Some Nothing;
          sequencer = sequencer_key.alias;
          genesis_timestamp = None;
          max_blueprints_lag = None;
          max_blueprints_ahead = None;
          max_blueprints_catchup = None;
          catchup_cooldown = None;
          max_number_of_chunks = None;
          wallet_dir = Some (Client.base_dir client);
          tx_pool_timeout_limit = None;
          tx_pool_addr_limit = None;
          tx_pool_tx_per_addr_limit = None;
          dal_slots = None;
        }
    in
    Evm_node.create ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config sequencer_node in
  let* () =
    repeat 10 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Run the sequencer from the rollup node state. *)
  let* () =
    Evm_node.init_from_rollup_node_data_dir sequencer_node sc_rollup_node
  in
  let* () = Evm_node.run sequencer_node in
  (* Same head after initialisation. *)
  let* () =
    check_head_consistency
      ~left:sequencer_node
      ~right:proxy_node
      ~error_msg:"block hash is not equal (sequencer: %L; rollup: %R)"
      ()
  in
  (* Produce a block in sequencer. *)
  let*@ _ = Rpc.produce_block sequencer_node in
  let* () =
    bake_until_sync
      ~sc_rollup_node
      ~client
      ~sequencer:sequencer_node
      ~proxy:proxy_node
      ()
  in
  (* Same head after first sequencer produced block. *)
  let* () =
    check_head_consistency
      ~left:sequencer_node
      ~right:proxy_node
      ~error_msg:"block hash is not equal (sequencer: %L; rollup: %R)"
      ()
  in

  (* Send a transaction to sequencer. *)
  let* () =
    let sender = Eth_account.bootstrap_accounts.(0) in
    let receiver = Eth_account.bootstrap_accounts.(1) in
    let full_evm_setup =
      {
        full_evm_setup with
        evm_node = sequencer_node;
        produce_block = (fun () -> Rpc.produce_block sequencer_node);
      }
    in
    let* tx = send ~sender ~receiver ~value:Wei.one_eth full_evm_setup in
    check_tx_succeeded ~endpoint:(Evm_node.endpoint sequencer_node) ~tx
  in
  let* () =
    bake_until_sync
      ~sc_rollup_node
      ~client
      ~sequencer:sequencer_node
      ~proxy:proxy_node
      ()
  in
  (* Same head after sequencer transaction. *)
  let* () =
    check_head_consistency
      ~left:sequencer_node
      ~right:proxy_node
      ~error_msg:"block hash is not equal (sequencer: %L; rollup: %R)"
      ()
  in
  unit

let test_migrate_proxy_to_sequencer_past =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "rollup_node"; "init"; "migration"; "sequencer"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:
      "migrate from proxy to sequencer using a sequencer admin contract with a \
       past timestamp"
  @@ fun protocol ->
  let sequencer_admin = Constant.bootstrap5 in
  let sequencer_key = Constant.bootstrap4 in
  let* ({
          evm_node = proxy_node;
          sc_rollup_node;
          client;
          kernel;
          sc_rollup_address;
          l1_contracts;
          _;
        } as full_evm_setup) =
    setup_evm_kernel ~sequencer_admin ~admin:(Some Constant.bootstrap3) protocol
  in
  (* Send a transaction in proxy mode. *)
  let* () =
    let sender = Eth_account.bootstrap_accounts.(0) in
    let receiver = Eth_account.bootstrap_accounts.(1) in
    let* tx = send ~sender ~receiver ~value:Wei.one_eth full_evm_setup in
    check_tx_succeeded ~endpoint:(Evm_node.endpoint proxy_node) ~tx
  in
  (* Send the internal message to add a sequencer on the rollup. *)
  let sequencer_governance_contract =
    match
      Option.bind l1_contracts (fun {sequencer_governance; _} ->
          sequencer_governance)
    with
    | Some contract -> contract
    | None -> Test.fail "missing sequencer admin contract"
  in
  let* () =
    sequencer_upgrade
      ~sc_rollup_address
      ~sequencer_admin:sequencer_admin.alias
      ~sequencer_governance_contract
      ~client
      ~upgrade_to:sequencer_key.alias
      ~activation_timestamp:"0"
      ~pool_address:Eth_account.bootstrap_accounts.(0).address
  in
  let* () =
    (* We need to bake 3 blocks, because otherwise the sequencer upgrade event
       is re-received by the EVM node. This is because `init from rollup node`
       reads the HEAD context of the rollup node, but the EVM node interacts
       with the rollup node two blocks in the past. As a consequence, without
       baking these blocks, the EVM node will virtually handle the sequencer
       upgrade event twice.

       However, the EVM node does not deal with sequencer event gracefully
       right now. It works for preventing the old sequencer to produce
       blueprints, but not for the new sequencer to start producing blueprints.

       This is because of the blueprint deletion mechanism of the kernel.
       When the sequencer upgrade is applied at the end of stage-1, the
       pending blueprints are deleted. This means the bluperint currently
       applied in `apply_blueprint` is deleted, meaning nothing is executed
       in the stage-2. *)
    repeat 3 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let sequencer_node =
    let mode =
      Evm_node.Sequencer
        {
          initial_kernel = kernel;
          preimage_dir =
            Some (Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0");
          private_rpc_port = Some (Port.fresh ());
          time_between_blocks = Some Nothing;
          sequencer = sequencer_key.alias;
          genesis_timestamp = None;
          max_blueprints_lag = None;
          max_blueprints_ahead = None;
          max_blueprints_catchup = None;
          catchup_cooldown = None;
          max_number_of_chunks = None;
          wallet_dir = Some (Client.base_dir client);
          tx_pool_timeout_limit = None;
          tx_pool_addr_limit = None;
          tx_pool_tx_per_addr_limit = None;
          dal_slots = None;
        }
    in
    Evm_node.create ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config sequencer_node in
  (* Run the sequencer from the rollup node state. *)
  let* () =
    Evm_node.init_from_rollup_node_data_dir sequencer_node sc_rollup_node
  in
  let* () = Evm_node.run sequencer_node in
  (* Same head after initialisation. *)
  let* () =
    check_head_consistency
      ~left:sequencer_node
      ~right:proxy_node
      ~error_msg:"block hash is not equal (sequencer: %L; rollup: %R)"
      ()
  in

  (* Produce a block in sequencer. *)
  let*@ _ = Rpc.produce_block sequencer_node in
  let* () =
    bake_until_sync
      ~sc_rollup_node
      ~client
      ~sequencer:sequencer_node
      ~proxy:proxy_node
      ()
  in
  (* Same head after first sequencer produced block. *)
  let* () =
    check_head_consistency
      ~left:sequencer_node
      ~right:proxy_node
      ~error_msg:"block hash is not equal (sequencer: %L; rollup: %R)"
      ()
  in

  (* Send a transaction to sequencer. *)
  let* () =
    let sender = Eth_account.bootstrap_accounts.(0) in
    let receiver = Eth_account.bootstrap_accounts.(1) in
    let full_evm_setup =
      {
        full_evm_setup with
        evm_node = sequencer_node;
        produce_block = (fun () -> Rpc.produce_block sequencer_node);
      }
    in
    let* tx = send ~sender ~receiver ~value:Wei.one_eth full_evm_setup in
    check_tx_succeeded ~endpoint:(Evm_node.endpoint sequencer_node) ~tx
  in
  let* () =
    bake_until_sync
      ~sc_rollup_node
      ~client
      ~sequencer:sequencer_node
      ~proxy:proxy_node
      ()
  in
  (* Same head after sequencer transaction. *)
  let* () =
    check_head_consistency
      ~left:sequencer_node
      ~right:proxy_node
      ~error_msg:"block hash is not equal (sequencer: %L; rollup: %R)"
      ()
  in

  unit

let test_ghostnet_kernel =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "ghostnet"; "version"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_evm_node;
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Regression test for Ghostnet kernel"
  @@ fun protocol ->
  let* {evm_node; _} = setup_evm_kernel ~kernel:Ghostnet ~admin:None protocol in
  let*@ version = Rpc.tez_kernelVersion evm_node in
  Check.((version = Constant.WASM.ghostnet_evm_commit) string)
    ~error_msg:"The ghostnet kernel has version %L but constant says %R" ;
  unit

let test_estimate_gas_out_of_ticks =
  register_both
    ~tags:["evm"; "estimate_gas"; "out_of_ticks"; "simulate"; "loop"]
    ~title:"estimateGas works with out of ticks"
  @@ fun ~protocol:_ ~evm_setup:({evm_node; _} as evm_setup) ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* loop_resolved = loop () in
  let* loop_address, _tx = deploy ~contract:loop_resolved ~sender evm_setup in
  (* Call estimateGas with an out of ticks transaction. *)
  let estimateGas =
    [
      ("from", `String sender.address);
      (* The data payload was retrieved by calling `loop(100000)` and reversed
         engineer the data field. *)
      ( "data",
        `String
          "0x0b7d796e00000000000000000000000000000000000000000000000000000000000186a0"
      );
      ("to", `String loop_address);
    ]
  in
  let*@? {message; code = _; data = _} =
    Rpc.estimate_gas estimateGas evm_node
  in
  Check.(message =~ rex "The transaction would exhaust all the ticks")
    ~error_msg:"The estimate gas should fail with out of ticks message." ;
  unit

let test_l2_call_selfdetruct_contract_in_same_transaction =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_call"; "selfdestruct"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Check destruct contract in same transaction can be called"
  @@ fun protocol ->
  let* evm_setup = setup_evm_kernel ~admin:None protocol in
  let*@ _ = evm_setup.produce_block () in
  let* call_selfdestruct_resolved = call_selfdestruct () in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* _address, _tx =
    deploy ~contract:call_selfdestruct_resolved ~sender evm_setup
  in
  unit

let test_call_recursive_contract_estimate_gas =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_call"; "estimate_gas"; "recursive"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Check recursive contract gasLimit is high enough"
  @@ fun protocol ->
  let* ({endpoint; produce_block; _} as evm_setup) =
    setup_evm_kernel ~admin:None protocol
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* recursive_resolved = recursive () in
  let* recursive_address, _tx =
    deploy ~contract:recursive_resolved ~sender evm_setup
  in
  let call () =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:recursive_resolved.label
      ~address:recursive_address
      ~method_call:"call(40)"
      ()
  in
  let* tx = wait_for_application ~produce_block call in
  let* () = check_tx_succeeded ~endpoint ~tx in
  unit

let test_limited_stack_depth =
  register_both
    ~kernels:[Kernel.Latest]
    ~tags:["evm"; "recursive"; "stack_depth"]
    ~title:"Check recursive contract gasLimit is high enough"
    ~maximum_allowed_ticks:1_000_000_000_000L
  @@ fun ~protocol:_ ~evm_setup:({endpoint; produce_block; _} as evm_setup) ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* recursive_resolved = recursive () in
  let* recursive_address, _tx =
    deploy ~contract:recursive_resolved ~sender evm_setup
  in
  (* 256 is ok. *)
  let call () =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:recursive_resolved.label
      ~address:recursive_address
      ~method_call:"call(256)"
      ~gas:30_000_000
      ()
  in
  let* tx = wait_for_application ~produce_block call in
  let* () = check_tx_succeeded ~endpoint ~tx in
  (* 257 is not. *)
  let call () =
    Eth_cli.contract_send
      ~expect_failure:true
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:recursive_resolved.label
      ~address:recursive_address
      ~method_call:"call(257)"
      ~gas:30_000_000
        (* The fee model kicks in because we spend too much ticks in the previous call. *)
      ~gas_price:2000000000
      ()
  in
  let* tx = wait_for_application ~produce_block call in
  let* () = check_tx_failed ~endpoint ~tx in
  unit

let test_check_estimateGas_enforces_limits =
  register_both
    ~kernels:[Latest]
    ~tags:["evm"; "estimate_gas"; "gas_limit"]
    ~title:"Check that the eth_estimateGas enforces the kernel gas limit."
  @@ fun ~protocol:_ ~evm_setup:({evm_node; _} as evm_setup) ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* gas_left_contract = Solidity_contracts.gas_left () in
  let* gas_left_address, _tx =
    deploy ~contract:gas_left_contract ~sender evm_setup
  in
  (* Let's call it without a gas limit. *)
  let* call_input =
    Eth_cli.encode_method ~abi_label:gas_left_contract.label ~method_:"check()"
  in
  let call_params =
    [
      ("from", `String sender.address);
      ("to", `String gas_left_address);
      ("input", `String call_input);
    ]
  in
  (* Without specifying gas limit it will default to the maximum allowed per
     transaction, and the contract call will fail as it asks for more than 30M
     gas. *)
  let*@? estimated = Rpc.estimate_gas call_params evm_node in
  Check.(
    (estimated.message =~ rex "execution reverted")
      ~error_msg:
        "Expected a revert, as the transaction shouldn't have more than 30M \
         available") ;
  (* With a gas limit too high, the node will enforce the gas limit to the
     maximum allowed and prevent a timeout, and the call will fail. *)
  let*@? estimated =
    Rpc.estimate_gas (("gas", `String "100000000") :: call_params) evm_node
  in
  Check.(
    (estimated.message =~ rex "execution reverted")
      ~error_msg:
        "Expected a revert, as the transaction shouldn't have more than 30M \
         available") ;
  unit

let test_reveal_storage =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "reveal_storage"]
    ~title:"Reveal storage"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  (* Start a regular rollup. *)
  let* {evm_node; sc_rollup_node; produce_block; _} =
    setup_evm_kernel ~admin:None protocol
  in
  let* () =
    repeat 6 (fun _ ->
        let*@ _ = produce_block () in
        unit)
  in
  let*@ first_rollup_head = Rpc.get_block_by_number ~block:"latest" evm_node in

  (* Dump the storage of the smart rollup node and convert it into a RLP file
     the kernel can read. *)
  let dump_json = Temp.file "dump.json" in
  let dump_rlp = Temp.file "dump.rlp" in
  let* () =
    Sc_rollup_node.dump_durable_storage ~sc_rollup_node ~dump:dump_json ()
  in
  let* () = Evm_node.transform_dump ~dump_json ~dump_rlp in

  (* Get root hash of the storage configuration *)
  let config_preimages_dir = Temp.dir "config_preimages" in
  let* {root_hash = configuration_root_hash; _} =
    prepare_installer_kernel_with_arbitrary_file
      ~preimages_dir:config_preimages_dir
      dump_rlp
  in

  (* Start a new EVM rollup chain, but this time, with a ad-hoc config that
     allows to duplicate the state of the previous one.

     The only way for this new rollup to see initialized balances is for the
     duplication process to work. *)
  let additional_config =
    Sc_rollup_helpers.Installer_kernel_config.
      [
        Reveal
          {
            hash = configuration_root_hash;
            to_ = Durable_storage_path.reveal_config;
          };
      ]
  in

  (* Setup the new rollup, but do not force the installation of the kernel as
     we need to setup the preimage directory first. *)
  let* {evm_node; sc_rollup_node; produce_block; _} =
    setup_evm_kernel
      ~admin:None
      ~additional_config
      ~force_install_kernel:false
      ~bootstrap_accounts:[]
      protocol
  in

  (* Copy the config preimages directory contents into the preimages directory
     of the new rollup node. *)
  let* _ =
    Lwt_unix.system
      Format.(
        sprintf
          "cp %s/* %s"
          config_preimages_dir
          (Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0"))
  in

  (* Force the installation of the kernel of the new chain *)
  let*@ _ = produce_block () in

  (* Check the head. We produced one additional head with the bake above. *)
  let*@ copied_rollup_head =
    Rpc.get_block_by_number
      ~block:(first_rollup_head.number |> Int32.to_string)
      evm_node
  in
  Check.((copied_rollup_head.hash = first_rollup_head.hash) string)
    ~error_msg:"Head should be the same in the copy" ;
  unit

let call_get_hash ~address ~block_number endpoint =
  Cast.call
    ~args:[string_of_int block_number]
    ~endpoint
    ~address
    "getHash(uint256)"

let test_blockhash_opcode =
  register_both
    ~block_storage_sqlite3:true
    ~time_between_blocks:Nothing
    ~max_blueprints_ahead:300
    ~tags:["evm"; "blockhash"; "opcode"]
    ~title:"Check if blockhash opcode returns the actual hash of the block"
  @@ fun ~protocol:_
             ~evm_setup:({produce_block; endpoint; evm_node; _} as evm_setup) ->
  let* blockhash_resolved = blockhash () in
  let* address, _tx =
    deploy
      ~contract:blockhash_resolved
      ~sender:Eth_account.bootstrap_accounts.(0)
      evm_setup
  in
  let*@ head = Rpc.block_number evm_node in
  let head = Int32.to_int head in
  (* The BLOCKHASH opcode gets the hash of the most 256 recent complete blocks. *)
  let* () =
    repeat 256 (fun () ->
        let*@ _ = produce_block () in
        unit)
  in

  let rec check_block_hash level =
    if level > head + 256 then (
      let* found_block_hash =
        call_get_hash ~address ~block_number:level endpoint
      in
      Check.(
        (found_block_hash
       = "0x0000000000000000000000000000000000000000000000000000000000000000")
          string)
        ~error_msg:
          "The BLOCKHASH opcode should return 0x00..00 when the block is \
           incomplete, but for %L" ;
      unit)
    else
      let*@ {hash = expected_block_hash; _} =
        Rpc.get_block_by_number ~block:(string_of_int level) evm_node
      in
      let* found_block_hash =
        call_get_hash ~address ~block_number:level endpoint
      in
      Check.((found_block_hash = expected_block_hash) string)
        ~error_msg:
          (sf
             "The block hash should be the same when called from an RPC and \
              return by the BLOCKHASH opcode, got %%L, but %%R was expected \
              for level %d."
             level) ;
      check_block_hash (level + 1)
  in
  check_block_hash (head + 1)

let test_block_constants_opcode =
  register_both
    ~kernels:[Kernel.Latest]
    ~tags:["evm"; "block"; "opcode"; "constants"]
    ~title:"Check block constants in opcode"
  @@ fun ~protocol:_
             ~evm_setup:({evm_node; produce_block; endpoint; _} as evm_setup) ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* Deploy the contracts with the block constants. *)
  let contract = block_constants in
  let* address, tx = deploy ~contract ~sender evm_setup in
  let* () = check_tx_succeeded ~endpoint ~tx in
  (* Set the block number in the contract's storage. *)
  let set_block_number =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:contract.label
      ~address
      ~method_call:"set_block_number()"
  in
  let* set_block_number_tx =
    wait_for_application ~produce_block set_block_number
  in
  (* Check that `block.number` was the block number the transaction
     was included in. *)
  let*@! set_block_number_receipt =
    Rpc.get_transaction_receipt ~tx_hash:set_block_number_tx evm_node
  in
  let* storage_block_number =
    let* storage_block_number =
      Eth_cli.contract_call
        ~endpoint
        ~abi_label:contract.label
        ~address
        ~method_call:"view_stored_block_number()"
        ()
    in
    return (Int32.of_string @@ String.trim storage_block_number)
  in
  Check.(
    (set_block_number_receipt.blockNumber = storage_block_number)
      int32
      ~error_msg:
        "Expected same block number, receipt is %L and block.number is %R") ;
  (* Set the block timestamp in the contract's storage. *)
  let set_block_timestamp =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:contract.label
      ~address
      ~method_call:"set_block_timestamp()"
  in
  let* set_block_timestamp_tx =
    wait_for_application ~produce_block set_block_timestamp
  in
  (* Check that `block.timestamp` was the block timestamp the transaction
     was included in. *)
  let*@! set_block_timestamp_receipt =
    Rpc.get_transaction_receipt ~tx_hash:set_block_timestamp_tx evm_node
  in
  let*@ block_timestamp =
    Rpc.get_block_by_number
      ~block:(Int32.to_string set_block_timestamp_receipt.blockNumber)
      evm_node
  in
  let* storage_block_timestamp =
    let* storage_block_timestamp =
      Eth_cli.contract_call
        ~endpoint
        ~abi_label:contract.label
        ~address
        ~method_call:"view_stored_block_timestamp()"
        ()
    in
    return (Int64.of_string @@ String.trim storage_block_timestamp)
  in
  Check.(
    (Tezos_base.Time.Protocol.to_seconds block_timestamp.timestamp
    = storage_block_timestamp)
      int64
      ~error_msg:
        "Expected same block timestamp, block is %L and block.timestamp is %R") ;
  unit

let test_revert_is_correctly_propagated =
  register_both
    ~tags:["evm"; "revert"]
    ~title:"Check that the node propagates reverts reason correctly."
  @@ fun ~protocol:_ ~evm_setup:({evm_node; _} as evm_setup) ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* error_resolved = error () in
  let* error_address, _tx = deploy ~contract:error_resolved ~sender evm_setup in
  let* data =
    Eth_cli.encode_method
      ~abi_label:error_resolved.label
      ~method_:"testRevert(0)"
  in
  let* call = Rpc.call ~to_:error_address ~data evm_node in
  match call with
  | Ok _ -> Test.fail "Call should have reverted"
  | Error {data = None; _} ->
      Test.fail "Call should have reverted with a reason"
  | Error {data = Some _reason; _} ->
      (* TODO: #6893
         eth-cli cannot decode an encoded string using Ethereum format. *)
      unit

let test_block_gas_limit =
  register_both
    ~tags:["evm"; "gas_limit"; "block"]
    ~title:"Block gas limit returns 2^50."
  @@ fun ~protocol:_ ~evm_setup:({evm_node; endpoint; _} as evm_setup) ->
  let* gas_limit_contract_resolved = gas_limit_contract () in
  let* contract, _tx =
    deploy
      ~contract:gas_limit_contract_resolved
      ~sender:Eth_account.bootstrap_accounts.(0)
      evm_setup
  in
  let* opcode_gas_limit =
    let* gas_limit =
      Eth_cli.contract_call
        ~endpoint
        ~abi_label:gas_limit_contract_resolved.label
        ~address:contract
        ~method_call:"retrieve()"
        ()
    in
    return (Int64.of_string (String.trim gas_limit))
  in
  let*@ block = Rpc.get_block_by_number ~block:"latest" evm_node in
  let block_gas_limit = block.gasLimit in
  let check_gas_limit gas_limit =
    Check.((gas_limit = 1125899906842624L) int64)
      ~error_msg:"The gas limit should be 2**50, got %L"
  in
  check_gas_limit opcode_gas_limit ;
  check_gas_limit block_gas_limit ;
  unit

let test_tx_pool_timeout =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "tx_pool"; "timeout"]
    ~title:"Check that transactions correctly timeout."
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  let sequencer_admin = Constant.bootstrap1 in
  let admin = Some Constant.bootstrap3 in
  let setup_mode =
    Setup_sequencer
      {
        return_sequencer = false;
        time_between_blocks = Some Nothing;
        sequencer = sequencer_admin;
        max_blueprints_ahead = None;
        block_storage_sqlite3 = false;
      }
  in
  let ttl = 15 in
  let* {evm_node = sequencer_node; produce_block; _} =
    setup_evm_kernel
      ~sequencer_admin
      ~admin
      ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
      ~tx_pool_timeout_limit:ttl
      ~setup_mode
      protocol
  in
  (* We send one transaction and produce a block immediatly to check that it's included
     as it should (within the TTL that was set). *)
  let* tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(0).address
      ()
  in
  let*@ tx_hash_expected = Rpc.send_raw_transaction ~raw_tx:tx sequencer_node in
  let*@ block_number = produce_block () in
  let*@ block =
    Rpc.get_block_by_number ~block:(Int.to_string block_number) sequencer_node
  in
  let tx_hash =
    match block.transactions with
    | Hash txs -> List.hd txs
    | Empty ->
        Test.fail
          "Inspected block should contain a list of one transaction hash and \
           not be empty."
    | Full _ ->
        Test.fail
          "Inspected block should contain a list of one transaction hash, not \
           full objects."
  in
  Check.((tx_hash = tx_hash_expected) string)
    ~error_msg:"Expected transaction hash is %R, got %L" ;
  (* We send one transaction and produce a block after the TTL to check that the
     produced block is empty. *)
  let* tx' =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(0).address
      ()
  in
  let*@ _tx_hash' = Rpc.send_raw_transaction ~raw_tx:tx' sequencer_node in
  let* () = Lwt_unix.sleep (Int.to_float ttl *. 1.5) in
  let*@ block_number = produce_block () in
  let*@ block =
    Rpc.get_block_by_number ~block:(Int.to_string block_number) sequencer_node
  in
  match block.transactions with
  | Empty -> unit
  | _ -> Test.fail "Inspected block shoud be empty."

let test_tx_pool_address_boundaries =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "tx_pool"; "address"; "boundaries"]
    ~title:
      "Check that the boundaries set for the transaction pool are properly \
       behaving."
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  let sequencer_admin = Constant.bootstrap1 in
  let admin = Some Constant.bootstrap3 in
  let setup_mode =
    Setup_sequencer
      {
        return_sequencer = true;
        time_between_blocks = Some Nothing;
        sequencer = sequencer_admin;
        max_blueprints_ahead = None;
        block_storage_sqlite3 = false;
      }
  in
  let* {evm_node = sequencer_node; produce_block; _} =
    setup_evm_kernel
      ~sequencer_admin
      ~admin
      ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
      ~tx_pool_addr_limit:1
      ~tx_pool_tx_per_addr_limit:1
      ~setup_mode
      protocol
  in
  let* tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(0).address
      ()
  in
  let* tx' =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:21_000
      ~gas:2_000_000
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(0).address
      ()
  in
  let* tx'' =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:23_300
      ~value:(Wei.of_string "100000")
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let*@ tx_hash_expected = Rpc.send_raw_transaction ~raw_tx:tx sequencer_node in
  (* Limitation on the number of transaction per address *)
  let*@? rejected_transaction' =
    Rpc.send_raw_transaction ~raw_tx:tx' sequencer_node
  in
  Check.(
    (rejected_transaction'.message
   = "Limit of transaction for a user was reached. Transaction is rejected.")
      string)
    ~error_msg:"This transaction should be rejected with error msg %R not %L" ;
  (* Limitation on the number of allowed address inside the transaction pool *)
  let*@? rejected_transaction'' =
    Rpc.send_raw_transaction ~raw_tx:tx'' sequencer_node
  in
  Check.(
    (rejected_transaction''.message
   = "The transaction pool has reached its maximum threshold for user \
      transactions. Transaction is rejected.")
      string)
    ~error_msg:"This transaction should be rejected with error msg %R not %L" ;
  let*@ block_number = produce_block () in
  let*@ block =
    Rpc.get_block_by_number ~block:(Int.to_string block_number) sequencer_node
  in
  let tx_hash =
    match block.transactions with
    | Hash txs -> List.hd txs
    | Empty ->
        Test.fail
          "Inspected block should contain a list of one transaction hash and \
           not be empty."
    | Full _ ->
        Test.fail
          "Inspected block should contain a list of one transaction hash, not \
           full objects."
  in
  Check.((tx_hash = tx_hash_expected) string)
    ~error_msg:"Expected transaction hash is %R, got %L" ;
  unit

let test_tx_pool_transaction_size_exceeded =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "tx_pool"; "max"; "transaction"; "size"]
    ~title:
      "Check that a transaction that exceed the data size limit will be \
       rejected."
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  let sequencer_admin = Constant.bootstrap1 in
  let admin = Some Constant.bootstrap3 in
  let setup_mode =
    Setup_sequencer
      {
        return_sequencer =
          true
          (* Requires https://gitlab.com/tezos/tezos/-/merge_requests/14098
             to set to [false]. *);
        time_between_blocks = Some Nothing;
        sequencer = sequencer_admin;
        max_blueprints_ahead = None;
        block_storage_sqlite3 = false;
      }
  in
  let* {evm_node = sequencer_node; _} =
    setup_evm_kernel
      ~sequencer_admin
      ~admin
      ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
      ~max_number_of_chunks:1
      ~setup_mode
      protocol
  in
  let* tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:23_300
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(1).address
      ~arguments:["0x01"]
      ()
  in
  (* Limitation on size of the transaction *)
  let*@? rejected_transaction =
    Rpc.send_raw_transaction ~raw_tx:tx sequencer_node
  in
  Check.(
    (rejected_transaction.message
   = "Transaction data exceeded the allowed size.")
      string)
    ~error_msg:"This transaction should be rejected with error msg %R not %L" ;
  unit

let test_whitelist_is_executed =
  let rollup_operator_key = Constant.bootstrap1.public_key_hash in
  let whitelist = [rollup_operator_key] in
  let commitment_period = 5 and challenge_window = 5 in
  register_both
    ~challenge_window
    ~commitment_period
    ~whitelist
    ~rollup_operator_key
    ~tags:["evm"; "whitelist"; "update"]
    ~title:
      "Check that the kernel submit a whitelist update message when flag is \
       set."
  @@ fun ~protocol:_
             ~evm_setup:{sc_rollup_node; client; node; sc_rollup_address; _} ->
  let get_whitelist () =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_whitelist
         sc_rollup_address
  in
  let* found_whitelist = get_whitelist () in
  Check.(
    (Some whitelist = found_whitelist)
      (option (list string))
      ~error_msg:"found %R expected %L") ;
  let* () =
    repeat
      ((commitment_period * challenge_window) + 3)
      (fun () ->
        let* _lvl = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* found_whitelist = get_whitelist () in
  Check.(
    (None = found_whitelist)
      (option (list string))
      ~error_msg:"found %R expected %L") ;
  unit

let test_rpc_maxPriorityFeePerGas =
  register_both
    ~tags:["evm"; "rpc"; "max_priority_fee_per_gas"]
    ~title:"RPC methods eth_maxPriorityFeePerGas"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let expected_max_priority_fee_per_gas = Wei.of_gwei_string "0" in
  let* max_priority_fee_per_gas =
    Evm_node.(
      let* price =
        call_evm_rpc
          evm_node
          {method_ = "eth_maxPriorityFeePerGas"; parameters = `A []}
      in
      return JSON.(price |-> "result" |> as_int64 |> Z.of_int64 |> Wei.to_wei_z))
  in
  Check.((max_priority_fee_per_gas = expected_max_priority_fee_per_gas) Wei.typ)
    ~error_msg:"Expected %R, but got %L" ;
  unit

let test_proxy_read_only =
  register_proxy
    ~title:"Proxy refuses transactions if read-only flag is set"
    ~tags:["evm"; "proxy"; "read_only"]
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* () = Evm_node.terminate evm_node in
  let* () = Evm_node.run ~extra_arguments:["--read-only"] evm_node in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:"0xabac" evm_node in
  Check.(err.message =~ rex "the node is in read-only mode")
    ~error_msg:"Unexpected error message" ;
  unit

let test_unsupported_rpc =
  register_both
    ~tags:["evm"; "rpc"; "unsupported"]
    ~title:"Unsupported RPC method"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* protocol_version =
    Evm_node.(
      call_evm_rpc
        evm_node
        {method_ = "eth_protocolVersion"; parameters = `A []})
  in
  Check.(
    (JSON.(protocol_version |-> "error" |-> "message" |> as_string)
    = "Method not supported")
      string)
    ~error_msg:"Expected unsupported method error." ;
  unit

let test_rpc_feeHistory =
  register_both
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
     Replace by [Any] after the next upgrade *)
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "fee_history"]
    ~title:"RPC methods eth_feeHistory"
  @@ fun ~protocol:_ ~evm_setup ->
  let* _ =
    repeat 2 (fun _ ->
        let*@ _ = evm_setup.produce_block () in
        unit)
  in
  let* _ =
    send
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~receiver:Eth_account.bootstrap_accounts.(1)
      ~value:Wei.one
      evm_setup
  in
  let* _ =
    repeat 2 (fun _ ->
        let*@ _ = evm_setup.produce_block () in
        unit)
  in
  let*@ history = Rpc.fee_history "0x03" "latest" evm_setup.evm_node in
  let*@ latest_block =
    Rpc.get_block_by_number ~block:"latest" evm_setup.evm_node
  in
  Check.(
    Int64.(history.oldest_block = sub (of_int32 latest_block.number) 2L) int64)
    ~error_msg:"Expected block %R, but got %L" ;
  Check.((List.length history.base_fee_per_gas = 4) int)
    ~error_msg:"Expected list of size %R, but got %L" ;
  Check.((List.length history.gas_used_ratio = 3) int)
    ~error_msg:"Expected list of size %R, but got %L" ;
  List.iter
    (fun fee ->
      Check.((fee = latest_block.baseFeePerGas) int64)
        ~error_msg:"Expected fee %L to be %R")
    history.base_fee_per_gas ;

  (* 21000 / (2 ^ 50) = 1.86517e-11 *)
  Check.((List.hd history.gas_used_ratio = 1.86517e-11) float)
    ~error_msg:"Expected gas used ratio to be %R, but got %L" ;

  List.iter
    (fun ratio ->
      Check.((ratio <= 1.) float)
        ~error_msg:"Expected gas used ratio to be less than 1, but got %L")
    history.gas_used_ratio ;
  unit

let test_rpc_feeHistory_past =
  register_both
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
     Replace by [Any] after the next upgrade *)
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "fee_history"; "past"]
    ~title:"RPC methods eth_feeHistory in the past"
  @@ fun ~protocol:_ ~evm_setup ->
  let* _ =
    repeat 6 (fun () ->
        let*@ _ = evm_setup.produce_block () in
        unit)
  in
  let*@ latest_block =
    Rpc.get_block_by_number ~block:"latest" evm_setup.evm_node
  in
  let old_block = Int64.of_int32 @@ Int32.sub latest_block.number 2l in
  let block_count = 3L in
  let*@ history =
    Rpc.fee_history
      (Int64.to_string block_count)
      (Int64.to_string old_block)
      evm_setup.evm_node
  in
  Check.(Int64.(history.oldest_block = sub old_block @@ pred block_count) int64)
    ~error_msg:"Expected block %R, but got %L" ;
  Check.(
    (List.length history.base_fee_per_gas = 1 + Int64.to_int block_count) int)
    ~error_msg:"Expected list of size %R, but got %L" ;
  Check.((List.length history.gas_used_ratio = Int64.to_int block_count) int)
    ~error_msg:"Expected list of size %R, but got %L" ;
  unit

let test_rpc_feeHistory_future =
  register_both
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
     Replace by [Any] after the next upgrade *)
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "fee_history"; "future"]
    ~title:"RPC methods eth_feeHistory in the future"
  @@ fun ~protocol:_ ~evm_setup ->
  let* _ =
    repeat 3 (fun () ->
        let*@ _ = evm_setup.produce_block () in
        unit)
  in
  let*@? error = Rpc.fee_history "0x02" "0xFFFFFFFF" evm_setup.evm_node in
  Check.(
    (error.message =~ rex "Unknown block 4294967295")
      ~error_msg:"The transaction should fail with message %R, got &L") ;
  unit

let test_rpc_feeHistory_long =
  register_both
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
     Replace by [Any] after the next upgrade *)
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "fee_history"; "block_count"]
    ~title:"RPC methods eth_feeHistory with high blockCount"
  @@ fun ~protocol:_ ~evm_setup ->
  let* _ =
    repeat 3 (fun () ->
        let*@ _ = evm_setup.produce_block () in
        unit)
  in
  let*@ history = Rpc.fee_history "0xffffffffff" "latest" evm_setup.evm_node in
  let*@ latest_block =
    Rpc.get_block_by_number ~block:"latest" evm_setup.evm_node
  in
  Check.((history.oldest_block = 1L) int64)
    ~error_msg:"Expected block %R, but got %L" ;
  Check.(
    (List.length history.base_fee_per_gas = 1 + Int32.to_int latest_block.number)
      int)
    ~error_msg:"Expected list of size %R, but got %L" ;
  Check.(
    (List.length history.gas_used_ratio = Int32.to_int latest_block.number) int)
    ~error_msg:"Expected list of size %R, but got %L" ;
  unit

let test_rpcs_can_be_disabled =
  register_both
    ~tags:["evm"; "rpc"; "restricted"]
    ~title:"RPCs can be restricted"
    ~restricted_rpcs:"tez_*"
  @@ fun ~protocol:_ ~evm_setup ->
  let* kernel_version = Rpc.tez_kernelVersion evm_setup.evm_node in
  (match kernel_version with
  | Ok _ -> Test.fail "tez_* methods should be unsupported"
  | Error err ->
      Check.(
        (err.message = "Method disabled")
          string
          ~error_msg:"Disabled method should return %R, but returned %L")) ;
  let* kernel_root_hash = Rpc.tez_kernelRootHash evm_setup.evm_node in
  (match kernel_root_hash with
  | Ok _ -> Test.fail "tez_* methods should be unsupported"
  | Error err ->
      Check.(
        (err.message = "Method disabled")
          string
          ~error_msg:"Disabled method should return %R, but returned %L")) ;
  (* Check that a non restricted RPC is available. *)
  let*@ _block_number = Rpc.block_number evm_setup.evm_node in
  unit

let test_simulation_out_of_funds =
  register_both
    ~kernels:[Kernel.Latest]
    ~tags:["evm"; "simulation"; "funds"]
    ~title:"Simulation works with no from"
  @@ fun ~protocol:_ ~evm_setup ->
  (* If a simulation doesn't provide the `from` field, the source is
     0x00..00. But the simulation checks if the source has sufficient
     funds, which is not the case for the zero address. *)
  let eth_call =
    [
      ("to", `String "0xce8a69B73034588BA81fB89A3533C6aB9934F117");
      ("data", `String "0x0000");
      ("value", `String (Wei.of_eth_int 1 |> Wei.to_string));
    ]
  in
  let*@ _res = Rpc.estimate_gas eth_call evm_setup.evm_node in
  unit

let test_rpc_state_value_and_subkeys =
  register_sequencer
    ~return_sequencer:true (* See {Note: TX Pool RPC mode} *)
    ~tags:["evm"; "rpc"; "state_value"; "state_subkeys"]
    ~title:"RPC methods stateValue and stateSubkeys"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; client; produce_block; _} = evm_setup in
  let* _ = produce_block () in
  let* () =
    repeat 3 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let*@! kernel_version = Rpc.state_value evm_node "/evm/kernel_root_hash" in
  Check.(
    (kernel_version = evm_setup.kernel_root_hash)
      string
      ~error_msg:"Kernel version is %L, but should be %R") ;
  let*@! world_state_subkeys = Rpc.state_subkeys evm_node "/evm/world_state" in
  Check.(
    (List.sort String.compare world_state_subkeys
    = List.sort String.compare ["indexes"; "blocks"; "fees"; "eth_accounts"])
      (list string)
      ~error_msg:"Kernel version is %L, but should be %R") ;
  unit

let test_cast_work () =
  Test.register
    ~__FILE__
    ~title:"cast version"
    ~tags:["cast"]
    ~uses_admin_client:false
    ~uses_client:false
    ~uses_node:false
    (fun _ ->
      let* _version = Cast.version () in
      unit)

let test_proxy_ignore_block_param =
  register_proxy
    ~tags:["evm"; "ignore_block_param"; "proxy"]
    ~title:"Proxy can ignore block parameter"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun ~protocol:_ ~evm_setup:{produce_block; evm_node; _} ->
  let int_to_hex i = Format.sprintf "0x%x" i in
  (* Restart the proxy node with the --ignore-block-param flag *)
  let* () = Evm_node.terminate evm_node in
  let* () = Evm_node.run ~extra_arguments:["--ignore-block-param"] evm_node in
  (* Produce a bunch of blocks to get a “realistic” history. *)
  let* () =
    fold 3 () @@ fun i () ->
    let* raw_tx =
      Cast.craft_tx
        ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
        ~chain_id:1337
        ~nonce:i
        ~gas_price:21_000
        ~gas:2_000_000
        ~value:Wei.zero
        ~address:Eth_account.bootstrap_accounts.(0).address
        ()
    in
    let* _ = send_n_transactions ~produce_block ~evm_node [raw_tx] in
    unit
  in
  (* Check the nonce for latest and genesis blocks, and assert they are
     equal. *)
  let*@ latest_nonce =
    Rpc.get_transaction_count
      ~block:"latest"
      ~address:Eth_account.bootstrap_accounts.(0).address
      evm_node
  in
  let*@ genesis_nonce =
    Rpc.get_transaction_count
      ~block:(int_to_hex 0)
      ~address:Eth_account.bootstrap_accounts.(0).address
      evm_node
  in
  Check.(
    (latest_nonce = genesis_nonce)
      int64
      ~error_msg:"Nonces should be equal since the block param is ignored") ;
  unit

let register_evm_node ~protocols =
  test_cast_work () ;
  test_originate_evm_kernel protocols ;
  test_kernel_root_hash_originate_absent protocols ;
  test_kernel_root_hash_originate_present protocols ;
  test_kernel_root_hash_after_upgrade protocols ;
  test_evm_node_connection protocols ;
  test_consistent_block_hashes protocols ;
  test_rpc_getBalance protocols ;
  test_rpc_getCode protocols ;
  test_rpc_blockNumber protocols ;
  test_rpc_net_version protocols ;
  test_rpc_getBlockByNumber protocols ;
  test_rpc_getBlockByHash protocols ;
  test_rpc_getBlockReceipts protocols ;
  test_rpc_getTransactionCount protocols ;
  test_rpc_getTransactionCountBatch protocols ;
  test_rpc_batch protocols ;
  test_rpc_eth_coinbase protocols ;
  test_l2_block_size_non_zero protocols ;
  test_l2_blocks_progression protocols ;
  test_l2_transfer protocols ;
  test_chunked_transaction protocols ;
  test_rpc_txpool_content protocols ;
  test_rpc_web3_clientVersion protocols ;
  test_rpc_web3_sha3 protocols ;
  test_simulate protocols ;
  test_full_blocks protocols ;
  test_latest_block protocols ;
  test_eth_call_nullable_recipient protocols ;
  test_eth_call_contract_create protocols ;
  test_l2_deploy_simple_storage protocols ;
  test_l2_call_simple_storage protocols ;
  test_l2_deploy_erc20 protocols ;
  test_deploy_contract_for_shanghai protocols ;
  test_inject_100_transactions protocols ;
  test_eth_call_storage_contract protocols ;
  test_eth_call_storage_contract_eth_cli protocols ;
  test_eth_call_large protocols ;
  test_eth_call_input protocols ;
  test_preinitialized_evm_kernel protocols ;
  test_deposit_and_withdraw protocols ;
  test_withdraw_amount protocols ;
  test_withdraw_via_calls protocols ;
  test_estimate_gas protocols ;
  test_estimate_gas_additionnal_field protocols ;
  test_kernel_upgrade_epoch protocols ;
  test_kernel_upgrade_delay protocols ;
  test_kernel_upgrade_evm_to_evm protocols ;
  test_kernel_upgrade_wrong_key protocols ;
  test_kernel_upgrade_wrong_rollup_address protocols ;
  test_kernel_upgrade_no_administrator protocols ;
  test_kernel_upgrade_failing_migration protocols ;
  test_kernel_upgrade_version_change protocols ;
  test_kernel_upgrade_via_governance protocols ;
  test_kernel_upgrade_via_kernel_security_governance protocols ;
  test_rpc_sendRawTransaction protocols ;
  test_cannot_prepayed_leads_to_no_inclusion protocols ;
  test_cannot_prepayed_with_delay_leads_to_no_injection protocols ;
  test_rpc_getTransactionByBlockHashAndIndex protocols ;
  test_rpc_getTransactionByBlockNumberAndIndex protocols ;
  test_rpc_getTransactionByHash protocols ;
  test_rpc_getBlockTransactionCountBy protocols ;
  test_rpc_getUncleCountByBlock protocols ;
  test_rpc_getUncleByBlockArgAndIndex protocols ;
  test_simulation_eip2200 protocols ;
  test_rpc_gasPrice protocols ;
  test_rpc_getStorageAt protocols ;
  test_rpc_sendRawTransaction_with_consecutive_nonce protocols ;
  test_rpc_sendRawTransaction_not_included protocols ;
  test_originate_evm_kernel_and_dump_pvm_state protocols ;
  test_l2_call_inter_contract protocols ;
  test_rpc_getLogs protocols ;
  test_log_index protocols ;
  test_l2_nested_create protocols ;
  test_block_hash_regression protocols ;
  test_l2_revert_returns_unused_gas protocols ;
  test_l2_create_collision protocols ;
  test_l2_intermediate_OOG_call protocols ;
  test_l2_ether_wallet protocols ;
  test_keep_alive protocols ;
  test_reboot_out_of_ticks protocols ;
  test_l2_timestamp_opcode protocols ;
  test_migrate_proxy_to_sequencer_past protocols ;
  test_migrate_proxy_to_sequencer_future protocols ;
  test_ghostnet_kernel protocols ;
  test_estimate_gas_out_of_ticks protocols ;
  test_l2_call_selfdetruct_contract_in_same_transaction protocols ;
  test_reveal_storage protocols ;
  test_call_recursive_contract_estimate_gas protocols ;
  test_limited_stack_depth protocols ;
  test_check_estimateGas_enforces_limits protocols ;
  test_blockhash_opcode protocols ;
  test_block_constants_opcode protocols ;
  test_revert_is_correctly_propagated protocols ;
  test_block_gas_limit protocols ;
  test_tx_pool_timeout protocols ;
  test_tx_pool_address_boundaries protocols ;
  test_tx_pool_transaction_size_exceeded protocols ;
  test_whitelist_is_executed protocols ;
  test_rpc_maxPriorityFeePerGas protocols ;
  test_proxy_read_only protocols ;
  test_unsupported_rpc protocols ;
  test_rpc_getBlockBy_return_base_fee_per_gas_and_mix_hash protocols ;
  test_rpc_feeHistory protocols ;
  test_rpc_feeHistory_past protocols ;
  test_rpc_feeHistory_future protocols ;
  test_rpc_feeHistory_long protocols ;
  test_rpcs_can_be_disabled protocols ;
  test_simulation_out_of_funds protocols ;
  test_rpc_state_value_and_subkeys protocols ;
  (* See https://docs.etherlink.com/get-started/network-information for chain constants *)
  test_fa_bridge_flag_after_migration_v14
    ~kernel_from:Mainnet
    ~chain_id:42793
    ~chain_id_hex:
      "29a7000000000000000000000000000000000000000000000000000000000000"
    ~flag_expected:false
    protocols ;
  test_proxy_ignore_block_param protocols

let protocols = Protocol.all

let () =
  register_evm_node ~protocols ;
  register_evm_migration ~protocols

(* {Note: TX Pool RPC mode}

   As of today, the most RPC-related methods are not correctly implemented by
   the RPC mode. This is because the RPC mode does not keep track of the
   transactions it has forwarded to its upstream EVM node endpoint.

   It’s important to consider the observer mode is affected as well, but since
   it is not tested in this module, it was implicit. *)
