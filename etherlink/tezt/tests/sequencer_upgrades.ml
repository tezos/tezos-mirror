(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Etherlink Sequencer
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli solc@0.8.31
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make fa-bridge-watchtower
                 ./scripts/install_dal_trusted_setup.sh
                 # Install websocat (see: https://github.com/vi/websocat?tab=readme-ov-file#installation)
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file sequencer_upgrades.ml
   Subject:      Sequencer upgrades and sequencer key changes.
*)

open Rpc.Syntax
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

(** This tests the situation where the kernel has an upgrade and the
    sequencer upgrade by following the event of the kernel. Observer
    must upgrade as well, even the one that is not connected to a
    rollup node. *)
let test_sequencer_upgrade =
  let check_sequencer ~evm_node ~expected_sequencer kernel =
    let*@! current_sequencer =
      Rpc.state_value evm_node (Durable_storage_path.sequencer kernel)
    in
    Check.(
      (Hex.to_string (`Hex current_sequencer) = expected_sequencer)
        ~__LOC__
        string
        ~error_msg:"Sequencer in state is %L but expected %R") ;
    return ()
  in

  let genesis_timestamp =
    "2020-01-01T00:00:00Z"
    (* timestamp used for the genesis of the l1 and the l2 *)
  in
  (* 30 sec later. *)
  let activation_timestamp = "2020-01-01T00:00:30Z" in

  let sequencer_key = Constant.bootstrap1 in
  let new_sequencer_key = Constant.bootstrap2 in

  register_all
    ~__FILE__
    ~sequencer:sequencer_key
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "sequencer_upgrade"; "auto"; "sync"]
    ~title:
      "Rollup-node sequencer upgrade is applied to the sequencer local state."
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
  @@
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  let* observer_dont_track_rollup =
    Evm_node.init
      ~node_setup:
        (Evm_node.make_setup
           ?initial_kernel:(Evm_node.initial_kernel observer)
           ~preimages_dir:(Evm_node.preimages_dir observer)
           ())
      ~mode:
        (Observer
           {
             rollup_node_endpoint = None;
             evm_node_endpoint = Evm_node.endpoint sequencer;
           })
      ()
  in

  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  let* () =
    check_rollup_head_consistency
      ~evm_node:sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same before the upgrade: observer is %R while \
         rollup node is %L"
      ()
  in

  (* dummy check, upgrade is not submitted yet *)
  let* () =
    check_sequencer
      ~evm_node:sequencer
      ~expected_sequencer:sequencer_key.public_key
      kernel
  in
  let* () =
    check_sequencer
      ~evm_node:observer
      ~expected_sequencer:sequencer_key.public_key
      kernel
  in

  Log.info "Sending the sequencer upgrade to the L1 contract" ;

  let* () =
    sequencer_upgrade
      ~sc_rollup_address
      ~sequencer_admin:Constant.bootstrap2.alias
      ~sequencer_governance_contract:l1_contracts.sequencer_governance
      ~pool_address:Eth_account.bootstrap_accounts.(0).address
      ~client
      ~upgrade_to:new_sequencer_key.alias
      ~activation_timestamp
  in

  Log.info
    "Baking 2 block to make the event final in the l1 so the upgrade is seen \
     by EVM node following a rollup node." ;
  let upgrade_info = Evm_node.wait_for_evm_event Sequencer_upgrade sequencer
  and upgrade_info_observer =
    Evm_node.wait_for_evm_event Sequencer_upgrade observer
  in
  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  and* _upgrade_info = upgrade_info
  and* _upgrade_info_observer = upgrade_info_observer in

  Log.info
    "Producing a block from the current sequencer so the event is propagated \
     to observer not following a rollup node." ;
  let upgrade_info_observer_no_rollup_node =
    Evm_node.wait_for_evm_event Sequencer_upgrade observer_dont_track_rollup
  in
  let* () =
    let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
    unit
  and* _upgrade_info_observer_no_rollup_node =
    upgrade_info_observer_no_rollup_node
  in

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  Log.info
    "The upgrade is not yet triggered, head should be equivalent with the \
     block just produced" ;
  let* () =
    check_rollup_head_consistency
      ~evm_node:sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same before the upgrade: observer is %R while \
         rollup node is %L"
      ()
  in

  let*@ previous_rollup_head = rollup_level sc_rollup_node in

  Log.info "Baking until the sequencer upgrade is triggered in the l1" ;
  let has_sequencer_changed () =
    let* current_sequencer_in_rollup_hex =
      Sc_rollup_node.RPC.call
        sc_rollup_node
        ~rpc_hooks:Tezos_regression.rpc_hooks
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:(Durable_storage_path.sequencer kernel)
           ()
    in
    let current_sequencer_in_rollup =
      match current_sequencer_in_rollup_hex with
      | Some s -> Hex.to_string (`Hex s)
      | None -> Test.fail "missing sequencer"
    in
    if String.equal current_sequencer_in_rollup new_sequencer_key.public_key
    then return (Some ())
    else return None
  in
  let* () =
    bake_until
      ~__LOC__
      ~timeout_in_blocks:100
      ~timeout:60.
      ~bake:(fun () -> Rollup.next_rollup_node_level ~client ~sc_rollup_node)
      ~result_f:has_sequencer_changed
      ()
  in

  (* upgrade is not triggered until a blueprint with a superior
     timestamp is applied *)
  let* () =
    check_sequencer
      ~evm_node:sequencer
      ~expected_sequencer:sequencer_key.public_key
      kernel
  in
  let* () =
    check_sequencer
      ~evm_node:observer
      ~expected_sequencer:sequencer_key.public_key
      kernel
  in

  Log.info
    "Sequencer fails to produce a block because blueprint timestamp is above \
     the activation timestamp." ;
  let*@? _ = produce_block ~timestamp:activation_timestamp sequencer in

  (* Check that the evm-node sequencer refuses the blocks as they do
     not respect the sequencer's signature. *)
  let* () =
    check_rollup_head_consistency
      ~evm_node:sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same after the sequencer tried to produce \
         blocks, they are are disregarded: sequencer is %%L while rollup node \
         is %%R"
      ()
  in
  let* () =
    check_rollup_head_consistency
      ~evm_node:observer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same after the sequencer tried to produce \
         blocks, they are are disregarded: observer is %%L while rollup node \
         is %%R"
      ()
  in

  Log.info
    "Baking couple of blocks to check the block is not propagated nor accepted \
     by the rollup node." ;
  (* maybe unnecessary check here, we already not the block production failed. *)
  let* () =
    repeat 5 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  let*@ rollup_head = rollup_level sc_rollup_node in
  Check.((previous_rollup_head = rollup_head) int32)
    ~error_msg:
      "The rollup should not have progressed because no block have been \
       produced by the current sequencer." ;

  Log.info
    "Stopping current sequencer and starting a new one with new sequencer key" ;
  let* () = Evm_node.terminate sequencer
  and* () = Evm_node.wait_termination sequencer in
  let* snapshot_file =
    Runnable.run @@ Evm_node.export_snapshot ~desync:true sequencer
  in

  let* new_sequencer =
    let mode =
      match Evm_node.mode sequencer with
      | Sequencer config ->
          Evm_node.Sequencer
            {config with sequencer_keys = [new_sequencer_key.alias]}
      | _ -> Test.fail "impossible case, it's a sequencer"
    in
    let new_sequencer =
      Evm_node.create
        ~node_setup:
          (Evm_node.make_setup
             ?initial_kernel:(Evm_node.initial_kernel sequencer)
             ~preimages_dir:(Evm_node.preimages_dir sequencer)
             ())
        ~mode
        ()
    in
    let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
    let* () =
      Runnable.run
      @@ Evm_node.import_snapshot ~desync:true new_sequencer ~snapshot_file
    in
    let* () = Evm_node.run new_sequencer in
    return new_sequencer
  in

  Log.info "Producing a block with the new sequencer." ;
  let*@ _ = produce_block ~timestamp:activation_timestamp new_sequencer in
  Log.info "Baking to check the block is accepted by the rollup node." ;
  let* () =
    bake_until_sync ~sequencer:new_sequencer ~sc_rollup_node ~client ()
  in
  let* () =
    check_rollup_head_consistency
      ~evm_node:new_sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while rollup node is %R"
      ()
  in
  let*@ rollup_head = rollup_level sc_rollup_node in
  let current_head_number = rollup_head in
  Check.((Int32.add previous_rollup_head 1l = current_head_number) int32)
    ~error_msg:
      "The block number should have incremented (previous: %L, current: %R)" ;

  Log.info
    "Transforming the old sequencer to a observer so it should receive and \
     apply the blueprint. The connected observers should also receive it." ;
  let* observer_sequencer =
    let observer_sequencer =
      Evm_node.switch_sequencer_to_observer
        ~old_sequencer:sequencer
        ~new_sequencer
    in
    let* () =
      Process.check
      @@ Evm_node.spawn_init_config
           ~extra_arguments:["--force"]
           observer_sequencer
    in
    let* () = Evm_node.run observer_sequencer in
    return observer_sequencer
  in
  let current_head_number = Int32.to_int rollup_head in
  let* () =
    Evm_node.wait_for_blueprint_applied observer_sequencer current_head_number
  in
  let* () = Evm_node.wait_for_blueprint_applied observer current_head_number in
  let* () =
    check_head_consistency
      ~left:new_sequencer
      ~right:observer_sequencer
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while observer_sequencer is %R"
      ()
  in
  let* () =
    check_head_consistency
      ~left:new_sequencer
      ~right:observer
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while observer is %R"
      ()
  in

  let* () =
    Evm_node.wait_for_blueprint_applied
      observer_dont_track_rollup
      current_head_number
  in
  let* () =
    check_head_consistency
      ~left:new_sequencer
      ~right:observer_dont_track_rollup
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while observer is %R"
      ()
  in

  Log.info
    "Bootstrapping an observer to make sure it applied the sequencer upgrade \
     as well." ;
  let* observer_bootstrap =
    match Evm_node.mode observer with
    | Observer mode ->
        Evm_node.init
          ~node_setup:
            (Evm_node.make_setup
               ?initial_kernel:(Evm_node.initial_kernel observer)
               ~preimages_dir:(Evm_node.preimages_dir observer)
               ())
          ~mode:(Observer mode)
          ()
    | _ -> Test.fail "impossible, it's an observer"
  in
  let* _ = Evm_node.wait_for_evm_event Sequencer_upgrade observer_bootstrap in
  let* () =
    Evm_node.wait_for_blueprint_applied observer_bootstrap current_head_number
  in
  let* () =
    check_head_consistency
      ~left:new_sequencer
      ~right:observer_bootstrap
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while observer_bootstrap is %R"
      ()
  in
  unit

(** This tests the scenario where two identical sequencer upgrade
    transactions are sent to the L1 contract. The test verifies that
    the system handles duplicate upgrades correctly without duplicated
    insertions. This is mostly [test_sequencer_upgrade] with less
    checks but two identical sequencer upgrades. *)
let test_duplicate_sequencer_upgrade =
  let genesis_timestamp =
    "2020-01-01T00:00:00Z"
    (* timestamp used for the genesis of the l1 and the l2 *)
  in
  (* 30 sec later. *)
  let activation_timestamp = "2020-01-01T00:00:30Z" in

  let sequencer_key = Constant.bootstrap1 in
  let new_sequencer_key = Constant.bootstrap2 in

  register_all
    ~__FILE__
    ~sequencer:sequencer_key
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "sequencer_upgrade"; "auto"; "sync"]
    ~title:"Duplicated sequencer upgrade."
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
  @@
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  Log.info "Sending two sequencer upgrades to the L1 contract" ;

  let send_sequencer_upgrade () =
    sequencer_upgrade
      ~sc_rollup_address
      ~sequencer_admin:Constant.bootstrap2.alias
      ~sequencer_governance_contract:l1_contracts.sequencer_governance
      ~pool_address:Eth_account.bootstrap_accounts.(0).address
      ~client
      ~upgrade_to:new_sequencer_key.alias
      ~activation_timestamp
  in

  Log.info
    "Send two identical sequencer upgrade transactions to the L1 contract" ;
  let* () = send_sequencer_upgrade () in
  let* () = send_sequencer_upgrade () in

  Log.info "Baking until the sequencer upgrade is triggered in the l1" ;
  let has_sequencer_changed () =
    let* current_sequencer_in_rollup_hex =
      Sc_rollup_node.RPC.call
        sc_rollup_node
        ~rpc_hooks:Tezos_regression.rpc_hooks
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:(Durable_storage_path.sequencer kernel)
           ()
    in
    let current_sequencer_in_rollup =
      match current_sequencer_in_rollup_hex with
      | Some s -> Hex.to_string (`Hex s)
      | None -> Test.fail "missing sequencer"
    in
    if String.equal current_sequencer_in_rollup new_sequencer_key.public_key
    then return (Some ())
    else return None
  in
  let* () =
    bake_until
      ~__LOC__
      ~timeout_in_blocks:100
      ~timeout:60.
      ~bake:(fun () -> Rollup.next_rollup_node_level ~client ~sc_rollup_node)
      ~result_f:has_sequencer_changed
      ()
  in

  Log.info
    "Stopping current sequencer and starting a new one with new sequencer key" ;
  let* () = Evm_node.terminate sequencer
  and* () = Evm_node.wait_termination sequencer in
  let* snapshot_file =
    Runnable.run @@ Evm_node.export_snapshot ~desync:true sequencer
  in

  let* new_sequencer =
    let mode =
      match Evm_node.mode sequencer with
      | Sequencer config ->
          Evm_node.Sequencer
            {config with sequencer_keys = [new_sequencer_key.alias]}
      | _ -> Test.fail "impossible case, it's a sequencer"
    in
    let new_sequencer = Evm_node.create ~mode () in
    let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
    let* () =
      Runnable.run
      @@ Evm_node.import_snapshot new_sequencer ~desync:true ~snapshot_file
    in
    let* () = Evm_node.run new_sequencer in
    return new_sequencer
  in

  Log.info "Producing a block with the new sequencer." ;
  let*@ _ = produce_block ~timestamp:activation_timestamp new_sequencer in
  Log.info "Baking to check the block is accepted by the rollup node." ;
  let* () =
    bake_until_sync ~sequencer:new_sequencer ~sc_rollup_node ~client ()
  in

  Log.info
    "Bootstrapping an observer to make sure it applied the sequencer upgrade \
     as well." ;
  let* observer_bootstrap =
    Evm_node.init
      ~node_setup:
        (Evm_node.make_setup
           ~preimages_dir:(Evm_node.preimages_dir observer)
           ?initial_kernel:(Evm_node.initial_kernel observer)
           ())
      ~mode:
        (Observer
           {
             rollup_node_endpoint = None;
             evm_node_endpoint = Evm_node.endpoint new_sequencer;
           })
      ()
  in
  let* _ = Evm_node.wait_for_evm_event Sequencer_upgrade observer_bootstrap in
  unit

(* Builds the [change_sequencer_key] calldata: the new public key (Tezos binary
   encoding) and a Tezos signature produced by [sequencer_owner] (the current
   sequencer). The signed payload is a domain-separation tag followed by the new
   key bound to [chain_id] and to the current change [counter], each appended
   as a 32-byte big-endian integer:

     sign( tag ++ pk_bytes ++ be32(chain_id) ++ be32(counter) )

   [tag] is a unique domain-separation label (kept in sync with the kernel's
   [SEQUENCER_KEY_CHANGE_SIGN_TAG]). It makes the change signing domain
   disjoint from every other message the sequencer key signs (e.g. RLP-encoded
   blueprint chunks), so a signature captured elsewhere cannot be replayed here.

   The kernel recomputes [chain_id] (from the block config) and [counter] (from
   storage) itself — they are NOT part of the calldata — so a captured
   [(publicKey, signature)] is valid on this chain only, and only until the next
   change bumps the counter. It therefore cannot be replayed on another
   Etherlink chain nor after the key has rotated again. *)
let sequencer_key_change_sign_tag = "sequencer_change"

let be32_of_int n =
  let b = Bytes.make 32 '\000' in
  Bytes.set_int64_be b 24 (Int64.of_int n) ;
  b

let sequencer_key_change_calldata ~sequencer_owner ~chain_id ~counter ~new_key =
  let new_pk = Tezos_crypto.Signature.Public_key.of_b58check_exn new_key in
  let bytes =
    match
      Data_encoding.Binary.to_bytes
        Tezos_crypto.Signature.Public_key.encoding
        new_pk
    with
    | Ok bytes -> bytes
    | Error err ->
        failwith
          (Format.asprintf
             "Failed to encode public key: %a"
             Data_encoding.Binary.pp_write_error
             err)
  in
  let hex_pk = Hex.of_bytes bytes |> Hex.show in
  let signed_payload =
    Bytes.concat
      Bytes.empty
      [
        Bytes.of_string sequencer_key_change_sign_tag;
        bytes;
        be32_of_int chain_id;
        be32_of_int counter;
      ]
  in
  let hex_signature =
    match
      Data_encoding.Binary.to_bytes
        Tezos_crypto.Signature.encoding
        (Account.sign_bytes ~signer:sequencer_owner signed_payload)
    with
    | Ok bytes -> Hex.of_bytes bytes |> Hex.show
    | Error err ->
        failwith
          (Format.asprintf
             "Failed to encode signature: %a"
             Data_encoding.Binary.pp_write_error
             err)
  in
  (hex_pk, hex_signature)

(* Wraps the [change_sequencer_key] calldata in a fresh EVM transaction with an
   explicit [nonce], so the same inner calldata can be (re)submitted any number
   of times under different EVM-level nonces / senders. *)
let craft_sequencer_key_change_tx ~sender ~sequencer ~chain_id ~nonce ~hex_pk
    ~hex_signature =
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  Cast.craft_tx
    ~signature:"change_sequencer_key(bytes,bytes)"
    ~source_private_key:sender
    ~chain_id
    ~nonce
    ~gas_price
    ~gas:100_000
    ~value:(Wei.of_eth_int 1)
    ~address:Solidity_contracts.Precompile.sequencer_key_change
    ~arguments:[hex_pk; hex_signature]
    ()

let call_evm_based_sequencer_key_change ~sender ~sequencer ~sequencer_owner
    ~new_key =
  (* Fetch the chain_id and the current change counter from the node rather
     than hardcoding them, so the helper is valid on any chain and after any
     number of previous changes (the payload must be signed against the
     counter the *next* change will be checked against). *)
  let*@ chain_id = Rpc.get_chain_id sequencer in
  let*@ counter = Rpc.tez_getSequencerKeyChangeCounter sequencer in
  let counter = Int64.to_int counter in
  let hex_pk, hex_signature =
    sequencer_key_change_calldata ~sequencer_owner ~chain_id ~counter ~new_key
  in
  let* raw_sequencer_upgrade =
    craft_sequencer_key_change_tx
      ~sender
      ~sequencer
      ~chain_id
      ~nonce:0
      ~hex_pk
      ~hex_signature
  in
  let*@ tx = Rpc.send_raw_transaction ~raw_tx:raw_sequencer_upgrade sequencer in
  return tx

let test_sequencer_key_change =
  let sequencer_owner = Constant.bootstrap1 in
  let new_sequencer_owner = Constant.bootstrap2 in
  let genesis_timestamp_str = "2020-01-01T00:00:00Z" in
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn genesis_timestamp_str))
  in
  let activation_timestamp = "2020-01-02T01:00:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "sequencer_upgrade"]
    ~title:"Sequencer can update his key from EVM"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~sequencer:sequencer_owner
    ~additional_sequencer_keys:[new_sequencer_owner]
    ~genesis_timestamp
    ~instant_confirmations:true
  @@ fun {sequencer; sc_rollup_node; client; kernel; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let new_key = new_sequencer_owner.public_key in
  let*@ () =
    (* We must propose a timestamp else the sequencer set it to now *)
    Rpc.propose_next_block_timestamp ~timestamp:genesis_timestamp_str sequencer
  in
  let* tx =
    call_evm_based_sequencer_key_change
      ~sender:whale.private_key
      ~sequencer
      ~sequencer_owner
      ~new_key
  in
  let*@ _ = produce_block sequencer in
  (* timestamp was set with `propose_next_block_timestamp` *)
  let*@! Transaction.{logs; status; _} =
    Rpc.get_transaction_receipt ~tx_hash:tx sequencer
  in
  Check.((status = true) bool ~error_msg:"Transaction failed") ;
  Check.((List.length logs = 1) int ~error_msg:"Expected 1 log") ;
  let*@ _ = produce_block ~timestamp:activation_timestamp sequencer in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  let*@ _ = produce_block ~timestamp:activation_timestamp sequencer in
  let*@! value =
    Rpc.state_value sequencer (Durable_storage_path.sequencer kernel)
  in
  let expected_key = Hex.of_string new_key |> Hex.show in
  Check.(
    (value = expected_key)
      string
      ~error_msg:"Expected sequencer key to be %R, got %L") ;
  let* () = Evm_node.wait_for_blueprint_injected sequencer 3 in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  let* current_sequencer_in_rollup_hex =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:(Durable_storage_path.sequencer kernel)
         ()
  in
  Check.(
    (current_sequencer_in_rollup_hex = Some expected_key)
      (option string)
      ~error_msg:
        "Expected sequencer key in rollup to be %R, got %L (rollup might be \
         not in sync)") ;
  unit

let test_sequencer_key_change_fails_if_governance_upgrade_exists =
  let sequencer_owner = Constant.bootstrap1 in
  let new_sequencer_owner = Constant.bootstrap2 in
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2100-01-02T00:00:00Z" in

  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "sequencer_upgrade"]
    ~title:"EVM change key cannot work if governance one is pending"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~genesis_timestamp
    ~sequencer:sequencer_owner
    ~additional_sequencer_keys:[new_sequencer_owner]
  @@
  fun {sequencer; sc_rollup_address; sc_rollup_node; l1_contracts; client; _}
      _protocol
    ->
  let* () =
    sequencer_upgrade
      ~sc_rollup_address
      ~sequencer_admin:Constant.bootstrap2.alias
      ~sequencer_governance_contract:l1_contracts.sequencer_governance
      ~pool_address:Eth_account.bootstrap_accounts.(0).address
      ~client
      ~upgrade_to:sequencer_owner.alias
      ~activation_timestamp
  in
  (* Wait for the sequencer to receive the upgrade *)
  let waiting_sequencer_upgrade =
    Evm_node.wait_for_pending_sequencer_upgrade sequencer
  in
  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* _res = waiting_sequencer_upgrade in
  let whale = Eth_account.bootstrap_accounts.(0) in
  let new_key = new_sequencer_owner.public_key in
  let* tx =
    call_evm_based_sequencer_key_change
      ~sender:whale.private_key
      ~sequencer
      ~sequencer_owner
      ~new_key
  in
  let*@ _ = produce_block sequencer in
  let*@! Transaction.{status; _} =
    Rpc.get_transaction_receipt ~tx_hash:tx sequencer
  in
  Check.((status = false) bool ~error_msg:"Transaction should have failed") ;
  unit

(* Replaying [change_sequencer_key] must NOT reset the activation clock.

   The precompile has no caller guard (auth is by signature), so the
   [(publicKey, signature)] calldata of a legitimate change can be re-wrapped
   in a fresh EVM tx by anyone. Were a resubmission accepted, it would overwrite
   the pending change with a *fresh* [block.timestamp + SEQUENCER_UPGRADE_DELAY],
   letting anyone postpone the change indefinitely (griefing).

   The signed payload binds the new key to the chain_id and the change counter,
   and the counter is bumped as soon as a change is *stored* (not only when it is
   applied). So the captured signature becomes stale the moment the pending change
   is scheduled: any replay is signed against an outdated counter and reverts. This
   scenario schedules a change at genesis (activates at genesis + 1d), replays
   the very same calldata once before that deadline (which must be rejected), and
   checks the change still fires at its ORIGINAL deadline — i.e. the clock was
   not pushed back. *)
let test_sequencer_key_change_replay_doesnt_reset_activation =
  let sequencer_owner = Constant.bootstrap1 in
  let new_sequencer_owner = Constant.bootstrap2 in
  let genesis_timestamp_str = "2020-01-01T00:00:00Z" in
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn genesis_timestamp_str))
  in
  (* SEQUENCER_UPGRADE_DELAY = 86400s (1 day), so the genesis change activates
     at 2020-01-02T00:00:00Z; this deadline is just past it. *)
  let original_activation_deadline = "2020-01-02T01:00:00Z" in
  (* A replay lands here, before the activation. Were it accepted it would push
     activation to 2020-01-02T12:00:00Z, strictly after the deadline above. *)
  let replay_timestamp = "2020-01-01T12:00:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "sequencer_upgrade"]
    ~title:
      "Replaying change_sequencer_key calldata does not reset the activation \
       delay"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~sequencer:sequencer_owner
    ~additional_sequencer_keys:[new_sequencer_owner]
    ~genesis_timestamp
    ~instant_confirmations:true
  @@ fun {sequencer; sc_rollup_node; client; kernel; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let new_key = new_sequencer_owner.public_key in
  let new_key_hex = Hex.of_string new_key |> Hex.show in
  let*@ chain_id = Rpc.get_chain_id sequencer in
  (* No change has happened yet on this fresh chain, so the counter the next
     change must be signed against is 0. *)
  let*@ counter_before = Rpc.tez_getSequencerKeyChangeCounter sequencer in
  Check.(
    (counter_before = 0L)
      int64
      ~error_msg:"Expected genesis change counter %R, got %L") ;
  (* The sequencer signs ONE change to [new_key] (against the current counter,
     fetched above); this calldata is reused for every (re)submission below. *)
  let hex_pk, hex_signature =
    sequencer_key_change_calldata
      ~sequencer_owner
      ~chain_id
      ~counter:(Int64.to_int counter_before)
      ~new_key
  in
  (* [propose_next_block_timestamp] writes the timestamp into the blueprint, so
     the observer replays the same block deterministically. (Producing a
     tx-bearing block with [produce_block ~timestamp] instead diverges the
     observer.) *)
  let send_key_change ~nonce ~timestamp =
    let*@ () = Rpc.propose_next_block_timestamp ~timestamp sequencer in
    let* raw =
      craft_sequencer_key_change_tx
        ~sender:whale.private_key
        ~sequencer
        ~chain_id
        ~nonce
        ~hex_pk
        ~hex_signature
    in
    let*@ tx = Rpc.send_raw_transaction ~raw_tx:raw sequencer in
    let*@ _ = produce_block sequencer in
    let*@! Transaction.{status; _} =
      Rpc.get_transaction_receipt ~tx_hash:tx sequencer
    in
    return status
  in
  (* Block 1 @ genesis: schedule the legitimate change.
     Stored activation = genesis + 1d = 2020-01-02T00:00:00Z. *)
  let* status0 = send_key_change ~nonce:0 ~timestamp:genesis_timestamp_str in
  Check.((status0 = true) bool ~error_msg:"Initial change tx failed") ;
  (* Storing the change bumps the counter immediately (the new key was signed
     against counter 0, so that calldata is now stale), even though the change
     has not activated yet. *)
  let*@ counter_after = Rpc.tez_getSequencerKeyChangeCounter sequencer in
  Check.(
    (counter_after = 1L)
      int64
      ~error_msg:"Expected change counter %R after scheduling, got %L") ;
  (* Block 2 @ genesis+12h: REPLAY the same calldata. Its signature was made
     against counter 0, which is now stale, so the precompile rejects it; the
     original activation (2020-01-02T00:00:00Z) is left untouched. *)
  let* status1 = send_key_change ~nonce:1 ~timestamp:replay_timestamp in
  Check.(
    (status1 = false)
      bool
      ~error_msg:
        "Replay of the change calldata was accepted: a stale-counter signature \
         must not be able to reschedule the pending change") ;
  (* Block 3 @ original deadline: we are past the ORIGINAL activation
     (2020-01-02T00:00:00Z). Since the replay was rejected, the clock was not
     pushed back, so the change has fired and the sequencer key is the new
     one. *)
  let*@ () =
    Rpc.propose_next_block_timestamp
      ~timestamp:original_activation_deadline
      sequencer
  in
  let*@ _ = produce_block sequencer in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  let*@! value =
    Rpc.state_value sequencer (Durable_storage_path.sequencer kernel)
  in
  Check.(
    (value = new_key_hex)
      string
      ~error_msg:
        "Change did not fire at its original deadline: expected the key to be \
         %R, but got %L") ;
  (* Applying the change must NOT bump the counter a second time: the bump
     already happened when the change was stored. A single change therefore
     advances the counter by exactly one (1, not 2). A post-activation value of
     2 would mean the apply path double-counts, which would silently reject a
     legitimately pre-signed next change. *)
  let*@ counter_applied = Rpc.tez_getSequencerKeyChangeCounter sequencer in
  Check.(
    (counter_applied = 1L)
      int64
      ~error_msg:
        "Expected change counter %R after the change activated, got %L (a \
         value of 2 means the apply path double-incremented)") ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_sequencer_upgrade protocols ;
  test_duplicate_sequencer_upgrade [Protocol.Alpha] ;
  test_sequencer_key_change [Alpha] ;
  test_sequencer_key_change_fails_if_governance_upgrade_exists [Alpha] ;
  test_sequencer_key_change_replay_doesnt_reset_activation [Alpha]
