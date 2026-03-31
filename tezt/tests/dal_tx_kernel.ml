(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL TX kernel end-to-end tests. *)

open Dal_helpers
module Dal = Dal_common
open Tezos_protocol_alpha.Protocol
open Tezt_tx_kernel

(** [keys_of_account account] returns a triplet of pk, pkh, sk of [account]  *)
let keys_of_account (account : Account.key) =
  let pk =
    account.public_key
    |> Tezos_crypto.Signature.Ed25519.Public_key.of_b58check_exn
  in
  let pkh =
    account.public_key_hash
    |> Tezos_crypto.Signature.Ed25519.Public_key_hash.of_b58check_exn
  in
  let sk =
    account.secret_key
    |> Account.require_unencrypted_secret_key ~__LOC__
    |> Tezos_crypto.Signature.Ed25519.Secret_key.of_b58check_exn
  in
  (pk, pkh, sk)

(** [get_ticket_balance  ~pvm_name ~pkh ~ticket_index] returns
      the L2 balance of the account with [pkh] for the ticket with [ticket_index] *)
let get_ticket_balance sc_rollup_node ~pvm_name ~pkh ~ticket_index =
  Sc_rollup_node.RPC.call sc_rollup_node
  @@ Sc_rollup_rpc.get_global_block_durable_state_value
       ~pvm_kind:pvm_name
       ~operation:Sc_rollup_rpc.Value
       ~key:
         (sf
            "/accounts/%s/%d"
            (Tezos_crypto.Signature.Ed25519.Public_key_hash.to_b58check pkh)
            ticket_index)
       ()

(** E2E test using the tx-kernel. Scenario:
      1. Deposit [450] tickets to the L2 [pk1] using the deposit contract.
      2. Construct two batches of L2 transactions to achieve the following:
          1. Transfer [60] tickets from [pk1] to [pk2].
          2. Withdraw [60] tickets from [pk2].
          3. Transfer [90] tickets from [pk1] to [pk2].
          3. Withdraw [40] tickets from [pk2].
      3. Publish the transaction batch to DAL at slot [0].
      4. Bake [attestation_lag] blocks and attest slot [0].
      5. Check that the L2 [pk1] has [300] tickets and [pk2] has [50] tickets.
   *)
let test_tx_kernel_e2e protocol parameters dal_node sc_rollup_node
    _sc_rollup_address node client pvm_name =
  Log.info "Originate the tx kernel." ;
  let* {boot_sector; _} =
    Sc_rollup_helpers.prepare_installer_kernel
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      Constant.WASM.tx_kernel_dal
  in
  (* The kernel is badly written and may ask pages in negative
       levels. We ensure it is not possible by baking enough
       blocks. *)
  let* () = bake_for ~count:parameters.Dal.Parameters.attestation_lag client in
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"tx_kernel_dal"
      ~src:Constant.bootstrap1.public_key_hash
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~parameters_ty:"pair string (ticket string)"
      client
  in
  let* () = bake_for client in
  Log.info "Run the rollup node and ensure origination succeeds." ;
  let* genesis_info =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  (* Set up event handler to capture kernel debug output for regression test *)
  let () =
    Sc_rollup_node.on_event
      sc_rollup_node
      (fun Sc_rollup_node.{name; value; _} ->
        if name = "kernel_debug.v0" then
          Regression.capture
            (Tx_kernel_helpers.replace_variables (JSON.as_string value)))
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  Log.info "Create key pairs used for the test." ;
  let pk1, pkh1, sk1 = keys_of_account Constant.bootstrap1 in
  let pk2, pkh2, sk2 = keys_of_account Constant.bootstrap2 in
  Log.info "Prepare contract for minting and depositing." ;
  let* mint_and_deposit_contract =
    Contracts.prepare_mint_and_deposit_contract client protocol
  in
  Log.info "Deposit [450] tickets to the L2 [pk1] using deposit contract." ;
  let ticket_content = "Hello, Ticket!" in
  let* () =
    Contracts.deposit_string_tickets
      client
      ~mint_and_deposit_contract:
        (Contract_hash.to_b58check mint_and_deposit_contract)
      ~sc_rollup_address
      ~destination_l2_addr:
        (Tezos_crypto.Signature.Ed25519.Public_key_hash.to_b58check pkh1)
      ~ticket_content
      ~amount:450
  in
  Log.info "Prepare contract for receiving withdrawn tickets." ;
  let* receive_withdrawn_tickets_contract =
    Contracts.prepare_receive_withdrawn_tickets_contract client protocol
  in
  (* We have slot index [0] hard-coded in the kernel.

       TODO: https://gitlab.com/tezos/tezos/-/issues/6390
       Make it possible to dynamically change tracked slot indexes. *)
  let slot_index = 0 in
  Log.info
    "Construct a batch of L2 transactions that:\n\
     1. Transfers [60] tickets from [pk1] to [pk2].\n\
     3. Withdraw [60] tickets from [pk2] to \
     [receive_withdrawn_tickets_contract]." ;
  let payload1 =
    Transaction_batch.(
      empty
      |> add_transfer
           ~counter:0
           ~signer:(Public_key pk1)
           ~signer_secret_key:sk1
           ~destination:pkh2
           ~ticketer:(Contract_hash.to_b58check mint_and_deposit_contract)
           ~ticket_content
           ~amount:60
      |> add_withdraw
           ~counter:0
           ~signer:(Public_key pk2)
           ~signer_secret_key:sk2
           ~destination:receive_withdrawn_tickets_contract
           ~entrypoint:"receive_tickets"
           ~ticketer:(Contract_hash.to_b58check mint_and_deposit_contract)
           ~ticket_content
           ~amount:60
      |> make_encoded_batch)
  in
  Log.info
    "Construct a batch of L2 transactions that:\n\
     1. Transfers [90] tickets from [pk1] to [pk2].\n\
     3. Withdraw [50] tickets from [pk2] to \
     [receive_withdrawn_tickets_contract]." ;
  let payload2 =
    Transaction_batch.(
      empty
      |> add_transfer
           ~counter:1
           ~signer:(Public_key pk1)
           ~signer_secret_key:sk1
           ~destination:pkh2
           ~ticketer:(Contract_hash.to_b58check mint_and_deposit_contract)
           ~ticket_content
           ~amount:90
      |> add_withdraw
           ~counter:1
           ~signer:(Public_key pk2)
           ~signer_secret_key:sk2
           ~destination:receive_withdrawn_tickets_contract
           ~entrypoint:"receive_tickets"
           ~ticketer:(Contract_hash.to_b58check mint_and_deposit_contract)
           ~ticket_content
           ~amount:40
      |> make_encoded_batch)
  in
  Log.info
    "Publish the [payload1] of size %d to DAL at slot [0]."
    (String.length payload1) ;
  let* () =
    publish_store_and_attest_slot
      ~protocol
      client
      node
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      ~content:
        (Helpers.make_slot
           ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
           payload1)
      parameters
  in
  Log.info
    "Publish the [payload2] of size %d to DAL at slot [0]."
    (String.length payload2) ;
  let* () =
    publish_store_and_attest_slot
      ~protocol
      client
      node
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      ~content:
        (Helpers.make_slot
           ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
           payload2)
      parameters
  in
  (* Before importing a slot, we wait 2 blocks for finality + 1 block for DAL
       node processing. *)
  let* () = bake_for client in
  let* () = bake_for client in
  let* () = bake_for client in

  Log.info "Wait for the rollup node to catch up." ;
  let* current_level = Node.get_level node in
  let* _level =
    Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node current_level
  in
  Log.info "Check that [pk1] has [300] tickets." ;
  let* balance =
    get_ticket_balance sc_rollup_node ~pvm_name ~ticket_index:0 ~pkh:pkh1
  in
  Check.(
    (* [2c01000000000000] is [300] when interpreted as little-endian u64. *)
    (balance = Some "2c01000000000000")
      ~__LOC__
      (option string)
      ~error_msg:"Expected %R, got %L") ;
  Log.info "Check that [pk2] has [50] tickets." ;
  let* balance =
    get_ticket_balance sc_rollup_node ~pvm_name ~ticket_index:0 ~pkh:pkh2
  in
  Check.(
    (* [3200000000000000] is [50] when interpreted as little-endian u64. *)
    (balance = Some "3200000000000000")
      (option string)
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  unit

let test_echo_kernel_e2e protocol parameters dal_node sc_rollup_node
    _sc_rollup_address node client pvm_name =
  Log.info "Originate the echo kernel." ;
  let* {boot_sector; _} =
    Sc_rollup_helpers.prepare_installer_kernel
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      Constant.WASM.dal_echo_kernel
  in
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"dal_echo_kernel"
      ~src:Constant.bootstrap1.public_key_hash
      ~kind:pvm_name
      ~boot_sector
      ~parameters_ty:"unit"
      client
  in
  let* () = bake_for client in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let* current_level = Node.get_level node in
  let target_level =
    current_level + parameters.Dal.Parameters.attestation_lag + 1
  in
  let payload = "hello" in
  let* () =
    publish_store_and_attest_slot
      ~protocol
      client
      node
      dal_node
      Constant.bootstrap1
      ~index:0
      ~content:
        (Helpers.make_slot
           ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
           payload)
      parameters
  in

  (* Before importing a slot, we wait 2 blocks for finality + 1 block for DAL
       node processing *)
  let* () = repeat 3 (fun () -> bake_for client) in
  let* _level = Sc_rollup_node.wait_sync ~timeout:10. sc_rollup_node in
  Log.info "Wait for the rollup node to catch up at level %d." target_level ;
  let* _level =
    Sc_rollup_node.wait_for_level ~timeout:60. sc_rollup_node target_level
  in
  let key = "/output/slot-0" in
  let* value_written =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:pvm_name
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  match value_written with
  | None -> Test.fail "Expected a value to be found. But none was found."
  | Some value ->
      let value = `Hex value |> Hex.to_string in
      Check.(
        (String.length value = parameters.Dal.Parameters.cryptobox.slot_size)
          int
          ~error_msg:"Expected a value of size %R. Got %L") ;
      if String.starts_with ~prefix:payload value then unit
      else
        let message =
          Format.asprintf
            "Expected the payload '%s' to be a prefix of the value written. \
             Instead found: %s"
            payload
            (String.sub value 0 (String.length payload))
        in
        Test.fail "%s" message

let test_manual_echo_kernel_for_bandwidth protocol parameters dal_node
    sc_rollup_node _sc_rollup_address node client pvm_name =
  Log.info "Originate the echo kernel." ;
  let config =
    Sc_rollup_helpers.Installer_kernel_config.
      [
        Set
          {
            to_ = "/slots";
            (* Only the slot 0 *)
            value = Z.to_bits Z.one |> Hex.of_string |> Hex.show;
          };
      ]
  in
  let* {boot_sector; _} =
    Sc_rollup_helpers.prepare_installer_kernel
      ~config:(`Config config)
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      Constant.WASM.dal_echo_kernel_bandwidth
  in
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"dal_echo_kernel_bandwidth"
      ~src:Constant.bootstrap1.public_key_hash
      ~kind:pvm_name
      ~boot_sector
      ~parameters_ty:"unit"
      client
  in
  let* () = bake_for client in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let* current_level = Node.get_level node in
  let target_level =
    current_level + parameters.Dal.Parameters.attestation_lag + 1
  in
  let payload = "hello" in
  let* () =
    publish_store_and_attest_slot
      ~protocol
      client
      node
      dal_node
      Constant.bootstrap1
      ~index:0
      ~content:
        (Helpers.make_slot
           ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
           payload)
      parameters
  in
  Log.info "Wait for the rollup node to catch up." ;
  let* _level =
    Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node target_level
  in
  let key = "/output/slot-0" in
  let* value_written =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:pvm_name
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  match value_written with
  | None -> Test.fail "Expected a value to be found. But none was found."
  | Some value ->
      let value = `Hex value |> Hex.to_string |> Z.of_bits in
      Check.(
        (Z.to_int value = parameters.cryptobox.slot_size)
          int
          ~error_msg:"Expected number of bytes length %R. Got %L") ;
      unit
