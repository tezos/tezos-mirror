(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: TX Kernel
   Requirements: make build-kernels
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_sc_rollup.ml

   Tests in this file originate tx kernel rollup
   from tx_kernel.wasm file using reveal_installer and DAC.
*)

open Base
open Sc_rollup_helpers

let send_message ?(src = Constant.bootstrap2.alias) client msg =
  let* () = Client.Sc_rollup.send_message ~hooks ~src ~msg client in
  Client.bake_for_and_wait client

(* TX Kernel external messages and their encodings *)
module Tx_kernel = struct
  open Tezos_protocol_alpha.Protocol
  open Tezos_crypto.Signature

  type ticket = {
    ticketer : Alpha_context.Contract.t;
    content : string;
    amount : int;
  }

  let ticket_of ~ticketer ~content amount =
    {
      ticketer = Result.get_ok (Alpha_context.Contract.of_b58check ticketer);
      content;
      amount;
    }

  (* Primitive operation of tx kernel.
     Several primitive operations might be outgoing from the same account.
     Corresponds to kernel_core::inbox::external::v1::OperationContent
     type in the tx kernel. *)
  type operation =
    | Withdrawal of {
        receiver_contract : Contract_hash.t;
        ticket : ticket;
        entrypoint : string;
      }
    | Transfer of {
        (* tz4 address *)
        destination : Tezos_crypto.Signature.Bls.Public_key_hash.t;
        ticket : ticket;
      }

  (* List of elemtary operations which are outgoing from the same account.
     Corresponds to a pair of
       * kernel_core::bls::BlsKey and
       * kernel_core::inbox::external::v1::verifiable::VerifiableOperation
     in the tx kernel.
     VerifiableOperation::signer is replaced by its private key
     in order to be able to sign outer [multiaccount_tx].
     In terms of the tx kernel it is rather *sending* type not *verifiable*.
  *)
  type account_operations = {
    signer : Bls.Secret_key.t;
    counter : int64;
    operations : operation list;
  }

  let account_operations_of ~sk ~counter operations =
    {signer = sk; counter; operations}

  (* Several account operations.
     Content of this tx signed by each account,
     and stored in the aggregated signature.
     Corresponds to
        kernel_core::inbox::external::v1::verifiable::VerifiableTransaction type
     in the tx kernel
  *)
  type multiaccount_tx = {
    accounts_operations : account_operations list;
    encoded_accounts_ops : string; (* just encoded concatenated list above *)
    aggregated_signature : Bls.t;
  }

  (* Batch of multiaccount transactions.
     Corresponds to
       kernel_core::inbox::external::v1::ParsedBatch type
     in the tx kernel
  *)
  type transactions_batch = {
    transactions : multiaccount_tx list;
    encoded_transactions : string; (* just encoded concatenated list above *)
    aggregated_signature : Bls.t;
  }

  module Encodings = struct
    let list_encode xs =
      Data_encoding.(Binary.to_string_exn string @@ String.concat "" xs)

    (* String ticket encoding for tx kernel.
       Corresponds to kernel_core::encoding::string_ticket::StringTicketRepr *)
    let ticket_repr {ticketer; content; amount} : string =
      let open Tezos_protocol_alpha.Protocol.Alpha_context in
      Printf.sprintf
        "\007\007\n\000\000\000\022%s\007\007\001%s\000%s"
        Data_encoding.(Binary.to_string_exn Contract.encoding ticketer)
        Data_encoding.(Binary.to_string_exn bytes @@ Bytes.of_string content)
        Data_encoding.(Binary.to_string_exn z @@ Z.of_int amount)

    (* Encoding of kernel_core::inbox::external::v1::OperationContent from tx_kernel *)
    let operation_repr = function
      | Withdrawal {receiver_contract; ticket; entrypoint} ->
          let open Alpha_context in
          let withdrawal_prefix = "\000" in
          let contract_bytes =
            Data_encoding.(
              Binary.to_string_exn Contract_hash.encoding receiver_contract)
          in
          let entrypoint_bytes =
            Data_encoding.(
              Entrypoint.of_string_strict_exn entrypoint
              |> Binary.to_string_exn Entrypoint.simple_encoding)
          in
          withdrawal_prefix ^ contract_bytes ^ ticket_repr ticket
          ^ entrypoint_bytes
      | Transfer {destination; ticket} ->
          let transfer_prefix = "\001" in
          let tz4address =
            Data_encoding.(
              Binary.to_string_exn
                Tezos_crypto.Signature.Bls.Public_key_hash.encoding
                destination)
          in
          transfer_prefix ^ tz4address ^ ticket_repr ticket

    let account_operations_repr {signer; counter; operations} : string =
      let signer_bytes =
        if Int64.equal counter 0L then
          "\000" (* PK signer tag *)
          ^ Data_encoding.(
              Bls.Secret_key.to_public_key signer
              |> Binary.to_string_exn Bls.Public_key.encoding)
        else
          "\001" (* tz4address signer tag *)
          ^ Data_encoding.(
              Bls.Secret_key.to_public_key signer
              |> Bls.Public_key.hash
              |> Binary.to_string_exn Bls.Public_key_hash.encoding)
      in
      let counter = Data_encoding.(Binary.to_string_exn int64 counter) in
      let contents = list_encode @@ List.map operation_repr operations in
      signer_bytes ^ counter ^ contents

    let list_of_account_operations_repr
        (accounts_operations : account_operations list) : string =
      let account_ops_encoded =
        List.map account_operations_repr accounts_operations
      in
      list_encode account_ops_encoded

    let list_of_multiaccount_tx_encoding (transactions : multiaccount_tx list) =
      let txs_encodings =
        List.map (fun x -> x.encoded_accounts_ops) transactions
      in
      list_encode txs_encodings
  end

  let multiaccount_tx_of (accounts_operations : account_operations list) =
    let encoded_accounts_ops =
      Encodings.list_of_account_operations_repr accounts_operations
    in
    let accounts_sks = List.map (fun x -> x.signer) accounts_operations in
    (* List consisting of single transaction, that is fine *)
    let aggregated_signature =
      Option.get
      @@ Bls.(
           aggregate_signature_opt
           @@ List.map
                (fun sk -> sign sk @@ Bytes.of_string encoded_accounts_ops)
                accounts_sks)
    in
    assert (
      Bls.aggregate_check
        (List.map
           (fun sk ->
             ( Bls.Secret_key.to_public_key sk,
               None,
               Bytes.of_string encoded_accounts_ops ))
           accounts_sks)
        aggregated_signature) ;
    {accounts_operations; encoded_accounts_ops; aggregated_signature}

  let transactions_batch_of (transactions : multiaccount_tx list) =
    let encoded_transactions =
      Encodings.list_of_multiaccount_tx_encoding transactions
    in
    let signatures =
      List.map
        (fun (x : multiaccount_tx) -> x.aggregated_signature)
        transactions
    in
    let aggregated_signature =
      Option.get @@ Bls.aggregate_signature_opt signatures
    in
    {transactions; encoded_transactions; aggregated_signature}

  let external_message_of_batch (batch : transactions_batch) =
    let v1_batch_prefix = "\000" in
    let signature =
      batch.aggregated_signature |> Tezos_crypto.Signature.Bls.to_bytes
      |> Bytes.to_string
    in
    hex_encode @@ v1_batch_prefix ^ batch.encoded_transactions ^ signature

  (* External message consisting of single transaction. *)
  let external_message_of_account_ops (accounts_ops : account_operations list) =
    external_message_of_batch @@ transactions_batch_of
    @@ [multiaccount_tx_of accounts_ops]
end

let assert_state_changed ?block sc_rollup_node prev_state_hash =
  let* state_hash =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ?block ()
  in
  Check.(state_hash <> prev_state_hash)
    Check.string
    ~error_msg:"State hash has not changed (%L <> %R)" ;
  Lwt.return_unit

let assert_ticks_advanced ?block sc_rollup_node prev_ticks =
  let* ticks =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ?block ()
  in
  Check.(ticks > prev_ticks)
    Check.int
    ~error_msg:"Tick counter did not advance (%L > %R)" ;
  Lwt.return_unit

let rec bake_until cond client sc_rollup_node =
  let* stop = cond client in
  if stop then unit
  else
    let* () = Client.bake_for_and_wait client in
    let* current_level = Client.level client in
    let* _ =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node current_level
    in
    bake_until cond client sc_rollup_node

let setup_classic ~commitment_period ~challenge_window protocol =
  let* node, client = setup_l1 ~commitment_period ~challenge_window protocol in
  let bootstrap1_key = Constant.bootstrap1.alias in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:bootstrap1_key
  in
  let* {boot_sector; _} =
    prepare_installer_kernel
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      Constant.WASM.tx_kernel
  in
  (* Initialise the sc rollup *)
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"rollup"
      ~src:bootstrap1_key
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~parameters_ty:"pair string (ticket string)"
      client
  in
  let* () = Client.bake_for_and_wait client in
  return (client, sc_rollup_node, sc_rollup_address, [])

let setup_bootstrap ~commitment_period ~challenge_window protocol =
  let sc_rollup_address = "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57" in
  let* {
         bootstrap_smart_rollup = bootstrap_tx_kernel;
         smart_rollup_node_data_dir;
         smart_rollup_node_extra_args;
       } =
    setup_bootstrap_smart_rollup
      ~name:"tx_kernel"
      ~address:sc_rollup_address
      ~parameters_ty:"pair string (ticket string)"
      ~installee:Constant.WASM.tx_kernel
      ()
  in
  let bootstrap1_key = Constant.bootstrap1.alias in
  let* node, client =
    setup_l1
      ~bootstrap_smart_rollups:[bootstrap_tx_kernel]
      ~commitment_period
      ~challenge_window
      protocol
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~data_dir:smart_rollup_node_data_dir
      ~base_dir:(Client.base_dir client)
      ~default_operator:bootstrap1_key
  in
  let* () = Client.bake_for_and_wait client in
  return
    (client, sc_rollup_node, sc_rollup_address, smart_rollup_node_extra_args)

let tx_kernel_e2e setup protocol =
  let open Tezt_tx_kernel in
  let commitment_period = 10 and challenge_window = 10 in
  let* client, sc_rollup_node, sc_rollup_address, node_args =
    setup ~commitment_period ~challenge_window protocol
  in

  (* Run the rollup node, ensure origination succeeds. *)
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address node_args in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  (* Originate a contract that will mint and transfer tickets to the tx kernel. *)
  let* mint_and_deposit_contract =
    Tezt_tx_kernel.Contracts.prepare_mint_and_deposit_contract client protocol
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in

  (* gen two tz1 accounts *)
  let pkh1, pk1, sk1 = Tezos_crypto.Signature.Ed25519.generate_key () in
  let pkh2, pk2, sk2 = Tezos_crypto.Signature.Ed25519.generate_key () in
  let ticket_content = "Hello, Ticket!" in
  let ticketer =
    Tezos_protocol_alpha.Protocol.Contract_hash.to_b58check
      mint_and_deposit_contract
  in

  (* Deposit *)
  let* () =
    Tezt_tx_kernel.Contracts.deposit_string_tickets
      ~hooks
      client
      ~mint_and_deposit_contract:ticketer
      ~sc_rollup_address
      ~destination_l2_addr:
        (Tezos_crypto.Signature.Ed25519.Public_key_hash.to_b58check pkh1)
      ~ticket_content
      ~amount:450
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in

  (* Construct transfer *)
  let sc_rollup_hash =
    Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_exn sc_rollup_address
  in
  let transfer_message =
    Transaction_batch.(
      empty
      |> add_transfer
           ~counter:0
           ~signer:(Public_key pk1)
           ~signer_secret_key:sk1
           ~destination:pkh2
           ~ticketer
           ~ticket_content
           ~amount:60
      |> add_transfer
           ~counter:0
           ~signer:(Public_key pk2)
           ~signer_secret_key:sk2
           ~destination:pkh1
           ~ticketer
           ~ticket_content
           ~amount:10
      |> make_encoded_batch ~wrap_with:(`External_message_frame sc_rollup_hash)
      |> hex_encode)
  in

  (* Send transfers *)
  let* prev_state_hash =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ()
  in
  let* () = send_message client (sf "hex:[%S]" transfer_message) in

  let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  let* () = assert_state_changed sc_rollup_node prev_state_hash in

  (* After that pkh1 has 400 tickets, pkh2 has 50 tickets *)

  (**** Withdrawal part ****)
  (* originate ticket receiver contract *)
  let* receive_tickets_contract =
    Tezt_tx_kernel.Contracts.prepare_receive_withdrawn_tickets_contract
      client
      protocol
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  (* pk withdraws part of his tickets, pk2 withdraws all of his tickets *)
  let withdraw_message =
    Transaction_batch.(
      empty
      |> add_withdraw
           ~counter:1
           ~signer:(Public_key pk1)
           ~signer_secret_key:sk1
           ~destination:receive_tickets_contract
           ~entrypoint:"receive_tickets"
           ~ticketer
           ~ticket_content
           ~amount:220
      |> add_withdraw
           ~counter:1
           ~signer:(Public_key pk2)
           ~signer_secret_key:sk2
           ~destination:receive_tickets_contract
           ~entrypoint:"receive_tickets"
           ~ticketer
           ~ticket_content
           ~amount:40
      |> make_encoded_batch ~wrap_with:(`External_message_frame sc_rollup_hash)
      |> hex_encode)
  in
  (* Send withdrawal *)
  let* prev_state_hash =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ()
  in
  let* prev_ticks =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let* () = send_message client (sf "hex:[%S]" withdraw_message) in
  let* withdrawal_level =
    Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node
  in

  let* _, last_lcc_level =
    Sc_rollup_helpers.last_cemented_commitment_hash_with_level
      ~sc_rollup:sc_rollup_address
      client
  in
  let next_lcc_level = last_lcc_level + commitment_period in

  (* Bake until the next commitment is cemented, this commitment
     will include the withdrawal. *)
  let* () =
    bake_until
      (fun client ->
        let* _, lcc_level =
          Sc_rollup_helpers.last_cemented_commitment_hash_with_level
            ~sc_rollup:sc_rollup_address
            client
        in
        return (lcc_level = next_lcc_level))
      client
      sc_rollup_node
  in

  let block = string_of_int next_lcc_level in
  let* () = assert_state_changed ~block sc_rollup_node prev_state_hash in
  let* () = assert_ticks_advanced ~block sc_rollup_node prev_ticks in

  (* EXECUTE withdrawal *)
  let* outbox =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_outbox ~outbox_level:withdrawal_level ()
  in
  Log.info "Outbox is %s" @@ JSON.encode outbox ;
  let execute_outbox_proof ~message_index =
    let outbox_level = withdrawal_level in
    let* proof =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.outbox_proof_simple ~message_index ~outbox_level ()
    in
    match proof with
    | Some {commitment_hash; proof} ->
        let*! () =
          Client.Sc_rollup.execute_outbox_message
            ~hooks
            ~burn_cap:(Tez.of_int 10)
            ~rollup:sc_rollup_address
            ~src:Constant.bootstrap1.alias
            ~commitment_hash
            ~proof
            client
        in
        Client.bake_for_and_wait client
    | _ -> failwith "Unexpected error during proof generation"
  in
  let* () = execute_outbox_proof ~message_index:0 in
  let* () = execute_outbox_proof ~message_index:1 in
  unit

let register_test ?supports ?(regression = false) ~__FILE__ ~tags ?uses ~title f
    =
  let tags = "tx_sc_rollup" :: tags in
  if regression then
    Protocol.register_regression_test ?supports ~__FILE__ ~title ~tags ?uses f
  else Protocol.register_test ?supports ~__FILE__ ~title ~tags ?uses f

let test_tx_kernel_e2e =
  register_test
    ~regression:true
    ~__FILE__
    ~tags:["wasm"; "kernel"; "wasm_2_0_0"; "kernel_e2e"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
        Constant.WASM.tx_kernel;
      ])
    ~title:(Printf.sprintf "wasm_2_0_0 - tx kernel should run e2e (kernel_e2e)")
    (tx_kernel_e2e setup_classic)

let test_bootstrapped_tx_kernel_e2e =
  register_test
    ~supports:(Protocol.From_protocol 018)
    ~__FILE__
    ~tags:["wasm"; "kernel"; "wasm_2_0_0"; "kernel_e2e"; "bootstrap"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
        Constant.WASM.tx_kernel;
      ])
    ~title:
      (Printf.sprintf
         "wasm_2_0_0 - bootstrapped tx kernel should run e2e (kernel_e2e)")
    (tx_kernel_e2e setup_bootstrap)

let register ~protocols =
  test_tx_kernel_e2e protocols ;
  test_bootstrapped_tx_kernel_e2e protocols
