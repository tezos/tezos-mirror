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
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_sc_rollup.ml

   Tests in this file originate tx kernel rollup
   from tx-kernel.wasm file using reveal_installer and DAC.
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

let assert_state_changed sc_rollup_client prev_state_hash =
  let*! state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client in
  Check.(state_hash <> prev_state_hash)
    Check.string
    ~error_msg:"State hash has not changed (%L <> %R)" ;
  Lwt.return_unit

let assert_ticks_advanced sc_rollup_client prev_ticks =
  let*! ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
  Check.(ticks > prev_ticks)
    Check.int
    ~error_msg:"Tick counter did not advance (%L > %R)" ;
  Lwt.return_unit

(* Send a deposit into the rollup. *)
let test_deposit ~client ~sc_rollup_node ~sc_rollup_client ~sc_rollup_address
    ~mint_and_deposit_contract level tz4_address =
  let*! prev_state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client in
  let* () =
    (* Internal message through forwarder *)
    let arg =
      sf
        {|Pair (Pair %S "%s") (Pair 450 "Hello, Ticket!")|}
        sc_rollup_address
        tz4_address
    in
    Client.transfer
      client
      ~hooks
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:mint_and_deposit_contract
      ~arg
      ~burn_cap:(Tez.of_int 1000)
  in
  let* () = Client.bake_for_and_wait client in
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node (level + 1)
  in
  let* () = assert_state_changed sc_rollup_client prev_state_hash in
  Lwt.return @@ (level + 1)

let test_tx_kernel_e2e protocol =
  let commitment_period = 2 and challenge_window = 5 in
  let* node, client = setup_l1 ~commitment_period ~challenge_window protocol in
  let bootstrap1_key = Constant.bootstrap1.alias in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:bootstrap1_key
  in
  let* boot_sector =
    prepare_installer_kernel
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      "tx-kernel"
  in
  (* Initialise the sc rollup *)
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~src:bootstrap1_key
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~parameters_ty:"pair string (ticket string)"
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node sc_rollup_address
  in

  (* Run the rollup node, ensure origination succeeds. *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node init_level
  in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;

  (* Originate a contract that will mint and transfer tickets to the tx kernel. *)
  (* Originate forwarder contract to send internal messages to rollup *)
  let* _, mint_and_deposit_contract =
    Client.originate_contract_at (* ~alias:"rollup_deposit" *)
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 1)
      client
      ["mini_scenarios"; "smart_rollup_mint_and_deposit_ticket"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  Log.info
    "The mint and deposit contract %s was successfully originated"
    mint_and_deposit_contract ;
  let level = init_level + 1 in

  (* gen two tz4 accounts *)
  let pkh, _pk, sk = Tezos_crypto.Signature.Bls.generate_key () in
  let pkh2, _pk2, sk2 = Tezos_crypto.Signature.Bls.generate_key () in
  (* Deposit *)
  let* level =
    test_deposit
      ~client
      ~sc_rollup_node
      ~sc_rollup_client
      ~sc_rollup_address
      ~mint_and_deposit_contract
      level
    @@ Tezos_crypto.Signature.Bls.Public_key_hash.to_b58check pkh
  in
  (* Construct transfer *)
  let ticket amount =
    Tx_kernel.ticket_of
      ~ticketer:mint_and_deposit_contract
      ~content:"Hello, Ticket!"
      amount
  in
  let transfer_message =
    Tx_kernel.(
      [
        multiaccount_tx_of
          [
            (* Transfer 50 tickets *)
            account_operations_of
              ~sk
              ~counter:0L
              [Transfer {destination = pkh2; ticket = ticket 60}];
            (* Transfer 10 tickets back *)
            account_operations_of
              ~sk:sk2
              ~counter:0L
              [Transfer {destination = pkh; ticket = ticket 10}];
          ];
        multiaccount_tx_of
          [
            (* Transfer another 10 tickets back but in a separate tx *)
            account_operations_of
              ~sk:sk2
              ~counter:1L
              [Transfer {destination = pkh; ticket = ticket 10}];
          ];
      ]
      |> transactions_batch_of |> external_message_of_batch)
  in

  (* Send transfers *)
  let*! prev_state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client in
  let* () = send_message client (sf "hex:[%S]" transfer_message) in
  let level = level + 1 in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level in
  let* () = assert_state_changed sc_rollup_client prev_state_hash in

  (* After that pkh has 410 tickets, pkh2 has 40 tickets *)

  (**** Withdrawal part ****)
  (* originate ticket receiver contract *)
  let* _, receive_tickets_contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.(of_int 1)
      client
      ["mini_scenarios"; "smart_rollup_receive_tickets"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  Log.info
    "The receiver contract %s was successfully originated"
    receive_tickets_contract ;
  let level = level + 1 in
  (* Construct withdrawal *)
  let withdrawal_op amount =
    Tx_kernel.Withdrawal
      {
        receiver_contract =
          Tezos_protocol_alpha.Protocol.Contract_hash.of_b58check_exn
            receive_tickets_contract;
        ticket = ticket amount;
        entrypoint = "receive_tickets";
      }
  in
  (* pk withdraws part of his tickets, pk2 withdraws all of his tickets *)
  let withdraw_message =
    Tx_kernel.(
      [
        account_operations_of
          ~sk
          ~counter:1L
          [withdrawal_op 220; withdrawal_op 100];
        account_operations_of ~sk:sk2 ~counter:2L [withdrawal_op 40];
      ]
      |> external_message_of_account_ops)
  in
  (* Send withdrawal *)
  let*! prev_state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client in
  let*! prev_ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
  let* () = send_message client (sf "hex:[%S]" withdraw_message) in
  let withdrawal_level = level + 1 in
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node withdrawal_level
  in
  let* () = assert_state_changed sc_rollup_client prev_state_hash in
  let* () = assert_ticks_advanced sc_rollup_client prev_ticks in

  (* EXECUTE withdrawal *)
  let blocks_to_wait = 1 + (2 * commitment_period) + challenge_window in
  let* () =
    repeat blocks_to_wait @@ fun () -> Client.bake_for_and_wait client
  in
  let*! outbox =
    Sc_rollup_client.outbox ~outbox_level:withdrawal_level sc_rollup_client
  in
  Log.info "Outbox is %s" @@ JSON.encode outbox ;
  let* answer =
    let message_index = 0 in
    let outbox_level = withdrawal_level in
    let destination = receive_tickets_contract in
    let open Tezos_protocol_alpha.Protocol.Alpha_context in
    let ticketer =
      mint_and_deposit_contract |> Contract.of_b58check |> Result.get_ok
      |> Data_encoding.(Binary.to_string_exn Contract.encoding)
      |> hex_encode
    in
    let parameters d =
      Printf.sprintf
        {|Pair 0x%s (Pair "%s" %s)|}
        ticketer
        "Hello, Ticket!"
        (Int.to_string d)
    in
    let outbox_transaction param =
      Sc_rollup_client.
        {
          destination;
          entrypoint = Some "receive_tickets";
          parameters = parameters param;
          parameters_ty = None;
        }
    in
    Sc_rollup_client.outbox_proof_batch
      sc_rollup_client
      ~message_index
      ~outbox_level
      (List.map outbox_transaction [220; 100; 40])
  in
  let* () =
    match answer with
    | Some {commitment_hash; proof} ->
        let* () = Client.bake_for_and_wait client in
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
  unit

let register_test ?(regression = false) ~__FILE__ ~tags ~title f =
  let tags = "tx_sc_rollup" :: tags in
  if regression then Protocol.register_regression_test ~__FILE__ ~title ~tags f
  else Protocol.register_test ~__FILE__ ~title ~tags f

let test_tx_kernel_e2e =
  register_test
    ~regression:true
    ~__FILE__
    ~tags:["wasm"; "kernel"; "wasm_2_0_0"; "kernel_e2e"]
    ~title:(Printf.sprintf "wasm_2_0_0 - tx kernel should run e2e (kernel_e2e)")
    test_tx_kernel_e2e

let register ~protocols = test_tx_kernel_e2e protocols
