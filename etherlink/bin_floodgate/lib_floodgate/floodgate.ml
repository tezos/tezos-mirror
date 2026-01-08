(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type attempt = Always | Never | Number of int

let one_xtz = Z.(of_int 1_000_000_000 * of_int 1_000_000_000)

let xtz_of_int x = Z.(of_int x * one_xtz)

let controller_from_signer ~rpc_endpoint ~min_balance controller =
  let open Lwt_result_syntax in
  let* controller =
    Account.from_signer ~evm_node_endpoint:rpc_endpoint controller
  in
  let* (Qty controller_balance) =
    Batch.call
      (module Rpc_encodings.Get_balance)
      ~keep_alive:true
      ~timeout:Network_info.timeout
      ~evm_node_endpoint:rpc_endpoint
      (Account.address_et controller, Block_parameter Latest)
  in
  let* () =
    when_ Compare.Z.(controller_balance < min_balance) @@ fun () ->
    failwith "Controller %a does not have enough fund" Account.pp_pkh controller
  in
  return controller

(** [send_transaction_and_wait ~infos from to_ value] injects, through the Tx
    queue, a transaction from [from] to [to_] of [value] native tokens, and
    wait for it to be confirmed. It will return an error if the transactions
    is not confirmed, either because it was dropped or refused. *)
let send_transaction_and_wait ~infos ~gas_limit ~from ~to_ ~value =
  let open Lwt_syntax in
  let result, waker = Lwt.wait () in
  let* () =
    Tx_queue.transfer
      ~callback:(function
        | `Accepted _ -> return_unit
        | `Confirmed ->
            Lwt.wakeup waker (Ok ()) ;
            Lwt.return_unit
        | `Refused ->
            Lwt.wakeup waker (Error [error_of_fmt "Transaction was refused"]) ;
            Lwt.return_unit
        | `Dropped ->
            Lwt.wakeup waker (Error [error_of_fmt "Transaction was dropped"]) ;
            Lwt.return_unit)
      ~to_
      ~value
      ~infos
      ~from
      ~gas_limit
      ()
  in
  result

module State = struct
  let transactions_count = ref 0

  let dummy_data_size = ref None

  let incr_transactions_count n = transactions_count := !transactions_count + n

  let rec report ~elapsed_time =
    let open Lwt_syntax in
    let start = Time.System.now () in
    transactions_count := 0 ;
    let* () = Lwt_unix.sleep elapsed_time in
    let stop = Time.System.now () in
    let transactions = !transactions_count in
    let diff = Ptime.diff stop start in
    let* () = Floodgate_events.measured_tps transactions diff in
    let* () =
      match !dummy_data_size with
      | None -> Lwt.return_unit
      | Some dummy_data_size ->
          let dummy_data_size_sent = transactions * dummy_data_size in
          let dummy_data_size_sent_kb = dummy_data_size_sent / 1000 in
          Floodgate_events.measured_dps dummy_data_size_sent_kb diff
    in
    report ~elapsed_time
end

let spam_with_account ~txs_per_salvo ~token ~infos ~gas_limit account
    ~retry_attempt =
  let data, to_ =
    match token with
    | `Native data ->
        (Option.map Efunc_core.Private.b data, Account.address account)
    | `ERC20 contract ->
        let data =
          Efunc_core.Evm.encode
            ~name:"transfer"
            [`address; `uint 256]
            [`address (Account.address account); `int Z.zero]
        in
        (Some data, Efunc_core.Private.a contract)
  in
  let rec salvo ~start ~nonce_limit ~nonce =
    let is_last_nonce = Compare.Z.(Z.succ nonce = nonce_limit) in
    let open Lwt_syntax in
    let rec retry_transfer attempt =
      let maybe_retry () =
        match retry_attempt with
        | Always -> retry_transfer (attempt + 1)
        | Never -> return_unit
        | Number max_attempt when attempt >= max_attempt ->
            let* () =
              Floodgate_events.transaction_retried_failed account attempt
            in
            return_unit
        | Number _ -> retry_transfer (attempt + 1)
      in
      let callback reason =
        match reason with
        | `Accepted _ -> return_unit
        | `Refused ->
            let* () = Floodgate_events.transaction_refused account in
            maybe_retry ()
        | `Confirmed ->
            let end_ = Time.System.now () in
            let* () =
              Floodgate_events.transaction_confirmed
                account
                Ptime.(diff end_ start)
            in
            let* () =
              if attempt > 0 then
                Floodgate_events.transaction_retried_confirmed
                  account
                  attempt
                  Ptime.(diff end_ start)
              else return_unit
            in
            if is_last_nonce then loop () else return_unit
        | `Dropped ->
            let* () = Floodgate_events.transaction_dropped account in
            maybe_retry ()
      in
      Tx_queue.transfer
        ~nonce
        ~infos
        ~callback
        ~gas_limit
        ~from:account
        ~to_
        ?data
        ()
    in
    let* () = retry_transfer 0 in
    if not is_last_nonce then salvo ~start ~nonce_limit ~nonce:(Z.succ nonce)
    else return_unit
  and loop () =
    let start = Time.System.now () in
    let nonce_limit = Z.(of_int txs_per_salvo + account.nonce) in
    salvo ~start ~nonce_limit ~nonce:account.nonce
  in
  loop ()

let rec get_transaction_receipt rpc_endpoint txn_hash =
  let open Lwt_result_syntax in
  let* receipt_opt =
    Batch.call
      (module Rpc_encodings.Get_transaction_receipt)
      ~keep_alive:true
      ~timeout:Network_info.timeout
      ~evm_node_endpoint:rpc_endpoint
      txn_hash
  in
  match receipt_opt with
  | Some receipt -> return receipt
  | None ->
      let*! () = Lwt_unix.sleep 0.1 in
      get_transaction_receipt rpc_endpoint txn_hash

let wait_for_receipt ~rpc_endpoint ?to_ ?value ?data ?nonce ~gas_limit ~infos
    ~from () =
  let open Lwt_syntax in
  let res, waiter = Lwt.task () in
  let txn_hash = ref Ethereum_types.(Hash (Hex "")) in
  let callback = function
    | `Refused ->
        Lwt.wakeup waiter
        @@ Result_syntax.tzfail
             (error_of_fmt
                "Could not get the transaction receipt: transaction was \
                 refused]") ;
        return_unit
    | `Dropped ->
        Lwt.wakeup waiter
        @@ Result_syntax.tzfail
             (error_of_fmt
                "Could not get the transaction receipt: transaction was \
                 dropped]") ;
        return_unit
    | `Accepted hash ->
        txn_hash := hash ;
        return_unit
    | `Confirmed ->
        let* result = get_transaction_receipt rpc_endpoint !txn_hash in
        Lwt.wakeup waiter result ;
        return_unit
  in
  let* () =
    Tx_queue.transfer
      ~callback
      ?to_
      ?value
      ?data
      ?nonce
      ~gas_limit
      ~infos
      ~from
      ()
  in
  res

let fund_fresh_account ~infos ~relay_endpoint ~initial_balance ~gas_limit
    controller =
  let open Lwt_result_syntax in
  let node = Account.fresh () in
  let* () =
    send_transaction_and_wait
      ~infos
      ~gas_limit
      ~from:controller
      ~to_:(Account.address node)
      ~value:initial_balance
  in
  Account.credit node initial_balance ;
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        let open Lwt_syntax in
        let fees = Z.(gas_limit * infos.base_fee_per_gas) in
        (* There is a race condition, here. We can be in a situation where
           a transaction was successfully injected, but we have interrupted the
           program before we can call the callback with [`Accepted].

           To handle this case, we do something _pragmatic_. We will send two
           transactions: the one based on our state, and the one that we should
           inject _if_ we were in the race condition case.

           The claim is: at least one of these transactions will be accepted
           and included. *)
        let* txn =
          Craft.transfer_exn
            ~infos
            ~from:node
            ~gas_limit
            ~to_:(Account.address controller)
            ~value:Z.(node.balance - fees)
            ()
        and* txn' =
          Craft.transfer_exn
            ~nonce:Z.(succ node.nonce)
            ~infos
            ~from:node
            ~gas_limit
            ~to_:(Account.address controller)
            ~value:Z.(node.balance - fees - fees)
            ()
        in
        let* _ = Tx_queue.Misc.send_raw_transaction ~relay_endpoint txn in
        let* _ = Tx_queue.Misc.send_raw_transaction ~relay_endpoint txn' in
        let*! () = Floodgate_events.reimbursed_controller node in
        return_unit)
  in
  return node

(* Classic deploy bytecode ERC20 contract.
   You can find an example here : https://testnet.explorer.etherlink.com/address/0xB1Ea698633d57705e93b0E40c1077d46CD6A51d8
   You can also find a list of ERC20 addresses here : https://docs.etherlink.com/building-on-etherlink/tokens/ *)
let deploy_erc20_bin =
  "606060405260408051908101604052600b81527f577261707065642058545a0000000000000000000000000000000000000000006020820152600090805161004b9291602001906100b1565b5060408051908101604052600481527f5758545a00000000000000000000000000000000000000000000000000000000602082015260019080516100939291602001906100b1565b506002805460ff1916601217905534156100ac57600080fd5b61014c565b828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f106100f257805160ff191683800117855561011f565b8280016001018555821561011f579182015b8281111561011f578251825591602001919060010190610104565b5061012b92915061012f565b5090565b61014991905b8082111561012b5760008155600101610135565b90565b6106c28061015b6000396000f3006060604052600436106100ae5763ffffffff7c010000000000000000000000000000000000000000000000000000000060003504166306fdde0381146100b8578063095ea7b31461014257806318160ddd1461017857806323b872dd1461019d5780632e1a7d4d146101c5578063313ce567146101db57806370a082311461020457806395d89b4114610223578063a9059cbb14610236578063d0e30db0146100ae578063dd62ed3e14610258575b6100b661027d565b005b34156100c357600080fd5b6100cb6102d3565b60405160208082528190810183818151815260200191508051906020019080838360005b838110156101075780820151838201526020016100ef565b50505050905090810190601f1680156101345780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b341561014d57600080fd5b610164600160a060020a0360043516602435610371565b604051901515815260200160405180910390f35b341561018357600080fd5b61018b6103dd565b60405190815260200160405180910390f35b34156101a857600080fd5b610164600160a060020a03600435811690602435166044356103eb565b34156101d057600080fd5b6100b6600435610531565b34156101e657600080fd5b6101ee6105df565b60405160ff909116815260200160405180910390f35b341561020f57600080fd5b61018b600160a060020a03600435166105e8565b341561022e57600080fd5b6100cb6105fa565b341561024157600080fd5b610164600160a060020a0360043516602435610665565b341561026357600080fd5b61018b600160a060020a0360043581169060243516610679565b600160a060020a033316600081815260036020526040908190208054349081019091557fe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c915190815260200160405180910390a2565b60008054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156103695780601f1061033e57610100808354040283529160200191610369565b820191906000526020600020905b81548152906001019060200180831161034c57829003601f168201915b505050505081565b600160a060020a03338116600081815260046020908152604080832094871680845294909152808220859055909291907f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b9259085905190815260200160405180910390a350600192915050565b600160a060020a0330163190565b600160a060020a0383166000908152600360205260408120548290101561041157600080fd5b33600160a060020a031684600160a060020a03161415801561045b5750600160a060020a038085166000908152600460209081526040808320339094168352929052205460001914155b156104c257600160a060020a03808516600090815260046020908152604080832033909416835292905220548290101561049457600080fd5b600160a060020a03808516600090815260046020908152604080832033909416835292905220805483900390555b600160a060020a038085166000818152600360205260408082208054879003905592861680825290839020805486019055917fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef9085905190815260200160405180910390a35060019392505050565b600160a060020a0333166000908152600360205260409020548190101561055757600080fd5b600160a060020a033316600081815260036020526040908190208054849003905582156108fc0290839051600060405180830381858888f19350505050151561059f57600080fd5b33600160a060020a03167f7fcf532c15f0a6db0bd6d0e038bea71d30d808c7d98cb3bf7268a95bf5081b658260405190815260200160405180910390a250565b60025460ff1681565b60036020526000908152604090205481565b60018054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156103695780601f1061033e57610100808354040283529160200191610369565b60006106723384846103eb565b9392505050565b6004602090815260009283526040808420909152908252902054815600a165627a7a72305820f1bd8063f17f72e77d475b2e912fc19810f4991d4da614f6b8545734890456750029"

let deploy ?nonce ?gas_limit ~rpc_endpoint ~scenario infos controller =
  let open Lwt_result_syntax in
  let bin =
    match scenario with `ERC20 -> deploy_erc20_bin | `Custom bin -> bin
  in
  let deploy_data = Efunc_core.Types.b bin in
  let* gas_limit =
    match gas_limit with
    | Some g -> return g
    | None ->
        Network_info.get_gas_limit
          ~rpc_endpoint
          ~base_fee_per_gas:infos.Network_info.base_fee_per_gas
          ~from:(Account.address_et controller)
          ~data:(Ethereum_types.hash_of_string (deploy_data :> string))
          ~value:Z.zero
          ()
  in
  let* receipt =
    wait_for_receipt
      ?nonce
      ~rpc_endpoint
      ~infos
      ~from:controller
      ~data:deploy_data
      ~gas_limit
      ~value:Z.zero
      ()
  in
  let* contract =
    match receipt.contractAddress with
    | Some (Address (Hex address)) -> return address
    | None ->
        failwith
          "Transaction receipt does not contain contract address: %s"
          Data_encoding.Json.(
            to_string (construct Transaction_receipt.encoding receipt))
  in
  let*! () = Floodgate_events.deploy_erc20 contract in
  return contract

let prepare_scenario ~rpc_endpoint ~scenario infos ~dummy_data_size controller =
  let open Lwt_result_syntax in
  match scenario with
  | `XTZ ->
      let data =
        Option.map
          (fun size ->
            let dummy_bytes = Bytes.create size in
            Hex.show (Hex.of_bytes dummy_bytes))
          dummy_data_size
      in
      let* gas_limit =
        Network_info.get_gas_limit
          ~rpc_endpoint
          ~base_fee_per_gas:infos.Network_info.base_fee_per_gas
          ~to_:(Account.address_et controller)
          ?data:(Option.map Ethereum_types.hash_of_string data)
          ()
      in
      return (`Native data, gas_limit)
  | `ERC20 ->
      let* contract = deploy ~rpc_endpoint ~scenario:`ERC20 infos controller in
      let data =
        Efunc_core.Evm.encode
          ~name:"transfer"
          [`address; `uint 256]
          [`address (Account.address controller); `int Z.zero]
      in
      let data = Ethereum_types.hash_of_string (data :> string) in
      let* gas_limit =
        Network_info.get_gas_limit
          ~rpc_endpoint
          ~base_fee_per_gas:infos.base_fee_per_gas
          ~from:(Account.address_et controller)
          ~to_:(Address (Hex contract))
          ~data
          ~value:Z.zero
          ()
      in
      return (`ERC20 contract, gas_limit)

let lwt_stream_iter_es f stream =
  let open Lwt_result_syntax in
  let rec loop () =
    let*! elt = Lwt_stream.get stream in
    match elt with
    | None -> return_unit
    | Some elt ->
        let* () = f elt in
        loop ()
  in
  loop ()

let start_new_head_monitor ~ws_uri =
  let open Lwt_result_syntax in
  let ws_client =
    Websocket_client.create
      ~monitoring:{ping_timeout = 60.; ping_interval = 10.}
      ~keep_alive:false
      Media_type.json
      ws_uri
  in
  let* () = Websocket_client.connect ws_client in
  let* heads_subscription = Websocket_client.subscribe_newHeads ws_client in
  lwt_stream_iter_es
    (fun head ->
      let*? block = head in
      let*! () =
        Floodgate_events.received_blueprint block.Ethereum_types.number
      in
      match block.Ethereum_types.transactions with
      | TxHash hashes ->
          State.incr_transactions_count (List.length hashes) ;
          List.iter_es Tx_queue.confirm hashes
      | TxFull _ -> return_unit)
    heads_subscription.stream

let start_blueprint_follower ~relay_endpoint ~rpc_endpoint =
  let open Lwt_result_syntax in
  let* next_blueprint_number =
    Batch.call
      (module Rpc_encodings.Block_number)
      ~keep_alive:true
      ~timeout:Network_info.timeout
      ~evm_node_endpoint:relay_endpoint
      ()
  in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint:rpc_endpoint
      ~timeout:Network_info.timeout
      ()
  in
  Blueprints_follower.start
    ~multichain:false
    ~time_between_blocks
    ~evm_node_endpoint:relay_endpoint
    ~rpc_timeout:Network_info.timeout
    ~next_blueprint_number
    ~on_new_blueprint:(fun number blueprint ->
      let*! () = Floodgate_events.received_blueprint number in
      let* () =
        match Blueprint_decoder.transaction_hashes blueprint with
        | Ok hashes ->
            State.incr_transactions_count (List.length hashes) ;
            List.iter_es Tx_queue.confirm hashes
        | Error _ -> return_unit
      in
      return (`Continue Blueprints_follower.{sbl_callbacks_activated = false}))
    ~on_finalized_levels:(fun ~l1_level:_ ~start_l2_level:_ ~end_l2_level:_ ->
      return_unit)
    ~on_next_block_info:(fun _ _ -> return_unit)
    ~on_inclusion:(fun _ _ -> return_unit)
    ~on_dropped:(fun _ _ -> return_unit)
    ()

let run ~(scenario : [< `ERC20 | `XTZ]) ~relay_endpoint ~rpc_endpoint
    ~ws_endpoint ~controller ~max_active_eoa ~max_transaction_batch_length
    ~spawn_interval ~tick_interval ~base_fee_factor ~initial_balance
    ~txs_per_salvo ~elapsed_time_between_report ~dummy_data_size ~retry_attempt
    =
  State.dummy_data_size := dummy_data_size ;
  let open Lwt_result_syntax in
  let* controller =
    controller_from_signer
      ~rpc_endpoint
      ~min_balance:(xtz_of_int 100)
      controller
  in
  let* infos = Network_info.fetch ~rpc_endpoint ~base_fee_factor in
  let* () = Tx_queue.start ~relay_endpoint ~max_transaction_batch_length () in
  let*! () = Floodgate_events.is_ready infos.chain_id infos.base_fee_per_gas in
  let* () =
    match ws_endpoint with
    | Some ws_uri -> start_new_head_monitor ~ws_uri
    | None -> start_blueprint_follower ~relay_endpoint ~rpc_endpoint
  and* () = Tx_queue.beacon ~tick_interval
  and* () =
    let* token, gas_limit =
      prepare_scenario ~rpc_endpoint ~scenario ~dummy_data_size infos controller
    in
    let* simple_gas_limit =
      Network_info.get_gas_limit
        ~rpc_endpoint
        ~base_fee_per_gas:infos.Network_info.base_fee_per_gas
        ~to_:(Account.address_et controller)
        ()
    in
    let* () =
      Seq.ES.iter
        (fun _ ->
          let* () = Lwt_result.ok (Lwt_unix.sleep spawn_interval)
          and* () =
            let* node =
              fund_fresh_account
                ~infos
                ~relay_endpoint
                ~initial_balance
                ~gas_limit:simple_gas_limit
                controller
            in
            let*! () =
              Floodgate_events.spam_started (Account.address_et node)
            in
            Lwt_result.ok
              (spam_with_account
                 ~txs_per_salvo
                 ~token
                 ~infos
                 ~gas_limit
                 node
                 ~retry_attempt)
          in
          return_unit)
        (Seq.ints 0 |> Stdlib.Seq.take max_active_eoa)
    in
    Lwt_result.ok (Floodgate_events.setup_completed ())
  and* () = State.report ~elapsed_time:elapsed_time_between_report in
  return_unit
