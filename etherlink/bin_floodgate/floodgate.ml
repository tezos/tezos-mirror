(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let one_xtz = Z.(of_int 1_000_000_000 * of_int 1_000_000_000)

let xtz_of_int x = Z.(of_int x * one_xtz)

let controller_from_sk ~rpc_endpoint ~min_balance controller =
  let open Lwt_result_syntax in
  let* controller =
    Account.from_secret_key ~evm_node_endpoint:rpc_endpoint controller
  in
  let* (Qty controller_balance) =
    Batch.call
      (module Rpc_encodings.Get_balance)
      ~keep_alive:true
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
let send_transaction_and_wait ~infos from to_ value =
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
      ~infos
      from
      to_
      value
  in
  result

let rec spam_with_account ~infos account =
  let open Lwt_syntax in
  let start = ref (Time.System.now ()) in
  Tx_queue.transfer
    ~infos
    ~callback:(fun reason ->
      match reason with
      | `Accepted _ ->
          start := Time.System.now () ;
          return_unit
      | `Confirmed ->
          let end_ = Time.System.now () in
          let* () =
            Floodgate_events.transaction_confirmed
              account
              Ptime.(diff end_ !start)
          in
          spam_with_account ~infos account
      | `Refused ->
          let* () = Floodgate_events.transaction_refused account in
          spam_with_account ~infos account
      | `Dropped ->
          let* () = Floodgate_events.transaction_dropped account in
          spam_with_account ~infos account)
    account
    (Account.address account)
    Z.zero

let fund_fresh_account ~infos ~relay_endpoint ~initial_balance controller =
  let open Lwt_result_syntax in
  let node = Account.fresh () in
  let* () =
    send_transaction_and_wait
      ~infos
      controller
      (Account.address node)
      initial_balance
  in
  Account.credit node initial_balance ;
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        let open Lwt_syntax in
        let fees = Z.(infos.Network_info.gas_limit * infos.base_fee_per_gas) in
        (* There is a race condition, here. We can be in a situation where
           a transaction was successfully injected, but we have interrupted the
           program before we can call the callback with [`Accepted].

           To handle this case, we do something _pragmatic_. We will send two
           transactions: the one based on our state, and the one that we should
           inject _if_ we were in the race condition case.

           The claim is: at least one of these transactions will be accepted
           and included. *)
        let txn =
          Craft.transfer
            ~infos
            ~from:node
            ~to_:(Account.address controller)
            Z.(node.balance - fees)
        and txn' =
          Craft.transfer
            ~nonce:Z.(succ node.nonce)
            ~infos
            ~from:node
            ~to_:(Account.address controller)
            Z.(node.balance - fees - fees)
        in
        let* _ = Tx_queue.Misc.send_raw_transaction ~relay_endpoint txn in
        let* _ = Tx_queue.Misc.send_raw_transaction ~relay_endpoint txn' in
        let*! () = Floodgate_events.reimbursed_controller node in
        return_unit)
  in
  return node

let run ~relay_endpoint ~rpc_endpoint ~controller ~max_active_eoa
    ~spawn_interval ~tick_interval ~base_fee_factor ~initial_balance =
  let open Lwt_result_syntax in
  let* controller =
    controller_from_sk ~rpc_endpoint ~min_balance:(xtz_of_int 100) controller
  in
  let* infos = Network_info.fetch ~rpc_endpoint ~base_fee_factor controller in
  let* () = Tx_queue.start ~relay_endpoint () in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks ~evm_node_endpoint:rpc_endpoint ()
  in
  let* next_blueprint_number =
    Batch.call
      (module Rpc_encodings.Block_number)
      ~keep_alive:true
      ~evm_node_endpoint:relay_endpoint
      ()
  in
  let*! () = Floodgate_events.is_ready infos.chain_id infos.base_fee_per_gas in
  let* () =
    Blueprints_follower.start
      ~ping_tx_pool:false
      ~time_between_blocks
      ~evm_node_endpoint:relay_endpoint
      ~next_blueprint_number
      (fun number blueprint ->
        let*! () = Floodgate_events.received_blueprint number in
        let* () =
          match Blueprint_decoder.transaction_hashes blueprint with
          | Ok hashes -> List.iter_es Tx_queue.confirm hashes
          | Error _ -> return_unit
        in
        return `Continue)
  and* () = Tx_queue.beacon ~tick_interval
  and* () =
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
                controller
            in
            let*! () =
              Floodgate_events.spam_started (Account.address_et node)
            in
            Lwt_result.ok (spam_with_account ~infos node)
          in
          return_unit)
        (Seq.ints 0 |> Stdlib.Seq.take max_active_eoa)
    in
    Lwt_result.ok (Floodgate_events.setup_completed ())
  in
  return_unit
