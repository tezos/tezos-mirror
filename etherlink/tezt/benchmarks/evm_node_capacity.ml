(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Test_helpers
open Setup
open Floodgate_lib

let ( let+? ) x f =
  match x with
  | Error e ->
      Format.kasprintf
        failwith
        "Error: %a"
        Tezos_base.TzPervasives.pp_print_top_error_of_trace
        e
  | Ok r -> f r

let ( let*? ) x f =
  let* x in
  let+? x in
  f x

let wait_for_application sequencer f =
  wait_for_application
    f
    ~time_between_blocks:0.5
    ~max_blocks:5
    ~produce_block:(fun _ -> produce_block sequencer)

let floodgate_accounts evm_node accounts =
  let evm_node_endpoint = Evm_node.endpoint evm_node |> Uri.of_string in
  let* accounts =
    Lwt_list.map_p
      (fun Eth_account.{private_key; _} ->
        let+? sk = Signer.secret_key_from_hex (`Hex private_key) in
        let signer = Signer.from_secret_key sk in
        let*? a = Account.from_signer ~evm_node_endpoint signer in
        return a)
      (Array.to_list accounts)
  in
  return (Array.of_list accounts)

let send_deploy ~sender infos evm_node =
  let rpc_endpoint = Evm_node.endpoint evm_node |> Uri.of_string in
  let*? simple_gas_limit =
    Network_info.get_gas_limit
      ~rpc_endpoint
      ~base_fee_per_gas:infos.Network_info.base_fee_per_gas
      ~from:(Account.address_et sender)
      ~to_:(Account.address_et sender)
      ()
  in
  let*? res =
    Floodgate.prepare_scenario
      ~rpc_endpoint
      ~scenario:`ERC20
      infos
      simple_gas_limit
      sender
  in
  match res with
  | `ERC20 addr, gas_limit -> return (addr, gas_limit)
  | _ -> assert false

let deploy_contracts evm_node infos sequencer accounts nb =
  let senders = Array.sub accounts 0 nb |> Array.to_list in
  let deploys () =
    Lwt_list.map_p (fun sender -> send_deploy ~sender infos evm_node) senders
  in
  wait_for_application sequencer deploys

let nb_refused = ref 0

let nb_dropped = ref 0

let nb_confirmed = ref 0

let call infos contract gas_limit sender ?nonce ~name abi params =
  let confirmed, waker = Lwt.task () in
  let data = Efunc_core.Evm.encode ~name abi params in
  let* () =
    Tx_queue.transfer
      ~gas_limit
      ~infos
      ~to_:(Efunc_core.Private.a contract)
      ~data
      ~value:Z.zero
      ~from:sender
      ?nonce
      ()
      ~callback:(function
      | `Accepted _ -> unit
      | (`Refused | `Dropped | `Confirmed) as status ->
          let c =
            match status with
            | `Refused -> nb_refused
            | `Dropped -> nb_dropped
            | `Confirmed -> nb_confirmed
          in
          incr c ;
          Lwt.wakeup waker status ;
          unit)
  in
  confirmed

let account_step infos gas_limit contract ~nonce (sender : Account.t) value
    (dest : Account.t) =
  let* _ =
    call
      infos
      contract
      gas_limit
      sender
      ~nonce
      ~name:"mint"
      [`uint 256]
      [`int (Z.of_int value)]
  and* _ =
    call
      infos
      contract
      gas_limit
      sender
      ~nonce:(Z.succ nonce)
      ~name:"transfer"
      [`address; `uint 256]
      [`address (Account.address dest); `int (Z.of_int value)]
  in
  unit

let sender_step infos gas_limit erc20s accounts iteration sender_index =
  let sender = accounts.(sender_index mod Array.length accounts) in
  let dest_index =
    (sender_index + (7 * (iteration + 3))) mod Array.length accounts
  in
  let dest = accounts.(dest_index) in
  let nonce = sender.nonce in
  Lwt_list.iteri_p
    (fun i contract ->
      let nonce = Z.add nonce (Z.of_int (i * 2)) in
      account_step infos gas_limit contract ~nonce sender iteration dest)
    erc20s

let step sequencer infos gas_limit erc20s accounts iteration =
  Log.report "Iteration %d" iteration ;
  let sender_indexes = List.init (Array.length accounts) Fun.id in
  let step_f () =
    Lwt_list.iter_p
      (sender_step infos gas_limit erc20s accounts iteration)
      sender_indexes
  in
  let* () = wait_for_application sequencer step_f in
  if !nb_dropped <> 0 then
    Log.info ~color:Log.Color.FG.red "%d operations DROPPED" !nb_dropped ;
  if !nb_refused <> 0 then
    Log.info ~color:Log.Color.FG.red "%d operations REFUSED" !nb_refused ;
  if !nb_confirmed <> 0 then Log.info "%d operations confirmed" !nb_confirmed ;
  nb_dropped := 0 ;
  nb_refused := 0 ;
  nb_confirmed := 0 ;
  unit

type gasometer = {mutable gas : Z.t; mutable time : Ptime.Span.t}

let empty_gasometer () = {gas = Z.zero; time = Ptime.Span.zero}

let capacity_mgas_sec {gas; time} =
  let s = Ptime.Span.to_float_s time in
  let mega_gas = Z.to_float gas /. 1_000_000. in
  mega_gas /. s

let pp_capacity fmt g = Format.fprintf fmt "%.3f MGas/s" (capacity_mgas_sec g)

let blueprint_application_event = "blueprint_application.v0"

let install_gasometer evm_node =
  let gasometer = empty_gasometer () in
  let () =
    Evm_node.on_event evm_node @@ fun {name; value; _} ->
    if name = blueprint_application_event then (
      let open JSON in
      let level = value |-> "level" |> as_string in
      let process_time =
        value |-> "process_time" |> as_float |> Ptime.Span.of_float_s
        |> Option.get
      in
      let execution_gas =
        value |-> "execution_gas" |> as_string |> Z.of_string
      in
      let ignored = execution_gas < Z.of_int 100_000 in
      let block_speed = {gas = execution_gas; time = process_time} in
      Log.info
        "Level %s: %a gas consumed in %a: %a"
        level
        Z.pp_print
        execution_gas
        Ptime.Span.pp
        process_time
        (fun fmt (ignored, speed) ->
          if ignored then Format.pp_print_string fmt "(ignored)"
          else pp_capacity fmt speed)
        (ignored, block_speed) ;
      if not ignored then (
        gasometer.gas <- Z.add gasometer.gas execution_gas ;
        gasometer.time <- Ptime.Span.add gasometer.time process_time ;
        let capacity = capacity_mgas_sec gasometer in
        let color =
          if capacity < 10. then Log.Color.FG.red else Log.Color.FG.green
        in
        Log.info ~color ~prefix:"Current capacity" "%a" pp_capacity gasometer))
  in
  gasometer

let monitor_gasometer evm_node f =
  let gasometer = install_gasometer evm_node in
  let* () = f () in
  let capacity = capacity_mgas_sec gasometer in
  let color =
    if capacity < 10. then Log.Color.BG.red
    else if capacity < 12. then Log.Color.BG.yellow
    else Log.Color.BG.green
  in
  Log.report
    ~color
    ~prefix:(Format.sprintf "Capacity of %s" (Evm_node.name evm_node))
    "%a"
    pp_capacity
    gasometer ;
  unit

let test_erc20_capacity =
  let nb_accounts = 100 in
  let nb_contracts = 1 in
  let iterations = 5 in
  let accounts = Eth_account.accounts nb_accounts in
  let eth_bootstrap_accounts =
    Array.to_list accounts |> List.map (fun a -> a.Eth_account.address)
  in
  register_all
    ~__FILE__
    ~tags:["erc20"; "benchmark"; "capacity"; "ci_disabled"]
    ~title:"Capacity of EVM node"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts
    ~websockets:true
    ~use_multichain:Register_without_feature
    ~use_dal:Register_without_feature
    ~da_fee:Wei.zero
    ~minimum_base_fee_per_gas:Wei.one
    ~tx_queue:{max_lifespan = 4; max_size = 4_000; tx_per_addr_limit = 1024}
  @@ fun {sequencer; evm_version = _; _} _protocol ->
  let* accounts = floodgate_accounts sequencer accounts in
  let endpoint = Evm_node.endpoint sequencer |> Uri.of_string in
  let*? infos =
    Network_info.fetch ~rpc_endpoint:endpoint ~base_fee_factor:100.
  in
  let*? () =
    Tx_queue.start
      ~relay_endpoint:endpoint
      ~max_transaction_batch_length:(Some 300)
      ~inclusion_timeout:2.
      ()
  in
  let follower =
    Floodgate.start_blueprint_follower
      ~relay_endpoint:endpoint
      ~rpc_endpoint:endpoint
  in
  let tx_queue = Tx_queue.beacon ~tick_interval:0.1 in
  Log.report "Deploying %d ERC20 contracts" nb_contracts ;
  let* erc20s =
    deploy_contracts sequencer infos sequencer accounts nb_contracts
  in
  let _, gas_limit = List.hd erc20s in
  let erc20s =
    List.map (fun ((c : Efunc_core.Private.address), _) -> (c :> string)) erc20s
  in
  Log.info "%d ERC20 contracts deployed" (List.length erc20s) ;
  List.iter (Log.debug "- %s") erc20s ;
  monitor_gasometer sequencer @@ fun () ->
  let* () =
    Lwt_list.iter_s
      (step sequencer infos gas_limit erc20s accounts)
      (List.init iterations succ)
  in
  Lwt.cancel follower ;
  Lwt.cancel tx_queue ;
  unit

let register () = test_erc20_capacity [Protocol.Alpha]
