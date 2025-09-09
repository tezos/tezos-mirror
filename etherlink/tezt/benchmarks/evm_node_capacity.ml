(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Setup
open Benchmark_utils
open Floodgate_lib
open Evm_node_lib_dev_encoding

type env = {
  sequencer : Evm_node.t;
  rpc_node : Evm_node.t;
  infos : Network_info.t;
  gas_limit : Z.t;
  accounts : Floodgate_lib.Account.t array;
  nb_contracts : int;
}

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

let sender_step {infos; gas_limit; accounts; _} erc20s iteration sender_index =
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

let step ({sequencer; accounts; _} as env) erc20s iteration =
  Log.report "Iteration %d" iteration ;
  let sender_indexes = List.init (Array.length accounts) Fun.id in
  let step_f () =
    Lwt_list.iter_p (sender_step env erc20s iteration) sender_indexes
  in
  wait_for_application sequencer step_f

let test_erc20_capacity =
  let nb_accounts = Option.value parameters.accounts ~default:100 in
  let nb_contracts = Option.value parameters.accounts ~default:5 in
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
  let env =
    {
      infos;
      sequencer;
      rpc_node = sequencer;
      gas_limit = Z.zero;
      accounts;
      nb_contracts;
    }
  in
  let*? () =
    Tx_queue.start
      ~relay_endpoint:endpoint
      ~max_transaction_batch_length:(Some 300)
      ~inclusion_timeout:parameters.timeout
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
    deploy_contracts
      ~rpc_node:env.rpc_node
      infos
      ~sequencer
      accounts
      `ERC20
      nb_contracts
  in
  let*? gas_limit =
    let contract = List.hd erc20s in
    let sender = accounts.(0) in
    let data =
      Efunc_core.Evm.encode
        ~name:"transfer"
        [`address; `uint 256]
        [`address (Account.address sender); `int Z.zero]
    in
    let data = Ethereum_types.hash_of_string (data :> string) in
    Network_info.get_gas_limit
      ~rpc_endpoint:endpoint
      ~base_fee_per_gas:infos.base_fee_per_gas
      ~from:(Account.address_et sender)
      ~to_:(Address (Hex contract))
      ~data
      ~value:Z.zero
      ()
  in
  let env = {env with gas_limit} in
  Log.info "%d ERC20 contracts deployed" (List.length erc20s) ;
  List.iter (Log.debug "- %s") erc20s ;
  monitor_gasometer sequencer @@ fun () ->
  let* stop_profile =
    if parameters.profiling then profile sequencer else return (fun () -> unit)
  in
  let* () =
    Lwt_list.iter_s (step env erc20s) (List.init parameters.iterations succ)
  in
  Lwt.cancel follower ;
  Lwt.cancel tx_queue ;
  let* () = Tx_queue.shutdown () in
  let* () = Evm_node.terminate sequencer in
  stop_profile ()

let register () = test_erc20_capacity [Protocol.Alpha]
