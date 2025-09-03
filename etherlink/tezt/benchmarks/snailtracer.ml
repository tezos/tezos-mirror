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
  accounts : Floodgate_lib.Account.t Array.t;
  contract : string;
  spp : int;
}

let get_gas_limit endpoint infos sender contract spp =
  let data =
    Efunc_core.Evm.encode ~name:"Benchmark" [`uint 8] [`int (Z.of_int spp)]
  in
  let data = Ethereum_types.hash_of_string (data :> string) in
  Network_info.get_gas_limit
    ~rpc_endpoint:endpoint
    ~base_fee_per_gas:infos.Network_info.base_fee_per_gas
    ~from:(Account.address_et sender)
    ~to_:(Address (Hex contract))
    ~data
    ~value:Z.zero
    ()

let _precompute_get_gas_limits endpoint infos sender contract =
  Lwt_list.iter_s
    (fun spp ->
      let*? g = get_gas_limit endpoint infos sender contract spp in
      Log.info "| %d -> Z.of_int %a" spp Z.pp_print g ;
      unit)
    (List.init 32 succ)

(* Obtained with [_precompute_get_gas_limits] on kernel latest. *)
let hardcoded_gas_limit err = function
  | 1 -> Z.of_int 9612952
  | 2 -> Z.of_int 20522760
  | 3 -> Z.of_int 36413281
  | 4 -> Z.of_int 57284910
  | 5 -> Z.of_int 87573645
  | 6 -> Z.of_int 115038502
  | 7 -> Z.of_int 151753656
  | 8 -> Z.of_int 191241874
  | 9 -> Z.of_int 231969195
  | 10 -> Z.of_int 275320949
  | 11 -> Z.of_int 333171493
  | 12 -> Z.of_int 389815714
  | 13 -> Z.of_int 443214869
  | 14 -> Z.of_int 520323434
  | 15 -> Z.of_int 586281357
  | 16 -> Z.of_int 665390447
  | 17 -> Z.of_int 745989144
  | 18 -> Z.of_int 829926870
  | 19 -> Z.of_int 894124361
  | 20 -> Z.of_int 997030540
  | 21 -> Z.of_int 1093622015
  | 22 -> Z.of_int 1190893054
  | 23 -> Z.of_int 1304518621
  | 24 -> Z.of_int 1430731450
  | 25 -> Z.of_int 1544955527
  | 26 -> Z.of_int 1667582873
  | 27 -> Z.of_int 1802108041
  | 28 -> Z.of_int 1946302868
  | 29 -> Z.of_int 2094926130
  | 30 -> Z.of_int 2245609288
  | 31 -> Z.of_int 2358551098
  | 32 -> Z.of_int 2528073528
  | spp ->
      Test.fail
        "No known gas limit for spp = %d, and estimate gas fails with %a"
        spp
        Tezos_base.TzPervasives.pp_print_top_error_of_trace
        err

let get_gas_limit endpoint infos sender contract spp =
  let* res = get_gas_limit endpoint infos sender contract spp in
  match res with
  | Ok gas_limit -> return gas_limit
  | Error err -> return (hardcoded_gas_limit err spp)

let call_one {infos; gas_limit; contract; spp; _} sender =
  let* _ =
    call
      infos
      contract
      gas_limit
      sender
      ~name:"Benchmark"
      [`uint 8]
      [`int (Z.of_int spp)]
  in
  unit

let step ({sequencer; accounts; _} as env) iteration =
  Log.report "Iteration %d" iteration ;
  let step_f () = Lwt_list.iter_p (call_one env) (Array.to_list accounts) in
  wait_for_application sequencer step_f

let encode_parameters width height =
  let enc_int x =
    Efunc_core.Evm.encode_value (`int 256) (`int (Z.of_int x))
    |> Rope.to_string |> Hex.of_string |> Hex.show
  in
  enc_int width ^ enc_int height

let test_snailtracer =
  let nb_accounts = Option.value parameters.accounts ~default:1 in
  (match parameters.contracts with
  | Some n when n <> 1 ->
      Log.warn "Deploying only one contract, ignoring argument"
  | _ -> ()) ;
  let width = 64 in
  let height = 48 in
  let accounts = Eth_account.accounts nb_accounts in
  let eth_bootstrap_accounts =
    Array.to_list accounts |> List.map (fun a -> a.Eth_account.address)
  in
  let spp = parameters.spp in
  register_all
    ~__FILE__
    ~tags:["benchmark"; "evm"; "transfer"; "snailtracer"; "ci_disabled"]
    ~title:"Benchmarking pure EVM execution in node"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts
    ~websockets:true
    ~use_multichain:Register_without_feature
    ~use_dal:Register_without_feature
    ~da_fee:Wei.zero
    ~minimum_base_fee_per_gas:Wei.one
    ~maximum_gas_per_transaction:(1 lsl 50 |> Int64.of_int)
    ~tx_queue:{max_lifespan = 4; max_size = 4_000; tx_per_addr_limit = 1024}
  @@ fun {sequencer; _} _protocol ->
  let* accounts = floodgate_accounts sequencer accounts in
  let rpc_node = sequencer in
  let endpoint = Evm_node.endpoint rpc_node |> Uri.of_string in
  let*? infos =
    Network_info.fetch ~rpc_endpoint:endpoint ~base_fee_factor:1000.
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
  let tx_queue = Tx_queue.beacon ~tick_interval:0.5 in
  Log.report "Deploying SnailTracer contract" ;
  let bin = Base.read_file Solidity_contracts.snailtracer.bin in
  let bin = bin ^ encode_parameters width height in
  let* contract =
    deploy_contract ~rpc_node infos ~sequencer accounts.(0) (`Custom bin)
  in
  let* gas_limit = get_gas_limit endpoint infos accounts.(0) contract spp in
  Log.info "SnailTracer contract deployed at %s" contract ;
  Log.info "Will use gas limit %a" Z.pp_print gas_limit ;
  let env = {sequencer; rpc_node; infos; gas_limit; accounts; contract; spp} in
  monitor_gasometer sequencer @@ fun () ->
  let* stop_profile =
    if parameters.profiling then profile sequencer else return (fun () -> unit)
  in
  let* () = Lwt_list.iter_s (step env) (List.init parameters.iterations succ) in
  Lwt.cancel follower ;
  Lwt.cancel tx_queue ;
  let* () = Evm_node.terminate sequencer in
  stop_profile ()

let register () = test_snailtracer [Protocol.Alpha]
