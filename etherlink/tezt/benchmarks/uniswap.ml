(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Setup
open Etherlink_benchmark_lib
open Benchmark_utils
open Uniswap

let test_swaps () =
  let nb_accounts = Option.value parameters.accounts ~default:100 in
  let nb_tokens = Option.value parameters.contracts ~default:1 in
  let nb_hops = parameters.swap_hops in
  let accounts = Eth_account.accounts nb_accounts in
  let eth_bootstrap_accounts =
    Array.to_list accounts |> List.map (fun a -> a.Eth_account.address)
  in
  register_all
    ~__FILE__
    ~tags:["benchmark"; "evm"; "uniswap"; "ci_disabled"]
    ~title:"Benchmarking Uniswap transactions"
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
  let* env, shutdown =
    setup ~accounts ~nb_tokens ~nb_hops ~sequencer ~rpc_node:sequencer
  in
  let* _ =
    monitor_gasometer sequencer @@ fun () ->
    let* stop_profile =
      if parameters.profiling then profile sequencer
      else return (fun () -> unit)
    in
    let* () =
      Lwt_list.iter_s (step env) (List.init parameters.iterations succ)
    in
    let* () = shutdown () in
    stop_profile ()
  in
  unit

let register () = test_swaps () [Protocol.Alpha]
