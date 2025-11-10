(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Evm_node_lib_dev_encoding
open Evm_node_lib_dev
open Setup
open Etherlink_benchmark_lib
open Benchmark_utils
open Floodgate_lib

type env = {
  sequencer : Evm_node.t;
  rpc_node : Evm_node.t;
  infos : Network_info.t;
  gas_limit : Z.t;
  accounts : Floodgate_lib.Account.t Array.t;
  contract : string;
  width : int;
  height : int;
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

let ray_trace_scanline {infos; contract; spp; rpc_node; height; _} sender
    callback y =
  let data =
    Efunc_core.Evm.encode
      ~name:"TraceScanline"
      [`int 256; `int 256]
      [`int (Z.of_int (height - 1 - y)); `int (Z.of_int spp)]
  in
  let data = Ethereum_types.hash_of_string (data :> string) in
  let*? (Hash (Hex res)) =
    Batch.call
      (module Rpc_encodings.Eth_call)
      ~evm_node_endpoint:(Evm_node.endpoint rpc_node |> Uri.of_string)
      ~keep_alive:true
      ~timeout:10.
      ( {
          from = Some (Account.address_et sender);
          to_ = Some (Ethereum_types.Address.of_string contract);
          gas = None;
          gasPrice = Some (Qty (Z.mul infos.base_fee_per_gas (Z.of_int 1000)));
          value = Some (Qty Z.zero);
          data = Some data;
        },
        Block_parameter Latest,
        Ethereum_types.AddressMap.empty )
  in
  let line = String.sub res 128 (String.length res - 128) in
  let bytes = Hex.to_bytes (`Hex line) in
  let* () = callback bytes in
  return bytes

let ray_trace_scanlines ({width; height; _} as env) sender =
  Log.report "Raytracing with eth_call" ;
  let lines = List.init height Fun.id in
  let f = Temp.file "image.ppm" in
  let* chan = Lwt_io.open_file ~mode:Output f in
  let* () =
    (* Write header for PPM image *)
    Lwt_io.write chan (Format.sprintf "P6\n%d %d\n255\n" width height)
  in
  let start = Ptime_clock.now () in
  let min_time = ref (Ptime.Span.of_d_ps (max_int, 0L) |> Option.get) in
  let received_lines = ref 0 in
  let* lines =
    Lwt_list.map_p
      (fun y ->
        let start = Ptime_clock.now () in
        let* line =
          ray_trace_scanline
            env
            sender
            (fun _ ->
              incr received_lines ;
              Log.info
                "Received line %d. Completed at %.1f%%"
                y
                (float_of_int !received_lines *. 100. /. float_of_int height) ;
              unit)
            y
        in
        let end_ = Ptime_clock.now () in
        let time = Ptime.diff end_ start in
        min_time := min time !min_time ;
        return line)
      lines
  in
  let end_ = Ptime_clock.now () in
  let* () =
    Lwt_list.iter_s
      (fun b -> Lwt_io.write chan (Bytes.unsafe_to_string b))
      lines
  in
  let* () = Lwt_io.close chan in
  let wall_time = Ptime.diff end_ start in
  let speedup =
    float_of_int height
    *. Ptime.Span.to_float_s !min_time
    /. Ptime.Span.to_float_s wall_time
  in
  Log.report
    ~color:Log.Color.bold
    "Ray traced in %a. Speed up = %.1f"
    Ptime.Span.pp
    wall_time
    speedup ;
  unit

let call_one {infos; rpc_node; gas_limit; contract; spp; _} sender =
  let* _ =
    call
      infos
      rpc_node
      contract
      ~gas_limit
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

let test_snailtracer () =
  let nb_accounts = Option.value parameters.accounts ~default:1 in
  let width = parameters.width in
  let height = parameters.height in
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
  (match parameters.contracts with
  | Some n when n <> 1 ->
      Log.warn "Deploying only one contract, ignoring argument"
  | _ -> ()) ;
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
  let bin = Contracts.Snailtracer.bin () in
  let bin = bin ^ encode_parameters width height in
  let* contract =
    deploy_contract ~rpc_node infos ~sequencer accounts.(0) (`Custom bin)
  in
  let* gas_limit = get_gas_limit endpoint infos accounts.(0) contract spp in
  Log.info "SnailTracer contract deployed at %s" contract ;
  Log.info "Will use gas limit %a" Z.pp_print gas_limit ;
  let env =
    {
      sequencer;
      rpc_node;
      infos;
      gas_limit;
      accounts;
      contract;
      width;
      height;
      spp;
    }
  in
  monitor_gasometer sequencer @@ fun () ->
  let* stop_profile =
    if parameters.profiling then profile sequencer else return (fun () -> unit)
  in
  let* () = Lwt_list.iter_s (step env) (List.init parameters.iterations succ) in
  Lwt.cancel follower ;
  Lwt.cancel tx_queue ;
  let* () = Tx_queue.shutdown () in
  let* () = Evm_node.terminate sequencer in
  stop_profile ()

let test_full_image_raytracing () =
  let width = parameters.width in
  let height = parameters.height in
  let accounts = Eth_account.accounts 1 in
  let sender = accounts.(0) in
  let spp = parameters.spp in
  register_all
    ~__FILE__
    ~tags:
      [
        "benchmark";
        "evm";
        "pure_execution";
        "snailtracer";
        "eth_call";
        "image";
        "ci_disabled";
      ]
    ~title:"Benchmarking pure EVM execution in eth_call RPCs"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:[sender.address]
    ~websockets:true
    ~use_multichain:Register_without_feature
    ~use_dal:Register_without_feature
    ~da_fee:Wei.zero
    ~minimum_base_fee_per_gas:Wei.one
    ~maximum_gas_per_transaction:(1 lsl 50 |> Int64.of_int)
    ~tx_queue:{max_lifespan = 4; max_size = 4_000; tx_per_addr_limit = 1024}
  @@ fun {sequencer; _} _protocol ->
  (match parameters.contracts with
  | Some n when n <> 1 ->
      Log.warn "Deploying only one contract, ignoring argument"
  | _ -> ()) ;
  let* sender = floodgate_account sequencer sender in
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
  let bin = Contracts.Snailtracer.bin () in
  let bin = bin ^ encode_parameters width height in
  let* contract =
    deploy_contract ~rpc_node infos ~sequencer sender (`Custom bin)
  in
  Lwt.cancel follower ;
  Lwt.cancel tx_queue ;
  let* () = Tx_queue.shutdown () in
  let env =
    {
      sequencer;
      rpc_node;
      infos;
      gas_limit = Z.zero;
      accounts = [|sender|];
      contract;
      spp;
      width;
      height;
    }
  in
  let* stop_profile =
    if parameters.profiling then profile sequencer else return (fun () -> unit)
  in
  let* () = ray_trace_scanlines env sender in
  let* () = Evm_node.terminate sequencer in
  stop_profile ()

let register () =
  test_snailtracer () [Protocol.Alpha] ;
  test_full_image_raytracing () [Protocol.Alpha] ;
  ()
