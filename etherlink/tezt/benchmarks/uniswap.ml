(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Evm_node_lib_dev_encoding
open Setup
open Benchmark_utils
open Floodgate_lib

type env = {
  sequencer : Evm_node.t;
  rpc_node : Evm_node.t;
  infos : Network_info.t;
  gas_limit : Z.t;
  accounts : Floodgate_lib.Account.t Array.t;
  wxtz_addr : string;
  gld_addr : string;
  gld2_addr : string;
  factory_addr : string;
  router_addr : string;
  multicall_addr : string;
  nb_hops : int;
}

let max_uint256 =
  let open Z in
  (one lsl 256) - one

let z1e18 = Z.of_float 1e18

let to_wei x = Z.(of_int x * z1e18)

let of_wei z = Z.to_float z /. 1e18

let encode_value v t =
  Efunc_core.Evm.encode_value v t |> Rope.to_string |> Hex.of_string |> Hex.show

let address a =
  Tezos_stdlib.TzString.remove_prefix ~prefix:"0x" a
  |> Option.value ~default:a |> Efunc_core.Private.a

let encode_address a = encode_value `address (`address (address a))

let bin_of_json_contract json =
  JSON.(json |-> "data" |-> "bytecode" |-> "object" |> as_string)

let deposit_wxtz_gas_limit {accounts; infos; rpc_node; wxtz_addr; _} =
  let sender = accounts.(0) in
  let rpc_endpoint = Evm_node.endpoint rpc_node |> Uri.of_string in
  let*? gas_limit =
    Network_info.get_gas_limit
      ~rpc_endpoint
      ~base_fee_per_gas:infos.base_fee_per_gas
      ~from:(Account.address_et sender)
      ~to_:(Address (Hex wxtz_addr))
      ~value:(to_wei 1000)
      ()
  in
  Log.debug "Deposit gas limit: %a@." Z.pp_print gas_limit ;
  return gas_limit

let deposit_wxtz ~gas_limit {infos; rpc_node; wxtz_addr; _} ?nonce value account
    =
  let* _ =
    call
      infos
      rpc_node
      wxtz_addr
      ~gas_limit
      account
      ?nonce
      ~value:(to_wei value)
      []
      []
  in
  unit

let deposits_wxtz env =
  Log.info "Depositing tokens to WXTZ contract" ;
  let* gas_limit = deposit_wxtz_gas_limit env in
  let f () =
    Lwt_list.iter_p
      (deposit_wxtz env ~gas_limit 1000)
      (Array.to_list env.accounts)
  in
  wait_for_application env.sequencer f

let get_total_supply {infos; rpc_node; accounts; _} contract =
  let* res =
    let open Evm_node_lib_dev in
    Batch.call
      (module Rpc_encodings.Eth_call)
      ~evm_node_endpoint:(Evm_node.endpoint rpc_node |> Uri.of_string)
      ~keep_alive:true
      ( {
          from = Some (Account.address_et accounts.(0));
          to_ = Some (Ethereum_types.Address.of_string contract);
          gas = None;
          gasPrice = Some (Qty infos.base_fee_per_gas);
          value = Some (Qty Z.zero);
          data =
            Some
              ((Efunc_core.Evm.encode ~name:"totalSupply" [] [] :> string)
              |> Ethereum_types.hash_of_string);
        },
        Block_parameter Latest,
        Ethereum_types.AddressMap.empty )
  in
  match res with
  | Error e ->
      Test.fail "supply error: %a" Tezos_base.TzPervasives.pp_print_trace e
  | Ok (Ethereum_types.Hash (Ethereum_types.Hex s)) ->
      Helpers.decode_z_be (`Hex s |> Hex.to_bytes) |> return

let get_balance {infos; rpc_node; _} contract sender =
  let* res =
    let open Evm_node_lib_dev in
    Batch.call
      (module Rpc_encodings.Eth_call)
      ~evm_node_endpoint:(Evm_node.endpoint rpc_node |> Uri.of_string)
      ~keep_alive:true
      ( {
          from = Some (Account.address_et sender);
          to_ = Some (Ethereum_types.Address.of_string contract);
          gas = None;
          gasPrice = Some (Qty infos.base_fee_per_gas);
          value = Some (Qty Z.zero);
          data =
            Some
              ((Efunc_core.Evm.encode
                  ~name:"balanceOf"
                  [`address]
                  [`address (Account.address sender)]
                 :> string)
              |> Ethereum_types.hash_of_string);
        },
        Block_parameter Latest,
        Ethereum_types.AddressMap.empty )
  in
  match res with
  | Error e ->
      Test.fail "supply error: %a" Tezos_base.TzPervasives.pp_print_trace e
  | Ok (Ethereum_types.Hash (Ethereum_types.Hex s)) ->
      Helpers.decode_z_be (`Hex s |> Hex.to_bytes) |> return

let z = Check.comparable Z.pp_print Z.compare

let check_wxtz_deposits env =
  let* supply = get_total_supply env env.wxtz_addr in
  Check.((supply = Z.(of_int (Array.length env.accounts) * to_wei 1000)) z)
    ~error_msg:(sf "Total supply for %s is %%L instead of %%R" env.wxtz_addr) ;
  Log.report "Deposited tokens to WXTZ contract" ;
  unit

let add_xtz_liquidity {sequencer; rpc_node; infos; router_addr; _} ~sender
    ~token_addr ~xtz ~token =
  let name = "addLiquidityETH" in
  let params_ty =
    [`address; `uint 256; `uint 256; `uint 256; `address; `uint 256]
  in
  let deadline = int_of_float (Unix.time ()) + 1000 in
  let to_ = Account.address sender in
  let params =
    [
      `address (address token_addr);
      `int (to_wei token);
      `int Z.one;
      `int Z.one;
      `address to_;
      `int (Z.of_int deadline);
    ]
  in
  wait_for_application sequencer @@ fun () ->
  let* _ =
    call
      infos
      rpc_node
      router_addr
      sender
      ~name
      params_ty
      params
      ~value:(to_wei xtz)
      ~check_success:true
  in
  unit

let add_liquidity
    {sequencer; rpc_node; infos; router_addr; gld_addr; gld2_addr; _} ~sender
    ~gld ~gld2 =
  let name = "addLiquidity" in
  let params_ty =
    [
      `address;
      `address;
      `uint 256;
      `uint 256;
      `uint 256;
      `uint 256;
      `address;
      `uint 256;
    ]
  in
  let deadline = int_of_float (Unix.time ()) + 1000 in
  let to_ = Account.address sender in
  let params =
    [
      `address (address gld_addr);
      `address (address gld2_addr);
      `int (to_wei gld);
      `int (to_wei gld2);
      `int Z.one;
      `int Z.one;
      `address to_;
      `int (Z.of_int deadline);
    ]
  in
  wait_for_application sequencer @@ fun () ->
  let* _ =
    call
      infos
      rpc_node
      router_addr
      sender
      ~name
      params_ty
      params
      ~check_success:true
  in
  unit

let approve_router {sequencer; rpc_node; infos; router_addr; _} ~sender
    ~token_addr =
  let name = "approve" in
  let params_ty = [`address; `uint 256] in
  let params = [`address (address router_addr); `int max_uint256] in
  wait_for_application sequencer @@ fun () ->
  let* _ =
    call
      infos
      rpc_node
      token_addr
      sender
      ~name
      params_ty
      params
      ~check_success:true
  in
  unit

let mk_path env ~nb_hops =
  let rec mk acc = function
    | 0 -> List.rev acc
    | i ->
        let acc =
          `address
            (address (if i mod 2 = 0 then env.gld_addr else env.gld2_addr))
          :: acc
        in
        mk acc (i - 1)
  in
  mk [`address (address env.wxtz_addr)] nb_hops

let swap_xtz ~nb_hops env iteration sender_index =
  let sender = env.accounts.(sender_index) in
  let dest =
    env.accounts.((sender_index + iteration) mod Array.length env.accounts)
  in
  let amount_out_min = Z.one in
  let deadline = int_of_float (Unix.time ()) + 600 in
  let params_ty = [`uint 256; `array `address; `address; `uint 256] in
  let params =
    [
      `int amount_out_min;
      `array (mk_path env ~nb_hops);
      `address (Account.address dest);
      `int (Z.of_int deadline);
    ]
  in
  let* _ =
    call
      env.infos
      env.rpc_node
      env.router_addr
      sender
      ~gas_limit:(Z.of_int 500_000)
      ~value:(Z.of_int (10000 + (iteration * 100)))
      ~name:"swapExactETHForTokens" (* "swapETHForExactTokens" *)
      params_ty
      params
  in
  unit

let step ({sequencer; accounts; nb_hops; _} as env) iteration =
  Log.report "Iteration %d" iteration ;
  let sender_indexes = List.init (Array.length accounts) Fun.id in
  let step_f () =
    Lwt_list.iter_p (swap_xtz ~nb_hops env iteration) sender_indexes
  in
  wait_for_application sequencer step_f

let create_pair
    {sequencer; rpc_node; infos; factory_addr; gld_addr; gld2_addr; _} ~sender =
  let name = "createPair" in
  let params_ty = [`address; `address] in
  let params = [`address (address gld_addr); `address (address gld2_addr)] in
  wait_for_application sequencer @@ fun () ->
  let* _ = call infos rpc_node factory_addr sender ~name params_ty params in
  unit

let test_swaps =
  let nb_accounts = Option.value parameters.accounts ~default:100 in
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
  let* accounts = floodgate_accounts sequencer accounts in
  let sender = accounts.(0) in
  (* Compile contracts *)
  Log.info "Deploying UniswapV2 contracts" ;
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
  Log.report "Deploying WXTZ" ;
  let* wxtz_addr = deploy_contract ~rpc_node infos ~sequencer sender `ERC20 in
  Log.info "  WXTZ: %s" wxtz_addr ;
  Log.report "Deploying GLD" ;
  let gldbin = Contracts.UniswapV2.GLDToken.json () |> bin_of_json_contract in
  let bin =
    gldbin
    ^ encode_value
        (`tuple [`string; `string])
        (`array [`string "Gold"; `string "GLD"])
  in
  let* gld_addr =
    deploy_contract ~rpc_node infos ~sequencer sender (`Custom bin)
  in
  Log.info "  GLD: %s" gld_addr ;
  Log.report "Deploying GLD2" ;
  let bin =
    gldbin
    ^ encode_value
        (`tuple [`string; `string])
        (`array [`string "Gold2"; `string "GLD2"])
  in
  let* gld2_addr =
    deploy_contract ~rpc_node infos ~sequencer sender (`Custom bin)
  in
  Log.info "  GLD2: %s" gld2_addr ;

  Log.report "Deploying UniswapV2 contracts" ;
  let fee_recv = sender in
  Log.report
    "- Deploying UniswapV2Factory (feeToSetter %s)"
    (Account.address fee_recv :> string) ;
  let bin = Contracts.UniswapV2.Factory.json () |> bin_of_json_contract in
  let bin = bin ^ encode_address (Account.address fee_recv :> string) in
  let* factory_addr =
    deploy_contract ~rpc_node infos ~sequencer sender (`Custom bin)
  in
  Log.info "    UniswapV2Factory address: %s" factory_addr ;
  Log.report "- Deploying UniswapV2Router02" ;
  let bin = Contracts.UniswapV2.Router02.json () |> bin_of_json_contract in
  let bin =
    String.concat
      ""
      [bin; encode_address factory_addr; encode_address wxtz_addr]
  in
  let* router_addr =
    deploy_contract ~rpc_node infos ~sequencer sender (`Custom bin)
  in
  Log.info "    UniswapV2Router02 address: %s" router_addr ;

  let env =
    {
      sequencer;
      rpc_node;
      infos;
      gas_limit = Z.zero;
      accounts;
      factory_addr;
      router_addr;
      multicall_addr;
      wxtz_addr;
      gld_addr;
      gld2_addr;
      nb_hops;
    }
  in
  Log.info "Creating pair GLD/GLD2" ;
  let* () = create_pair env ~sender in

  Log.info "Approving router to GLD" ;
  let* () = approve_router env ~sender ~token_addr:env.gld_addr in
  Log.info "Approving router to GLD2" ;
  let* () = approve_router env ~sender ~token_addr:env.gld2_addr in

  Log.info "Adding XTZ/GLD Liquidity" ;
  let* () =
    add_xtz_liquidity
      env
      ~sender
      ~token_addr:env.gld_addr
      ~xtz:1000
      ~token:10_000
  in
  Log.info "Adding XTZ/GLD2 Liquidity" ;
  let* () =
    add_xtz_liquidity
      env
      ~sender
      ~token_addr:env.gld2_addr
      ~xtz:1000
      ~token:100_000
  in
  Log.info "Adding GLD/GLD2 Liquidity" ;
  let* () = add_liquidity env ~sender ~gld:100_000 ~gld2:200_000 in

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

let register () = test_swaps [Protocol.Alpha]
