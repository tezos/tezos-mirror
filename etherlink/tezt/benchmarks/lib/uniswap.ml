(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Evm_node_lib_dev_encoding
open Benchmark_utils
open Floodgate_lib

type env = {
  container :
    L2_types.evm_chain_family Evm_node_lib_dev.Services_backend_sig.tx_container;
  sequencer : Evm_node.t;
  rpc_node : Evm_node.t;
  infos : Network_info.t;
  gas_limit : Z.t;
  accounts : Floodgate_lib.Account.t Array.t;
  wxtz_addr : string;
  gld_tokens : string list;
  factory_addr : string;
  router_addr : string;
  nb_hops : int;
  total_confirmed : int ref;
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

let deposit_wxtz ~gas_limit {container; infos; rpc_node; wxtz_addr; _} ?nonce
    value account =
  let* _ =
    call
      ~container
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
      ~timeout:10.
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
      ~timeout:10.
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

let add_xtz_liquidity {container; sequencer; rpc_node; infos; router_addr; _}
    ~sender ~token_addr ~xtz ~token =
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
      ~container
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

let add_xtz_liquidities env ~sender =
  Lwt_list.iteri_s
    (fun i token_addr ->
      Log.info "Adding XTZ/GLD%i Liquidity" (i + 1) ;
      let token = 1000 * 10 * (i + 1) in
      add_xtz_liquidity env ~sender ~token_addr ~xtz:1000 ~token)
    env.gld_tokens

let add_liquidity {container; sequencer; rpc_node; infos; router_addr; _}
    ~sender ~gld ~gld2 =
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
      `address (address (fst gld));
      `address (address (fst gld2));
      `int (to_wei (snd gld));
      `int (to_wei (snd gld2));
      `int Z.one;
      `int Z.one;
      `address to_;
      `int (Z.of_int deadline);
    ]
  in
  wait_for_application sequencer @@ fun () ->
  let* _ =
    call
      ~container
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

let pairs {gld_tokens; _} =
  let tokens = List.mapi (fun i t -> (sf "GLD%d" (i + 1), t)) gld_tokens in
  match tokens with
  | [] -> assert false
  | [_] -> []
  | [gld1; gld2] -> [(gld1, gld2)]
  | t :: rest -> List.combine tokens (rest @ [t])

let add_token_liquidities env ~sender =
  match env.gld_tokens with
  | [] | [_] -> unit
  | _ ->
      Log.info "Adding token liquidities" ;
      Lwt_list.iter_s
        (fun ((n1, p1), (n2, p2)) ->
          Log.info " - Adding liquidity in %s/%s" n1 n2 ;
          add_liquidity env ~sender ~gld:(p1, 100_000) ~gld2:(p2, 200_000))
        (pairs env)

let approve_router {container; sequencer; rpc_node; infos; router_addr; _}
    ~sender ~token_addr =
  let name = "approve" in
  let params_ty = [`address; `uint 256] in
  let params = [`address (address router_addr); `int max_uint256] in
  wait_for_application sequencer @@ fun () ->
  let* _ =
    call
      ~container
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

let approve_router_to_tokens env ~sender =
  Lwt_list.iteri_s
    (fun i token_addr ->
      Log.info "Approving router to GLD%d" (i + 1) ;
      approve_router env ~sender ~token_addr)
    env.gld_tokens

let mk_path env ~nb_hops =
  let tokens = Seq.cycle (List.to_seq env.gld_tokens) in
  let hops = Seq.take nb_hops tokens in
  let rpath = Seq.map (fun a -> `address (address a)) hops in
  `address (address env.wxtz_addr) :: List.of_seq rpath

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
      ~container:env.container
      env.infos
      env.rpc_node
      env.router_addr
      sender
      ~gas_limit:(Z.of_int (2_600_000 * nb_hops)) (* Rough approximation *)
      ~value:(Z.of_int (10000 + (iteration * 100)))
      ~name:"swapExactETHForTokens" (* "swapETHForExactTokens" *)
      params_ty
      params
      ~total_confirmed:env.total_confirmed
      ~check_success:false
    (* Don't check success as this is an additional RPC and will slow the
       benchmark down. It can be set to true for debugging. *)
  in
  unit

let step ({sequencer; accounts; nb_hops; _} as env) iteration =
  Log.report "Iteration %d" iteration ;
  let sender_indexes = List.init (Array.length accounts) Fun.id in
  let step_f () =
    Lwt_list.iter_p (swap_xtz ~nb_hops env iteration) sender_indexes
  in
  wait_for_application sequencer step_f

let create_pair ?nonce {container; sequencer; rpc_node; infos; factory_addr; _}
    ~sender ((n, gld_addr), (n2, gld2_addr)) =
  Log.info " - Create pair %s/%s" n n2 ;
  let name = "createPair" in
  let params_ty = [`address; `address] in
  let params = [`address (address gld_addr); `address (address gld2_addr)] in
  wait_for_application sequencer @@ fun () ->
  let* _ =
    call
      ~container
      infos
      rpc_node
      factory_addr
      sender
      ?nonce
      ~name
      params_ty
      params
  in
  unit

let create_pairs env ~sender =
  match env.gld_tokens with
  | [] | [_] -> unit
  | _ ->
      Log.info "Creating pairs" ;
      Lwt_list.iter_s (create_pair env ~sender) (pairs env)

let deploy_gld_token infos ~sequencer ~rpc_node ~sender i =
  Log.report "Deploying GLD%d" i ;
  let gldbin = Contracts.UniswapV2.GLDToken.json () |> bin_of_json_contract in
  let bin =
    gldbin
    ^ encode_value
        (`tuple [`string; `string])
        (`array [`string (sf "Gold%d" i); `string (sf "GLD%d" i)])
  in
  wait_for_application sequencer @@ fun () ->
  deploy_contract ~rpc_node infos ~sequencer sender (`Custom bin)

let deploy_gld_tokens infos ~sequencer ~rpc_node ~sender nb =
  Lwt_list.map_s
    (fun i -> deploy_gld_token infos ~sequencer ~rpc_node ~sender (i + 1))
    (List.init nb Fun.id)

let setup ~accounts ~nb_tokens ~nb_hops ~sequencer ~rpc_node =
  let* accounts = floodgate_accounts sequencer accounts in
  let sender = accounts.(0) in
  (* Compile contracts *)
  Log.info "Deploying UniswapV2 contracts" ;
  let endpoint = Evm_node.endpoint rpc_node |> Uri.of_string in
  let*? infos =
    Network_info.fetch ~rpc_endpoint:endpoint ~base_fee_factor:1000.
  in
  let start_container, container =
    Evm_node_lib_dev.Tx_queue.tx_container ~chain_family:EVM
  in
  let (Evm_node_lib_dev.Services_backend_sig.Evm_tx_container
         (module Tx_container)) =
    container
  in
  let max_size = 999_999 in
  let tx_per_addr_limit = Int64.max_int in
  let max_transaction_batch_length = None in
  let max_lifespan_s = int_of_float parameters.timeout in
  let config : Evm_node_config.Configuration.tx_queue =
    {max_size; max_transaction_batch_length; max_lifespan_s; tx_per_addr_limit}
  in
  let*? () =
    start_container ~config ~keep_alive:true ~timeout:parameters.timeout ()
  in
  let* () = Floodgate_events.is_ready infos.chain_id infos.base_fee_per_gas in
  let follower =
    Floodgate.start_blueprint_follower
      ~relay_endpoint:endpoint
      ~rpc_endpoint:endpoint
      ()
  in
  let tx_queue =
    Tx_container.tx_queue_beacon
      ~evm_node_endpoint:(Rpc endpoint)
      ~tick_interval:0.25
  in
  Log.report "Deploying WXTZ" ;
  let* wxtz_addr = deploy_contract ~rpc_node infos ~sequencer sender `ERC20 in
  Log.info "  WXTZ: %s" wxtz_addr ;

  let* gld_tokens =
    deploy_gld_tokens infos ~sequencer ~rpc_node ~sender nb_tokens
  in

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
      container;
      sequencer;
      rpc_node;
      infos;
      gas_limit = Z.zero;
      accounts;
      factory_addr;
      router_addr;
      wxtz_addr;
      gld_tokens;
      nb_hops;
      total_confirmed = ref 0;
    }
  in

  let* () = create_pairs env ~sender in

  let* () = approve_router_to_tokens env ~sender in

  Log.info "Adding XTZ/GLD1 Liquidity" ;
  let* () =
    add_xtz_liquidity
      env
      ~sender
      ~token_addr:(List.hd env.gld_tokens)
      ~xtz:1000
      ~token:10_000
  in
  let* () = add_token_liquidities env ~sender in

  let shutdown () =
    Lwt.cancel follower ;
    Lwt.cancel tx_queue ;
    let*? () = Tx_container.shutdown () in
    let* () = Evm_node.terminate sequencer in
    unit
  in

  return (env, shutdown)
