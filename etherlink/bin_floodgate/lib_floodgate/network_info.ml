(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = {chain_id : L2_types.chain_id; base_fee_per_gas : Z.t}

let timeout = 10.

let get_chain_id ~evm_node_endpoint =
  let open Lwt_result_syntax in
  let* chain_id =
    Batch.call
      (module Rpc_encodings.Chain_id)
      ~keep_alive:true
      ~timeout
      ~evm_node_endpoint
      ()
  in
  let mainnet_chain_id = Configuration.chain_id Mainnet in
  let* () =
    when_ (chain_id = mainnet_chain_id) @@ fun () ->
    Lwt_result.ok (Floodgate_events.mainnet_experiment ())
  in
  return chain_id

let get_base_fee_per_gas ~rpc_endpoint =
  let open Lwt_result_syntax in
  let* fee_history =
    Batch.call
      (module Rpc_encodings.Eth_fee_history)
      ~evm_node_endpoint:rpc_endpoint
      ~keep_alive:true
      ~timeout
      (Qty Z.one, Latest, [])
  in
  let (Qty base_fee_per_gas) = Stdlib.List.hd fee_history.base_fee_per_gas in
  return base_fee_per_gas

let get_gas_limit ?data ?from ?to_ ?(value = Z.one) ~rpc_endpoint
    ~base_fee_per_gas () =
  let open Lwt_result_syntax in
  let* (Qty gas_limit) =
    Batch.call
      (module Rpc_encodings.Get_estimate_gas)
      ~evm_node_endpoint:rpc_endpoint
      ~keep_alive:true
      ~timeout
      ( {
          from;
          to_;
          gas = None;
          gasPrice = Some (Qty base_fee_per_gas);
          value = Some (Qty value);
          data;
        },
        Block_parameter Latest,
        Ethereum_types.AddressMap.empty )
  in
  return gas_limit

let fetch ~rpc_endpoint ~base_fee_factor =
  let open Lwt_result_syntax in
  let* chain_id = get_chain_id ~evm_node_endpoint:rpc_endpoint in
  let* base_fee_per_gas = get_base_fee_per_gas ~rpc_endpoint in
  let base_fee_per_gas =
    Z.(to_float base_fee_per_gas *. base_fee_factor |> of_float)
  in
  return {chain_id; base_fee_per_gas}
