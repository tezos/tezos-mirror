(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  chain_id : Ethereum_types.quantity;
  base_fee_per_gas : Z.t;
  gas_limit : Z.t;
}

let get_chain_id ~evm_node_endpoint =
  let open Lwt_result_syntax in
  let* chain_id =
    Batch.call
      (module Rpc_encodings.Chain_id)
      ~keep_alive:true
      ~evm_node_endpoint
      ()
  in
  let* () =
    when_ (chain_id = Constants.chain_id Mainnet) @@ fun () ->
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
      (Qty Z.one, Latest, [])
  in
  let (Qty base_fee_per_gas) = Stdlib.List.hd fee_history.base_fee_per_gas in
  return base_fee_per_gas

let get_gas_limit ~base_fee_per_gas ~rpc_endpoint account =
  let open Lwt_result_syntax in
  let* (Qty gas_limit) =
    Batch.call
      (module Rpc_encodings.Get_estimate_gas)
      ~evm_node_endpoint:rpc_endpoint
      ~keep_alive:true
      ( {
          from = Some (Account.address_et account);
          to_ = Some (Account.address_et account);
          gas = None;
          gasPrice = Some (Qty base_fee_per_gas);
          value = Some (Qty Z.one);
          data = None;
        },
        Latest )
  in
  return gas_limit

let fetch ~rpc_endpoint ~base_fee_factor account =
  let open Lwt_result_syntax in
  let* chain_id = get_chain_id ~evm_node_endpoint:rpc_endpoint in
  let* base_fee_per_gas = get_base_fee_per_gas ~rpc_endpoint in
  let base_fee_per_gas =
    Z.(to_float base_fee_per_gas *. base_fee_factor |> of_float)
  in
  let* gas_limit = get_gas_limit ~rpc_endpoint ~base_fee_per_gas account in
  return {chain_id; base_fee_per_gas; gas_limit}
