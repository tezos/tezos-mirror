(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let last_observed_drift_ref = ref Z.zero

let last_observed_drift () = !last_observed_drift_ref

let measure_drift (type f) ~(chain_family : f L2_types.chain_family)
    ~evm_node_endpoint ~timeout get_next_blueprint_number =
  let open Ethereum_types in
  let open Lwt_result_syntax in
  let*! (Qty next_blueprint_number) = get_next_blueprint_number () in
  let method_ :
      (module Rpc_encodings.METHOD
         with type input = unit
          and type output = quantity) =
    match chain_family with
    | L2_types.EVM -> (module Rpc_encodings.Block_number)
    | L2_types.Michelson -> (module Rpc_encodings.Generic_block_number)
  in
  let* (Qty remote_head) =
    Batch.call method_ ~keep_alive:true ~timeout ~evm_node_endpoint ()
  in
  (last_observed_drift_ref := Z.(sub remote_head (pred next_blueprint_number))) ;
  return_unit

let run ~chain_family ~evm_node_endpoint ~timeout get_next_blueprint_number =
  let open Lwt_result_syntax in
  let*! () = Lwt_unix.sleep 60. in
  let* () =
    measure_drift
      ~chain_family
      ~evm_node_endpoint
      ~timeout
      get_next_blueprint_number
  in
  let*! () = Events.drift_monitor_is_ready !last_observed_drift_ref in
  let rec aux () =
    let*! () = Lwt_unix.sleep 60. in
    let* () =
      measure_drift
        ~chain_family
        ~evm_node_endpoint
        ~timeout
        get_next_blueprint_number
    in
    aux ()
  in
  aux ()
