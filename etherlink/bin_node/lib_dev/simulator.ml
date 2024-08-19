(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

module type SimulationBackend = sig
  type simulation_state

  val simulation_state :
    ?block:Ethereum_types.Block_parameter.extended ->
    unit ->
    simulation_state tzresult Lwt.t

  val simulate_and_read :
    simulation_state ->
    input:Simulation.Encodings.simulate_input ->
    bytes tzresult Lwt.t

  val read : simulation_state -> path:string -> bytes option tzresult Lwt.t
end

(* This value is a hard maximum used by estimateGas. Set at Int64.max_int / 2 *)
let max_gas_limit = Z.of_int64 0x3FFFFFFFFFFFFFFFL

module Make (SimulationBackend : SimulationBackend) = struct
  let get_kernel_version simulation_state =
    let open Lwt_result_syntax in
    let* bytes =
      SimulationBackend.read
        simulation_state
        ~path:Durable_storage_path.kernel_version
    in
    let result =
      match bytes with
      | Some bytes -> Bytes.to_string bytes
      | None -> "KERNEL_VERSION_NOT_INITIALISED"
    in
    return result

  let get_storage_version simulation_state =
    let open Lwt_result_syntax in
    let+ bytes =
      SimulationBackend.read
        simulation_state
        ~path:Durable_storage_path.storage_version
    in
    match bytes with
    | Some bytes -> Z.of_bits (Bytes.unsafe_to_string bytes) |> Z.to_int
    | None -> 0

  let call_simulation ~log_file ~input_encoder ~input simulation_state =
    let open Lwt_result_syntax in
    let*? messages = input_encoder input in
    let insight_requests =
      [Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"]]
    in
    SimulationBackend.simulate_and_read
      simulation_state
      ~input:
        {
          messages;
          reveal_pages = None;
          insight_requests;
          log_kernel_debug_file = Some log_file;
        }

  let simulation_input ~simulation_version ~with_da_fees call =
    match simulation_version with
    | `V0 -> Simulation.V0 call
    | `V1 -> V1 {call; with_da_fees}
    | `V2 -> V2 {call; with_da_fees; timestamp = Misc.now ()}

  (* Simulation have different versions in the kernel, the inputs change
     between the different versions.

     As the simulation can be performed on past states, including past kernels,
     we cannot consider only latest version if it's supported by all latest
     kernels on ghostnet and mainnet.
  *)
  let simulation_version simulation_state =
    let open Lwt_result_syntax in
    let* storage_version = get_storage_version simulation_state in
    if storage_version < 12 then return `V0
    else if storage_version > 12 then return `V2
    else
      (* We are in the unknown, some kernels with STORAGE_VERSION = 12 have
         the features, some do not. *)
      let* kernel_version = get_kernel_version simulation_state in
      (* This is supposed to be the only version where STORAGE_VERSION is 12,
         but with_da_fees isn't enabled. *)
      if kernel_version = "ec7c3b349624896b269e179384d0a45cf39e1145" then
        return `V0
      else return `V1

  let simulate_call call block_param =
    let open Lwt_result_syntax in
    let* simulation_state =
      SimulationBackend.simulation_state ~block:block_param ()
    in
    let* simulation_version = simulation_version simulation_state in
    let* bytes =
      call_simulation
        simulation_state
        ~log_file:"simulate_call"
        ~input_encoder:Simulation.encode
        ~input:(simulation_input ~simulation_version ~with_da_fees:true call)
    in
    Lwt.return (Simulation.simulation_result bytes)

  let call_estimate_gas call simulation_state =
    let open Lwt_result_syntax in
    let* bytes =
      call_simulation
        ~log_file:"estimate_gas"
        ~input_encoder:Simulation.encode
        ~input:call
        simulation_state
    in
    Lwt.return (Simulation.gas_estimation bytes)

  (** [gas_for_fees simulation_state tx_data] returns the DA fees, i.e.
      the gas unit necessary for the data availability.

      The gas for fees must be computed based on a context, to retrieve
      the base fee per gas and da_fee_per_byte, these information are
      taken from [simulation_state].

      /!\
          This function must return enough gas for fees. Therefore it must
          be synchronised to fee model in the kernel.
      /!\

      The whole point of this function is to avoid an unncessary call
      to the WASM PVM to improve the performances.
  *)
  let gas_for_fees simulation_state tx_data : (Z.t, tztrace) result Lwt.t =
    let open Lwt_result_syntax in
    let read_qty path =
      let+ bytes = SimulationBackend.read simulation_state ~path in
      Option.map Ethereum_types.decode_number_le bytes
    in
    let* da_fee_per_byte = read_qty Durable_storage_path.da_fee_per_byte in
    let* (Qty gas_price) =
      let path = Durable_storage_path.base_fee_per_gas in
      let* gas_price_opt = read_qty path in
      match gas_price_opt with
      | None ->
          (* Base fee per gas is supposed to be updated in the storage after
             every block. *)
          failwith "Internal error: base fee per gas is not found at %s" path
      | Some gas_price -> return gas_price
    in
    let da_fee = Fees.gas_for_fees ?da_fee_per_byte ~gas_price tx_data in
    return da_fee

  let check_node_da_fees ~execution_gas ~node_da_fees ~simulation_version
      simulation_state call =
    let open Lwt_result_syntax in
    let* kernel_da_fees =
      let* res =
        call_estimate_gas
          (simulation_input ~simulation_version ~with_da_fees:true call)
          simulation_state
      in
      let* (Qty total_gas) =
        match res with
        | Ok (Ok {gas_used = Some gas; _}) -> return gas
        | _ -> failwith "The gas estimation simulation with DA fees failed."
      in
      (* DA fees computed by the kernel is the difference between total gas
         and previous gas call. *)
      return (Z.sub total_gas execution_gas)
    in
    unless (node_da_fees = kernel_da_fees) (fun () ->
        Prometheus.Counter.inc_one
          Metrics.metrics.simulation.inconsistent_da_fees ;
        let*! () = Events.invalid_node_da_fees ~node_da_fees ~kernel_da_fees in
        return_unit)

  let rec confirm_gas ~simulation_version (call : Ethereum_types.call) gas
      simulation_state =
    let open Ethereum_types in
    let open Lwt_result_syntax in
    let double (Qty z) = Qty Z.(mul (of_int 2) z) in
    let reached_max (Qty z) = z >= max_gas_limit in
    let new_call = {call with gas = Some gas} in
    let* result =
      call_estimate_gas
        (simulation_input ~simulation_version ~with_da_fees:false new_call)
        simulation_state
    in
    match result with
    | Error _ | Ok (Error _) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6984
           All errors should not be treated the same *)
        let new_gas = double gas in
        if reached_max new_gas then
          failwith "Gas estimate reached max gas limit."
        else confirm_gas ~simulation_version call new_gas simulation_state
    | Ok (Ok {gas_used = Some gas_used; _}) ->
        (* The gas returned by confirm gas can be ignored. What we care about
           is only knowing if the gas provided in {!new_call} is enough. The
           gas used returned when confirming may remove the "safe" amount
           we added. *)
        if simulation_version = `V0 then
          (* `V0 is the only simulation version that puts the DA fees
             in the gas used. *)
          return gas
        else
          (* If enabled, previous simulation did not take into account
             da fees, we need to add extra units here. *)
          let tx_data =
            match call.data with
            | Some (Hash (Hex data)) -> `Hex data |> Hex.to_bytes_exn
            | None -> Bytes.empty
          in
          let* da_fees = gas_for_fees simulation_state tx_data in
          (* As computing the DA fees in the node directly is an
             experimental feature, we check the locally computed value
             against the one computed by the kernel.

             We aim to deloy this checks for a couple weeks, then remove
             the kernel call. We could also keep the check if the node
             is in a debug like mode to make sure the two implementations
             are consistent.
          *)
          let* () =
            let (Qty gas_used) = gas_used in
            check_node_da_fees
              ~execution_gas:gas_used
              ~node_da_fees:da_fees
              ~simulation_version
              simulation_state
              call
          in
          let (Qty gas) = gas in
          return @@ quantity_of_z @@ Z.add gas da_fees
    | Ok (Ok {gas_used = None; _}) ->
        failwith "Internal error: gas used is missing from simulation"

  let estimate_gas call =
    let open Lwt_result_syntax in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7376

       Gas estimation currently ignores the block parameter. When this is fixed,
       we need to give the block parameter to the call to
       {!simulation_version}. *)
    let* simulation_state = SimulationBackend.simulation_state () in
    let* simulation_version = simulation_version simulation_state in
    let* res =
      call_estimate_gas
        (simulation_input ~simulation_version ~with_da_fees:false call)
        simulation_state
    in
    match res with
    | Ok (Ok {gas_used = Some (Qty gas); value}) ->
        (* See EIP2200 for reference. But the tl;dr is: we cannot do the
           opcode SSTORE if we have less than 2300 gas available, even
           if we don't consume it. The simulated amount then gives an
           amount of gas insufficient to execute the transaction.

           The extra gas units, i.e. 2300, will be refunded.
        *)
        let safe_gas = Z.(add gas (of_int 2300)) in
        (* add a safety margin of 2%, sufficient to cover a 1/64th difference *)
        let safe_gas = Z.(add safe_gas (cdiv safe_gas (of_int 50))) in
        let+ gas_used =
          confirm_gas ~simulation_version call (Qty safe_gas) simulation_state
        in
        Ok (Ok {Simulation.gas_used = Some gas_used; value})
    | _ -> return res

  let is_tx_valid tx_raw =
    let open Lwt_result_syntax in
    let* simulation_state = SimulationBackend.simulation_state () in
    let* bytes =
      call_simulation
        ~log_file:"tx_validity"
        ~input_encoder:Simulation.encode_tx
        ~input:tx_raw
        simulation_state
    in
    Lwt.return (Simulation.is_tx_valid bytes)
end
