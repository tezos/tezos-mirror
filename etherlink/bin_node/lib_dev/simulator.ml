(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

module type SimulationBackend = sig
  include Durable_storage.READER

  val modify : key:string -> value:string -> state -> state tzresult Lwt.t

  val simulate_and_read :
    ?state_override:Ethereum_types.state_override ->
    state ->
    input:Simulation.Encodings.simulate_input ->
    bytes tzresult Lwt.t
end

module MakeEtherlink (SimulationBackend : SimulationBackend) = struct
  let call_simulation ?(state_override = Ethereum_types.state_override_empty)
      ~log_file ~input_encoder ~input simulation_state =
    let open Lwt_result_syntax in
    let*? messages = input_encoder input in
    let insight_requests =
      [Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"]]
    in
    SimulationBackend.simulate_and_read
      ~state_override
      simulation_state
      ~input:
        {
          messages;
          reveal_pages = None;
          insight_requests;
          log_kernel_debug_file = Some log_file;
        }

  let simulation_input ~simulation_version ~with_da_fees ~timestamp call =
    match simulation_version with
    | `V0 -> Simulation.V0 call
    | `V1 -> V1 {call; with_da_fees}
    | `V2 -> V2 {call; with_da_fees; timestamp}

  (* Simulation have different versions in the kernel, the inputs change
     between the different versions.

     As the simulation can be performed on past states, including past kernels,
     we cannot consider only latest version if it's supported by all latest
     kernels on ghostnet and mainnet.
  *)
  let simulation_version simulation_state =
    let open Lwt_result_syntax in
    let* storage_version =
      Durable_storage.storage_version (SimulationBackend.read simulation_state)
    in
    if Storage_version.simulation_v0 ~storage_version then return `V0
    else if Storage_version.simulation_v2 ~storage_version then return `V2
    else
      (* We are in the unknown, some kernels with STORAGE_VERSION = 12 have
         the features, some do not. *)
      let* kernel_version =
        Durable_storage.kernel_version (SimulationBackend.read simulation_state)
      in
      (* This is supposed to be the only version where STORAGE_VERSION is 12,
         but with_da_fees isn't enabled. *)
      if kernel_version = "ec7c3b349624896b269e179384d0a45cf39e1145" then
        return `V0
      else return `V1

  let simulate_call ~overwrite_tick_limit call block_param state_override =
    let open Lwt_result_syntax in
    let* simulation_state = SimulationBackend.get_state ~block:block_param () in
    let timestamp = Misc.now () in
    let* simulation_version = simulation_version simulation_state in
    let* simulation_state =
      if overwrite_tick_limit then
        SimulationBackend.modify
          simulation_state
          ~key:"/evm/maximum_allowed_ticks"
          ~value:
            Data_encoding.(
              Binary.to_string_exn Little_endian.int64 1_000_000_000_000L)
      else return simulation_state
    in
    let* bytes =
      call_simulation
        ~state_override
        simulation_state
        ~log_file:"simulate_call"
        ~input_encoder:Simulation.encode
        ~input:
          (simulation_input
             ~timestamp
             ~simulation_version
             ~with_da_fees:true
             call)
    in
    Lwt.return (Simulation.simulation_result bytes)

  let call_estimate_gas ?(state_override = Ethereum_types.state_override_empty)
      call simulation_state =
    let open Lwt_result_syntax in
    let* bytes =
      call_simulation
        ~log_file:"estimate_gas"
        ~input_encoder:Simulation.encode
        ~input:call
        ~state_override
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
  let da_fees_gas_limit_overhead simulation_state tx_data :
      (Z.t, tztrace) result Lwt.t =
    let open Lwt_result_syntax in
    let read_qty path =
      let+ bytes = SimulationBackend.read simulation_state path in
      Option.map Ethereum_types.decode_number_le bytes
    in
    let* da_fee_per_byte =
      Etherlink_durable_storage.da_fee_per_byte
        (SimulationBackend.read simulation_state)
    in
    let* (Qty minimum_base_fee_per_gas) =
      (* In future iterations of the kernel, the default value will be
         written to the storage. This default value will no longer need to
         be declared here. *)
      let path = Durable_storage_path.minimum_base_fee_per_gas in
      let* minimum_base_feer_per_gas_opt = read_qty path in
      match minimum_base_feer_per_gas_opt with
      | None ->
          return
            (Ethereum_types.quantity_of_z Fees.default_minimum_base_fee_per_gas)
      | Some minimum_base_feer_per_gas -> return minimum_base_feer_per_gas
    in
    let da_fee =
      Fees.da_fees_gas_limit_overhead
        ~da_fee_per_byte
        ~minimum_base_fee_per_gas
        tx_data
    in
    return da_fee

  let rec confirm_gas ~timestamp ~maximum_gas_per_transaction
      ~simulation_version ~state_override (call : Ethereum_types.call) gas
      simulation_state =
    let open Ethereum_types in
    let open Lwt_result_syntax in
    let double (Qty z) = Qty Z.(mul (of_int 2) z) in
    let reached_max (Qty z) = z >= maximum_gas_per_transaction in
    let new_call = {call with gas = Some gas} in
    let* result =
      call_estimate_gas
        ~state_override
        (simulation_input
           ~timestamp
           ~simulation_version
           ~with_da_fees:false
           new_call)
        simulation_state
    in
    match result with
    | Error _ | Ok (Error _) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6984
           All errors should not be treated the same *)
        Metrics.inc_confirm_gas_needed () ;
        if reached_max gas then
          failwith
            "Gas estimate reached max gas limit of %s"
            (Z.to_string maximum_gas_per_transaction)
        else
          let new_gas = double gas in
          if reached_max new_gas then
            (* We try one last time with maximum gas possible. *)
            confirm_gas
              ~timestamp
              ~maximum_gas_per_transaction
              ~simulation_version
              ~state_override
              call
              (Qty maximum_gas_per_transaction)
              simulation_state
          else
            confirm_gas
              ~timestamp
              ~maximum_gas_per_transaction
              ~simulation_version
              ~state_override
              call
              new_gas
              simulation_state
    | Ok (Ok {gas_used = Some _; _}) ->
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
          let* da_fees = da_fees_gas_limit_overhead simulation_state tx_data in
          let (Qty gas) = gas in
          return @@ quantity_of_z @@ Z.add gas da_fees
    | Ok (Ok {gas_used = None; _}) ->
        failwith "Internal error: gas used is missing from simulation"

  let estimate_gas call block_param state_override =
    let open Lwt_result_syntax in
    let* simulation_state = SimulationBackend.get_state ~block:block_param () in
    let timestamp = Misc.now () in
    let* (Qty maximum_gas_per_transaction) =
      Etherlink_durable_storage.maximum_gas_per_transaction
        (SimulationBackend.read simulation_state)
    in
    let* simulation_version = simulation_version simulation_state in
    let* res =
      call_estimate_gas
        ~state_override
        (simulation_input
           ~timestamp
           ~simulation_version
           ~with_da_fees:false
           call)
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
          confirm_gas
            ~state_override
            ~timestamp
            ~maximum_gas_per_transaction
            ~simulation_version
            call
            (Qty safe_gas)
            simulation_state
        in
        Ok (Ok {Simulation.gas_used = Some gas_used; value})
    | _ -> return res
end

type error += Operation_serialization_error of Data_encoding.Binary.write_error

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.operation_serialization_error"
    ~title:"Operation serialization failed"
    ~description:"An error occured during the serialization of an operation."
    ~pp:(fun ppf e ->
      Format.fprintf
        ppf
        "Operation serialization failed: %a"
        Data_encoding.Binary.pp_write_error
        e)
    Data_encoding.(obj1 (req "error" Binary.write_error_encoding))
    (function Operation_serialization_error e -> Some e | _ -> None)
    (fun e -> Operation_serialization_error e)

module MakeTezlink (SimulationBackend : SimulationBackend) = struct
  open Tezlink_imports
  open Imported_protocol

  let call_simulation ~input ~skip_signature block =
    let open Lwt_result_syntax in
    let* simulation_state = SimulationBackend.get_state ~block () in
    let skip_signature_tag = if skip_signature then "\000" else "\001" in
    let*? messages = String.chunk_bytes 4096 (Bytes.of_string input) in
    let nb_messages = Ethereum_types.u16_to_bytes (List.length messages) in
    let insight_requests =
      [
        Simulation.Encodings.Durable_storage_key ["tezlink"; "simulation_result"];
      ]
    in
    SimulationBackend.simulate_and_read
      ~state_override:Ethereum_types.state_override_empty
      simulation_state
      ~input:
        {
          messages =
            [Simulation.simulation_tag; skip_signature_tag; nb_messages]
            @ messages;
          reveal_pages = None;
          insight_requests;
          log_kernel_debug_file = Some "simulate_call";
        }

  let simulate_operation ~chain_id ~skip_signature ~read ~data_model
      (op : Imported_protocol.operation) _hash block =
    let open Lwt_result_syntax in
    let*? input =
      Data_encoding.Binary.to_string Alpha_context.Operation.encoding op
      |> Result.map_error_e @@ fun e ->
         Result_syntax.tzfail (Operation_serialization_error e)
    in
    (* First, prevalidate the operation because there is no point
       in simulating it if it's invalid (invalid operations don't
       produce any receipt). *)
    let* (prevalidation_res : (Tezos_types.Operation.t, string) result) =
      Tezlink_prevalidation.parse_and_validate_for_queue
        ~check_signature:(not skip_signature)
        ~read
        ~data_model
        input
    in
    let* () =
      match prevalidation_res with
      | Ok _op -> return_unit
      | Error message -> Error_monad.failwith "Prevalidation error: %s" message
    in

    (* Now, the actual simulation. *)
    let* bytes = call_simulation ~input ~skip_signature block in
    let*? operations =
      Tezos_services.Current_block_services.deserialize_operations
        ~chain_id
        bytes
    in
    let* simulation_receipt =
      match operations with
      | [op] -> (
          match op.receipt with
          | Receipt receipt -> return receipt
          | Empty -> failwith "Simulation produced an empty receipt"
          | Too_large -> failwith "Produced simulation receipt was too large.")
      | _ ->
          failwith
            "Simulation produced a list of operations whose length is not 1"
    in
    return simulation_receipt
end
