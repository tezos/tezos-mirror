(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Block_services =
  Tezos_client_014_PtKathma.Protocol_client_context.Alpha_block_services

open Lwt_result_syntax
open Tezos_protocol_014_PtKathma
open Tezos_protocol_plugin_014_PtKathma

let public_key_hash_of_v0 :
    Environment.Signature.public_key_hash ->
    Tezos_crypto.Signature.public_key_hash = function
  | Environment.Signature.Ed25519 x -> Tezos_crypto.Signature.Ed25519 x
  | Environment.Signature.Secp256k1 x -> Tezos_crypto.Signature.Secp256k1 x
  | Environment.Signature.P256 x -> Tezos_crypto.Signature.P256 x

module Services : Protocol_machinery.PROTOCOL_SERVICES = struct
  let hash = Protocol.hash

  type wrap_full = Tezos_client_014_PtKathma.Protocol_client_context.wrap_full

  let wrap_full cctxt =
    new Tezos_client_014_PtKathma.Protocol_client_context.wrap_full cctxt

  let slot_to_int x =
    (* YES this is Fun.x ! *)
    Data_encoding.Binary.of_bytes_exn
      Data_encoding.uint16
      (Data_encoding.Binary.to_bytes_exn Protocol.Alpha_context.Slot.encoding x)

  let attesting_rights cctxt ~reference_level level =
    let*? level =
      Environment.wrap_tzresult
      @@ Protocol.Alpha_context.Raw_level.of_int32 level
    in
    let* answers =
      Plugin.RPC.Endorsing_rights.get
        cctxt
        ~levels:[level]
        (cctxt#chain, `Level reference_level)
    in
    match answers with
    | answer :: _ ->
        return
          (List.map
             (fun Plugin.RPC.Endorsing_rights.
                    {delegate; first_slot; endorsing_power}
                ->
               Consensus_ops.
                 {
                   address = public_key_hash_of_v0 delegate;
                   first_slot = slot_to_int first_slot;
                   power = endorsing_power;
                 })
             answer.Plugin.RPC.Endorsing_rights.delegates_rights)
    | [] -> return_nil

  type block_id = Protocol.Block_payload_hash.t

  module BlockIdMap = Map.Make (Protocol.Block_payload_hash)

  let extract_attestation
      (operation_content : Protocol.Alpha_context.packed_operation) =
    match operation_content with
    | {
     Protocol.Main.protocol_data =
       Protocol.Alpha_context.Operation_data
         {
           contents =
             Single (Endorsement {slot; level; round; block_payload_hash});
           signature = _;
         };
     shell = _;
    } ->
        Some
          ( ( block_payload_hash,
              Protocol.Alpha_context.Raw_level.to_int32 level,
              Consensus_ops.Attestation,
              Some (Protocol.Alpha_context.Round.to_int32 round) ),
            slot_to_int slot )
    | {
     Protocol.Main.protocol_data =
       Protocol.Alpha_context.Operation_data
         {
           contents =
             Single (Preendorsement {slot; level; round; block_payload_hash});
           signature = _;
         };
     shell = _;
    } ->
        Some
          ( ( block_payload_hash,
              Protocol.Alpha_context.Raw_level.to_int32 level,
              Consensus_ops.Preattestation,
              Some (Protocol.Alpha_context.Round.to_int32 round) ),
            slot_to_int slot )
    | _ -> None

  let consensus_operation_stream cctxt =
    let* ops_stream, stopper =
      Block_services.Mempool.monitor_operations
        cctxt
        ~chain:cctxt#chain
        ~validated:true
        ~refused:false
        ~branch_delayed:true
        ~branch_refused:true
        ~outdated:false
        ~validation_passes:[0]
        ()
    in
    let op_stream = Lwt_stream.flatten ops_stream in
    return
      ( Lwt_stream.filter_map
          (fun ((hash, op), errors) ->
            Option.map (fun x -> ((hash, x), errors)) (extract_attestation op))
          op_stream,
        stopper )

  let cycles_size =
    let ans = ref None in
    fun cctxt ref_block ->
      match !ans with
      | Some x -> return x
      | None ->
          let* constants =
            Protocol.Alpha_services.Constants.all cctxt ref_block
          in
          let out =
            constants.Protocol.Alpha_context.Constants.parametric
              .blocks_per_cycle
          in
          ans := Some out ;
          return out

  let cycle_info (metadata : Block_services.block_metadata) cctxt ref_block =
    let* cycle_size = cycles_size cctxt ref_block in
    return
      Data.
        {
          cycle =
            Protocol.Alpha_context.Cycle.to_int32
              metadata.protocol_data.level_info.cycle;
          cycle_position = metadata.protocol_data.level_info.cycle_position;
          cycle_size;
        }

  let baker_and_cycle cctxt hash =
    let* metadata =
      Block_services.metadata
        ~chain:cctxt#chain
        ~block:(`Hash (hash, 0))
        cctxt
        ()
    in
    let* cycle_info =
      cycle_info metadata cctxt (cctxt#chain, `Hash (hash, 0))
    in
    return (public_key_hash_of_v0 metadata.protocol_data.baker, cycle_info)

  let baking_rights cctxt level round =
    let* baking_rights =
      Plugin.RPC.Baking_rights.get
        ?levels:None
        ~max_round:round
        ~all:true
        cctxt
        (cctxt#chain, `Level (Int32.pred level))
    in
    match
      List.rev_map
        (fun ({delegate; round; _} : RPC.Baking_rights.t) ->
          {
            Data.delegate = public_key_hash_of_v0 delegate;
            round = Protocol.Alpha_context.Round.to_int32 round;
          })
        baking_rights
    with
    | [] -> fail_with_exn Not_found
    | hd :: _ as baking_rights ->
        assert (Int32.of_int round = hd.round) ;
        return (hd.delegate, baking_rights)

  let raw_block_round shell_header =
    let wrap = Environment.wrap_tzresult in
    let open Result_syntax in
    let* round =
      wrap
      @@ Protocol.Alpha_context.Fitness.round_from_raw
           shell_header.Block_header.fitness
    in
    wrap @@ Protocol.Alpha_context.Round.to_int round

  let block_round (header : Block_header.t) =
    raw_block_round header.Block_header.shell

  let get_attestation_round protocol_data =
    match protocol_data with
    | Protocol.Alpha_context.Operation_data {contents; _} -> (
        match contents with
        | Single (Endorsement {round; _}) ->
            Protocol.Alpha_context.Round.to_int32 round
        | _ -> assert false)

  let get_preattestation_round protocol_data =
    match protocol_data with
    | Protocol.Alpha_context.Operation_data {contents; _} -> (
        match contents with
        | Single (Preendorsement {round; _}) ->
            Protocol.Alpha_context.Round.to_int32 round
        | _ -> assert false)

  let consensus_ops_info_of_block cctxt hash =
    let* ops =
      Block_services.Operations.operations_in_pass
        cctxt
        ~chain:cctxt#chain
        ~block:(`Hash (hash, 0))
        0
    in
    List.fold_left
      (fun acc Block_services.{hash; receipt; protocol_data; _} ->
        match receipt with
        | Receipt
            (Protocol.Apply_results.Operation_metadata
               {
                 contents =
                   Single_result
                     (Protocol.Apply_results.Preendorsement_result
                        {delegate; preendorsement_power; _});
               }) ->
            Consensus_ops.
              {
                op =
                  {
                    hash;
                    round = Some (get_preattestation_round protocol_data);
                    kind = Consensus_ops.Preattestation;
                  };
                delegate = public_key_hash_of_v0 delegate;
                power = preendorsement_power;
              }
            :: acc
        | Receipt
            (Protocol.Apply_results.Operation_metadata
               {
                 contents =
                   Single_result
                     (Protocol.Apply_results.Endorsement_result
                        {delegate; endorsement_power; _});
               }) ->
            Consensus_ops.
              {
                op =
                  {
                    hash;
                    round = Some (get_attestation_round protocol_data);
                    kind = Consensus_ops.Attestation;
                  };
                delegate = public_key_hash_of_v0 delegate;
                power = endorsement_power;
              }
            :: acc
        | _ -> acc)
      []
      ops
    |> return

  let get_block_info cctxt level =
    let* header =
      Block_services.header ~chain:cctxt#chain ~block:(`Level level) cctxt ()
    in
    let* metadata =
      Block_services.metadata ~chain:cctxt#chain ~block:(`Level level) cctxt ()
    in
    let*? round = raw_block_round header.shell in
    let* cycle_info = cycle_info metadata cctxt (cctxt#chain, `Level level) in
    return
      ( ( public_key_hash_of_v0 metadata.protocol_data.baker,
          header.shell.timestamp,
          round,
          header.hash,
          Some header.shell.predecessor ),
        cycle_info )
end

module M = General_archiver.Define (Services)
