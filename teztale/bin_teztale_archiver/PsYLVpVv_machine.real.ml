(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Block_services =
  Tezos_client_002_PsYLVpVv.Alpha_client_context.Alpha_block_services

open Lwt_result_syntax
open Tezos_protocol_002_PsYLVpVv

let public_key_hash_of_v0 :
    Environment.Signature.public_key_hash ->
    Tezos_crypto.Signature.public_key_hash = function
  | Environment.Signature.Ed25519 x -> Tezos_crypto.Signature.Ed25519 x
  | Environment.Signature.Secp256k1 x -> Tezos_crypto.Signature.Secp256k1 x
  | Environment.Signature.P256 x -> Tezos_crypto.Signature.P256 x

module Services : Protocol_machinery.PROTOCOL_SERVICES = struct
  let hash = Protocol.hash

  type wrap_full = Tezos_client_002_PsYLVpVv.Alpha_client_context.full

  let wrap_full cctxt =
    new Tezos_client_002_PsYLVpVv.Alpha_client_context.wrap_full cctxt

  let attesting_rights cctxt ~reference_level level =
    let*? level =
      Environment.wrap_error @@ Protocol.Alpha_context.Raw_level.of_int32 level
    in
    let* rights =
      Protocol.Delegate_services.Endorsing_rights.get
        cctxt
        ~levels:[level]
        (cctxt#chain, `Level reference_level)
    in
    return
    @@ List.map
         (fun Protocol.Delegate_services.Endorsing_rights.{delegate; slots; _}
            ->
           Consensus_ops.
             {
               address = public_key_hash_of_v0 delegate;
               first_slot = Stdlib.List.hd (List.sort compare slots);
               power = List.length slots;
             })
         rights

  type block_id = Block_hash.t

  module BlockIdMap = Block_hash.Map

  let extract_attestation
      (_operation_content : Protocol.Alpha_context.packed_operation) =
    (* match operation_content with
       | {
        Protocol.Main.protocol_data =
          Protocol.Alpha_context.Operation_data
            {
              contents = Single (Endorsement {level});
              signature = _;
            };
        shell = {branch};
       } ->
           Some
             ( ( branch,
                 Protocol.Alpha_context.Raw_level.to_int32 level,
                 Consensus_ops.Attestation,
                 None ),
               slot )
          | _ ->*)
    None

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
              metadata.protocol_data.level.cycle;
          cycle_position = metadata.protocol_data.level.cycle_position;
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

  let baking_rights cctxt level priority =
    let* baking_rights =
      Protocol.Delegate_services.Baking_rights.get
        ?levels:None
        ~max_priority:priority
        ~all:true
        cctxt
        (cctxt#chain, `Level (Int32.pred level))
    in
    match
      List.rev_map
        (fun {Protocol.Delegate_services.Baking_rights.delegate; priority; _} ->
          {
            Data.delegate = public_key_hash_of_v0 delegate;
            round = Int32.of_int priority;
          })
        baking_rights
    with
    | [] -> fail_with_exn Not_found
    | hd :: _ as baking_rights ->
        assert (Int32.of_int priority = hd.round) ;
        return (hd.delegate, baking_rights)

  let block_round header =
    match
      Data_encoding.Binary.of_bytes
        Protocol.block_header_data_encoding
        header.Block_header.protocol_data
    with
    | Ok data -> Ok data.contents.priority
    | Error err -> Error [Tezos_base.Data_encoding_wrapper.Decoding_error err]

  let consensus_ops_info_of_block cctxt hash =
    let* ops =
      Block_services.Operations.operations_in_pass
        cctxt
        ~chain:cctxt#chain
        ~block:(`Hash (hash, 0))
        0
    in
    let attestations =
      List.filter_map
        (fun Block_services.{hash; receipt; _} ->
          match receipt with
          | Receipt
              (Protocol.Apply_results.Operation_metadata
                 {
                   contents =
                     Single_result
                       (Tezos_raw_protocol_002_PsYLVpVv.Apply_results
                        .Endorsement_result
                          {delegate; slots; _});
                 }) ->
              Some
                Consensus_ops.
                  {
                    op = {hash; round = None; kind = Consensus_ops.Attestation};
                    delegate = public_key_hash_of_v0 delegate;
                    power = List.length slots;
                  }
          | _ -> None)
        ops
    in
    return attestations

  let get_block_info cctxt level =
    let* header =
      Block_services.header ~chain:cctxt#chain ~block:(`Level level) cctxt ()
    in
    let* metadata =
      Block_services.metadata ~chain:cctxt#chain ~block:(`Level level) cctxt ()
    in
    let* cycle_info = cycle_info metadata cctxt (cctxt#chain, `Level level) in
    return
      ( ( public_key_hash_of_v0 metadata.protocol_data.baker,
          header.shell.timestamp,
          header.protocol_data.contents.priority,
          header.hash,
          Some header.shell.predecessor ),
        cycle_info )
end

module M = General_archiver.Define (Services)
