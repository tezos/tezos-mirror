(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Block_services =
  Tezos_client_016_PtMumbai.Protocol_client_context.Alpha_block_services

open Lwt_result_syntax
open Tezos_protocol_016_PtMumbai
open Tezos_protocol_plugin_016_PtMumbai

module Services : Protocol_machinery.PROTOCOL_SERVICES = struct
  let hash = Protocol.hash

  type wrap_full = Tezos_client_016_PtMumbai.Protocol_client_context.wrap_full

  let wrap_full cctxt =
    new Tezos_client_016_PtMumbai.Protocol_client_context.wrap_full cctxt

  let slot_to_int x =
    (* YES this is Fun.x ! *)
    Data_encoding.Binary.of_bytes_exn
      Data_encoding.uint16
      (Data_encoding.Binary.to_bytes_exn Protocol.Alpha_context.Slot.encoding x)

  let endorsing_rights cctxt ~reference_level level =
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
                    {delegate; first_slot; endorsing_power; _} ->
               Consensus_ops.
                 {
                   address = delegate;
                   first_slot = slot_to_int first_slot;
                   power = endorsing_power;
                 })
             answer.Plugin.RPC.Endorsing_rights.delegates_rights)
    | [] -> return_nil

  type block_id = Protocol.Block_payload_hash.t

  module BlockIdMap = Map.Make (Protocol.Block_payload_hash)

  let extract_endorsement
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
              Consensus_ops.Endorsement,
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
              Consensus_ops.Preendorsement,
              Some (Protocol.Alpha_context.Round.to_int32 round) ),
            slot_to_int slot )
    | _ -> None

  let consensus_operation_stream cctxt =
    let* ops_stream, stopper =
      Block_services.Mempool.monitor_operations
        cctxt
        ~chain:cctxt#chain
        ~applied:true
        ~refused:false
        ~branch_delayed:true
        ~branch_refused:true
        ()
    in
    let op_stream = Lwt_stream.flatten ops_stream in
    return
      ( Lwt_stream.filter_map
          (fun ((hash, op), errors) ->
            Option.map (fun x -> ((hash, x), errors)) (extract_endorsement op))
          op_stream,
        stopper )

  let baker cctxt hash =
    let* metadata =
      Block_services.metadata
        ~chain:cctxt#chain
        ~block:(`Hash (hash, 0))
        cctxt
        ()
    in
    return metadata.protocol_data.baker.delegate

  let baking_right cctxt level round =
    let* baking_rights =
      Plugin.RPC.Baking_rights.get
        ?levels:None
        ~max_round:round
        ~all:true
        cctxt
        (cctxt#chain, `Level (Int32.pred level))
    in
    match List.last_opt baking_rights with
    | None -> fail_with_exn Not_found
    | Some {delegate; round = r; _} ->
        assert (Int32.of_int round = Protocol.Alpha_context.Round.to_int32 r) ;
        return delegate

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

  let get_endorsement_round protocol_data =
    match protocol_data with
    | Protocol.Alpha_context.Operation_data {contents; _} -> (
        match contents with
        | Single (Endorsement {round; _}) ->
            Protocol.Alpha_context.Round.to_int32 round
        | _ -> assert false)

  let get_preendorsement_round protocol_data =
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
                    round = Some (get_preendorsement_round protocol_data);
                    kind = Consensus_ops.Preendorsement;
                  };
                delegate;
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
                    round = Some (get_endorsement_round protocol_data);
                    kind = Consensus_ops.Endorsement;
                  };
                delegate;
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
    return
      ( metadata.protocol_data.baker.delegate,
        header.shell.timestamp,
        round,
        header.hash,
        Some header.shell.predecessor )
end

module M = General_archiver.Define (Services)
