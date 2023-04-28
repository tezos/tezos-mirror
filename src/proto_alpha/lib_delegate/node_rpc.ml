(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Alpha_context
open Baking_cache
open Baking_state
module Block_services = Block_services.Make (Protocol) (Protocol)
module Events = Baking_events.Node_rpc

let inject_block cctxt ?(force = false) ~chain signed_block_header operations =
  let signed_shell_header_bytes =
    Data_encoding.Binary.to_bytes_exn Block_header.encoding signed_block_header
  in
  Shell_services.Injection.block
    ~async:true
    cctxt
    ~chain
    ~force
    signed_shell_header_bytes
    operations

let inject_operation cctxt ~chain operation =
  let encoded_op =
    Data_encoding.Binary.to_bytes_exn
      Operation.encoding_with_legacy_attestation_name
      operation
  in
  Shell_services.Injection.operation cctxt ~async:true ~chain encoded_op

let preapply_block cctxt ~chain ~head ~timestamp ~protocol_data operations =
  Block_services.Helpers.Preapply.block
    cctxt
    ~chain
    ~timestamp
    ~block:(`Hash (head, 0))
    operations
    ~protocol_data

let extract_prequorum preendorsements =
  match preendorsements with
  | h :: _ ->
      let ({protocol_data = {contents = Single (Preendorsement content); _}; _})
          =
        (h : Kind.preendorsement Operation.t)
      in
      Some
        {
          level = Raw_level.to_int32 content.level;
          round = content.round;
          block_payload_hash = content.block_payload_hash;
          preendorsements;
        }
  | _ -> None

let info_of_header_and_ops ~in_protocol block_hash block_header operations =
  let open Result_syntax in
  let shell = block_header.Tezos_base.Block_header.shell in
  let dummy_payload_hash = Block_payload_hash.zero in
  let* round =
    Environment.wrap_tzresult @@ Fitness.round_from_raw shell.fitness
  in
  let payload_hash, payload_round, prequorum, quorum, dal_attestations, payload
      =
    if not in_protocol then
      (* The first block in the protocol is baked using the previous
         protocol, the encodings might change. The baker's logic is to
         consider final the first block of a new protocol and not
         endorse it. Therefore, we do not need to have the correct
         values here. *)
      ( dummy_payload_hash,
        Round.zero,
        None,
        [],
        [],
        Operation_pool.empty_payload )
    else
      let payload_hash, payload_round =
        match
          Data_encoding.Binary.of_bytes_opt
            Protocol.block_header_data_encoding
            block_header.protocol_data
        with
        | Some {contents = {payload_hash; payload_round; _}; _} ->
            (payload_hash, payload_round)
        | None -> assert false
      in
      let preendorsements, quorum, dal_attestations, payload =
        WithExceptions.Option.get
          ~loc:__LOC__
          (Operation_pool.extract_operations_of_list_list operations)
      in
      let prequorum = Option.bind preendorsements extract_prequorum in
      (payload_hash, payload_round, prequorum, quorum, dal_attestations, payload)
  in
  return
    {
      hash = block_hash;
      shell;
      payload_hash;
      payload_round;
      round;
      prequorum;
      quorum;
      dal_attestations;
      payload;
    }

let compute_block_info cctxt ~in_protocol ?operations ~chain block_hash
    block_header =
  let open Lwt_result_syntax in
  let* operations =
    match operations with
    | None when not in_protocol -> return_nil
    | None ->
        let open Protocol_client_context in
        let* operations =
          Alpha_block_services.Operations.operations
            cctxt
            ~chain
            ~block:(`Hash (block_hash, 0))
            ()
        in
        let packed_operations =
          List.map
            (fun l ->
              List.map
                (fun {Alpha_block_services.shell; protocol_data; _} ->
                  {Alpha_context.shell; protocol_data})
                l)
            operations
        in
        return packed_operations
    | Some operations ->
        let parse_op (raw_op : Tezos_base.Operation.t) =
          let protocol_data =
            Data_encoding.Binary.of_bytes_exn
              Operation.protocol_data_encoding_with_legacy_attestation_name
              raw_op.proto
          in
          {shell = raw_op.shell; protocol_data}
        in
        protect @@ fun () -> return (List.map (List.map parse_op) operations)
  in
  let*? block_info =
    info_of_header_and_ops ~in_protocol block_hash block_header operations
  in
  return block_info

let proposal cctxt ?(cache : block_info Block_cache.t option) ?operations ~chain
    block_hash (block_header : Tezos_base.Block_header.t) =
  let open Lwt_result_syntax in
  let predecessor_hash = block_header.shell.predecessor in
  let pred_block = `Hash (predecessor_hash, 0) in
  let predecessor_opt =
    Option.bind cache (fun cache -> Block_cache.find_opt cache predecessor_hash)
  in
  let* is_proposal_in_protocol, predecessor =
    match predecessor_opt with
    | Some predecessor ->
        return
          ( predecessor.shell.proto_level = block_header.shell.proto_level,
            predecessor )
    | None ->
        let* {
               current_protocol = pred_current_protocol;
               next_protocol = pred_next_protocol;
             } =
          Shell_services.Blocks.protocols cctxt ~chain ~block:pred_block ()
        in
        let is_proposal_in_protocol =
          Protocol_hash.(pred_next_protocol = Protocol.hash)
        in
        let* predecessor =
          let in_protocol =
            Protocol_hash.(pred_current_protocol = Protocol.hash)
          in
          let* raw_header_b =
            Shell_services.Blocks.raw_header cctxt ~chain ~block:pred_block ()
          in
          let predecessor_header =
            Data_encoding.Binary.of_bytes_exn
              Tezos_base.Block_header.encoding
              raw_header_b
          in
          compute_block_info
            cctxt
            ~in_protocol
            ~chain
            predecessor_hash
            predecessor_header
        in
        Option.iter
          (fun cache -> Block_cache.replace cache predecessor_hash predecessor)
          cache ;
        return (is_proposal_in_protocol, predecessor)
  in
  let block_opt =
    Option.bind cache (fun cache -> Block_cache.find_opt cache block_hash)
  in
  let* block =
    match block_opt with
    | Some pi -> return pi
    | None ->
        let* pi =
          compute_block_info
            cctxt
            ~in_protocol:is_proposal_in_protocol
            ?operations
            ~chain
            block_hash
            block_header
        in
        Option.iter (fun cache -> Block_cache.replace cache block_hash pi) cache ;
        return pi
  in
  return {block; predecessor}

let proposal cctxt ?cache ?operations ~chain block_hash block_header =
  protect @@ fun () ->
  proposal cctxt ?cache ?operations ~chain block_hash block_header

let monitor_valid_proposals cctxt ~chain ?cache () =
  let open Lwt_result_syntax in
  let next_protocols = [Protocol.hash] in
  let* block_stream, stopper =
    Monitor_services.validated_blocks cctxt ~chains:[chain] ~next_protocols ()
  in
  let stream =
    let map (_chain_id, block_hash, block_header, operations) =
      let*! map_result =
        proposal cctxt ?cache ~operations ~chain block_hash block_header
      in
      match map_result with
      | Ok proposal -> Lwt.return_some proposal
      | Error err ->
          let*! () = Events.(emit error_while_monitoring_valid_proposals err) in
          Lwt.return_none
    in
    Lwt_stream.filter_map_s map block_stream
  in
  return (stream, stopper)

let monitor_heads cctxt ~chain ?cache () =
  let open Lwt_result_syntax in
  let next_protocols = [Protocol.hash] in
  let* block_stream, stopper =
    Monitor_services.heads cctxt ~next_protocols chain
  in
  let stream =
    let map (block_hash, block_header) =
      let*! map_result = proposal cctxt ?cache ~chain block_hash block_header in
      match map_result with
      | Ok proposal -> Lwt.return_some proposal
      | Error err ->
          let*! () = Events.(emit error_while_monitoring_heads err) in
          Lwt.return_none
    in
    Lwt_stream.filter_map_s map block_stream
  in
  return (stream, stopper)

let await_protocol_activation cctxt ~chain () =
  Monitor_services.heads cctxt ~next_protocols:[Protocol.hash] chain
  >>=? fun (block_stream, stop) ->
  Lwt_stream.get block_stream >>= fun _ ->
  stop () ;
  return_unit

let get_attestable_slots dal_node_rpc_ctxt pkh ~level =
  Tezos_rpc.Context.make_call
    Tezos_dal_node_services.Services.get_attestable_slots
    dal_node_rpc_ctxt
    (((), pkh), level)
    ()
    ()
