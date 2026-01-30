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
open Baking_state_types
module Block_services = Block_services.Make (Protocol) (Protocol)
module Events = Node_rpc_events
module Profiler = Baking_profiler.Node_rpc_Profiler
module RPC_profiler = Baking_profiler.RPC_profiler

let warn_on_stalling_rpc ~rpc_name f =
  Utils.event_on_stalling_promise
    ~event:(fun sum -> Node_rpc_events.(emit stalling_rpc (rpc_name, sum)))
    f

let inject_block cctxt ?(force = false) ~chain signed_block_header operations =
  let signed_shell_header_bytes =
    Data_encoding.Binary.to_bytes_exn Block_header.encoding signed_block_header
  in
  warn_on_stalling_rpc ~rpc_name:"inject_block"
  @@ Shell_services.Injection.block
       ~async:true
       cctxt
       ~chain
       ~force
       signed_shell_header_bytes
       operations

let inject_operation cctxt ~chain operation =
  let encoded_op =
    Data_encoding.Binary.to_bytes_exn Operation.encoding operation
  in
  warn_on_stalling_rpc ~rpc_name:"inject_operation"
  @@ Shell_services.Injection.operation cctxt ~async:true ~chain encoded_op

let preapply_block cctxt ~chain ~head ~timestamp ~protocol_data operations =
  warn_on_stalling_rpc ~rpc_name:"preapply_block"
  @@ Block_services.Helpers.Preapply.block
       cctxt
       ~chain
       ~timestamp
       ~block:(`Hash (head, 0))
       operations
       ~protocol_data

let extract_prequorum (preattestations : packed_operation list) =
  match preattestations with
  | packed_op :: _ -> (
      let (Operation_data protocol_data) = packed_op.protocol_data in
      match protocol_data.contents with
      | Single (Preattestation content) ->
          Some
            {
              level = Raw_level.to_int32 content.level;
              round = content.round;
              block_payload_hash = content.block_payload_hash;
              preattestations;
            }
      | Single (Preattestations_aggregate {consensus_content; _}) ->
          Some
            {
              level = Raw_level.to_int32 consensus_content.level;
              round = consensus_content.round;
              block_payload_hash = consensus_content.block_payload_hash;
              preattestations;
            }
      | _ -> None)
  | _ -> None

let info_of_header_and_ops ~in_protocol ~grandparent block_hash block_header
    operations =
  let open Result_syntax in
  let shell = block_header.Tezos_base.Block_header.shell in
  let dummy_payload_hash = Block_payload_hash.zero in
  let* round =
    Environment.wrap_tzresult @@ Fitness.round_from_raw shell.fitness
  in
  let payload_hash, payload_round, prequorum, quorum, payload =
    if not in_protocol then
      (* The first block in the protocol is baked using the previous
         protocol, the encodings might change. The baker's logic is to
         consider final the first block of a new protocol and not
         attest it. Therefore, we do not need to have the correct
         values here. *)
      (dummy_payload_hash, Round.zero, None, [], Operation_pool.empty_payload)
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
      let preattestations, quorum, payload =
        (WithExceptions.Option.get
           ~loc:__LOC__
           (Operation_pool.extract_operations_of_list_list operations)
         [@profiler.record_f {verbosity = Debug} "operations classification"])
      in
      let prequorum = Option.bind preattestations extract_prequorum in
      (payload_hash, payload_round, prequorum, quorum, payload)
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
      payload;
      grandparent;
    }

let compute_block_info cctxt ~in_protocol ?operations ~chain block_hash
    ~grandparent block_header =
  let open Lwt_result_syntax in
  ((let* operations =
      match operations with
      | None when not in_protocol -> return_nil
      | None ->
          let open Protocol_client_context in
          ((let* operations =
              warn_on_stalling_rpc ~rpc_name:"operations"
              @@ Alpha_block_services.Operations.operations
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
            return packed_operations)
          [@profiler.record_s
            {verbosity = Debug}
              ("retrieve block "
              ^ Block_hash.to_short_b58check block_hash
              ^ " operations")])
      | Some operations ->
          let parse_op (raw_op : Tezos_base.Operation.t) =
            let protocol_data =
              (Data_encoding.Binary.of_bytes_exn
                 Operation.protocol_data_encoding
                 raw_op.proto
               [@profiler.aggregate_f {verbosity = Debug} "parse operation"])
            in
            {shell = raw_op.shell; protocol_data}
          in
          protect @@ fun () ->
          return
            (List.mapi
               (fun [@warning "-27"] i -> function
                 | [] -> []
                 | l ->
                     List.map
                       parse_op
                       l
                     [@profiler.record_f
                       {verbosity = Debug}
                         (Printf.sprintf "parse operations (pass : %d)" i)])
               operations)
    in
    let*? block_info =
      info_of_header_and_ops
        ~in_protocol
        ~grandparent
        block_hash
        block_header
        operations
    in
    return block_info)
  [@profiler.record_s
    {verbosity = Info}
      ("compute block " ^ Block_hash.to_short_b58check block_hash ^ " info")])

let protocols cctxt ~chain ?(block = `Head 0) () =
  warn_on_stalling_rpc ~rpc_name:"protocols"
  @@ Shell_services.Blocks.protocols cctxt ~chain ~block ()

let raw_header cctxt ~chain ?(block = `Head 0) () =
  warn_on_stalling_rpc ~rpc_name:"raw_header"
  @@ Shell_services.Blocks.raw_header cctxt ~chain ~block ()

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
        ()
        [@profiler.mark
          {verbosity = Info}
            [
              "pred_block("
              ^ Block_hash.to_short_b58check predecessor_hash
              ^ ") : cache_hit";
            ]] ;
        return
          ( predecessor.shell.proto_level = block_header.shell.proto_level,
            predecessor )
    | None ->
        ()
        [@profiler.mark
          {verbosity = Info}
            [
              "pred_block("
              ^ Block_hash.to_short_b58check predecessor_hash
              ^ ") : cache_miss";
            ]] ;
        let* {
               current_protocol = pred_current_protocol;
               next_protocol = pred_next_protocol;
             } =
          (protocols
             cctxt
             ~chain
             ~block:pred_block
             ()
           [@profiler.record_s {verbosity = Info} "pred block protocol RPC"])
        in
        let is_proposal_in_protocol =
          Protocol_hash.(pred_next_protocol = Protocol.hash)
        in
        let* predecessor =
          let in_protocol =
            Protocol_hash.(pred_current_protocol = Protocol.hash)
          in
          let* raw_header_b = raw_header cctxt ~chain ~block:pred_block () in
          let predecessor_header =
            (Data_encoding.Binary.of_bytes_exn
               Tezos_base.Block_header.encoding
               raw_header_b
             [@profiler.record_f {verbosity = Info} "parse pred block header"])
          in
          let* grandparent =
            let* raw_header =
              raw_header
                cctxt
                ~chain
                ~block:(`Hash (predecessor_header.shell.predecessor, 0))
                ()
            in
            let header =
              (Data_encoding.Binary.of_bytes_exn
                 Tezos_base.Block_header.encoding
                 raw_header
               [@profiler.record_f
                 {verbosity = Info} "parse grandparent block header"])
            in
            return header.shell.predecessor
          in
          compute_block_info
            cctxt
            ~in_protocol
            ~chain
            ~grandparent
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
    | Some pi ->
        ()
        [@profiler.mark
          {verbosity = Info}
            [
              "new_block("
              ^ Block_hash.to_short_b58check pi.hash
              ^ ") : cache_hit";
            ]] ;
        return pi
    | None ->
        ()
        [@profiler.mark
          {verbosity = Info}
            [
              "new_block("
              ^ Block_hash.to_short_b58check block_hash
              ^ ") : cache_miss";
            ]] ;
        let* pi =
          compute_block_info
            cctxt
            ~in_protocol:is_proposal_in_protocol
            ?operations
            ~chain
            ~grandparent:predecessor.shell.predecessor
            block_hash
            block_header
        in
        Option.iter (fun cache -> Block_cache.replace cache block_hash pi) cache ;
        return pi
  in
  return {block; predecessor}

let proposal cctxt ?cache ?operations ~chain block_hash block_header =
  (protect @@ fun () ->
  proposal cctxt ?cache ?operations ~chain block_hash block_header)
  [@profiler.record_s {verbosity = Notice} "proposal_computation"]

let monitor_valid_proposals cctxt ~chain ?cache () =
  let open Lwt_result_syntax in
  let next_protocols = [Protocol.hash] in
  let* block_stream, stopper =
    Monitor_services.validated_blocks cctxt ~chains:[chain] ~next_protocols ()
  in
  let stream =
    let map (_chain_id, block_hash, block_header, operations) =
      () [@profiler.overwrite Profiler.reset_block_section (block_hash, [])] ;
      () [@profiler.overwrite RPC_profiler.reset_block_section (block_hash, [])] ;
      (let*! map_result =
         proposal cctxt ?cache ~operations ~chain block_hash block_header
       in
       match map_result with
       | Ok proposal -> Lwt.return_some proposal
       | Error err ->
           let*! () =
             Events.(emit error_while_monitoring_valid_proposals err)
           in
           Lwt.return_none)
      [@profiler.record_s {verbosity = Notice} "received valid proposal"]
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
      () [@profiler.overwrite Profiler.reset_block_section (block_hash, [])] ;
      (let*! map_result =
         proposal cctxt ?cache ~chain block_hash block_header
       in
       match map_result with
       | Ok proposal -> Lwt.return_some proposal
       | Error err ->
           let*! () = Events.(emit error_while_monitoring_heads err) in
           Lwt.return_none)
      [@profiler.record_s {verbosity = Notice} "received new head"]
    in
    Lwt_stream.filter_map_s map block_stream
  in
  return (stream, stopper)

let get_validators cctxt ~chain ?(block = `Head 0) ?(levels = []) ?delegates
    ?consensus_keys () =
  let open Lwt_result_syntax in
  let*? levels =
    List.map_e
      (fun level -> Environment.wrap_tzresult (Raw_level.of_int32 level))
      levels
  in
  warn_on_stalling_rpc ~rpc_name:"get_validators"
  @@ (Plugin.RPC.Validators.get
        cctxt
        (chain, block)
        ~levels
        ?delegates
        ?consensus_keys
      [@profiler.record_s {verbosity = Debug} "RPC: get attesting rights"])

let current_level cctxt ~chain ?(block = `Head 0) ?offset () =
  warn_on_stalling_rpc ~rpc_name:"current_level"
  @@ Plugin.RPC.current_level cctxt ?offset (chain, block)

let forge_seed_nonce_revelation cctxt ~chain ?(block = `Head 0) ~branch ~level
    ~nonce () =
  warn_on_stalling_rpc ~rpc_name:"forge_seed_nonce_revelation"
  @@ Plugin.RPC.Forge.seed_nonce_revelation
       cctxt
       (chain, block)
       ~branch
       ~level
       ~nonce
       ()

let forge_vdf_revelation cctxt ~chain ~block ~branch ~solution =
  warn_on_stalling_rpc ~rpc_name:"forge_vdg_revelation"
  @@ Plugin.RPC.Forge.vdf_revelation cctxt (chain, block) ~branch ~solution ()

let levels_in_current_cycle cctxt ~offset ~chain ~block =
  warn_on_stalling_rpc ~rpc_name:"levels_in_current_cycle"
  @@ Plugin.RPC.levels_in_current_cycle cctxt ~offset (chain, block)

let forge_double_consensus_operation_evidence cctxt ~chain ~block ~branch ~slot
    ~op1 ~op2 =
  warn_on_stalling_rpc ~rpc_name:"forge_double_consensus_operation_evidence"
  @@ Plugin.RPC.Forge.double_consensus_operation_evidence
       cctxt
       (chain, block)
       ~branch
       ~slot
       ~op1
       ~op2
       ()

let forge_double_baking_evidence cctxt ~chain ~block ~branch ~bh1 ~bh2 =
  warn_on_stalling_rpc ~rpc_name:"forge_double_baking_evidence"
  @@ Plugin.RPC.Forge.double_baking_evidence
       cctxt
       (chain, block)
       ~branch
       ~bh1
       ~bh2
       ()

let await_protocol_activation cctxt ~chain () =
  let open Lwt_result_syntax in
  let* block_stream, stop =
    Monitor_services.heads cctxt ~next_protocols:[Protocol.hash] chain
  in
  let*! _ = Lwt_stream.get block_stream in
  stop () ;
  return_unit

let fetch_dal_config cctxt =
  let open Lwt_syntax in
  let* r =
    warn_on_stalling_rpc ~rpc_name:"fetch_dal_config"
    @@ Config_services.dal_config cctxt
  in
  match r with
  | Error e -> return_error e
  | Ok dal_config -> return_ok dal_config

let monitor_attestable_slots (dal_node_rpc_ctxt : Tezos_rpc.Context.generic)
    ~delegate_id =
  let open Lwt_syntax in
  let pkh = Delegate_id.to_pkh delegate_id in
  let* result =
    Tezos_rpc.Context.make_streamed_call
      Tezos_dal_node_services.Services.monitor_attestable_slots
      dal_node_rpc_ctxt
      ((), Tezos_crypto.Signature.Of_V3.public_key_hash pkh)
      ()
      ()
  in
  match result with
  | Ok result -> return_ok result
  | Error trace -> return_error trace

let get_dal_profiles dal_node_rpc_ctxt =
  warn_on_stalling_rpc ~rpc_name:"get_dal_profiles"
  @@ Tezos_rpc.Context.make_call
       Tezos_dal_node_services.Services.get_profiles
       dal_node_rpc_ctxt
       ()
       ()
       ()

let register_dal_profiles dal_node_rpc_ctxt delegates =
  warn_on_stalling_rpc ~rpc_name:"register_dal_profiles"
  @@
  let profiles =
    Tezos_dal_node_services.Controller_profiles.make
      ~attesters:
        (List.map
           (fun pkh -> Tezos_crypto.Signature.Of_V3.public_key_hash pkh)
           delegates)
      ()
  in
  Tezos_rpc.Context.make_call
    Tezos_dal_node_services.Services.patch_profiles
    dal_node_rpc_ctxt
    ()
    ()
    profiles

let get_dal_health dal_node_rpc_ctxt =
  warn_on_stalling_rpc ~rpc_name:"get_dal_health"
  @@ Tezos_rpc.Context.make_call
       Tezos_dal_node_services.Services.health
       dal_node_rpc_ctxt
       ()
       ()
       ()

let get_nonce cctxt ~chain ?(block = `Head 0) ~level () =
  warn_on_stalling_rpc ~rpc_name:"get_nonce"
  @@ Alpha_services.Nonce.get cctxt (chain, block) level

let delegate_deactivated cctxt ~chain ?(block = `Head 0) pkh =
  warn_on_stalling_rpc ~rpc_name:"delegate_deactivated"
  @@ Plugin.Alpha_services.Delegate.deactivated cctxt (chain, block) pkh

let constants cctxt ~chain ~block =
  warn_on_stalling_rpc ~rpc_name:"constants"
  @@ Plugin.Alpha_services.Constants.all cctxt (chain, block)

let seed_computation cctxt ~chain ~block =
  warn_on_stalling_rpc ~rpc_name:"seed_computation"
  @@ Alpha_services.Seed_computation.get cctxt (chain, block)

let chain_id cctxt ~chain =
  warn_on_stalling_rpc ~rpc_name:"chain_id"
  @@ Shell_services.Chain.chain_id cctxt ~chain ()

let shell_header cctxt ~chain ?(block = `Head 0) () =
  warn_on_stalling_rpc ~rpc_name:"shell_header"
  @@ Shell_services.Blocks.Header.shell_header cctxt ~chain ~block ()

let block_hash cctxt ~chain ~block =
  warn_on_stalling_rpc ~rpc_name:"block_hash"
  @@ Shell_services.Blocks.hash cctxt ~chain ~block ()

let blocks cctxt ~chain ~heads ~length =
  warn_on_stalling_rpc ~rpc_name:"blocks"
  @@ Shell_services.Blocks.list cctxt ~chain ~heads ~length ()

let inject_private_operation_bytes cctxt ~chain bytes =
  warn_on_stalling_rpc ~rpc_name:"inject_private_operation"
  @@ Shell_services.Injection.private_operation cctxt ~chain bytes

let inject_operation_bytes cctxt ?async ~chain bytes =
  warn_on_stalling_rpc ~rpc_name:"inject_operation"
  @@ Shell_services.Injection.operation cctxt ?async ~chain bytes

let block_resulting_context_hash cctxt ~chain ?(block = `Head 0) () =
  warn_on_stalling_rpc ~rpc_name:"resulting_context_hash"
  @@ Shell_services.Blocks.resulting_context_hash cctxt ~chain ~block ()

let live_blocks cctxt ~chain ?(block = `Head 0) () =
  warn_on_stalling_rpc ~rpc_name:"live_blocks"
  @@ Chain_services.Blocks.live_blocks cctxt ~chain ~block ()

let block_header cctxt ~chain ~block =
  warn_on_stalling_rpc ~rpc_name:"block_header"
  @@ Protocol_client_context.Alpha_block_services.header cctxt ~chain ~block ()

let block_info cctxt ~chain ~block =
  warn_on_stalling_rpc ~rpc_name:"block_info"
  @@ Protocol_client_context.Alpha_block_services.info cctxt ~chain ~block ()

let block_metadata cctxt ~chain ~block =
  warn_on_stalling_rpc ~rpc_name:"block_metadata"
  @@ Protocol_client_context.Alpha_block_services.metadata
       cctxt
       ~chain
       ~block
       ()

let mempool_monitor_operations cctxt ~chain =
  Protocol_client_context.Alpha_block_services.Mempool.monitor_operations
    cctxt
    ~chain
    ~validated:true
    ~branch_delayed:true
    ~branch_refused:false
    ~refused:false
    ~outdated:false
    ()

let user_activated_upgrades cctxt =
  warn_on_stalling_rpc ~rpc_name:"user_activated_upgrades"
  @@ Config_services.user_activated_upgrades cctxt
