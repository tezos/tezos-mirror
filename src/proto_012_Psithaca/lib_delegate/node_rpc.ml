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
module Block_services = Block_services.Make (Protocol) (Protocol)
module Events = Baking_events.Node_rpc

let inject_block cctxt ?(force = false) ~chain signed_block_header operations =
  let signed_shell_header_bytes =
    Data_encoding.Binary.to_bytes_exn Block_header.encoding signed_block_header
  in
  Shell_services.Injection.block
    cctxt
    ~chain
    ~force
    signed_shell_header_bytes
    operations

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
  | h :: _ as l ->
      let ({protocol_data = {contents = Single (Preendorsement content); _}; _})
          =
        (h : Kind.preendorsement Operation.t)
      in
      Some
        {
          Baking_state.level = Raw_level.to_int32 content.level;
          round = content.round;
          block_payload_hash = content.block_payload_hash;
          preendorsements = l;
        }
  | _ -> None

let raw_info cctxt ~chain ~block_hash shell payload_hash payload_round
    current_protocol next_protocol live_blocks =
  Events.(emit raw_info (block_hash, shell.Tezos_base.Block_header.level))
  >>= fun () ->
  let open Protocol_client_context in
  let block = `Hash (block_hash, 0) in
  let is_in_protocol = Protocol_hash.(current_protocol = Protocol.hash) in
  (if is_in_protocol then
   Alpha_block_services.Operations.operations cctxt ~chain ~block ()
   >>=? fun operations ->
   let operations =
     List.map
       (fun l ->
         List.map
           (fun {Alpha_block_services.shell; protocol_data; _} ->
             {Alpha_context.shell; protocol_data})
           l)
       operations
   in
   match Operation_pool.extract_operations_of_list_list operations with
   | None -> failwith "Unexpected operation list size"
   | Some operations -> return operations
  else
    (* If we are not in the current protocol, do no consider operations *)
    return (None, [], Operation_pool.empty_payload))
  >>=? fun (preendorsements, quorum, payload) ->
  (if is_in_protocol then Baking_state.round_of_shell_header shell
  else (* If we are not in the current protocol, the round is 0 *)
    ok Round.zero)
  >>?= fun round ->
  let prequorum =
    Option.fold ~none:None ~some:extract_prequorum preendorsements
  in
  return
    {
      Baking_state.hash = block_hash;
      shell;
      payload_hash;
      payload_round;
      round;
      protocol = current_protocol;
      next_protocol;
      prequorum;
      quorum;
      payload;
      live_blocks;
    }

let dummy_payload_hash = Block_payload_hash.zero

let info cctxt ~chain ~block () =
  let open Protocol_client_context in
  (* Fails if the block's protocol is not the current one *)
  Shell_services.Blocks.protocols cctxt ~chain ~block ()
  >>=? fun {current_protocol; next_protocol} ->
  (if Protocol_hash.(current_protocol <> Protocol.hash) then
   Block_services.Header.shell_header cctxt ~chain ~block () >>=? fun shell ->
   Chain_services.Blocks.Header.raw_protocol_data cctxt ~chain ~block ()
   >>=? fun protocol_data ->
   let hash =
     Tezos_base.Block_header.hash {Tezos_base.Block_header.shell; protocol_data}
   in
   let payload_hash =
     (* If the protocol is not the same, then we won't need to use
        the payload_hash *)
     dummy_payload_hash
   in
   return (hash, shell, payload_hash, Round.zero)
  else
    Alpha_block_services.header cctxt ~chain ~block ()
    >>=? fun {hash; shell; protocol_data; _} ->
    return
      ( hash,
        shell,
        protocol_data.contents.payload_hash,
        protocol_data.contents.payload_round ))
  >>=? fun (hash, shell, payload_hash, payload_round) ->
  (Chain_services.Blocks.live_blocks cctxt ~chain ~block () >>= function
   | Error _ ->
       (* The RPC might fail when a block's metadata is not available *)
       Lwt.return Block_hash.Set.empty
   | Ok live_blocks -> Lwt.return live_blocks)
  >>= fun live_blocks ->
  raw_info
    cctxt
    ~chain
    ~block_hash:hash
    shell
    payload_hash
    payload_round
    current_protocol
    next_protocol
    live_blocks

let find_in_cache_or_fetch cctxt ?cache ~chain block_hash =
  let open Baking_cache in
  let fetch () = info cctxt ~chain ~block:(`Hash (block_hash, 0)) () in
  match cache with
  | None -> fetch ()
  | Some block_cache -> (
      match Block_cache.find_opt block_cache block_hash with
      | Some block_info -> return block_info
      | None ->
          fetch () >>=? fun block_info ->
          Block_cache.replace block_cache block_hash block_info ;
          return block_info)

let proposal cctxt ?cache ~chain block_hash =
  find_in_cache_or_fetch cctxt ~chain ?cache block_hash >>=? fun block ->
  let predecessor_hash = block.shell.predecessor in
  find_in_cache_or_fetch cctxt ~chain ?cache predecessor_hash
  >>=? fun predecessor -> return {Baking_state.block; predecessor}

let monitor_proposals cctxt ~chain () =
  let cache = Baking_cache.Block_cache.create 100 in
  Monitor_services.heads cctxt ~next_protocols:[Protocol.hash] chain
  >>=? fun (block_stream, stopper) ->
  return
    ( Lwt_stream.filter_map_s
        (fun (block_hash, _) ->
          protect (fun () -> proposal cctxt ~cache ~chain block_hash)
          >>= function
          | Ok proposal -> Lwt.return_some proposal
          | Error err ->
              Events.(emit error_while_monitoring_heads err) >>= fun () ->
              Lwt.return_none)
        block_stream,
      stopper )

let await_protocol_activation cctxt ~chain () =
  Monitor_services.heads cctxt ~next_protocols:[Protocol.hash] chain
  >>=? fun (_block_stream, stop) ->
  stop () ;
  return_unit
