(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
  Tezos_client_012_Psithaca.Protocol_client_context.Alpha_block_services

open Lwt_result_syntax
open Tezos_protocol_012_Psithaca
open Tezos_protocol_plugin_012_Psithaca

module Services : Protocol_machinery.PROTOCOL_SERVICES = struct
  let hash = Protocol.hash

  type wrap_full = Tezos_client_012_Psithaca.Protocol_client_context.wrap_full

  let wrap_full cctxt =
    new Tezos_client_012_Psithaca.Protocol_client_context.wrap_full cctxt

  type endorsing_rights = Plugin.RPC.Endorsing_rights.delegate_rights list

  let endorsing_rights cctxt level =
    let* answers =
      Plugin.RPC.Endorsing_rights.get cctxt (cctxt#chain, `Level level)
    in
    match answers with
    | answer :: _ -> return answer.Plugin.RPC.Endorsing_rights.delegates_rights
    | [] -> return_nil

  type block_id = Protocol.Block_payload_hash.t

  module BlockIdMap = Map.Make (Protocol.Block_payload_hash)

  let slot_to_int x =
    (* YES this is Fun.x ! *)
    Data_encoding.Binary.of_bytes_exn
      Data_encoding.uint16
      (Data_encoding.Binary.to_bytes_exn Protocol.Alpha_context.Slot.encoding x)

  let same_slot slot right =
    Int.equal slot (slot_to_int right.Plugin.RPC.Endorsing_rights.first_slot)

  let couple_ops_to_rights ops rights =
    let (items, missing) =
      List.fold_left
        (fun (acc, remaining_rights) (errors, delay, round, slot) ->
          match
            List.partition (fun right -> same_slot slot right) remaining_rights
          with
          | (_ :: _ :: _, _) -> assert false
          | ([right], rights') ->
              ( ( right.Plugin.RPC.Endorsing_rights.delegate,
                  [(round, errors, delay)] )
                :: acc,
                rights' )
          | ([], _) -> (
              match List.find (fun right -> same_slot slot right) rights with
              | None -> assert false
              | Some right ->
                  ( ( right.Plugin.RPC.Endorsing_rights.delegate,
                      [(round, errors, delay)] )
                    :: acc,
                    remaining_rights )))
        ([], rights)
        ops
    in
    ( items,
      List.map (fun right -> right.Plugin.RPC.Endorsing_rights.delegate) missing
    )

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
              Some (Protocol.Alpha_context.Round.to_int32 round) ),
            slot_to_int slot )
    | _ -> None

  let consensus_operation_stream cctxt =
    let* (ops_stream, stopper) =
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

  let baking_right cctxt hash priority =
    let* baking_rights =
      Plugin.RPC.Baking_rights.get
        ?levels:None
        ~max_round:priority
        cctxt
        (cctxt#chain, `Hash (hash, 0))
    in
    match List.last_opt baking_rights with
    | None -> fail_with_exn Not_found
    | Some {delegate; round = p; timestamp; _} ->
        let () = assert (Compare.Int.equal priority p) in
        return (delegate, timestamp)

  let block_round (header : Block_header.t) =
    let wrap = Protocol.Environment.wrap_tzresult in
    let open Result_syntax in
    let* round =
      wrap
      @@ Protocol.Alpha_context.Fitness.round_from_raw
           header.Block_header.shell.fitness
    in
    wrap @@ Protocol.Alpha_context.Round.to_int round

  let consensus_op_participants_of_block cctxt hash =
    let* ops =
      Block_services.Operations.operations_in_pass
        cctxt
        ~chain:cctxt#chain
        ~block:(`Hash (hash, 0))
        0
    in
    let pks =
      List.filter_map
        (fun Block_services.{receipt; _} ->
          match receipt with
          | Receipt
              (Protocol.Apply_results.Operation_metadata
                {
                  contents =
                    Single_result
                      (Protocol.Apply_results.Endorsement_result {delegate; _});
                }) ->
              Some delegate
          | _ -> None)
        ops
    in
    return pks
end

include Protocol_machinery.Make (Services)
