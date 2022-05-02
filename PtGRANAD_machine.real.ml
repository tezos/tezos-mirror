(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
  Tezos_client_010_PtGRANAD.Protocol_client_context.Alpha_block_services

open Lwt_result_syntax
open Tezos_protocol_010_PtGRANAD

module Services : Protocol_machinery.PROTOCOL_SERVICES = struct
  let hash = Protocol.hash

  type wrap_full = Tezos_client_010_PtGRANAD.Protocol_client_context.wrap_full

  let wrap_full cctxt =
    new Tezos_client_010_PtGRANAD.Protocol_client_context.wrap_full cctxt

  type endorsing_right = Protocol.Delegate_services.Endorsing_rights.t

  type endorsing_rights = endorsing_right list

  let endorsing_rights cctxt block =
    Protocol.Delegate_services.Endorsing_rights.get
      cctxt
      (cctxt#chain, `Level block)

  type block_id = Block_hash.t

  module BlockIdMap = Block_hash.Map

  let couple_ops_to_rights ops rights =
    let (items, missing) =
      List.fold_left
        (fun (acc, rights) (op_kind, errors, delay, round, slot) ->
          match
            List.partition
              (fun right ->
                List.mem
                  ~equal:Int.equal
                  slot
                  right.Protocol.Delegate_services.Endorsing_rights.slots)
              rights
          with
          | (([] | _ :: _ :: _), _) -> assert false
          | ([right], rights') ->
              ( ( right.Protocol.Delegate_services.Endorsing_rights.delegate,
                  [(op_kind, round, errors, delay)] )
                :: acc,
                rights' ))
        ([], rights)
        ops
    in
    ( items,
      List.map
        (fun right ->
          right.Protocol.Delegate_services.Endorsing_rights.delegate)
        missing )

  let extract_endorsement
      (operation_content : Protocol.Alpha_context.packed_operation) =
    match operation_content with
    | {
     Protocol.Main.protocol_data =
       Protocol.Alpha_context.Operation_data
         {
           contents =
             Single
               (Endorsement_with_slot
                 {
                   endorsement =
                     {
                       shell = _;
                       protocol_data =
                         {
                           contents = Single (Endorsement {level});
                           signature = _;
                         };
                     };
                   slot;
                 });
           signature = _;
         };
     shell = {branch};
    } ->
        Some
          ( ( branch,
              Protocol.Alpha_context.Raw_level.to_int32 level,
              Operation_kind.Endorsement,
              None ),
            slot )
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
      Protocol.Delegate_services.Baking_rights.get
        ?levels:None
        ~max_priority:priority
        cctxt
        (cctxt#chain, `Hash (hash, 0))
    in
    match List.last_opt baking_rights with
    | None -> fail_with_exn Not_found
    | Some {delegate; priority = p; timestamp; _} ->
        let () = assert (Compare.Int.equal priority p) in
        return (delegate, timestamp)

  let block_round header =
    match
      Data_encoding.Binary.of_bytes
        Protocol.block_header_data_encoding
        header.Block_header.protocol_data
    with
    | Ok data -> Ok data.contents.priority
    | Error err -> Error [Tezos_base.Data_encoding_wrapper.Decoding_error err]

  let endorsements_info_of_block cctxt hash =
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
                      (Protocol.Apply_results.Endorsement_with_slot_result
                        (Tezos_raw_protocol_010_PtGRANAD.Apply_results
                         .Endorsement_result {delegate; _}));
                }) ->
              Some delegate
          | _ -> None)
        ops
    in
    return Protocol_machinery.{endorsers = pks; round = None}
end

include Protocol_machinery.Make (Services)
