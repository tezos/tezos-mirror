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

  let endorsing_rights cctxt ~reference_level level =
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
         (fun Protocol.Delegate_services.Endorsing_rights.{delegate; slots; _} ->
           Consensus_ops.
             {
               address = public_key_hash_of_v0 delegate;
               first_slot = Stdlib.List.hd (List.sort compare slots);
               power = List.length slots;
             })
         rights

  type block_id = Block_hash.t

  module BlockIdMap = Block_hash.Map

  let extract_endorsement
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
                 Consensus_ops.Endorsement,
                 None ),
               slot )
          | _ ->*)
    None

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
    return (public_key_hash_of_v0 metadata.protocol_data.baker)

  let baking_right cctxt level priority =
    let* baking_rights =
      Protocol.Delegate_services.Baking_rights.get
        ?levels:None
        ~max_priority:priority
        ~all:true
        cctxt
        (cctxt#chain, `Level (Int32.pred level))
    in
    match List.last_opt baking_rights with
    | None -> fail_with_exn Not_found
    | Some {delegate; priority = p; _} ->
        assert (Compare.Int.equal priority p) ;
        return (public_key_hash_of_v0 delegate)

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
    let endorsements =
      List.filter_map
        (fun Block_services.{hash; receipt; _} ->
          match receipt with
          | Receipt
              (Protocol.Apply_results.Operation_metadata
                {
                  contents =
                    Single_result
                      (Tezos_raw_protocol_002_PsYLVpVv.Apply_results
                       .Endorsement_result {delegate; slots; _});
                }) ->
              Some
                Consensus_ops.
                  {
                    op = {hash; round = None; kind = Consensus_ops.Endorsement};
                    delegate = public_key_hash_of_v0 delegate;
                    power = List.length slots;
                  }
          | _ -> None)
        ops
    in
    return endorsements

  let get_block_info cctxt level =
    let* header =
      Block_services.header ~chain:cctxt#chain ~block:(`Level level) cctxt ()
    in
    let* metadata =
      Block_services.metadata ~chain:cctxt#chain ~block:(`Level level) cctxt ()
    in
    return
      ( public_key_hash_of_v0 metadata.protocol_data.baker,
        header.shell.timestamp,
        header.protocol_data.contents.priority,
        header.hash,
        Some header.shell.predecessor )
end

module M = General_archiver.Define (Services)
