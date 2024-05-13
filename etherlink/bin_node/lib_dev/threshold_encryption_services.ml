(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@ntrilli.tech>                       *)
(*                                                                           *)
(*****************************************************************************)

(* Definition of services to interact with the threshold encryption sequencer.
   sidecar. The services are defined to be compatible with those defined in
   `etherlink/bin_dsn_node/crates/core/sequencer/rpc_server.rs`. The EVM node
   only uses the rpc services defined in this file to make requests to the
   threshold encryption sequencer sidecar. *)

open Tezos_rpc

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7213.
   Split `Rollup_services` in helper and rollup node RPCs modules. *)
let call_service = Rollup_services.call_service

let post_proposal_service =
  Service.post_service
    ~query:Query.empty
    ~input:Threshold_encryption_types.proposal_encoding
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(root / "proposal")

let get_monitor_preblocks_service =
  Service.get_service
    ~description:"Monitor new blueprints"
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7190
         Handle starting from a given blueprint number. *)
    ~query:Query.empty
    ~output:Threshold_encryption_types.preblock_encoding
    Tezos_rpc.Path.(root / "monitor" / "preblocks")

let submit_proposal ~keep_alive ~sidecar_endpoint proposal =
  let open Lwt_result_syntax in
  let* _answer =
    call_service
      ~keep_alive
      ~base:sidecar_endpoint
      post_proposal_service
      ()
      ()
      proposal
  in
  return_unit

let monitor_preblocks ~sidecar_endpoint () =
  let open Lwt_result_syntax in
  let stream, push = Lwt_stream.create () in
  let on_chunk v = push (Some v) and on_close () = push None in
  let* _spill_all =
    Tezos_rpc_http_client_unix.RPC_client_unix.call_streamed_service
      [Media_type.json]
      ~base:sidecar_endpoint
      get_monitor_preblocks_service
      ~on_chunk
      ~on_close
      ()
      ()
      ()
  in
  return stream
