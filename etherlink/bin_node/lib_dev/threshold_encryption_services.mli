(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@ntrilli.tech>                       *)
(*                                                                           *)
(*****************************************************************************)

(**  Definition of functions to interact with the Dsn node (sequencer sidecar).
     The functions perform calls to the services of the DSN node defined at
     `etherlink/bin_dsn_node/crates/core/sequencer/rpc_server.rs`. *)

(** [submit_proposal ~keep_alive ~sidecar_endpoint proposal] calls the service
    `POST /proposal` on [sidecar_endpoint] with the Json encoded [proposal]
    as the request body. The connection is retried on error if [keep_alive] is
    set to true. The corresponding promise is fulfilled succesfully when the
    DSN node sends a response to the request. *)
val submit_proposal :
  keep_alive:bool ->
  sidecar_endpoint:Uri.t ->
  Threshold_encryption_types.proposal ->
  (unit, tztrace) result Lwt.t

(** [monitor_prebloks ~sidecar_endpoint ()] calls the service
    `GET /monitor/preblocks` on the DSN node on [sidecar_endpoint].
    This function returns a stream of `Threshold_encryption_types.preblock`
    values that can be used to poll preblocks streamed by the DSN node. *)
val monitor_preblocks :
  sidecar_endpoint:Uri.t ->
  unit ->
  (Threshold_encryption_types.preblock Lwt_stream.t, tztrace) result Lwt.t
