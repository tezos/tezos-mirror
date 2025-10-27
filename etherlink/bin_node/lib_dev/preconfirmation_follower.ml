(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Lwt_result_syntax

type preconfirmation_handler = Transaction_object.t -> unit tzresult Lwt.t

type timestamp_handler = Time.Protocol.t -> unit tzresult Lwt.t

type parameters = {
  evm_node_endpoint : Uri.t;
  evm_node_endpoint_timeout : float;
  on_timestamp : timestamp_handler;
  on_preconfirmation : preconfirmation_handler;
}

let rec stream_loop ~monitor ~(params : parameters) =
  let*! message = Evm_services.get_from_monitor monitor in
  match message with
  | Some (Broadcast.Block_timestamp ts) ->
      let* () = params.on_timestamp ts in
      (stream_loop [@tailcall]) ~monitor ~params
  | Some (Broadcast.Preconfirmed_transaction txns) ->
      let* () = params.on_preconfirmation txns in
      (stream_loop [@tailcall]) ~monitor ~params
  | None ->
      Evm_services.close_monitor monitor ;
      return_unit

let start
    ({evm_node_endpoint; evm_node_endpoint_timeout; _} as params : parameters) =
  let* monitor =
    Evm_services.monitor_preconfirmations
      ~timeout:evm_node_endpoint_timeout
      evm_node_endpoint
  in
  stream_loop ~monitor ~params
