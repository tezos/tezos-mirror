(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type preconfirmation_handler = Broadcast.transaction -> unit tzresult Lwt.t

type timestamp_handler = Time.Protocol.t -> unit tzresult Lwt.t

type parameters = {
  evm_node_endpoint : Uri.t;
  evm_node_endpoint_timeout : float;
  on_timestamp : timestamp_handler;
  on_preconfirmation : preconfirmation_handler;
}

(**
  [start parameters] starts the monitoring process using the given [parameters].
  The process connects to the specified EVM node and listens for
  {!preconfirmation_message} values. When a [Block_timestamp] or
  [Confirmed_transaction] message is received, the corresponding handler
  ([on_timestamp] or [on_preconfirmation]) is invoked.
*)
val start : parameters -> unit tzresult Lwt.t
