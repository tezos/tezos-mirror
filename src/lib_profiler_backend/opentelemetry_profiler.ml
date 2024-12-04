(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let initialize service_name =
  Opentelemetry.Globals.service_name := service_name ;
  Opentelemetry.GC_metrics.basic_setup () ;
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ()) ;
  Opentelemetry_client_cohttp_lwt.setup ()

let op_hash_to_trace_id op_hash =
  (* A trace id must be of 16 bytes. We use the first 16 bytes of an operation
     hash to make it replicable and unique enough. *)
  Bytes.sub (Tezos_crypto.Hashed.Operation_hash.to_bytes op_hash) 0 16
  |> Opentelemetry.Trace_id.of_bytes

let add_event ?attrs name =
  Opentelemetry.(
    Scope.get_ambient_scope ()
    |> Option.iter @@ fun scope ->
       Trace.add_event scope @@ fun () -> Event.make ?attrs name)

let trace ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind ?trace_id
    ?parent ?scope ?links name k =
  Opentelemetry.Trace.with_
    ?force_new_trace_id
    ?trace_state
    ?service_name
    ?attrs
    ?kind
    ?trace_id
    ?parent
    ?scope
    ?links
    name
  @@ fun _scope -> k ()

let trace_operation op ?attrs =
  let op_hash =
    match op with `Operation op -> Operation.hash op | `Hash oph -> oph
  in
  let metadata =
    ( "operation_hash",
      `String (Tezos_crypto.Hashed.Operation_hash.to_b58check op_hash) )
  in
  let attrs =
    match attrs with None -> [metadata] | Some attrs -> metadata :: attrs
  in
  trace ~trace_id:(op_hash_to_trace_id op_hash) ~attrs
