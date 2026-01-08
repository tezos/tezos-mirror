(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let message_on_tztrace tztrace =
  Format.asprintf "%a" Error_monad.pp_print_trace tztrace

let with_result ?(message_on_success = Fun.const "Success")
    ?(message_on_error = Fun.const "Something went wrong") ?force_new_trace_id
    ?trace_state ?service_name ?attrs ?kind ?trace_id ?parent ?scope ?links
    span_name k =
  Opentelemetry_lwt.Trace.with_
    ?force_new_trace_id
    ?trace_state
    ?service_name
    ?attrs
    ?kind
    ?trace_id
    ?parent
    ?scope
    ?links
    span_name
  @@ fun scope ->
  let open Lwt_result_syntax in
  let*! res = k scope in
  match res with
  | Ok res when Option.is_none (Opentelemetry.Scope.status scope) ->
      Opentelemetry.Scope.set_status
        scope
        {code = Status_code_ok; message = message_on_success res} ;
      return res
  | Error err when Option.is_none (Opentelemetry.Scope.status scope) ->
      Opentelemetry.Scope.set_status
        scope
        {code = Status_code_error; message = message_on_error err} ;
      fail err
  | res -> Lwt.return res

let with_tzresult = with_result ~message_on_error:message_on_tztrace

let add_attrs f =
  if Opentelemetry.Collector.has_backend () then
    match Opentelemetry.Scope.get_ambient_scope () with
    | Some scope -> Opentelemetry.Scope.add_attrs scope @@ f
    | None -> ()

let add_event f =
  if Opentelemetry.Collector.has_backend () then
    match Opentelemetry.Scope.get_ambient_scope () with
    | Some scope -> Opentelemetry.Scope.add_event scope @@ f
    | None -> ()
