(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Jsonrpc = struct
  open Rpc_encodings

  let id_to_otel_value : JSONRPC.id_repr option -> Opentelemetry.value =
    function
    | None -> `None
    | Some (JSONRPC.Id_float f) -> `Float f
    | Some (Id_string s) -> `String s

  let trace_dispatch_with ?(websocket = false) ~service_name method_ id =
    let span_name = Format.sprintf "%s/%s" service_name method_ in
    Opentelemetry_lwt.Trace.with_
      ~service_name
      ~kind:Span_kind_server
      ~attrs:
        [
          ("rpc.system", `String "jsonrpc");
          ("rpc.jsonrpc.version", `String "2.0");
          ("rpc.method", `String method_);
          ("rpc.jsonrpc.request_id", id_to_otel_value id);
          ( "network.protocol.name",
            `String (if websocket then "websocket" else "http") );
        ]
      span_name

  let return_error : JSONRPC.error -> ('a, JSONRPC.error) result Lwt.t =
   fun err ->
    (match Opentelemetry.Scope.get_ambient_scope () with
    | Some scope ->
        Opentelemetry_lwt.Trace.add_attrs
          scope
          (Fun.const
             [
               ("rpc.jsonrpc.error_code", `Int err.code);
               ("rpc.jsonrpc.error_message", `String err.message);
             ])
    | None -> ()) ;
    Lwt.return_error err
end
