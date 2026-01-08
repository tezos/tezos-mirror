(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc
open Path

let traceparent_header scope =
  ( Opentelemetry.Trace_context.Traceparent.name,
    Traceparent.(from_scope scope |> to_string) )

(* HTTP client span attributes that follow Opentelemetry conventions of
   https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client-span *)
let span_attributes media_types method_ ?route uri =
  let full_attr = ("url.full", `String (Uri.to_string uri)) in
  let http_server_attr =
    match Uri.host uri with
    | Some server -> [("server.address", `String server)]
    | None -> []
  in
  let http_port_attr =
    match (Uri.scheme uri, Uri.port uri) with
    | Some "http", None -> [("server.port", `Int 80)]
    | Some "https", None -> [("server.port", `Int 443)]
    | _, Some port -> [("server.port", `Int port)]
    | _, _ -> []
  in
  let url_scheme_attr =
    match Uri.scheme uri with
    | Some sc -> [("url.scheme", `String sc)]
    | None -> []
  in
  let media_type_attr =
    match Tezos_rpc_http.Media_type.first_complete_media media_types with
    | Some ((ty, sub), _) ->
        [
          ( "http.request.header.content-type",
            `String (String.concat "/" [ty; sub]) );
        ]
    | None -> []
  in
  let route_attr =
    match route with
    | None -> []
    | Some route -> [("http.template", `String route)]
  in
  [("http.request.method", `String (Resto.string_of_meth method_)); full_attr]
  @ route_attr @ http_server_attr @ http_port_attr @ url_scheme_attr
  @ media_type_attr

let call_service media_types ?logger ?(headers = []) ~base rpc b c input =
  let method_ = Service.meth rpc in
  let base_str = Uri.path base in
  let path = Filename.Infix.(base_str // (Service.path rpc |> to_string)) in
  let make_attrs () =
    let req = Tezos_rpc.Service.forge_request ~base rpc b c in
    span_attributes media_types method_ ~route:path req.uri
  in
  Trace.with_tzresult
    ~service_name:"RPC_client"
    ~kind:Span_kind_client
    Format.(sprintf "%s %s" (Resto.string_of_meth method_) path)
  @@ fun scope ->
  Opentelemetry.Scope.add_attrs scope make_attrs ;
  let headers =
    if Opentelemetry.Collector.has_backend () then
      traceparent_header scope :: headers
    else headers
  in
  Tezos_rpc_http_client_unix.RPC_client_unix.call_service
    media_types
    ~base
    ~headers
    ?logger
    rpc
    b
    c
    input
