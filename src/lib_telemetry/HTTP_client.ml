(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc
open Path

let call_service media_types ?logger ?headers ~base rpc b c input =
  let method_ = Service.meth rpc |> Resto.string_of_meth in
  let base_str = Uri.path base in
  let path = Filename.Infix.(base_str // (Service.path rpc |> to_string)) in
  let make_attrs () =
    let full_attr =
      let req = Tezos_rpc.Service.forge_request ~base rpc b c in
      ("url.full", `String (Uri.to_string req.uri))
    in
    let http_server_attr =
      match Uri.host base with
      | Some server -> [("server.name", `String server)]
      | None -> []
    in
    let http_port_attr =
      match (Uri.scheme base, Uri.port base) with
      | Some "http", None -> [("server.port", `Int 80)]
      | Some "https", None -> [("server.port", `Int 443)]
      | _, Some port -> [("server.port", `Int port)]
      | _, _ -> []
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
    [
      ("http.route", `String path);
      ("http.request.method", `String method_);
      full_attr;
    ]
    @ http_server_attr @ http_port_attr @ media_type_attr
  in
  Trace.with_tzresult
    ~service_name:"RPC_client"
    ~kind:Span_kind_client
    Format.(sprintf "%s %s" method_ path)
  @@ fun scope ->
  Opentelemetry_lwt.Trace.add_attrs scope make_attrs ;
  Tezos_rpc_http_client_unix.RPC_client_unix.call_service
    media_types
    ~base
    ?headers
    ?logger
    rpc
    b
    c
    input
