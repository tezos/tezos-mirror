(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let attributes ~port ~meth io req () =
  let uri = Cohttp.Request.uri req in
  let scheme =
    match Uri.scheme uri with None -> `None | Some scheme -> `String scheme
  in
  let host =
    match Uri.host uri with None -> `None | Some host -> `String host
  in
  let path, query =
    match String.split_on_char '?' (Uri.path_and_query uri) with
    | [] -> ("", `None)
    | [p] -> (p, `None)
    | p :: q :: _ -> (p, `String q)
  in
  let transport, client_addr, peer_addr, peer_port =
    match io with
    | Conduit_lwt_unix.TCP {ip; port; _} ->
        let ip = Ipaddr.to_string ip in
        (`String "tcp", `String ip, `String ip, `Int port)
    | Conduit_lwt_unix.Tunnel (t, _, _) ->
        (`String "tunnel", `None, `String t, `None)
    | Conduit_lwt_unix.Domain_socket {path; _} ->
        (`String "unix socket", `None, `String path, `None)
    | Conduit_lwt_unix.Vchan {domid; port} ->
        ( `String "vchan",
          `None,
          `String (Format.sprintf "dom%d" domid),
          `String port )
  in
  let headers_attrs =
    List.map
      (fun (header, v) -> ("http.request.header." ^ header, `String v))
      (Cohttp.Header.to_list req.headers)
  in
  [
    ("http.request.method", `String meth);
    ("url.scheme", scheme);
    ("url.path", `String path);
    ("url.query", query);
    ("network.protocol.name", `String "http");
    ("server.port", `Int port);
    ("client.address", client_addr);
    ("network.peer.address", peer_addr);
    ("network.peer.port", peer_port);
    ( "network.protocol.version",
      `String (Cohttp.Code.string_of_version req.version) );
    ("network.transport", transport);
    ("server.address", host);
  ]
  @ headers_attrs

let result_attributes scope action () =
  match action with
  | `Expert (resp, _) | `Response (resp, _) ->
      let headers_attrs =
        List.map
          (fun (header, v) -> ("http.response.header." ^ header, `String v))
          (Cohttp.Header.to_list (Cohttp_lwt.Response.headers resp))
      in
      let status = Cohttp_lwt.Response.status resp in
      let status_code = Cohttp.Code.code_of_status status in
      Opentelemetry.Scope.set_status
        scope
        (let code =
           if status_code >= 300 then
             Opentelemetry.Span_status.Status_code_error
           else Status_code_ok
         in
         Opentelemetry.Span_status.make
           ~message:(Cohttp.Code.string_of_status status)
           ~code) ;
      [("http.response.status_code", `Int status_code)] @ headers_attrs

let resto_callback ~port server ((io, _conn) as iconn) (req : Cohttp.Request.t)
    body =
  let open Lwt_syntax in
  let meth = Cohttp.Code.string_of_method req.meth in
  let trace_name = String.concat " " [meth; req.resource] in
  let traceparent =
    Cohttp.Header.get req.headers Opentelemetry.Trace_context.Traceparent.name
    |> Option.map Opentelemetry.Trace_context.Traceparent.of_value
  in
  let trace_id, parent =
    match traceparent with
    | None | Some (Error _) -> (None, None)
    | Some (Ok (tid, sid)) -> (Some tid, Some sid)
  in
  Opentelemetry_lwt.Trace.with_
    ~kind:Span_kind_server
    ~service_name:"HTTP_server"
    ?trace_id
    ?parent
    trace_name
  @@ fun scope ->
  Opentelemetry.Scope.add_attrs scope (attributes ~port ~meth io req) ;
  let* action =
    Tezos_rpc_http_server.RPC_server.resto_callback server iconn req body
  in
  Opentelemetry.Scope.add_attrs scope (result_attributes scope action) ;
  return action
