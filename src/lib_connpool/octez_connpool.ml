(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type connection = {
  conn : Cohttp_lwt_unix.IO.conn;
  ic : Cohttp_lwt_unix.IO.ic;
  oc : Cohttp_lwt_unix.IO.oc;
  mutable alive : bool;
}

module Net :
  Cohttp_lwt.S.Net
    with module IO = Cohttp_lwt_unix.IO
     and type ctx = connection option = struct
  module IO = Cohttp_lwt_unix.IO

  type ctx = connection option

  let sexp_of_ctx = function
    | Some {conn; _} -> Conduit_lwt_unix.sexp_of_flow conn
    | None -> Sexplib0.Sexp.List []

  let default_ctx = None

  let connect_uri ~ctx _uri =
    let open Lwt_syntax in
    match ctx with
    | Some {conn; ic; oc; _} -> return (conn, ic, oc)
    | None ->
        (* By construction of this module API, this cannot happen since the
           [ctx] value is always contructed by the public functions. *)
        assert false

  let close_in _ic = ()
  (* See {Note persistent connections} *)

  let close_out _oc = ()
  (* See {Note persistent connections} *)

  let close _ic _oc = ()
  (* See {Note persistent connections} *)

  (* {Note persistent connections}
     Our goal with this module is to implement a pool of persistent connections
     that can be reused to carry out multiple HTTP requests. The [close*]
     functions defined in this module are used by [Cohttp_lwt.Make_client]
     after each HTTP requests. This is why we implement them as no-op. The
     actual closing of the channels will be done on exception handling, when a
     connection is deemed corrupted and to be disposed of. *)
end

type t = {pool : connection Lwt_pool.t; pool_len : int; endpoint : Uri.t}

let close_conn conn =
  let open Lwt_syntax in
  conn.alive <- false ;
  Lwt.catch
    (fun () ->
      let* () = Lwt_io.close conn.ic and* () = Lwt_io.close conn.oc in
      return_unit)
    (fun _ -> return_unit)

let make_pool ?(ctx = Cohttp_lwt_unix.Net.default_ctx) ~pool_len uri =
  Lwt_pool.create
    pool_len
    ~validate:(fun {alive; _} -> Lwt.return alive)
    ~check:(fun {alive; _} is_ok -> is_ok alive)
    ~dispose:(fun conn ->
      let open Lwt_syntax in
      let* () = Connpool_events.disposed_connection uri in
      close_conn conn)
    (fun () ->
      let open Lwt_syntax in
      let* conn, ic, oc = Cohttp_lwt_unix.Net.connect_uri ~ctx uri in
      let* () = Connpool_events.new_connection uri in
      return {conn; ic; oc; alive = true})

let clear t = Lwt_pool.clear t.pool

let make ?ctx ~n endpoint =
  if n <= 0 then
    raise (Invalid_argument "Connection_pool.make: negative or zero pool size") ;
  {endpoint; pool = make_pool ?ctx ~pool_len:n endpoint; pool_len = n}

module Client = Cohttp_lwt.Make_client (Cohttp_lwt_unix.IO) (Net)

let concat_path p1 p2 = String.concat "/" [p1; p2]

let make_uri uri ?query ?userinfo route =
  let uri = Uri.with_path uri (concat_path (Uri.path uri) route) in
  let uri =
    match query with Some query -> Uri.with_query uri query | None -> uri
  in
  let uri =
    match userinfo with
    | Some userinfo -> Uri.with_userinfo uri (Some userinfo)
    | None -> uri
  in
  uri

exception Call_failure of exn

let warm t =
  Opentelemetry.Trace.with_ ~service_name:"Octez_connpool" "warm" @@ fun _ ->
  let rec warm n pool =
    Lwt_pool.use pool @@ fun _conn ->
    if n > 1 then warm (n - 1) pool else Lwt.return_unit
  in
  warm t.pool_len t.pool

let add_traceparent_header header
    ({trace_id; span_id; _} : Opentelemetry.Scope.t) =
  Cohttp.Header.add
    header
    Opentelemetry.Trace_context.Traceparent.name
    (Opentelemetry.Trace_context.Traceparent.to_value
       ~trace_id
       ~parent_id:span_id
       ())

let call_exn ?(headers = Cohttp.Header.init ()) ?body ?query ?userinfo t meth
    route =
  let open Lwt_syntax in
  let uri = make_uri t.endpoint ?query ?userinfo route in
  Lwt_pool.use t.pool @@ fun conn ->
  let make_attrs () =
    let full_attr = ("url.full", `String (Uri.to_string uri)) in
    let http_server_attr =
      match Uri.host uri with
      | Some server -> [("server.name", `String server)]
      | None -> []
    in
    let http_port_attr =
      match (Uri.scheme uri, Uri.port uri) with
      | Some "http", None -> [("server.port", `Int 80)]
      | Some "https", None -> [("server.port", `Int 443)]
      | _, Some port -> [("server.port", `Int port)]
      | _, _ -> []
    in
    let headers_attrs =
      List.map
        (fun (h, v) -> ("http.request.header." ^ h, `String v))
        (Cohttp.Header.to_list headers)
    in
    [
      ("http.route", `String route);
      ("http.request.method", `String (Cohttp.Code.string_of_method meth));
      full_attr;
    ]
    @ http_server_attr @ http_port_attr @ headers_attrs
  in
  Opentelemetry.Trace.with_
    ~service_name:"Octez_connpool"
    ~kind:Span_kind_client
    Format.(sprintf "%s %s" (Cohttp.Code.string_of_method meth) route)
  @@ fun scope ->
  Opentelemetry_lwt.Trace.add_attrs scope make_attrs ;
  Lwt.catch
    (fun () ->
      let headers =
        if Opentelemetry.Collector.has_backend () then
          add_traceparent_header headers scope
        else headers
      in
      let* resp, body = Client.call ~ctx:(Some conn) ~headers ?body meth uri in
      let* body = Cohttp_lwt.Body.to_string body in
      let* () =
        match Cohttp.Header.connection (Cohttp.Response.headers resp) with
        | None | Some `Keep_alive -> Lwt.return_unit
        | Some `Close | Some (`Unknown _) -> close_conn conn
      in
      Opentelemetry_lwt.Trace.add_attrs scope (fun () ->
          let resp_code = Cohttp.Code.code_of_status resp.status in
          if resp_code >= 400 then
            Opentelemetry.Scope.set_status
              scope
              {message = ""; code = Status_code_error} ;
          let headers_attrs =
            List.map
              (fun (h, v) -> ("http.request.header." ^ h, `String v))
              (Cohttp.Header.to_list resp.headers)
          in
          ("http.response.status_code", `Int resp_code) :: headers_attrs) ;
      return (resp, body))
    (fun exn ->
      let* () = close_conn conn in
      Lwt.fail (Call_failure exn))

type error +=
  | Cannot_perform_http_request
  | Connection_pool_internal_error of string

let rec retry n k =
  let open Lwt_result_syntax in
  Lwt.catch k (function
      | Call_failure _ | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
          if n < 0 then tzfail Cannot_perform_http_request else retry (n - 1) k
      | exn -> tzfail (Connection_pool_internal_error (Printexc.to_string exn)))

let call ?headers ?body ?query ?userinfo t meth route =
  (* When trying to make a call, we need to take into account the edge case
     that the scenario could have gone down and all connections are stalled (or
     it had to close some of the connections for some reasons). So we retry
     [pool_len + 1] to handle the worst case scenario where all connections are
     stalled. *)
  retry (t.pool_len + 1) @@ fun () ->
  Lwt_result.ok (call_exn ?headers ?body ?query ?userinfo t meth route)

let get ?headers ?query ?userinfo t route =
  call ?headers ?query ?userinfo t `GET route

let post ?headers ?body ?query ?userinfo t route =
  call ?headers ?body ?query ?userinfo t `POST route

let put ?headers ?body ?query ?userinfo t route =
  call ?headers ?body ?query ?userinfo t `PUT route

let delete ?headers ?body ?query ?userinfo t route =
  call ?headers ?body ?query ?userinfo t `DELETE route

let patch ?headers ?body ?query ?userinfo t route =
  call ?headers ?body ?query ?userinfo t `PATCH route

let () =
  register_error_kind
    ~id:"connection_pool.cannot_perform_http_request"
    ~title:"Cannot perform HTTP request"
    ~description:
      "Could not acquire a connection from the pool able to carry out the \
       provided HTTP request"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Could not acquire a connection from the pool able to carry out the \
         provided HTTP request")
    `Permanent
    Data_encoding.empty
    (function Cannot_perform_http_request -> Some () | _ -> None)
    (fun () -> Cannot_perform_http_request) ;
  register_error_kind
    ~id:"connection_pool.internal_error"
    ~title:"Connection pool failed unexpectedly"
    ~description:"Internal error from the connection pool"
    ~pp:(fun ppf exn ->
      Format.fprintf ppf "Internal error from the connection pool (%s)" exn)
    `Permanent
    Data_encoding.(obj1 (req "exception" string))
    (function Connection_pool_internal_error str -> Some str | _ -> None)
    (fun str -> Connection_pool_internal_error str)
