module Otel = Opentelemetry
module Otel_lwt = Opentelemetry_lwt
open Cohttp
open Cohttp_lwt

module Server : sig
  val trace :
    ?service_name:string ->
    ?attrs:Otel.Span.key_value list ->
    ('conn -> Request.t -> 'body -> (Response.t * 'body) Lwt.t) ->
    'conn ->
    Request.t ->
    'body ->
    (Response.t * 'body) Lwt.t
  (** Trace requests to a Cohttp server.

      Use it like this:

      {[
      let my_server callback =
        let callback_traced =
          Opentelemetry_cohttp_lwt.Server.trace
            ~service_name:"my-service"
            (fun _scope -> callback)
        in
        Cohttp_lwt_unix.Server.create
          ~mode:(`TCP (`Port 8080))
          (Server.make () ~callback:callback_traced)
      ]}
   *)

  val with_ :
    ?trace_state:string ->
    ?service_name:string ->
    ?attrs:Otel.Span.key_value list ->
    ?kind:Otel.Span.kind ->
    ?links:Otel.Span_link.t list ->
    string ->
    Request.t ->
    (Request.t -> 'a Lwt.t) ->
    'a Lwt.t
  (** Trace a new internal span.

      Identical to [Opentelemetry_lwt.Trace.with_], but fetches/stores the trace
      scope in the [x-ocaml-otel-traceparent] header in the request for
      convenience.
   *)

  val get_trace_context :
    ?from:[ `Internal | `External ] -> Request.t -> Otel.Scope.t option
  (** Get the tracing scope from the custom [x-ocaml-otel-traceparent] header
      added by [trace] and [with_].
   *)

  val set_trace_context : Otel.Scope.t -> Request.t -> Request.t
  (** Set the tracing scope in the custom [x-ocaml-otel-traceparent] header used
      by [trace] and [with_].
   *)

  val remove_trace_context : Request.t -> Request.t
  (** Strip the custom [x-ocaml-otel-traceparent] header added by [trace] and
      [with_].
   *)
end = struct
  let attrs_of_request (req : Request.t) =
    let meth = req |> Request.meth |> Code.string_of_method in
    let referer = Header.get (Request.headers req) "referer" in
    let host = Header.get (Request.headers req) "host" in
    let ua = Header.get (Request.headers req) "user-agent" in
    let uri = Request.uri req in
    List.concat
      [
        [ "http.method", `String meth ];
        (match host with
        | None -> []
        | Some h -> [ "http.host", `String h ]);
        [ "http.url", `String (Uri.to_string uri) ];
        (match ua with
        | None -> []
        | Some ua -> [ "http.user_agent", `String ua ]);
        (match referer with
        | None -> []
        | Some r -> [ "http.request.header.referer", `String r ]);
      ]

  let attrs_of_response (res : Response.t) =
    let code = Response.status res in
    let code = Code.code_of_status code in
    [ "http.status_code", `Int code ]

  let header_x_ocaml_otel_traceparent = "x-ocaml-otel-traceparent"

  let set_trace_context (scope : Otel.Scope.t) req =
    let module Traceparent = Otel.Trace_context.Traceparent in
    let headers =
      Header.add (Request.headers req) header_x_ocaml_otel_traceparent
        (Traceparent.to_value ~trace_id:scope.trace_id ~parent_id:scope.span_id
           ())
    in
    { req with headers }

  let get_trace_context ?(from = `Internal) req =
    let module Traceparent = Otel.Trace_context.Traceparent in
    let name =
      match from with
      | `Internal -> header_x_ocaml_otel_traceparent
      | `External -> Traceparent.name
    in
    match Header.get (Request.headers req) name with
    | None -> None
    | Some v ->
      (match Traceparent.of_value v with
      | Ok (trace_id, parent_id) ->
        Some
          Otel.Trace.{ trace_id; span_id = parent_id; events = []; attrs = [] }
      | Error _ -> None)

  let remove_trace_context req =
    let headers =
      Header.remove (Request.headers req) header_x_ocaml_otel_traceparent
    in
    { req with headers }

  let trace ?service_name ?(attrs = []) callback conn req body =
    let scope = get_trace_context ~from:`External req in
    Otel_lwt.Trace.with_ ?service_name "request" ~kind:Span_kind_server
      ?trace_id:(Option.map (fun scope -> scope.Otel.Trace.trace_id) scope)
      ?parent:(Option.map (fun scope -> scope.Otel.Trace.span_id) scope)
      ~attrs:(attrs @ attrs_of_request req)
      (fun scope ->
        let open Lwt.Syntax in
        let req = set_trace_context scope req in
        let* res, body = callback conn req body in
        Otel.Trace.add_attrs scope (fun () -> attrs_of_response res);
        Lwt.return (res, body))

  let with_ ?trace_state ?service_name ?attrs
      ?(kind = Otel.Span.Span_kind_internal) ?links name req
      (f : Request.t -> 'a Lwt.t) =
    let scope = get_trace_context ~from:`Internal req in
    Otel_lwt.Trace.with_ ?trace_state ?service_name ?attrs ~kind
      ?trace_id:(Option.map (fun scope -> scope.Otel.Trace.trace_id) scope)
      ?parent:(Option.map (fun scope -> scope.Otel.Trace.span_id) scope)
      ?links name
      (fun scope ->
        let req = set_trace_context scope req in
        f req)
end

let client ?(scope : Otel.Scope.t option) (module C : Cohttp_lwt.S.Client) =
  let module Traced = struct
    open Lwt.Syntax

    let attrs_for ~uri ~meth:_ () =
      [
        "http.method", `String (Code.string_of_method `GET);
        "http.url", `String (Uri.to_string uri);
      ]

    let context_for ~uri ~meth =
      let trace_id =
        match scope with
        | Some scope -> Some scope.trace_id
        | None -> None
      in
      let parent =
        match scope with
        | Some scope -> Some scope.span_id
        | None -> None
      in
      let attrs = attrs_for ~uri ~meth () in
      trace_id, parent, attrs

    let add_traceparent (scope : Otel.Scope.t) headers =
      let module Traceparent = Otel.Trace_context.Traceparent in
      let headers =
        match headers with
        | None -> Header.init ()
        | Some headers -> headers
      in
      Header.add headers Traceparent.name
        (Traceparent.to_value ~trace_id:scope.trace_id ~parent_id:scope.span_id
           ())

    type ctx = C.ctx

    let call ?ctx ?headers ?body ?chunked meth (uri : Uri.t) :
        (Response.t * Cohttp_lwt.Body.t) Lwt.t =
      let trace_id, parent, attrs = context_for ~uri ~meth in
      Otel_lwt.Trace.with_ "request" ~kind:Span_kind_client ?trace_id ?parent
        ~attrs (fun scope ->
          let headers = add_traceparent scope headers in
          let* res, body = C.call ?ctx ~headers ?body ?chunked meth uri in
          Otel.Trace.add_attrs scope (fun () ->
              let code = Response.status res in
              let code = Code.code_of_status code in
              [ "http.status_code", `Int code ]);
          Lwt.return (res, body))

    let head ?ctx ?headers uri =
      let open Lwt.Infix in
      call ?ctx ?headers `HEAD uri >|= fst

    let get ?ctx ?headers uri = call ?ctx ?headers `GET uri

    let delete ?ctx ?body ?chunked ?headers uri =
      call ?ctx ?headers ?body ?chunked `DELETE uri

    let post ?ctx ?body ?chunked ?headers uri =
      call ?ctx ?headers ?body ?chunked `POST uri

    let put ?ctx ?body ?chunked ?headers uri =
      call ?ctx ?headers ?body ?chunked `PUT uri

    let patch ?ctx ?body ?chunked ?headers uri =
      call ?ctx ?headers ?body ?chunked `PATCH uri

    let post_form ?ctx ?headers ~params uri =
      let trace_id, parent, attrs = context_for ~uri ~meth:`POST in
      Otel_lwt.Trace.with_ "request" ~kind:Span_kind_client ?trace_id ?parent
        ~attrs (fun scope ->
          let headers = add_traceparent scope headers in
          let* res, body = C.post_form ?ctx ~headers ~params uri in
          Otel.Trace.add_attrs scope (fun () ->
              let code = Response.status res in
              let code = Code.code_of_status code in
              [ "http.status_code", `Int code ]);
          Lwt.return (res, body))

    let callv = C.callv (* TODO *)
  end in
  (module Traced : Cohttp_lwt.S.Client)
