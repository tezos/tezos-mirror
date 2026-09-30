(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type forwarder_events = {
  on_forwarding : Cohttp.Request.t -> unit Lwt.t;
  on_locally_handled : Cohttp.Request.t -> unit Lwt.t;
}

module Events = struct
  include Internal_event.Simple

  let section = ["rpc_middleware"]

  let forwarding_accepted_conn =
    declare_3
      ~section
      ~name:"forwarding_accepted_conn"
      ~msg:"[pid:{pid}] ({con}) got accepted conn, forwarding to {uri}"
      ~level:Debug
      ("pid", Data_encoding.int32)
      ("con", Data_encoding.string)
      ("uri", Data_encoding.string)

  let rpc_metrics_callback =
    declare_3
      ~section
      ~name:"rpc_metrics_callback"
      ~msg:"[pid:{pid}] ({con}) rpc metrics callback for {path}"
      ~level:Debug
      ("pid", Data_encoding.int32)
      ("con", Data_encoding.string)
      ("path", Data_encoding.string)
end

module ForwarderConnMap = Map.Make (Cohttp.Connection)

type forwarder_resources = {
  mutable opened_conns : (unit -> unit) ForwarderConnMap.t;
}

let src = Logs.Src.create "rpc_middleware" ~doc:"RPC middleware"

module Log = (val Logs.src_log src : Logs.LOG)

let init_forwarder () = {opened_conns = ForwarderConnMap.empty}

let forwarding_conn_closed forwarder_resources (_, con) : unit =
  if ForwarderConnMap.mem con forwarder_resources.opened_conns then
    let callback = ForwarderConnMap.find con forwarder_resources.opened_conns in
    match callback with
    | Some f -> f ()
    | None ->
        Log.warn (fun m ->
            m
              "(%s) got conn closed, shutdown callback not found"
              (Cohttp.Connection.to_string con))
  else ()

let make_transform_callback ?ctx ?forwarder_events forwarder_resources
    forwarding_endpoint callback conn req body =
  let open Lwt_syntax in
  let open Cohttp in
  let _, con = conn in
  (* Using a [Cohttp_lwt.Body.t] destructs it. As we may need it
     twice, we explicitly clone the underlying [Lwt_stream.t]. *)
  let body_stream = Cohttp_lwt.Body.to_stream body in
  let* answer =
    (* We need to catch non-lwt errors to handle them through the same
       Lwt workflow. *)
    Lwt.catch
      (fun () ->
        callback
          conn
          req
          (Cohttp_lwt.Body.of_stream (Lwt_stream.clone body_stream)))
      (function
        | Not_found ->
            (* Not_found exception are handled and forwarded as a "not
               found response" to allow a potential redirection to the
               node. *)
            let* nf = Cohttp_lwt_unix.Server.respond_not_found () in
            Lwt.return (`Response nf)
        | exn -> Lwt.fail exn)
  in
  let answer_has_not_found_status = function
    | `Expert (response, _) | `Response (response, _) ->
        Response.status response = `Not_found
  in
  if answer_has_not_found_status answer then (
    let* () =
      match forwarder_events with
      | Some {on_forwarding; _} -> on_forwarding req
      | None -> Lwt.return_unit
    in
    let uri = Request.uri req in
    let uri =
      Uri.make
        ?scheme:(Uri.scheme forwarding_endpoint)
        ?userinfo:(Uri.userinfo forwarding_endpoint)
        ?host:(Uri.host forwarding_endpoint)
        ?port:(Uri.port forwarding_endpoint)
        ~path:(Filename.concat (Uri.path forwarding_endpoint) (Uri.path uri))
        ~query:(Uri.query uri)
        ?fragment:(Uri.fragment uri)
        ()
    in
    (* Strip out hop-by-hop connection headers (stolen from
       cohttp-lwt-unix/bin/cohttp_proxy_lwt.ml from
       github.com/mirage/ocaml-cohttp) *)
    let headers =
      Request.headers req |> fun h ->
      Header.remove h "accept-encoding" |> fun h ->
      Header.remove h "content-length" |> fun h ->
      Header.remove h "transfer-encoding" |> fun h ->
      Header.remove h "connection" |> fun h ->
      Header.add h "accept-encoding" "identity"
    in
    let* () =
      Events.(emit forwarding_accepted_conn)
        ( Int32.of_int (Unix.getpid ()),
          Cohttp.Connection.to_string con,
          Uri.to_string uri )
    in
    let* t, closefn =
      Cohttp_lwt_unix.Client.call_with_closefn
        ?ctx
        ~headers
        ~body:(Cohttp_lwt.Body.of_stream body_stream)
        (Request.meth req)
        uri
    in
    let shutdown () =
      Log.debug (fun m ->
          m
            "(%s) got conn closed, closing bound conn for %s"
            (Cohttp.Connection.to_string con)
            (Uri.path uri)) ;
      forwarder_resources.opened_conns <-
        ForwarderConnMap.remove con forwarder_resources.opened_conns ;
      closefn ()
    in
    forwarder_resources.opened_conns <-
      ForwarderConnMap.add con shutdown forwarder_resources.opened_conns ;
    let* resp, body = t in
    let status = Response.status resp in
    let headers =
      Response.headers resp |> fun h ->
      Header.remove h "transfer-encoding" |> fun h ->
      Header.remove h "content-length" |> fun h -> Header.remove h "connection"
    in
    let* answer = Cohttp_lwt_unix.Server.respond ~headers ~status ~body () in
    Lwt.return (`Response answer))
  else
    let shutdown =
      Log.debug (fun m ->
          m
            "(%s) conn closed for locally handled, do nothing: %s"
            (Cohttp.Connection.to_string con)
            (Uri.path (Request.uri req))) ;
      fun () -> ()
    in
    forwarder_resources.opened_conns <-
      ForwarderConnMap.add con shutdown forwarder_resources.opened_conns ;
    let* () =
      match forwarder_events with
      | Some {on_locally_handled; _} -> on_locally_handled req
      | None -> Lwt.return_unit
    in
    Lwt.return answer

let make_transform_callback_with_acl ~acl ?ctx ?forwarder_events
    forwarder_resources forwarding_endpoint callback conn req body =
  let allowed =
    let path =
      Resto.Utils.decode_split_path (Uri.path @@ Cohttp.Request.uri req)
    in
    match Cohttp.Request.meth req with
    | #Resto.meth as meth -> RPC_server.Acl.allowed acl ~meth ~path
    | `HEAD | `OPTIONS | `Other _ | `CONNECT | `TRACE -> true
  in
  if allowed then
    make_transform_callback
      ?ctx
      ?forwarder_events
      forwarder_resources
      forwarding_endpoint
      callback
      conn
      req
      body
  else
    let response =
      let body, encoding = (Cohttp_lwt.Body.empty, Cohttp.Transfer.Fixed 0L) in
      let status = `Unauthorized in
      (Cohttp.Response.make ~status ~encoding (), body)
    in
    Lwt.return (`Response response)

let rpc_metrics_transform_callback ~update_metrics dir callback conn req body =
  let open Lwt_result_syntax in
  let do_call () = callback conn req body in
  let cohttp_meth = Cohttp.Request.meth req in
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let decoded = Resto.Utils.decode_split_path path in
  let*! () =
    Events.(emit rpc_metrics_callback)
      ( Int32.of_int (Unix.getpid ()),
        Cohttp.Connection.to_string (snd conn),
        path )
  in
  let*! description =
    let* resto_meth =
      match cohttp_meth with
      | #Resto.meth as meth -> Lwt.return_ok meth
      | _ -> Lwt.return_error @@ `Method_not_allowed []
    in
    let* uri_desc =
      Tezos_rpc.Directory.lookup_uri_desc dir () resto_meth decoded
    in
    Lwt.return_ok (uri_desc, Resto.string_of_meth resto_meth)
  in
  match description with
  | Ok (uri, meth) ->
      (* We update the metric only if the URI can succesfully
         be matched in the directory tree. *)
      update_metrics uri meth do_call
  | Error _ ->
      (* Otherwise, the call must be done anyway. *)
      do_call ()

(* Default cap, in bytes, on the request body a single RPC call may
   materialize. *)
let default_max_body_size = 16_777_216L (* 16 MiB *)

(* Bounds of the lingering read performed on a request that has been rejected
   for being too large, in the spirit of nginx's [lingering_close]. A client
   that is still uploading will not look at our response until its own writes
   stop blocking, so we keep reading for a little while; but never for an
   unbounded time nor an unbounded number of bytes.

   Three bounds, as nginx has: a per-read idle timeout, which cuts off a
   client that stalls without punishing one that is uploading at speed; a
   total timeout, without which a client dribbling one byte at a time would
   stay under the idle timeout for hours; and a byte ceiling, which caps the
   bandwidth we are willing to waste on a request we have already refused.

   TODO: https://gitlab.com/tezos/tezos/-/issues/8428
   All of this is a workaround for two lines in our cohttp fork:
   [Cohttp_lwt.Server.handle_request] drains the request body in an
   [Lwt.finalize], unconditionally and without a bound, and
   [handle_client] derives keep-alive from the request alone so the
   [Connection: close] we send is ignored. Once the fork drains
   conditionally, {!shutdown_read} and these bounds can go away. *)
let lingering_read_idle_timeout = 1.

let lingering_read_total_timeout = 5.

(* The byte ceiling, as a multiple of the cap rather than as a constant. The
   unread remainder of a rejected request is by construction larger than the
   cap, so a ceiling below the cap would make {!bounded_drain} always give up
   and would leave the reusable-connection path below unreachable at every
   configured cap. Twice the cap lets a client that merely overshot finish
   sending, and stops there.

   Note that it is the total timeout, not this ceiling, that bounds how long a
   connection slot is held; the ceiling only bounds the bandwidth. *)
let lingering_read_max_bytes ~max_body_size = Int64.mul 2L max_body_size

(* Shuts the read half of the connection down, best effort. When it lands,
   cohttp's pending drain consumes what the kernel has queued and then reads
   zero bytes, which every transfer decoder turns into [Transfer.Done], so the
   drain terminates instead of following the client for ever, and the next
   [Request.read] returns [`Eof].

   Best effort, and the failures are not exceptional: the peer may already be
   gone, and a connection carrying no file descriptor cannot be shut down at
   all. Nothing bounds cohttp's drain in those cases. The caller must not read
   anything into success or failure here beyond that -- in particular, whether
   to announce [Connection: close] depends on whether the request stream was
   truncated, not on whether this call worked.

   The shutdown does not, on Linux, discard what the client sends afterwards:
   bytes already in flight are still queued and still readable, and [recv]
   reports end of file only once the queue has drained. What it buys is
   termination, not immediate silence.

   The shutdown also wakes cohttp's EOF monitor, which cancels the request
   handler, and with it the write of any response not written yet. Callers
   must therefore return their response straight after this call, without
   yielding in between: no event emitted, no I/O, no [Lwt.pause]. The response
   is then safe only because, in our cohttp fork, nothing yields between the
   handler returning and the response being flushed -- a property of that code,
   not of ours. https://gitlab.com/tezos/tezos/-/issues/8428 removes the
   shutdown and, with it, this constraint.

   The write half is untouched, so the response can still be written. Whether
   the client gets to read it is another matter: closing a socket that still
   has unread data queued makes the kernel send an RST, which may discard a
   response already sitting in the peer's receive queue. A client that is still
   uploading therefore often reports a send failure rather than the 413 -- one
   more reason for {!bounded_drain} to avoid reaching here when it can.

   Only [TCP] and [Domain_socket] carry a file descriptor. Server side,
   [Conduit_lwt_unix] never builds the other two: [Tunnel] is only built by the
   client-side TLS tunnel connector, and the [Vchan] listeners raise in
   [Conduit_lwt_unix.serve].

   Caveat, TLS: a TLS listener also yields a [TCP] flow, holding the socket
   *underneath* the TLS session, so the shutdown lands below the TLS layer and
   [Ssl.read] then fails mid-record instead of returning 0. The connection is
   still released -- which is the point -- but the 413 is lost and the failure
   surfaces in the server's exception log. *)
let shutdown_read (conn : Cohttp_lwt_unix.Server.conn) =
  match fst conn with
  | Conduit_lwt_unix.TCP {fd; _} | Conduit_lwt_unix.Domain_socket {fd; _} -> (
      try Lwt_unix.shutdown fd Unix.SHUTDOWN_RECEIVE with
      | (Stack_overflow | Out_of_memory) as exn ->
          (* Not ours to swallow: these report a runtime condition, not a dead
             connection. *)
          raise exn
      | _ ->
          (* [ENOTCONN] if the peer is already gone -- which is routine, a
             client that half-closes gets us here -- [EBADF] if the descriptor
             was closed concurrently, and whatever [Lwt_unix.abort] was handed
             if it was aborted. Nothing else here should ever escape. *)
          ())
  | Conduit_lwt_unix.Tunnel _ | Conduit_lwt_unix.Vchan _ -> ()

(* Reads and discards what is left of [stream] within the bounds above.

   Returns [true] iff the request has been received in full, and hence iff the
   connection can be handed back for reuse. When the declared length [expected]
   is known, that means the stream ended *and* exactly that many bytes came out
   of it: the end of the stream on its own is not enough, since
   [Cohttp.Transfer] also reports it on a premature EOF and on a malformed
   chunk header, neither of which leaves a usable connection. Without
   [expected] -- a chunked body, whose length cohttp does not give us -- the end
   of the stream is all there is to go on.

   [false] therefore covers both a connection that is broken and one that is
   merely still busy: a client that stalled, that dribbled past the total
   timeout, or that has more left to send than we are willing to read. The
   caller treats them alike, none of them being reusable.

   The loop reads with [Lwt_stream.get], one chunk per call, and that is what
   keeps its memory at one chunk. Do not replace it with
   [Lwt_stream.junk_while] (what [Cohttp_lwt.Body.drain_body] uses),
   [Lwt_stream.iter] or [Lwt_stream.iter_s]: when chunks are ready without
   blocking, as they are while a client uploads at speed, those recurse
   through [>>=] on the stack, each frame holding a node already consumed, and
   hence every chunk read since, until they return. With the byte ceiling
   below, that is up to twice the cap per connection, for every connection
   being rejected at once. *)
let bounded_drain ?expected ~max_bytes stream =
  let open Lwt_syntax in
  let rec drain drained =
    let* read =
      (* Cancelling [Lwt_stream.get] is safe here: for a stream built with
         [Lwt_stream.from], which is what [Cohttp_lwt.Body.create_stream]
         uses, [Lwt_stream.feed] hands the reading promise out through
         [Lwt.protected]. Cancelling therefore detaches this waiter only --
         the in-flight read carries on, pushes its chunk into the stream, and
         whoever reads next (here, cohttp's own drain) picks it up. No byte is
         lost and no read is issued twice. *)
      Lwt.pick
        [
          (let* chunk = Lwt_stream.get stream in
           Lwt.return (`Read chunk));
          (let* () = Lwt_unix.sleep lingering_read_idle_timeout in
           Lwt.return `Idle);
        ]
    in
    match read with
    | `Idle -> Lwt.return_false
    | `Read None -> (
        match expected with
        | None -> Lwt.return_true
        | Some expected -> Lwt.return (Int64.equal drained expected))
    | `Read (Some chunk) ->
        let drained = Int64.add drained (Int64.of_int (String.length chunk)) in
        if Int64.compare drained max_bytes > 0 then Lwt.return_false
        else drain drained
  in
  Lwt.catch
    (fun () ->
      Lwt.pick
        [
          (let* () = Lwt_unix.sleep lingering_read_total_timeout in
           Lwt.return_false);
          drain 0L;
        ])
    (function
      | Lwt.Canceled as exn ->
          (* The whole callback is being cancelled: the client is gone. *)
          Lwt.reraise exn
      | (Stack_overflow | Out_of_memory) as exn ->
          (* Not ours to swallow: these report a runtime condition, not a
             broken connection. *)
          Lwt.reraise exn
      | _exn ->
          (* The read failed, so the connection is already broken. Report it
             as not fully received so that the caller stops trusting it. *)
          Lwt.return_false)

(* Bounds on reading a body we mean to accept, i.e. one within the cap. Without
   them a client that declares a length under the cap and then sends nothing
   holds its connection, and hence one of the [max_active_rpc_connections]
   slots, for ever and at no cost.

   Much looser than the lingering read's: this is a legitimate request, and a
   client uploading a body of up to the cap over a slow link must be served.
   The idle timeout cuts off a client that stalls; the total one, a client that
   dribbles a byte just often enough to stay under the idle timeout. *)
let body_read_idle_timeout = 10.

let body_read_total_timeout = 60.

type body_read = Received of string | Too_large | Timed_out

(* [Some (received, declared)] if [contents], read to the end of the stream,
   falls short of the length [req] declared. Only a [Content-Length] declares
   one: the end of a chunked body is all cohttp tells us about it.

   Only a body cohttp reads can fall short. For [GET], [HEAD], [CONNECT] and
   [TRACE], [Request.has_body] is [`No] whatever the headers say, and
   [Cohttp_lwt.Server.read_body] hands over an empty body: an empty [contents]
   there is not a truncation, and such requests have always been served. *)
let truncation req contents =
  match (Cohttp_lwt_unix.Request.has_body req, Cohttp.Request.encoding req) with
  | `Yes, Cohttp.Transfer.Fixed declared ->
      let received = Int64.of_int (String.length contents) in
      if Int64.equal received declared then None else Some (received, declared)
  | (`Yes | `No | `Unknown), _ -> None

(* Reads [stream] in full, within the bounds above and up to [max_body_size]
   bytes. Reads with [Lwt_stream.get] for the same reasons as
   {!bounded_drain}: cancelling it is safe, and it holds no chunk already
   consumed.

   Unlike {!bounded_drain}, a failing read is not caught: the exception
   reaches cohttp's [handle_request], which logs it as an error and answers
   500, as it did when resto read the body itself. On a connection that broke
   while the request was still coming in, that 500 does not reach the client:
   writing it fails and the connection ends. *)
let read_body_within_bounds ~max_body_size stream =
  let open Lwt_syntax in
  let buf = Buffer.create 4096 in
  let rec loop () =
    let* read =
      Lwt.pick
        [
          (let* chunk = Lwt_stream.get stream in
           Lwt.return (`Read chunk));
          (let* () = Lwt_unix.sleep body_read_idle_timeout in
           Lwt.return `Idle);
        ]
    in
    match read with
    | `Idle -> Lwt.return Timed_out
    | `Read None -> Lwt.return (Received (Buffer.contents buf))
    | `Read (Some chunk) ->
        Buffer.add_string buf chunk ;
        if Int64.of_int (Buffer.length buf) > max_body_size then
          Lwt.return Too_large
        else loop ()
  in
  Lwt.pick
    [
      (let* () = Lwt_unix.sleep body_read_total_timeout in
       Lwt.return Timed_out);
      loop ();
    ]

let error_response ~status ~connection_closing () =
  let headers =
    if connection_closing then
      (* We did not read the request out in full, so the connection is
         finished: say so, as RFC 9112 §9.6 requires, and so that no
         intermediary reuses or pipelines a connection whose request stream we
         truncated. This header does not drive cohttp itself, which derives
         keep-alive from the request alone. *)
      Cohttp.Header.init_with "connection" "close"
    else
      (* The request was received in full, so the connection is still usable
         and saying otherwise would cost the client a handshake. *)
      Cohttp.Header.init ()
  in
  let body, encoding = (Cohttp_lwt.Body.empty, Cohttp.Transfer.Fixed 0L) in
  (Cohttp.Response.make ~status ~encoding ~headers (), body)

(* Answers 413 after a bounded lingering read, so that a client which is done
   sending still gets a readable response on a reusable connection, while an
   endless body cannot hold the connection -- and hence one of the
   [max_active_rpc_connections] slots, which are taken before [accept] and so
   wedge the whole listener -- for ever.

   [expected], when known, is the length the client declared for the whole
   body, none of which has been read yet. *)
let reject_oversized_body ?expected ~max_body_size conn stream =
  let open Lwt_syntax in
  let max_bytes = lingering_read_max_bytes ~max_body_size in
  let* fully_received =
    match expected with
    | Some expected when Int64.compare expected max_bytes > 0 ->
        (* The client has declared more than we are prepared to read, so the
           lingering read cannot end in a reusable connection whatever it does.
           Skip it rather than spend the bandwidth and delay releasing the slot
           by up to the total timeout: the client ends up reset either way. *)
        Lwt.return_false
    | Some _ | None -> bounded_drain ?expected ~max_bytes stream
  in
  (* Whether to announce the close turns on the request stream having been
     truncated, not on the shutdown having landed: a client that half-closed
     early leaves us a request we cannot complete and a socket whose read half
     can no longer be shut down, and that connection must still not be
     advertised as reusable. *)
  let connection_closing = not fully_received in
  (* Nothing may yield between the shutdown and returning the response, see
     {!shutdown_read}. *)
  if connection_closing then shutdown_read conn ;
  Lwt.return
    (`Response
       (error_response ~status:`Request_entity_too_large ~connection_closing ()))

(* Rejects a request whose body exceeds [max_body_size] before the wrapped
   [callback] runs. A declared [Content-Length] over the cap is rejected
   without the body ever being buffered; any other body is read, within
   {!read_body_within_bounds}, before [callback] runs. An oversized remainder
   is disposed of by {!reject_oversized_body} rather than left for cohttp's
   unbounded drain, and a body that does not arrive in time is answered with
   408 and its connection shut down.

   Not buffering is not the same as not reading: the remainder still goes
   through the bounded lingering read, which is what lets the client read the
   413 before the connection goes away. Buffering a body within the cap, on the
   other hand, costs nothing: resto reads it in full before dispatching
   anyway. *)
let limit_body_size ~max_body_size callback conn req body =
  let open Lwt_syntax in
  match Cohttp.Request.encoding req with
  | Cohttp.Transfer.Fixed len when Int64.compare len max_body_size > 0 ->
      reject_oversized_body
        ~expected:len
        ~max_body_size
        conn
        (Cohttp_lwt.Body.to_stream body)
  | Cohttp.Transfer.Fixed 0L | Cohttp.Transfer.Unknown ->
      (* No body to bound. [Unknown] -- neither [Content-Length] nor
         [Transfer-Encoding], as on most GETs -- cannot carry one:
         [Request.has_body] maps it to [`Unknown], which
         [Cohttp_lwt.Server.read_body] turns into [`Empty]. Passing these
         straight through keeps the common case free of the buffer and timers
         below. *)
      callback conn req body
  | Cohttp.Transfer.Fixed _ | Cohttp.Transfer.Chunked -> (
      let stream = Cohttp_lwt.Body.to_stream body in
      let* read = read_body_within_bounds ~max_body_size stream in
      match read with
      | Received contents -> (
          match truncation req contents with
          | None -> callback conn req (Cohttp_lwt.Body.of_string contents)
          | Some _ ->
              (* The client half-closed, or went away, before sending all it
                 declared: [Cohttp.Transfer] ends the stream on that end of
                 file exactly as on a complete body. Passing the prefix on
                 would have resto decode it, and a prefix can decode -- [12]
                 out of [123] -- to a value the client never sent. Nothing is
                 left to read, so the connection only needs to be announced as
                 closing. *)
              Lwt.return
                (`Response
                   (error_response
                      ~status:`Bad_request
                      ~connection_closing:true
                      ())))
      | Too_large ->
          (* Only a chunked body gets here: a fixed-length one within the cap
             cannot exceed it. No [expected]: cohttp does not tell us the length
             of a chunked body, so the end of the stream is the only completion
             signal available. *)
          reject_oversized_body ~max_body_size conn stream
      | Timed_out ->
          (* The request was not received in full, so the connection is not
             reusable, and leaving the remainder to cohttp's drain would hold
             the connection for as long as the client cares to. *)
          shutdown_read conn ;
          Lwt.return
            (`Response
               (error_response
                  ~status:`Request_timeout
                  ~connection_closing:true
                  ())))

let proxy_server_query_forwarder ?acl ?ctx ?forwarder_events forwarder_resources
    forwarding_endpoint =
  match acl with
  | Some acl ->
      make_transform_callback_with_acl
        ~acl
        ?ctx
        ?forwarder_events
        forwarder_resources
        forwarding_endpoint
  | None ->
      make_transform_callback
        ?ctx
        ?forwarder_events
        forwarder_resources
        forwarding_endpoint

module Http_cache_headers = struct
  (* Using Regex to parse the url path is dirty. Instead, we want to re-use
     [Block_services.path] to parse the prefix path. Ideally, we want to define
     header features as part of a Resto.Service. Unfortunately, Resto needs
     to be extended to support either use case.

     TODO: https://gitlab.com/tezos/tezos/-/issues/7339
     Parse URL using Resto path defined in Block_services.path

     TODO: https://gitlab.com/tezos/tezos/-/issues/7297
     Support headers in Resto Services
  *)
  (* Matches any path with the prefix /chains/<chain-id>/blocks/head<*> where
     `head<*>` is a valid head alias eg. `head`, `head-10`,`head~123` *)
  let block_pattern =
    Re.Str.regexp {|/chains/[A-Za-z0-9]+/blocks/\(head\(\(-\|~\)[0-9]+\)?\).*|}

  let parse_block_subpath uri =
    let path = Uri.path uri in
    if Re.Str.string_match block_pattern path 0 then
      let block_alias = Re.Str.matched_group 1 path in
      Some block_alias
    else None

  let add_header field value response =
    let add f v resp =
      let headers =
        let hs = Cohttp.Response.headers resp in
        Cohttp.Header.add hs f v
      in
      Cohttp.Response.make
        ~status:(Cohttp.Response.status resp)
        ~encoding:(Cohttp.Response.encoding resp)
        ~version:(Cohttp.Response.version resp)
        ~flush:(Cohttp.Response.flush resp)
        ~headers
        ()
    in
    match response with
    | `Response (response, body) -> `Response (add field value response, body)
    | `Expert (response, body) -> `Expert (add field value response, body)

  let may_add_max_age get_estimated_time_to_next_level resp =
    let open Lwt_syntax in
    let* seconds_to_round_end_opt = get_estimated_time_to_next_level () in
    match seconds_to_round_end_opt with
    | None -> return resp
    | Some s ->
        (* `floor` the value to ensure data stored in caches is always
            correct. This means a potential increase in cache misses at
            the end of a round. The value needs to be truncated because
            max-age expects an integer. *)
        let s = Float.floor (Ptime.Span.to_float_s s) in
        if s = 0. then return resp
        else
          return
          @@ add_header
               "cache-control"
               (Format.sprintf "public, max-age=%d" (int_of_float s))
               resp

  (* [may_handle_if_none_match block_hash request] returns a response
     with status `Not_modified` if the request contains an
     `if-none-match` header with the block hash. *)
  let may_handle_if_none_match (block_hash : string) req =
    let open Lwt_syntax in
    let if_none_match_field =
      Cohttp.(Header.get (Request.headers req) "if-none-match")
    in
    match if_none_match_field with
    | None -> return_none
    | Some hashes ->
        let hashes =
          String.split_on_char ',' hashes |> List.map (fun s -> String.trim s)
        in
        if List.mem ~equal:String.equal block_hash hashes then
          let* resp =
            Cohttp_lwt_unix.Server.respond ~body:`Empty ~status:`Not_modified ()
          in
          Lwt.return_some (`Response resp)
        else Lwt.return_none

  let is_success_status_code resp =
    let cohttp_response =
      match resp with `Response (resp, _) -> resp | `Expert (resp, _) -> resp
    in
    let status = Cohttp.Response.status cohttp_response in
    Cohttp.Code.code_of_status status |> Cohttp.Code.is_success

  let make ~get_estimated_time_to_next_level ~get_block_hash callback conn req
      body =
    let open Lwt_syntax in
    let uri = Cohttp.Request.uri req in
    let* block_hash_opt =
      match parse_block_subpath uri with
      | Some block_alias -> get_block_hash block_alias
      | None -> return_none
    in
    match block_hash_opt with
    | None -> callback conn req body
    | Some block_hash ->
        let block_hash = Block_hash.to_b58check block_hash in
        let* resp =
          let* resp_opt = may_handle_if_none_match block_hash req in
          match resp_opt with
          | Some resp -> return resp
          | None -> callback conn req body
        in
        if is_success_status_code resp then
          let* resp = may_add_max_age get_estimated_time_to_next_level resp in
          return (add_header "etag" block_hash resp)
        else return resp
end
