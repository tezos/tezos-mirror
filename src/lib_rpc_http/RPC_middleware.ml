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
