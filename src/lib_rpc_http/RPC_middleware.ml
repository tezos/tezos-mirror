(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type forwarder_events = {
  on_forwarding : Cohttp.Request.t -> unit Lwt.t;
  on_locally_handled : Cohttp.Request.t -> unit Lwt.t;
}

let make_transform_callback ?ctx ?forwarder_events forwarding_endpoint callback
    conn req body =
  let open Lwt_syntax in
  let open Cohttp in
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
  if answer_has_not_found_status answer then
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
    let* resp, body =
      Cohttp_lwt_unix.Client.call
        ?ctx
        ~headers
        ~body:(Cohttp_lwt.Body.of_stream body_stream)
        (Request.meth req)
        uri
    in
    let status = Response.status resp in
    let headers =
      Response.headers resp |> fun h ->
      Header.remove h "transfer-encoding" |> fun h ->
      Header.remove h "content-length" |> fun h -> Header.remove h "connection"
    in
    let* answer = Cohttp_lwt_unix.Server.respond ~headers ~status ~body () in
    Lwt.return (`Response answer)
  else
    let* () =
      match forwarder_events with
      | Some {on_locally_handled; _} -> on_locally_handled req
      | None -> Lwt.return_unit
    in
    Lwt.return answer

let make_transform_callback_with_acl ~acl ?ctx ?forwarder_events
    forwarding_endpoint callback conn req body =
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

let proxy_server_query_forwarder ?acl ?ctx ?forwarder_events forwarding_endpoint
    =
  match acl with
  | Some acl ->
      make_transform_callback_with_acl
        ~acl
        ?ctx
        ?forwarder_events
        forwarding_endpoint
  | None -> make_transform_callback ?ctx ?forwarder_events forwarding_endpoint
