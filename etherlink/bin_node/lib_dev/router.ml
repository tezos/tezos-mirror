(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc

type media = [`Json | `Octets]

let default_media = `Json

let application_octet_stream = "application/octet-stream"

type accept_media = {media : [media | `Any]; q : float}

let parse_media_header s =
  match String.split_on_char ';' s with
  | [] -> None
  | accept :: rest -> (
      try
        let media =
          match accept |> String.trim |> String.lowercase_ascii with
          | "application/json" -> `Json
          | "application/octet-stream" -> `Octets
          | "application/*" | "*/*" | "*" -> `Any
          | _ -> raise Not_found
        in
        let rec prio r =
          match r with
          | [] -> 1.0
          | q :: r -> (
              try Scanf.sscanf q " q = %f " (fun f -> f) with _ -> prio r)
        in
        Some {media; q = prio rest}
      with Not_found -> None)

let media header_name request =
  let medias = Dream.headers request header_name in
  let medias =
    List.fold_left
      (fun acc h -> List.rev_append (String.split_on_char ',' h) acc)
      []
      medias
    |> List.rev
  in
  let parsed =
    List.filter_map parse_media_header medias
    |> List.stable_sort (fun a1 a2 -> Compare.Float.compare a2.q a1.q)
  in
  match parsed with
  | [] | {media = `Any; _} :: _ -> default_media
  | {media = #media as m; _} :: _ -> m

let accept_media = media "accept"

let content_media = media "content-type"

let encode media encoding =
  (* We use the functions from Tezos_rpc_http to remain compatible with the
     Resto encodings. *)
  match media with
  | `Json -> Tezos_rpc_http.Media_type.json.construct encoding
  | `Octets -> Tezos_rpc_http.Media_type.octet_stream.construct encoding

let decode media encoding =
  match media with
  | `Json -> (
      fun s ->
        match Ezjsonm.value_from_string_result s with
        | Error err -> Error (Ezjsonm.read_error_description err)
        | Ok json -> (
            try Ok (Data_encoding.Json.destruct encoding json)
            with exn ->
              let err =
                Format.asprintf
                  "%a"
                  (Data_encoding.Json.print_error ~print_unknown:(fun fmt exn ->
                       Format.fprintf
                         fmt
                         "Unknown exception %s"
                         (Printexc.exn_slot_name exn)))
                  exn
              in
              Error err))
  | `Octets ->
      fun s ->
        Data_encoding.Binary.of_string (Data_encoding.dynamic_size encoding) s
        |> Result.map_error
             (Format.asprintf "%a" Data_encoding.Binary.pp_read_error)

let content_header = function
  | `Json -> Dream.application_json
  | `Octets -> application_octet_stream

let respond ?status ?code ?headers media v =
  let open Lwt_syntax in
  let* response = Dream.respond ?status ?code ?headers v in
  Dream.set_header response "Content-type" (content_header media) ;
  return response

let respond_error ?status ?code ?headers media err =
  respond
    ?status
    ?code
    ?headers
    media
    (encode media Rpc_encodings.JSONRPC.error_encoding err)

let get_params : type params.
    Dream.request -> (unit, params) Path.t -> (params, string) result =
 fun request p ->
  let open Result_syntax in
  let rec extract : type param.
      (unit, param) Resto.Internal.path -> (param, string) result = function
    | Root -> return_unit
    | Static (path, _s) -> extract path
    | Dynamic (path, arg) ->
        let* rest = extract path in
        let* param_str =
          try Dream.param request arg.descr.name |> return with
          | Failure s -> fail s
          | exn -> fail (Printexc.to_string exn)
        in
        let* param = arg.destruct param_str in
        return (rest, param)
    | DynamicTail (_, _) ->
        fail "Dynamic tail services not implemented for Dream router"
  in
  extract (Resto.Internal.to_path p)

let get_queries request query =
  try Ok (Query.parse query (Dream.all_queries request)) with
  | Resto.Query.Invalid s -> Error s
  | exn -> Error (Printexc.to_string exn)

let dream_path (path : _ Path.t) : string =
  let to_segments : type pr p. (pr, p) Resto.Internal.path -> string list =
   fun path ->
    let rec flatten_rev : type pr p. (pr, p) Resto.Internal.path -> string list
        = function
      | Root -> []
      | Static (p, s) -> s :: flatten_rev p
      | Dynamic (p, arg) -> Printf.sprintf ":%s" arg.descr.name :: flatten_rev p
      | DynamicTail (_, _) ->
          invalid_arg "Dynamic tail services not implemented for Dream router"
    in
    List.rev @@ flatten_rev path
  in
  "/" ^ String.concat "/" (to_segments (Resto.Internal.to_path path))

let bad_request media msg =
  respond ~status:`Bad_Request media (encode media Data_encoding.string msg)

let make_gen_route : type params query input.
    (_, _, params, query, input, _) Service.t ->
    (Dream.request ->
    params:params ->
    query:query ->
    input ->
    Dream.response Lwt.t) ->
    Dream.route =
 fun service handler ->
  let f =
    match Service.meth service with
    | `PUT -> Dream.put
    | `GET -> Dream.get
    | `DELETE -> Dream.delete
    | `POST -> Dream.post
    | `PATCH -> Dream.patch
  in
  let input_encoding = Service.input_encoding service in
  let path = dream_path (Service.path service) in
  f path @@ fun request ->
  let open Lwt_syntax in
  let output_media = accept_media request in
  match get_queries request (Service.query service) with
  | Error e -> bad_request output_media e
  | Ok query -> (
      match get_params request (Service.path service) with
      | Error e -> bad_request output_media e
      | Ok params -> (
          match input_encoding with
          | No_input -> handler request ~params ~query ()
          | Input input_encoding -> (
              let* body = Dream.body request in
              let media = content_media request in
              match decode media input_encoding body with
              | Error msg ->
                  respond_error output_media (Rpc_errors.parse_error msg)
              | Ok input -> handler request ~params ~query input)))

let make_route service handler =
  make_gen_route service @@ fun request ~params ~query input ->
  let open Lwt_syntax in
  let output_encoding = Service.output_encoding service in
  let* output = handler ~params ~query input in
  let media = accept_media request in
  respond media (encode media output_encoding output)

let make_tz_route service handler =
  make_gen_route service @@ fun request ~params ~query input ->
  let open Lwt_syntax in
  let* output = handler ~params ~query input in
  let media = accept_media request in
  match output with
  | Ok output ->
      let output_encoding = Service.output_encoding service in
      respond media (encode media output_encoding output)
  | Error e ->
      respond
        ~status:`Internal_Server_Error
        media
        (encode media trace_encoding e)

let make_opt_tz_route service handler =
  make_gen_route service @@ fun request ~params ~query input ->
  let open Lwt_syntax in
  let* output = handler ~params ~query input in
  let media = accept_media request in
  match output with
  | Ok (Some output) ->
      let output_encoding = Service.output_encoding service in
      respond media (encode media output_encoding output)
  | Ok None ->
      respond
        ~status:`Not_Found
        media
        (encode
           media
           Data_encoding.string
           (Dream.target request ^ " not found on server"))
  | Error e ->
      respond
        ~status:`Internal_Server_Error
        media
        (encode media trace_encoding e)

let make_stream_route service handler =
  make_gen_route service @@ fun request ~params ~query input ->
  let open Lwt_syntax in
  let output_encoding = Service.output_encoding service in
  let media = accept_media request in
  Dream.stream ~headers:[("Content-Type", content_header media)]
  @@ fun response_stream ->
  let* stream, shutdown = handler ~params ~query input in
  let* () =
    Lwt_stream.iter_s
      (fun output ->
        let chunk = encode media output_encoding output in
        let* () = Dream.write response_stream chunk in
        Dream.flush response_stream)
      stream
  in
  shutdown () ;
  return_unit

let make_metrics_route path =
  let open Lwt_syntax in
  Dream.get path @@ fun _request ->
  let* {body; content_type} = Metrics.get_metrics () in
  let* response = Dream.respond body in
  Dream.set_header response "Content-type" content_type ;
  return response

let send_error media websocket error =
  let text_or_binary = match media with `Json -> `Text | `Octets -> `Binary in
  Dream.send
    ~text_or_binary
    websocket
    (encode media Rpc_encodings.JSONRPC.error_encoding error)

let make_jsonrpc_websocket_route path
    (handler : Rpc_encodings.websocket_handler) =
  let open Lwt_syntax in
  let open Rpc_encodings in
  Dream.get path @@ fun request ->
  let input_media = content_media request in
  let output_media = accept_media request in
  let text_or_binary =
    match output_media with `Json -> `Text | `Octets -> `Binary
  in
  Dream.websocket @@ fun websocket ->
  let subscriptions = Stdlib.Hashtbl.create 17 in
  let write_stream {id; stream; stopper} =
    Stdlib.Hashtbl.add subscriptions id stopper ;
    let* () =
      Lwt_stream.iter_s
        (function
          | Ok output ->
              let output =
                encode output_media Subscription.notification_encoding output
              in
              (* WARNING: https://gitlab.com/tezos/tezos/-/issues/7645
                 Dream.send can 100% cpu on closed connections (see
                 https://github.com/aantron/dream/issues/230). *)
              Dream.send ~text_or_binary websocket output
          | Error exn ->
              let msg =
                match exn with
                | Failure msg -> msg
                | _ -> Printexc.to_string exn
              in
              send_error output_media websocket (Rpc_errors.internal_error msg))
        (Lwt_stream.wrap_exn stream)
    in
    Stdlib.Hashtbl.remove subscriptions id ;
    let* (_ : bool tzresult) = stopper () in
    return_unit
  in
  let async_write_stream subscription =
    Lwt.dont_wait
      (fun () -> write_stream subscription)
      (fun exn ->
        let (_ : bool tzresult Lwt.t) = subscription.stopper () in
        Stdlib.Hashtbl.remove subscriptions subscription.id ;
        Dream.error @@ fun log ->
        log
          "Websocket write exception with %s: %s"
          (Dream.client request)
          (Printexc.to_string exn))
  in
  let rec loop () =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7645
       Dream.receive may not resolve (with None) on closed connections. *)
    let* message = Dream.receive websocket in
    match message with
    | None ->
        (* The websocket is closed, cleaning up. *)
        Dream.log
          "Websocket connection with %s is closed, cleaning up (%d \
           subscriptions)"
          (Dream.client request)
          (Stdlib.Hashtbl.length subscriptions) ;
        Stdlib.Hashtbl.iter
          (fun _id stopper ->
            let (_ : bool tzresult Lwt.t) = stopper () in
            ())
          subscriptions ;
        Stdlib.Hashtbl.clear subscriptions ;
        Dream.close_websocket websocket
    | Some message ->
        let* () =
          match decode input_media JSONRPC.request_encoding message with
          | Error msg ->
              send_error output_media websocket (Rpc_errors.parse_error msg)
          | Ok input ->
              let* ws_response = handler input in
              let response =
                encode
                  output_media
                  JSONRPC.response_encoding
                  ws_response.response
              in
              let* () = Dream.send ~text_or_binary websocket response in
              (* Support multiple asynchronous requests on websocket. *)
              Option.iter async_write_stream ws_response.subscription ;
              return_unit
        in
        loop ()
  in
  loop ()
