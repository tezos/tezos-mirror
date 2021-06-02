(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module type S = sig
  module type LOGGER = sig
    type request

    val log_empty_request : Uri.t -> request Lwt.t

    val log_request :
      ?media:Media_type.t ->
      'a Data_encoding.t ->
      Uri.t ->
      string ->
      request Lwt.t

    val log_response :
      request ->
      ?media:Media_type.t ->
      'a Data_encoding.t ->
      Cohttp.Code.status_code ->
      string Lwt.t Lazy.t ->
      unit Lwt.t
  end

  type logger = (module LOGGER)

  val null_logger : logger

  val timings_logger :
    gettimeofday:(unit -> float) -> Format.formatter -> logger

  val full_logger : Format.formatter -> logger

  type config = {endpoint : Uri.t; logger : logger}

  val config_encoding : config Data_encoding.t

  val default_config : config

  class http_ctxt : config -> Media_type.t list -> RPC_context.json

  (**/**)

  val call_service :
    Media_type.t list ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    base:Uri.t ->
    ([< Resto.meth], unit, 'p, 'q, 'i, 'o) RPC_service.t ->
    'p ->
    'q ->
    'i ->
    'o tzresult Lwt.t

  val call_streamed_service :
    Media_type.t list ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    base:Uri.t ->
    ([< Resto.meth], unit, 'p, 'q, 'i, 'o) RPC_service.t ->
    on_chunk:('o -> unit) ->
    on_close:(unit -> unit) ->
    'p ->
    'q ->
    'i ->
    (unit -> unit) tzresult Lwt.t

  val generic_json_call :
    ?headers:(string * string) list ->
    ?body:Data_encoding.json ->
    [< RPC_service.meth] ->
    Uri.t ->
    (Data_encoding.json, Data_encoding.json option) RPC_context.rest_result
    Lwt.t

  type content_type = string * string

  type content = Cohttp_lwt.Body.t * content_type option * Media_type.t option

  val generic_call :
    ?headers:(string * string) list ->
    ?accept:Media_type.t list ->
    ?body:Cohttp_lwt.Body.t ->
    ?media:Media_type.t ->
    [< RPC_service.meth] ->
    Uri.t ->
    (content, content) RPC_context.rest_result Lwt.t
end

module Make (Client : Resto_cohttp_client.Client.CALL) = struct
  module Client = Resto_cohttp_client.Client.Make (RPC_encoding) (Client)

  module type LOGGER = Client.LOGGER

  type logger = (module LOGGER)

  let null_logger = Client.null_logger

  let timings_logger = Client.timings_logger

  let full_logger = Client.full_logger

  type content_type = string * string

  type content = Cohttp_lwt.Body.t * content_type option * Media_type.t option

  let request_failed meth uri error =
    let meth = (meth : [< RPC_service.meth] :> RPC_service.meth) in
    fail (RPC_client_errors.Request_failed {meth; uri; error})

  let generic_call ?headers ?accept ?body ?media meth uri :
      (content, content) RPC_context.rest_result Lwt.t =
    Client.generic_call meth ?headers ?accept ?body ?media uri >>= function
    | `Ok (Some v) -> return (`Ok v)
    | `Ok None -> request_failed meth uri Empty_answer
    | ( `Conflict _ | `Error _ | `Forbidden _ | `Unauthorized _ | `Not_found _
      | `Gone _ ) as v ->
        return v
    | `Unexpected_status_code (code, (content, _, media_type)) ->
        let media_type = Option.map Media_type.name media_type in
        Cohttp_lwt.Body.to_string content >>= fun content ->
        request_failed
          meth
          uri
          (Unexpected_status_code {code; content; media_type})
    | `Method_not_allowed allowed ->
        let allowed = List.filter_map RPC_service.meth_of_string allowed in
        request_failed meth uri (Method_not_allowed allowed)
    | `Unsupported_media_type ->
        let media = Option.map Media_type.name media in
        request_failed meth uri (Unsupported_media_type media)
    | `Not_acceptable acceptable ->
        let proposed =
          Option.fold accept ~none:"" ~some:Media_type.accept_header
        in
        request_failed meth uri (Not_acceptable {proposed; acceptable})
    | `Bad_request msg -> request_failed meth uri (Bad_request msg)
    | `Connection_failed msg -> request_failed meth uri (Connection_failed msg)
    | `OCaml_exception msg -> request_failed meth uri (OCaml_exception msg)
    | `Unauthorized_host host ->
        request_failed meth uri (Unauthorized_host host)

  let handle_error meth uri (body, media, _) f =
    Cohttp_lwt.Body.is_empty body >>= fun empty ->
    if empty then return (f None)
    else
      match media with
      | Some ("application", "json") | None -> (
          Cohttp_lwt.Body.to_string body >>= fun body ->
          match Data_encoding.Json.from_string body with
          | Ok body -> return (f (Some body))
          | Error msg ->
              request_failed
                meth
                uri
                (Unexpected_content
                   {
                     content = body;
                     media_type = Media_type.(name json);
                     error = msg;
                   }))
      | Some (l, r) ->
          Cohttp_lwt.Body.to_string body >>= fun body ->
          request_failed
            meth
            uri
            (Unexpected_content_type
               {
                 received = l ^ "/" ^ r;
                 acceptable = [Media_type.(name json)];
                 body;
               })

  let generic_json_call ?headers ?body meth uri :
      (Data_encoding.json, Data_encoding.json option) RPC_context.rest_result
      Lwt.t =
    let body =
      Option.map
        (fun b -> Cohttp_lwt.Body.of_string (Data_encoding.Json.to_string b))
        body
    in
    let media = Media_type.json in
    generic_call meth ?headers ~accept:Media_type.[bson; json] ?body ~media uri
    >>=? function
    | `Ok (body, (Some ("application", "json") | None), _) -> (
        Cohttp_lwt.Body.to_string body >>= fun body ->
        match Data_encoding.Json.from_string body with
        | Ok json -> return (`Ok json)
        | Error msg ->
            request_failed
              meth
              uri
              (Unexpected_content
                 {
                   content = body;
                   media_type = Media_type.(name json);
                   error = msg;
                 }))
    | `Ok (body, Some ("application", "bson"), _) -> (
        Cohttp_lwt.Body.to_string body >>= fun body ->
        match
          Json_repr_bson.bytes_to_bson
            ~laziness:false
            ~copy:false
            (Bytes.unsafe_of_string body)
        with
        | exception Json_repr_bson.Bson_decoding_error (msg, _, pos) ->
            let error = Format.asprintf "(at offset: %d) %s" pos msg in
            request_failed
              meth
              uri
              (Unexpected_content
                 {content = body; media_type = Media_type.(name bson); error})
        | bson ->
            return
              (`Ok
                (Json_repr.convert
                   (module Json_repr_bson.Repr)
                   (module Json_repr.Ezjsonm)
                   bson)))
    | `Ok (body, Some (l, r), _) ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        request_failed
          meth
          uri
          (Unexpected_content_type
             {
               received = l ^ "/" ^ r;
               acceptable = [Media_type.(name json)];
               body;
             })
    | `Conflict body -> handle_error meth uri body (fun v -> `Conflict v)
    | `Error body -> handle_error meth uri body (fun v -> `Error v)
    | `Forbidden body -> handle_error meth uri body (fun v -> `Forbidden v)
    | `Not_found body ->
        (* The client's proxy mode matches on the `Not_found returned here,
           to detect that a local RPC is unavailable at generic_json_call,
           and hence that delegation to the endpoint must be done. *)
        handle_error meth uri body (fun v -> `Not_found v)
    | `Gone body -> handle_error meth uri body (fun v -> `Gone v)
    | `Unauthorized body ->
        handle_error meth uri body (fun v -> `Unauthorized v)

  let handle accept (meth, uri, ans) =
    match ans with
    | `Ok (Some v) -> return v
    | `Ok None -> request_failed meth uri Empty_answer
    | `Gone None -> fail (RPC_context.Gone {meth; uri})
    | `Not_found None ->
        (* The client's proxy mode matches on the error raised here,
           to detect that a local RPC is unavailable at call_service and
           call_streamed_service, and hence that delegation
           to the endpoint must be done. *)
        fail (RPC_context.Not_found {meth; uri})
    | `Conflict (Some err)
    | `Error (Some err)
    | `Forbidden (Some err)
    | `Unauthorized (Some err)
    | `Gone (Some err)
    | `Not_found (Some err) ->
        Lwt.return_error err
    | `Unauthorized None -> request_failed meth uri Unauthorized_uri
    | `Forbidden None -> request_failed meth uri Forbidden
    | `Conflict None | `Error None ->
        fail (RPC_context.Generic_error {meth; uri})
    | `Unexpected_status_code (code, (content, _, media_type)) ->
        let media_type = Option.map Media_type.name media_type in
        Cohttp_lwt.Body.to_string content >>= fun content ->
        request_failed
          meth
          uri
          (Unexpected_status_code {code; content; media_type})
    | `Method_not_allowed allowed ->
        let allowed = List.filter_map RPC_service.meth_of_string allowed in
        request_failed meth uri (Method_not_allowed allowed)
    | `Unsupported_media_type ->
        let name =
          match Media_type.first_complete_media accept with
          | None -> None
          | Some ((l, r), _) -> Some (l ^ "/" ^ r)
        in
        request_failed meth uri (Unsupported_media_type name)
    | `Not_acceptable acceptable ->
        let proposed = Media_type.accept_header accept in
        request_failed meth uri (Not_acceptable {proposed; acceptable})
    | `Bad_request msg -> request_failed meth uri (Bad_request msg)
    | `Unexpected_content ((content, media_type), error)
    | `Unexpected_error_content ((content, media_type), error) ->
        let media_type = Media_type.name media_type in
        request_failed
          meth
          uri
          (Unexpected_content {content; media_type; error})
    | `Unexpected_error_content_type (body, media)
    | `Unexpected_content_type (body, media) ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        let received =
          Option.fold media ~none:"" ~some:(fun (l, r) -> l ^ "/" ^ r)
        in
        request_failed
          meth
          uri
          (Unexpected_content_type
             {received; acceptable = List.map Media_type.name accept; body})
    | `Connection_failed msg -> request_failed meth uri (Connection_failed msg)
    | `OCaml_exception msg -> request_failed meth uri (OCaml_exception msg)
    | `Unauthorized_host host ->
        request_failed meth uri (Unauthorized_host host)

  let call_streamed_service (type p q i o) accept ?logger ?headers ~base
      (service : (_, _, p, q, i, o) RPC_service.t) ~on_chunk ~on_close
      (params : p) (query : q) (body : i) : (unit -> unit) tzresult Lwt.t =
    Client.call_streamed_service
      accept
      ?logger
      ?headers
      ~base
      ~on_chunk
      ~on_close
      service
      params
      query
      body
    >>= fun ans -> handle accept ans

  let call_service (type p q i o) accept ?logger ?headers ~base
      (service : (_, _, p, q, i, o) RPC_service.t) (params : p) (query : q)
      (body : i) : o tzresult Lwt.t =
    Client.call_service ?logger ?headers ~base accept service params query body
    >>= fun ans -> handle accept ans

  type config = {endpoint : Uri.t; logger : logger}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {endpoint; logger = _} -> endpoint)
      (fun endpoint -> {endpoint; logger = null_logger})
      (obj1 (req "endpoint" RPC_encoding.uri_encoding))

  let default_config =
    {endpoint = Uri.of_string "http://localhost:8732"; logger = null_logger}

  class http_ctxt config media_types : RPC_context.json =
    let base = config.endpoint in
    let logger = config.logger in
    object
      method generic_json_call meth ?body uri =
        let path = Uri.path uri and query = Uri.query uri in
        let prefix = Uri.path base in
        let prefixed_path = if prefix = "" then path else prefix ^ "/" ^ path in
        let uri = Uri.with_path base prefixed_path in
        let uri = Uri.with_query uri query in
        generic_json_call meth ?body uri

      method call_service
          : 'm 'p 'q 'i 'o.
            (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t ->
            'p ->
            'q ->
            'i ->
            'o tzresult Lwt.t =
        fun service params query body ->
          call_service media_types ~logger ~base service params query body

      method call_streamed_service
          : 'm 'p 'q 'i 'o.
            (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t ->
            on_chunk:('o -> unit) ->
            on_close:(unit -> unit) ->
            'p ->
            'q ->
            'i ->
            (unit -> unit) tzresult Lwt.t =
        fun service ~on_chunk ~on_close params query body ->
          call_streamed_service
            media_types
            service
            ~logger
            ~base
            ~on_chunk
            ~on_close
            params
            query
            body

      method base = base
    end
end
