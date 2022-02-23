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

  type config = {
    media_type : Media_type.t list;
    endpoint : Uri.t;
    logger : logger;
  }

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

  type content_type = Media_type.Content_type.t

  type content = Cohttp_lwt.Body.t * content_type option * Media_type.t option

  val generic_media_type_call :
    ?headers:(string * string) list ->
    accept:Media_type.t list ->
    ?body:Data_encoding.json ->
    [< RPC_service.meth] ->
    Uri.t ->
    RPC_context.generic_call_result tzresult Lwt.t

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

  type content_type = Media_type.Content_type.t

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

  let handle_error (body, content_type, _) f =
    Cohttp_lwt.Body.is_empty body >>= fun empty ->
    if empty then return (content_type, f None)
    else
      Cohttp_lwt.Body.to_string body >>= fun body ->
      return (content_type, f (Some body))

  let jsonify_other meth uri content_type error :
      (Data_encoding.json, Data_encoding.json option) RPC_context.rest_result
      Lwt.t =
    let jsonify_body string_body =
      match content_type with
      | Some ("application", "json") | None -> (
          match Data_encoding.Json.from_string string_body with
          | Ok json_body -> return json_body
          | Error msg ->
              request_failed
                meth
                uri
                (Unexpected_content
                   {
                     content = string_body;
                     media_type = Media_type.(name json);
                     error = msg;
                   }))
      | Some content_type ->
          request_failed
            meth
            uri
            (Unexpected_content_type
               {
                 received =
                   Format.asprintf "%a" Media_type.Content_type.pp content_type;
                 acceptable = [Media_type.(name json)];
                 body = string_body;
               })
    in
    let jsonify_body_opt = function
      | None -> return_none
      | Some string_body ->
          jsonify_body string_body >>=? fun json_body -> return_some json_body
    in
    match error with
    | `Conflict s -> jsonify_body_opt s >|=? fun s -> `Conflict s
    | `Error s -> jsonify_body_opt s >|=? fun s -> `Error s
    | `Forbidden s -> jsonify_body_opt s >|=? fun s -> `Forbidden s
    | `Not_found s -> jsonify_body_opt s >|=? fun s -> `Not_found s
    | `Gone s -> jsonify_body_opt s >|=? fun s -> `Gone s
    | `Unauthorized s -> jsonify_body_opt s >|=? fun s -> `Unauthorized s
    | `Ok s -> jsonify_body s >|=? fun s -> `Ok s

  let post_process_error_responses response meth uri accept =
    match response with
    | `Conflict body -> handle_error body (fun v -> `Conflict v)
    | `Error body -> handle_error body (fun v -> `Error v)
    | `Forbidden body -> handle_error body (fun v -> `Forbidden v)
    | `Not_found body ->
        (* The client's proxy mode matches on the `Not_found returned here,
           to detect that a local RPC is unavailable at generic_json_call,
           and hence that delegation to the endpoint must be done. *)
        handle_error body (fun v -> `Not_found v)
    | `Gone body -> handle_error body (fun v -> `Gone v)
    | `Unauthorized body -> handle_error body (fun v -> `Unauthorized v)
    | `Ok (body, (Some _ as content_type), _) ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        request_failed
          meth
          uri
          (Unexpected_content_type
             {
               received =
                 Format.asprintf
                   "%a"
                   (Format.pp_print_option Media_type.Content_type.pp)
                   content_type;
               acceptable = List.map Media_type.name accept;
               body;
             })
    | `Ok (body, None, _) ->
        Cohttp_lwt.Body.to_string body >>= fun body -> return (None, `Ok body)

  let post_process_json_response ~body meth uri =
    match Data_encoding.Json.from_string body with
    | Ok json -> return json
    | Error msg ->
        request_failed
          meth
          uri
          (Unexpected_content
             {content = body; media_type = Media_type.(name json); error = msg})

  let post_process_bson_response ~body meth uri =
    match
      Json_repr_bson.bytes_to_bson
        ~laziness:false
        ~copy:false
        (Bytes.of_string body)
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
          (Json_repr.convert
             (module Json_repr_bson.Repr)
             (module Json_repr.Ezjsonm)
             bson)

  let generic_json_call ?headers ?body meth uri =
    let body =
      Option.map
        (fun b -> Cohttp_lwt.Body.of_string (Data_encoding.Json.to_string b))
        body
    in
    let media = Media_type.json in
    generic_call ?headers ?body meth ~accept:Media_type.[json; bson] ~media uri
    >>=? fun response ->
    match response with
    | `Ok (body, Some ("application", "json"), _) ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        post_process_json_response ~body meth uri >>=? fun body ->
        return (`Ok body)
    | `Ok (body, Some ("application", "bson"), _) ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        post_process_bson_response ~body meth uri >>=? fun body ->
        return (`Ok body)
    | _ ->
        post_process_error_responses response meth uri Media_type.[json; bson]
        >>=? fun (content_type, other) ->
        jsonify_other meth uri content_type other

  (* This function checks that the content type of the answer belongs to accepted ones in [accept]. If not, it is processed as an error. If the answer lacks content-type, the response is decoded as JSON if possible. *)
  let generic_media_type_call ?headers ~accept ?body meth uri :
      RPC_context.generic_call_result tzresult Lwt.t =
    let body =
      Option.map
        (fun b -> Cohttp_lwt.Body.of_string (Data_encoding.Json.to_string b))
        body
    in
    let media = Media_type.json in
    generic_call meth ?headers ~accept ?body ~media uri >>=? fun response ->
    match response with
    | `Ok (body, Some ("application", "octet-stream"), _)
      when List.mem ~equal:( == ) Media_type.octet_stream accept -> (
        Cohttp_lwt.Body.to_string body >>= fun body ->
        (* The binary RPCs are prefixed with a size header, we remove it here. *)
        match Data_encoding.Binary.of_string_opt Data_encoding.string body with
        | Some response -> return (`Binary (`Ok response))
        | None -> return (`Binary (`Error (Some body))))
    | `Ok (body, Some ("application", "json"), _)
      when List.mem ~equal:( == ) Media_type.json accept ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        post_process_json_response ~body meth uri >>=? fun body ->
        return (`Json (`Ok body))
    | `Ok (body, Some ("application", "bson"), _)
      when List.mem ~equal:( == ) Media_type.bson accept ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        post_process_bson_response ~body meth uri >>=? fun body ->
        return (`Json (`Ok body))
    | _ -> (
        post_process_error_responses response meth uri accept
        >>=? fun (content_type, other_resp) ->
        (* We attempt to decode in JSON. It might
           work. *)
        jsonify_other meth uri content_type other_resp >>= function
        | Ok jsonified -> return (`Json jsonified)
        | Error _ -> return (`Other (content_type, other_resp)))

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

  type config = {
    media_type : Media_type.t list;
    endpoint : Uri.t;
    logger : logger;
  }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {media_type; endpoint; logger = _} -> (media_type, endpoint))
      (fun (media_type, endpoint) ->
        {media_type; endpoint; logger = null_logger})
      (obj2
         (req "media-type" (list Media_type.encoding))
         (req "endpoint" RPC_encoding.uri_encoding))

  let default_config =
    {
      media_type = Media_type.all_media_types;
      endpoint = Uri.of_string "http://localhost:8732";
      logger = null_logger;
    }

  class http_ctxt config media_types : RPC_context.json =
    let base = config.endpoint in
    let logger = config.logger in
    let call meth uri f =
      let path = Uri.path uri and query = Uri.query uri in
      let prefix = Uri.path base in
      let prefixed_path = if prefix = "" then path else prefix ^ "/" ^ path in
      let uri = Uri.with_path base prefixed_path in
      let uri = Uri.with_query uri query in
      f meth uri
    in
    object
      method generic_json_call meth ?body uri =
        call meth uri (generic_json_call ?body)

      method generic_media_type_call meth ?body uri =
        call meth uri (generic_media_type_call ?body ~accept:config.media_type)

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
