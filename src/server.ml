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

open Lwt.Infix
module ConnectionMap = Map.Make (Cohttp.Connection)

module type LOGGING = sig
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_info : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_notice : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warn : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_error : ('a, Format.formatter, unit, unit) format4 -> 'a

  val lwt_debug : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_info : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_notice : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_warn : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_error : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
end

let ( >>=? ) = Lwt_result.bind

module Make_internal_only (Encoding : Resto.ENCODING) = struct
  open Cohttp
  module Service = Resto.MakeService (Encoding)
  module Directory = Resto_directory.Make (Encoding)
  module Media_type = Media_type.Make (Encoding)

  module Internal = struct
    type medias = {
      media_types : Media_type.t list;
      default_media_type : string * Media_type.t;
    }

    let default_agent = "OCaml-Resto"

    let default_media_type media_types =
      match Media_type.first_complete_media media_types with
      | None ->
          invalid_arg "Resto_directory_cohttp.launch(empty media type list)"
      | Some ((l, r), m) ->
          (l ^ "/" ^ r, m)

    let invalid_cors (cors : Cors.t) headers =
      cors.allowed_origins <> [] && not (Cors.check_host headers cors)

    let invalid_cors_response agent =
      let headers =
        Cohttp.Header.init_with
          (Format.asprintf "X-%s-CORS-Error" agent)
          "invalid host"
      in
      Lwt.return_ok
        (Response.make ~headers ~status:`Forbidden (), Cohttp_lwt.Body.empty)

    let input_media_type ?headers medias =
      match headers with
      | None ->
          Ok (snd medias.default_media_type)
      | Some headers -> (
        match Header.get headers "content-type" with
        | None ->
            Ok (snd medias.default_media_type)
        | Some content_type -> (
          match Resto.Utils.split_path content_type with
          | [x; y] -> (
            match Media_type.find_media (x, y) medias.media_types with
            | None ->
                Error (`Unsupported_media_type content_type)
            | Some media_type ->
                Ok media_type )
          | _ ->
              Error (`Unsupported_media_type content_type) ) )

    let output_content_media_type ?headers medias =
      match headers with
      | None ->
          Ok medias.default_media_type
      | Some headers -> (
        match Header.get headers "accept" with
        | None ->
            Ok medias.default_media_type
        | Some accepted -> (
          match
            Media_type.resolve_accept_header medias.media_types (Some accepted)
          with
          | None ->
              Error `Not_acceptable
          | Some media_type ->
              Ok media_type ) )

    let handle_error medias
        (error :
          [< `Cannot_parse_body of string
          | `Cannot_parse_path of string list * Resto.Arg.descr * string
          | `Cannot_parse_query of string
          | `Method_not_allowed of [< Resto.meth] list
          | `Not_acceptable
          | `Not_found
          | `Not_implemented
          | `Unsupported_media_type of 'a ]) :
        Cohttp.Response.t * Cohttp_lwt.Body.t =
      let open Resto.Arg in
      match error with
      | `Not_implemented ->
          (Response.make ~status:`Not_implemented (), Cohttp_lwt.Body.empty)
      | `Method_not_allowed methods ->
          let headers = Header.init () in
          let headers =
            Header.add_multi
              headers
              "allow"
              (List.map Resto.string_of_meth methods)
          in
          ( Response.make ~status:`Method_not_allowed ~headers (),
            Cohttp_lwt.Body.empty )
      | `Cannot_parse_path (context, arg, value) ->
          let headers = Header.init () in
          let headers = Header.add headers "content-type" "text/plain" in
          ( Response.make ~status:`Bad_request ~headers (),
            Format.kasprintf
              Cohttp_lwt.Body.of_string
              "Failed to parsed an argument in path. After \"%s\", the value \
               \"%s\" is not acceptable for type \"%s\""
              (String.concat "/" context)
              value
              arg.name )
      | `Cannot_parse_body s ->
          let headers = Header.init () in
          let headers = Header.add headers "content-type" "text/plain" in
          ( Response.make ~status:`Bad_request ~headers (),
            Format.kasprintf
              Cohttp_lwt.Body.of_string
              "Failed to parse the request body: %s"
              s )
      | `Cannot_parse_query s ->
          let headers = Header.init () in
          let headers = Header.add headers "content-type" "text/plain" in
          ( Response.make ~status:`Bad_request ~headers (),
            Format.kasprintf
              Cohttp_lwt.Body.of_string
              "Failed to parse the query string: %s"
              s )
      | `Not_acceptable ->
          let accepted_encoding =
            Media_type.acceptable_encoding medias.media_types
          in
          ( Response.make ~status:`Not_acceptable (),
            Cohttp_lwt.Body.of_string accepted_encoding )
      | `Unsupported_media_type _ ->
          ( Response.make ~status:`Unsupported_media_type (),
            Cohttp_lwt.Body.empty )
      | `Not_found ->
          (Response.make ~status:`Not_found (), Cohttp_lwt.Body.empty)

    let handle_rpc_answer ?headers output error answer =
      match answer with
      | `Ok o ->
          let body = output o in
          let encoding = Transfer.Fixed (Int64.of_int (String.length body)) in
          Lwt.return_ok
            ( Response.make ~status:`OK ~encoding ?headers (),
              Cohttp_lwt.Body.of_string body )
      | `Created s ->
          let headers = Header.init () in
          let headers =
            match s with
            | None ->
                headers
            | Some s ->
                Header.add headers "location" s
          in
          Lwt.return_ok
            (Response.make ~status:`Created ~headers (), Cohttp_lwt.Body.empty)
      | `No_content ->
          Lwt.return_ok
            (Response.make ~status:`No_content (), Cohttp_lwt.Body.empty)
      | `Unauthorized e ->
          let (body, encoding) = error e in
          let status = `Unauthorized in
          Lwt.return_ok (Response.make ~status ~encoding ?headers (), body)
      | `Forbidden e ->
          let (body, encoding) = error e in
          let status = `Forbidden in
          Lwt.return_ok (Response.make ~status ~encoding ?headers (), body)
      | `Gone e ->
          let (body, encoding) = error e in
          let status = `Gone in
          Lwt.return_ok (Response.make ~status ~encoding ?headers (), body)
      | `Not_found e ->
          let (body, encoding) = error e in
          let status = `Not_found in
          Lwt.return_ok (Response.make ~status ~encoding ?headers (), body)
      | `Conflict e ->
          let (body, encoding) = error e in
          let status = `Conflict in
          Lwt.return_ok (Response.make ~status ~encoding ?headers (), body)
      | `Error e ->
          let (body, encoding) = error e in
          let status = `Internal_server_error in
          Lwt.return_ok (Response.make ~status ~encoding ?headers (), body)

    let handle_options root cors headers path =
      let origin_header = Header.get headers "origin" in
      ( if (* Default OPTIONS handler for CORS preflight *)
           origin_header = None
      then Directory.allowed_methods root () path
      else
        match Header.get headers "Access-Control-Request-Method" with
        | None ->
            Directory.allowed_methods root () path
        | Some meth -> (
          match Code.method_of_string meth with
          | #Resto.meth as meth ->
              Directory.lookup root () meth path
              >>=? fun _handler -> Lwt.return_ok [meth]
          | _ ->
              Lwt.return_error `Not_found ) )
      >>=? fun cors_allowed_meths ->
      let headers = Header.init () in
      let headers =
        Header.add_multi
          headers
          "Access-Control-Allow-Methods"
          (List.map Resto.string_of_meth cors_allowed_meths)
      in
      let headers = Cors.add_headers headers cors origin_header in
      Lwt.return_ok
        ( Response.make ~flush:true ~status:`OK ~headers (),
          Cohttp_lwt.Body.empty )
  end
end

module Make (Encoding : Resto.ENCODING) (Log : LOGGING) = struct
  include Make_internal_only (Encoding)
  open Cohttp
  open Log

  type server = {
    root : unit Directory.directory;
    mutable streams : (unit -> unit) ConnectionMap.t;
    cors : Cors.t;
    medias : Internal.medias;
    stopper : unit Lwt.u;
    mutable acl : Acl.t;
    agent : string;
    mutable worker : unit Lwt.t;
  }

  let create_stream server con to_string s =
    let running = ref true in
    let stream =
      Lwt_stream.from (fun () ->
          if not !running then Lwt.return None
          else s.Resto_directory.Answer.next () >|= Option.map to_string)
    in
    let shutdown () =
      log_info "streamed connection closed %s" (Connection.to_string con) ;
      running := false ;
      s.shutdown () ;
      server.streams <- ConnectionMap.remove con server.streams
    in
    server.streams <- ConnectionMap.add con shutdown server.streams ;
    stream

  let ( >>? ) v f =
    match v with Ok x -> f x | Error err -> Lwt.return_error err

  let callback server ((_io, con) : Cohttp_lwt_unix.Server.conn) req body =
    let uri = Request.uri req in
    let path = Uri.path uri in
    lwt_log_info "(%s) receive request to %s" (Connection.to_string con) path
    >>= fun () ->
    let path = Resto.Utils.decode_split_path path in
    let req_headers = Request.headers req in
    ( match Request.meth req with
    | #Resto.meth when Internal.invalid_cors server.cors req_headers ->
        Internal.invalid_cors_response server.agent
    | #Resto.meth as meth -> (
        Directory.lookup server.root () meth path
        >>=? fun (Directory.Service s) ->
        Internal.input_media_type ~headers:req_headers server.medias
        >>? fun input_media_type ->
        lwt_debug
          "(%s) input media type %s"
          (Connection.to_string con)
          (Media_type.name input_media_type)
        >>= fun () ->
        Internal.output_content_media_type ~headers:req_headers server.medias
        >>? fun (output_content_type, output_media_type) ->
        ( match
            Resto.Query.parse
              s.types.query
              (List.map
                 (fun (k, l) -> (k, String.concat "," l))
                 (Uri.query uri))
          with
        | exception Resto.Query.Invalid s ->
            Lwt.return_error (`Cannot_parse_query s)
        | query ->
            Lwt.return_ok query )
        >>=? fun query ->
        lwt_debug
          "(%s) ouput media type %s"
          (Connection.to_string con)
          (Media_type.name output_media_type)
        >>= fun () ->
        let headers = Header.init () in
        let headers = Header.add headers "content-type" output_content_type in
        let headers =
          Cors.add_allow_origin
            headers
            server.cors
            (Header.get req_headers "origin")
        in
        ( if not @@ Acl.allowed server.acl ~meth ~path then
          Lwt.return_ok (`Unauthorized None)
        else
          match s.types.input with
          | Service.No_input ->
              s.handler query () >>= Lwt.return_ok
          | Service.Input input -> (
              Cohttp_lwt.Body.to_string body
              >>= fun body ->
              match input_media_type.destruct input body with
              | Error s ->
                  Lwt.return_error (`Cannot_parse_body s)
              | Ok body ->
                  s.handler query body >>= Lwt.return_ok ) )
        >>=? fun answer ->
        let output = output_media_type.construct s.types.output
        and error = function
          | None ->
              (Cohttp_lwt.Body.empty, Transfer.Fixed 0L)
          | Some e ->
              let s = output_media_type.construct s.types.error e in
              ( Cohttp_lwt.Body.of_string s,
                Transfer.Fixed (Int64.of_int (String.length s)) )
        in
        match answer with
        | ( `Ok _
          | `Created _
          | `No_content
          | `Unauthorized _
          | `Forbidden _
          | `Gone _
          | `Not_found _
          | `Conflict _
          | `Error _ ) as a ->
            Internal.handle_rpc_answer ~headers output error a
        | `OkStream o ->
            let body = create_stream server con output o in
            let encoding = Transfer.Chunked in
            Lwt.return_ok
              ( Response.make ~status:`OK ~encoding ~headers (),
                Cohttp_lwt.Body.of_stream body ) )
    | `HEAD ->
        (* TODO ??? *)
        Lwt.return_error `Not_implemented
    | `OPTIONS ->
        Internal.handle_options server.root server.cors req_headers path
        >>= fun res ->
        lwt_log_info "(%s) RPC preflight" (Connection.to_string con)
        >>= fun () -> Lwt.return res
    | _ ->
        Lwt.return_error `Not_implemented )
    >>= function
    | Ok answer ->
        Lwt.return answer
    | Error err ->
        Lwt.return @@ Internal.handle_error server.medias err

  (* Promise a running RPC server. *)

  let launch ?(host = "::") ?(cors = Cors.default)
      ?(agent = Internal.default_agent) ?(acl = Acl.Allow_all {except = []})
      ~media_types mode root =
    let default_media_type = Internal.default_media_type media_types in
    let (stop, stopper) = Lwt.wait () in
    let medias : Internal.medias = {media_types; default_media_type} in
    let server =
      {
        root;
        streams = ConnectionMap.empty;
        cors;
        medias;
        stopper;
        acl;
        agent;
        worker = Lwt.return_unit;
      }
    in
    Conduit_lwt_unix.init ~src:host ()
    >>= fun ctx ->
    let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
    server.worker <-
      (let conn_closed (_, con) =
         debug "connection closed %s" (Connection.to_string con) ;
         try ConnectionMap.find con server.streams () with Not_found -> ()
       and on_exn = function
         | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
             log_error
               "RPC server port already taken, the node will be shutdown" ;
             exit 1
         | Unix.Unix_error (ECONNRESET, _, _) | Unix.Unix_error (EPIPE, _, _)
           ->
             ()
         | exn ->
             Format.eprintf
               "@[<v 2>Uncaught (asynchronous) exception:@ %s@ %s@]%!"
               (Printexc.to_string exn)
               (Printexc.get_backtrace ())
       and callback (io, con) req body =
         Lwt.catch
           (fun () -> callback server (io, con) req body)
           (function
             | Not_found ->
                 let status = `Not_found in
                 let body = Cohttp_lwt.Body.empty in
                 Lwt.return (Response.make ~status (), body)
             | exn ->
                 let headers = Header.init () in
                 let headers =
                   Header.add headers "content-type" "text/ocaml.exception"
                 in
                 let status = `Internal_server_error in
                 let body =
                   Cohttp_lwt.Body.of_string (Printexc.to_string exn)
                 in
                 Lwt.return (Response.make ~status ~headers (), body))
       in
       Cohttp_lwt_unix.Server.create
         ~stop
         ~ctx
         ~mode
         ~on_exn
         (Cohttp_lwt_unix.Server.make ~callback ~conn_closed ())) ;
    lwt_log_info "Server started (agent: %s)" server.agent
    >>= fun () -> Lwt.return server

  let shutdown server =
    Lwt.wakeup_later server.stopper () ;
    server.worker
    >>= fun () ->
    ConnectionMap.iter (fun _ f -> f ()) server.streams ;
    Lwt.return_unit

  let set_acl server acl = server.acl <- acl
end
