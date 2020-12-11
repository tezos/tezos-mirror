(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Directory = RPC_directory
module Service = RPC_service

type rpc_error =
  | Rpc_generic_error of string option
  | Rpc_not_found of string option
  | Rpc_unauthorized of string option
  | Rpc_unexpected_type_of_failure
  | Rpc_cannot_parse_path
  | Rpc_cannot_parse_query
  | Rpc_cannot_parse_body
  | Rpc_streams_not_handled

(* FIXME @smelc Useless? *)
type error += Not_implemented_in_local_RPC_mode of Uri.t

type error += Local_RPC_error of rpc_error

let rpc_error_encoding =
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"Rpc_generic_error"
        (obj2
           (req "kind" (constant "rpc_generic_error"))
           (opt "description" string))
        (function Rpc_generic_error m -> Some ((), m) | _ -> None)
        (fun ((), m) -> Rpc_generic_error m);
      case
        (Tag 1)
        ~title:"Rpc_not_found"
        (obj2
           (req "kind" (constant "rpc_not_found"))
           (opt "description" string))
        (function Rpc_not_found m -> Some ((), m) | _ -> None)
        (fun ((), m) -> Rpc_not_found m);
      case
        (Tag 2)
        ~title:"Rpc_unauthorized"
        (obj2
           (req "kind" (constant "rpc_unauthorized"))
           (opt "description" string))
        (function Rpc_unauthorized m -> Some ((), m) | _ -> None)
        (fun ((), m) -> Rpc_unauthorized m);
      case
        (Tag 3)
        ~title:"Rpc_unexpected_type_of_failure"
        (obj1 (req "kind" (constant "rpc_unexpected_failure")))
        (function Rpc_unexpected_type_of_failure -> Some () | _ -> None)
        (fun () -> Rpc_unexpected_type_of_failure);
      case
        (Tag 4)
        ~title:"Rpc_cannot_parse_path"
        (obj1 (req "kind" (constant "rpc_cannot_parse")))
        (function Rpc_cannot_parse_path -> Some () | _ -> None)
        (fun () -> Rpc_cannot_parse_path);
      case
        (Tag 5)
        ~title:"Rpc_cannot_parse_query"
        (obj1 (req "kind" (constant "rpc_cannot_query")))
        (function Rpc_cannot_parse_query -> Some () | _ -> None)
        (fun () -> Rpc_cannot_parse_query);
      case
        (Tag 6)
        ~title:"Rpc_cannot_parse_body"
        (obj1 (req "kind" (constant "rpc_cannot_body")))
        (function Rpc_cannot_parse_body -> Some () | _ -> None)
        (fun () -> Rpc_cannot_parse_body);
      case
        (Tag 7)
        ~title:"Rpc_streams_not_handled"
        (obj1 (req "kind" (constant "rpc_streams_not_handled")))
        (function Rpc_streams_not_handled -> Some () | _ -> None)
        (fun () -> Rpc_streams_not_handled) ]

let rec print_path : type pr p. (pr, p) Resto.Internal.path -> string list =
 fun path ->
  match path with
  | Root ->
      []
  | Static (path, s) ->
      s :: print_path path
  | Dynamic (path, arg) ->
      Printf.sprintf "<%s>" arg.descr.name :: print_path path
  | DynamicTail (path, arg) ->
      Printf.sprintf "<%s>" arg.descr.name :: print_path path

let print_service : type p q i o. (_, _, p, q, i, o) Service.t -> string =
 fun serv ->
  let iserv = Service.Internal.to_service serv in
  String.concat "/" (List.rev (print_path iserv.path))

(** The subset of resto's client.mli that we implement,
    Signatures are slightly different (we have less error cases
    and support a single [Tezos_rpc_http.Media_type]). *)
module type LOCAL_CLIENT = sig
  val generic_call :
    Resto.meth ->
    ?body:Cohttp_lwt.Body.t ->
    Uri.t ->
    unit Directory.t ->
    (Data_encoding.json, Data_encoding.json option) RPC_context.rest_result
    Lwt.t

  val call_service :
    ([< Resto.meth], unit, 'p, 'q, 'i, 'o) Service.t ->
    'p ->
    'q ->
    'i ->
    unit Directory.t ->
    'o tzresult Lwt.t

  val call_streamed_service :
    ?base:Uri.t ->
    ([< Resto.meth], unit, 'p, 'q, 'i, 'o) Service.t ->
    on_chunk:('o -> unit) ->
    on_close:(unit -> unit) ->
    'p ->
    'q ->
    'i ->
    unit Directory.t ->
    (unit -> unit) tzresult Lwt.t
end

module LocalClient : LOCAL_CLIENT = struct
  let prepare (type i) media_types ?base
      (service : (_, _, _, _, i, _) Service.t) params query body :
      ( Resto.meth
      * Uri.t
      * Cohttp_lwt.Body.t option
      * Tezos_rpc_http.Media_type.t option )
      Lwt.t =
    let media =
      match Tezos_rpc_http.Media_type.first_complete_media media_types with
      | None ->
          invalid_arg "Resto_local_client.prepare"
      | Some (_, m) ->
          m
    in
    let {Service.meth; uri; input} =
      Service.forge_request ?base service params query
    in
    ( match input with
    | Service.No_input ->
        Lwt.return (None, None)
    | Service.Input input ->
        let body = media.construct input body in
        Lwt.return (Some (Cohttp_lwt.Body.of_string body), Some media) )
    >>= fun (body, media) -> Lwt.return (meth, uri, body, media)

  let generic_call meth ?body uri directory =
    let path = Uri.pct_decode (Uri.path uri) in
    let path = Resto_cohttp.Utils.split_path path in
    Directory.lookup directory () meth path
    >>= fun result ->
    match result with
    | Error (`Cannot_parse_path _) ->
        Error_monad.fail (Local_RPC_error Rpc_cannot_parse_path)
    | Error (`Method_not_allowed _) ->
        Error_monad.fail (Local_RPC_error (Rpc_unauthorized None))
    | Error `Not_found ->
        Error_monad.fail (Local_RPC_error (Rpc_not_found None)) (*TODO*)
    | Ok (Directory.Service s) -> (
        ( match
            Resto.Query.parse
              s.types.query
              (List.map
                 (fun (k, l) -> (k, String.concat "," l))
                 (Uri.query uri))
          with
        | exception Resto.Query.Invalid _s ->
            Error_monad.fail (Local_RPC_error Rpc_cannot_parse_query)
        | query ->
            return query )
        >>=? fun query ->
        let output_encoding = s.types.output in
        let output = Data_encoding.Json.construct output_encoding in
        let error = Option.map (Data_encoding.Json.construct s.types.error) in
        ( match s.types.input with
        | Service.No_input ->
            s.handler query () >>= return
        | Service.Input input -> (
            let body = Option.value ~default:Cohttp_lwt.Body.empty body in
            Cohttp_lwt.Body.to_string body
            >>= fun body ->
            match Tezos_rpc_http.Media_type.json.destruct input body with
            | Error _s ->
                Error_monad.fail (Local_RPC_error Rpc_cannot_parse_body)
            | Ok body ->
                s.handler query body >>= return ) )
        >>=? function
        | `Ok o ->
            let body = output o in
            return (`Ok body)
        | `Error e ->
            let body = error e in
            return (`Error body)
        | `Not_found e ->
            let body = error e in
            return (`Not_found body)
        | `Unauthorized e ->
            let body = error e in
            return (`Unauthorized body)
        | `Forbidden e ->
            let body = error e in
            return (`Forbidden body)
        | `Conflict e ->
            let body = error e in
            return (`Conflict body)
        | `OkStream _ostream ->
            Error_monad.fail (Local_RPC_error Rpc_streams_not_handled)
        | `Created _ | `No_content | `Gone _ ->
            Error_monad.fail (Local_RPC_error (Rpc_generic_error None)) )

  let media_type = Tezos_rpc_http.Media_type.json

  let media_types = [media_type]

  let create_error (service : (_, _, _, _, _, _) Service.t) = function
    | `Error None ->
        Local_RPC_error (Rpc_generic_error None)
    | `Error (Some err) ->
        Local_RPC_error
          (Rpc_generic_error (Some (Data_encoding.Json.to_string err)))
    | `Not_found None ->
        let path = print_service service in
        Local_RPC_error (Rpc_not_found (Some path))
    | `Not_found (Some err) ->
        let path = print_service service in
        Local_RPC_error
          (Rpc_not_found (Some (path ^ " " ^ Data_encoding.Json.to_string err)))
    | `Unauthorized None ->
        Local_RPC_error (Rpc_unauthorized None)
    | `Unauthorized (Some err) ->
        Local_RPC_error
          (Rpc_unauthorized (Some (Data_encoding.Json.to_string err)))
    | _ ->
        Local_RPC_error Rpc_unexpected_type_of_failure

  let call_service (type p q i o) (service : (_, _, p, q, i, o) Service.t)
      (params : p) (query : q) (body : i) (directory : unit Directory.t) :
      o tzresult Lwt.t =
    prepare media_types service params query body
    >>= fun (meth, uri, body, _media_opt) ->
    generic_call meth ?body uri directory
    >>=? fun res ->
    match res with
    | `Ok output ->
        let o_data_encoding = Service.output_encoding service in
        let o = Data_encoding.Json.destruct o_data_encoding output in
        return o
    | e ->
        fail @@ create_error service e

  let call_streamed_service ?base service ~on_chunk ~on_close params query body
      directory =
    prepare media_types ?base service params query body
    >>= fun (meth, uri, body, _media_opt) ->
    generic_call meth ?body uri directory
    >>=? fun call_result ->
    match call_result with
    | `Ok output -> (
        let output_string = Data_encoding.Json.to_string output in
        let stream =
          Cohttp_lwt.Body.of_string output_string |> Cohttp_lwt.Body.to_stream
        in
        Lwt_stream.get stream
        >>= function
        | None ->
            on_close () ;
            return (function () -> ())
        | Some chunk ->
            let buffer = Buffer.create 2048 in
            let output = Service.output_encoding service in
            let rec loop = function
              | None ->
                  on_close () ; Lwt.return_unit
              | Some chunk -> (
                  Buffer.add_string buffer chunk ;
                  let data = Buffer.contents buffer in
                  match media_type.destruct output data with
                  | Ok body ->
                      Buffer.reset buffer ;
                      on_chunk body ;
                      Lwt_stream.get stream >>= loop
                  | Error _msg ->
                      Lwt_stream.get stream >>= loop )
            in
            ignore (loop (Some chunk) : unit Lwt.t) ;
            return (fun () ->
                ignore
                  (Lwt_stream.junk_while (fun _ -> true) stream : unit Lwt.t) ;
                ()) )
    | e ->
        fail @@ create_error service e
end

class local_ctxt (directory : unit RPC_directory.t) : RPC_context.json =
  object
    method base = Uri.empty

    method call_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t -> 'p ->
          'q -> 'i -> 'o tzresult Lwt.t =
      fun service params query input ->
        let fail_not_found () =
          let description = print_service service in
          (* The proxy mode relies on this code, because it matches
             Local_RPC_error (Rpc_not_found _) to detect RPCs
             that cannot be done locally, and delegates them to the node
             instead. *)
          fail (Local_RPC_error (Rpc_not_found (Some description)))
        in
        try
          LocalClient.call_service service params query input directory
          >>= function
          | Error (Tezos_rpc.RPC_context.Not_found _ :: _) ->
              fail_not_found ()
          | _ as res ->
              Lwt.return res
        with Not_found -> fail_not_found ()

    method call_streamed_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
          on_chunk:('o -> unit) -> on_close:(unit -> unit) -> 'p -> 'q -> 'i ->
          (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query input ->
        LocalClient.call_streamed_service
          service
          ~on_chunk
          ~on_close
          params
          query
          input
          directory

    method generic_json_call
        : RPC_service.meth ->
          ?body:Data_encoding.json ->
          Uri.t ->
          ( Data_encoding.json,
            Data_encoding.json option )
          RPC_context.rest_result
          Lwt.t =
      fun meth ?body uri ->
        let body =
          match body with
          | None ->
              Cohttp_lwt.Body.empty
          | Some body ->
              Data_encoding.Json.to_string body |> Cohttp_lwt.Body.of_string
        in
        LocalClient.generic_call (meth :> Resto.meth) ~body uri directory
  end

let () =
  register_error_kind
    `Permanent
    ~id:"local_rpc_client.request_failed"
    ~title:"Local mockup RPC request failure"
    ~description:"An RPC request failed in mockup mode"
    ~pp:(fun ppf error ->
      Format.fprintf ppf "A mockup RPC request failed" ;
      match error with
      | Rpc_generic_error None ->
          Format.fprintf ppf ": RPC generic error"
      | Rpc_generic_error (Some desc) ->
          Format.fprintf ppf ": RPC generic error (%s)" desc
      | Rpc_not_found None ->
          Format.fprintf ppf ": RPC not found"
      | Rpc_not_found (Some desc) ->
          Format.fprintf ppf ": RPC not found (%s)" desc
      | Rpc_unauthorized None ->
          Format.fprintf ppf ": RPC unauthorized"
      | Rpc_unauthorized (Some desc) ->
          Format.fprintf ppf ": RPC unauthorized (%s)" desc
      | Rpc_unexpected_type_of_failure ->
          Format.fprintf ppf ": unexpected type of failure"
      | Rpc_cannot_parse_path ->
          Format.fprintf ppf ": cannot parse path"
      | Rpc_cannot_parse_query ->
          Format.fprintf ppf ": cannot parse query"
      | Rpc_cannot_parse_body ->
          Format.fprintf ppf ": cannot parse body"
      | Rpc_streams_not_handled ->
          Format.fprintf ppf ": streamed RPCs not handled yet")
    (let open Data_encoding in
    obj1 (req "rpc_error" rpc_error_encoding))
    (function Local_RPC_error rpc_error -> Some rpc_error | _ -> None)
    (fun rpc_error -> Local_RPC_error rpc_error)

let () =
  register_error_kind
    `Permanent
    ~id:"local_rpc_client.not_implemented_in_local_mode"
    ~title:"Local mockup RPC request not implemented"
    ~description:"A specific RPC is not implemented in mockup mode"
    ~pp:(fun ppf (uri : Uri.t) ->
      Format.fprintf ppf "RPC not implemented in mockup mode: %a" Uri.pp uri)
    Data_encoding.(obj1 (req "uri" RPC_encoding.uri_encoding))
    (function Not_implemented_in_local_RPC_mode uri -> Some uri | _ -> None)
    (fun uri -> Not_implemented_in_local_RPC_mode uri)
