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
  | Rpc_generic_error
  | Rpc_not_found of string option
  | Rpc_unauthorized
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
        (obj1 (req "kind" (constant "rpc_generic_error")))
        (function Rpc_generic_error -> Some () | _ -> None)
        (fun () -> Rpc_generic_error);
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
        (obj1 (req "kind" (constant "rpc_unauthorized")))
        (function Rpc_unauthorized -> Some () | _ -> None)
        (fun () -> Rpc_unauthorized);
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

exception Rpc_dir_creation_failure of tztrace

let media_types = [Tezos_rpc_http.Media_type.json]

module NullLogger = struct
  let debug fmt = Format.kasprintf ignore fmt

  let log_info fmt = Format.kasprintf ignore fmt

  let log_notice fmt = Format.kasprintf ignore fmt

  let warn fmt = Format.kasprintf ignore fmt

  let log_error fmt = Format.kasprintf ignore fmt

  let lwt_debug fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_info fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_notice fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_warn fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_error fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt
end

module Call =
  Resto_cohttp_self_serving_client.Self_serving_client.Make
    (RPC_encoding)
    (NullLogger)

let local_ctxt (directory : unit RPC_directory.t) : RPC_context.json =
  let local_client =
    Call.launch ?cors:None ?agent:None ~media_types directory
  in
  let module C = Tezos_rpc_http_client.RPC_client.Make ((val local_client)) in
  let base = Uri.empty in
  object
    method base = base

    method generic_json_call meth ?body uri =
      C.generic_json_call meth ?body uri

    method call_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t -> 'p ->
          'q -> 'i -> 'o tzresult Lwt.t =
      fun service params query body ->
        C.call_service media_types ~base service params query body

    method call_streamed_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t ->
          on_chunk:('o -> unit) -> on_close:(unit -> unit) -> 'p -> 'q -> 'i ->
          (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query body ->
        C.call_streamed_service
          media_types
          service
          ~base
          ~on_chunk
          ~on_close
          params
          query
          body
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
      | Rpc_generic_error ->
          Format.fprintf ppf ": RPC generic error"
      | Rpc_not_found None ->
          Format.fprintf ppf ": RPC not found"
      | Rpc_not_found (Some desc) ->
          Format.fprintf ppf ": RPC not found (%s)" desc
      | Rpc_unauthorized ->
          Format.fprintf ppf ": RPC unauthorized"
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
