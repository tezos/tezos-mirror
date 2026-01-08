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

type rpc_error =
  | Empty_answer
  | Connection_failed of string
  | Bad_request of string
  | Forbidden
  | Method_not_allowed of Tezos_rpc.Service.meth list
  | Unsupported_media_type of string option
  | Not_acceptable of {proposed : string; acceptable : string}
  | Unexpected_status_code of {
      code : Cohttp.Code.status_code;
      content : string;
      media_type : string option;
    }
  | Unexpected_content_type of {
      received : string;
      acceptable : string list;
      body : string;
    }
  | Unexpected_content of {
      content : string;
      media_type : string;
      error : string;
    }
  | OCaml_exception of string
  | Unauthorized_host of string option
  | Unauthorized_uri
  | Too_many_redirects of string
  | Redirect_without_location of string

type error +=
  | Request_failed of {
      meth : Tezos_rpc.Service.meth;
      uri : Uri.t;
      error : rpc_error;
    }

let rpc_error_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Empty_answer"
        (obj1 (req "kind" (constant "empty_answer")))
        (function Empty_answer -> Some () | _ -> None)
        (fun () -> Empty_answer);
      case
        (Tag 1)
        ~title:"Connection_failed"
        (obj2
           (req "kind" (constant "connection_failed"))
           (req "message" string))
        (function Connection_failed msg -> Some ((), msg) | _ -> None)
        (function (), msg -> Connection_failed msg);
      case
        (Tag 2)
        ~title:"Bad_request"
        (obj2 (req "kind" (constant "bad_request")) (req "message" string))
        (function Bad_request msg -> Some ((), msg) | _ -> None)
        (function (), msg -> Bad_request msg);
      case
        (Tag 3)
        ~title:"Method_not_allowed"
        (obj2
           (req "kind" (constant "method_not_allowed"))
           (req "allowed" (list Tezos_rpc.Service.meth_encoding)))
        (function Method_not_allowed meths -> Some ((), meths) | _ -> None)
        (function (), meths -> Method_not_allowed meths);
      case
        (Tag 4)
        ~title:"Unsupported_media_type"
        (obj2
           (req "kind" (constant "unsupported_media_type"))
           (opt "content_type" string))
        (function Unsupported_media_type m -> Some ((), m) | _ -> None)
        (function (), m -> Unsupported_media_type m);
      case
        (Tag 5)
        ~title:"Not_acceptable"
        (obj3
           (req "kind" (constant "not_acceptable"))
           (req "proposed" string)
           (req "acceptable" string))
        (function
          | Not_acceptable {proposed; acceptable} ->
              Some ((), proposed, acceptable)
          | _ -> None)
        (function
          | (), proposed, acceptable -> Not_acceptable {proposed; acceptable});
      case
        (Tag 6)
        ~title:"Unexpected_status_code"
        (obj4
           (req "kind" (constant "unexpected_status_code"))
           (req "code" uint16)
           (req "content" string)
           (opt "media_type" string))
        (function
          | Unexpected_status_code {code; content; media_type} ->
              Some ((), Cohttp.Code.code_of_status code, content, media_type)
          | _ -> None)
        (function
          | (), code, content, media_type ->
          let code = Cohttp.Code.status_of_code code in
          Unexpected_status_code {code; content; media_type});
      case
        (Tag 7)
        ~title:"Unexpected_content_type"
        (obj4
           (req "kind" (constant "unexpected_content_type"))
           (req "received" string)
           (req "acceptable" (list string))
           (req "body" string))
        (function
          | Unexpected_content_type {received; acceptable; body} ->
              Some ((), received, acceptable, body)
          | _ -> None)
        (function
          | (), received, acceptable, body ->
          Unexpected_content_type {received; acceptable; body});
      case
        (Tag 8)
        ~title:"Unexpected_content"
        (obj4
           (req "kind" (constant "unexpected_content"))
           (req "content" string)
           (req "media_type" string)
           (req "error" string))
        (function
          | Unexpected_content {content; media_type; error} ->
              Some ((), content, media_type, error)
          | _ -> None)
        (function
          | (), content, media_type, error ->
          Unexpected_content {content; media_type; error});
      case
        (Tag 9)
        ~title:"OCaml_exception"
        (obj2 (req "kind" (constant "ocaml_exception")) (req "content" string))
        (function OCaml_exception msg -> Some ((), msg) | _ -> None)
        (function (), msg -> OCaml_exception msg);
      case
        (Tag 10)
        ~title:"Unauthorized URI"
        unit
        (function Unauthorized_uri -> Some () | _ -> None)
        (function () -> Unauthorized_uri);
      case
        (Tag 11)
        ~title:"Too many redirects"
        (obj2
           (req "kind" (constant "too_many_redirects"))
           (req "content" string))
        (function Too_many_redirects msg -> Some ((), msg) | _ -> None)
        (function (), msg -> Too_many_redirects msg);
      case
        (Tag 12)
        ~title:"Redirect without location"
        (obj2
           (req "kind" (constant "redirect_without_location"))
           (req "content" string))
        (function Redirect_without_location msg -> Some ((), msg) | _ -> None)
        (function (), msg -> Redirect_without_location msg);
    ]

let pp_rpc_error ppf err =
  match err with
  | Empty_answer ->
      Format.fprintf ppf "The server answered with an empty response."
  | Connection_failed msg ->
      Format.fprintf ppf "Unable to connect to the node: \"%s\"" msg
  | Bad_request msg ->
      Format.fprintf
        ppf
        "@[<v 2>Oops! It looks like we forged an invalid HTTP request.@,%s@]"
        msg
  | Forbidden -> Format.fprintf ppf "@[<v 2>The server forbids access.@]"
  | Method_not_allowed meths ->
      Format.fprintf
        ppf
        "@[<v 2>The requested service only accepts the following method:@ %a@]"
        (Format.pp_print_list (fun ppf m ->
             Format.pp_print_string ppf (Tezos_rpc.Service.string_of_meth m)))
        meths
  | Unsupported_media_type None ->
      Format.fprintf
        ppf
        "@[<v 2>The server wants to known the media type we used.@]"
  | Unsupported_media_type (Some media) ->
      Format.fprintf
        ppf
        "@[<v 2>The server does not support the media type we used: %s.@]"
        media
  | Not_acceptable {proposed; acceptable} ->
      Format.fprintf
        ppf
        "@[<v 2>No intersection between the media types we accept and  the \
         ones the server is able to send.@,\
        \ We proposed: %s@,\
        \ The server is only able to serve: %s."
        proposed
        acceptable
  | Unexpected_status_code {code; content; _} ->
      Format.fprintf
        ppf
        "@[<v 2>Unexpected error %d:@,%S"
        (Cohttp.Code.code_of_status code)
        content
  | Unexpected_content_type {received; acceptable = _; body} ->
      Format.fprintf
        ppf
        "@[<v 0>The server answered with a media type we do not understand: \
         %s.@,\
         The response body was:@,\
         %s@]"
        received
        body
  | Unexpected_content {content; media_type; error} ->
      Format.fprintf
        ppf
        "@[<v 2>Failed to parse the answer (%s):@,\
         @[<v 2>error:@ %s@]@,\
         @[<v 2>content:@ %S@]@]"
        media_type
        error
        content
  | OCaml_exception msg ->
      Format.fprintf
        ppf
        "@[<v 2>The server failed with an unexpected exception:@ %s@]"
        msg
  | Unauthorized_host host ->
      Format.fprintf
        ppf
        "@[<v 2>The server refused connection to host \"%s\", please check the \
         node settings for CORS allowed origins.@]"
        (Option.value ~default:"" host)
  | Unauthorized_uri ->
      Format.fprintf
        ppf
        "@[<v 2>The server doesn't authorize this endpoint (ACL filtering).@]"
  | Too_many_redirects msg ->
      Format.fprintf ppf "@[<v 2>Too many redirects: %s.@]" msg
  | Redirect_without_location msg ->
      Format.fprintf ppf "@[<v 2>Redirect without location: %s.@]" msg

let () =
  register_error_kind
    `Permanent
    ~id:"rpc_client.request_failed"
    ~title:""
    ~description:""
    ~pp:(fun ppf (meth, uri, error) ->
      Format.fprintf
        ppf
        "@[<v 2>Rpc request failed:@  - meth: %s@  - uri: %s@  - error: %a@]"
        (Tezos_rpc.Service.string_of_meth meth)
        (Uri.to_string uri)
        pp_rpc_error
        error)
    Data_encoding.(
      obj3
        (req "meth" Tezos_rpc.Service.meth_encoding)
        (req "uri" Tezos_rpc.Encoding.uri_encoding)
        (req "error" rpc_error_encoding))
    (function
      | Request_failed {uri; error; meth} -> Some (meth, uri, error) | _ -> None)
    (fun (meth, uri, error) -> Request_failed {uri; meth; error})
