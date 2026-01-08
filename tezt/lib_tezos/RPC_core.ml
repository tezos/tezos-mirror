(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
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

open Base

type verb = GET | PUT | POST | PATCH | DELETE

type data = Data of JSON.u | File of string

let show_verb = function
  | GET -> "GET"
  | PUT -> "PUT"
  | POST -> "POST"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"

let cohttp_of_verb : verb -> Cohttp.Code.meth = function
  | GET -> `GET
  | PUT -> `PUT
  | POST -> `POST
  | PATCH -> `PATCH
  | DELETE -> `DELETE

type 'result t = {
  verb : verb;
  path : string list;
  query_string : (string * string) list;
  data : data option;
  decode : JSON.t -> 'result;
}

module HeaderMap = Map.Make (String)

let make ?data ?(query_string = []) verb path decode =
  {verb; path; query_string; data; decode}

let decode_raw ?(origin = "RPC response") rpc raw =
  rpc.decode (JSON.parse ~origin raw)

let decode rpc json = rpc.decode json

type 'a response = {body : 'a; code : int; headers : string HeaderMap.t}

let check_string_response ?(body_rex = "") ~code (response : string response) =
  Check.(
    (code = response.code)
      int
      ~error_msg:"Unexpected HTTP status: expecting %L, got %R.") ;
  Check.(response.body =~ rex body_rex)
    ~error_msg:"Unexpected HTTP body: expecting a pattern %R, got a body %L."

let make_uri endpoint rpc =
  Uri.make
    ~scheme:(Endpoint.rpc_scheme endpoint)
    ~host:(Endpoint.rpc_host endpoint)
    ~port:(Endpoint.rpc_port endpoint)
    ~path:(String.concat "/" (endpoint.path :: rpc.path))
    ~query:(List.map (fun (k, v) -> (k, [v])) rpc.query_string)
    ()

type rpc_hooks = {
  on_request : verb -> uri:string -> data option -> unit;
  on_response : Cohttp.Code.status_code -> body:string -> unit;
}

module type CALLERS = sig
  type uri_provider

  val call :
    ?rpc_hooks:rpc_hooks ->
    ?log_request:bool ->
    ?log_response_status:bool ->
    ?log_response_body:bool ->
    uri_provider ->
    'result t ->
    'result Lwt.t

  val call_raw :
    ?rpc_hooks:rpc_hooks ->
    ?log_request:bool ->
    ?log_response_status:bool ->
    ?log_response_body:bool ->
    ?extra_headers:(string * string) list ->
    uri_provider ->
    'result t ->
    string response Lwt.t

  val call_json :
    ?rpc_hooks:rpc_hooks ->
    ?log_request:bool ->
    ?log_response_status:bool ->
    ?log_response_body:bool ->
    uri_provider ->
    'result t ->
    JSON.t response Lwt.t
end

let call_raw ?rpc_hooks ?(log_request = true) ?(log_response_status = true)
    ?(log_response_body = true) ?(extra_headers = []) endpoint rpc =
  let uri = make_uri endpoint rpc in
  let () =
    Option.iter
      (fun {on_request; _} ->
        on_request rpc.verb ~uri:(Uri.to_string uri) rpc.data)
      rpc_hooks
  in
  if log_request then
    Log.debug
      ~color:Log.Color.bold
      ~prefix:"RPC"
      "%s %s"
      (show_verb rpc.verb)
      (Uri.to_string uri) ;
  let headers =
    match rpc.data with
    | None -> []
    | Some _ -> [("Content-Type", "application/json")]
  in
  let headers = headers @ extra_headers in
  let* response, response_body =
    Cohttp_lwt_unix.Client.call
      ~headers:(Cohttp.Header.of_list headers)
      ?body:
        (Option.map
           (function
             | Data body -> Cohttp_lwt.Body.of_string (JSON.encode_u body)
             | File filename -> Cohttp_lwt.Body.of_string (read_file filename))
           rpc.data)
      (cohttp_of_verb rpc.verb)
      uri
  in
  if log_response_status then
    Log.debug
      ~prefix:"RPC"
      "RPC response: %s"
      (Cohttp.Code.string_of_status response.status) ;
  let* body = Cohttp_lwt.Body.to_string response_body in
  let () =
    Option.iter
      (fun {on_response; _} -> on_response response.status ~body)
      rpc_hooks
  in
  if log_response_body then Log.debug ~prefix:"RPC" "%s" body ;
  let headers =
    Cohttp.Header.to_list (Cohttp.Response.headers response)
    |> List.to_seq |> HeaderMap.of_seq
  in
  return {body; code = Cohttp.Code.code_of_status response.status; headers}

let call_json ?rpc_hooks ?log_request ?log_response_status
    ?(log_response_body = true) endpoint rpc =
  let* response =
    call_raw
      endpoint
      ?rpc_hooks
      ?log_request
      ?log_response_status
      ~log_response_body:false
      rpc
  in
  match JSON.parse ~origin:"RPC response" response.body with
  | exception (JSON.Error _ as exn) ->
      if log_response_body then Log.debug ~prefix:"RPC" "%s" response.body ;
      raise exn
  | body ->
      if log_response_body then Log.debug ~prefix:"RPC" "%s" (JSON.encode body) ;
      return {response with body}

let check_status_code node rpc code =
  if not (Cohttp.Code.is_success code) then
    Test.fail
      "%s %s returned %s"
      (show_verb rpc.verb)
      (Uri.to_string (make_uri node rpc))
      (Cohttp.Code.string_of_status (Cohttp.Code.status_of_code code))

let call ?rpc_hooks ?log_request ?log_response_status ?log_response_body node
    rpc =
  let* response =
    call_json
      ?rpc_hooks
      ?log_request
      ?log_response_status
      ?log_response_body
      node
      rpc
  in
  check_status_code node rpc response.code ;
  return (rpc.decode response.body)
