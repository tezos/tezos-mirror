(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type verb = Client.meth = GET | PUT | POST | PATCH | DELETE

type data = Client.data

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

type ('endpoint, 'result) t = {
  verb : verb;
  path : string list;
  query_string : (string * string) list;
  data : Client.data option;
  decode : JSON.t -> 'result;
  get_host : 'endpoint -> string;
  get_port : 'endpoint -> int;
  get_scheme : 'endpoint -> string;
}

let make ?data ?(query_string = []) ~get_host ~get_port ~get_scheme verb path
    decode =
  {verb; path; query_string; data; decode; get_host; get_port; get_scheme}

let decode_raw ?(origin = "RPC response") rpc raw =
  rpc.decode (JSON.parse ~origin raw)

let decode rpc json = rpc.decode json

type 'a response = {body : 'a; code : int}

let check_string_response ?(body_rex = "") ~code (response : string response) =
  Check.(
    (code = response.code)
      int
      ~error_msg:"Unexpected HTTP status: expecting %L, got %R.") ;
  Check.(response.body =~ rex body_rex)
    ~error_msg:"Unexpected HTTP body: expecting a pattern %R, got a body %L."

let make_uri endpoint rpc =
  Uri.make
    ~scheme:(rpc.get_scheme endpoint)
    ~host:(rpc.get_host endpoint)
    ~port:(rpc.get_port endpoint)
    ~path:(String.concat "/" rpc.path)
    ~query:(List.map (fun (k, v) -> (k, [v])) rpc.query_string)
    ()

let call_raw ?(log_request = true) ?(log_response_status = true)
    ?(log_response_body = true) endpoint rpc =
  let uri = make_uri endpoint rpc in
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
  let* response, response_body =
    Cohttp_lwt_unix.Client.call
      ~headers:(Cohttp.Header.of_list headers)
      ?body:
        (Option.map
           (function
             | Client.Data body ->
                 Cohttp_lwt.Body.of_string (JSON.encode_u body)
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
  if log_response_body then Log.debug ~prefix:"RPC" "%s" body ;
  return {body; code = Cohttp.Code.code_of_status response.status}

let call_json ?log_request ?log_response_status ?(log_response_body = true)
    endpoint rpc =
  let* response =
    call_raw
      endpoint
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

let call ?log_request ?log_response_status ?log_response_body node rpc =
  let* response =
    call_json ?log_request ?log_response_status ?log_response_body node rpc
  in
  check_status_code node rpc response.code ;
  return (rpc.decode response.body)

module Client = struct
  type nonrec 'a t = (Node.t, 'a) t

  let call_raw ?log_command ?log_status_on_exit ?log_output ?better_errors
      ?endpoint ?hooks ?env ?protocol_hash client
      {verb; path; query_string; data; decode = _; _} =
    (* No need to log here, the [Process] module already logs. *)
    Client.spawn_rpc
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?data
      ?protocol_hash
      ~query_string
      verb
      path
      client
    |> Process.check_and_read_stdout

  let call_json ?log_command ?log_status_on_exit ?log_output ?better_errors
      ?endpoint ?hooks ?env ?protocol_hash client rpc =
    let* raw =
      call_raw
        ?log_command
        ?log_status_on_exit
        ?log_output
        ?better_errors
        ?endpoint
        ?hooks
        ?env
        ?protocol_hash
        client
        rpc
    in
    return (JSON.parse ~origin:"RPC response" raw)

  let call ?log_command ?log_status_on_exit ?log_output ?better_errors ?endpoint
      ?hooks ?env ?protocol_hash client rpc =
    let* json =
      call_json
        ?log_command
        ?log_status_on_exit
        ?log_output
        ?better_errors
        ?endpoint
        ?hooks
        ?env
        ?protocol_hash
        client
        rpc
    in
    return (rpc.decode json)

  let schema ?log_command ?log_status_on_exit ?log_output ?better_errors
      ?endpoint ?hooks ?env ?protocol_hash client {verb; path; _} =
    Client.rpc_schema
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?protocol_hash
      verb
      path
      client

  let spawn ?log_command ?log_status_on_exit ?log_output ?better_errors
      ?endpoint ?hooks ?env ?protocol_hash client
      {verb; path; query_string; data; decode = _; _} =
    Client.Spawn.rpc
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?data
      ?protocol_hash
      ~query_string
      verb
      path
      client
end
