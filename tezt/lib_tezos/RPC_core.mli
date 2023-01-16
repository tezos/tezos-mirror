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

(** RPC description type and functions to call RPCs. *)

(** HTTP request methods. *)
type verb = Client.meth = GET | PUT | POST | PATCH | DELETE

(** RPC descriptions.

    ['endpoint] is the type the target endpoint.
    ['result] is the type of values returned by the RPC after decoding. *)
type ('endpoint, 'result) t

(** Data type for RPCs. *)
type data = Client.data

(** Make an RPC description.

    Usage: [make ~get_host ~get_port verb path decode]

    - If [data] is specified, it is provided as the request body.

    - If [query_string] is specified, it is added to the URL after the [?] character.
      It is a list of key-value pairs.
      For instance, [~query_string: ["key1", "value1"; "key2", "value2"]]
      denotes [?key1=value1&key2=value2].

    - [path] is the part of the URL that denotes the RPC's endpoint.
      For instance, [["chains"; "main"; "blocks"; "head"]]
      denotes [/chains/main/blocks/head].

    - [decode] is the function that will be used to decode the response body of the RPC.
      If you do not want to define it, use [Fun.id] (to return a [JSON.t])
      or [ignore] (to ignore the response body).

    - [get_host], [get_port] and [get_scheme] are callbacks to extract host, port and
      scheme from the endpoint to build the targeted url.

    Use one of the [call] functions below to actually call the RPC. *)
val make :
  ?data:data ->
  ?query_string:(string * string) list ->
  get_host:('endpoint -> string) ->
  get_port:('endpoint -> int) ->
  get_scheme:('endpoint -> string) ->
  verb ->
  string list ->
  (JSON.t -> 'result) ->
  ('endpoint, 'result) t

(** [make_uri endpoint rpc] returns the URI of the RPC [rpc] at [endpoint]. *)
val make_uri : 'endpoint -> ('endpoint, 'result) t -> Uri.t

(** Parse and decode a response body using the decode function of an RPC description.

    [origin] is used in error messages.
    Its default value is ["RPC response"]. *)
val decode_raw : ?origin:string -> ('endpoint, 'result) t -> string -> 'result

(** Decode a response body using the decode function of an RPC description. *)
val decode : ('endpoint, 'result) t -> JSON.t -> 'result

(** RPC responses. *)
type 'a response = {
  body : 'a;  (** Response body. *)
  code : int;  (** Status code (e.g. 200 for OK, 404 for Not Found). *)
}

(** [check_string_response ?body_rex ~code response] verifies that the given
    response's body  and HTTP status match the expected ones using
    the facilities provided by module {!val:Check}.

    The function checks exact equality for the HTTP status and for regular
    expression matching for the body (if provided).  *)
val check_string_response :
  ?body_rex:string -> code:int -> string response -> unit

(** Call an RPC.

    The response body is parsed as JSON, then decoded using the decode function
    of the RPC description.

    Calls [Test.fail] if the status code is not a success code.
    To handle error cases, use [call_raw] or [call_json] instead.
    They return the status code and you can use [decode] to parse the response body
    in case of success.

    If [log_request] is [true], log the HTTP method and URI before calling the RPC.
    Default is [true].

    If [log_response_status] is [true], log the HTTP status code of the response
    after the RPC call. Default is [true].

    If [log_response_body] is [true], log the response body after the RPC call.
    For [call] and [call_json], if the response is valid JSON, it is pretty-printed
    with indentation. Default is [true]. *)
val call :
  ?log_request:bool ->
  ?log_response_status:bool ->
  ?log_response_body:bool ->
  'endpoint ->
  ('endpoint, 'result) t ->
  'result Lwt.t

(** Call an RPC, but do not parse its response body.

    Does not fail if the status code is not a success code. *)
val call_raw :
  ?log_request:bool ->
  ?log_response_status:bool ->
  ?log_response_body:bool ->
  'endpoint ->
  ('endpoint, 'result) t ->
  string response Lwt.t

(** Call an RPC, but do not decode its response body, only parse as JSON.

    Does not fail if the status code is not a success code,
    except if the response body is not valid JSON. *)
val call_json :
  ?log_request:bool ->
  ?log_response_status:bool ->
  ?log_response_body:bool ->
  'endpoint ->
  ('endpoint, 'result) t ->
  JSON.t response Lwt.t

module Client : sig
  type nonrec 'result t = (Node.t, 'result) t

  (** Perform RPC calls using [octez-client]. *)

  (** RPC calls performed this way are slower and should only be used to test
      the [rpc] command of the client. *)

  (** Call an RPC using [octez-client rpc].

      The response body is parsed as JSON, then decoded using the decode function
      of the RPC description.

      The following arguments:
      - [log_command];
      - [log_status_on_exit];
      - [log_output];
      - [better_errors];
      - [endpoint];
      - [hooks];
      - [env];
      - [protocol_hash];
      are passed to [Client.rpc]. *)
  val call :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    Client.t ->
    'result t ->
    'result Lwt.t

  (** Call an RPC, but do not parse the client output. *)
  val call_raw :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    Client.t ->
    'result t ->
    string Lwt.t

  (** Call an RPC, but do not decode the client output, only parse it. *)
  val call_json :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    Client.t ->
    'result t ->
    JSON.t Lwt.t

  (** Get the schema of an RPC as JSON. *)
  val schema :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    Client.t ->
    'result t ->
    JSON.t Lwt.t

  (** Same as [call_raw], but do not wait for the process to exit.

      Because this function is mostly used to test error cases, the response body
      is not decoded. *)
  val spawn :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    Client.t ->
    'result t ->
    JSON.t Runnable.process
end
