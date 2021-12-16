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

  class http_ctxt : config -> Media_type.t list -> RPC_context.generic

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

  val generic_media_type_call :
    ?headers:(string * string) list ->
    accept:Media_type.t list ->
    ?body:Data_encoding.json ->
    [< Resto.meth] ->
    Uri.t ->
    RPC_context.generic_call_result tzresult Lwt.t

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

module Make (Client : Resto_cohttp_client.Client.CALL) : S
