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

module Directory = Tezos_rpc.Directory
module Service = Tezos_rpc.Service

let media_types = [Tezos_rpc_http.Media_type.json]

module NullLogger = struct
  let log_debug fmt = Format.kasprintf ignore fmt

  let log_info fmt = Format.kasprintf ignore fmt

  let log_notice fmt = Format.kasprintf ignore fmt

  let log_warn fmt = Format.kasprintf ignore fmt

  let log_error fmt = Format.kasprintf ignore fmt

  let lwt_log_debug fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_info fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_notice fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_warn fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_error fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt
end

module Call =
  Resto_cohttp_self_serving_client.Self_serving_client.Make
    (Tezos_rpc.Encoding)
    (NullLogger)

let local_ctxt (directory : unit Tezos_rpc.Directory.t) :
    Tezos_rpc.Context.generic =
  let local_client =
    Call.launch ?cors:None ?agent:None ~media_types directory
  in
  let module C = Tezos_rpc_http_client.RPC_client.Make ((val local_client)) in
  let base = Uri.empty in
  object
    method base = base

    method generic_media_type_call meth ?body uri =
      C.generic_media_type_call ~accept:media_types meth ?body uri

    method call_service :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
        'p ->
        'q ->
        'i ->
        'o tzresult Lwt.t =
      fun service params query body ->
        C.call_service media_types ~base service params query body

    method call_streamed_service :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
        on_chunk:('o -> unit) ->
        on_close:(unit -> unit) ->
        'p ->
        'q ->
        'i ->
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
