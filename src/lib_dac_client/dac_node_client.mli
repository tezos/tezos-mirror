(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** Instance of [Tezos_client_base.Client_context] that only handles IOs and
    RPCs. Can be used for keys and RPCs related commands. *)
class type cctxt =
  object
    inherit Tezos_rpc.Context.generic
  end

(** Instance of [cctxt] for linux systems. Relies on
    [Tezos_rpc_http_client_unix]. *)
class unix_cctxt :
  rpc_config:Tezos_rpc_http_client_unix.RPC_client_unix.config -> cctxt

(** [make_unix_client_context scheme host port] generates a cctxt from
    the client configuration parameters. *)
val make_unix_cctxt : scheme:string -> host:string -> port:int -> cctxt

(** Generic function for a normal RPC call. *)
val call :
  #cctxt ->
  ([< Resto.meth], unit, 'a, 'b, 'c, 'd) Tezos_rpc.Service.t ->
  'a ->
  'b ->
  'c ->
  'd tzresult Lwt.t

(** Generic function for a streamed RPC call. *)
val streamed_call :
  #cctxt ->
  ([< Resto.meth], unit, 'a, 'b, 'c, 'd) Tezos_rpc.Service.t ->
  on_chunk:('d -> unit) ->
  on_close:(unit -> unit) ->
  'a ->
  'b ->
  'c ->
  (unit -> unit) tzresult Lwt.t

(** [get_preimage cctxt hash] requests the preimage of hash, consisting of a
    single page, from cctxt. When the request succeeds, the raw page will be
    returned as a sequence of bytes. *)
val get_preimage :
  Dac_plugin.t -> #cctxt -> Dac_plugin.hash -> bytes tzresult Lwt.t
