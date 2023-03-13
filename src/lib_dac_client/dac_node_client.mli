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

(** [retrieve_preimage cctxt hash] requests the preimage of hash, consisting of a
    single page, from cctxt. When the request succeeds, the raw page will be
    returned as a sequence of bytes. *)
val retrieve_preimage :
  Dac_plugin.t -> #cctxt -> page_hash:Dac_plugin.hash -> bytes tzresult Lwt.t

(** [store_preimage ~payload ~pagination_scheme] posts a [payload] to dac/store_preimage 
    using a given [pagination_scheme]. It returns the base58 encoded root page hash 
    and the raw bytes. *)
val store_preimage :
  Dac_plugin.t ->
  #cctxt ->
  payload:bytes ->
  pagination_scheme:Pagination_scheme.t ->
  (Dac_plugin.hash * bytes) tzresult Lwt.t

(** [verify_external_message_signature external_message] requests the DAL node to verify
    the signature of the external message [external_message] via
    the plugin/dac/verify_signature endpoint. The DAC committee
    of the DAL node must be the same that was used to produce the
    [external_message]. *)
val verify_external_message_signature :
  #cctxt -> external_message:string option -> bool tzresult Lwt.t

(** [store_dac_member_signature signature:Signature_repr.t]
    stores the [signature] generated from signing [hex_root_hash] by
    [dac_member_pkh]. *)
val store_dac_member_signature :
  Dac_plugin.t -> #cctxt -> signature:Signature_repr.t -> unit tzresult Lwt.t

(** [get_certificate root_page_hash] fetches the DAC certificate for the
    provided [root_page_hash]. *)
val get_certificate :
  Dac_plugin.t ->
  #cctxt ->
  root_page_hash:Dac_plugin.hash ->
  Certificate_repr.t option tzresult Lwt.t

(** [coordinator_post_preimage ~payload] sends a [payload] to the DAC
  [Coordinator] via a POST RPC call to dac/preimage. It returns a hex
  encoded root page hash, produced by [Merkle_tree_V0] pagination scheme.
  On the backend side it also pushes root page hash of the preimage to all
  the subscribed DAC Members and Obervers. *)
val coordinator_post_preimage :
  Dac_plugin.t -> #cctxt -> payload:bytes -> Dac_plugin.hash tzresult Lwt.t
