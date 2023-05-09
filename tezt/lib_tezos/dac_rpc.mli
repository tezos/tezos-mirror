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

(** [Api] module is used for versioning DAC API. *)
module Api : sig
  (** [v0] is experimental DAC API, which will be deprecated soon. However, for
      the time being the API is binding. It will be used by 1M/tps demo.
      The plan is to remove it once we get rid of the [Legacy] mode.
      Do not use it! *)
  val v0 : string
end

(** [get_preimage hash ~api_version] requests the preimage of hash, consisting of a
    single page, from cctxt. When the request succeeds, the raw page will be
    returned as a sequence of bytes. *)
val get_preimage :
  string -> api_version:string -> (Dac_node.t, string) RPC_core.t

(** [post_store_preimage cctxt ~payload ~pagination_scheme ~api_version] posts a
    [payload] to [api_version]/store_preimage using a given [pagination_scheme].
    It returns the hex encoded root page hash and the raw bytes that can be used
    as contents of a rollup message to trigger the request of the payload in a
    WASM rollup. *)
val post_store_preimage :
  payload:string ->
  pagination_scheme:string ->
  api_version:string ->
  (Dac_node.t, string * string) RPC_core.t

(** [get_verify_signature cctxt external_message ~api_version] requests the DAL
    node to verify the signature of the external message [external_message] via
    the plugin/dac/verify_signature endpoint. The DAC committee
    of the DAL node must be the same that was used to produce the
    [external_message]. *)
val get_verify_signature :
  string -> api_version:string -> (Dac_node.t, bool) RPC_core.t

(** [put_dac_member_signature hex_root_hash dac_member_pkh signature ~api_version]
    stores the [signature] generated from signing [hex_root_hash] by
    [dac_member_pkh]. *)
val put_dac_member_signature :
  hex_root_hash:Hex.t ->
  dac_member_pkh:string ->
  signature:Tezos_crypto.Aggregate_signature.t ->
  api_version:string ->
  (Dac_node.t, unit) RPC_core.t

(** [get_missing_page ~hex_root_hash ~api_version] calls GET
    [api_version]/missing_page/[page_hash] endpoint. *)
val get_missing_page :
  hex_root_hash:Hex.t -> api_version:string -> (Dac_node.t, string) RPC_core.t

(** [get_certificate ~hex_root_hash ~api_version] fetches the DAC certificate
    for the provided [hex_root_hash]. *)
val get_certificate :
  hex_root_hash:Hex.t ->
  api_version:string ->
  (Dac_node.t, int * string * string * int) RPC_core.t

module Coordinator : sig
  (** [post_preimage ~payload ~api_version] sends a [payload] to the DAC
    [Coordinator] via a POST RPC call to dac/preimage. It returns a hex
    encoded root page hash, produced by [Merkle_tree_V0] pagination scheme.
    On the backend side it also pushes root page hash of the preimage to all
    the subscribed DAC Members and Observers. *)
  val post_preimage :
    payload:string -> api_version:string -> (Dac_node.t, string) RPC_core.t
end
