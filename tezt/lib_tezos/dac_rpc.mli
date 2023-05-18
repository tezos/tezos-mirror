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

(** [V0] is experimental DAC API. *)
module V0 : sig
  (** [api_prefix] is a URL prefix of the [V0] API endpoints. *)
  val api_prefix : string

  (** [get_preimage hash] requests the preimage of hash, consisting of a
      single page, from cctxt. When the request succeeds, the raw page will be
      returned as a sequence of bytes. *)
  val get_preimage : string -> (Dac_node.t, string) RPC_core.t

  (** [post_store_preimage cctxt ~payload ~pagination_scheme] posts a
      [payload] to "v0/store_preimage" using a given [pagination_scheme].
      It returns the hex encoded root page hash and the raw bytes that can be used
      as contents of a rollup message to trigger the request of the payload in a
      WASM rollup. *)
  val post_store_preimage :
    payload:string ->
    pagination_scheme:string ->
    (Dac_node.t, string * string) RPC_core.t

  (** [get_verify_signature cctxt external_message] requests the DAC
      node to verify the signature of the external message [external_message] via
      the v0/verify_signature endpoint. The DAC committee of the DAC node must
      be the same that was used to produce the [external_message]. *)
  val get_verify_signature : string -> (Dac_node.t, bool) RPC_core.t

  (** [put_dac_member_signature hex_root_hash dac_member_pkh signature]
      stores the [signature] generated from signing [hex_root_hash] by
      [dac_member_pkh]. *)
  val put_dac_member_signature :
    hex_root_hash:Hex.t ->
    dac_member_pkh:string ->
    signature:Tezos_crypto.Aggregate_signature.t ->
    (Dac_node.t, unit) RPC_core.t

  (** [get_missing_page ~hex_root_hash] calls
      "GET v0/missing_page/[page_hash]" endpoint. *)
  val get_missing_page : hex_root_hash:Hex.t -> (Dac_node.t, string) RPC_core.t

  (** [get_certificate ~hex_root_hash] fetches the DAC certificate
      for the provided [hex_root_hash]. *)
  val get_certificate :
    hex_root_hash:Hex.t -> (Dac_node.t, int * string * string * int) RPC_core.t

  module Coordinator : sig
    (** [post_preimage ~payload] sends a [payload] to the DAC [Coordinator] via
        a POST RPC call to v0/preimage. It returns a hex encoded root page hash,
        produced by [Merkle_tree_V0] pagination scheme. On the backend side it
        also pushes root page hash of the preimage to all the subscribed
        DAC Members and Observers. *)
    val post_preimage : payload:string -> (Dac_node.t, string) RPC_core.t
  end
end
