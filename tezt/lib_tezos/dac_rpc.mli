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
  val get_preimage : string -> string RPC_core.t

  (** [make_put_dac_member_sigature_request_body ~dac_member_pkh ~root_hash signature]
      creates a JSON body for "PUT v0/dac_member_signature". *)
  val make_put_dac_member_signature_request_body :
    dac_member_pkh:string ->
    root_hash:Hex.t ->
    Tezos_crypto.Aggregate_signature.signature ->
    Ezjsonm.value

  (** [put_dac_member_signature hex_root_hash dac_member_pkh signature]
      stores the [signature] generated from signing [hex_root_hash] by
      [dac_member_pkh]. *)
  val put_dac_member_signature :
    hex_root_hash:Hex.t ->
    dac_member_pkh:string ->
    signature:Tezos_crypto.Aggregate_signature.t ->
    unit RPC_core.t

  (** [get_missing_page ~hex_root_hash] calls
      "GET v0/missing_page/[page_hash]" endpoint. *)
  val get_missing_page : hex_root_hash:Hex.t -> string RPC_core.t

  (** [get_certificate ~hex_root_hash] fetches the DAC certificate
      for the provided [hex_root_hash]. *)
  val get_certificate :
    hex_root_hash:Hex.t -> (int * string * string * int) RPC_core.t

  (** [get_serialized_certificate ~hex_root_hash] fetches the DAC certificate for the
      provided [hex_root_hash] with SDK kernel compatible [root_hash] encoding. *)
  val get_serialized_certificate : hex_root_hash:Hex.t -> string RPC_core.t

  module Coordinator : sig
    (** [post_preimage ~payload] sends a [payload] to the DAC [Coordinator] via
        a POST RPC call to v0/preimage. It returns a hex encoded root page hash,
        produced by [Merkle_tree_V0] pagination scheme. On the backend side it
        also pushes root page hash of the preimage to all the subscribed
        DAC Members and Observers. *)
    val post_preimage : payload:string -> string RPC_core.t
  end
end

(** [get_health_live] returns [true] if
    [Node_context.get_status cctxt] is [Starting] or [Ready]. *)
val get_health_live : bool RPC_core.t

(** [get_health_ready] returns [true] if
    [Node_context.get_status cctxt] is [Ready]
    and fail with [tzfail Dac_node_not_ready] otherwise. *)
val get_health_ready : bool RPC_core.t

(** [V1] is a second major DAC API release which is currently work in progress. *)
module V1 : sig
  (** [get_pages hash] requests the preimage of hash, consisting of a
      single page, from cctxt. When the request succeeds, the raw page will be
      returned as a sequence of bytes. This is achieved by calling
      "GET v1/pages". *)
  val get_pages : string -> string RPC_core.t
end
