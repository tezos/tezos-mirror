(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold  <contact@tmarigold.dev>                      *)
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
  (** [version] type is used to version DAC API. *)
  type version =
    | V0
        (** [V0] is experimental DAC API. [V0] is deprecated, however for the
            time being the API will be binding. It will be used by
            1M/tps demo. The plan is to remove it once we get rid of the 
            [Legacy] mode. Use at your own risk! *)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5579
     Add support for first publicly released DAC API [V1]. *)

  (** [version_rpc_arg] is a API version argument for the RPCs.*)
  val version_rpc_arg : version Tezos_rpc.Arg.arg
end

(** "POST [api_version]/store_preimage" stores a payload using a given
    [pagination_scheme]. It returns the base58 encoded root page hash 
    and the raw bytes. *)
val post_store_preimage :
  ( [`POST],
    unit,
    unit * Api.version,
    unit,
    Bytes.t * Pagination_scheme.t,
    Dac_plugin.raw_hash * Bytes.t )
  Tezos_rpc.Service.service

(** "GET [api_version]/verify_signature" endpoint requests the DAL node to verify
    the signature of the external message [external_message]. The DAC committee
    of the DAL node must be the same that was used to produce the
    [external_message]. *)
val get_verify_signature :
  ( [`GET],
    unit,
    unit * Api.version,
    string option,
    unit,
    bool )
  Tezos_rpc.Service.service

(** "GET [api_version]/preimage" requests the preimage of hash, consisting of a
    single page, from cctxt. When the request succeeds, the raw page will be
    returned as a sequence of bytes. *)
val get_preimage :
  ( [`GET],
    unit,
    (unit * Api.version) * Dac_plugin.raw_hash,
    unit,
    unit,
    Bytes.t )
  Tezos_rpc.Service.service

(** "PUT [api_version]/member_signature" endpoint stores the [signature] 
    generated from signing [hex_root_hash] by [dac_member_pkh]. *)
val put_dac_member_signature :
  ( [`PUT],
    unit,
    unit * Api.version,
    unit,
    Signature_repr.t,
    unit )
  Tezos_rpc.Service.service

(** "GET [api_version]/certificate" endpoint returns the DAC certificate for the
    provided [root_page_hash]. *)
val get_certificate :
  ( [`GET],
    unit,
    (unit * Api.version) * Dac_plugin.raw_hash,
    unit,
    unit,
    Certificate_repr.t option )
  Tezos_rpc.Service.service

(** "GET [api_version]/missing_page/[page_hash]" Observer fetches the missing page 
    from a Coordinator node. The missing page is then saved to a 
    page store before returning the page as a response. *)
val get_missing_page :
  ( [`GET],
    unit,
    (unit * Api.version) * Dac_plugin.raw_hash,
    unit,
    unit,
    Bytes.t )
  Tezos_rpc.Service.service

module Coordinator : sig
  (** "POST [api_version]/preimage" sends a [payload] to the DAC
      [Coordinator]. It returns a hex encoded root page hash, 
      produced by [Merkle_tree_V0] pagination scheme.
      On the backend side it also pushes root page hash of the preimage to all
      the subscribed DAC Members and Observers. *)
  val post_preimage :
    ( [`POST],
      unit,
      unit * Api.version,
      unit,
      Bytes.t,
      Dac_plugin.raw_hash )
    Tezos_rpc.Service.service
end
