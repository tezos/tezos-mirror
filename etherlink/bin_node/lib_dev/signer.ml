(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Wallet : #Client_context.wallet * Client_keys.sk_uri -> t
  | Gcp_kms of Gcp_kms.t

let wallet cctxt sk_uri = Wallet (cctxt, sk_uri)

let gcp_kms kms = Gcp_kms kms

let of_sequencer_key cctxt (k : Configuration.sequencer_key) =
  let open Lwt_syntax in
  match k with
  | Wallet k -> return (Wallet (cctxt, k))
  | Gcp_key k ->
      let* kms = Gcp_kms.from_gcp_key k in
      return (Gcp_kms kms)

let of_string cctxt key_str =
  let open Lwt_result_syntax in
  match Configuration.gcp_key_from_string_opt key_str with
  | Some key ->
      let*! kms_handler = Gcp_kms.from_gcp_key key in
      return (gcp_kms kms_handler)
  | None ->
      let* sk_uri = Client_keys.Secret_key.parse_source_string cctxt key_str in
      return (wallet cctxt sk_uri)

let public_key =
  let open Lwt_result_syntax in
  function
  | Wallet (_cctxt, sk_uri) ->
      let* pk_uri = Client_keys.neuterize sk_uri in
      Client_keys.public_key pk_uri
  | Gcp_kms kms -> Gcp_kms.public_key kms

let sequencer_key = function
  | Wallet (_, sk_uri) -> Configuration.Wallet sk_uri
  | Gcp_kms kms -> Gcp_key (Gcp_kms.gcp_key kms)

let sign = function
  | Wallet (cctxt, sk) -> Client_keys.sign cctxt sk
  | Gcp_kms kms -> Gcp_kms.sign kms
