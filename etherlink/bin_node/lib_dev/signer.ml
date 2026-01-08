(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module PKMap = Map.Make (Signature.Public_key)

type t =
  | Wallet : #Client_context.wallet * Client_keys.sk_uri -> t
  | Gcp_kms of Gcp_kms.t

type map = t PKMap.t

let wallet cctxt sk_uri = Wallet (cctxt, sk_uri)

let gcp_kms kms = Gcp_kms kms

let of_sequencer_key config cctxt (k : Configuration.sequencer_key) =
  let open Lwt_result_syntax in
  match k with
  | Wallet k -> return (Wallet (cctxt, k))
  | Gcp_key k ->
      let* kms = Gcp_kms.from_gcp_key config.Configuration.gcp_kms k in
      return (Gcp_kms kms)

let of_sequencer_keys config cctxt keys =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun acc k ->
      let* value = of_sequencer_key config cctxt k in
      let* pk =
        match value with
        | Wallet (_cctxt, sk_uri) ->
            let* pk_uri = Client_keys.neuterize sk_uri in
            let* pk = Client_keys.public_key pk_uri in
            return pk
        | Gcp_kms kms ->
            let pk = Gcp_kms.public_key kms in
            return pk
      in
      return (PKMap.add pk value acc))
    PKMap.empty
    keys

let first_lexicographic_signer map =
  let open Result_syntax in
  match PKMap.bindings map with
  | [] -> fail [error_of_fmt "Excepted at list one signer defined but found 0."]
  | signer :: _ -> return signer

let get_signer map pk =
  let open Result_syntax in
  match PKMap.find pk map with
  | Some k -> return k
  | None ->
      [
        error_of_fmt
          "Public key %a don't have signer associated"
          Signature.Public_key.pp
          pk;
      ]
      |> fail

let of_string config cctxt key_str =
  let open Lwt_result_syntax in
  match Configuration.gcp_key_from_string_opt key_str with
  | Some key ->
      let* kms_handler =
        Gcp_kms.from_gcp_key config.Configuration.gcp_kms key
      in
      return (gcp_kms kms_handler)
  | None ->
      let* sk_uri = Client_keys.Secret_key.parse_source_string cctxt key_str in
      return (wallet cctxt sk_uri)

let sequencer_key_of_string cctxt key_str =
  let open Lwt_result_syntax in
  match Configuration.gcp_key_from_string_opt key_str with
  | Some key -> return (Configuration.Gcp_key key)
  | None ->
      let+ sk_uri = Client_keys.Secret_key.parse_source_string cctxt key_str in
      Configuration.Wallet sk_uri

let public_key =
  let open Lwt_result_syntax in
  function
  | Wallet (_cctxt, sk_uri) ->
      let* pk_uri = Client_keys.neuterize sk_uri in
      Client_keys.public_key pk_uri
  | Gcp_kms kms -> return (Gcp_kms.public_key kms)

let sequencer_key = function
  | Wallet (_, sk_uri) -> Configuration.Wallet sk_uri
  | Gcp_kms kms -> Gcp_key (Gcp_kms.gcp_key kms)

let sign = function
  | Wallet (cctxt, sk) -> Client_keys.sign cctxt sk
  | Gcp_kms kms -> Gcp_kms.sign kms Blake2B
