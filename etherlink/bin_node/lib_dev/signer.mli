(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Wallet : #Client_context.wallet * Client_keys.sk_uri -> t
  | Gcp_kms of Gcp_kms.t

type map

val wallet : #Client_context.wallet -> Client_keys.sk_uri -> t

val of_sequencer_keys :
  Configuration.t ->
  #Client_context.wallet ->
  Configuration.sequencer_key list ->
  map tzresult Lwt.t

val first_signer : map -> (Signature.public_key * t) option

val of_sequencer_key :
  Configuration.t ->
  #Client_context.wallet ->
  Configuration.sequencer_key ->
  t tzresult Lwt.t

val of_string :
  Configuration.t -> #Client_context.wallet -> string -> t tzresult Lwt.t

val sequencer_key_of_string :
  #Client_context.wallet ->
  string ->
  (Configuration.sequencer_key, tztrace) result Lwt.t

val public_key : t -> Signature.public_key tzresult Lwt.t

val sequencer_key : t -> Configuration.sequencer_key

val sign : t -> bytes -> Signature.t tzresult Lwt.t
