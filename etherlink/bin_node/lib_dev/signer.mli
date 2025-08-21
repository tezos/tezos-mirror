(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val wallet : #Client_context.wallet -> Client_keys.sk_uri -> t

val gcp_kms : Gcp_kms.t -> t

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
