(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

type map

val wallet : #Client_context.wallet -> Client_keys.sk_uri -> t

val of_sequencer_keys :
  Configuration.t ->
  #Client_context.wallet ->
  Configuration.sequencer_key list ->
  map tzresult Lwt.t

(** Get the first lexicographic signer from the map.
It's not very useful but it's used in the sandbox node to have
a first signer. Shouldn't be used in production. *)
val first_lexicographic_signer : map -> (Signature.public_key * t) tzresult

val get_signer : map -> Signature.public_key -> t tzresult

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
