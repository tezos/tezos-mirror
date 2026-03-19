(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2019-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_sapling.Core.Client

(** Add to the wallet a new spending key derived from a mnemonic and identified
    by an alias. The wallet is updated and the corresponding viewing key is
    returned.
    If [force] it will overwrite an existing alias. *)
val register :
  #Client_context.full ->
  ?force:bool ->
  ?unencrypted:bool ->
  Bip39.t ->
  string ->
  Viewing_key.t tzresult Lwt.t

(** [derive parent child index] derives a key with alias [child] from an
    existing key with alias [parent] at [index] using ZIP32.
    If a new index is required the state of the wallet is updated.
    The path and viewing key corresponding to the generated key are returned. *)
val derive :
  #Client_context.full ->
  ?force:bool ->
  ?unencrypted:bool ->
  string ->
  string ->
  int ->
  (string * Viewing_key.t) tzresult Lwt.t

val find_vk : #Client_context.full -> string -> Viewing_key.t tzresult Lwt.t

(** Generate a new address.
    If an optional index is provided, try to derive the address at this index,
    otherwise use the first viable one.
    Not all indexes correspond to a valid address so successive ones are tried.
    Once a valid index is found it is recorded in the wallet.
    Return also the corresponding sk and vk to avoid asking the user multiple
    times for the description password. *)
val new_address :
  #Client_context.full ->
  string ->
  int option ->
  (Spending_key.t * Viewing_key.index * Viewing_key.address) tzresult Lwt.t

val export_vk :
  #Client_context.full -> string -> Data_encoding.Json.json tzresult Lwt.t
