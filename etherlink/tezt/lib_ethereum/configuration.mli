(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Default balance given to bootstrap account. *)
val default_bootstrap_account_balance : Wei.t

(** [make_config ?bootstrap_accounts ?ticketer ?administrator
    ?sequencer ()] creates an installer configuration compatible with
    the EVM kernel. *)
val make_config :
  ?bootstrap_accounts:Eth_account.t array ->
  ?ticketer:string ->
  ?administrator:string ->
  ?sequencer:string ->
  ?delayed_bridge:string ->
  ?flat_fee:Wei.t ->
  unit ->
  [> `Config of Sc_rollup_helpers.Installer_kernel_config.instr list] option
