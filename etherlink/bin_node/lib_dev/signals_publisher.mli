(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [start ~cctxt ~smart_rollup_address ~sequencer_key
    ~rollup_node_endpoint ~max_blueprints_lag ()] starts the signal
    publisher. It should only be called once and only if DAL is
    enabled. The signal publisher should be started before the
    blueprint publisher because it expects messages from the blueprint
    publisher.*)
val start :
  cctxt:Client_context.wallet ->
  smart_rollup_address:string ->
  sequencer_key:Client_keys.sk_uri ->
  rollup_node_endpoint:Uri.t ->
  max_blueprints_lag:int ->
  unit ->
  unit tzresult Lwt.t

(** [shutdown ()] shuts down the signal publisher. *)
val shutdown : unit -> unit tzresult Lwt.t
