(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [start ~signer ~smart_rollup_address ~rollup_node_endpoint ()] starts the
    signal publisher. It should only be called once and only if DAL is enabled.
    The signal publisher should be started before the blueprint publisher
    because it expects messages from the blueprint publisher.*)
val start :
  signer:Signer.map ->
  smart_rollup_address:string ->
  rollup_node_endpoint:Uri.t ->
  rollup_node_endpoint_timeout:float ->
  unit ->
  unit tzresult Lwt.t

(** [shutdown ()] shuts down the signal publisher. *)
val shutdown : unit -> unit tzresult Lwt.t

(** [new_rollup_block finalized_level] tells the
    worker to check whether the blueprints that have been previously
    injected on the DAL have been committed/included and finalized. If
    it is the case, the worker builds the corresponding signals. For
    now it does not send them to the kernel. *)
val new_rollup_block : finalized_level:int32 -> unit tzresult Lwt.t
