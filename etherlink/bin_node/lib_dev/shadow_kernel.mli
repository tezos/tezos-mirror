(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Execution of the blueprints of an observer with a shadow kernel.

    Each blueprint applied by the observer is applied twice on the same
    pre-state: with the kernel of the node (whose result is committed as usual),
    and with the shadow kernel (whose result is always discarded). The shadow
    kernel never makes the node fail: its failures are only logged. *)

type t

(** [create ~kernel_path ~data_dir ~chain_family ~config] loads the shadow
    kernel and launches the worker applying the blueprints with it, with the
    same [data_dir], [chain_family] and PVM [config] as the node. *)
val create :
  kernel_path:Pvm_types.kernel ->
  data_dir:string ->
  chain_family:_ L2_types.chain_family ->
  config:Pvm_types.config ->
  t tzresult Lwt.t

(** [shutdown t] stops the worker; pending shadow executions are dropped, and
    the one in progress, if any, is awaited. *)
val shutdown : t -> unit Lwt.t

(** [push_blueprint t ~level ~pre_state ~chunks] pushes to the worker the
    request to apply [chunks], the blueprint of level [level], on top of
    [pre_state] with the shadow kernel, and returns immediately. It is meant to
    be called right before the node applies the same blueprint with its own
    kernel on [pre_state], so that both run concurrently: the worker applies the
    blueprints one at a time, in order, on a domain pool of its own. The node
    never fails because of the shadow kernel: its failures are logged. The
    shadow kernel can still slow the node down: each of its storage accesses
    runs on the main thread, shared with the node.

    The blueprint is not applied, and counted as skipped, if too many requests
    are already pending in the worker (the shadow kernel being slower than the
    node), so that memory stays bounded. *)
val push_blueprint :
  t ->
  level:Z.t ->
  pre_state:Evm_state.t ->
  chunks:Sequencer_blueprint.unsigned_chunked_blueprint ->
  unit
