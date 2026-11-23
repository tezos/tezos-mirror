(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ?network ?kernel_path ~config ~no_sync ~init_from_snapshot ~sandbox ()]
    starts the main event-loop of the Observer, consuming the blueprints
    received from the EVM node endpoint (unless [no_sync] is true).

    If [init_from_snapshot] and [network] are set, then the node will fetch and
    import the latest known snapshot for this [network] if [data_dir] was not
    already created.

    If [sandbox] is [true], then the observer node will fetch the sequencer’s
    public key of its upstream EVM node and patch its own state.

    If [shadow_kernel] is set, each blueprint is also applied with this kernel
    on the same pre-state, and its result is discarded. See {!Shadow_kernel}.

    It fails if [shadow_kernel] is set while the preconfirmation stream is
    enabled: the observer then assembles its blocks from the transactions it
    already executed instead of applying their blueprint, so there is nothing
    the shadow kernel could apply. *)
val main :
  ?network:Configuration.supported_network ->
  ?kernel_path:Pvm_types.kernel ->
  ?shadow_kernel:Pvm_types.kernel ->
  config:Configuration.t ->
  no_sync:bool ->
  init_from_snapshot:string option ->
  sandbox:bool ->
  unit ->
  unit tzresult Lwt.t
