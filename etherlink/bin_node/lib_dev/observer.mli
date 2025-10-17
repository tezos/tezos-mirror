(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ?kernel_path ~config ~no_sync ()] starts the main
    event-loop of the Observer, consuming the blueprints received from
    the EVM node endpoint, unless [no_sync] is true.

    If [init_from_snapshot] and [network] is set, then the node will
    fetch and import the latest known snapshot for this [network] if [data_dir]
    was not already created. *)
val main :
  ?network:Configuration.supported_network ->
  ?kernel_path:Pvm_types.kernel ->
  config:Configuration.t ->
  no_sync:bool ->
  init_from_snapshot:string option ->
  unit ->
  unit tzresult Lwt.t
