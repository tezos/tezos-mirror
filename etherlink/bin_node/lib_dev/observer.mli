(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ?kernel_path ~data_dir ~config ~no_sync ()] starts the main
    event-loop of the Observer, consuming the blueprints received from
    the EVM node endpoint, unless [no_sync] is true. *)
val main :
  ?kernel_path:string ->
  data_dir:string ->
  config:Configuration.t ->
  no_sync:bool ->
  unit ->
  unit tzresult Lwt.t
