(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ~data_dir ~smart_rollup_address config level]
    replays the [level]th blueprint on top of the expected context. *)
val main :
  ?profile:bool ->
  ?kernel_path:string ->
  ?kernel_verbosity:Events.kernel_log_level ->
  data_dir:string ->
  Configuration.t ->
  Ethereum_types.quantity ->
  unit tzresult Lwt.t
