(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ~data_dir ~preimages ~preimages_endpoint ~smart_rollup_address level]
    replays the [level]th blueprint on top of the expected context. *)
val main :
  ?profile:bool ->
  ?kernel_path:string ->
  data_dir:string ->
  preimages:string ->
  preimages_endpoint:Uri.t option ->
  ?smart_rollup_address:string ->
  Ethereum_types.quantity ->
  unit tzresult Lwt.t
