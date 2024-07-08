(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [start ~sidecar_endpoint ~time_between_blocks]
    starts the hreshold encryption preblocks monitor. *)
val start :
  sidecar_endpoint:Uri.t ->
  time_between_blocks:Configuration.time_between_blocks ->
  unit tzresult Lwt.t
