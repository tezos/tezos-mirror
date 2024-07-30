(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** The RPC mode exposes a read-only JSON RPC API endpoint, which reuses an
    existing data-dir. It can be used in conjunction with a node running in
    sequencer or observer mode. *)

val main : data_dir:string -> config:Configuration.t -> unit tzresult Lwt.t
