(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** General purposes events. *)

(** [received_upgrade payload] advertises that the sequencer received an
    upgrade of payload [payload]. *)
val received_upgrade : string -> unit Lwt.t
