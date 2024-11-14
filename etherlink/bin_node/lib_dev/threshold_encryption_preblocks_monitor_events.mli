(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Emits a `Notice` log that notifies that the
    [Threshold_encryption_preblocks_monitor] has been started. *)
val started : unit -> unit Lwt.t

(** Emits a `Notice` log that notifies that the
    [Threshold_encryption_preblocks_monitor] is shutting down. *)
val shutdown : unit -> unit Lwt.t

(** Emits a `Debug` log that notifies that the
    [Threshold_encryption_preblocks_monitor] has received a preblock for the
    blueprint number given in input. *)
val received_preblock : Ethereum_types.quantity -> unit Lwt.t
