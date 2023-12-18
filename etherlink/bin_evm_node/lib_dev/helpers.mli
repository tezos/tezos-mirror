(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [now ()] returns the current time. *)
val now : unit -> Time.Protocol.t

(** [timestamp_to_bytes timestamp] transforms the timestamp to bytes
    compatible with the kernel. *)
val timestamp_to_bytes : Time.Protocol.t -> bytes
