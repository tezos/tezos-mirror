(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [is ready ()] advertises the prevalidator can now receive prevalidation
    requests. *)
val is_ready : unit -> unit Lwt.t

(** [cannot_start ()] advertises the node was not able to start its
    prevalidator worker.

    This typically happen when starting a proxy node or a observer node in
    finalized view at the very beginning of an Etherlink chain. *)
val cannot_start : unit -> unit Lwt.t
