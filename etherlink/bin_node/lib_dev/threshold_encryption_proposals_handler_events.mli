(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val started : unit -> unit Lwt.t

val shutdown : unit -> unit Lwt.t

val proposal_processed : unit -> unit Lwt.t

(** Emits a `Debug` log that notifies that the
    [Threshold_encryption_proposals_handler] has received a request to submit
    a new proposal to the dsn node. *)
val received_proposal_submission_request : unit -> unit Lwt.t
