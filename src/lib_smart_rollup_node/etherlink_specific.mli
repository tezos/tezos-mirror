(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [current_level ctxt] returns the current Etherlink level for the PVM
    state in the context [ctxt].

    NOTE: This function only works for Etherlink rollups (returns [None]
    otherwise). *)
val current_level : 'a Context.t -> int option Lwt.t
