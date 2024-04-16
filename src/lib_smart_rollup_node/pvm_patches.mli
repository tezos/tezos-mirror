(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** The type of individual unsafe patch content. *)
type unsafe_patch =
  | Increase_max_nb_ticks of int64
      (** Increase the maximum number of ticks.  *)
