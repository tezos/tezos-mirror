(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Storage maintenance configuration.

    Storage maintenance aims to configure the internals of the storage
    maintenance procedure that aims to be run on regular basis.

 *)

(** The type [context_pruning] specifies whether or not a storage maintenance
    should be triggered (if [Enabled]) or not (if [Disabled]). *)
type context_pruning = Enabled | Disabled

val context_pruning_encoding : context_pruning Data_encoding.t

val pp_context_pruning : Format.formatter -> context_pruning -> unit
