(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module is in charge of the storage {!Storage.All_bakers_attest_activation},
    which contains the activation level for "All bakers attest". *)

(** [may_update_all_bakers_attest_first_level ctxt cycle ~total_number_stakers ~tz4_number_stakers]
    checks whether the [tz4_number_stakers] is greater than a portion of the [total_number_stakers],
    the portion being defined in the constant [all_bakers_attest_activation_threshold].
    If it is the case, then the first level of the given [cycle] becomes the activation level
    for the "All bakers attest" feature, and is stored in {!Storage.All_bakers_attest_activation}. *)
val may_update_all_bakers_attest_first_level :
  Raw_context.t ->
  Cycle_repr.t ->
  total_number_stakers:int ->
  tz4_number_stakers:int ->
  Raw_context.t Lwt.t

(** [set_all_bakers_attest_first_level ctxt] reads the stored value in
    {!Storage.All_bakers_attest_activation}, and if not [None], puts it in
    the context back as [all_bakers_attest_first_level]. *)
val set_all_bakers_attest_first_level :
  Raw_context.t -> Raw_context.t tzresult Lwt.t
