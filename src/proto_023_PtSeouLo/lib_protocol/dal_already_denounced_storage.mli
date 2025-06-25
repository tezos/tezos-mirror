(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
(*                                                                           *)
(*****************************************************************************)

(** This module is responsible for ensuring that a delegate doesn't get slashed
    twice for the same DAL offense. To do so, it maintains the
    {!Storage.Dal_already_denounced} table, which tracks which DAL (entrapment)
    denunciations have already been seen in blocks.

    A denunciation is uniquely characterized by the delegate (the culprit), the
    level and the slot index of the entrapment evidence.

    Invariant: {!Storage.Dal_already_denounced} is empty for cycles equal
    to [current_cycle - max_slashing_period] or older. Indeed, such
    denunciations are no longer allowed (see
    [Anonymous.check_denunciation_age] in {!Validate}) so there is no
    need to track them anymore. *)

(** Returns true if the given delegate has already been denounced at the given
    level and slot index. *)
val is_already_denounced :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  Dal_slot_index_repr.t ->
  bool Lwt.t

(** Returns whether the given delegate has been denounced in the current cycle. *)
val is_denounced : Raw_context.t -> Signature.Public_key_hash.t -> bool Lwt.t

(** Records a denunciation in {!Storage.Dal_already_denounced}.

    Returns a pair [(ctxt, already_denounced)], where
    [already_denounced] is a boolean indicating whether the
    denunciation was already recorded in the storage previously.

    When [already_denounced] is [true], the returned [ctxt] is
    actually the unchanged context argument.

    Precondition: the given level should be more recent than
    [current_cycle - max_slashing_period] in order to maintain the
    invariant on the age of tracked denunciations. Fortunately, this
    is already enforced in {!Validate} by
    [Anonymous.check_denunciation_age]. *)
val add_denunciation :
  Raw_context.t ->
  Signature.public_key_hash ->
  Level_repr.t ->
  Dal_slot_index_repr.t ->
  (Raw_context.t * bool) Lwt.t

(** Clear {!Storage.Dal_already_denounced} for old cycles that we no
    longer need denunciations for. *)
val clear_outdated_cycle :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t Lwt.t
