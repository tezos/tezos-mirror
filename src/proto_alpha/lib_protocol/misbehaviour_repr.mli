(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Which double signing event has occurred. *)
type kind =
  | Double_baking
  | Double_attesting
      (** Includes both double preattestion and double attestion. *)

(** Internal representation of a double signing event used in
    {!Denunciations_repr.item}.

    For a double baking event, [level] and [round] are those of the
    duplicate blocks, and [slot] is [Round_repr.to_slot round].

    For a double (pre)attestation event, [level], [round], and [slot]
    are those indicated in the {!Operation_repr.consensus_content} of
    the consensus operation that appears first in the denunciation
    operation. Note that both duplicate operations always have the
    same [level] and [round] anyway, but might have different [slot]s.

    Note: the culprit pkh doesn't appear as a field here because it is
    typically used as a key when storing denunciation items in the
    context. *)
type t = {
  kind : kind;
  level : Raw_level_repr.t;
  round : Round_repr.t;
  slot : Slot_repr.t;
}

val encoding : t Data_encoding.t
