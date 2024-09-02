(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Which double signing event has occurred. *)
type kind = Double_baking | Double_attesting | Double_preattesting

(** Internal representation of a double signing event used in
    {!Denunciations_repr.item}.

    For a double baking event, the [level] and [round] are those of
    both duplicate blocks. For a double (pre)attestating event, the
    [level] and [round] are those that appear in the
    {!Operation_repr.consensus_content} of both duplicate consensus
    operations.

    Note: the culprit pkh doesn't appear as a field here because it is
    typically used as a key when storing denunciation items in the
    context. *)
type t = {level : Raw_level_repr.t; round : Round_repr.t; kind : kind}

val kind_encoding : kind Data_encoding.t

val encoding : t Data_encoding.t

(** Comparison function for double signing kinds.

    [Double_baking < Double_attesting < Double_preattesting] *)
val compare_kind : kind -> kind -> int

val equal_kind : kind -> kind -> bool

(** Comparison function for misbehaviours.

    Misbehaviours are ordered by increasing level, then increasing
    round, then kind using {!compare_kind}. *)
val compare : t -> t -> int
