(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
(*                                                                           *)
(*****************************************************************************)

(** This module is responsible for ensuring that a delegate doesn't
    get slashed twice for the same offense. To do so, it maintains the
    {!Storage.Already_denounced} table, which tracks which
    denunciations have already been seen in blocks. *)

(** Returns true if the given delegate has already been denounced for
    double baking for the given level and round. *)
val already_denounced_for_double_baking :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  Round_repr.t ->
  bool tzresult Lwt.t

(** Returns true if the given delegate has already been denounced for
    double preattesting or double attesting for the given level and
    round. *)
val already_denounced_for_double_attesting :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  Round_repr.t ->
  bool tzresult Lwt.t
