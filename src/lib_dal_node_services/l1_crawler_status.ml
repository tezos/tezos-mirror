(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Catching_up of {levels_to_process : int32}
  | Synced
  | Lagging of {levels_to_process : int32}
  | L1_bootstrapping
  | L1_unreachable
  | Unknown

let catching_up_lagging_or_synced_status ~delta_kind ~head_level
    ~last_processed_level =
  let last_finalized = Int32.sub head_level 2l in
  let levels_to_process = Int32.(sub last_finalized last_processed_level) in
  if levels_to_process > 0l then
    match delta_kind with
    | `Catching_up -> Catching_up {levels_to_process}
    | `Lagging -> Lagging {levels_to_process}
  else Synced

let catching_up_or_synced_status =
  catching_up_lagging_or_synced_status ~delta_kind:`Catching_up

let lagging_or_synced_status =
  catching_up_lagging_or_synced_status ~delta_kind:`Lagging
