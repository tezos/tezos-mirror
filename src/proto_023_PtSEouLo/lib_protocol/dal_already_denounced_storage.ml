(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
(*                                                                           *)
(*****************************************************************************)

let is_already_denounced ctxt delegate (level : Level_repr.t) slot_index =
  Storage.Dal_already_denounced.mem
    (ctxt, level.cycle)
    ((level.level, slot_index), delegate)

let is_denounced ctxt delegate =
  Storage.Dal.Denounced_delegates.mem ctxt delegate

let add_denunciation ctxt delegate (level : Level_repr.t) slot_index =
  let open Lwt_syntax in
  let* already_denounced =
    is_already_denounced ctxt delegate level slot_index
  in
  let* ctxt =
    if already_denounced then Lwt.return ctxt
    else
      let* ctxt =
        Storage.Dal_already_denounced.add
          (ctxt, level.cycle)
          ((level.level, slot_index), delegate)
          ()
      in
      Storage.Dal.Denounced_delegates.add ctxt delegate ()
  in
  return (ctxt, already_denounced)

let clear_outdated_cycle ctxt ~new_cycle =
  let open Lwt_syntax in
  let* ctxt = Storage.Dal.Denounced_delegates.clear ctxt in
  (* Misbehaviours from cycles [new_cycle - denunciation_period] and
     higher might still be denounced during [new_cycle], so we need to
     keep known denunciations on them. Anything older than that can be
     discarded. In practice, we only need to clear cycle [new_cycle -
     denunciation_period - 1], because prior cycles have already been
     cleaned up earlier. *)
  match Cycle_repr.(sub new_cycle (Constants_repr.denunciation_period + 1)) with
  | None -> Lwt.return ctxt
  | Some outdated_cycle ->
      Storage.Dal_already_denounced.clear (ctxt, outdated_cycle)
