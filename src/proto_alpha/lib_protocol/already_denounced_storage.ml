(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
(*                                                                           *)
(*****************************************************************************)

let already_denounced_for_double_attesting ctxt delegate (level : Level_repr.t)
    round =
  let open Lwt_result_syntax in
  let* denounced_opt =
    Storage.Already_denounced.find
      (ctxt, level.cycle)
      ((level.level, round), delegate)
  in
  match denounced_opt with
  | None -> return_false
  | Some denounced -> return denounced.for_double_attesting

let already_denounced_for_double_baking ctxt delegate (level : Level_repr.t)
    round =
  let open Lwt_result_syntax in
  let* denounced_opt =
    Storage.Already_denounced.find
      (ctxt, level.cycle)
      ((level.level, round), delegate)
  in
  match denounced_opt with
  | None -> return_false
  | Some denounced -> return denounced.for_double_baking

let add_denunciation ctxt delegate (level : Level_repr.t) round kind =
  let open Lwt_result_syntax in
  let* denounced_opt =
    Storage.Already_denounced.find
      (ctxt, level.cycle)
      ((level.level, round), delegate)
  in
  let denounced =
    Option.value denounced_opt ~default:Storage.default_denounced
  in
  let already_denounced, updated_denounced =
    let Storage.{for_double_baking; for_double_attesting} = denounced in
    match kind with
    | Misbehaviour_repr.Double_baking ->
        (for_double_baking, {denounced with for_double_baking = true})
    | Double_attesting | Double_preattesting ->
        (for_double_attesting, {denounced with for_double_attesting = true})
  in
  assert (Compare.Bool.(already_denounced = false)) ;
  let*! ctxt =
    Storage.Already_denounced.add
      (ctxt, level.cycle)
      ((level.level, round), delegate)
      updated_denounced
  in
  return ctxt

let clear_outdated_cycle ctxt ~new_cycle =
  match Cycle_repr.(sub new_cycle Constants_repr.max_slashing_period) with
  | None -> Lwt.return ctxt
  | Some outdated_cycle -> Storage.Already_denounced.clear (ctxt, outdated_cycle)
