(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
(*                                                                           *)
(*****************************************************************************)

let already_denounced_aux ctxt delegate (level : Level_repr.t) round kind =
  let open Lwt_result_syntax in
  let* denounced_opt =
    Storage.Already_denounced.find
      (ctxt, level.cycle)
      ((level.level, round), delegate)
  in
  match (denounced_opt, (kind : Misbehaviour_repr.kind)) with
  | None, _ -> return_false
  | Some denounced, Double_preattesting ->
      return denounced.for_double_preattesting
  | Some denounced, Double_attesting -> return denounced.for_double_attesting
  | Some denounced, Double_baking -> return denounced.for_double_baking

let already_denounced ctxt delegate level round kind =
  let open Lwt_result_syntax in
  let* answer = already_denounced_aux ctxt delegate level round kind in
  if answer || Round_repr.(round = zero) then return answer
  else
    let* first_level = Storage.Tenderbake.First_level_of_protocol.get ctxt in
    if Raw_level_repr.(level.level >= first_level) then return answer
    else
      (* Exception related to the migration from Oxford to P: because
         Oxford doesn't record the round of misbehaviours, all
         misbehaviours present in the storage at stitching time got
         assigned the round zero. So we also check with the round set
         to zero in the specific case where a misbehaviour:

         - is old enough to have potentially been denounced during
         Oxford (ie. its level is before the first level of P),

         - has a non-zero round (otherwise the new check is identical
         to the previous one anyway), and

         - has not been found in the storage under its own round
         (ie. [answer] is [false]).

         TODO #6957: This whole control flow should be removed from
         protocol Q. *)
      already_denounced_aux ctxt delegate level Round_repr.zero kind

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
  let already_denounced =
    match kind with
    | Misbehaviour_repr.Double_baking -> denounced.for_double_baking
    | Double_attesting -> denounced.for_double_attesting
    | Double_preattesting -> denounced.for_double_preattesting
  in
  let*! ctxt =
    if already_denounced then Lwt.return ctxt
    else
      Storage.Already_denounced.add
        (ctxt, level.cycle)
        ((level.level, round), delegate)
        (match kind with
        | Double_baking -> {denounced with for_double_baking = true}
        | Double_attesting -> {denounced with for_double_attesting = true}
        | Double_preattesting -> {denounced with for_double_preattesting = true})
  in
  return (ctxt, already_denounced)

let clear_outdated_cycle ctxt ~new_cycle =
  match Cycle_repr.(sub new_cycle Constants_repr.max_slashing_period) with
  | None -> Lwt.return ctxt
  | Some outdated_cycle -> Storage.Already_denounced.clear (ctxt, outdated_cycle)
