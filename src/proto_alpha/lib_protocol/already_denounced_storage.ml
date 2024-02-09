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
