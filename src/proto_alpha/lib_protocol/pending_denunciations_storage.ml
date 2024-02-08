(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let find ctxt delegate =
  let open Lwt_result_syntax in
  let* denunciations_opt = Storage.Pending_denunciations.find ctxt delegate in
  return @@ Option.value denunciations_opt ~default:[]

let add_denunciation ctxt ~misbehaving_delegate operation_hash
    ~rewarded_delegate misbehaviour =
  let open Lwt_result_syntax in
  let* denunciations = find ctxt misbehaving_delegate in
  let denunciations =
    Denunciations_repr.add
      operation_hash
      rewarded_delegate
      misbehaviour
      denunciations
  in
  let*! ctxt =
    Storage.Pending_denunciations.add ctxt misbehaving_delegate denunciations
  in
  return ctxt
