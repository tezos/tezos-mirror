(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let is_forbidden_delegate ctxt delegate =
  let forbidden_delegates = Raw_context.Consensus.forbidden_delegates ctxt in
  Signature.Public_key_hash.Set.mem delegate forbidden_delegates

let forbid_delegate ctxt delegate =
  let ctxt = Raw_context.Consensus.forbid_delegate ctxt delegate in
  let new_forbidden_delegates =
    Raw_context.Consensus.forbidden_delegates ctxt
  in
  Storage.Tenderbake.Forbidden_delegates.add ctxt new_forbidden_delegates

let load_forbidden_delegates ctxt =
  let open Lwt_result_syntax in
  let* forbidden_delegates_opt =
    Storage.Tenderbake.Forbidden_delegates.find ctxt
  in
  let ctxt =
    match forbidden_delegates_opt with
    | Some forbidden_delegates ->
        Raw_context.Consensus.set_forbidden_delegates ctxt forbidden_delegates
    | None ->
        Raw_context.Consensus.set_forbidden_delegates
          ctxt
          Signature.Public_key_hash.Set.empty
  in
  return ctxt

let set_forbidden_delegates ctxt forbidden_delegates =
  let open Lwt_syntax in
  let* ctxt =
    Storage.Tenderbake.Forbidden_delegates.add ctxt forbidden_delegates
  in
  let ctxt =
    Raw_context.Consensus.set_forbidden_delegates ctxt forbidden_delegates
  in
  return ctxt

let reset_forbidden_delegates ctxt =
  if
    Signature.Public_key_hash.Set.is_empty
      (Raw_context.Consensus.forbidden_delegates ctxt)
  then Lwt.return ctxt
  else set_forbidden_delegates ctxt Signature.Public_key_hash.Set.empty
