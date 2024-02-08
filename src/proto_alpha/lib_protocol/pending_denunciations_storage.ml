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
