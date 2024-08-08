(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val download :
  preimages_endpoint:Uri.t ->
  preimages:string ->
  root_hash:Hex.t ->
  unit tzresult Lwt.t
