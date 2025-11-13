(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let export ~data_dir ~min_published_level ~max_published_level _path =
  let open Lwt_result_syntax in
  let _ = data_dir in
  let _ = min_published_level in
  let _ = max_published_level in
  (* TODO: Implement snapshot export logic *)
  return_unit
