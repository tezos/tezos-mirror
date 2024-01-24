(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  fast_exec_panicked : (unit -> unit Lwt.t) option;
  fast_exec_completed : (unit -> unit Lwt.t) option;
}

let no_hooks = {fast_exec_panicked = None; fast_exec_completed = None}

let on_fast_exec_panicked hook hooks =
  {hooks with fast_exec_panicked = Some hook}

let on_fast_exec_completed hook hooks =
  {hooks with fast_exec_completed = Some hook}
