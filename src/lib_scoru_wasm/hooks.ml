(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  fast_exec_invalid_kernel : (unit -> unit Lwt.t) option;
  fast_exec_panicked : (unit -> unit Lwt.t) option;
  fast_exec_completed : (unit -> unit Lwt.t) option;
  fast_exec_fallback : bool;
}

let no_hooks =
  {
    fast_exec_invalid_kernel = None;
    fast_exec_panicked = None;
    fast_exec_completed = None;
    fast_exec_fallback = true;
  }

let on_fast_exec_invalid_kernel hook hooks =
  {hooks with fast_exec_invalid_kernel = Some hook}

let on_fast_exec_panicked hook hooks =
  {hooks with fast_exec_panicked = Some hook}

let on_fast_exec_completed hook hooks =
  {hooks with fast_exec_completed = Some hook}

let fast_exec_fallback c hooks = {hooks with fast_exec_fallback = c}
