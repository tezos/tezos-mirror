(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type fast_exec = {
  invalid_kernel :
    [`Check_with_hook of (unit -> unit Lwt.t) option | `No_check];
  panicked : (exn -> unit Lwt.t) option;
  completed : (unit -> unit Lwt.t) option;
  fallback : bool;
}

type pvm = {reboot : (int64 -> unit Lwt.t) option}

type t = {fast_exec : fast_exec; pvm : pvm}

let no_hooks =
  {
    fast_exec =
      {
        invalid_kernel = `Check_with_hook None;
        panicked = None;
        completed = None;
        fallback = true;
      };
    pvm = {reboot = None};
  }

let disable_fast_exec_invalid_kernel_check hooks =
  {hooks with fast_exec = {hooks.fast_exec with invalid_kernel = `No_check}}

let on_fast_exec_invalid_kernel hook hooks =
  {
    hooks with
    fast_exec =
      {hooks.fast_exec with invalid_kernel = `Check_with_hook (Some hook)};
  }

let on_fast_exec_panicked hook hooks =
  {hooks with fast_exec = {hooks.fast_exec with panicked = Some hook}}

let on_fast_exec_completed hook hooks =
  {hooks with fast_exec = {hooks.fast_exec with completed = Some hook}}

let fast_exec_fallback c hooks =
  {hooks with fast_exec = {hooks.fast_exec with fallback = c}}

let on_pvm_reboot hook hooks = {hooks with pvm = {reboot = Some hook}}
