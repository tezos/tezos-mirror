(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  fast_exec_invalid_kernel :
    [`Check_with_hook of (unit -> unit Lwt.t) option | `No_check];
  fast_exec_panicked : (exn -> unit Lwt.t) option;
  fast_exec_completed : (unit -> unit Lwt.t) option;
  fast_exec_fallback : bool;
  pvm_reboot : (int64 -> unit Lwt.t) option;
}

let no_hooks =
  {
    fast_exec_invalid_kernel = `Check_with_hook None;
    fast_exec_panicked = None;
    fast_exec_completed = None;
    fast_exec_fallback = true;
    pvm_reboot = None;
  }

let disable_fast_exec_invalid_kernel_check hooks =
  {hooks with fast_exec_invalid_kernel = `No_check}

let on_fast_exec_invalid_kernel hook hooks =
  {hooks with fast_exec_invalid_kernel = `Check_with_hook (Some hook)}

let on_fast_exec_panicked hook hooks =
  {hooks with fast_exec_panicked = Some hook}

let on_fast_exec_completed hook hooks =
  {hooks with fast_exec_completed = Some hook}

let fast_exec_fallback c hooks = {hooks with fast_exec_fallback = c}

let on_pvm_reboot hook hooks = {hooks with pvm_reboot = Some hook}
