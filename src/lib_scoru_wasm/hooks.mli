(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A collection of hooks to personalize the execution of the WASM Fast
    Execution. *)
type t = {
  fast_exec_panicked : (unit -> unit Lwt.t) option;
  fast_exec_completed : (unit -> unit Lwt.t) option;
}

(** [no_hooks] is the empty collection of hooks, which can be used as a
    baseline to build hooks collection. *)
val no_hooks : t

(** [on_fast_exec_panicked k hooks] {b replaces b} the hook executed when
    the Fast Execution engine panics by [k] in [hooks]. *)
val on_fast_exec_panicked : (unit -> unit Lwt.t) -> t -> t

(** [on_fast_exec_completed k hooks] {b replaces b} the hook executed when
    the Fast Execution engine completes a [kernel_run] by [k] in [hooks]. *)
val on_fast_exec_completed : (unit -> unit Lwt.t) -> t -> t
