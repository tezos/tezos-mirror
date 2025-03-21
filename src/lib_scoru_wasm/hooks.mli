(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Hooks used by the fast execution. *)
type fast_exec = {
  invalid_kernel :
    [`Check_with_hook of (unit -> unit Lwt.t) option | `No_check];
  panicked : (exn -> unit Lwt.t) option;
  completed : (unit -> unit Lwt.t) option;
  fallback : bool;
}

(** Hooks used by the pvm. *)
type pvm = {
  reboot : (int64 -> unit Lwt.t) option;  (** Called after a kernel reboot. *)
}

(** A collection of hooks to personalize the execution of the WASM Execution. *)
type t = {fast_exec : fast_exec; pvm : pvm}

(** [no_hooks] is the empty collection of hooks, which can be used as a
    baseline to build hooks collection. *)
val no_hooks : t

(** [disable_fast_exec_invalid_kernel_check hooks] returns a new set of
    hooks signaling to the fast execution runtime to disable the sanity check of
    the kernel it is trying to load. *)
val disable_fast_exec_invalid_kernel_check : t -> t

(** [on_fast_exec_invalid_kernel k hooks] {b replaces b} the hook executed
    when the Fast Execution engine is requested to run a kernel not satisfying
    the WASM PVM constraints. *)
val on_fast_exec_invalid_kernel : (unit -> unit Lwt.t) -> t -> t

(** [on_fast_exec_panicked k hooks] {b replaces b} the hook executed when
    the Fast Execution engine panics by [k] in [hooks]. *)
val on_fast_exec_panicked : (exn -> unit Lwt.t) -> t -> t

(** [on_fast_exec_completed k hooks] {b replaces b} the hook executed when
    the Fast Execution engine completes a [kernel_run] by [k] in [hooks]. *)
val on_fast_exec_completed : (unit -> unit Lwt.t) -> t -> t

(** [fast_exec_fallback c hooks] returns a new collection of hooks
    instructing the Fast Execution engine to fallback to the WASM PVM in
    case of errors.

    It is true by default, but keep in mind the Fast Execution is {b very b}
    slow. *)
val fast_exec_fallback : bool -> t -> t

(** [on_pvm_reboot k hooks] {b replaces b} the hook executed when
    the PVM reaches a reboot point by [k] in [hooks]. *)
val on_pvm_reboot : (int64 -> unit Lwt.t) -> t -> t
