(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Simple dependency solver for internal components.

    This is not a full-featured solver because it only supports
    equality version constraints. This makes it much simpler than
    a regular solver that has to solve an NP-complete problem. *)

open Misc

(** Reason why a component needs to be built. *)
type reason

(** Convert a reason to a string to display to the user. *)
val show_reasons : reason list -> string

(** Components and why they need to be built. *)
type component_with_reason = {component : Component.t; reasons : reason list}

(** Solve for a set of components.

    Takes a list of requested components to install,
    and the version they need to be installed in.

    Returns a list of components that need to be installed,
    in the order specified by this list,
    to reach a point where the requested components are installed. *)
val solve :
  (string * Version.t) list ->
  (component_with_reason list, [> `failed] error) result
