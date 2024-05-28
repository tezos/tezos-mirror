(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** GitLab CI Configuration variables *)

(** Variables in GitLab CI configurations.

    Variables appear in:
    - GitLab [if:] expressions
    - Job scripts
    and elsewhere (see
    {{:https://docs.gitlab.com/ee/ci/variables/where_variables_can_be_used.html}
    here} for more information) *)
type t

(** [make variable_name] creates a variable called [variable_name].

    Raises [Invalid_argument] if the [name] is not composed
    exclusively of characters from the set [a-zA-Z0-9_]. *)
val make : string -> t

(** The string representation of a variable.

    [encode @@ make "foo"] is ["$foo"]. *)
val encode : t -> string
