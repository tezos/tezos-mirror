(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** GitLab CI variable expressions

    For more information, see
    {{:https://docs.gitlab.com/ee/ci/jobs/job_control.html#cicd-variable-expressions}
    here}. *)

(** Predicates for GitLab [if:] expression, as used in [rules:] clauses. *)
type t

(** Terms for predicates in GitLab [if:] clauses. *)
type term

(** The string representation of an [if:] expression. *)
val encode : t -> string

(** [var name] is the [if:] expression [$name].

    This is an alias of {!Var.make} to simplify usage of variables in
    [if:] expressions. It allows you to write e.g.
    {{
    If.(var "foo" == "bar")
    }}
    instead of
    {{
    If.(Var.make "foo" == "bar")
    }}

    Raises [Invalid_argument] if the variable name is not composed
    exclusively of characters from the set [a-zA-Z0-9_]. *)
val var : string -> Var.t

(** [str s] is the [if:] expression ["s"].

    Raises [Invalid_argument] if the literal string contains both
    single and double quotes. Such strings cannot be encoded in GitLab
    CI [if:] expressions. *)
val str : string -> term

(** The [if:]-expression [null]. *)
val null : term

(** Equality in [if:]-expressions.

    Example: [var "foo" == str "bar"] translates to [$foo == "bar"]. *)
val ( == ) : Var.t -> term -> t

(** Inequality in [if:]-expressions.

    Example: [var "foo" != str "bar"] translates to [$foo != "bar"]. *)
val ( != ) : Var.t -> term -> t

(** Conjunction of [if:]-expressions. *)
val ( && ) : t -> t -> t

(** Disjunction of [if:]-expressions. *)
val ( || ) : t -> t -> t

(** Pattern match on [if:]-expressions.

    Example: [var "foo" =~ str "/bar/"] translates to [$foo =~ "/bar/"]. *)
val ( =~ ) : Var.t -> string -> t

(** Negated pattern match on [if:]-expressions.

    Example: [var "foo" =~! str "/bar/"] translates to [$foo !~ "/bar/"]. *)
val ( =~! ) : Var.t -> string -> t
