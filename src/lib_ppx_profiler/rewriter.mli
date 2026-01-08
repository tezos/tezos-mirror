(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** {1 Rewriters}

    Implements rewriters corresponding to the attached {!section-constants} *)

(** A rewriter essentially contains:
    - a key: see {!page-key.key}
    - an action
    - a location: a compilerlib location allowing finer error tracking *)
type action =
  | Aggregate
  | Aggregate_f
  | Aggregate_s
  | Mark
  | Overwrite
  | Record
  | Record_f
  | Record_s
  | Span
  | Span_f
  | Span_s
  | Stamp
  | Stop
  | Wrap_f
  | Wrap_s

type t = {key : Key.t; action : action; location : Ppxlib.location}

val to_constant : t -> Constants.t

(** [to_fully_qualified_lident_expr t _] transforms a [t] in a
    [Parsetree.expression] that contains a
    {{: https://ocaml-ppx.github.io/ppxlib/ppxlib/Ppxlib/index.html#val-lident}Lident}

    Example:

    [Aggregate_s] will be transformed in [Lident "Profiler.aggregate_s"] *)
val to_fully_qualified_lident_expr :
  t -> Warnings.loc -> Ppxlib.Parsetree.expression

val get_location : t -> Ppxlib.location

(** [extract_rewriters handled_drivers attributes] inspects the given list
      of [attributes] and returns the rewriters that are handled by this ppx.

      If an [attribute] has a [driver_ids] field that is not enabled by
      `TEZOS_PPX_PROFILER`, the attribute is removed from the list of attributes
      but will not be preprocessed *)
val extract_rewriters : Handled_drivers.t -> Ppxlib.attribute list -> t list
