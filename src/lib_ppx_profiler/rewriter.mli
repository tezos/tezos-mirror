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

(** Describes how to call a profiler method *)
type profiler_method_call =
  | Direct of Ppxlib.expression
      (** Call the method directly: [Profiler.record_s] *)
  | Unpack of {
      module_expr : Ppxlib.expression;  (** The first-class module expression *)
      method_name : string;  (** The method name, e.g., "record_s" *)
    }
      (** Unpack First Class Module then call:
          [let (module Profiler__) = expr in Profiler__.record_s] *)

(** [to_profiler_method_call t loc] returns how to call the profiler method
    for the given rewriter action.

    Examples:
    - With static module [Profiler]: [Direct (Profiler.record_s)]
    - With First Class Module [state.profiler]: [Unpack {module_expr; method_name = "record_s"}] *)
val to_profiler_method_call : t -> Warnings.loc -> profiler_method_call

val get_location : t -> Ppxlib.location

(** [extract_rewriters handled_drivers attributes] inspects the given list
      of [attributes] and returns the rewriters that are handled by this ppx.

      If an [attribute] has a [driver_ids] field that is not enabled by
      `TEZOS_PPX_PROFILER`, the attribute is removed from the list of attributes
      but will not be preprocessed *)
val extract_rewriters : Handled_drivers.t -> Ppxlib.attribute list -> t list
