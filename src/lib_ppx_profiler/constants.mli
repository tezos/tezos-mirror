(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** {1 Constants}

       OCaml representations of attribute values in OCaml code *)

(** A constant is, essentially:
      - an attribute: the full name of a ppx attribute like [profiler.record_s]
      - an action: the shortened name of a ppx attribute like [record_s]
  *)
type t

(** List containing all the constants handled by the ppx *)
val constants : t list

(** {2 Getters and setters} *)

(* val of_rewriter : Rewriter.t -> t *)

(** [get_attribute constant] will return the fully qualified attribute
      of [constant] like ["profiler.aggregate_s"] *)
val get_attribute : t -> string

(** [get_action constant] will return the action registed in [constant]
      like ["aggregate_s"] *)
val get_action : t -> string

(** {2 Utility functions} *)

(** Remove all the attributes that are handled by this ppx from
      an attribute list (usually used to remove these attributes
      from the list of attributes attached to an AST node *)
val filter_out_all_handled_attributes :
  Ppxlib.attribute list -> Ppxlib.attribute list

(** Constant representing [@profiler.aggregate] *)
val aggregate_constant : t

(** Constant representing [@profiler.aggregate_f] *)
val aggregate_f_constant : t

(** Constant representing [@profiler.aggregate_s] *)
val aggregate_s_constant : t

(** Constant representing [@profiler.mark] *)
val mark_constant : t

(** Constant representing [@profiler.overwrite] *)
val overwrite_constant : t

(** Constant representing [@profiler.record] *)
val record_constant : t

(** Constant representing [@profiler.record_f] *)
val record_f_constant : t

(** Constant representing [@profiler.record_s] *)
val record_s_constant : t

(** Constant representing [@profiler.reset_block_section] *)
val reset_block_section_constant : t

(** Constant representing [@profiler.span] *)
val span_constant : t

(** Constant representing [@profiler.span_f] *)
val span_f_constant : t

(** Constant representing [@profiler.span_s] *)
val span_s_constant : t

(** Constant representing [@profiler.stamp] *)
val stamp_constant : t

(** Constant representing [@profiler.stop] *)
val stop_constant : t

(** Constant representing [@profiler.wrap_f] *)
val wrap_f_constant : t

(** Constant representing [@profiler.wrap_s] *)
val wrap_s_constant : t
