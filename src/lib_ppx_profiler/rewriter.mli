(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** {1 Rewriters and Constants} *)

module rec Constants : sig
  (** {2 Constants}

       OCaml representations of attribute values in OCaml code *)

  (** A constant is, essentially:
      - an attribute: the full name of a ppx attribute like [profiler.record_s]
      - an action: the shortened name of a ppx attribute like [record_s]
  *)
  type t

  (** {3 Getters and setters} *)

  val of_rewriter : Rewriter.t -> t

  (** [get_attribute constant] will return the fully qualified attribute
      of [constant] like ["profiler.aggregate_s"] *)
  val get_attribute : t -> string

  (** {3 Utility functions} *)

  (** Remove all the attributes that are handled by this ppx from
      an attribute list (usually used to remove these attributes
      from the list of attributes attached to an AST node *)
  val filter_out_all_handled_attributes :
    Parsetree.attribute list -> Parsetree.attribute list
end

and Rewriter : sig
  (** {2 Rewriter}

       Implements rewriters corresponding to the attached {!section-constants}
  *)

  (** A rewriter essentially contains:
      - a key: see {!page-key.key}
      - a location: a compilerlib location allowing finer error tracking
  *)
  type content

  val get_key : content -> Key.t

  (** Possible rewriters *)
  type t =
    | Aggregate of content
    | Aggregate_f of content
    | Aggregate_s of content
    | Mark of content
    | Record of content
    | Record_f of content
    | Record_s of content
    | Reset_block_section of content
    | Span of content
    | Span_f of content
    | Span_s of content
    | Stamp of content
    | Stop of content

  val to_constant : t -> Constants.t

  (** [to_fully_qualified_lident_expr t _] transforms a [t] in a
      [Parsetree.expression] that contains a
      {{: https://ocaml-ppx.github.io/ppxlib/ppxlib/Ppxlib/index.html#val-lident}Lident}

      Example:

      [Aggregate_s] will be transformed in [Lident "Profiler.aggregate_s"]
  *)
  val to_fully_qualified_lident_expr :
    t -> Warnings.loc -> Ppxlib.Parsetree.expression

  val get_location : t -> Ppxlib.location

  (** [extract_rewriters attributes] inspects the given list of [attributes]
      and returns the rewriters that are handled by this ppx. *)
  val extract_rewriters : Parsetree.attribute list -> t list
end
