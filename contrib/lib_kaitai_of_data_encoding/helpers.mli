(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module consists of helpers for building kaitai specifications. *)

open Kaitai.Types

(** A [tid_gen] is a generator for tuple ids. *)
type tid_gen = unit -> string

(** [mk_tid_gen prefix] is a [tid_gen] where generated ids have the given
    [prefix]. *)
val mk_tid_gen : string -> tid_gen

(** [default_doc_spec] is without summary and references.  *)
val default_doc_spec : DocSpec.t

(** [cond_no_cond] is default conditional specification that has no [if]
    expression and no repetition. *)
val cond_no_cond : AttrSpec.ConditionalSpec.t

(** [default_attr_spec ~id] is initialized with default (empty) values except
    [id] set to [~id]. *)
val default_attr_spec : id:string -> AttrSpec.t

(** [default_meta_spec ~id] returns default [MetaSpec.t].

    The following meta section properties are set:
    - [endian] is set to [BE] (as per data-encoding default).
    - [imports] is set to [imports] if not empty.
    - Other fields are [[]] or [None]. *)
val default_meta_spec : ?imports:string list -> unit -> MetaSpec.t

(** [default_class_spec ?id] builds an default (empty) [ClassSpec.t].

    @param [?description] is added into [doc] section as [summary].
    @param [?imports] is added to class specification if not empty. *)
val default_class_spec :
  ?description:string -> ?imports:string list -> unit -> ClassSpec.t

(** [add_uniq_assoc kvs kv] returns an association list with associations from
    [kvs] as well as [kv].

    If [kvs] already contains [kv], then [kvs] is returned.

    @raises Invalid_argument if [kvs] includes an association with the same key
    as but a different value than [kv]. *)
val add_uniq_assoc : (string * 'a) list -> string * 'a -> (string * 'a) list

(** [class_spec_of_attrs ~id ?description ?enums ?instances attrs]
    returns a [ClassSpec.t] for the seq [attrs].

    @param [?description] is used as [doc] section [summary].
    @param [?enums] is added to class specification if present.
    @param [?types] is added to class specification if present.
    @param [?instances] is added to class specification if present.
    @param [?imports] is added to class specification if not empty.
*)
val class_spec_of_attrs :
  ?description:string ->
  ?enums:(string * EnumSpec.t) list ->
  ?types:(string * ClassSpec.t) list ->
  ?instances:(string * InstanceSpec.t) list ->
  ?imports:string list ->
  AttrSpec.t list ->
  ClassSpec.t

(** [default_instance_spec ~id expr] returns a default instance specification for
    of a given [id] and [expr]. *)
val default_instance_spec : id:string -> Ast.t -> InstanceSpec.t

(** [merge_summaries a s] adds the summary [s] to the doc of [a] preserving the
    existing doc if any. *)
val merge_summaries : AttrSpec.t -> string option -> AttrSpec.t
