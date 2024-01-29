(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright 2014 OCamlPro                                                   *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** {2 Paths in JSON documents} *) (*****************************************)

(** An abstract type for paths into a JSON document.
    A sequence of sub-tree selectors to descend into a JSON tree. *)
type path = path_item list

(** A JSON sub-tree selector.
    Indendent from any concrete format (JSON pointer, JSON path, etc.)
    The semantics depends on the use (selection, insertion, etc.) *)
and path_item =
  [ `Field of string  (** A field in an object. *)
  | `Index of int  (** An index in an array. *)
  | `Star  (** Any / every field or index. *)
  | `Next  (** The next element after an array. *) ]

(** Pretty prints a path in JSON pointer format (RFC6901).  May throw
    {!Unsupported_path_item}. Use [~wildcards:false] to deactivate the
    support of wildcard path items, which may lead to
    {!Unsupported_path_item}. *)
val print_path_as_json_pointer :
  ?wildcards:bool -> Format.formatter -> path -> unit

(** Pretty prints a path in JSON path format. Use [~wildcards:false] to
    deactivate the support of wildcard path items, which may lead to
    {!Unsupported_path_item}. *)
val print_path_as_json_path :
  ?wildcards:bool -> Format.formatter -> path -> unit

(** Pretty prints a path in JSON pointer format into a fresh string.
    May throw {!Unsupported_path_item}. Use [~wildcards:false] to
    deactivate the support of wildcard path items, which may lead to
    {!Unsupported_path_item}. *)
val json_pointer_of_path : ?wildcards:bool -> path -> string

(** Parses a path from a string in JSON pointer format.  May throw
    {!Illegal_pointer_notation}. The string is expected to be ASCII
    compatible, including UTF-8. Use [~wildcards:false] to deactivate
    the support of wildcard path items, which may lead to
    {!Unsupported_path_item}. *)
val path_of_json_pointer : ?wildcards:bool -> string -> path

(** {2 Querying JSON documents} *)

(*******************************************)

(** Extracts the value located at a given path. If multiple locations
    satisfy the path (in presence of wildcard path items), the chosen
    one is unspecified. May throw [Not_found].

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. *)
val query : path -> Json_repr.ezjsonm -> Json_repr.ezjsonm

(** Extracts the values located at a given path (may be more than one
    in presence of wildcard path items). The order is unspecified.

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. *)
val query_all : path -> Json_repr.ezjsonm -> Json_repr.ezjsonm list

(** Insert a value at a given path. If multiple locations satisfy the
    path (in presence of wildcard path items), the chosen one is
    unspecified. Will create parent objects or arrays if needed, for
    instance inserting [3] at [/a/b/c] in [{}] will result in
    [{"a":{"b":{"c":3}}}]. Inserting in an array at an index bigger
    than the previous size will expand the array, filling potential
    missing cells with [`Null]. Inserting in an array at [`Index n]
    where [n] is negative inserts from the last element of the
    array. If a value is inserted at a location where there is already
    one, both are merged as if with {!merge}. May throw
    {!Cannot_merge} if the path is incompatible with the original
    object (such as inserting in a field of something which is not an
    object) or if the value is to be merged with an incompatible
    existing value.

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. *)
val insert : path -> Json_repr.ezjsonm -> Json_repr.ezjsonm -> Json_repr.ezjsonm

(** Same as {!insert}, except that if the path leads to a pre-existing
    value, it is replaced with the new one instead of being merged.

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. *)
val replace :
  path -> Json_repr.ezjsonm -> Json_repr.ezjsonm -> Json_repr.ezjsonm

(** Merges two compatible JSON values. Merges [`Null] with any JSON
    value. Merges two deeply equal values together. Merges two objects
    by merging their common fields and adding all the others. Merges
    two arrays by merging their common cells pairwise and adding the
    remaining ones if one array is bigger than the other. May throw
    {!Cannot_merge}.

    This function works with JSON data represented in the {!Json_repr.ezjsonm}
    format. See functor {!Make} for using another representation. *)
val merge : Json_repr.ezjsonm -> Json_repr.ezjsonm -> Json_repr.ezjsonm

(** {2 Errors} *) (**********************************************************)

(** When two incompatible objects are unsuccessfully merged. Comes
    with the path to the first incompatibility encountered.*)
exception Cannot_merge of path

(** An path litteral could not be parsed.  Comes with the original
    string, the position and an explanation. *)
exception Illegal_pointer_notation of string * int * string

(** An operation was given a path containing an unsupported construct.
    Comes with an explanation as its second argument. *)
exception Unsupported_path_item of path_item * string

(** Produces a human readable version of an error. *)
val print_error :
  ?print_unknown:(Format.formatter -> exn -> unit) ->
  Format.formatter ->
  exn ->
  unit

(** {2 Advanced interface for using a custom JSON representation} *)

(**********)

module Make (Repr : Json_repr.Repr) : sig
  (** Same as {!query} for a custom JSON representation. *)
  val query : path -> Repr.value -> Repr.value

  (** Same as {!query_all} for a custom JSON representation. *)
  val query_all : path -> Repr.value -> Repr.value list

  (** Same as {!insert} for a custom JSON representation. *)
  val insert : path -> Repr.value -> Repr.value -> Repr.value

  (** Same as {!replace} for a custom JSON representation. *)
  val replace : path -> Repr.value -> Repr.value -> Repr.value

  (** Same as {!merge} for a custom JSON representation. *)
  val merge : Repr.value -> Repr.value -> Repr.value
end
