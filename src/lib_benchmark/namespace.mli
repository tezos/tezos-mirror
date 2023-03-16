(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** [Namespace] implements a type for the name of the benchmarks in Snoop
    It can be seen as a path of several names, similar to a file system *)

(** The type of the namespaces *)
type t

val encoding : t Data_encoding.t

(** The empty namespace, printed as ["."] *)
val empty : t

val equal : t -> t -> bool

val is_empty : t -> bool

(** Tests if the namespace contains only one name *)
val is_singleton : t -> bool

val compare : t -> t -> int

val hash : t -> int

val to_string : t -> string

val of_string : string -> t

(** String representation without '/', usable for file names.
    It is usable for Graphviz vertex names with [String.escaped].

    Example:
    [to_filename (of_string "foo/bar/gee.txt") = "foo__bar__gee.txt"]
*)
val to_filename : t -> string

(** The type of namespace constructors, which represent intermediate namespaces.
    Given a [string], it returns a terminal namespace which is then used for benchmark names.
*)
type cons = string -> t

(* Add a string at the end of a namespace *)
val cons : t -> string -> t

(** The basic constructor for namespaces, at the root of every other namespace.
    [root "bench"] would return the namespace ["bench"] for a benchmark *)
val root : cons

(** Creates a constructor by appending a name to another constructor
    If [ns] represents the namespace [A/B/C/], then [make ns "D"] would return
    the constructor for the namespace [A/B/C/D/] *)
val make : cons -> string -> cons

(** Returns the basename of [t], which is the name furthest from the root.
    [basename empty] is ["."].
    [basename (cons s)] is [s] for any constructor [cons]. *)
val basename : t -> string

(** Returns the path of [n] as a list. The head of the list is the start of the path,
   which is always ["."]. If [n] is empty, only returns [["."]].
   Never returns the empty list. *)
val to_list : t -> string list

(** [name_match pattern name] returns [true] if and only if [pattern] is a prefix of [name].*)
val name_match : t -> t -> bool

(** Pretty printer *)
val pp : Format.formatter -> t -> unit

(** Short printer that only prints the basename of the namespace *)
val pp_short : Format.formatter -> t -> unit

module Hashtbl : Hashtbl.SeededS with type key = t

module Set : Set.S with type elt = t

module Map : Map.S with type key = t
