(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type version_range = {range_start : int; range_end : int option}

val in_range : version_range -> Protocol.t -> bool

(**
  Represents a Michelson script on the filesystem. Documentation for each field:
  - [prefix]
    This is the path to the scripts directory, for instance: [michelson_scripts].
  - [dirname]
    This is the relative path under [prefix] that leads you to the actual script.
  - [basename]
    This is the name of the file in which the script is stored. E.g: [foobar_001.tz].
  - [name]
    This is the logical name of the script. E.g: if basename = foobar_001.tz then name = foobar.
  - [version_range]
    If the basename contains a version suffix [foobar_MMM(_NNN).tz], then this value is
    [{range_start = MMM, (range_end = NNN)}].
    If there is no version suffix, then this value is [{range_start = 000, range_end = None}].
  - [depth]
    Depth in the directory at which this script is stored. Should equal [List.length dirname].
*)
type t = {
  prefix : string;
  dirname : string list;
  basename : string;
  name : string;
  version_range : version_range;
  extension : string;
  depth : int;
}

(** This is the directory that will be used as the "root", otherwise known as
    the "prefix", where michelson scripts will be searched for. *)
val default_prefix : string

(** Find a Michelson script file.

    Usage: [find name protocol]

    This returns [PREFIX/NAME_MMM_NNN.tz] for the highest [MMM] and [NNN]
    such that [MMM <= Protocol.number protocol <= NNN] and such that this file
    exists. If no such file exists but [PREFIX/NAME_MMM.tz] or [PREFIX/NAME.tz]
    does, this returns the matching file instead. Else, this fails.

    The intent is that:
    - new scripts are added without a [_MMM] suffix;
    - if a script needs to be adapted for a new protocol, the file is
      duplicated and the new version is suffixed with [_MMM];
    - if a script is no longer needed for future protocols, it can be
      disabled by attaching [_NNN] such that NNN is the last protocol
      which it is valid for.

    [name] is a list of path items where all but the last one are subdirectories
    and the last one is the base filename without [_NNN.tz].

    For instance, assume the following files exist:
    - [michelson_scripts/foo/bar/baz.tz]
    - [michelson_scripts/foo/bar/baz_015.tz]
    Then [path ["foo"; "bar"; "baz"] Lima] returns:
    [michelson_scripts/foo/bar/baz_015.tz]
    while [path ["foo"; "bar"; "baz"] Kathmandu] returns:
    [michelson_scripts/foo/bar/baz.tz]. *)
val find : ?prefix:string -> ?maxdepth:int -> string list -> Protocol.t -> t

(** [find_all ?prefix ?maxdepth protocol] returns all Michelson scripts for a given
    protocol up to a maxdepth [maxdepth]. Setting [~maxdepth:1]
    is useful when you don't want to recurse into subdirectories.
    
    For instance, assume the following files exist:
    - [prefix/a.tz]
    - [prefix/a_015.tz]
    - [prefix/a_016.tz]
    - [prefix/b/c.tz]
    - [prefix/b/c_015.tz]
    - [prefix/b/c_016.tz]
    - [prefix/d/e/f.tz]
    - [prefix/d/e/f_016.tz]

    And assume [p] is a [Protocol.t] such that [Protocol.number p = 015].
    
    [find_all p] returns:
    - [prefix/a_015.tz]
    - [prefix/b/c_015.tz]
    - [prefix/d/e/f.tz]
    *)
val find_all : ?prefix:string -> ?maxdepth:int -> Protocol.t -> t list

val find_all_res :
  ?prefix:string ->
  ?maxdepth:int ->
  Protocol.t ->
  (t list, string * string) result

(** [find_all_in protocol dirs] returns all scripts in the folders [dirs] for the given protocol. *)
val find_all_in :
  ?prefix:string -> ?maxdepth:int -> Protocol.t -> string list -> t list

(** Returns all scripts in the [legacy] directory for the given protocol. *)
val find_all_legacy : ?prefix:string -> ?maxdepth:int -> Protocol.t -> t list

(** Returns all well typed scripts for the given protocol. *)
val find_all_well_typed :
  ?prefix:string -> ?maxdepth:int -> Protocol.t -> t list

(** Returns all ill typed scripts for the given protocol. *)
val find_all_ill_typed : ?prefix:string -> ?maxdepth:int -> Protocol.t -> t list

(** Returns all TZT tests from the reference test suite. *)
val find_all_tzt_tests : ?prefix:string -> ?maxdepth:int -> Protocol.t -> t list

(** The path to the script relative to [/] (the repo root).

    If [?no_prefix] is true, the prefix is excluded from the output string.
    This is mostly useful for pretty printing in tests. *)
val path : ?no_prefix:bool -> t -> string

(** The logical name of the script. This includes the names of all the
    directories leading to the script as well as the basename of the script
    itself without any version information or extensions.

    For instance, assume the following files exist:
    - prefix/foo_123.tz
    - prefix/bar.tz
    - prefix/baz/quux.tz

    If each of those files is represented by a value [v] of type [t], then
    calling [name v] on each [v] would result in the following:
    - ["foo"]
    - ["bar"]
    - ["baz"; "quux"]
*)
val name : t -> string list

(** The logical name as a string. This is [name t |> String.concat "/"].
    This function is mostly used as a convenient pretty printer.

    NOTE: [Filename.dir_sep] is _not_ used because [name] is not a path. *)
val name_s : t -> string

(** Pretty printer for [t]. This is mostly useful for debugging. *)
val pretty_string : t -> string
