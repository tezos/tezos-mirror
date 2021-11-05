(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Base functions. *)

(** {2 Strings} *)

(** Same as [Filename.concat]. *)
val ( // ) : string -> string -> string

(** Same as [Printf.sprintf]. *)
val sf : ('a, unit, string) format -> 'a

(** {2 Concurrency Monad} *)

(** Same as [Lwt.bind]. *)
val ( let* ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

(** Same as [Lwt.both]. *)
val ( and* ) : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

(** Same as [Lwt.both], but immediately propagate exceptions.

    More precisely, if one of the two promises is rejected
    or canceled, cancel the other promise and reject the resulting
    promise immediately with the original exception. *)
val ( and*! ) : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

(** Same as [Lwt.return]. *)
val return : 'a -> 'a Lwt.t

(** Same as [Lwt.return_unit]. *)
val unit : unit Lwt.t

(** Same as [Lwt.return_none]. *)
val none : 'a option Lwt.t

(** Same as [Lwt.return_some]. *)
val some : 'a -> 'a option Lwt.t

(** Get the value of an option that must not be [None].

    Usage: [mandatory name option]

    [name] is used in the error message if [option] is [None]. *)
val mandatory : string -> 'a option -> 'a

(** {2 Lists} *)

(** Make a list of all integers between two integers.

    If the first argument is greater than the second argument,
    return the empty list. *)
val range : int -> int -> int list

(** Backport of [List.find_map] from OCaml 4.10. *)
val list_find_map : ('a -> 'b option) -> 'a list -> 'b option

(** [take n l] returns the first [n] elements of [l] if longer than [n],
    else [l] itself. *)
val take : int -> 'a list -> 'a list

(** {2 Regular Expressions} *)

(** Compiled regular expressions. *)
type rex

(** Compile a regular expression using Perl syntax. *)
val rex : ?opts:Re.Perl.opt list -> string -> rex

(** Convert a regular expression to a string using Perl syntax. *)
val show_rex : rex -> string

(** Test whether a string matches a regular expression.

    Example: ["number 1234 matches" =~ rex "\\d+"] *)
val ( =~ ) : string -> rex -> bool

(** Negation of [=~]. *)
val ( =~! ) : string -> rex -> bool

(** Match a regular expression with one capture group. *)
val ( =~* ) : string -> rex -> string option

(** Match a regular expression with two capture groups. *)
val ( =~** ) : string -> rex -> (string * string) option

(** [replace_string ~all rex ~by s] iterates on [s], and replaces every
    occurrence of [rex] with [by]. If [all = false], then only the first
    occurrence of [rex] is replaced. *)
val replace_string :
  ?pos:int ->
  (* Default: 0 *)
  ?len:int ->
  ?all:bool ->
  (* Default: true. Otherwise only replace first occurrence *)
  rex ->
  (* matched groups *)
  by:string ->
  (* replacement string *)
  string ->
  (* string to replace in *)
  string

(** {2 Promises} *)

(** Repeat something a given amount of times. *)
val repeat : int -> (unit -> unit Lwt.t) -> unit Lwt.t

(** {2 Input/Output} *)

(** Open file, use function to write output then close the output. In case of
   error while writing, the channel is closed before raising the exception *)
val with_open_out : string -> (out_channel -> unit) -> unit

(** Open file, use function to read input then close the input. In case of
   error while reading, the channel is closed before raising the exception **)
val with_open_in : string -> (in_channel -> 'a) -> 'a

(** [read_file filename] returns the full contents of file [filename] *)
val read_file : string -> string Lwt.t

(** {2 Common structures} *)

module String_map : Map.S with type key = string

module String_set : Set.S with type elt = string
