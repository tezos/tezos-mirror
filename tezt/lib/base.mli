(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** Base functions. *)

(** {2 Strings} *)

(** Same as [Filename.concat]. *)
val ( // ) : string -> string -> string

(** Same as [Printf.sprintf]. *)
val sf : ('a, unit, string) format -> 'a

(** {2 Concurrency Monad} *)

(** Same as [Lwt.bind]. *)
val (let*) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

(** Same as [Lwt.both]. *)
val (and*) : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

(** Same as [Lwt.return]. *)
val return : 'a -> 'a Lwt.t

(** Same as [Lwt.return_unit]. *)
val unit : unit Lwt.t

(** Same as [Lwt.return_none]. *)
val none : 'a option Lwt.t

(** Same as [Lwt.return_some]. *)
val some : 'a -> 'a option Lwt.t

(** {2 Lists} *)

(** Make a list of all integers between two integers.

    If the first argument is greater than the second argument,
    return the empty list. *)
val range : int -> int -> int list

(** Backport of [List.find_map] from OCaml 4.10. *)
val list_find_map : ('a -> 'b option) -> 'a list -> 'b option

(** {2 Regular Expressions} *)

(** Compiled regular expressions. *)
type rex

(** Compile a regular expression using Perl syntax. *)
val rex : string -> rex

(** Test whether a string matches a regular expression.

    Example: ["number 1234 matches" =~ rex "\\d+"] *)
val ( =~ ) : string -> rex -> bool

(** Negation of [=~]. *)
val ( =~! ) : string -> rex -> bool

(** Match a regular expression with one capture group. *)
val ( =~* ) : string -> rex -> string option

(** {2 Asynchronous Promises} *)

(** Register a promise so that [wait_for_async] handles it.

    It is important to use [async] instead of [Lwt.async] so that we wait for those
    promises to resolve (if only by being canceled) before moving on to other tests.

    Warning: if an [async] promise raises an exception other than [Test.Failed],
    it currently does not stop the test. You should probably not use [async], it
    is mostly for internal use. *)
val async : unit Lwt.t -> unit

(** Return a promise which resolves once all {!async}s have resolved. *)
val wait_for_async : unit -> unit Lwt.t

(** {2 Promises} *)

(** Repeat something a given amount of times. *)
val repeat : int -> (unit -> unit Lwt.t) -> unit Lwt.t


(** {2 Input/Output} *)

(** Open file, use function to write output then close the output. In case of
   error while writing, the channel is closed before raising the exception *)
val with_open_out : string -> (out_channel -> unit) ->unit

(** Open file, use function to read input then close the input. In case of
   error while reading, the channel is closed before raising the exception **)
val with_open_in : string -> (in_channel -> 'a) -> 'a

(** Read all contents from an input channel. *)
val read_all : Lwt_io.input_channel -> string Lwt.t

(** [read_file filename] returns the full contents of file [filename] *)
val read_file : string -> string Lwt.t
