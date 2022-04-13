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

(** Runnable values. *)

(** Values that come with functions to handle them.

    This is intended mainly for use with ['a = Process.t].
    Indeed, it is convenient to define functions that declare both how to
    spawn processes and how to read their output. Sometimes you want
    the output, sometimes you just want the [Process.t] itself
    (e.g. to check its exit code). Instead of defining one function for
    each use case, you can define a single function that returns a [runnable],
    and use [let*!] when you need the output value, or [let*?] if you just
    need the process itself. There is no function to inject a value
    into the monad ([return]), as there is no need for it.

    For instance, let's say you have a function which runs [git log]:
    {[
      val git_log: unit -> (Process.t, string list) runnable
    ]}
    If you just want to check the exit code, use it like this:
    {[
      let*? process : Process.t = git_log () in
      Process.check process
    ]}
    If you just want to get its output, use it like this:
    {[
      let*! log : string list = git_log () in
    ]} *)
type ('a, 'b) t = {value : 'a; run : 'a -> 'b Lwt.t}

(** Apply the function of a runnable to its value. *)
val run : ('a, 'b) t -> 'b Lwt.t

(** Convert the output of a runnable to make another runnable. *)
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

(** Processes with associated run functions.

    Typically, processes are associated to run functions that parse their output
    into values of type ['a]. See [Base.runnable] for an example. *)
type 'a process = (Process.t, 'a) t

module Syntax : sig
  (** Part of the [Runnable] module that is intended to be [open]ed. *)

  (** Makes fields [value] and [run] available in the environment. *)
  type nonrec ('a, 'b) t = ('a, 'b) t = {value : 'a; run : 'a -> 'b Lwt.t}

  (** Same as [run], then continue with the given function. *)
  val ( let*! ) : ('a, 'b) t -> ('b -> 'c Lwt.t) -> 'c Lwt.t

  (** Get the value of a runnable and pass it to a continuation.

    You can also just access field [value] directly. *)
  val ( let*? ) : ('a, 'b) t -> ('a -> 'c) -> 'c
end
