(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Promises to run in the background during tests. *)

(** Register a promise that will run in the background.

    After a test runs, [Test.run] waits for all registered promises to finish.

    If a registered promise raises an exception which is not [Lwt.Canceled],
    the current test fails immediately, but [Test.run] still waits for
    other background promises to finish.

    Make sure that the promise you register eventually resolves.
    If it doesn't, [stop] (and thus [Test.run], which calls [stop])
    will hang forever.

    Calls to [register] when no test is running result in an error. *)
val register : unit Lwt.t -> unit

(** Allow calls to [register] until [stop] is called.

    If a promise that is later [register]ed is rejected by an exception
    which is not [Lwt.Canceled], [start] calls its argument.
    This may occur several times, including during [stop].

    @raise Invalid_arg if calls to [register] are already allowed,
    i.e. if [start] has already been called without a corresponding [stop].

    Don't call this directly, it is called by {!Test.run}. *)
val start : (exn -> unit) -> unit

(** Let all [register]ed promises resolve, then stop allowing calls to [register].

    The promise returned by [stop] is never rejected except if you cancel it.
    If you do cancel it, you may have to call [stop] again though as it is not
    guaranteed that all background promises have also been canceled.
    In other words, canceling the promise returned by [stop] is probably
    a bad idea.

    Calling [stop] when calls to [register] are not allowed has no effect,
    i.e. you can call [stop] even with no corresponding [start].

    Don't call this directly, it is called by {!Test.run}. *)
val stop : unit -> unit Lwt.t
