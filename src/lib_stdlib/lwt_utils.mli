(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

val never_ending : unit -> 'a Lwt.t

(** [worker name ~on_event ~run ~cancel] internally calls [run ()] (which
    returns a promise [p]) and returns its own promise [work].
    If [p] becomes fulfilled, then [work] also becomes fulfilled.
    If [p] becomes rejected then [cancel ()] is called and, once its promise is
    resolved, [work] is fulfilled. This gives the opportunity for the function
    [cancel] to clean-up some resources.

    The function [on_event] is called at different times (start, failure, end)
    and is mostly meant as a logging mechanism but can also be used for other
    purposes such as synchronization between different workers.

    If the promises returned by [on_event] or [cancel] raise an exception or
    become rejected, the exception/failure is simply ignored and the promise is
    treated as having resolved anyway.

    Note that the promise [work] returned by the [worker] function is not
    cancelable. If you need to cancel the promise returned by [run], you need
    to embed your own synchronization system within [run]. E.g.,

    [let p, r = Lwt.wait in
     let run () =
        let main = … in
        Lwt.pick [main ; p]
     in]

*)
val worker :
  string ->
  on_event:(string -> [`Ended | `Failed of string | `Started] -> unit Lwt.t) ->
  run:(unit -> unit Lwt.t) ->
  cancel:(unit -> unit Lwt.t) ->
  unit Lwt.t
