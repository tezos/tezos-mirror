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

(** Evaluates fold_left_s on a batch of [n] elements and returns a pair
    containing the result of the first batch and the unprocessed elements *)
val fold_left_s_n :
  n:int -> ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> ('a * 'b list) Lwt.t

(** [dont_wait handler f] calls [f ()] and essentially ignores the returned
    promise. In particular it does not wait for the promise to resolve.

    [dont_wait] is meant as an alternative to [Lwt.async]. The former requires
    an explicit, local exception handler whereas the latter uses a global
    handler that is set by side-effects.

    CAVEAT!

    Note that, because of the semantics of execution in Lwt, the evaluation of
    [f ()] is immediate and some progress towards the resolution of the promise
    may happen immediately. Specifically, the progress towards the resolution of
    the promise [p] returned by [f ()] is made until the point where it yields.
    At that point, control comes back to the caller of [dont_wait] and
    continues. More concretely, consider the order of the side-effects in the
    following piece of code and in particular how the second side-effect in the
    order of execution is within the promise created by [dont_wait].

    [side_effect (); (* first *)
     dont_wait
       (fun exc -> ..)
       (fun () ->
          side_effect (); (* second *)
          Lwt.pause () >>= fun () ->
          side_effect (); (* delayed *)
          ..);
     side_effect (); (* third *)
    ]

    If you want to delay any progress towards promise resolution being made
    (e.g., if you need strong guarantees about side-effects because you are in a
    critical section), then you need to add an explicit cooperation point. You
    can use [Lwt.pause] at the very beginning of the promise you pass to
    [dont_wait]:
    [dont_wait handler (fun () -> Lwt.pause () >>= fun () -> ..)].

    With this pattern, in the expression
    [dont_wait handler (fun () -> Lwt.pause () >>= f)], the anonymous lambda
    ([(fun () -> …)]) is called immediately. However, when this call is
    evaluated, the call to [pause] immediately suspend progress towards the
    resolution of the promise, delaying the call [f ()].
*)
val dont_wait : (exn -> unit) -> (unit -> unit Lwt.t) -> unit

(** Lwt version of [TzList.find_map] *)
val find_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b option Lwt.t
