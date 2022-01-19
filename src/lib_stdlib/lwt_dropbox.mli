(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** A 'dropbox' with a single element. *)

(** Type of dropbox holding a value of type ['a] *)
type 'a t

(** The exception returned when trying to access a 'closed' dropbox. *)
exception Closed

(** Create an empty dropbox. *)
val create : unit -> 'a t

(** [put t e] puts the element [e] inside the dropbox [t]. If the dropbox
    already held an element, the old element is discarded and replaced by the
    new one.

    @raise Closed if [close t] has been called. *)
val put : 'a t -> 'a -> unit

(** [take t] is a promise that resolves as soon as an element is held by [t].
    The element is removed from [t] when the promise resolves.

    If [t] already holds an element when [take t] is called, the promise
    resolves immediately. Otherwise, the promise resolves when an element is
    [put] there.

    @raise Closed if [close t] has been called. *)
val take : 'a t -> 'a Lwt.t

(** [take_with_timeout timeout t] behaves like [take t] except that it returns
    [None] if [timeout] resolves before an element is [put].

    Note that [timeout] is canceled (i.e., fails with [Canceled]) if an element
    is [put] in time (or if one is already present).

    @raise Closed if [close t] has been called. *)
val take_with_timeout : unit Lwt.t -> 'a t -> 'a option Lwt.t

(** [peek t] is [Some e] if [t] holds [e] and [None] if [t] does not hold any
    element.

    @raise Closed if [close t] has been called. *)
val peek : 'a t -> 'a option

(** [close t] closes the dropbox [t]. It terminates all the waiting reader with
    the exception [Closed]. All further read or write will also immediately
    fail with [Closed], except if the dropbox is not empty when
    [close] is called. In that case, a single (and last) [take] will
    succeed. *)
val close : 'a t -> unit
