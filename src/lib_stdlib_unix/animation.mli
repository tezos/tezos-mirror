(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** [make_with_animation] is meant to be used to execute time consuming
    functions that can be interrupted. Typically proof-of-work tasks. Whilst
    doing so, it displays a progress animation on the provided formatter
    (assumes support for '\b`). The animations leave the terminal clean.

    [make_with_animation ppf ~make ~on_retry seed] behaves as follows:
    (a) if [make seed] is [Ok v] (completion of the task), then it returns [v]
    (b) if [make seed] is [Error r] (task is incomplete), then
    (b.1) [on_retry t r] is evaluated where [t] is the time elapsed during the
    call to [make], and then
    (b.2) the result is used to attempt the task again. *)
val make_with_animation :
  Format.formatter ->
  make:('seed -> ('result, 'failure) result Lwt.t) ->
  on_retry:(Mtime.Span.t -> 'failure -> 'seed Lwt.t) ->
  'seed ->
  'result Lwt.t

(** The number of steps that the animation cycles through. *)
val number_of_frames : int

(** [display_progress ?every ?out ~pp_print_step f] calls
    [pp_print_step] when the first argument of [f] is called and
    increments the number of steps which will be given to
    [pp_print_step].

    If [every] is passed, it is instead displayed whenever [nb_step
    mod every = 0] (defaults to 1). Each time [pp_print_step] is
    called, the previous output will be erased and replaced by the new
    output.

    [pp_print_step] must only write on a single-line with no carriage
    return.

    @raise Invalid_argument if the given [every] is less than 1.*)
val display_progress :
  ?every:int ->
  ?out:Lwt_unix.file_descr ->
  pp_print_step:(Format.formatter -> int -> unit) ->
  ((unit -> unit Lwt.t) -> 'a Lwt.t) ->
  'a Lwt.t
