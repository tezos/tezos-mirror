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

(** A replacement for {!Stdlib.Seq} which
    - is exception-safe,
    - includes Lwt-, result- and Lwt-result-aware traversal functions.

    See {!Lwtreslib} for a general description of traversors and the meaning for
    the name suffixes. A full description is also below. *)
module type S = sig
  include Bare_sigs.Seq.S

  (** ['error trace] is intended to be substituted by a type provided by a
      [Trace] module ([with type 'error trace := 'error Trace.trace]) *)
  type 'error trace

  (** Similar to {!iter} but wraps the iteration in [result Lwt.t]. All the
      steps of the iteration are started concurrently. The promise [iter_ep]
      resolves once all the promises of the traversal resolve. At this point it
      either:
      - is rejected if at least one of the promises is, otherwise
      - is fulfilled with [Error _] if at least one of the promises is,
        otherwise
      - is fulfilled with [Ok ()] if all the promises are. *)
  val iter_ep :
    ('a -> (unit, 'error trace) result Lwt.t) ->
    'a t ->
    (unit, 'error trace) result Lwt.t

  (** Similar to {!iteri} but wraps the iteration in [result Lwt.t]. All the
      steps of the iteration are started concurrently. The promise [iteri_ep]
      resolves once all the promises of the traversal resolve. At this point it
      either:
      - is rejected if at least one of the promises is, otherwise
      - is fulfilled with [Error _] if at least one of the promises is,
        otherwise
      - is fulfilled with [Ok ()] if all the promises are. *)
  val iteri_ep :
    (int -> 'a -> (unit, 'error trace) result Lwt.t) ->
    'a t ->
    (unit, 'error trace) result Lwt.t

  (** Similar to {!iter2} but wraps the iteration in [result Lwt.t]. All the
      steps of the iteration are started concurrently. The promise [iter2_ep]
      resolves once all the promises of the traversal resolve. At this point it
      either:
      - is rejected if at least one of the promises is, otherwise
      - is fulfilled with [Error _] if at least one of the promises is,
        otherwise
      - is fulfilled with [Ok ()] if all the promises are.

      Following the behaviour of the Stdlib, the two-sequence traversors stop as
      soon as one of the two sequences ends: the suffix of the longer sequence
      is ignored. *)
  val iter2_ep :
    ('a -> 'b -> (unit, 'error trace) result Lwt.t) ->
    'a t ->
    'b t ->
    (unit, 'error trace) result Lwt.t
end
