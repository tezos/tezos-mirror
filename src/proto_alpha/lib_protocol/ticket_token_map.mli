(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** A module exposing a carbonated map where keys are [Ticket_token.ex_token]
    values. *)

(** A map where keys are [Ticket_token.ex_token] values. *)
type 'a t

(** [empty] is a map without any elements. *)
val empty : 'a t

(** [update ctxt k f map] updates or adds the value of the key [k] using [f].
    The function accounts for the gas cost for finding the element. [f] must
    account for its own gas costs. *)
val update :
  Alpha_context.context ->
  Ticket_token.ex_token ->
  (Alpha_context.context ->
  'a option ->
  ('a option * Alpha_context.context) tzresult) ->
  'a t ->
  ('a t * Alpha_context.context) tzresult Lwt.t

(** [fold ctxt f z m] folds over the map [m] using the initial value [z] and
    the accumulator function [f]. [f] must account for its own gas costs.  *)
val fold :
  Alpha_context.context ->
  (Alpha_context.context ->
  'state ->
  Ticket_token.ex_token ->
  'a ->
  ('state * Alpha_context.context) tzresult) ->
  'state ->
  'a t ->
  ('state * Alpha_context.context) tzresult
