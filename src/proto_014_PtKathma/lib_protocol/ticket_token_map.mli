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

(** [find ctxt k m] looks up the value with key [k] in the given map [m] and
    also accounts for the gas cost of finding the key. *)
val find :
  Alpha_context.context ->
  Ticket_token.ex_token ->
  'a t ->
  ('a option * Alpha_context.context) tzresult Lwt.t

(** [of_list ctxt ~merge_overlaps m] creates a map from a list of key-value
    pairs. In case there are overlapping keys, their values are combined
    using the [merge_overlap] function. The function accounts for gas for
    traversing the elements. [merge_overlap] should account for its own gas
    cost. *)
val of_list :
  Alpha_context.context ->
  merge_overlap:
    (Alpha_context.context ->
    'a ->
    'a ->
    ('a * Alpha_context.context, error trace) result) ->
  (Ticket_token.ex_token * 'a) list ->
  ('a t * Alpha_context.context) tzresult Lwt.t

(** [to_list m] transforms a map [m] into a list. It also accounts for the gas
    cost for traversing the elements. *)
val to_list :
  Alpha_context.context ->
  'a t ->
  ((Ticket_token.ex_token * 'a) list * Alpha_context.context) tzresult

(** [map ctxt f m] maps over all key-value pairs in the map [m] using the
    function [f]. It accounts for gas costs associated with traversing the
    elements. [f] must account for its own gas cost. *)
val map :
  Alpha_context.context ->
  (Alpha_context.context ->
  Ticket_token.ex_token ->
  'a ->
  ('b * Alpha_context.context) tzresult) ->
  'a t ->
  ('b t * Alpha_context.context) tzresult

(** [merge ctxt ~merge_overlap m1 m2] merges the maps [m1] and [m2]. In case
    there are overlapping keys, their values are combined using the
    [merge_overlap] function. Gas costs for traversing all elements from both
    maps are accounted for. [merge_overlap] must account for its own gas
    costs. *)
val merge :
  Alpha_context.context ->
  merge_overlap:
    (Alpha_context.context -> 'a -> 'a -> ('a * Alpha_context.context) tzresult) ->
  'a t ->
  'a t ->
  ('a t * Alpha_context.context) tzresult
