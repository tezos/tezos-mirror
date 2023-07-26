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

open Alpha_context

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
  context ->
  Ticket_token.ex_token ->
  (context -> 'a option -> ('a option * context) tzresult) ->
  'a t ->
  ('a t * context) tzresult Lwt.t

(** [fold_e ctxt f z m] folds over the map [m] using the initial value [z] and
    the accumulator function [f]. [f] must account for its own gas costs.  *)
val fold_e :
  context ->
  (context ->
  'state ->
  Ticket_token.ex_token ->
  'a ->
  ('state * context) tzresult) ->
  'state ->
  'a t ->
  ('state * context) tzresult

(** Lwt-aware variant of {!fold_e}. *)
val fold_es :
  context ->
  (context ->
  'state ->
  Ticket_token.ex_token ->
  'a ->
  ('state * context) tzresult Lwt.t) ->
  'state ->
  'a t ->
  ('state * context) tzresult Lwt.t

(** [find ctxt k m] looks up the value with key [k] in the given map [m] and
    also accounts for the gas cost of finding the key. *)
val find :
  context ->
  Ticket_token.ex_token ->
  'a t ->
  ('a option * context) tzresult Lwt.t

(** [of_list ctxt ~merge_overlaps m] creates a map from a list of key-value
    pairs. In case there are overlapping keys, their values are combined
    using the [merge_overlap] function. The function accounts for gas for
    traversing the elements. [merge_overlap] should account for its own gas
    cost. *)
val of_list :
  context ->
  merge_overlap:(context -> 'a -> 'a -> ('a * context, error trace) result) ->
  (Ticket_token.ex_token * 'a) list ->
  ('a t * context) tzresult Lwt.t

(** [to_list m] transforms a map [m] into a list. It also accounts for the gas
    cost for traversing the elements. *)
val to_list :
  context -> 'a t -> ((Ticket_token.ex_token * 'a) list * context) tzresult

(** [map_e ctxt f m] maps over all key-value pairs in the map [m] using the
    function [f]. It accounts for gas costs associated with traversing the
    elements. [f] must account for its own gas cost. *)
val map_e :
  context ->
  (context -> Ticket_token.ex_token -> 'a -> ('b * context) tzresult) ->
  'a t ->
  ('b t * context) tzresult

(** [merge ctxt ~merge_overlap m1 m2] merges the maps [m1] and [m2]. In case
    there are overlapping keys, their values are combined using the
    [merge_overlap] function. Gas costs for traversing all elements from both
    maps are accounted for. [merge_overlap] must account for its own gas
    costs. *)
val merge :
  context ->
  merge_overlap:(context -> 'a -> 'a -> ('a * context) tzresult) ->
  'a t ->
  'a t ->
  ('a t * context) tzresult

(** [to_ticket_receipt ctxt ~owner t] converts a ticket token map into a ticket receipt.
    It also accounts for the gas cost for traversing map and unparsing the elements. *)
val to_ticket_receipt :
  context ->
  owner:Destination.t ->
  Z.t t ->
  (Ticket_receipt.t * context) tzresult Lwt.t
