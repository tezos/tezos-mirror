(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

(** An in-memory data-structure for a key-value map where all operations
    account for gas costs.
 *)
module type S = sig
  type 'a t

  (** The type of keys in the map. *)
  type key

  (** [empty] an empty map. *)
  val empty : 'a t

  (** [size m] returns the number of elements of the map [m] in constant time. *)
  val size : 'a t -> int

  (** [find ctxt k m] looks up the value with key [k] in the given map [m] and
      also consumes the gas associated with the lookup. The complexity is
      logarithmic in the size of the map. *)
  val find :
    Alpha_context.context ->
    key ->
    'a t ->
    ('a option * Alpha_context.context) tzresult

  (** [update ctxt k f map] updates or adds the value of the key [k] using [f].
      The function accounts for the gas cost for finding the element. The updating
      function [f] should also account for its own gas cost. The complexity is
      logarithmic in the size of the map. *)
  val update :
    Alpha_context.context ->
    key ->
    (Alpha_context.context ->
    'a option ->
    ('a option * Alpha_context.context) tzresult) ->
    'a t ->
    ('a t * Alpha_context.context) tzresult

  (** [to_list m] transforms a map [m] into a list. It also accounts for the
      gas cost for traversing the elements. The complexity is linear in the size
      of the map. *)
  val to_list :
    Alpha_context.context ->
    'a t ->
    ((key * 'a) list * Alpha_context.context) tzresult

  (** [of_list ctxt ~merge_overlaps m] creates a map from a list of key-value
      pairs. In case there are overlapping keys, their values are combined
      using the [merge_overlap] function. The function accounts for gas for
      traversing the elements. [merge_overlap] should account for its own gas
      cost. The complexity is [n * log n] in the size of the list.
      *)
  val of_list :
    Alpha_context.context ->
    merge_overlap:
      (Alpha_context.context ->
      'a ->
      'a ->
      ('a * Alpha_context.context) tzresult) ->
    (key * 'a) list ->
    ('a t * Alpha_context.context) tzresult

  (** [merge ctxt ~merge_overlap m1 m2] merges the maps [m1] and [m2]. In case
      there are overlapping keys, their values are combined using the
      [merge_overlap] function. Gas costs for traversing all elements from both
      maps are accounted for. [merge_overlap] should account for its own gas
      cost. The complexity is [n * log n], where [n]
      is [size m1 + size m2]. *)
  val merge :
    Alpha_context.context ->
    merge_overlap:
      (Alpha_context.context ->
      'a ->
      'a ->
      ('a * Alpha_context.context) tzresult) ->
    'a t ->
    'a t ->
    ('a t * Alpha_context.context) tzresult

  (** [map ctxt f m] maps over all key-value pairs in the map [m] using the
      function [f]. It accounts for gas costs associated with traversing the
      elements. The mapping function [f] should also account for its own gas
      cost. The complexity is linear in the size of the map [m]. *)
  val map :
    Alpha_context.context ->
    (Alpha_context.context ->
    key ->
    'a ->
    ('b * Alpha_context.context) tzresult) ->
    'a t ->
    ('b t * Alpha_context.context) tzresult

  (** [fold ctxt f z m] folds over the key-value pairs of the given map [m],
      accumulating values using [f], with [z] as the initial state. The function
      [f] must account for its own gas cost. The complexity is linear in the
      size of the map [m]. *)
  val fold :
    Alpha_context.context ->
    (Alpha_context.context ->
    'state ->
    key ->
    'value ->
    ('state * Alpha_context.context) tzresult) ->
    'state ->
    'value t ->
    ('state * Alpha_context.context) tzresult
end

(** A module type for comparable values that also includes a cost function
    for metering gas depending on the cost of comparing values. *)
module type COMPARABLE = sig
  include Compare.COMPARABLE

  (** [compare_cost k] returns the cost of comparing the given key [k] with
      another value of the same type. *)
  val compare_cost : t -> Alpha_context.Gas.cost
end

(** A functor for building gas metered maps. *)
module Make (O : COMPARABLE) : S with type key := O.t
