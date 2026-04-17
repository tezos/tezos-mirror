(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Trili Tech, <contact@trili.tech>                  *)
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

  (** The type used for the context. *)
  type context

  (** [empty] an empty map. *)
  val empty : 'a t

  (** [singleton k v] returns a map with a single key [k] and value [v] pair. *)
  val singleton : key -> 'a -> 'a t

  (** [size m] returns the number of elements of the map [m] in constant time. *)
  val size : 'a t -> int

  (** [find ctxt k m] looks up the value with key [k] in the given map [m] and
      also consumes the gas associated with the lookup. The complexity is
      logarithmic in the size of the map. *)
  val find : context -> key -> 'a t -> ('a option * context) tzresult

  (** [update ctxt k f map] updates or adds the value of the key [k] using [f].
      The function accounts for the gas cost for finding the element. The updating
      function [f] should also account for its own gas cost. The complexity is
      logarithmic in the size of the map. *)
  val update :
    context ->
    key ->
    (context -> 'a option -> ('a option * context) tzresult) ->
    'a t ->
    ('a t * context) tzresult

  (** [to_list m] transforms a map [m] into a list. It also accounts for the
      gas cost for traversing the elements. The complexity is linear in the size
      of the map. *)
  val to_list : context -> 'a t -> ((key * 'a) list * context) tzresult

  (** [of_list ctxt ~merge_overlaps m] creates a map from a list of key-value
      pairs. In case there are overlapping keys, their values are combined
      using the [merge_overlap] function. The function accounts for gas for
      traversing the elements. [merge_overlap] should account for its own gas
      cost. The complexity is [n * log n] in the size of the list.
      *)
  val of_list :
    context ->
    merge_overlap:(context -> 'a -> 'a -> ('a * context) tzresult) ->
    (key * 'a) list ->
    ('a t * context) tzresult

  (** [merge ctxt ~merge_overlap m1 m2] merges the maps [m1] and [m2]. In case
      there are overlapping keys, their values are combined using the
      [merge_overlap] function. Gas costs for traversing all elements from both
      maps are accounted for. [merge_overlap] should account for its own gas
      cost. The complexity is [n * log n], where [n]
      is [size m1 + size m2]. *)
  val merge :
    context ->
    merge_overlap:(context -> 'a -> 'a -> ('a * context) tzresult) ->
    'a t ->
    'a t ->
    ('a t * context) tzresult

  (** [map_e ctxt f m] maps over all key-value pairs in the map [m] using the
      function [f]. It accounts for gas costs associated with traversing the
      elements. The mapping function [f] should also account for its own gas
      cost. The complexity is linear in the size of the map [m]. *)
  val map_e :
    context ->
    (context -> key -> 'a -> ('b * context) tzresult) ->
    'a t ->
    ('b t * context) tzresult

  (** [fold_e ctxt f z m] folds over the key-value pairs of the given map [m],
      accumulating values using [f], with [z] as the initial state. The function
      [f] must account for its own gas cost. The complexity is linear in the
      size of the map [m]. *)
  val fold_e :
    context ->
    (context -> 'state -> key -> 'value -> ('state * context) tzresult) ->
    'state ->
    'value t ->
    ('state * context) tzresult

  (** Lwt-aware variant of {!fold_e}. *)
  val fold_es :
    context ->
    (context -> 'state -> key -> 'value -> ('state * context) tzresult Lwt.t) ->
    'state ->
    'value t ->
    ('state * context) tzresult Lwt.t
end

(** This module is used to provide the function for consuming gas when
    constructing carbonated maps. *)
module type GAS = sig
  (* The context type. *)
  type context

  (** [consume ctxt cost] returns a context where [cost] has been consumed. *)
  val consume :
    context ->
    Saturation_repr.may_saturate Saturation_repr.t ->
    context tzresult
end

(** Standard [Compare.COMPARE] extended with a [compare_cost] function
    specifying the cost for comparing values. *)
module type COMPARABLE = sig
  include Compare.COMPARABLE

  (** [compare_cost k] returns the cost of comparing the given key [k] with
      another value of the same type. *)
  val compare_cost : t -> Saturation_repr.may_saturate Saturation_repr.t
end

(** A functor for exposing the type of a carbonated map before 
    the carbonated make is created. This is useful in scenarios where 
    the map that will need to be carbonated is defined before the 
    gas consuming functions for the carbonation are available. 
    See for example [Raw_context].
*)
module Make_builder (C : COMPARABLE) : sig
  type 'a t

  module Make (G : GAS) :
    S with type key = C.t and type context = G.context and type 'a t := 'a t
end

(** A functor for building gas metered maps. When building a gas metered map via
    [Make(G)(C)], [C] is a [COMPARABLE] required to construct a the map while
    [G] is a module providing the gas consuming functions. The type of the
    context on which the gas consuming function operates is
    determined by [G.context].
*)
module Make (G : GAS) (C : COMPARABLE) :
  S with type key = C.t and type context = G.context
