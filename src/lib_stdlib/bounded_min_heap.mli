(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Bounded min-heap: keep only the [n] smallest elements. *)

(**
   [Make (ID) (E)] creates a min heap for element of type E.t using
   the [E.compare] function. The heap is bounded and adding more
   elements than its capacity will fail. The module [ID] is used to
   check if an element already exists at insertion.
*)
module Make
    (Id : Stdlib.Hashtbl.HashedType)
    (E : sig
      include Stdlib.Set.OrderedType

      val id : t -> Id.t
    end) : sig
  module Hash_map : Hashtbl.S with type key := Id.t

  type t

  (** [create capacity] creates a bounded sequence of at most
      [capacity] elements.

      Raise [Invalid_argument] if [size < 0] or [size > Sys.max_array_length]. *)
  val create : int -> t

  (** [insert e h] adds element [e] to bounded sequence [h] if [h] is
      not full.

      Worst-case complexity: O(log n) where n is the capacity of the
      heap.

      If there is an element [e'] with [E.id e' = E.id e], it will be
      replaced iff [E.compare e e' < 0] .  *)
  val insert : E.t -> t -> (unit, string) result

  (** [pop h] removes the smallest element from the heap [h],
      [None] if empty. *)
  val pop : t -> E.t option

  (** [peek_min h] returns, without removing, the smallest element from
      the heap [h], [None] if empty. *)
  val peek_min : t -> E.t option

  (** [elements h] returns the contents of [h] as a sorted
      list in increasing order according to [E.compare].

      Worst-case complexity: O(n log n) where n is the size of the heap.
  *)
  val elements : t -> E.t list

  (** [length h] is the number of elements held by [h]. *)
  val length : t -> int

  (** [mem elt h] returns true is the [elt] already exists in
      [h], i.e. E.compare returns 0. *)
  val mem : E.t -> t -> bool

  (** [find id h] returns the first elements of [h], that as [E.id e =
      id] is true. *)
  val find_opt : Id.t -> t -> E.t option

  (** [remove h id] removes the element with [id] from the heap, or leaves [h] unchanged if [id] is not present in [h]. *)
  val remove : t -> Id.t -> unit

  (** [remove_predicate f h] removes the elements [elt] for which [f elt = true] from
      the heap and returns the list of ids removed. *)
  val remove_predicate : (E.t -> bool) -> t -> Id.t list

  (** [clear h] deletes all data from the heap. *)
  val clear : t -> unit

  module Internal_for_tests : sig
    val check_heap_invariant :
      pp_id:(Format.formatter -> Id.t -> unit) ->
      pp_elt:(Format.formatter -> E.t -> unit) ->
      t ->
      unit
  end
end
