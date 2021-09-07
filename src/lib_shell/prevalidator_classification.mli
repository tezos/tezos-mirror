(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type classification =
  [ `Applied
  | `Branch_delayed of tztrace
  | `Branch_refused of tztrace
  | `Refused of tztrace ]

type bounded_map

(** [map bounded_map] gets the underling map of the [bounded_map]. *)
val map : bounded_map -> (Operation.t * tztrace) Operation_hash.Map.t

type parameters = {
  map_size_limit : int;
  on_discarded_operation : Operation_hash.t -> unit;
}

type t = private {
  parameters : parameters;
  refused : bounded_map;
  branch_refused : bounded_map;
  branch_delayed : bounded_map;
  mutable applied_rev : (Operation_hash.t * Operation.t) list;
  mutable in_mempool : Operation_hash.Set.t;
}

(** [create parameters] returns an empty {!t} whose bounded maps hold
    at most [parameters.map_size_limit] values. The
    [on_discarded_operation] is called when a new operation is added
    and an old one is discarded because the limit was reached.

    {!Invalid_argument} is raised if [ring_size] is [0] or less.
    *)
val create : parameters -> t

(** [flush classes ~handle_branch_refused] resets fields of [classes]; except for
    the [refused] field which is never reset, to avoid revalidating
    operations that will never be valid. Also, the [branch_refused] field is
    reset iff [handle_branch_refused] is [true]. *)
val flush : t -> handle_branch_refused:bool -> unit

(** [is_in_mempool oph classes] indicates whether [oph] is present
    in field [in_mempool] of [classes]. *)
val is_in_mempool : Operation_hash.t -> t -> bool

(** [is_applied oph classes] indicates whether [oph] is present
    in field [applied_rev] of [classes]. *)
val is_applied : Operation_hash.t -> t -> bool

(** [remove oph classes] removes operation of hash [oph] from all
    fields of [classes].

    {b Warning:} If an operation is removed from the [applied] field,
    this may invalidate the classification of all the other
    operations. It is left to the caller to restore a consistent
    state. *)
val remove : Operation_hash.t -> t -> unit

(** [add ~notify classification oph op classes] adds the operation [op] with
    hash [oph] classified as [classification] to the classifier [classes]. The
    [classes.parameters.on_discarded_operation] callback is called for any operation discarded in this
    process. Currently, an operation is discarded in the following cases:

    - the corresponding error class field is full. In that case, the new operation is added to the class, and the removed one is discarded.
    - an operation is classified as [Refused].

    Note that a [Refused] operation may thus be passed twice to [on_discarded_operation]: as soon as it is added, and if it is removed because the [classes.refused] bounded map is full.

    As a summary:

    - [Applied] is never discarded
    - [Branch_refused] and [Branch_delayed] are discarded 0 or 1 time (if the corresponding bounded_map is full)
    - [Refused] is discarded 1 or 2 times (if the corresponding bounded_map is full)

    [notify] is called at the very beginning of [add]. Its goal is to remind call sites that classifying an operation should send a notification somewhere. Its presence in this API is arguable, and may change/move in the future. *)
val add :
  notify:(unit -> unit) ->
  classification ->
  Operation_hash.t ->
  Operation.t ->
  t ->
  unit

(** [map applied branch_delayed branch_refused refused t]
    returns the pairs [(operation_hash, operation)] contained in [t].
    Fields of [t] are included according to the value of the corresponding
    named argument. *)
val to_map :
  applied:bool ->
  branch_delayed:bool ->
  branch_refused:bool ->
  refused:bool ->
  t ->
  Operation.t Operation_hash.Map.t

(**/**)

module Internal_for_tests : sig
  val pp : Format.formatter -> t -> unit

  val bounded_map_pp : Format.formatter -> bounded_map -> unit

  (** Returns a deep copy of the input [t], so that mutating the one
      doesn't affect the other. *)
  val copy : t -> t
end
