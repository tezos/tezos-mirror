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
  | `Prechecked
  | `Branch_delayed of tztrace
  | `Branch_refused of tztrace
  | `Refused of tztrace
  | `Outdated of tztrace ]

type bounded_map

(** [map bounded_map] gets the underling map of the [bounded_map]. *)
val map : bounded_map -> (Operation.t * tztrace) Operation_hash.Map.t

type parameters = {
  map_size_limit : int;
  on_discarded_operation : Operation_hash.t -> unit;
}

(** Invariants ensured by this module **provided that the caller does
   not {!add} an operation which is already present in [t]**:

    - The field [in_mempool] is the set of all operation hashes
   present in fields: [refused; branch_refused; branch_delayed;
   prechecked; applied].

    - An operation cannot be at the same time in two of the following
   fields: [refused; branch_refused; branch_delayed; prechecked;
   applied].

    Note: We could always enforce these invariants by checking in
   {!add} whether the operation is already present. However, this
   would make the behavior of {!add} less predictable, so we do not
   think this to be an improvement from the point of view of the
   caller. *)
type t = private {
  parameters : parameters;
  refused : bounded_map;
  outdated : bounded_map;
  branch_refused : bounded_map;
  branch_delayed : bounded_map;
  mutable applied_rev : (Operation_hash.t * Operation.t) list;
  mutable prechecked : Operation.t Operation_hash.Map.t;
  mutable in_mempool : (Operation.t * classification) Operation_hash.Map.t;
}

(** [create parameters] returns an empty {!t} whose bounded maps hold
    at most [parameters.map_size_limit] values. The
    [on_discarded_operation] is called when a new operation is added
    and an old one is discarded because the limit was reached.

    {!Invalid_argument} is raised if [ring_size] is [0] or less.
    *)
val create : parameters -> t

(** [is_empty t] returns [true] iff [t] doesn't contain any operation. *)
val is_empty : t -> bool

(** [is_in_mempool oph classes] indicates whether [oph] is present
    in field [in_mempool] of [classes]. It returns the corresponding
    operation and its classification if present, and None otherwise. *)
val is_in_mempool :
  Operation_hash.t -> t -> (Operation.t * classification) option

(** [remove oph classes] removes operation of hash [oph] from all
    fields of [classes]. If the [oph] was classified as [Applied], the
    function is in [O(n)] with [n] being the length of
    [classes.applied]. Otherwise, the function is [O(log n)] with [n]
    the number of operations in the corresponding class.

    If [oph] was found, its classification as well as the operation it
    was bound to are returned. If [oph] was not found, [None]
    is returned.

    {b Warning:} If an operation is removed from the [applied] field,
    this may invalidate the classification of all the other operations.
    It is left to the caller to restore a consistent state. *)
val remove : Operation_hash.t -> t -> (Operation.t * classification) option

(** [add ~notify classification oph op classes] adds the operation
   [op] with hash [oph] classified as [classification] to the
   classifier [classes]. The
   [classes.parameters.on_discarded_operation] callback is called for
   any operation discarded in this process. Currently, an operation is
   discarded in the following cases:

    - the corresponding error class field is full. In that case, the
   new operation is added to the class, and the removed one is
   discarded.

    - an operation is classified as [Refused].

    Note that a [Refused] operation may thus be passed twice to
   [on_discarded_operation]: as soon as it is added, and if it is
   removed because the [classes.refused] bounded map is full.

    As a summary:

    - [Applied] and [Prechecked] are never discarded

    - [Branch_refused] and [Branch_delayed] are discarded 0 or 1 time
   (if the corresponding bounded_map is full)

    - [Refused] is discarded 1 or 2 times (if the corresponding
   bounded_map is full) *)
val add : classification -> Operation_hash.t -> Operation.t -> t -> unit

(** Functions to query data on a polymorphic block-like type ['block]. *)
type 'block block_tools = {
  hash : 'block -> Block_hash.t;  (** The hash of a block *)
  operations : 'block -> Operation.t list list;
      (** The list of operations of a block ordered by their validation pass *)
  all_operation_hashes : 'block -> Operation_hash.t list list;
      (** The list of hashes of operations of a block ordered by their
      validation pass. Could be implemented
      using {!operations} but this lets an alternative implementation
      to be provided. *)
}

(** A wrapper over chain-related modules, to make client code easier to
    emulate, and hence to test *)
type 'block chain_tools = {
  clear_or_cancel : Operation_hash.t -> unit;
      (** Removes the operation from the distributed database. *)
  inject_operation : Operation_hash.t -> Operation.t -> unit Lwt.t;
      (** Puts the operation in the distributed database. Returns [false] if
      the [hash] is already in the distributed database *)
  new_blocks :
    from_block:'block -> to_block:'block -> ('block * 'block list) Lwt.t;
      (** [new_blocks ~from_block ~to_block] returns a pair [(ancestor,
      path)], where [ancestor] is the common ancestor of [from_block]
      and [to_block] and where [path] is the chain from [ancestor]
      (excluded) to [to_block] (included).

      @raise assert failure when the two provided blocks do not belong
      to the same [chain]. *)
  read_predecessor_opt : 'block -> 'block option Lwt.t;
      (** [read_predecessor_opt block] returns
      the direct predecessor of [block] or [None] if it cannot
      be found. *)
}

(** [recycle_operations] returns the new pending operations when
    a reorganisation or a head update occurs. Returned operations come from:

    1. operations in [from_branch] that are NOT in [to_branch],
    2. operations in the relevant classes of [classification]
    3. operations the [pending] map

    This function guarantees that the branch of all returned operations
    is in [live_blocks] ([live_blocks] acts as a filter).

    See also {!Internal_for_tests.handle_live_operations}. *)
val recycle_operations :
  from_branch:'block ->
  to_branch:'block ->
  live_blocks:Block_hash.Set.t ->
  classification:t ->
  pending:Operation.t Operation_hash.Map.t ->
  block_store:'block block_tools ->
  chain:'block chain_tools ->
  handle_branch_refused:bool ->
  Operation.t Operation_hash.Map.t Lwt.t

(**/**)

module Internal_for_tests : sig
  val pp : Format.formatter -> t -> unit

  val bounded_map_pp : Format.formatter -> bounded_map -> unit

  (** Returns a deep copy of the input [t], so that mutating the one
      doesn't affect the other. *)
  val copy : t -> t

  (** [set_of_bounded_map m] returns all the operation hashes in [m]. *)
  val set_of_bounded_map : bounded_map -> Operation_hash.Set.t

  (** [pp_t_sizes t] prints the [map_size_limit] parameter of [t]
      and the sizes of its fields (number of elements in the map and
      in the ring of [bounded_map] / length of list / cardinal of set). *)
  val pp_t_sizes : Format.formatter -> t -> unit

  (** [map applied branch_delayed branch_refused refused t]
    returns the pairs [(operation_hash, operation)] contained in [t].
    Fields of [t] are included according to the value of the corresponding
    named argument. *)
  val to_map :
    applied:bool ->
    prechecked:bool ->
    branch_delayed:bool ->
    branch_refused:bool ->
    refused:bool ->
    outdated:bool ->
    t ->
    Operation.t Operation_hash.Map.t

  (** [flush classes ~handle_branch_refused] partially resets [classes]:
      - fields [applied_rev] and [branch_delayed] are emptied;
      - field [branch_refused] is emptied iff [handle_branch_refused] is [true];
      - field [refused] is left unchanged, to avoid revalidating operations that
        will never be valid;
      - field [outdated] is left unchanged.
      Also updates field [in_mempool] to maintain the corresponding invariant
      of {!t}. *)
  val flush : t -> handle_branch_refused:bool -> unit

  (** [handle_live_operations chain_db from_branch to_branch is_branch_alive old_mempool]
      returns the operations from:

      1. [old_mempool],
      2. [from_branch] that are NOT in [to_branch],

      for the subset of these operations whose branch is up-to-date according
      to [is_branch_alive] (operations that are not alive are cleared).

      Returned operations can be considered pending afterwards and
      are eligible to be propagated. On the other hand, all operations
      from [ancestor] to [to_branch] are cleared and won't be propagated.

      Most general case:
                               to_branch ┐
                                  /      │
                from_branch      .       ├ path
                     \          /        │
                      .        .         ┘
                       \      /
                       ancestor

      Increment the head case:

                  to_branch = path
                     |
                from_branch = ancestor
      *)
  val handle_live_operations :
    block_store:'block block_tools ->
    chain:'block chain_tools ->
    from_branch:'block ->
    to_branch:'block ->
    is_branch_alive:(Block_hash.t -> bool) ->
    Operation.t Operation_hash.Map.t ->
    Operation.t Operation_hash.Map.t Lwt.t
end
