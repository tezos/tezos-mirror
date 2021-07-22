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
  mutable applied : (Operation_hash.t * Operation.t) list;
  mutable in_mempool : Operation_hash.Set.t;
}

(** [create parameters] returns an empty {!t} whose bounded maps hold
   at most [parameters.map_size_limit] values. The
   [on_discarded_operation] is called when a new operation is added
   and an old one is discarded because the limit was reached.

   {!Invalid_argument} is raised if [ring_size] is [0] or less.
   *)
val create : parameters -> t

(** [clear classes] resets the state of all fields of [classes],
    * except for [refused] *)
val clear : t -> unit

(** [is_in_mempool oph classes] indicates whether [oph] is present
      in field [in_mempool] of [classes]. *)
val is_in_mempool : Operation_hash.t -> t -> bool

(** [is_applied oph classes] indicates whether [oph] is present
      in field [applied] of [classes]. *)
val is_applied : Operation_hash.t -> t -> bool

(** [remove_applied oph classes] removes operation of hash [oph] from
   fields [applied] and [in_mempool] of [classes]. The operations
   which were applied but not the one removed are sent back to the
   caller as a map. Consequently, the order in which these operations
   were applied might be lost. *)
val remove_applied :
  Operation_hash.t -> t -> Operation.t Operation_hash.Map.t option

(** [remove_not_applied oph classes] removes operation of hash [oph]
      from all fields of [classes] except from [applied]. *)
val remove_not_applied : Operation_hash.t -> t -> unit

(** [add ~on_discarded_operation class oph op classes] adds the
   operation [op] with hash [oph] classified as [class] to the
   classifier [classes]. The [on_discarded_operation] callback is
   called for any operation discarded in this process. Currently, an
   operation is discarded if the corresponding class field is full. In
   that case, the new operation is added to the class, and the one
   removed is discarded. An operation is also discarded when it is
   classified as [Refused]. The callback [on_discarded_operation]
   which was given by the function {create} when the value [classes]
   was created is called on each discarded operation. **)
val add :
  notify:(unit -> unit) ->
  classification ->
  Operation_hash.t ->
  Operation.t ->
  t ->
  unit
