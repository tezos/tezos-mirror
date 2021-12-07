(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(**
   This type is used for data representing pending operations of the
   prevalidator.
*)
type t

(** The empty structure of pending operations. *)
val empty : t

(** [from_operations m] constructs a value of type [t] from the given map [m] *)
val from_operations : Operation.t Operation_hash.Map.t -> t

(** [hashes p] returns the set of hashes contained in [p] *)
val hashes : t -> Operation_hash.Set.t

(** [operations p] returns the Map of bindings [oph -> op] contained in [p] *)
val operations : t -> Operation.t Operation_hash.Map.t

(** [is_empty p] returns [true] if [p] has operations, [false] otherwise. *)
val is_empty : t -> bool

(** [mem oph p] returns [true] if [oph] is found in [p], [false] otherwise.

    Complexity is O(log(n)), where n is the number of operations (hashes) in the
    structure.
*)
val mem : Operation_hash.t -> t -> bool

(** [add oph op p] addd the binding [oph] |-> [op] into [p].

    Complexity is O(log(n)), where n is the number of operations (hashes) in the
    structure.
*)
val add : Operation_hash.t -> Operation.t -> t -> t

(** [remove oph op p] removes the binding [oph] from [p].

    Complexity is O(log(n)), where n is the number of operations (hashes) in the
    structure.
*)
val remove : Operation_hash.t -> t -> t

(** [cardinal p] returns the number of operations (hashes) in [p].

    Complexity is O(n), where n is the number of operations (hashes) in the
    structure.
*)
val cardinal : t -> int

(** [fold f p acc] applies the function [f] on every binding [oph] |-> [op] of
    [p], which also takes and updates [acc]
*)
val fold : (Operation_hash.t -> Operation.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [iter f p] is similar to [fold] where [acc] is unit *)
val iter : (Operation_hash.t -> Operation.t -> unit) -> t -> unit

(** [fold_es f p acc] is the Lwt version of [fold], except that [fold_es]
    returns if a value [Error e] is returned *)
val fold_es :
  (Operation_hash.t -> Operation.t -> 'a -> ('a, 'b) result Lwt.t) ->
  t ->
  'a ->
  ('a, 'b) result Lwt.t
