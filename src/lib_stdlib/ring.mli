(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Imperative Ring Buffer *)

type 'a t

(** Allocates a ring buffer for a given number of values. *)
val create : int -> 'a t

(** Adds a value, dropping the oldest present one if full. *)
val add : 'a t -> 'a -> unit

(** Same as {!add}, but returns the dropped value if any. *)
val add_and_return_erased : 'a t -> 'a -> 'a option

(** Adds the values of a list, in order. *)
val add_list : 'a t -> 'a list -> unit

(** Removes all values in the ring buffer. *)
val clear : 'a t -> unit

(** Retrieves the most recent value, or [None] when empty. *)
val last : 'a t -> 'a option

(** Iterates over the elements, oldest to newest. *)
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

(** Retrieves the elements as a list, oldest first.. *)
val elements : 'a t -> 'a list

(** Ring Buffer Table

   The Ring Buffer Table implements a fixed-sized hash Table interface
   on top of the Ring Buffer: it extends the latter with a O(1)
   membership operation, and the ability to remove entries, while
   still preserving the fast O(1) [add] operation from the Ring
   Buffer.  *)

module type TABLE = sig
  type t

  type v

  (** [create size] initializes an empty table. *)
  val create : int -> t

  (** [add t v] adds v value to the table. If the table already
     contains size elements, the first element is removed and [v] is
     added. *)
  val add : t -> v -> unit

  (** [mem t v] checks if v is in the table. O(1) *)
  val mem : t -> v -> bool

  (** [remove t v] removes one element from the table *)
  val remove : t -> v -> unit

  (** [retest t] removes all bindings from the table *)
  val clear : t -> unit

  (** [elements t] returns the list of elements currently in the table *)
  val elements : t -> v list
end

module MakeTable (V : Hashtbl.HashedType) : TABLE with type v = V.t
