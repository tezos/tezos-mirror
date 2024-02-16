(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** A bounded cache associating values to keys.

This data structure is basically a bounded association table that stores
(a finite number of) given [(key, value)], with the following properties:
{ul
{li The insertion ordering is remembered / important. When the structure is full,
    older entries are removed to insert new ones;}
{li Stored keys are unique in the data-structure.}
}
*)

module type NAME = sig
  val name : string
end

(** The required interface for keys stored in the table. *)
module type KEY = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t
end

(** The required interface for values stored in the table. *)
module type VALUE = sig
  type t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t
end

(** The exported interface of the data structure. *)
module type S = sig
  type t

  type key

  module Map : Map.S with type key = key

  type value

  type view = value Map.t

  (** [empty ~capacity] returns a new table whose maximum capacity is given. *)
  val empty : capacity:int64 -> t

  (** Export a view of the given bounded cache *)
  val view : t -> view

  (** Encoding for values of type {!t} *)
  val encoding : t Data_encoding.t

  (** Pretty-printer for values of type {!t} *)
  val pp : Format.formatter -> t -> unit

  (** [find key t] returns [Some value] if there exists some [value] associated
      to [key] in the table, and [None] otherwise. *)
  val find : key -> t -> value option

  type error +=
    | Key_bound_to_different_value of {
        key : key;
        existing_value : value;
        given_value : value;
      }

  (** [remember key value t] inserts a new entry [(key |-> value)] in [t].

      If [key] already exists in [t], its associated binding [value'] should
      be equal to [value]. In this case, [t] is returned unchanged. Otherwise,
      an error [Key_bound_to_different_value] is returned.

      If [key] is not already present in [t], the new binding (key |-> value) is
      inserted in [t]. If the number of elements would exceed [t]'s capacity
      after the insertion of the new binding, the oldest binding is removed
      from [t].

      The structure [t] is returned unchanged if its [capacity] is negative or
      null.
  *)
  val remember : key -> value -> t -> t tzresult

  module Internal_for_tests : sig
    (** A more flexible [empty] function for testing purpose. *)
    val empty : capacity:int64 -> next_index:int64 -> t

    (** [keys t] returns the keys of the entries stored in [t] in the order of
        their insertion. *)
    val keys : t -> key list
  end
end

module Make (Name : NAME) (Key : KEY) (Value : VALUE) :
  S with type key = Key.t and type value = Value.t
