(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
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
module type S = sig
  type 'a t

  type key

  (** [create n] is a table with at most [n] elements except when it has more.
      Specifically, the table may contain up to [n] _strongly-held_ bindings and
      an unlimited number of _weakly-held_ bindings.

      Strongly-held bindings are not garbage collected. Weakly-held bindings may
      be garbage collected. *)
  val create : int -> 'a t

  (** [add t k v] adds a strongly-held binding from key [k] to value [v] in the
      table. If the table is a full capacity for strongly-held bindings (i.e.,
      if there are as many strongly-held bindings as the size specified when the
      table was [create]d), the oldest strongly-held binding becomes
      weakly-held.

      Note however, that when inserting multiple bindings to the same key in the
      table, the previous bindings become unavailable immediately, but they may
      still count towards the strongly-held binding limits until there are more
      calls to [add]. In other words, the size limit for the strongly-held
      bindings may be lowered when inserting bindings from the same key.

      Unlike a usual hash-table, hash collisions are severe. Specifically, when
      inserting two elements that have the same hash, only the second element
      will be retrieved by [find_opt], [fold], and [iter].

  *)
  val add : 'a t -> key -> 'a -> unit

  (** [fold f t acc] folds the function [f] and value [acc] through the
      strongly-held bindings of [t]. It does not fold over the weakly-held
      bindings. Consequently, [fold] never folds over more bindings than the
      size bound of the table, even if the table temporarily holds more
      bindings. *)
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [iter f t] iterates the function [f] through the strongly-held bindings of
      [t]. It does not fold over the weakly-held bindings. Consequently, [iter]
      never calls [f] over more bindings than the size bound of the table, even
      if the table temporarily holds more bindings. *)
  val iter : (key -> 'a -> unit) -> 'a t -> unit

  (** [find_opt t k] is [Some v] if [t] holds a binding from [k] to [v] and
      [None] otherwise. Whether the binding is strongly- or weakly-held does not
      affect [find_opt]. As a result:

      - [find_opt] is able to return values that [fold] or [iter] would miss.
      - [find_opt] may return a value, and then, if the GC decides to collect
        some weakly-held bindings, may not be able to return the same value even
        if the table was not used. *)
  val find_opt : 'a t -> key -> 'a option

  (** [remove t k] removes the binding from [key] to the associated element in
      [t]. This works regardless of whether the binding is strongly- or
      weakly-held.

      Note however, that when removing a strongly-held binding, it may still
      count towards the limit of strongly-held bindings until further bindings
      are added. *)
  val remove : 'a t -> key -> unit

  (** [length t] is the number of bindings currently held by [t]. This include
      both strongly- and weakly-held bindings. As a result, [length t] is
      greater or equal to [fold (+) t 0]. *)
  val length : 'a t -> int
end

(** A bounded table which optimistically cheats on the bound and sometimes
    counts wrong. See [S] above for more details.

    For documentation about seeded/non-seeded, check [Hashtbl] in the standard
    library. *)
module MakeSeeded (K : Hashtbl.SeededHashedType) : S with type key = K.t

module Make (K : Hashtbl.HashedType) : S with type key = K.t
