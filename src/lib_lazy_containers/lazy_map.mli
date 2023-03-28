(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(** [KeyS] is the qualifier signature for key types in the lazy map.
    Externally visible and accessible keys of the lazy map are always
    non-negative. However, the lazy map implementation may internally use
    negative keys therefore modules of type [KeyS] must support them.
*)
module type KeyS = sig
  include Map.OrderedType

  val to_string : t -> string
end

(* [S] is the signature of an instantiated lazy map. *)
module type S = sig
  type key

  type 'a producer = key -> 'a Lwt.t

  module Map : Map.S with type key = key

  type 'a t

  (** [origin map] returns the tree of origin of the map, if it exists.

      {b Note:} The sole consumer of this function is expected to be
      the tree-encoding library. *)
  val origin : 'a t -> Tezos_tree_encoding.wrapped_tree option

  (** [string_of_key key] turns the given [key] into a string. *)
  val string_of_key : key -> string

  (** [pp pp_value] gives you a pretty-printer. This function is a witness of
      internal mutation. *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** [to_string show map] generates a string representation of [map] by using
      [show] for its values. Like [pp] this function is witness of internal
      mutation. *)
  val to_string : ('a -> string) -> 'a t -> string

  (** [create ?values ?produce_value ?origin num_elements] produces a
      lazy map with [num_elements] entries where each is created using
      [produce_value]. [values] may be provided to supply an initial
      set of entries.

      {b Note:} This function is intended to be used [produce_value]
      should only be used by the tree-encoding library. If you want to
      fill a newly created map with some value, use [values] or
      [set] on the empty map. *)
  val create :
    ?values:'a Map.t ->
    ?produce_value:'a producer ->
    ?origin:Tezos_tree_encoding.wrapped_tree ->
    unit ->
    'a t

  (** [get key map] retrieves the element at [key].

      @raise UnexpectedAccess when trying to access an invalid key
      (e.g., when the key has been deleted). *)
  val get : key -> 'a t -> 'a Lwt.t

  (** [set key value map] sets the element at [key] to [value].

      @raises Exn.Bounds when trying to set an invalid key *)
  val set : key -> 'a -> 'a t -> 'a t

  (** [remove key map] marks the element at [key] as removed. *)
  val remove : key -> 'a t -> 'a t

  (** [dup map] duplicates [map].

      {b Note:} the [produce_value] continuation is shared between the
      resulting map and [map], meaning that if said continuation
      carries a state, the two maps will interfere with each
      others. This is safe when used in conjunction with
      [lib_tree_encoding], because the continuation is pure in the
      sense it will always returns the same result when called with
      the same argument. *)
  val dup : 'a t -> 'a t

  (** [loaded_bindings map] returns the [(key * 'a) list] representation of the
      map [map] containing only the loaded values, in order of increasing keys.
      This function is a witness of internal mutations. *)
  val loaded_bindings : 'a t -> (key * 'a option) list
end

(** [UnexpectedAccess] is raised in the default of the [produce_value] argument
    to [S.create]. *)
exception UnexpectedAccess

module Make (Key : KeyS) : S with type key = Key.t

module LwtIntMap : S with type key = int

module LwtInt32Map : S with type key = int32

module LwtInt64Map : S with type key = int64

(** [Make] generates a lazy map module using a given [Key] module. *)
module Mutable : sig
  (** [S] is the signature of a mutable lazy map module. *)
  module type S = sig
    type key

    module Map : S with type key = key

    type 'a t

    val of_immutable : 'a Map.t -> 'a t

    val create :
      ?values:'a Map.Map.t ->
      ?produce_value:'a Map.producer ->
      ?origin:Tezos_tree_encoding.wrapped_tree ->
      unit ->
      'a t

    val get : key -> 'a t -> 'a Lwt.t

    val set : key -> 'a -> 'a t -> unit

    val remove : key -> 'a t -> unit

    val snapshot : 'a t -> 'a Map.t
  end

  module Make (Key : KeyS) : S with type key = Key.t

  module LwtIntMap : S with type key = int

  module LwtInt32Map : S with type key = int32

  module LwtInt64Map : S with type key = int64
end
