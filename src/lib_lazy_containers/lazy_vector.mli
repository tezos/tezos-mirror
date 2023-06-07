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

(**
  A lazy vector is a fixed-size key-value association where each value is
  created dynamically.

  In many ways this data structure mimics an array, but its lookup has
  logarithmic time complexity and it supports non-int keys.
*)

exception Bounds

exception SizeOverflow

(** [KeyS] is the qualifier signature for key types in the lazy vector.
    Externally visible and accessible keys of the lazy vector are always
    non-negative. However, the lazy vector implementation may internally use
    negative keys therefore modules of type [KeyS] must support them.
*)
module type KeyS = sig
  include Map.OrderedType

  val unsigned_compare : t -> t -> int

  val zero : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val pred : t -> t

  val succ : t -> t

  val to_string : t -> string
end

(* [S] is the signature of an instantiated lazy vector. *)
module type S = sig
  type key

  type 'a producer = key -> 'a Lwt.t

  module Map : Lazy_map.S with type key = key

  type 'a t

  (** [pp pp_value] gives you a pretty-printer. This function is a witness of
      internal mutation. *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** [to_string show vector] generates a string representation of [vector] by
      using [show] for its values. Like [pp] this function is witness of
      internal mutation. *)
  val to_string : ('a -> string) -> 'a t -> string

  (** [string_of_key key] turns the given [key] into a string. *)
  val string_of_key : key -> string

  (** [num_elements vector] returns the maximum number of elements in the lazy
      vector. *)
  val num_elements : 'a t -> key

  (** [create ?first_key ?values ?produce_value ?origin num_elements]
      produces a lazy vector with [num_elements] entries where each is
      created using [produce_value]. [values] may be provided to
      supply an initial set of entries. [first_key] specifies the
      first index of the vector if given and defaults to zero.

      {b Note:} The [produce_value] and [origin] arguments are
      expected to be used by the 'tree-encoding' library. If you want
      to pre-fill your vector, creates an empty vector and use [grow]
      or [set]. *)
  val create :
    ?first_key:key ->
    ?values:'a Map.Map.t ->
    ?produce_value:'a producer ->
    ?origin:Tezos_tree_encoding.wrapped_tree ->
    key ->
    'a t

  (** [origin vec] returns the tree of origin of the vector, if it exists.

      {b Note:} The sole consumer of this function is expected to be
      the tree-encoding library. *)
  val origin : 'a t -> Tezos_tree_encoding.wrapped_tree option

  (** [empty ()] creates a vector of size zero. This is used in conjunction with
      {!cons} to model list-like structure. *)
  val empty : unit -> 'a t

  (** [singleton v] creates a vector of size one containing only [v]. *)
  val singleton : 'a -> 'a t

  (** [of_list values] creates a vector where each association is the index in
      the list to its value. The first item's key is [zero], the second is
      [succ zero] and so on.

      {b Note:} This function may be dangerous to use in a tick, if
      the size of [of_list] is unbounded. *)
  val of_list : 'a list -> 'a t

  (** [get key vector] retrieves the element at [key].

      @raises Exn.Bounds when trying to access an invalid key  *)
  val get : key -> 'a t -> 'a Lwt.t

  (** [set key value vector] sets the element at [key] to [value].

      @raises Exn.Bounds when trying to set an invalid key *)
  val set : key -> 'a -> 'a t -> 'a t

  (** [cons value vector] prepends a value to the front and grows the vector by
      one. That value can then be accessed using the [zero] key. *)
  val cons : 'a -> 'a t -> 'a t

  (** [split vec at] splits [vec] into two sub vectors at element
      [at]. The first vector has [at] elements, the second [length vec
      - at] elements.

      @raise Bounds when [at < 0]
      @raise Bounds when [at > num_elements vec] *)
  val split : 'a t -> key -> 'a t * 'a t

  (** [grow delta ?default vector] creates a new lazy vector that has
      [delta] more items than [vector]. This also retains all values that have
      previously been created. New values will be created with [default]
      if provided.

      {b Note:} This function may be dangerous to use in a tick, if
      [delta] is unbounded, or if the result of [default] is
      large. *)
  val grow : ?default:(unit -> 'a) -> key -> 'a t -> 'a t

  (** [drop vector] removes the head from [vector] without returning it. It
      doesn't read the value before removing it.

      @raise Bounds when applied on an empty vector. *)
  val drop : 'a t -> 'a t

  (** [pop vector] removes the head from [vector], and returns it.

      @raise Bounds when applied on an empty vector. *)
  val pop : 'a t -> ('a * 'a t) Lwt.t

  (** [prepend_list l vec] adds the elements of [l] at the front of [vec].

      {b Note:} This function may be dangerous to use in a tick, if
      [List.length l] is significant. *)
  val prepend_list : 'a list -> 'a t -> 'a t

  (** [append elt vector] creates a new lazy vector that has one
      more item than [vector] whose value is [elt]. This is a shortcut
      for [vector |> grow Key.(succ zero) |> set (num_elements vector) elt].
      Also returns the key of the added element. *)
  val append : 'a -> 'a t -> 'a t * key

  (** [concat lhs rhs] concatenates two lazy vectors.

      {b Note:} This function maybe dangerous to use in a tick because
      {i every} entries of both [lhs] and [rhs] will be loaded in
      memory. *)
  val concat : 'a t -> 'a t -> 'a t Lwt.t

  (** [unsafe_concat] concatenates two lazy vectors, {b assuming every
      entries of both vectors are already loaded in memory}. *)
  val unsafe_concat : 'a t -> 'a t -> 'a t

  (** [to_list vector] extracts all values of the given [vector] and
      collects them in a list.

      {b Note:} This function may be dangerous to use in a tick
      because all entries of the vector are loaded in memory. *)
  val to_list : 'a t -> 'a list Lwt.t

  (** [loaded_bindings vector] returns the [(key * 'a) list] representation of
      the vector [vector] containing only the loaded values, in order of
      increasing keys. This function is a witness of internal mutations. *)
  val loaded_bindings : 'a t -> (key * 'a option) list

  (** [first_key v] returns the first key of the given vector [v]. *)
  val first_key : 'a t -> key

  (** [encoding len_encoding elem_encoding] returns [Tezos_tree_encoding] for the vector *)
  val encoding :
    key Tezos_tree_encoding.t ->
    'a Tezos_tree_encoding.t ->
    'a t Tezos_tree_encoding.t
end

module Make (Key : KeyS) : S with type key = Key.t

module IntVector : S with type key = int

module Int32Vector : S with type key = int32

module Int64Vector : S with type key = int64

module ZVector : S with type key = Z.t

(** [Make] generates a lazy vector module using a given [Key] module. *)
module Mutable : sig
  module type ImmutableS = S

  (** [S] is the signature of a mutable lazy vector module. *)
  module type S = sig
    type key

    module Vector : S with type key = key

    type 'a t

    val num_elements : 'a t -> key

    val of_immutable : 'a Vector.t -> 'a t

    val create :
      ?values:'a Vector.Map.Map.t ->
      ?produce_value:'a Vector.producer ->
      ?origin:Tezos_tree_encoding.wrapped_tree ->
      key ->
      'a t

    val origin : 'a t -> Tezos_tree_encoding.wrapped_tree option

    val get : key -> 'a t -> 'a Lwt.t

    val set : key -> 'a -> 'a t -> unit

    val grow : ?default:(unit -> 'a) -> key -> 'a t -> unit

    val append : 'a -> 'a t -> key

    val cons : 'a -> 'a t -> unit

    val drop : 'a t -> unit

    val pop : 'a t -> 'a Lwt.t

    (** [reset vec] empties [vec] completely. Contrary to [pop], no
        values are read from the underlying backend. *)
    val reset : 'a t -> unit

    val snapshot : 'a t -> 'a Vector.t
  end

  module Make (Vector : ImmutableS) :
    S with type key = Vector.key and module Vector = Vector

  module IntVector : S with type key = int and module Vector = IntVector

  module Int32Vector : S with type key = int32 and module Vector = Int32Vector

  module Int64Vector : S with type key = int64 and module Vector = Int64Vector

  module ZVector : S with type key = Z.t and module Vector = ZVector
end
