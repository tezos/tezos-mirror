(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** Generic interface for a datatype with comparison, pretty-printer
    and serialization functions. *)
module type T = sig
  type t

  include Compare.S with type t := t

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t option
end

(** Generic interface for a datatype with comparison, pretty-printer,
    serialization functions and a hashing function. *)
module type HASHABLE = sig
  include T

  type hash

  val hash : t -> hash

  val hash_raw : bytes -> hash
end

(** {2 Hash Types} *)

(** The signature of an abstract hash type, as produced by functor
    {!Make_SHA256}. The {!t} type is abstracted for separating the
    various kinds of hashes in the system at typing time. Each type is
    equipped with functions to use it as is of as keys in the database
    or in memory sets and maps. *)

module type MINIMAL_HASH = sig
  type t

  val name : string

  val title : string

  val pp : Format.formatter -> t -> unit

  val pp_short : Format.formatter -> t -> unit

  include Compare.S with type t := t

  val hash_bytes : ?key:bytes -> bytes list -> t

  val hash_string : ?key:string -> string list -> t

  val zero : t
end

module type RAW_DATA = sig
  type t

  val size : int (* in bytes *)

  val to_bytes : t -> bytes

  val of_bytes_opt : bytes -> t option

  val of_bytes_exn : bytes -> t
end

module type B58_DATA = sig
  type t

  val to_b58check : t -> string

  val to_short_b58check : t -> string

  val of_b58check_exn : string -> t

  val of_b58check_opt : string -> t option

  type Base58.data += Data of t

  val b58check_encoding : t Base58.encoding
end

module type ENCODER = sig
  type t

  val encoding : t Data_encoding.t

  val rpc_arg : t RPC_arg.t
end

module type SET =
  sig
    type elt
    (** The type of the set elements. *)

    type t
    (** The type of sets. *)

    val empty: t
    (** The empty set. *)

    val is_empty: t -> bool
    (** Test whether a set is empty or not. *)

    val mem: elt -> t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

    val add: elt -> t -> t
    (** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged
       (the result of the function is then physically equal to [s]).
       @before 4.03 Physical equality was not ensured. *)

    val singleton: elt -> t
    (** [singleton x] returns the one-element set containing only [x]. *)

    val remove: elt -> t -> t
    (** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged
       (the result of the function is then physically equal to [s]).
       @before 4.03 Physical equality was not ensured. *)

    val union: t -> t -> t
    (** Set union. *)

    val inter: t -> t -> t
    (** Set intersection. *)

    val diff: t -> t -> t
    (** Set difference: [diff s1 s2] contains the elements of [s1]
       that are not in [s2]. *)

    val compare: t -> t -> int
    (** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

    val equal: t -> t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)

    val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

    val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
       The elements of [s] are presented to [f] in increasing order
       with respect to the ordering over the type of the elements. *)

    val map: (elt -> elt) -> t -> t
    (** [map f s] is the set whose elements are [f a0],[f a1]... [f
        aN], where [a0],[a1]...[aN] are the elements of [s].

       The elements are passed to [f] in increasing order
       with respect to the ordering over the type of the elements.

       If no element of [s] is changed by [f], [s] is returned
       unchanged. (If each output of [f] is physically equal to its
       input, the returned set is physically equal to [s].)
       @since 4.04.0 *)

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s], in increasing order. *)

    val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

    val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. If [p] satisfies every element in [s],
       [s] is returned unchanged (the result of the function is then
       physically equal to [s]).
       @before 4.03 Physical equality was not ensured.*)

    val partition: (elt -> bool) -> t -> t * t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

    val cardinal: t -> int
    (** Return the number of elements of a set. *)

    val elements: t -> elt list
    (** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

    val min_elt_opt: t -> elt option
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or [None]
       if the set is empty.
        @since 4.05
    *)

    val max_elt_opt: t -> elt option
    (** Same as {!Set.S.min_elt_opt}, but returns the largest element of the
        given set.
        @since 4.05
    *)

    val choose_opt: t -> elt option
    (** Return one element of the given set, or [None] if
        the set is empty. Which element is chosen is unspecified,
        but equal elements will be chosen for equal sets.
        @since 4.05
    *)

    val split: elt -> t -> t * bool * t
    (** [split x s] returns a triple [(l, present, r)], where
          [l] is the set of elements of [s] that are
          strictly less than [x];
          [r] is the set of elements of [s] that are
          strictly greater than [x];
          [present] is [false] if [s] contains no element equal to [x],
          or [true] if [s] contains an element equal to [x]. *)

    val find_opt: elt -> t -> elt option
    (** [find_opt x s] returns the element of [s] equal to [x] (according
        to [Ord.compare]), or [None] if no such element
        exists.
        @since 4.05 *)

    val find_first_opt: (elt -> bool) -> t -> elt option
    (** [find_first_opt f s], where [f] is a monotonically increasing function,
       returns an option containing the lowest element [e] of [s] such that
       [f e], or [None] if no such element exists.
        @since 4.05
       *)

    val find_last_opt: (elt -> bool) -> t -> elt option
    (** [find_last_opt f s], where [f] is a monotonically decreasing function,
       returns an option containing the highest element [e] of [s] such that
       [f e], or [None] if no such element exists.
        @since 4.05
       *)

    val of_list: elt list -> t
    (** [of_list l] creates a set from a list of elements.
        This is usually more efficient than folding [add] over the list,
        except perhaps for lists with many duplicated elements.
        @since 4.02.0 *)
  end
(** Output signature of the functor {!Set.Make}. *)

module type MAP =
  sig
    type key
    (** The type of the map keys. *)

    type (+'a) t
    (** The type of maps from type [key] to type ['a]. *)

    val empty: 'a t
    (** The empty map. *)

    val is_empty: 'a t -> bool
    (** Test whether a map is empty or not. *)

    val mem: key -> 'a t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val add: key -> 'a -> 'a t -> 'a t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m] to a value that is physically equal to [y],
       [m] is returned unchanged (the result of the function is
       then physically equal to [m]). Otherwise, the previous binding
       of [x] in [m] disappears.
       @before 4.03 Physical equality was not ensured. *)

    val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
    (** [update x f m] returns a map containing the same bindings as
        [m], except for the binding of [x]. Depending on the value of
        [y] where [y] is [f (find_opt x m)], the binding of [x] is
        added, removed or updated. If [y] is [None], the binding is
        removed if it exists; otherwise, if [y] is [Some z] then [x]
        is associated to [z] in the resulting map.  If [x] was already
        bound in [m] to a value that is physically equal to [z], [m]
        is returned unchanged (the result of the function is then
        physically equal to [m]).
        @since 4.06.0
    *)

    val singleton: key -> 'a -> 'a t
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x].
        @since 3.12.0
     *)

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map.
       If [x] was not in [m], [m] is returned unchanged
       (the result of the function is then physically equal to [m]).
       @before 4.03 Physical equality was not ensured. *)

    val merge:
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        In terms of the [find_opt] operation, we have
        [find_opt x (merge f m1 m2) = f (find_opt x m1) (find_opt x m2)]
        for any key [x], provided that [f None None = None].
        @since 3.12.0
     *)

    val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    (** [union f m1 m2] computes a map whose keys is the union of keys
        of [m1] and of [m2].  When the same binding is defined in both
        arguments, the function [f] is used to combine them.
        This is a special case of [merge]: [union f m1 m2] is equivalent
        to [merge f' m1 m2], where
        - [f' _key None None = None]
        - [f' _key (Some v) None = Some v]
        - [f' _key None (Some v) = Some v]
        - [f' key (Some v1) (Some v2) = f key v1 v2]

        @since 4.03.0
    *)

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    (** [for_all p m] checks if all the bindings of the map
        satisfy the predicate [p].
        @since 3.12.0
     *)

    val exists: (key -> 'a -> bool) -> 'a t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfies the predicate [p].
        @since 3.12.0
     *)

    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p]. If [p] satisfies every binding in [m],
        [m] is returned unchanged (the result of the function is then
        physically equal to [m])
        @since 3.12.0
       @before 4.03 Physical equality was not ensured.
     *)

    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
        @since 3.12.0
     *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map.
        @since 3.12.0
     *)

    val bindings: 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order of keys with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Map.Make}.
        @since 3.12.0
     *)

    val min_binding_opt: 'a t -> (key * 'a) option
    (** Return the binding with the smallest key in the given map
       (with respect to the [Ord.compare] ordering), or [None]
       if the map is empty.
        @since 4.05
     *)

    val max_binding_opt: 'a t -> (key * 'a) option
    (** Same as {!Map.S.min_binding_opt}, but returns the binding with
        the largest key in the given map.
        @since 4.05
     *)

    val choose_opt: 'a t -> (key * 'a) option
    (** Return one binding of the given map, or [None] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
        @since 4.05
     *)

    val split: key -> 'a t -> 'a t * 'a option * 'a t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find_opt: key -> 'a t -> 'a option
    (** [find_opt x m] returns [Some v] if the current binding of [x]
        in [m] is [v], or [None] if no such binding exists.
        @since 4.05
    *)

    val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
    (** [find_first_opt f m], where [f] is a monotonically increasing function,
       returns an option containing the binding of [m] with the lowest key [k]
       such that [f k], or [None] if no such key exists.
        @since 4.05
       *)

    val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
    (** [find_last_opt f m], where [f] is a monotonically decreasing function,
       returns an option containing the binding of [m] with the highest key [k]
       such that [f k], or [None] if no such key exists.
        @since 4.05
       *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)
  end
(** Output signature of the functor {!Map.Make}. *)

module type INDEXES_SET = sig
  include SET

  val encoding : t Data_encoding.t
end

module type INDEXES_MAP = sig
  include MAP

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end

module type INDEXES = sig
  type t

  val to_path : t -> string list -> string list

  val of_path : string list -> t option

  val of_path_exn : string list -> t

  val prefix_path : string -> string list

  val path_length : int

  module Set : INDEXES_SET with type elt = t

  module Map : INDEXES_MAP with type key = t
end

module type HASH = sig
  include MINIMAL_HASH

  include RAW_DATA with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  include INDEXES with type t := t
end

module type MERKLE_TREE = sig
  type elt

  include HASH

  val compute : elt list -> t

  val empty : t

  type path = Left of path * t | Right of t * path | Op

  val compute_path : elt list -> int -> path

  val check_path : path -> elt -> t * int

  val path_encoding : path Data_encoding.t
end

module type SIGNATURE_PUBLIC_KEY_HASH = sig
  type t

  val pp : Format.formatter -> t -> unit

  val pp_short : Format.formatter -> t -> unit

  include Compare.S with type t := t

  include RAW_DATA with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  include INDEXES with type t := t

  val zero : t
end

module type SIGNATURE_PUBLIC_KEY = sig
  type t

  val pp : Format.formatter -> t -> unit

  include Compare.S with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  type public_key_hash_t

  val hash : t -> public_key_hash_t

  val size : t -> int (* in bytes *)

  val of_bytes_without_validation : bytes -> t option
end

module type SIGNATURE = sig
  module Public_key_hash : SIGNATURE_PUBLIC_KEY_HASH

  module Public_key :
    SIGNATURE_PUBLIC_KEY with type public_key_hash_t := Public_key_hash.t

  type t

  val pp : Format.formatter -> t -> unit

  include RAW_DATA with type t := t

  include Compare.S with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  val zero : t

  type watermark

  (** Check a signature *)
  val check : ?watermark:watermark -> Public_key.t -> t -> bytes -> bool
end

module type FIELD = sig
  type t

  (** The order of the finite field *)
  val order : Z.t

  (** minimal number of bytes required to encode a value of the field. *)
  val size_in_bytes : int

  (** [check_bytes bs] returns [true] if [bs] is a correct byte
      representation of a field element *)
  val check_bytes : Bytes.t -> bool

  (** The neutral element for the addition *)
  val zero : t

  (** The neutral element for the multiplication *)
  val one : t

  (** [add a b] returns [a + b mod order] *)
  val add : t -> t -> t

  (** [mul a b] returns [a * b mod order] *)
  val mul : t -> t -> t

  (** [eq a b] returns [true] if [a = b mod order], else [false] *)
  val eq : t -> t -> bool

  (** [negate x] returns [-x mod order]. Equivalently, [negate x] returns the
      unique [y] such that [x + y mod order = 0]
  *)
  val negate : t -> t

  (** [inverse_opt x] returns [x^-1] if [x] is not [0] as an option, else [None] *)
  val inverse_opt : t -> t option

  (** [pow x n] returns [x^n] *)
  val pow : t -> Z.t -> t

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes [(Option.get (of_bytes_opt t)) = t]. By default,
      little endian encoding is used and the given element is modulo the prime
      order *)
  val of_bytes_opt : Bytes.t -> t option

  (** Convert the value t to a bytes representation which can be used for
      hashing for instance. It is not required that [to_bytes (Option.get
      (of_bytes_opt t)) = t]. By default, little endian encoding is used, and
      length of the resulting bytes may vary depending on the order.
  *)
  val to_bytes : t -> Bytes.t
end

(** Module type for the prime fields GF(p) *)
module type PRIME_FIELD = sig
  include FIELD

  (** [of_z x] builds an element t from the Zarith element [x]. [mod order] is
      applied if [x >= order] or [x < 0]. *)
  val of_z : Z.t -> t

  (** [to_z x] builds a Zarith element, using the decimal representation.
      Arithmetic on the result can be done using the modular functions on
      integers *)
  val to_z : t -> Z.t
end

module type CURVE = sig
  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : FIELD

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array *)
  val of_bytes_opt : Bytes.t -> t option

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** Double the element *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return [true] if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t
end

module type PAIRING = sig
  module Gt : FIELD

  module G1 : CURVE

  module G2 : CURVE

  val miller_loop : (G1.t * G2.t) list -> Gt.t

  val final_exponentiation_opt : Gt.t -> Gt.t option

  val pairing : G1.t -> G2.t -> Gt.t
end

module type PVSS_ELEMENT = sig
   type t

   include B58_DATA with type t := t

   include ENCODER with type t := t
end

module type PVSS_PUBLIC_KEY = sig
   type t

   val pp : Format.formatter -> t -> unit

   include Compare.S with type t := t

   include RAW_DATA with type t := t

   include B58_DATA with type t := t

   include ENCODER with type t := t
end

module type PVSS_SECRET_KEY = sig
   type public_key

   type t

    include ENCODER with type t := t

    val to_public_key : t -> public_key
end

module type PVSS = sig
  type proof

  module Clear_share : PVSS_ELEMENT

  module Commitment : PVSS_ELEMENT

  module Encrypted_share : PVSS_ELEMENT

  module Public_key : PVSS_PUBLIC_KEY

  module Secret_key : PVSS_SECRET_KEY with type public_key := Public_key.t

  val proof_encoding : proof Data_encoding.t

  val check_dealer_proof :
    Encrypted_share.t list ->
    Commitment.t list ->
    proof:proof ->
    public_keys:Public_key.t list ->
    bool

  val check_revealed_share :
    Encrypted_share.t ->
    Clear_share.t ->
    public_key:Public_key.t ->
    proof ->
    bool

  val reconstruct : Clear_share.t list -> int list -> Public_key.t
end
