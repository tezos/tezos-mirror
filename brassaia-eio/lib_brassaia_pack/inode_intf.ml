(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import

module type Child_ordering = sig
  type key

  val key : Path.step -> key

  val index : depth:int -> key -> int
end

module type Snapshot = sig
  type hash

  type kinded_hash = Contents of hash | Node of hash [@@deriving brassaia]

  type entry = {step : string; hash : kinded_hash} [@@deriving brassaia]

  type inode_tree = {depth : int; length : int; pointers : (int * hash) list}
  [@@deriving brassaia]

  type v = Inode_tree of inode_tree | Inode_value of entry list
  [@@deriving brassaia]

  type inode = {v : v; root : bool} [@@deriving brassaia]
end

module type Value = sig
  type key

  include
    Brassaia.Node.Generic_key.S
      with type node_key = key
       and type contents_key = key

  val pred :
    t ->
    (Path.step option
    * [`Node of node_key | `Inode of node_key | `Contents of contents_key])
    list

  module Portable :
    Brassaia.Node.Portable.S with type node := t and type hash = hash

  val nb_children : t -> int

  (** Recompute hash for inodes, used in eager integrity checks.*)
  val recompute_hash : t -> hash
end

module type Raw = sig
  include Pack_value.S

  val depth : t -> int option

  exception Invalid_depth of {expected : int; got : int; v : t}

  val decode_children_offsets :
    entry_of_offset:(int63 -> 'a) ->
    entry_of_hash:(hash -> 'a) ->
    string ->
    int ref ->
    'a list
end

module type S = sig
  include Brassaia.Indexable.S

  module Hash : Brassaia.Hash.S with type t = hash

  val unsafe_find : check_integrity:bool -> [< read] t -> key -> value option

  module Val :
    Value
      with type t = value
       and type key = key
       and type hash = Hash.t
       and type Portable.hash := hash

  val decode_bin_length : string -> int -> int

  val integrity_check_inodes : [`Read] t -> key -> (unit, string) result

  val save : ?allow_non_root:bool -> 'a t -> value -> key
end

module type Compress = sig
  type hash

  type dict_key = int

  type pack_offset = int63

  type name = Indirect of dict_key | Direct of Path.step

  type address = Offset of pack_offset | Hash of hash

  type ptr = {index : dict_key; hash : address}

  type tree = {depth : dict_key; length : dict_key; entries : ptr list}

  type value = Contents of name * address | Node of name * address

  type v = Values of value list | Tree of tree

  type v1 = {mutable length : int; v : v}

  type tagged_v =
    | V1_stable of v
    | V1_unstable of v
    | V2_root of v1
    | V2_nonroot of v1

  type t = {hash : hash; tv : tagged_v} [@@deriving brassaia]
end

(** Unstable internal API agnostic about the underlying storage. Use it only to
    implement or test inodes. *)
module type Internal = sig
  type hash

  type key

  val pp_hash : hash Fmt.t

  module Snapshot : Snapshot with type hash = hash

  module Raw : Raw with type hash = hash and type key = key

  module Val : sig
    include Value with type hash = hash and type key = key

    val of_raw : (expected_depth:int -> key -> Raw.t option) -> Raw.t -> t

    val to_raw : t -> Raw.t

    val save :
      ?allow_non_root:bool ->
      add:(hash -> Raw.t -> key) ->
      index:(hash -> key option) ->
      mem:(key -> bool) ->
      t ->
      key

    val stable : t -> bool

    val length : t -> int

    val index : depth:int -> Path.step -> int

    (** Checks the integrity of an inode. *)
    val integrity_check : t -> bool

    module Concrete : sig
      (** {1 Concrete trees} *)

      (** The type for pointer kinds. *)
      type kinded_key =
        | Contents of contents_key
        | Contents_x of contents_key
        | Node of node_key
      [@@deriving brassaia]

      (** The type of entries. *)
      type entry = {name : Path.step; key : kinded_key} [@@deriving brassaia]

      (** The type for internal pointers between concrete {!type-tree}s. *)
      type 'a pointer = {index : int; pointer : hash; tree : 'a}
      [@@deriving brassaia]

      (** The type for trees. *)
      type 'a tree = {depth : int; length : int; pointers : 'a pointer list}
      [@@deriving brassaia]

      (** The type for concrete trees. *)
      type t = Tree of t tree | Values of entry list | Blinded
      [@@deriving brassaia]

      type len := [`Eq of int | `Ge of int]

      (** The type for errors. *)
      type error =
        [ `Invalid_hash of hash * hash * t
        | `Invalid_depth of int * int * t
        | `Invalid_length of len * int * t
        | `Duplicated_entries of t
        | `Duplicated_pointers of t
        | `Unsorted_entries of t
        | `Unsorted_pointers of t
        | `Blinded_root
        | `Too_large_values of t
        | `Empty ]
      [@@deriving brassaia]

      (** [pp_error] is the pretty-printer for errors. *)
      val pp_error : error Fmt.t
    end

    (** [to_concrete t] is the concrete inode tree equivalent to [t]. *)
    val to_concrete : t -> Concrete.t

    (** [of_concrete c] is [Ok t] iff [c] and [t] are equivalent.

        The result is [Error e] when a subtree tree of [c] has an integrity
        error. *)
    val of_concrete : Concrete.t -> (t, Concrete.error) result

    module Portable : sig
      (* Extend to the portable signature *)
      include module type of Portable

      module Proof : sig
        val of_concrete : Concrete.t -> proof

        (** This function produces unfindable keys. Only use in tests *)
        val to_concrete : proof -> Concrete.t
      end
    end

    val of_snapshot :
      Snapshot.inode ->
      index:(hash -> key) ->
      (expected_depth:int -> key -> Raw.t option) ->
      t
  end

  val to_snapshot : Raw.t -> Snapshot.inode

  module Compress : Compress with type hash := hash

  module Child_ordering : Child_ordering
end

module type Sigs = sig
  module type S = S

  module type Internal = Internal

  module type Child_ordering = Child_ordering

  module type Raw = Raw

  module type Snapshot = Snapshot

  exception Max_depth of int

  module Make_internal
      (Conf : Conf.S)
      (H : Brassaia.Hash.S)
      (Key : sig
        include Brassaia.Key.S with type hash = H.t

        val unfindable_of_hash : hash -> t
      end)
      (Node :
        Brassaia.Node.Generic_key.S
          with type hash = H.t
           and type contents_key = Key.t
           and type node_key = Key.t) :
    Internal with type hash = H.t and type key = Key.t

  module Make
      (H : Brassaia.Hash.S)
      (Key : Brassaia.Key.S with type hash = H.t)
      (Node :
        Brassaia.Node.Generic_key.S
          with type hash = H.t
           and type contents_key = Key.t
           and type node_key = Key.t)
      (Inter : Internal with type hash = H.t and type key = Key.t)
      (Pack :
        Indexable.S
          with type key = Key.t
           and type hash = H.t
           and type value = Inter.Raw.t) :
    S
      with type 'a t = 'a Pack.t
       and type key = Key.t
       and type hash = H.t
       and type value = Inter.Val.t
end
