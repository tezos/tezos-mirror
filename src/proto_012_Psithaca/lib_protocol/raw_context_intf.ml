(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

(** All context manipulation functions. This signature is included
    as-is for direct context accesses, and used in {!Storage_functors}
    to provide restricted views to the context. *)

module type VIEW = sig
  (* Same as [Environment_context.VIEW] but with extra getters and
     setters functions. *)

  (** The type for context handler. *)
  type t

  (** The type for context trees. *)
  type tree

  (** The type for context keys. *)
  type key = string list

  (** The type for context values. *)
  type value = bytes

  (** {2 Getters} *)

  (** [mem t k] is an Lwt promise that resolves to [true] iff [k] is bound
      to a value in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [mem_tree t k] is like {!mem} but for trees. *)
  val mem_tree : t -> key -> bool Lwt.t

  (** [get t k] is an Lwt promise that resolves to [Ok v] if [k] is
      bound to the value [v] in [t] and {!Storage_Error Missing_key}
      otherwise. *)
  val get : t -> key -> value tzresult Lwt.t

  (** [get_tree] is like {!get} but for trees. *)
  val get_tree : t -> key -> tree tzresult Lwt.t

  (** [find t k] is an Lwt promise that resolves to [Some v] if [k] is
      bound to the value [v] in [t] and [None] otherwise. *)
  val find : t -> key -> value option Lwt.t

  (** [find_tree t k] is like {!find} but for trees. *)
  val find_tree : t -> key -> tree option Lwt.t

  (** [list t key] is the list of files and sub-nodes stored under [k] in [t].
      The result order is not specified but is stable.

      [offset] and [length] are used for pagination. *)
  val list :
    t -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t

  (** {2 Setters} *)

  (** [init t k v] is an Lwt promise that resolves to [Ok c] if:

      - [k] is unbound in [t];
      - [k] is bound to [v] in [c];
      - and [c] is similar to [t] otherwise.

      It is {!Storage_error Existing_key} if [k] is already bound in [t]. *)
  val init : t -> key -> value -> t tzresult Lwt.t

  (** [init_tree] is like {!init} but for trees. *)
  val init_tree : t -> key -> tree -> t tzresult Lwt.t

  (** [update t k v] is an Lwt promise that resolves to [Ok c] if:

      - [k] is bound in [t];
      - [k] is bound to [v] in [c];
      - and [c] is similar to [t] otherwise.

      It is {!Storage_error Missing_key} if [k] is not already bound in [t]. *)
  val update : t -> key -> value -> t tzresult Lwt.t

  (** [update_tree] is like {!update} but for trees. *)
  val update_tree : t -> key -> tree -> t tzresult Lwt.t

  (** [add t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is bound to [v] in [c];
    - and [c] is similar to [t] otherwise.

    If [k] was already bound in [t] to a value that is physically equal
    to [v], the result of the function is a promise that resolves to
    [t]. Otherwise, the previous binding of [k] in [t] disappears. *)
  val add : t -> key -> value -> t Lwt.t

  (** [add_tree] is like {!add} but for trees. *)
  val add_tree : t -> key -> tree -> t Lwt.t

  (** [remove t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is unbound in [c];
    - and [c] is similar to [t] otherwise. *)
  val remove : t -> key -> t Lwt.t

  (** [remove_existing t k v] is an Lwt promise that resolves to [Ok c] if:

      - [k] is bound in [t] to a value;
      - [k] is unbound in [c];
      - and [c] is similar to [t] otherwise.*)
  val remove_existing : t -> key -> t tzresult Lwt.t

  (** [remove_existing_tree t k v] is an Lwt promise that reolves to [Ok c] if:

      - [k] is bound in [t] to a tree;
      - [k] is unbound in [c];
      - and [c] is similar to [t] otherwise.*)
  val remove_existing_tree : t -> key -> t tzresult Lwt.t

  (** [add_or_remove t k v] is:

      - [add t k x] if [v] is [Some x];
      - [remove t k] otherwise. *)
  val add_or_remove : t -> key -> value option -> t Lwt.t

  (** [add_or_remove_tree t k v] is:

      - [add_tree t k x] if [v] is [Some x];
      - [remove t k] otherwise. *)
  val add_or_remove_tree : t -> key -> tree option -> t Lwt.t

  (** {2 Folds} *)

  (** [fold ?depth t root ~init ~f] recursively folds over the trees
      and values of [t]. The [f] callbacks are called with a key relative
      to [root]. [f] is never called with an empty key for values; i.e.,
      folding over a value is a no-op.

      Elements are traversed in lexical order of keys.

      The depth is 0-indexed. If [depth] is set (by default it is not), then [f]
      is only called when the conditions described by the parameter is true:

      - [Eq d] folds over nodes and contents of depth exactly [d].
      - [Lt d] folds over nodes and contents of depth strictly less than [d].
      - [Le d] folds over nodes and contents of depth less than or equal to [d].
      - [Gt d] folds over nodes and contents of depth strictly more than [d].
      - [Ge d] folds over nodes and contents of depth more than or equal to [d]. *)
  val fold :
    ?depth:[`Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int] ->
    t ->
    key ->
    order:[`Sorted | `Undefined] ->
    init:'a ->
    f:(key -> tree -> 'a -> 'a Lwt.t) ->
    'a Lwt.t
end

module type TREE = sig
  (** [Tree] provides immutable, in-memory partial mirror of the
      context, with lazy reads and delayed writes. The trees are Merkle
      trees that carry the same hash as the part of the context they
      mirror.

      Trees are immutable and non-persistent (they disappear if the
      host crash), held in memory for efficiency, where reads are done
      lazily and writes are done only when needed, e.g. on
      [Context.commit]. If a key is modified twice, only the last
      value will be written to disk on commit. *)

  (** The type for context views. *)
  type t

  (** The type for context trees. *)
  type tree

  include VIEW with type t := tree and type tree := tree

  (** [empty _] is the empty tree. *)
  val empty : t -> tree

  (** [is_empty t] is true iff [t] is [empty _]. *)
  val is_empty : tree -> bool

  (** [kind t] is [t]'s kind. It's either a tree node or a leaf
      value. *)
  val kind : tree -> [`Value | `Tree]

  (** [to_value t] is [Some v] is [t] is a leaf tree and [None] otherwise. *)
  val to_value : tree -> value option Lwt.t

  (** [hash t] is [t]'s Merkle hash. *)
  val hash : tree -> Context_hash.t

  (** [equal x y] is true iff [x] and [y] have the same Merkle hash. *)
  val equal : tree -> tree -> bool

  (** {2 Caches} *)

  (** [clear ?depth t] clears all caches in the tree [t] for subtrees with a
      depth higher than [depth]. If [depth] is not set, all of the subtrees are
      cleared. *)
  val clear : ?depth:int -> tree -> unit
end

module type T = sig
  (** The type for root contexts. *)
  type root

  include VIEW

  module Tree :
    TREE
      with type t := t
       and type key := key
       and type value := value
       and type tree := tree

  (** Internally used in {!Storage_functors} to escape from a view. *)
  val project : t -> root

  (** Internally used in {!Storage_functors} to retrieve a full key
      from partial key relative a view. *)
  val absolute_key : t -> key -> key

  (** Raised if block gas quota is exhausted during gas
     consumption. *)
  type error += Block_quota_exceeded

  (** Raised if operation gas quota is exhausted during gas
     consumption. *)
  type error += Operation_quota_exceeded

  (** Internally used in {!Storage_functors} to consume gas from
     within a view. May raise {!Block_quota_exceeded} or
     {!Operation_quota_exceeded}. *)
  val consume_gas : t -> Gas_limit_repr.cost -> t tzresult

  (** Check if consume_gas will fail *)
  val check_enough_gas : t -> Gas_limit_repr.cost -> unit tzresult

  val description : t Storage_description.t
end
