(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Tarides <contact@tarides.com>                          *)
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

module Kind = struct
  type t = [`Value | `Tree]
end

module type VIEW = sig
  (** @inline *)
  include Tezos_context_sigs.Context.VIEW
end

module type TREE = sig
  (** @inline *)
  include Tezos_context_sigs.Context.TREE
end

module type PROOF = sig
  (** @inline *)
  include Tezos_context_sigs.Context.PROOF
end

module type PROOF_ENCODING = sig
  (** @inline *)
  include Tezos_context_sigs.Context.PROOF_ENCODING
end

module type HASH_VERSION = sig
  (** @inline *)
  include Tezos_context_sigs.Context.HASH_VERSION
end

(* Copy of sigs/v3/context.mli:CACHE *)
module type CACHE = sig
  type t

  type size

  type index

  type identifier

  type key

  type value = ..

  val key_of_identifier : cache_index:index -> identifier -> key

  val identifier_of_key : key -> identifier

  val pp : Format.formatter -> t -> unit

  val find : t -> key -> value option Lwt.t

  val set_cache_layout : t -> size list -> t Lwt.t

  val update : t -> key -> (value * size) option -> t

  val sync : t -> cache_nonce:Bytes.t -> t Lwt.t

  val clear : t -> t

  val list_keys : t -> cache_index:index -> (key * size) list option

  val key_rank : t -> key -> int option

  val future_cache_expectation : t -> time_in_blocks:int -> t

  val cache_size : t -> cache_index:index -> size option

  val cache_size_limit : t -> cache_index:index -> size option
end

module type CORE = sig
  type t

  val set_protocol : t -> Tezos_crypto.Hashed.Protocol_hash.t -> t Lwt.t

  val get_protocol : t -> Tezos_crypto.Hashed.Protocol_hash.t Lwt.t

  val fork_test_chain :
    t ->
    protocol:Tezos_crypto.Hashed.Protocol_hash.t ->
    expiration:Time.Protocol.t ->
    t Lwt.t

  val set_hash_version :
    t -> Tezos_crypto.Hashed.Context_hash.Version.t -> t tzresult Lwt.t

  val get_hash_version : t -> Tezos_crypto.Hashed.Context_hash.Version.t
end

module type TREE_CORE = sig
  type t

  type tree

  type value

  val empty : t -> tree

  val is_empty : tree -> bool

  val kind : tree -> Kind.t

  val to_value : tree -> value option Lwt.t

  val of_value : t -> value -> tree Lwt.t

  val hash : tree -> Tezos_crypto.Hashed.Context_hash.t

  val equal : tree -> tree -> bool

  val clear : ?depth:int -> tree -> unit
end

module V2 = struct
  type depth = [`Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int]

  module type VIEW = sig
    type t

    type key

    type value

    type tree

    val mem : t -> key -> bool Lwt.t

    val mem_tree : t -> key -> bool Lwt.t

    val find : t -> key -> value option Lwt.t

    val find_tree : t -> key -> tree option Lwt.t

    val list :
      t -> ?offset:int -> ?length:int -> key -> (string * tree) trace Lwt.t

    val add : t -> key -> value -> t Lwt.t

    val add_tree : t -> key -> tree -> t Lwt.t

    val remove : t -> key -> t Lwt.t

    val fold :
      ?depth:depth ->
      t ->
      key ->
      init:'a ->
      f:(key -> tree -> 'a -> 'a Lwt.t) ->
      'a Lwt.t
  end

  module Kind = Kind

  module type TREE = sig
    type t

    type tree

    include VIEW with type t := tree and type tree := tree

    include
      TREE_CORE with type t := t and type tree := tree and type value := value
  end

  module type S = sig
    include VIEW with type key = string list and type value = bytes

    module Tree : sig
      include
        TREE
          with type t := t
           and type key := key
           and type value := value
           and type tree := tree

      val pp : Format.formatter -> tree -> unit
    end

    include CORE with type t := t
  end
end

module V3 = V2

module V4 = struct
  type depth = V3.depth

  module type VIEW = sig
    include V3.VIEW

    val fold :
      ?depth:depth ->
      t ->
      key ->
      order:[`Sorted | `Undefined] ->
      init:'a ->
      f:(key -> tree -> 'a -> 'a Lwt.t) ->
      'a Lwt.t
  end

  module Kind = Kind

  module type TREE = sig
    type t

    type tree

    include VIEW with type t := tree and type tree := tree

    include
      TREE_CORE with type t := t and type tree := tree and type value := value
  end

  module type S = sig
    include VIEW with type key = string list and type value = bytes

    module Tree : sig
      include
        TREE
          with type t := t
           and type key := key
           and type value := value
           and type tree := tree

      val pp : Format.formatter -> tree -> unit
    end

    include CORE with type t := t
  end

  module type CACHE = CACHE
end

module V5 = struct
  type depth = V4.depth

  type config = Tezos_context_sigs.Config.t

  let equal_config = Tezos_context_sigs.Config.equal

  module type VIEW = VIEW

  module Kind = Kind

  module type TREE = TREE

  module type S = sig
    val equal_config : config -> config -> bool

    include VIEW with type key = string list and type value = bytes

    module Tree : sig
      include
        TREE
          with type t := t
           and type key := key
           and type value := value
           and type tree := tree

      val pp : Format.formatter -> tree -> unit
    end

    include CORE with type t := t

    module Proof : PROOF

    type tree_proof := Proof.tree Proof.t

    type stream_proof := Proof.stream Proof.t

    type ('proof, 'result) verifier :=
      'proof ->
      (tree -> (tree * 'result) Lwt.t) ->
      ( tree * 'result,
        [ `Proof_mismatch of string
        | `Stream_too_long of string
        | `Stream_too_short of string ] )
      result
      Lwt.t

    val verify_tree_proof : (tree_proof, 'a) verifier

    val verify_stream_proof : (stream_proof, 'a) verifier
  end

  module type CACHE = CACHE
end

module V6 = V5
module V7 = V6
module V8 = V7
module V9 = V8
module V10 = V9
module V11 = V10
module V12 = V11

module type S = V7.S

module type Sigs = sig
  module V2 = V2
  module V3 = V3
  module V4 = V4
  module V5 = V5
  module V6 = V6
  module V7 = V7
  module V8 = V8
  module V9 = V9
  module V10 = V10
  module V11 = V11
  module V12 = V12

  module type VIEW = VIEW

  module type TREE = TREE

  module type S = S

  module type HASH_VERSION = HASH_VERSION

  module type CACHE = CACHE
end
