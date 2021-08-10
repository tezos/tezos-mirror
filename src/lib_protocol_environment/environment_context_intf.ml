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

module type VIEW = sig
  (** @inline *)
  include Tezos_context_sigs.Context.VIEW
end

module type TREE = sig
  (** @inline *)
  include Tezos_context_sigs.Context.TREE
end

module type HASH_VERSION = sig
  (** @inline *)
  include Tezos_context_sigs.Context.HASH_VERSION
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

  val set_protocol : t -> Protocol_hash.t -> t Lwt.t

  val get_protocol : t -> Protocol_hash.t Lwt.t

  val fork_test_chain :
    t -> protocol:Protocol_hash.t -> expiration:Time.Protocol.t -> t Lwt.t

  val set_hash_version : t -> Context_hash.Version.t -> t tzresult Lwt.t

  val get_hash_version : t -> Context_hash.Version.t
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

  val list_keys : t -> cache_index:index -> (identifier * size) list

  val key_rank : t -> key -> int option

  val future_cache_expectation : t -> time_in_blocks:int -> t
end
