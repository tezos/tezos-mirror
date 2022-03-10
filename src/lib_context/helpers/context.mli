(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

open Tezos_context_encoding.Context

module type DB = Irmin.Generic_key.S with module Schema = Schema

module Make_tree (Conf : Conf) (DB : DB) : sig
  include
    Tezos_context_sigs.Context.TREE
      with type t := DB.t
       and type key := DB.path
       and type value := DB.contents
       and type tree := DB.tree

  val pp : Format.formatter -> DB.tree -> unit

  val empty : _ -> DB.tree

  val of_value : _ -> DB.contents -> DB.tree Lwt.t

  type raw = [`Value of DB.contents | `Tree of raw String.Map.t]

  val raw_encoding : raw Data_encoding.t

  val to_raw : DB.tree -> raw Lwt.t

  val of_raw : raw -> DB.tree

  type kinded_key := [`Value of DB.contents_key | `Node of DB.node_key]

  type repo = DB.repo

  val make_repo : unit -> DB.repo Lwt.t

  val shallow : DB.repo -> kinded_key -> DB.tree

  val is_shallow : DB.tree -> bool

  (** Exception raised by [find_tree] and [add_tree] when applied to shallow
    trees. It is exposed for so that the memory context can in turn raise it. *)
  exception Context_dangling_hash of string
end

module Proof_encoding = Merkle_proof_encoding

module Make_proof (DB : DB) (Store_conf : Tezos_context_encoding.Context.Conf) : sig
  module Proof : Tezos_context_sigs.Context.PROOF

  type kinded_key := [`Value of DB.contents_key | `Node of DB.node_key]

  type tree_proof := Proof.tree Proof.t

  type stream_proof := Proof.stream Proof.t

  type ('proof, 'result) producer :=
    DB.repo ->
    kinded_key ->
    (DB.tree -> (DB.tree * 'result) Lwt.t) ->
    ('proof * 'result) Lwt.t

  type ('proof, 'result) verifier :=
    'proof ->
    (DB.tree -> (DB.tree * 'result) Lwt.t) ->
    (DB.tree * 'result, [`Msg of string]) result Lwt.t

  val produce_tree_proof : (tree_proof, 'a) producer

  val verify_tree_proof : (tree_proof, 'a) verifier

  val produce_stream_proof : (stream_proof, 'a) producer

  val verify_stream_proof : (stream_proof, 'a) verifier
end

module Make_config (Conf : Irmin_pack.Conf.S) : sig
  val equal_config :
    Tezos_context_sigs.Config.t -> Tezos_context_sigs.Config.t -> bool

  val config : 'a -> Tezos_context_sigs.Config.t
end

type error += Unsupported_context_hash_version of Context_hash.Version.t

(** See [Tezos_context_sigs.Context.Proof_types.t] *)
type proof_version_expanded = {is_stream : bool; is_binary : bool}

val decode_proof_version :
  int -> (proof_version_expanded, [`Invalid_proof_version]) result

val encode_proof_version : is_stream:bool -> is_binary:bool -> int
