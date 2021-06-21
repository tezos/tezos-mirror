(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = {
  chain_name : Distributed_db_version.Name.t;
  distributed_db_version : Distributed_db_version.t;
  p2p_version : P2p_version.t;
}

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

(** Get the network protocol version to announce on peer connection.

    Use the highest [distributed_db_versions] and the highest [p2p_versions].
    The version also contains the [chain_name] since it is used to prevent
    peers from different networks to communicate.

    Neither [distributed_db_versions] nor [p2p_versions] can be empty. *)
val announced :
  chain_name:Distributed_db_version.Name.t ->
  distributed_db_versions:Distributed_db_version.t list ->
  p2p_versions:P2p_version.t list ->
  t

(** Try to find a version which is supported both by us and a peer.

    Usage: [select ~chain_name ~distributed_db_versions ~p2p_versions remote_version]

    If the chain name of [remote_version] is not equal to [chain_name],
    there is no compatible version.

    [distributed_db_versions] is the list of distributed database versions
    supported by the node.
    If the highest supported version is lesser or equal to the remote version,
    use this highest supported version.
    Otherwise, there is no compatible version.

    Similarly, [p2p_versions] is the list of peer-to-peer versions
    supported by the node. The rules to find a compatible version are the same
    as the ones for [distributed_db_versions].

    If there is no compatible version, return a [P2p_rejection.Rejecting] error. *)
val select :
  chain_name:Distributed_db_version.Name.t ->
  distributed_db_versions:Distributed_db_version.t list ->
  p2p_versions:P2p_version.t list ->
  t ->
  t Error_monad.tzresult

(**/**)

module Internal_for_tests : sig
  val mock : unit -> t
end
