(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error = {code : int; message : string}

(** [block_number evm_node] calls [eth_blockNumber]. *)
val block_number : Evm_node.t -> (int32, error) result Lwt.t

(** [get_block_by_number ?full_tx_objets ~block evm_node] calls
    [eth_getBlockByNumber]. [full_tx_objects] is false by default, so
    the block contains the transaction hashes. [block] can be
    ["latest"] or its number. *)
val get_block_by_number :
  ?full_tx_objects:bool ->
  block:string ->
  Evm_node.t ->
  (Block.t, error) result Lwt.t

module Syntax : sig
  val ( let*@ ) : ('a, error) result Lwt.t -> ('a -> 'c Lwt.t) -> 'c Lwt.t

  val ( let*@? ) : ('a, error) result Lwt.t -> (error -> 'c Lwt.t) -> 'c Lwt.t
end

(** [produce_block ?timestamp evm_node] calls the private RPC [produceBlock]. If
    provided the block will have timestamp [timestamp] (in RFC3339) format. *)
val produce_block : ?timestamp:string -> Evm_node.t -> int32 Lwt.t

(** [inject_upgrade ~payload evm_node] calls the private RPC [injectUpgrade].
    It will store the [payload] under the kernel upgrade path. *)
val inject_upgrade : payload:string -> Evm_node.t -> unit Lwt.t
