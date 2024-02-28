(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type error = {code : int; message : string; data : string option}

module Request : sig
  val eth_blockNumber : Evm_node.request

  val eth_getBlockByNumber :
    block:string -> full_tx_objects:bool -> Evm_node.request

  val produceBlock : ?timestamp:string -> unit -> Evm_node.request

  val eth_sendRawTransaction : raw_tx:string -> Evm_node.request

  val eth_getTransactionReceipt : tx_hash:string -> Evm_node.request

  val eth_estimateGas :
    eth_call:(string * Ezjsonm.value) list -> block:string -> Evm_node.request

  val eth_getTransactionCount :
    address:string -> block:string -> Evm_node.request

  val eth_getTransactionByHash : transaction_hash:string -> Evm_node.request

  val eth_getCode : address:string -> block:string -> Evm_node.request

  val net_version : Evm_node.request
end

(** [net_version evm_node] calls [net_version]. *)
val net_version : Evm_node.t -> (string, error) result Lwt.t

(** [get_transaction_by_hash ~transaction_hash evm_node] calls [eth_getTransactionByHash]. *)
val get_transaction_by_hash :
  transaction_hash:string ->
  Evm_node.t ->
  (Transaction.transaction_object, error) result Lwt.t

(** [get_code ~address evm_node] calls [eth_getCode]. *)
val get_code : address:string -> Evm_node.t -> (string, error) result Lwt.t

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

val get_gas_price : Evm_node.t -> Int32.t Lwt.t

module Syntax : sig
  val ( let*@ ) : ('a, error) result Lwt.t -> ('a -> 'c Lwt.t) -> 'c Lwt.t

  val ( let*@? ) : ('a, error) result Lwt.t -> (error -> 'c Lwt.t) -> 'c Lwt.t

  val ( let*@! ) :
    ('a option, error) result Lwt.t -> ('a -> 'c Lwt.t) -> 'c Lwt.t
end

(** [produce_block ?timestamp evm_node] calls the private RPC [produceBlock]. If
    provided the block will have timestamp [timestamp] (in RFC3339) format. *)
val produce_block : ?timestamp:string -> Evm_node.t -> int32 Lwt.t

(** [send_raw_transaction ~raw_tx evm_node] calls [eth_sendRawTransaction]
    with [raw_tx] as argument. *)
val send_raw_transaction :
  raw_tx:string -> Evm_node.t -> (string, error) result Lwt.t

(** [get_transaction_receipt ~tx_hash evm_node] calls
    [eth_getTransactionReceipt] with [tx_hash] as argument. *)
val get_transaction_receipt :
  tx_hash:string ->
  Evm_node.t ->
  (Transaction.transaction_receipt option, error) result Lwt.t

(** [estimate_gas eth_call evm_node] calls [eth_estimateGas] with [eth_call]
    as payload. *)
val estimate_gas :
  (string * Ezjsonm.value) list -> Evm_node.t -> (int64, error) result Lwt.t

(** [get_transaction_count ~address evm_node] calls [eth_getTransactionCount]
    with [address] as argument (on block ["latest"]). *)
val get_transaction_count :
  address:string -> Evm_node.t -> (int64, error) result Lwt.t

(** [tez_kernelVersion evm_node] calls [tez_kernelVersion]. Returns the
    kernel commit hash. *)
val tez_kernelVersion : Evm_node.t -> (string, error) result Lwt.t

(** [call ~to_ ~data] call [eth_call] with [to] and [data] as argument (on block
    latest) *)
val call :
  to_:string -> data:string -> Evm_node.t -> (string, error) result Lwt.t
