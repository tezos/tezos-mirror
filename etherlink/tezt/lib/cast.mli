(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* This interface defines how tezt tests will interact with cast client.
   Cast is an ethereum client to interact with the node provided by Foundry
   https://book.getfoundry.sh/cast *)

(** [version ()] returns the version of cast. *)
val version : unit -> string Lwt.t

(** [craft_tx ~source_private_key ~chain_id ~nonce ~value ~gas
    ~gas_price ~address ?signature ?arguments ()] crafts and signs a
    transaction from [source_private_key] to [address]. Returns the
    rlp-encoded transaction. [signature] and [arguments] are parameters
    that are used when crafting a transaction that makes a call to a contract.
    [signature] is the function signature and [arguments] is a list for parameters.
    example:
    [craft_tx
      ~source_private_key
      ~chain_id:1337
      ~nonce:0
      ~value:Wei.zero
      ~gas:25_000
      ~gas_price:1_000_000
      ~address:"0xaaaa....aaaa"
      ~signature:"set(uint256)"
      ~arguments:["42"]
      ()] *)
val craft_tx :
  source_private_key:string ->
  chain_id:int ->
  nonce:int ->
  value:Wei.t ->
  gas:int ->
  gas_price:int ->
  address:string ->
  ?signature:string ->
  ?arguments:string list ->
  unit ->
  string Lwt.t
