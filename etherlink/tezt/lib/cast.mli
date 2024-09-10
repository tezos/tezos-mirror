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
    ~gas_price ?legacy ~address ?signature ?arguments ()]
    crafts and signs a transaction from [source_private_key] to [address].
    Returns the rlp-encoded transaction. If [legacy] is false the transaction
    will be an eip1559 transaction. [signature] and [arguments] are parameters
    that are used when crafting a transaction that makes a call to a contract.
    [signature] is the function signature of the contract you want to call
    and [arguments] is a list for parameters.
    example:
    [craft_tx
      ~source_private_key
      ~chain_id:1337
      ~nonce:0
      ~value:Wei.zero
      ~gas:25_000
      ~gas_price:1_000_000
      ~legacy:false
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
  ?legacy:bool ->
  address:string ->
  ?signature:string ->
  ?arguments:string list ->
  unit ->
  string Lwt.t

(** Wallet is a struct containing private key and Ethereum address 
    derived from it (NOTE that public key is not included). *)
type wallet = {address : string; private_key : string}

(** [gen_wallets ~number ()]
    generates [number] random wallets and returns a list of [wallet] structs.
    This call does not store anything in the file system. *)
val gen_wallets : number:int -> unit -> wallet list Lwt.t

(** [calldata ?args signature] returns the encoded calldata. *)
val calldata : ?args:string list -> string -> string Lwt.t
