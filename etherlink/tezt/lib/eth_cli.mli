(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
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

(** [balance ~account ~endpoint ()] asks the balance of [account] to the
    JSON-RPC API server listening at [endpoint]. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally. *)
val balance :
  ?runner:Runner.t -> account:string -> endpoint:string -> unit -> Wei.t Lwt.t

(** [transaction_send ~source_private_key ~to_public_key ~value
    ~endpoint ()] crafts and signs a transaction transferring [value] (as
    Wei) from [source_private_key] to [to_public_key], sends the raw
    transaction to the JSON-RPI API server listening at [endpoint].
    It is allowed to optionally provide:
    - The [runner] optional argument can be specified to execute the command 
      on a remote runner. If omitted, the command is executed locally. 
    - [data] The raw data field of the transaction.
      NB: Consider using [contract_send] instead of this function.
    - [gas_limit] The gas limit of the transaction. Will be estimated if not specified.
    - [gas_price] The gas price of the transaction, in wei. Defaults to 1 gwei. *)
val transaction_send :
  source_private_key:string ->
  to_public_key:string ->
  value:Wei.t ->
  endpoint:string ->
  ?runner:Runner.t ->
  ?data:string ->
  ?gas_limit:Z.t ->
  ?gas_price:Wei.t ->
  unit ->
  string Lwt.t

(** [transaction_get ~endpoint ~tx_hash ()] returns the transaction object
    of [tx_hash] if it exists. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val transaction_get :
  ?runner:Runner.t ->
  endpoint:string ->
  tx_hash:string ->
  unit ->
  Transaction.transaction_object option Lwt.t

(** [add_abi ~label ~abi ()] register an ABI (Application Binary Interface) in
    the client. [abi] should be a path to the json file. [label] is a string
    used by the client to register the ABI in memory so it can be refered to
    later.
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val add_abi :
  ?runner:Runner.t -> label:string -> abi:string -> unit -> unit Lwt.t

(** [update_abi ~label ~abi ()] update an ABI (Application Binary Interface) in
    the client. [abi] should be a path to the json file. [label] is a string
    used by the client to register the ABI in memory so it can be refered to
    later.
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val update_abi :
  ?runner:Runner.t -> label:string -> abi:string -> unit -> unit Lwt.t

(** [check_abi ~label ()] checks if an ABI is registered in the client. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val check_abi : ?runner:Runner.t -> label:string -> unit -> bool Lwt.t

(** [show_abi ~label ()] returns the ABI registered in the client. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val show_abi : ?runner:Runner.t -> label:string -> unit -> string Lwt.t

(** [deploy ?args ~source_private_key ~endpoint ~abi ~bin ()] crafts and sign a
    transaction deploying [bin] whose interface [abi] is registered in the
    client, and sends the raw transaction to the JSON-API server listening at
    [endpoint]. [bin] is a path to the binary file, and [abi] is the label used
    while registering the ABI in the client.
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.

    Returns a pair [(address, tx_hash)]. *)
val deploy :
  ?runner:Runner.t ->
  ?args:string ->
  source_private_key:string ->
  endpoint:string ->
  abi:string ->
  bin:string ->
  unit ->
  (string * string) Lwt.t

(** [contract_send ~source_private_key ~endpoint ~abi_label ~address
    ~method_call ?value ?gas ()] makes a call to a contract found at
    [address], with interface registered as [abi_labbel] in the
    client, signed with a user's [source_private_key].  [method_call]
    is the call data, as a solidity expression. If [gas] is specified,
    it will not try to estimate it.
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.

    This is a transaction to be included in a block.

    example:
    [contract_send
      ~source_private_key
      ~endpoint
      ~abi_label:"storage"
      ~address:"0xaaaa....aaaa"
      ~method_call:"set(42)"
      ()]*)
val contract_send :
  ?runner:Runner.t ->
  ?expect_failure:bool ->
  ?value:Wei.t ->
  ?gas:int ->
  ?gas_price:int ->
  source_private_key:string ->
  endpoint:string ->
  abi_label:string ->
  address:string ->
  method_call:string ->
  unit ->
  string Lwt.t

(** [contract_call ~endpoint ~abi_label ~address ~method_call ()]
    makes a call to a contract found at [address], with interface registered as
    [abi_label] in the client, signed with a user's [source_private_key].
    [method_call] is the call data, as a solidity expression.
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.

    This is a NOT transaction to be included in a block, but a simulation. It
    can be a state modifying transaction, in which case the modification are not
    included in a block. It can still be useful to test the result of a
    transaction before executing it "for real", however the result might be
    different as the state could change between testing and execution.

    example:
    [contract_call
      ~endpoint
      ~abi_label:"storage"
      ~address:"0xaaaa....aaaa"
      ~method_call:"get()"
      ()]*)
val contract_call :
  ?runner:Runner.t ->
  ?expect_failure:bool ->
  endpoint:string ->
  abi_label:string ->
  address:string ->
  method_call:string ->
  unit ->
  string Lwt.t

(** [get_block ~block_id ~endpoint ()] asks the block [block_id] (it can be a
    hash or a number) to the JSON-RPC API server listening at [endpoint]. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val get_block :
  ?runner:Runner.t ->
  block_id:string ->
  endpoint:string ->
  unit ->
  Block.t Lwt.t

(** [block_number ~endpoint ()] asks the current block number to the
    JSON-RPC API server listening at [endpoint]. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val block_number : ?runner:Runner.t -> endpoint:string -> unit -> int Lwt.t

(** [get_receipt ~endpoint ~tx ()] returns the [transaction_receipt] objects of a
    mined transaction if it exists. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val get_receipt :
  ?runner:Runner.t ->
  endpoint:string ->
  tx:string ->
  unit ->
  Transaction.transaction_receipt option Lwt.t

(** [encode_method ~abi_label ~method_ ()] returns the data corresponding to
    [method_] call considering the given [abi]. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val encode_method :
  ?runner:Runner.t -> abi_label:string -> method_:string -> unit -> string Lwt.t

val decode_method :
  ?runner:Runner.t -> abi_label:string -> method_:string -> unit -> string Lwt.t

(** [gen_eth_account ()] genarate a fresh eth account. 
    The [runner] optional argument can be specified to execute the command 
    on a remote runner. If omitted, the command is executed locally.*)
val gen_eth_account : ?runner:Runner.t -> unit -> Eth_account.t Lwt.t
