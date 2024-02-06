(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** [balance ~account ~endpoint] asks the balance of [account] to the
    JSON-RPC API server listening at [endpoint]. *)
val balance : account:string -> endpoint:string -> Wei.t Lwt.t

(** [transaction_send ~source_private_key ~to_public_key ~value
    ~endpoint] crafts and signs a transaction transferring [value] (as
    Wei) from [source_private_key] to [to_public_key], sends the raw
    transaction to the JSON-RPI API server listening at [endpoint]. *)
val transaction_send :
  source_private_key:string ->
  to_public_key:string ->
  value:Wei.t ->
  endpoint:string ->
  ?data:string ->
  unit ->
  string Lwt.t

(** [transaction_get ~endpoint ~tx_hash] returns the transaction object
    of [tx_hash] if it exists. *)
val transaction_get :
  endpoint:string ->
  tx_hash:string ->
  Transaction.transaction_object option Lwt.t

(** [add_abi ~label ~abi ()] register an ABI (Application Binary Interface) in
    the client. [abi] should be a path to the json file. [label] is a string
    used by the client to register the ABI in memory so it can be refered to
    later.*)
val add_abi : label:string -> abi:string -> unit -> unit Lwt.t

(** [deploy ~source_private_key ~endpoint ~abi ~bin] crafts and sign a
    transaction deploying [bin] whose interface [abi] is registered in the
    client, and sends the raw transaction to the JSON-API server listening at
    [endpoint]. [bin] is a path to the binary file, and [abi] is the label used
    while registering the ABI in the client.

    Returns a pair [(address, tx_hash)]. *)
val deploy :
  source_private_key:string ->
  endpoint:string ->
  abi:string ->
  bin:string ->
  (string * string) Lwt.t

(** [contract_send ~source_private_key ~endpoint ~abi_label ~address
    ~method_call ?value ?gas ()] makes a call to a contract found at
    [address], with interface registered as [abi_labbel] in the
    client, signed with a user's [source_private_key].  [method_call]
    is the call data, as a solidity expression. If [gas] is specified,
    it will not try to estimate it.

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
  ?expect_failure:bool ->
  source_private_key:string ->
  endpoint:string ->
  abi_label:string ->
  address:string ->
  method_call:string ->
  ?value:Wei.t ->
  ?gas:int ->
  unit ->
  string Lwt.t

(** [contract_call ~endpoint ~abi_label ~address ~method_call ()]
    makes a call to a contract found at [address], with interface registered as
    [abi_label] in the client, signed with a user's [source_private_key].
    [method_call] is the call data, as a solidity expression.

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
  ?expect_failure:bool ->
  endpoint:string ->
  abi_label:string ->
  address:string ->
  method_call:string ->
  unit ->
  string Lwt.t

(** [get_block ~block_id ~endpoint] asks the block [block_id] (it can be a
    hash or a number) to the JSON-RPC API server listening at [endpoint]. *)
val get_block : block_id:string -> endpoint:string -> Block.t Lwt.t

(** [block_number ~endpoint] asks the current block number to the
    JSON-RPC API server listening at [endpoint]. *)
val block_number : endpoint:string -> int Lwt.t

(** [get_receipt ~endpoint ~tx] returns the [transaction_receipt] objects of a
    mined transaction if it exists. *)
val get_receipt :
  endpoint:string -> tx:string -> Transaction.transaction_receipt option Lwt.t
