(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Michelson type to use when originating the EVM rollup. *)
val evm_type : string

(** [u16_to_bytes n] translate an int in a binary string of two bytes
    (little endian).
    NB: Ints greater than 2 bytes are truncated. *)
val u16_to_bytes : int -> string

(** [add_0x s] will add the hexa prefix `0x` before the given string. *)
val add_0x : string -> string

(** [mapping_position key map_position] computes the storage position for
    a value in a mapping given its [key] and the position of the map
    itself.
    It computes this position as:
    [keccack(LeftPad32(key, 0), LeftPad32(map_position, 0))]
    as specified in
    https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_getlogs
*)
val mapping_position : string -> int -> string

(** Transform an hexadecimal string to an integer using {!Z.of_bits}. *)
val hex_string_to_int : string -> int

(** [hex_256_of_int n] returns the H256 of [n]. *)
val hex_256_of_int : int -> string

(** [next_rollup_node_level ~sc_rollup_node ~client] moves
    [sc_rollup_node] to the next level l1. *)
val next_rollup_node_level :
  sc_rollup_node:Sc_rollup_node.t -> client:Client.t -> int Lwt.t

(** [produce_block timestampt ~sc_rollup_node ~client] moves
    [evm_node] to the next L2 level. *)
val produce_block :
  ?wait_on_blueprint_applied:bool ->
  ?timestamp:string ->
  Evm_node.t ->
  (int, Rpc.error) result Lwt.t

(** [next_evm_level ~evm_node ~sc_rollup_node ~client] moves
    [evm_node] to the next L2 level. *)
val next_evm_level :
  evm_node:Evm_node.t ->
  sc_rollup_node:Sc_rollup_node.t ->
  client:Client.t ->
  unit Lwt.t

(** Path to the directory containing sample inputs. *)
val kernel_inputs_path : string

(** [read_tx_from_file ()] reads a file containing 100 transactions.
    The list returned contains pairs of the shape [(tx_raw, tx_hash)].
*)
val read_tx_from_file : unit -> (string * string) list

(** [force_kernel_upgrade ~sc_rollup_address ~sc_rollup_node ~node
    ~client] produces the force kernel upgrade and sends it via the
    client. [sc_rollup_address] is expected to be the b58 address. *)
val force_kernel_upgrade :
  sc_rollup_address:string ->
  sc_rollup_node:Sc_rollup_node.t ->
  client:Client.t ->
  unit Lwt.t

(** [upgrade ~sc_rollup_node ~sc_rollup_address ~admin ~admin_contract
    ~client ~upgrade_to ~activation_timestamp ] prepares the kernel
    upgrade payload and sends it to the layer 1. Returns the root hash
    of the kernel preimages. *)
val upgrade :
  sc_rollup_node:Sc_rollup_node.t ->
  sc_rollup_address:string ->
  admin:string ->
  admin_contract:string ->
  client:Client.t ->
  upgrade_to:Uses.t ->
  activation_timestamp:string ->
  string Lwt.t

(** [check_block_consistency ~left ~right ?error_msg ~blocks ()]
    checks that the block hash of [left] and [right] are equal. Fails
    if they are not with [error_msg] *)
val check_block_consistency :
  left:Evm_node.t ->
  right:Evm_node.t ->
  ?error_msg:string ->
  block:[< `Latest | `Level of int32 | `Finalized] ->
  unit ->
  unit Lwt.t

(** Checks latest block using {!check_block_consistency}. *)
val check_head_consistency :
  left:Evm_node.t -> right:Evm_node.t -> ?error_msg:string -> unit -> unit Lwt.t

(** [sequencer_upgrade ~sc_rollup_address ~sequencer_admin
    ~sequencer_admin_contract ~client ~upgrade_to
    ~activation_timestamp] prepares the sequencer upgrade payload and
    sends it to the layer 1. *)
val sequencer_upgrade :
  sc_rollup_address:string ->
  sequencer_admin:string ->
  sequencer_governance_contract:string ->
  client:Client.t ->
  upgrade_to:string ->
  pool_address:string ->
  activation_timestamp:string ->
  unit Lwt.t

(** [bake_until ?__LOC__ ?timeout_in_blocks ?timeout ~bake ~result_f ()] bakes
    using function [bake] until the function [result_f] returns a
    value or fails if:
    - it takes more than [timeout] sec, 30. by default.
    - it takes more than [timeout_in_blocks] blocks to bake, 5 by default.
*)
val bake_until :
  ?__LOC__:string ->
  ?timeout_in_blocks:int ->
  ?timeout:float ->
  bake:(unit -> 'a Lwt.t) ->
  result_f:(unit -> 'b option Lwt.t) ->
  unit ->
  'b Lwt.t

(** [bake_until_sync ?timeout_in_blocks ?timeout ~sc_rollup_node ~proxy ~sequencer
    ~client] bakes blocks until the rollup node is synced with
    evm_node. Uses {!bake_until} *)
val bake_until_sync :
  ?__LOC__:string ->
  ?timeout_in_blocks:int ->
  ?timeout:float ->
  sc_rollup_node:Sc_rollup_node.t ->
  proxy:Evm_node.t ->
  sequencer:Evm_node.t ->
  client:Client.t ->
  unit ->
  unit Lwt.t

(** [wait_for_transaction_receipt ?count ~evm_node ~transaction_hash ()] takes a
    transaction_hash and returns only when the receipt is non null, or [count]
    blocks have passed and the receipt is still not available. *)
val wait_for_transaction_receipt :
  ?count:int ->
  evm_node:Evm_node.t ->
  transaction_hash:string ->
  unit ->
  Transaction.transaction_receipt Lwt.t

(** [wait_for_application ~produce_block apply] returns only when the `apply`
    yields, or fails when 10 blocks (produced with [produce_block] have passed. *)
val wait_for_application :
  produce_block:(unit -> ('a, Rpc.error) result Lwt.t) ->
  (unit -> 'b Lwt.t) ->
  'b Lwt.t

(** [batch_n_transactions ~evm_node raw_transactions] batches [raw_transactions]
    to the [evm_node] and returns the requests and transaction hashes. *)
val batch_n_transactions :
  evm_node:Evm_node.t ->
  string list ->
  (Evm_node.request list * string list) Lwt.t

(** [send_n_transactions ~sc_rollup_node ~evm_node ?wait_for_blocks
    raw_transactions] batches [raw_transactions] to the [evm_node] and waits
    until the first one is applied in a block and returns, or fails if it isn't
    applied after [wait_for_blocks] blocks. *)
val send_n_transactions :
  produce_block:(unit -> ('a, Rpc.error) result Lwt.t) ->
  evm_node:Evm_node.t ->
  ?wait_for_blocks:int ->
  string list ->
  (Evm_node.request list * Transaction.transaction_receipt * string list) Lwt.t

val default_bootstrap_account_balance : Wei.t

(** Returns the timestamp of the L1 head block. *)
val l1_timestamp : Client.t -> Tezos_base.Time.Protocol.t Lwt.t

(** [find_and_execute_withdrawal ~withdrawal_level ~commitment_period ~challenge_window
    ~evm_node ~sc_rollup_node ~sc_rollup_address ~client] bakes enough levels to have
    a commitment and cement it, then constructs outbox proof
    and executes the outbox message *)
val find_and_execute_withdrawal :
  ?outbox_lookup_depth:int ->
  withdrawal_level:int ->
  commitment_period:int ->
  challenge_window:int ->
  evm_node:Evm_node.t ->
  sc_rollup_node:Sc_rollup_node.t ->
  sc_rollup_address:string ->
  client:Client.t ->
  unit ->
  int Lwt.t

(** Runs a sequencer in mode sandbox, with no connection needed to a
    rollup node. *)
val init_sequencer_sandbox :
  ?set_account_code:(string * string) list ->
  ?da_fee_per_byte:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?patch_config:(JSON.t -> JSON.t) ->
  ?kernel:Uses.t ->
  ?bootstrap_accounts:string list ->
  unit ->
  Evm_node.t Lwt.t

(** [send_transaction_to_sequencer send evm_node] sends the
    transaction using [send] and produces a block to include it. *)
val send_transaction_to_sequencer : (unit -> 'a Lwt.t) -> Evm_node.t -> 'a Lwt.t

(** [send_transactions_to_sequencer sends evm_node] sends the
    transactions using [sends] and produces a block to include them.
    Returns the number of transactions included in the block together
    with the results of [sends]. *)
val send_transactions_to_sequencer :
  sends:(unit -> 'a Lwt.t) list -> Evm_node.t -> (int * 'a list) Lwt.t
