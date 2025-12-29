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

(** [u16_to_bytes n] translate an int in a binary string of two bytes
    (little endian).
    NB: Ints greater than 2 bytes are truncated. *)
val u16_to_bytes : int -> string

(** [add_0x s] will add the hexa prefix `0x` before the given string. *)
val add_0x : string -> string

(** [remove_0x s] removes the [0x] prefix in [s] if it is present. *)
val remove_0x : string -> string

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

(** [hex_256_of_address address] returns the H256 of [address]. *)
val hex_256_of_address : Eth_account.t -> string

val genesis_time : Ptime.t

val genesis_timestamp : Client.timestamp

val days : int -> Ptime.span

val get_timestamp : int -> string

(** [produce_block timestampt ~sc_rollup_node ~client] moves
    [evm_node] to the next L2 level. *)
val produce_block :
  ?timestamp:string -> Evm_node.t -> (int, Rpc.error) result Lwt.t

(** [check_header ~previous_header ~current_header] checks that two
    consecutive headers are consistent. *)
val check_header :
  previous_header:JSON.t ->
  current_header:JSON.t ->
  chain_id:int option ->
  current_timestamp:string option ->
  unit

(** [check_block_info
      ~previous_block_info
      ~current_block_info
      ~chain_id
      ~current_timestamp
      ~expected_operations] checks that two
    consecutive block's info are consistent. *)
val check_block_info :
  previous_block_info:JSON.t ->
  current_block_info:JSON.t ->
  chain_id:int option ->
  current_timestamp:string option ->
  expected_operations:string list list ->
  unit

(** [next_evm_level ~evm_node ~sc_rollup_node ~client] moves
    [evm_node] to the next L2 level. *)
val next_evm_level :
  evm_node:Evm_node.t ->
  sc_rollup_node:Sc_rollup_node.t ->
  client:Client.t ->
  unit Lwt.t

(** [check_chain_id ~expected_chain_id ~chain_id] checks that
    the value received for chain_id is correct using the appropriate
    type conversions. *)
val check_chain_id : expected_chain_id:int -> chain_id:string -> unit

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

type network = Etherlink | Tezlink

(** [rollup_level sc_rollup_node] returns the current level of the
    rollup node, or [None] if it has not processed any level yet. *)
val rollup_level :
  ?network:network -> Sc_rollup_node.t -> (int32, Rpc.error) result Lwt.t

(** [check_rollup_head_consistency ~evm_node ~sc_rollup_node ?error_msg ()]
    checks that the latest block of [evm_node] and the block at the level
    of the rollup node are equal. Fails if they are not with [error_msg] *)
val check_rollup_head_consistency :
  evm_node:Evm_node.t ->
  sc_rollup_node:Sc_rollup_node.t ->
  ?error_msg:string ->
  unit ->
  unit Lwt.t

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

(** [bake_until_sync ?timeout_in_blocks ?timeout ~sc_rollup_node ~sequencer
    ~client] bakes blocks until the rollup node is synced with
    evm_node. Uses {!bake_until} *)
val bake_until_sync :
  ?__LOC__:string ->
  ?timeout_in_blocks:int ->
  ?timeout:float ->
  ?network:network ->
  sc_rollup_node:Sc_rollup_node.t ->
  sequencer:Evm_node.t ->
  client:Client.t ->
  unit ->
  unit Lwt.t

(** [wait_for_transaction_receipt ?count ~evm_node ~transaction_hash ()] takes a
    transaction_hash and returns only when the receipt is non null, or [count]
    blocks have passed and the receipt is still not available. *)
val wait_for_transaction_receipt :
  ?websocket:Websocket.t ->
  ?count:int ->
  evm_node:Evm_node.t ->
  transaction_hash:string ->
  unit ->
  Transaction.transaction_receipt Lwt.t

(** [wait_for_application ?time_between_blocks ~produce_block apply] returns
    only when the `apply` yields, or fails when [max_blocks] blocks (10 by
    default, produced with [produce_block], every [time_between_blocks] -- 5s by
    default, have passed. *)
val wait_for_application :
  ?time_between_blocks:float ->
  ?max_blocks:int ->
  produce_block:(unit -> ('a, Rpc.error) result Lwt.t) ->
  (unit -> 'b Lwt.t) ->
  'b Lwt.t

val wait_for_event :
  ?timeout:float ->
  ?levels:int ->
  'a Lwt.t ->
  sequencer:Evm_node.t ->
  sc_rollup_node:Sc_rollup_node.t ->
  client:Client.t ->
  error_msg:(unit Lwt.t, Format.formatter, unit, 'b) format4 ->
  'a Lwt.t

(** [batch_n_transactions ~evm_node raw_transactions] batches [raw_transactions]
    to the [evm_node] and returns the requests and transaction hashes. *)
val batch_n_transactions :
  ?websocket:Websocket.t ->
  evm_node:Evm_node.t ->
  string list ->
  (Evm_node.request list * string list) Lwt.t

(** [send_n_transactions ~sc_rollup_node ~evm_node ?wait_for_blocks
    raw_transactions] batches [raw_transactions] to the [evm_node] and waits
    until the first one is applied in a block and returns, or fails if it isn't
    applied after [wait_for_blocks] blocks. *)
val send_n_transactions :
  ?websocket:Websocket.t ->
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
    rollup node. Will setup bootstrap accounts by default, for activated
    runtimes (EVM is always active). *)
val init_sequencer_sandbox :
  ?maximum_gas_per_transaction:int64 ->
  ?genesis_timestamp:Client.timestamp ->
  ?tx_queue_max_lifespan:int ->
  ?tx_queue_max_size:int ->
  ?tx_queue_tx_per_addr_limit:int ->
  ?set_account_code:(string * string) list ->
  ?da_fee_per_byte:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?history_mode:Evm_node.history_mode ->
  ?patch_config:(JSON.t -> JSON.t) ->
  ?websockets:bool ->
  ?kernel:Uses.t ->
  ?evm_version:Evm_version.t ->
  ?eth_bootstrap_accounts:string list ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?sequencer_keys:Account.key list ->
  ?with_runtimes:Tezosx_runtime.t list ->
  unit ->
  Evm_node.t Lwt.t

(** [send_transaction_to_sequencer send evm_node] sends the
    transaction using [send] and produces a block to include it. *)
val send_transaction_to_sequencer :
  ?timestamp:string -> (unit -> 'a Lwt.t) -> Evm_node.t -> 'a Lwt.t

(** [send_transactions_to_sequencer sends evm_node] sends the
    transactions using [sends] and produces a block to include them.
    Returns the number of transactions included in the block together
    with the results of [sends]. *)
val send_transactions_to_sequencer :
  sends:(unit -> 'a Lwt.t) list -> Evm_node.t -> (int * 'a list) Lwt.t

(** Run [octez-client transfer amount from giver to bridge --entrypoint deposit --arg '(Pair "sr_address" l2_address)']. *)
val deposit :
  ?env:string String_map.t ->
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  ?endpoint:Client.endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?safety_guard:int ->
  ?storage_limit:int ->
  ?counter:int ->
  ?simulation:bool ->
  ?force:bool ->
  ?expect_failure:bool ->
  amount:Tez.t ->
  giver:string ->
  sr_address:string ->
  bridge:string ->
  l2_address:string ->
  Client.t ->
  unit Lwt.t

(** [check_operations ~client ~block ~expected] fetches the list of hashes of
    the manager operations of [block] and check it's equal to [expected]. *)
val check_operations :
  client:Client.t -> block:string -> expected:string list -> unit Lwt.t

(** [produce_block_and_wait_for ~sequencer n] Produces a block and wait for
    blueprint [n] to be applied. *)
val produce_block_and_wait_for : sequencer:Evm_node.t -> int -> unit Lwt.t

val register_sandbox :
  __FILE__:string ->
  ?uses_client:bool ->
  ?kernel:Kernel.t ->
  ?tx_queue_tx_per_addr_limit:int ->
  title:string ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?set_account_code:(string * string) list ->
  ?da_fee_per_byte:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  tags:string list ->
  ?patch_config:(JSON.t -> JSON.t) ->
  ?websockets:bool ->
  ?sequencer_keys:Account.key list ->
  ?with_runtimes:Tezosx_runtime.t list ->
  (Evm_node.t -> unit Lwt.t) ->
  unit

type sandbox_test = {sandbox : Evm_node.t; observer : Evm_node.t}

val register_sandbox_with_observer :
  __FILE__:string ->
  ?kernel:Kernel.t ->
  ?tx_queue_tx_per_addr_limit:int ->
  title:string ->
  ?set_account_code:(string * string) list ->
  ?da_fee_per_byte:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  tags:string list ->
  ?patch_config:(JSON.t -> JSON.t) ->
  ?websockets:bool ->
  ?sequencer_keys:Account.key list ->
  (sandbox_test -> unit Lwt.t) ->
  unit
