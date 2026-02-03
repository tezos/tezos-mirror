(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type error = {code : int; message : string; data : string option}

type block_param =
  | Earliest
  | Latest
  | Pending
  | Number of int  (** Put directly the value. *)
  | Block_number of {number : int; require_canonical : bool}
      (** Instead of [Number], it uses a JSON object. *)
  | Block_hash of {hash : string; require_canonical : bool}

module Request : sig
  val eth_blockNumber : Evm_node.request

  val eth_getBlockByNumber :
    block:string -> full_tx_objects:bool -> Evm_node.request

  val eth_getBlockByHash :
    block:string -> full_tx_objects:bool -> Evm_node.request

  val produceBlock :
    ?with_delayed_transactions:bool ->
    ?timestamp:string ->
    unit ->
    Evm_node.request

  val proposeNextBlockTimestamp : timestamp:string -> Evm_node.request

  val produceProposal : ?timestamp:string -> unit -> Evm_node.request

  val eth_sendRawTransaction : raw_tx:string -> Evm_node.request

  val eth_sendRawTransactionSync :
    raw_tx:string ->
    ?timeout:string ->
    block:block_param ->
    unit ->
    Evm_node.request

  val eth_getTransactionReceipt : tx_hash:string -> Evm_node.request

  val eth_estimateGas :
    eth_call:(string * Ezjsonm.value) list ->
    block:block_param ->
    Evm_node.request

  val eth_getTransactionCount :
    address:string -> block:string -> Evm_node.request

  val eth_getTransactionByHash : transaction_hash:string -> Evm_node.request

  val eth_getCode : address:string -> block:block_param -> Evm_node.request

  val eth_getChainId : Evm_node.request

  val net_version : Evm_node.request

  val eth_maxPriorityFeePerGas : Evm_node.request

  val txpool_content : Evm_node.request

  val eth_feeHistory :
    block_count:string -> newest_block:string -> Evm_node.request

  val coinbase : Evm_node.request

  type address = Single of string | Multi of string list

  type logs_input_param = {
    address : address option;
    topics : string list option;
  }

  type subscription_kind =
    | NewHeads
    | Logs of logs_input_param option
    | NewPendingTransactions
    | Syncing
    | NewIncludedTransactions
    | NewPreconfirmedReceipts

  val eth_subscribe : kind:subscription_kind -> Evm_node.request

  val eth_unsubscribe : id:string -> Evm_node.request
end

(** {2 RPC calls wrappers}


    Calls below are made using an HTTP request to the EVM node by default unless
    a websocket is provided, in which case the communication happens on it.
*)

(** [net_version evm_node] calls [net_version]. *)
val net_version :
  ?websocket:Websocket.t -> Evm_node.t -> (string, error) result Lwt.t

(** [get_chain_id evm_node] calls [eth_getChainId]. *)
val get_chain_id :
  ?websocket:Websocket.t -> Evm_node.t -> (int, error) result Lwt.t

(** [get_chain_family node] calls [tez_getChainFamily]. *)
val get_chain_family :
  ?websocket:Websocket.t -> Evm_node.t -> int -> (string, error) result Lwt.t

(** [get_transaction_by_hash ~transaction_hash evm_node] calls [eth_getTransactionByHash]. *)
val get_transaction_by_hash :
  ?websocket:Websocket.t ->
  transaction_hash:string ->
  Evm_node.t ->
  (Transaction.transaction_object option, error) result Lwt.t

(** [get_code ~address evm_node] calls [eth_getCode]. *)
val get_code :
  ?websocket:Websocket.t ->
  address:string ->
  Evm_node.t ->
  (string, error) result Lwt.t

(** [get_logs evm_node] calls [eth_getLogs]. *)
val get_logs :
  ?websocket:Websocket.t ->
  ?from_block:block_param ->
  ?to_block:block_param ->
  ?address:Request.address ->
  ?topics:string list list ->
  ?block_hash:string ->
  Evm_node.t ->
  (Transaction.tx_log list, error) result Lwt.t

(** [block_number evm_node] calls [eth_blockNumber]. *)
val block_number :
  ?websocket:Websocket.t -> Evm_node.t -> (int32, error) result Lwt.t

(** [generic_block_number evm_node] calls [tez_blockNumber]. Works for Tezlink/Etherlink. *)
val generic_block_number :
  ?websocket:Websocket.t -> Evm_node.t -> (int32, error) result Lwt.t

(** [generic_block_number evm_node] calls [tez_blockNumber]. Works for Tezlink/Etherlink. *)
val generic_block_number_opt :
  ?websocket:Websocket.t -> Evm_node.t -> (int32 option, error) result Lwt.t

(** [block_number_opt evm_node] calls [eth_blockNumber]. allows None
    when no block have been produced yet.  *)
val block_number_opt :
  ?websocket:Websocket.t -> Evm_node.t -> (int32 option, error) result Lwt.t

(** [get_block_by_number ?full_tx_objets ~block evm_node] calls
    [eth_getBlockByNumber]. [full_tx_objects] is false by default, so
    the block contains the transaction hashes. [block] can be
    ["latest"] or its number. *)
val get_block_by_number :
  ?websocket:Websocket.t ->
  ?full_tx_objects:bool ->
  block:string ->
  Evm_node.t ->
  (Block.t, error) result Lwt.t

(** Same as {!get_block_by_number} but uses the hash instead of the number. *)
val get_block_by_hash :
  ?websocket:Websocket.t ->
  ?full_tx_objects:bool ->
  block:string ->
  Evm_node.t ->
  (Block.t, error) result Lwt.t

val get_gas_price : ?websocket:Websocket.t -> Evm_node.t -> Int32.t Lwt.t

val subscribe :
  ?websocket:Websocket.t ->
  kind:Request.subscription_kind ->
  Evm_node.t ->
  string Lwt.t

val unsubscribe :
  ?websocket:Websocket.t -> id:string -> Evm_node.t -> bool Lwt.t

module Syntax : sig
  val ( let*@ ) : ('a, error) result Lwt.t -> ('a -> 'c Lwt.t) -> 'c Lwt.t

  val ( let*@? ) : ('a, error) result Lwt.t -> (error -> 'c Lwt.t) -> 'c Lwt.t

  val ( let*@! ) :
    ('a option, error) result Lwt.t -> ('a -> 'c Lwt.t) -> 'c Lwt.t
end

(** [produce_block ?with_delayed_transactions ?timestamp evm_node]
    calls the private RPC [produceBlock]. If provided the block will
    have timestamp [timestamp] (in RFC3339) format. *)
val produce_block :
  ?websocket:Websocket.t ->
  ?with_delayed_transactions:bool ->
  ?timestamp:string ->
  Evm_node.t ->
  (int, error) result Lwt.t

(** [propose_next_block_timestamp ~timestamp evm_node] calls the
    private RPC [proposeNextBlockTimestamp]. If provided the next
    produced block might have timestamp [timestamp] (in RFC3339)
    format. *)
val propose_next_block_timestamp :
  ?websocket:Websocket.t ->
  timestamp:string ->
  Evm_node.t ->
  (unit, error) result Lwt.t

(** [produce_proposal ?timestamp evm_node] calls the private RPC [produceProposal].
    If provided the block will have timestamp [timestamp] (in RFC3339) format. *)
val produce_proposal :
  ?websocket:Websocket.t ->
  ?timestamp:string ->
  Evm_node.t ->
  (unit, error) result Lwt.t

(** [state_value ?websocket evm_node ?block path] calls the private
    RPC [stateValue]. *)
val state_value :
  ?websocket:Websocket.t ->
  Evm_node.t ->
  ?block:string ->
  string ->
  (string option, error) result Lwt.t

(** [state_subkeys ?websocket evm_node ?block path] calls the private
    RPC [stateSubkeys]. *)
val state_subkeys :
  ?websocket:Websocket.t ->
  Evm_node.t ->
  ?block:string ->
  string ->
  (string list option, error) result Lwt.t

(** [send_raw_transaction ~raw_tx evm_node] calls [eth_sendRawTransaction]
    with [raw_tx] as argument. *)
val send_raw_transaction :
  ?websocket:Websocket.t ->
  raw_tx:string ->
  Evm_node.t ->
  (string, error) result Lwt.t

(** [eth_send_raw_transaction_sync ~raw_tx evm_node] calls
    [eth_sendRawTransactionSync] with [raw_tx] as argument. *)
val eth_send_raw_transaction_sync :
  ?websocket:Websocket.t ->
  raw_tx:string ->
  ?timeout:int ->
  ?block:block_param ->
  Evm_node.t ->
  (Transaction.transaction_receipt, error) result Lwt.t

(** [get_transaction_receipt ~tx_hash evm_node] calls
    [eth_getTransactionReceipt] with [tx_hash] as argument. *)
val get_transaction_receipt :
  ?websocket:Websocket.t ->
  tx_hash:string ->
  Evm_node.t ->
  (Transaction.transaction_receipt option, error) result Lwt.t

(** [get_transaction_gas_info evm_node ~tx_hash] calls
    [tez_getTransactionGasInfo]. Returns the execution gas and the
    inclusion gas corresponding to [tx_hash] if a transaction receipt
    is found. *)
val get_transaction_gas_info :
  ?websocket:Websocket.t ->
  tx_hash:string ->
  Evm_node.t ->
  (* todo: use labeled tuples once we use OCaml 5.4 *)
  (([`Inclusion_gas of int64] * [`Execution_gas of int64]) option, error) result
  Lwt.t

(** [estimate_gas eth_call evm_node] calls [eth_estimateGas] with [eth_call]
    as payload. *)
val estimate_gas :
  ?websocket:Websocket.t ->
  (string * Ezjsonm.value) list ->
  ?block:block_param ->
  Evm_node.t ->
  (int64, error) result Lwt.t

(** [get_transaction_count ?block ~address evm_node] calls [eth_getTransactionCount]
    with [address] as argument (on [block], default to ["latest"] if omitted). *)
val get_transaction_count :
  ?websocket:Websocket.t ->
  ?block:string ->
  address:string ->
  Evm_node.t ->
  (int64, error) result Lwt.t

(** [tez_kernelVersion evm_node] calls [tez_kernelVersion]. Returns the
    kernel commit hash. *)
val tez_kernelVersion :
  ?websocket:Websocket.t -> Evm_node.t -> (string, error) result Lwt.t

(** [tez_kernelRootHash evm_node] calls [tez_kernelRootHash]. Returns the
    kernel root hash. *)
val tez_kernelRootHash :
  ?websocket:Websocket.t -> Evm_node.t -> (string option, error) result Lwt.t

(** [call ~to_ ~data ?block evm_node] call [eth_call] with [to] and
    [data] as argument. [block] defaults to [Latest]. *)
val call :
  ?websocket:Websocket.t ->
  to_:string ->
  data:string ->
  ?block:block_param ->
  Evm_node.t ->
  (string, error) result Lwt.t

(** [get_balance ~address ?block evm_node] calls [eth_getBalance]. [block]
    defaults to [Latest].*)
val get_balance :
  ?websocket:Websocket.t ->
  address:string ->
  ?block:block_param ->
  Evm_node.t ->
  (Wei.t, error) result Lwt.t

(** [get_balance ~address ?block ~pos evm_node] calls [eth_getStorageAt]. [block]
    defaults to [Latest]. *)
val get_storage_at :
  ?websocket:Websocket.t ->
  address:string ->
  ?block:block_param ->
  pos:string ->
  Evm_node.t ->
  (string, error) result Lwt.t

val get_max_priority_fee_per_gas :
  ?websocket:Websocket.t -> Evm_node.t -> Int32.t Lwt.t

(** [replay_block number evm_node] replays the block [number] and returns its
    representation. *)
val replay_block :
  ?websocket:Websocket.t -> int -> Evm_node.t -> (Block.t, error) result Lwt.t

(** A slot in the transaction pool associates an address to a mapping of nonces
    to transactions. *)
type txpool_slot = {address : string; transactions : (int64 * JSON.t) list}

(** [txpool_content evm_node] returns the transaction hash and nonce
    contained in the `pending` and `queued` pools. *)
val txpool_content :
  ?websocket:Websocket.t ->
  Evm_node.t ->
  (txpool_slot list * txpool_slot list, error) result Lwt.t

(** [trace_transaction ~transaction_hash evm_node] replays the given transaction
    in the same execution context. Doesn't return the trace for now. *)
val trace_transaction :
  ?websocket:Websocket.t ->
  transaction_hash:string ->
  ?tracer:string ->
  ?tracer_config:(string * JSON.u) list ->
  Evm_node.t ->
  (JSON.t, error) result Lwt.t

val trace_call :
  ?websocket:Websocket.t ->
  block:block_param ->
  to_:string ->
  data:string ->
  ?tracer:string ->
  ?tracer_config:(string * JSON.u) list ->
  Evm_node.t ->
  (JSON.t, error) result Lwt.t

val trace_block :
  ?websocket:Websocket.t ->
  block:block_param ->
  ?tracer:string ->
  ?tracer_config:(string * JSON.u) list ->
  Evm_node.t ->
  (JSON.t list, error) result Lwt.t

type fee_history = {
  oldest_block : int64;
  base_fee_per_gas : int64 list;
  gas_used_ratio : float list;
}

(** [fee_history block_count newest_block evm_node] calls [eth_feeHistory]. *)
val fee_history :
  ?websocket:Websocket.t ->
  string ->
  string ->
  Evm_node.t ->
  (fee_history, error) result Lwt.t

(** [coinbase] calls [eth_coinbase]. *)
val coinbase :
  ?websocket:Websocket.t -> Evm_node.t -> (string, error) result Lwt.t

(** Returns the EVM node configuration. *)
val configuration : Evm_node.t -> JSON.t Lwt.t

(** Returns collected Prometheus metrics with /metrics RPC. *)
val metrics : Evm_node.t -> string Lwt.t

module Tezosx : sig
  val tez_getTezosEthereumAddress :
    ?websocket:Websocket.t ->
    string ->
    Evm_node.t ->
    (string, error) result Lwt.t

  val tez_getEthereumTezosAddress :
    ?websocket:Websocket.t ->
    string ->
    Evm_node.t ->
    (string, error) result Lwt.t
end
