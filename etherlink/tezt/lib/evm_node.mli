(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

(** EVM node server state. *)
type t

type time_between_blocks =
  | Nothing  (** Does not produce any block if not forced by the private RPC *)
  | Time_between_blocks of float
      (** Interval at which the sequencer creates an empty block by
          default. *)

(** EVM node mode. *)
type mode =
  | Observer of {initial_kernel : string; preimage_dir : string}
  | Sequencer of {
      initial_kernel : string;
          (** Path to the initial kernel used by the sequencer. *)
      preimage_dir : string;
          (** Path to the directory with the associated preimages. *)
      private_rpc_port : int;  (** Port for private RPC server*)
      time_between_blocks : time_between_blocks option;
          (** See {!time_between_blocks}, if the value is not
              provided, the sequencer uses it default value. *)
      sequencer : string;  (** Secret key used to sign the blueprints. *)
      genesis_timestamp : Client.timestamp option;  (** Genesis timestamp *)
      max_blueprints_lag : int option;
      max_blueprints_catchup : int option;
      catchup_cooldown : int option;
      devmode : bool;  (** --devmode flag. *)
      wallet_dir : string option;  (** --wallet-dir: client directory. *)
    }
  | Proxy of {devmode : bool  (** --devmode flag. *)}

(** Returns the mode of the EVM node. *)
val mode : t -> mode

(** [create ?runner ?mode ?data_dir ?rpc_addr ?rpc_port
    rollup_node_endpoint] creates an EVM node server.

    The server listens to requests at address [rpc_addr] and the port
    [rpc_port]. [rpc_addr] defaults to [Constant.default_host] and a fresh port is
    chosen if [rpc_port] is not set.

    The server communicates with a rollup-node and sets its endpoint via
    [rollup_node_endpoint].

    [mode] defaults to [Proxy].
*)
val create :
  ?runner:Runner.t ->
  ?mode:mode ->
  ?data_dir:string ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  string ->
  t

(** [initial_kernel node] returns the path to the kernel used to initialize the
    EVM state. Fails if [node] is a proxy node. *)
val initial_kernel : t -> string

(** [run ?wait ?extra_arguments evm_node] launches the EVM node server with
    the arguments given during {!create}, additional arguments can be
    passed via [extra_arguments].
    [wait] defaults to true, if it is set to false, the evm node is ran but we
    do not wait for it to be ready. *)
val run : ?wait:bool -> ?extra_arguments:string list -> t -> unit Lwt.t

(** [wait_for_ready evm_node] waits until [evm_node] is ready. *)
val wait_for_ready : t -> unit Lwt.t

(** [wait_for_blueprint_applied ~timeout evm_node level] waits until
    [evm_node] has applied a blueprint locally for level [level]. *)
val wait_for_blueprint_applied : timeout:float -> t -> int -> unit Lwt.t

(** [wait_for_blueprint_injected ~timeout evm_node level] waits until
    [evm_node] has injected a blueprint for level [level] to its rollup node. *)
val wait_for_blueprint_injected : timeout:float -> t -> int -> unit Lwt.t

(** [init ?runner ?mode ?data_dir ?rpc_addr ?rpc_port rollup_node_endpoint]
    creates an EVM node server with {!create} and runs it with {!run}. *)
val init :
  ?runner:Runner.t ->
  ?mode:mode ->
  ?data_dir:string ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  string ->
  t Lwt.t

(** [spawn_run ?extra_arguments evm_node] same as {!run} but spawns a
    process. *)
val spawn_run : ?extra_arguments:string list -> t -> Process.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** The same exact behavior as {!Sc_rollup_node.wait_for} but for the EVM node. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** [endpoint ?private_ evm_node] returns the endpoint to communicate with the
    [evm_node]. If [private_] is true, the endpoint for the private
    RPC server is returned. *)
val endpoint : ?private_:bool -> t -> string

(** JSON-RPC request. *)
type request = {method_ : string; parameters : JSON.u}

(** [call_evm_rpc ?private_ evm_node ~request] sends a JSON-RPC request to
    the [evm_node], for the given [request].
    If [private_] is true, the request is sent to the private RPC
    server. *)
val call_evm_rpc : ?private_:bool -> t -> request -> JSON.t Lwt.t

(** [batch_evm_rpc ?private_ evm_node ~requests] sends multiple JSON-RPC requests
    to the [evm_node], for the given [requests].
    If [private_] is true, the requests are sent to the private RPC
    server. *)
val batch_evm_rpc : ?private_:bool -> t -> request list -> JSON.t Lwt.t

(** [extract_result json] expects a JSON-RPC `result` and returns the value. *)
val extract_result : JSON.t -> JSON.t

(** [extract_error_message json] expects a JSON-RPC `error.message` and returns the value. *)
val extract_error_message : JSON.t -> JSON.t

(** [fetch_contract_code evm_node contract] returns the code associated to
    the given contract in the rollup. *)
val fetch_contract_code : t -> string -> string Lwt.t

(** A slot in the transaction pool associates an address to a mapping of nonces
    to transactions. *)
type txpool_slot = {address : string; transactions : (int64 * JSON.t) list}

(** [txpool_content evm_node] returns the transaction hash and nonce
    contained in the `pending` and `queued` pools. *)
val txpool_content : t -> (txpool_slot list * txpool_slot list) Lwt.t

(** [upgrade_payload ~root_hash ~activation_timestamp] gives the
    upgrade payload to put in a upgrade message, it will upgrade to
    [root_hash] at the first block after [activation_timestamp] (in
    RFC3399 format). *)
val upgrade_payload :
  root_hash:string -> activation_timestamp:string -> string Lwt.t

(** [init_from_rollup_node_data_dir evm_node rollup_node] initialises
    the data dir of the evm node by importing the evm state from a
    rollup node data dir. *)
val init_from_rollup_node_data_dir : t -> Sc_rollup_node.t -> unit Lwt.t

(** [transform_dump ~dump_json ~dump_rlp] transforms a JSON list of
    instructions stored in [dump_json] to an RLP list, which is
    stored in [dump_rlp].  *)
val transform_dump : dump_json:string -> dump_rlp:string -> unit Lwt.t
