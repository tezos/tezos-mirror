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

(** EVM node mode. *)
type mode =
  | Sequencer of {
      kernel : string;  (** Path to the kernel used by the sequencer. *)
      preimage_dir : string;
          (** Path to the directory with the associated preimages. *)
    }
  | Proxy of {devmode : bool  (** --devmode flag. *)}

(** Returns the mode of the EVM node. *)
val mode : t -> mode

(** [create ?runner ?mode ?data_dir ?rpc_addr ?rpc_port
    rollup_node_endpoint] creates an EVM node server.

    The server listens to requests at address [rpc_addr] and the port
    [rpc_port]. [rpc_addr] defaults to ["127.0.0.1"] and a fresh port is
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

(** [run evm_node] launches the EVM node server with the arguments
    given during {!create}. *)
val run : t -> unit Lwt.t

(** [init ?runner ?mode ?data_dir ?rpc_addr ?rpc_port
    rollup_node_endpoint] creates an EVM node server with {!create}
    and runs it with {!run}. *)
val init :
  ?runner:Runner.t ->
  ?mode:mode ->
  ?data_dir:string ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  string ->
  t Lwt.t

(** [spawn_run evm_node] same as {!run} but spawns a process. *)
val spawn_run : t -> Process.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** [endpoint evm_node] returns the endpoint to communicate with the
    [evm_node]. *)
val endpoint : t -> string

(** JSON-RPC request. *)
type request = {method_ : string; parameters : JSON.u}

(** [call_evm_rpc evm_node ~request] sends a JSON-RPC request to the
    [evm_node], for the given [request]. *)
val call_evm_rpc : t -> request -> JSON.t Lwt.t

(** [batch_evm_rpc evm_node ~requests] sends multiple JSON-RPC requests to the
    [evm_node], for the given [requests]. *)
val batch_evm_rpc : t -> request list -> JSON.t Lwt.t

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
