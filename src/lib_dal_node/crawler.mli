(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** This module maintains information about the layer 1 chain.

   This module follows the evolution of the layer 1 chain by
   subscribing to the head monitoring RPC offered by the Tezos node.
*)

type t

(** [start ~name ~chain ~reconnection_delay ~l1_blocks_cache_size
    ~last_notified_level cctxt] connects to a Tezos node and starts monitoring
    new heads. [reconnection_delay] gives an initial delay for the reconnection
    which is used in an exponential backoff. The [name] is used to differentiate
    events. [l1_blocks_cache_size] is the size of the block headers cache.

    The [last_notified_level] parameter indicates the level of the last notified
    finalized block, to start catching-up starting from that level. The function
    enforces the value to be at least equal to 0. If no level is given, the
    function starts catching-up from the current level - 2.*)
val start :
  name:string ->
  chain:Shell_services.chain ->
  reconnection_delay:float ->
  l1_blocks_cache_size:int ->
  ?last_notified_level:int32 ->
  Tezos_rpc.Context.generic ->
  t Lwt.t

(** [finalized_heads_stream t] returns a clone of the current stream containing
    finalized heads. *)
val finalized_heads_stream :
  t -> (Block_hash.t * Block_header.shell_header) Lwt_stream.t

(** [shutdown t] shuts down the stream of finalized heads and cancels the monad
    instance of {!iter_heads} that populates the stream. *)
val shutdown : t -> unit

(** [last_seen_head t] returns the information of the last block that the L1
    crawler saw. *)
val last_seen_head : t -> (Block_hash.t * Block_header.shell_header) option
