(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>                    *)
(* Copyright (c) Functori, <contact@functori.com>                            *)
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

(** This module allow to follow the layer 1 chain by subscribing to the head
    monitoring RPC offered by the Tezos node, reconnecting, etc. *)

type error +=
  | Cannot_find_predecessor of Block_hash.t
  | Http_connection_error of (Cohttp.Code.status_code * string)

(** The type of layer 1 followers. *)
type t

(** {2 Monitoring the Layer 1 chain} *)

(** [start ~name ~reconnection_delay ?protocols cctxt] connects to a Tezos node
    and starts monitoring new heads. One can iterate on the heads by calling
    {!iter_heads} on its result. [reconnection_delay] gives an initial delay for
    the reconnection which is used in an exponential backoff. The [name] is used
    to differentiate events. If [protocols] is provided, only heads of these
    protocols will be monitored. *)
val start :
  name:string ->
  chain:Tezos_shell_services.Chain_services.chain ->
  reconnection_delay:float ->
  ?protocols:Protocol_hash.t list ->
  Tezos_rpc.Context.generic ->
  t Lwt.t

(** [create ~name ~reconnection_delay ?protocols cctxt] creates a Layer 1
    context without connecting to a Tezos node. Use {!connect} to connect to
    start monitoring heads. If [protocols] is provided, only heads of these
    protocols will be monitored. *)
val create :
  name:string ->
  chain:Tezos_shell_services.Chain_services.chain ->
  reconnection_delay:float ->
  ?protocols:Protocol_hash.t list ->
  Tezos_rpc.Context.generic ->
  t

(** [shutdown t] properly shuts the layer 1 down. This function is to be used on
    exit. *)
val shutdown : t -> unit Lwt.t

(** [iter_heads ?name t f] calls [f] on all new heads appearing in the layer 1
    chain. In case of a disconnection with the layer 1 node, it reconnects
    automatically. If [f] returns an error (other than a disconnection),
    [iter_heads] terminates and returns the error. A [name] can be provided to
    differentiate iterations on the same connection. *)
val iter_heads :
  ?name:string ->
  t ->
  (Block_hash.t * Block_header.t -> unit tzresult Lwt.t) ->
  unit tzresult Lwt.t

(** [wait_first t] waits for the first head to appear in the stream and
    returns it. *)
val wait_first : t -> (Block_hash.t * Block_header.t) Lwt.t

(** [get_latest_head t] returns the latest L1 head if at least one was seen by
    [t]. The head is the one sent by the heads monitoring RPC of the L1 node,
    independently of how they were processed by the current process. *)
val get_latest_head : t -> (Block_hash.t * Block_header.t) option

(** [get_status t] returns the connection status to the L1 node. *)
val get_status : t -> [`Connected | `Disconnected | `Reconnecting]

(** {2 Helper functions for the Layer 1 chain} *)

(** [get_predecessor_opt ?max_read state head] returns the predecessor of block
    [head], when [head] is not the genesis block. [max_read] (by default [8]) is
    used to cache a number of predecessors (including [head]) to avoid some RPC
    calls when one need to access multiple predecessors. *)
val get_predecessor_opt :
  ?max_read:int ->
  t ->
  Block_hash.t * int32 ->
  (Block_hash.t * int32) option tzresult Lwt.t

(** [get_predecessor ?max_read state head] returns the predecessor block of [head]. *)
val get_predecessor :
  ?max_read:int ->
  t ->
  Block_hash.t * int32 ->
  (Block_hash.t * int32) tzresult Lwt.t

(** [nth_predecessor ~get_predecessor n head] returns [block, history] where
    [block] is the nth predecessor of [head] and [history] is the list of blocks
    between [block] (excluded) and [head] (included) on the chain. It uses the
    function [get_predecessor] to retrieve the predecessor of one block. *)
val nth_predecessor :
  get_predecessor:('a -> 'a tzresult Lwt.t) ->
  int ->
  'a ->
  ('a * 'a trace) tzresult Lwt.t

(** [get_tezos_reorg_for_new_head l1_ctxt ?get_old_predecessor old_head
    new_head] returns the reorganization of L1 blocks between [old_head] and
    [new_head]. If [old_head] is [`Level l], then it returns the reorganization
    between [new_head] and level [l] on the same chain. [get_old_predecessor]
    can be provided to use a different function (than by default
    {!get_predecessor}) to retrieve the predecessors of the old head. This is
    necessary when the old head is not in the L1 chain anymore and forgotten by
    the L1 node. *)
val get_tezos_reorg_for_new_head :
  t ->
  ?get_old_predecessor:
    (Block_hash.t * int32 -> (Block_hash.t * int32) tzresult Lwt.t) ->
  [`Head of Block_hash.t * int32 | `Level of int32] ->
  Block_hash.t * int32 ->
  (Block_hash.t * int32) Reorg.t tzresult Lwt.t

(** [client_context_with_timeout ctxt timeout] creates a client context where
    RPCs will be made with timeout [timeout] seconds. Calls that timeout will
    resolve with an error [RPC_timeout] which will trigger a reconnection in
    {!iter_heads}.  *)
val client_context_with_timeout :
  #Client_context.full -> float -> Client_context.full

(** Returns true iff a connection error is present in the given error trace. *)
val is_connection_error : error trace -> bool

(**/**)

module Internal_for_tests : sig
  (** Create a dummy Layer 1 object that does not follow any L1 chain. This
      function is only to be used as a placeholder for unit tests (that do not
      exercise the Layer 1 connection). *)
  val dummy : Tezos_rpc.Context.generic -> t
end
