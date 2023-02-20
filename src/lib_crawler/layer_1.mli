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

(** The type of layer 1 followers. *)
type t

(** {2 Monitoring the Layer 1 chain} *)

(** [start ~name ~reconnection_delay cctxt] connects to a Tezos node and starts
    monitoring new heads. One can iterate on the heads by calling {!iter_heads}
    on its result. [reconnection_delay] gives an initial delay for the
    reconnection which is used in an exponential backoff. The [name] is used to
    differentiate events. *)
val start :
  name:string ->
  reconnection_delay:float ->
  #Client_context.full ->
  t tzresult Lwt.t

(** [shutdown t] properly shuts the layer 1 down. *)
val shutdown : t -> unit Lwt.t

(** [iter_heads t f] calls [f] on all new heads appearing in the layer 1
    chain. In case of a disconnection with the layer 1 node, it reconnects
    automatically. If [f] returns an error (other than a disconnection),
    [iter_heads] terminates and returns the error.  *)
val iter_heads :
  t ->
  (Block_hash.t * Block_header.t -> unit tzresult Lwt.t) ->
  unit tzresult Lwt.t

(** {2 Helper functions for the Layer 1 chain} *)

(** [get_predecessor_opt state head] returns the predecessor of block [head],
    when [head] is not the genesis block. *)
val get_predecessor_opt :
  t -> Block_hash.t * int32 -> (Block_hash.t * int32) option tzresult Lwt.t

(** [get_predecessor state head] returns the predecessor block of [head]. *)
val get_predecessor :
  t -> Block_hash.t * int32 -> (Block_hash.t * int32) tzresult Lwt.t

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
