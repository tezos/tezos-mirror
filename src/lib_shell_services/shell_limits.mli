(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** The configurable constants used by shell components as maximum,
    with their encodings and default values. *)

(** States whether or not the operation metadata size must be caped
    and potentially discard if the given size limit is exceeded. *)
type operation_metadata_size_limit = Unlimited | Limited of int

val operation_metadata_size_limit_encoding :
  operation_metadata_size_limit Data_encoding.t

type block_validator_limits = {
  protocol_timeout : Time.System.Span.t;
  operation_metadata_size_limit : operation_metadata_size_limit;
}

val default_block_validator_limits : block_validator_limits

val block_validator_limits_encoding : block_validator_limits Data_encoding.t

(** This record contains the different limits and settings that can be updated
    from a node configuration for a prevalidator *)
type prevalidator_limits = {
  max_refused_operations : int;
      (** The maximum number of operations tracked by the mempool for each of
          the [refused], [branch delayed], [branch refused] and [outdated]
          operation classifications. Default is [1000] *)
  operation_timeout : Time.System.Span.t;
      (** The maximum time allowed to fetch the contents of an operation
          advertised by a remote peer. Default is [10] seconds *)
  operations_batch_size : int;
      (** Maximum number of pending operations processed (or classified) at the
          end of each request to the prevalidator worker. Default is [50] *)
}

(** Sane default values for {!limits} *)
val default_prevalidator_limits : prevalidator_limits

val prevalidator_limits_encoding : prevalidator_limits Data_encoding.t

type peer_validator_limits = {
  new_head_request_timeout : Time.System.Span.t;
  block_header_timeout : Time.System.Span.t;
  block_operations_timeout : Time.System.Span.t;
  protocol_timeout : Time.System.Span.t;
}

val default_peer_validator_limits : peer_validator_limits

val peer_validator_limits_encoding : peer_validator_limits Data_encoding.t

(** Constants parameterizing the bootstrap heuristics. *)
type synchronisation_limits = {
  latency : int;
      (** [latency] is the time interval (seconds) used to determine
          if a node is synchronized with a chain. For instance, a node that
          knows head with timestamp T is synchronized if T >= now -
          max_latency. This parameter depends on the baking rate and the
          latency of the network. *)
  threshold : int;
      (** [threshold] determines the number of peers the synchronization
     heuristic looks at to determine if the node is synchronized or
     not.  *)
}

type chain_validator_limits = {synchronisation : synchronisation_limits}

val default_chain_validator_limits : chain_validator_limits

val chain_validator_limits_encoding : chain_validator_limits Data_encoding.t

type limits = {
  block_validator_limits : block_validator_limits;
  prevalidator_limits : prevalidator_limits;
  peer_validator_limits : peer_validator_limits;
  chain_validator_limits : chain_validator_limits;
  history_mode : History_mode.t option;
}

val default_limits : limits

val limits_encoding : limits Data_encoding.t
