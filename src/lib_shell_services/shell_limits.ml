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

let timeout_encoding = Time.System.Span.encoding

type operation_metadata_size_limit = Unlimited | Limited of int

let operation_metadata_size_limit_encoding =
  let open Data_encoding in
  def
    "operation_metadata_size_limit"
    ~title:"operation_metadata_size_limit"
    ~description:"The operation metadata size limit"
    (union
       ~tag_size:`Uint8
       [
         case
           ~title:"unlimited"
           ~description:"The metadata size is unlimited."
           (Tag 0)
           (constant "unlimited")
           (function Unlimited -> Some () | _ -> None)
           (fun () -> Unlimited);
         case
           ~title:"limited"
           ~description:
             "The metadata size is limited to the given integer's value (in \
              bytes)."
           (Tag 1)
           int31
           (function Limited i -> Some i | _ -> None)
           (fun i -> Limited i);
       ])

type block_validator_limits = {
  protocol_timeout : Time.System.Span.t;
  operation_metadata_size_limit : operation_metadata_size_limit;
}

(* [default_operation_metadata_size_limit] is used to filter and
   potentially discard a given metadata if its size exceed the cap. *)
let default_operation_metadata_size_limit = Limited 10_000_000

let default_block_validator_limits =
  {
    protocol_timeout = Time.System.Span.of_seconds_exn 120.;
    operation_metadata_size_limit = default_operation_metadata_size_limit;
  }

let block_validator_limits_encoding =
  let open Data_encoding in
  conv
    (fun {protocol_timeout; operation_metadata_size_limit} ->
      (protocol_timeout, operation_metadata_size_limit))
    (fun (protocol_timeout, operation_metadata_size_limit) ->
      {protocol_timeout; operation_metadata_size_limit})
    (obj2
       (dft
          "protocol_request_timeout"
          timeout_encoding
          default_block_validator_limits.protocol_timeout)
       (dft
          "operation_metadata_size_limit"
          operation_metadata_size_limit_encoding
          default_block_validator_limits.operation_metadata_size_limit))

type prevalidator_limits = {
  max_refused_operations : int;
  operation_timeout : Time.System.Span.t;
  operations_batch_size : int;
}

let default_prevalidator_limits =
  {
    operation_timeout = Time.System.Span.of_seconds_exn 10.;
    max_refused_operations = 1000;
    operations_batch_size = 50;
  }

let prevalidator_limits_encoding =
  let open Data_encoding in
  conv
    (fun {operation_timeout; max_refused_operations; operations_batch_size} ->
      (operation_timeout, max_refused_operations, operations_batch_size))
    (fun (operation_timeout, max_refused_operations, operations_batch_size) ->
      {operation_timeout; max_refused_operations; operations_batch_size})
    (obj3
       (dft
          "operations_request_timeout"
          timeout_encoding
          default_prevalidator_limits.operation_timeout)
       (dft
          "max_refused_operations"
          uint16
          default_prevalidator_limits.max_refused_operations)
       (dft
          "operations_batch_size"
          int31
          default_prevalidator_limits.operations_batch_size))

type peer_validator_limits = {
  new_head_request_timeout : Time.System.Span.t;
  block_header_timeout : Time.System.Span.t;
  block_operations_timeout : Time.System.Span.t;
  protocol_timeout : Time.System.Span.t;
}

let default_peer_validator_limits =
  {
    block_header_timeout = Time.System.Span.of_seconds_exn 300.;
    block_operations_timeout = Time.System.Span.of_seconds_exn 300.;
    protocol_timeout = Time.System.Span.of_seconds_exn 600.;
    new_head_request_timeout = Time.System.Span.of_seconds_exn 90.;
  }

let peer_validator_limits_encoding =
  let open Data_encoding in
  conv
    (fun {
           block_header_timeout;
           block_operations_timeout;
           protocol_timeout;
           new_head_request_timeout;
         }
       ->
      ( block_header_timeout,
        block_operations_timeout,
        protocol_timeout,
        new_head_request_timeout ))
    (fun ( block_header_timeout,
           block_operations_timeout,
           protocol_timeout,
           new_head_request_timeout )
       ->
      {
        block_header_timeout;
        block_operations_timeout;
        protocol_timeout;
        new_head_request_timeout;
      })
    (obj4
       (dft
          "block_header_request_timeout"
          timeout_encoding
          default_peer_validator_limits.block_header_timeout)
       (dft
          "block_operations_request_timeout"
          timeout_encoding
          default_peer_validator_limits.block_operations_timeout)
       (dft
          "protocol_request_timeout"
          timeout_encoding
          default_peer_validator_limits.protocol_timeout)
       (dft
          "new_head_request_timeout"
          timeout_encoding
          default_peer_validator_limits.new_head_request_timeout))

type synchronisation_limits = {latency : int; threshold : int}

let synchronisation_heuristic_encoding default_latency default_threshold =
  let open Data_encoding in
  conv
    (fun {latency; threshold} -> (latency, threshold))
    (fun (latency, threshold) -> {latency; threshold})
    (obj2
       (dft
          "latency"
          ~description:
            "[latency] is the time interval (in seconds) used to determine if \
             a peer is synchronized with a chain. For instance, a peer whose \
             known head has a timestamp T is considered synchronized if T >= \
             now - latency. This parameter depends on the baking rate and the \
             latency of the network."
          uint16
          default_latency)
       (dft
          "synchronisation_threshold"
          ~description:
            "The minimal number of peers this peer should be synchronized with \
             in order to be bootstrapped."
          uint8
          default_threshold))

type chain_validator_limits = {synchronisation : synchronisation_limits}

let default_chain_validator_limits =
  {synchronisation = {latency = 50; threshold = 4}}

let chain_validator_limits_encoding =
  let open Data_encoding in
  conv
    (fun {synchronisation} -> synchronisation)
    (fun synchronisation -> {synchronisation})
    (* Use a union to support both the deprecated
       bootstrap_threshold and the new synchronisation_threshold
       options when parsing.  When printing, use the new
       synchronisation_threshold option. *)
    (union
       [
         case
           ~title:"synchronisation_heuristic_encoding"
           (Tag 0)
           (synchronisation_heuristic_encoding
              default_chain_validator_limits.synchronisation.latency
              default_chain_validator_limits.synchronisation.threshold)
           (fun x -> Some x)
           (fun x -> x);
         case
           ~title:"legacy_bootstrap_threshold_encoding"
           (Tag 1)
           (obj1
              (dft
                 "bootstrap_threshold"
                 ~description:
                   "[DEPRECATED] Set the number of peers with whom a chain \
                    synchronisation must be completed to bootstrap the node."
                 uint8
                 4))
           (fun _ -> None) (* This is used for legacy *)
           (fun x ->
             {
               threshold = x;
               latency = default_chain_validator_limits.synchronisation.latency;
             });
       ])

let default_disable_context_pruning = false

let default_storage_maintenance_delay = Storage_maintenance.Auto

type limits = {
  block_validator_limits : block_validator_limits;
  prevalidator_limits : prevalidator_limits;
  peer_validator_limits : peer_validator_limits;
  chain_validator_limits : chain_validator_limits;
  history_mode : History_mode.t option;
  disable_context_pruning : bool option;
  storage_maintenance_delay : Storage_maintenance.delay option;
}

let default_limits =
  {
    block_validator_limits = default_block_validator_limits;
    prevalidator_limits = default_prevalidator_limits;
    peer_validator_limits = default_peer_validator_limits;
    chain_validator_limits = default_chain_validator_limits;
    history_mode = None;
    disable_context_pruning = None;
    storage_maintenance_delay = None;
  }

let limits_encoding =
  let open Data_encoding in
  conv
    (fun {
           peer_validator_limits;
           block_validator_limits;
           prevalidator_limits;
           chain_validator_limits;
           history_mode;
           disable_context_pruning;
           storage_maintenance_delay;
         }
       ->
      ( peer_validator_limits,
        block_validator_limits,
        prevalidator_limits,
        chain_validator_limits,
        history_mode,
        disable_context_pruning,
        storage_maintenance_delay ))
    (fun ( peer_validator_limits,
           block_validator_limits,
           prevalidator_limits,
           chain_validator_limits,
           history_mode,
           disable_context_pruning,
           storage_maintenance_delay )
       ->
      {
        peer_validator_limits;
        block_validator_limits;
        prevalidator_limits;
        chain_validator_limits;
        history_mode;
        disable_context_pruning;
        storage_maintenance_delay;
      })
    (obj7
       (dft
          "peer_validator"
          peer_validator_limits_encoding
          default_peer_validator_limits)
       (dft
          "block_validator"
          block_validator_limits_encoding
          default_block_validator_limits)
       (dft
          "prevalidator"
          prevalidator_limits_encoding
          default_prevalidator_limits)
       (dft
          "chain_validator"
          chain_validator_limits_encoding
          default_chain_validator_limits)
       (opt "history_mode" History_mode.encoding)
       (opt "disable_context_pruning" bool)
       (opt "storage_maintenance_delay" Storage_maintenance.delay_encoding))
