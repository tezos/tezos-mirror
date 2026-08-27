(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
module CallTracerRead : sig
  (** [build_calltrace end_call get_next] rebuilds a call trace.
      [end_call node list] adds a list of calls to a node, and
      [get_next i] gets the data [(node,depth)] at line [i] in storage.

      This is left relatively generic to facilitate testing the algorithm.
  *)
  val build_calltrace :
    ('a -> 'a list -> 'a) ->
    (int -> ('a * int) option tzresult Lwt.t) ->
    'a tzresult Lwt.t

  val build_calltraces :
    ('a -> 'a list -> 'a) ->
    (int -> ('a * int) option tzresult Lwt.t) ->
    'a list tzresult Lwt.t
end

module CallTracerRevertedLogs : sig
  (** [drop node] removes the [logs] field of every frame in a reverted
      subtree — geth omits the field rather than emitting an empty list —
      leaving successful siblings untouched. Exposed for unit testing. *)
  val drop :
    ?reverted:bool ->
    Tracer_types.CallTracer.output ->
    Tracer_types.CallTracer.output
end

(** [drop_reverted_logs] below (default [true]) applies
    {!CallTracerRevertedLogs.drop}; [false] keeps the kernel-faithful trace
    (debug CLI). *)

(** [trace_transaction (module Executor) ~block_number ~transaction ~config]
    replays the block [block_number] and traces [transaction_hash] in it, with
    the given [config]. *)
val trace_transaction :
  ?drop_reverted_logs:bool ->
  (module Evm_execution.S) ->
  block_number:Ethereum_types.quantity ->
  transaction_hash:Ethereum_types.hash ->
  config:Tracer_types.config ->
  Tracer_types.output tzresult Lwt.t

(** [trace_call (module Executor) ~call ~block ~config] simulates and traces
    call [call] in block [block], with the given [config]. *)
val trace_call :
  ?drop_reverted_logs:bool ->
  (module Evm_execution.S) ->
  call:Ethereum_types.call ->
  block:Ethereum_types.Block_parameter.extended ->
  config:Tracer_types.config ->
  Tracer_types.output tzresult Lwt.t

(** [trace_block (module Executor) (module Storage) ~block_number ~config]
    replays the block [block_number] and traces all transactions in it, with
    the given [config]. *)
val trace_block :
  ?drop_reverted_logs:bool ->
  (module Evm_execution.S) ->
  (module Block_storage_sig.S) ->
  block_number:Ethereum_types.quantity ->
  config:Tracer_types.config ->
  Tracer_types.block_output tzresult Lwt.t
