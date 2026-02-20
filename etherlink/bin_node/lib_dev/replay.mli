(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [replay_blueprint ~profile ~disable_da_fees ?kernel ?kernel_verbosity ~number ?upto config]
    Replays the blueprint at the given [number] (Ethereum block level) on top of the expected context.

    [disable_da_fees] Disables data availability fees if [true].
    [kernel] WASM kernel used to patch before the replay.
    [kernel_verbosity] Verbosity level for kernel logs.
    [number] Block number to replay.
    [profile] Enables profiling with the corresponding mode.
    [upto] Upper bound block number to replay ranges such as ([number], [upto]).
    [config] Node configuration.
*)
val replay_blueprint :
  strategy:Evm_ro_context.replay_strategy ->
  disable_da_fees:bool ->
  ?kernel:Pvm_types.kernel ->
  ?kernel_verbosity:Events.kernel_log_level ->
  number:Ethereum_types.quantity ->
  ?profile:Configuration.profile_mode ->
  ?upto:Ethereum_types.quantity ->
  Configuration.t ->
  unit tzresult Lwt.t

(** [trace_block ~tracer_kind ~disable_da_fees ?kernel ?kernel_verbosity ~number ?profile config]
    Traces all transactions in the block at the given [number], printing the
    result as JSON to stdout.

    [tracer_kind] Selects the tracer ([CallTracer] or [StructLogger]).
    [disable_da_fees] Disables data availability fees if [true].
    [kernel] WASM kernel used to patch before the replay.
    [kernel_verbosity] Verbosity level for kernel logs.
    [number] Block number to trace.
    [profile] Enables profiling with the corresponding mode.
    [config] Node configuration.
*)
val trace_block :
  tracer_kind:Tracer_types.tracer_kind ->
  disable_da_fees:bool ->
  ?kernel:Pvm_types.kernel ->
  ?kernel_verbosity:Events.kernel_log_level ->
  number:Ethereum_types.quantity ->
  ?profile:Configuration.profile_mode ->
  Configuration.t ->
  unit tzresult Lwt.t

(** [trace_transaction ~tracer_kind ~disable_da_fees ?kernel ?kernel_verbosity ~tx_hash ?profile config]
    Traces a single transaction identified by [tx_hash], printing the result as
    JSON to stdout.

    [tracer_kind] Selects the tracer ([CallTracer] or [StructLogger]).
    [disable_da_fees] Disables data availability fees if [true].
    [kernel] WASM kernel used to patch before the replay.
    [kernel_verbosity] Verbosity level for kernel logs.
    [tx_hash] Hash of the transaction to trace.
    [profile] Enables profiling with the corresponding mode.
    [config] Node configuration.
*)
val trace_transaction :
  tracer_kind:Tracer_types.tracer_kind ->
  disable_da_fees:bool ->
  ?kernel:Pvm_types.kernel ->
  ?kernel_verbosity:Events.kernel_log_level ->
  tx_hash:Ethereum_types.hash ->
  ?profile:Configuration.profile_mode ->
  Configuration.t ->
  unit tzresult Lwt.t
