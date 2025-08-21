(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ~profile ~disable_da_fees ?kernel ?kernel_verbosity ~data_dir ~number ?upto config]
    Replays the blueprint at the given [number] (Ethereum block level) on top of the expected context.

    [disable_da_fees] Disables data availability fees if [true].
    [kernel] WASM kernel used to patch before the replay.
    [kernel_verbosity] Verbosity level for kernel logs.
    [data_dir] Path to the data directory.
    [number] Block number to replay.
    [profile] Enables profiling with the corresponding mode.
    [upto] Upper bound block number to replay ranges such as ([number], [upto]).
    [config] Node configuration.
*)
val main :
  disable_da_fees:bool ->
  ?kernel:Wasm_debugger.kernel ->
  ?kernel_verbosity:Events.kernel_log_level ->
  data_dir:string ->
  number:Ethereum_types.quantity ->
  ?profile:Configuration.profile_mode ->
  ?upto:Ethereum_types.quantity ->
  Configuration.t ->
  unit tzresult Lwt.t
