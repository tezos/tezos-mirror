(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

val alcotezt : Manifest.target

val bls12_381 : Manifest.target

val octez_base : Manifest.target

val octez_base_test_helpers : Manifest.target

val octez_base_unix : Manifest.target

val octez_clic : Manifest.target

val octez_client_base : Manifest.target

val octez_client_base_unix : Manifest.target

val octez_context_disk : Manifest.target

val octez_context_encoding : Manifest.target

val octez_context_sigs : Manifest.target

val octez_crypto : Manifest.target

val octez_event_logging : Manifest.target

val octez_layer2_store : Manifest.target

val octez_rpc_http_client_unix : Manifest.target

val octez_rpc_http : Manifest.target

val octez_rpc_http_server : Manifest.target

val octez_scoru_wasm_debugger_lib : Manifest.target

val octez_scoru_wasm_debugger_plugin : Manifest.target

val octez_scoru_wasm_helpers : Manifest.target

val octez_scoru_wasm : Manifest.target

val octez_signer_services : Manifest.target

val octez_smart_rollup_lib : Manifest.target

val octez_stdlib_unix : Manifest.target

val octez_test_helpers : Manifest.target

val octez_version_value : Manifest.target

val octez_workers : Manifest.target

val registered_octez_l2_libs : Manifest.Sub_lib.container

val registered_octez_libs : Manifest.Sub_lib.container

val registered_octez_proto_libs : Manifest.Sub_lib.container

val registered_octez_shell_libs : Manifest.Sub_lib.container

val tezt_performance_regression : Manifest.target

val tezt_tezos : Manifest.target

val tezt_tx_kernel : Manifest.target

val tezt_wrapper : Manifest.target

module Protocol : sig
  type t

  type number = Alpha | V of int | Other

  val main : t -> Manifest.target

  val alpha : t

  val active : t list

  val number : t -> number

  val name_dash : t -> string

  val short_hash : t -> string

  val all_optionally :
    (t -> Manifest.target option) list -> Manifest.target list

  val client : t -> Manifest.target option
end
