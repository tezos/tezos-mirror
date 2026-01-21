(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

val alcotezt : Manifest.target

val bls12_381_archive : Manifest.target

val bls12_381 : Manifest.target

val lazy_containers : Manifest.target

val octez_base : Manifest.target

val octez_base_test_helpers : Manifest.target

val octez_base_unix : Manifest.target

val octez_profiler : Manifest.target

val octez_profiler_backends : Manifest.target

val octez_telemetry : Manifest.target

val octez_clic : Manifest.target

val octez_client_base : Manifest.target

val octez_client_base_unix : Manifest.target

val octez_context_disk : Manifest.target

val octez_context_encoding : Manifest.target

val octez_context_sigs : Manifest.target

val octez_crypto : Manifest.target

val octez_dal_node_services : Manifest.target

val octez_event_logging : Manifest.target

val octez_layer2_store : Manifest.target

val octez_layer2_irmin_context : Manifest.target

val octez_layer2_riscv_context : Manifest.target

val octez_layer2_shell : Manifest.target

val octez_openapi : Manifest.target

val octez_sqlite : Manifest.target

val octez_connpool : Manifest.target

val octez_rpc_http_client_unix : Manifest.target

val octez_rpc_http : Manifest.target

val octez_rpc_http_server : Manifest.target

val octez_rust_deps : Manifest.target

val octez_scoru_wasm_debugger_lib : Manifest.target

val octez_scoru_wasm_debugger_plugin : Manifest.target

val octez_scoru_wasm_helpers_functor : Manifest.target

val octez_scoru_wasm : Manifest.target

val octez_shell_services : Manifest.target

val octez_signer_services : Manifest.target

val performance_metrics : Manifest.target

val octez_smart_rollup_lib : Manifest.target

val octez_smart_rollup_node_store_lib : Manifest.target

val octez_stdlib_unix : Manifest.target

val octez_test_helpers : Manifest.target

val octez_micheline : Manifest.target

val octez_version : Manifest.target

val octez_version_value : Manifest.target

val octez_workers : Manifest.target

val registered_octez_l2_libs : Manifest.Sub_lib.container

val registered_octez_libs : Manifest.Sub_lib.container

val registered_octez_proto_libs : Manifest.Sub_lib.container

val registered_octez_shell_libs : Manifest.Sub_lib.container

val tezt_performance_regression : Manifest.target

val tezt_migration_registry : Manifest.target

val tezt_tezos : Manifest.target

val tezt_cloud : Manifest.target

val tezt_tx_kernel : Manifest.target

val tezt_wrapper : Manifest.target

module Protocol : sig
  type t

  type number = Dev | V of int | Other

  (** Status of the protocol on Mainnet.

      - [Active]: the protocol is the current protocol on Mainnet, is being proposed,
        or was active recently and was not deleted or frozen yet.
        Or, it is protocol Alpha.
      - [Frozen]: the protocol is an old protocol of Mainnet which was frozen
        (its tests, daemons etc. have been removed).
      - [Overridden]: the protocol has been replaced using a user-activated protocol override.
      - [Not_mainnet]: this protocol was never on Mainnet (e.g. demo protocols). *)
  type status = Active | Frozen | Overridden | Not_mainnet

  val status : t -> status

  val main : t -> Manifest.target

  val plugin : t -> Manifest.target option

  val parameters : t -> Manifest.target option

  val alpha : t

  (** List of all protocols. *)
  val all : t list

  (** List of active protocols. *)
  val active : t list

  val number : t -> number

  val name_dash : t -> string

  (** E.g. ["src/proto_007_PsDELPH1"]. *)
  val base_path : t -> string

  val short_hash : t -> string

  (** Get packages to link.

      This takes a function that selects packages from a protocol.
      For instance, the node wants the embedded protocol and the plugin registerer,
      while the client wants the client commands etc.

      The result is the list of all such packages that exist.
      All of them are optional dependencies. *)
  val all_optionally :
    (t -> Manifest.target option) list -> Manifest.target list

  val client : t -> Manifest.target option

  val test_helpers_exn : t -> Manifest.target

  val test_migration_exn : t -> Manifest.target
end
