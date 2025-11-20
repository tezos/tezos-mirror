(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2025 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals
open Internals
open Product_octez
open Product_prometheus
open Product_websocket
open Product_efunc_core
open Product_lwt_domain

include Product (struct
  let name = "etherlink"

  let source = ["etherlink"; "src"] @ Product_websocket.product_source
end)

(* Container of the registered sublibraries of [octez-evm-node] *)
let registered_octez_evm_node_libs = Sub_lib.make_container ()

(* Registers a sub-library in the [octez-evm-node] package. *)
let octez_evm_node_lib =
  Sub_lib.sub_lib
    ~package_synopsis:"Octez EVM node libraries"
    ~container:registered_octez_evm_node_libs
    ~package:"octez-evm-node-libs"

let lib_etherlink_wasm_runtime =
  rust_archive
    Manifest_link_deps.LinkTypes.
      [
        RustDep Rustzcash;
        RustDep Riscv_pvm;
        RustDep Wasmer;
        RustDep Etherlink_wasm_runtime;
      ]
    ~inline_tests_link_flags:["-cclib"; "-levm_node_rust_deps.a"]
    (octez_evm_node_lib
       "evm_node_rust_deps"
       ~path:"etherlink/lib_wasm_runtime"
       ~synopsis:"WASM runtime foreign archive"
       ~foreign_archives:["octez_evm_node_rust_deps"]
       ~dune:
         Dune.
           [
             [
               S "dirs";
               S ":standard";
               (* We need this stanza to ensure .cargo can be used as a
                  dependency via source_tree. *)
               S ".cargo";
               (* Do not track Cargo output directory. *)
               [S "not"; S "target"];
             ];
             [
               S "rule";
               [
                 S "targets";
                 S "liboctez_evm_node_rust_deps.a";
                 S "dlloctez_evm_node_rust_deps.so";
               ];
               [
                 S "deps";
                 [S "file"; S "build.sh"];
                 [S "file"; S "Cargo.toml"];
                 [S "file"; S "Cargo.lock"];
                 [S "file"; S "../../rust-toolchain"];
                 [S "source_tree"; S ".cargo"];
                 [S "source_tree"; S "../sputnikvm"];
                 [S "source_tree"; S "../kernel_bifrost"];
                 [S "source_tree"; S "../kernel_calypso"];
                 [S "source_tree"; S "../kernel_calypso2"];
                 [S "source_tree"; S "../kernel_dionysus"];
                 [S "source_tree"; S "../kernel_dionysus_r1"];
                 [S "source_tree"; S "../kernel_ebisu"];
                 [S "source_tree"; S "../kernel_farfadet"];
                 [S "source_tree"; S "../../src/rustzcash_deps"];
                 [S "source_tree"; S "../../src/rust_deps/wasmer-3.3.0"];
                 [S "source_tree"; S "../../src/riscv"];
                 [S "source_tree"; S "../../src/kernel_sdk"];
                 [S "source_tree"; S "../../sdk/rust"];
                 [S "source_tree"; S "src"];
               ];
               [S "action"; [S "no-infer"; [S "bash"; S "./build.sh"]]];
             ];
           ])

let tezt ?(deps = []) =
  tezt ~deps:(bls12_381_archive :: deps) ~include_in_main_tezt_exe:false

let wasm_runtime_callbacks =
  octez_evm_node_lib
    "wasm_runtime_callbacks"
    ~path:"etherlink/lib_wasm_runtime_callbacks"
    ~synopsis:
      "Callbacks implementing the I/O functions required to implement the host \
       functions of the WASM runtime"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_layer2_irmin_context |> open_;
        cohttp_lwt_unix;
        lwt_domain;
        opentelemetry_lwt;
      ]

let wasm_runtime_callbacks_tests =
  tezt
    ["test_vector"; "test_store"]
    ~path:"etherlink/lib_wasm_runtime_callbacks/test"
    ~opam:"octez-evm-wasm-runtime-tests"
    ~synopsis:"Tests for the WASM Runtime"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_base_test_helpers |> open_;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        octez_layer2_irmin_context |> open_;
        wasm_runtime_callbacks;
      ]

let wasm_runtime =
  octez_evm_node_lib
    "evm_node_wasm_runtime"
    ~path:"etherlink/lib_wasm_runtime/ocaml-api"
    ~synopsis:"WASM runtime compatible with the WASM PVM"
    ~link_deps:lib_etherlink_wasm_runtime
    ~deps:[octez_layer2_irmin_context |> open_; wasm_runtime_callbacks]
    ~flags:
      (Flags.standard
         ~disable_warnings:[66]
         (* We disable the warnings emitted by the files generated by [ocaml-rs]. *)
         ())

let supported_installers =
  octez_evm_node_lib
    "evm_node_supported_installers"
    ~path:"etherlink/bin_node/installers"
    ~synopsis:"Collections of embedded installers binaries"
    ~deps:[crunch]
    ~dune:
      Dune.
        [
          [
            S "rule";
            [S "target"; S "installers.ml"];
            [S "deps"; [S "glob_files"; S "*.wasm"]];
            [
              S "action";
              [
                S "run";
                S "ocaml-crunch";
                S "-e";
                S "wasm";
                S "-m";
                S "plain";
                S "-o";
                S "%{target}";
                S "-s";
                S ".";
              ];
            ];
          ];
        ]

let evm_node_migrations =
  octez_evm_node_lib
    "evm_node_migrations"
    ~path:"etherlink/bin_node/migrations"
    ~synopsis:"SQL migrations for the EVM node store"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        caqti_lwt;
        crunch;
        re;
        octez_sqlite |> open_;
      ]
    ~dune:
      Dune.
        [
          [
            S "rule";
            [S "target"; S "migrations.ml"];
            [S "deps"; [S "glob_files"; S "*.sql"]];
            [
              S "action";
              [
                S "run";
                S "ocaml-crunch";
                S "-e";
                S "sql";
                S "-m";
                S "plain";
                S "-o";
                S "%{target}";
                S "-s";
                S ".";
              ];
            ];
          ];
        ]

let evm_node_lib_dev_encoding =
  octez_evm_node_lib
    "evm_node_lib_dev_encoding"
    ~path:"etherlink/bin_node/lib_dev/encodings"
    ~synopsis:
      "EVM encodings for the EVM node and plugin for the WASM Debugger [dev \
       version]"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_scoru_wasm_debugger_plugin;
        websocket;
        re;
        uuidm;
      ]

let tezt_etherlink =
  private_lib
    "tezt_etherlink"
    ~path:"etherlink/tezt/lib"
    ~opam:"tezt-etherlink"
    ~bisect_ppx:No
    ~deps:
      [
        evm_node_lib_dev_encoding;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_performance_regression |> open_;
        octez_crypto;
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_cloud |> open_;
        octez_test_helpers |> open_;
      ]
    ~release_status:Unreleased

let evm_node_lib_dev_tezlink =
  let tezlink_target_proto =
    List.find (fun proto -> Protocol.short_hash proto = "PtSeouLo") Protocol.all
  in
  let tezlink_protocol_plugin =
    match Protocol.plugin tezlink_target_proto with
    | Some target -> target
    | None -> (* unreachable *) assert false
  in
  let tezlink_protocol_parameters =
    match Protocol.parameters tezlink_target_proto with
    | Some target -> target
    | None -> (* unreachable *) assert false
  in
  let tezlink_genesis_proto =
    List.find (fun proto -> Protocol.short_hash proto = "Ps9mPmXa") Protocol.all
  in
  let tezlink_genesis_protocol_plugin =
    match Protocol.client tezlink_genesis_proto with
    | Some genesis -> genesis
    | None -> (* unreachable *) assert false
  in

  octez_evm_node_lib
    "evm_node_lib_dev_tezlink"
    ~path:"etherlink/bin_node/lib_dev/tezlink"
    ~synopsis:"Tezlink dependencies for the EVM node"
    ~deps:
      [
        evm_node_lib_dev_encoding |> open_;
        tezlink_protocol_plugin;
        tezlink_protocol_parameters;
        tezlink_genesis_protocol_plugin;
        octez_base |> open_ ~m:"TzPervasives";
        octez_shell_services;
        octez_version;
        octez_micheline;
        Protocol.test_helpers_exn tezlink_target_proto;
        lwt_watcher;
      ]

let evm_node_config =
  octez_evm_node_lib
    "evm_node_config"
    ~path:"etherlink/bin_node/config"
    ~synopsis:"Configuration for the EVM node"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_client_base |> open_;
        octez_rpc_http |> open_;
        octez_rpc_http_server;
        octez_stdlib_unix |> open_;
        evm_node_lib_dev_encoding |> open_;
        octez_telemetry;
      ]

let evm_node_lib_dev =
  octez_evm_node_lib
    "evm_node_lib_dev"
    ~path:"etherlink/bin_node/lib_dev"
    ~synopsis:
      "An implementation of a subset of Ethereum JSON-RPC API for the EVM \
       rollup [dev version]"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc_http |> open_;
        octez_rpc_http_server;
        dream;
        websocket_lwt_unix;
        websocket_cohttp_lwt;
        octez_workers |> open_;
        octez_connpool;
        octez_rpc_http_client_unix;
        octez_version_value;
        octez_stdlib_unix |> open_;
        evm_node_lib_dev_encoding |> open_;
        evm_node_lib_dev_tezlink |> open_;
        lwt_watcher;
        lwt_exit;
        octez_sqlite |> open_;
        octez_client_base |> open_;
        evm_node_config |> open_;
        octez_context_sigs;
        octez_context_disk;
        octez_context_encoding;
        octez_scoru_wasm;
        octez_scoru_wasm_helpers_functor |> open_;
        octez_scoru_wasm_debugger_lib;
        octez_layer2_store |> open_;
        octez_layer2_irmin_context |> open_;
        octez_smart_rollup_lib |> open_;
        octez_smart_rollup_node_store_lib;
        evm_node_migrations;
        prometheus_app;
        octez_dal_node_services;
        supported_installers;
        wasm_runtime;
        performance_metrics;
        opentelemetry_lwt;
        octez_telemetry;
        lwt_domain;
      ]

let floodgate_lib =
  private_lib
    "floodgate_lib"
    ~path:"etherlink/bin_floodgate/lib_floodgate"
    ~opam:"floodgate"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        efunc_core;
        octez_rpc_http |> open_;
        octez_rpc_http_client_unix;
        octez_clic;
        evm_node_lib_dev |> open_;
        evm_node_lib_dev_encoding |> open_;
        evm_node_config |> open_;
        octez_workers;
      ]

let _octez_evm_node_tests =
  tests
    [
      "test_rlp";
      "test_ethbloom";
      "test_call_tracer_algo";
      "test_wasm_runtime";
      "test_blueprint_roundtrip";
      "test_bitset_nonce";
    ]
    ~path:"etherlink/bin_node/test"
    ~opam:"octez-evm-node-tests"
    ~synopsis:"Tests for the EVM Node"
    ~with_macos_security_framework:true
    ~linkall:
      (* Necessary to get proper code coverage on Alpine working. Without this,
         calling Rust FFI segfault. *)
      true
    ~dep_files:
      [
        "rlptest.json";
        "invalidRLPTest.json";
        "../../kernel_latest/kernel/tests/resources/mainnet_kernel.wasm";
      ]
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_base_test_helpers |> open_;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
        evm_node_lib_dev;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
        octez_layer2_irmin_context |> open_;
        Protocol.(main alpha);
        octez_client_base;
        octez_client_base_unix;
      ]

let _etherlink_tezts =
  tezt
    [
      "evm_rollup";
      "evm_sequencer";
      "validate";
      "dal_sequencer";
      "eth_call";
      "gc";
      "tezlink";
      "preconfirmation";
      "tezosx";
    ]
    ~path:"etherlink/tezt/tests"
    ~opam:"tezt-etherlink"
    ~synopsis:"Tezt integration tests for Etherlink"
    ~deps:
      [
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
        (* This executable includes the tests of [lib_wasm_runtime_callbacks]. *)
        wasm_runtime_callbacks_tests;
        evm_node_lib_dev_encoding;
        Protocol.(main alpha);
      ]
    ~with_macos_security_framework:true
    ~dep_globs:
      ["evm_kernel_inputs/*"; "../../tezos_contracts/*"; "../../config/*"]
    ~dep_globs_rec:["../../kernel_latest/*"]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let _courier =
  public_exe
    "courier"
    ~internal_name:"main"
    ~path:"etherlink/bin_courier"
    ~opam:"etherlink-courier"
    ~synopsis:
      "An ad-hoc client to allow the sequencer to withdraw inclusion fees to \
       the Layer 1 using a key hosted on GCP KMS"
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_clic;
        evm_node_config |> open_;
        floodgate_lib;
      ]

let _evm_node =
  public_exe
    "octez-evm-node"
    ~internal_name:"main"
    ~path:"etherlink/bin_node"
    ~opam:"octez-evm-node"
    ~synopsis:
      "An implementation of a subset of Ethereum JSON-RPC API for the EVM \
       rollup"
    ~release_status:Experimental
    ~with_macos_security_framework:true
    ~link_flags:
      (* Required to deal with larger stacks required to natively execute
         kernels *)
      [[S ":include"; S "%{workspace_root}/link-flags-evm-node.sexp"]]
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_clic;
        octez_rpc_http |> open_;
        octez_rpc_http_server;
        octez_version_value;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        octez_sqlite |> open_;
        evm_node_lib_dev;
        evm_node_config |> open_;
      ]
    ~bisect_ppx:Yes

let _tezt_testnet_scenarios =
  public_exe
    "octez-testnet-scenarios"
    ~internal_name:"main"
    ~path:"src/bin_testnet_scenarios"
    ~synopsis:"Run scenarios on testnets"
    ~bisect_ppx:No
    ~static:false
    ~deps:
      [
        bls12_381_archive;
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
      ]

let tezt_benchmark_lib =
  private_lib
    "etherlink_benchmark_lib"
    ~path:"etherlink/tezt/benchmarks/lib"
    ~opam:"tezt-etherlink"
    ~bisect_ppx:No
    ~deps:
      [
        bls12_381_archive;
        crunch;
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
        floodgate_lib;
      ]
    ~dune:
      Dune.
        [
          [
            S "rule";
            [S "target"; S "static_contracts.ml"];
            [S "deps"; [S "glob_files_rec"; S "contracts/**.{abi,bin,json}"]];
            [
              S "action";
              [
                S "run";
                S "ocaml-crunch";
                S "-e";
                S "bin";
                S "-e";
                S "abi";
                S "-e";
                S "json";
                S "-m";
                S "plain";
                S "-o";
                S "%{target}";
                S "-s";
                S "contracts";
              ];
            ];
          ];
        ]

let _tezt_node_benchmark =
  public_exe
    "etherlink-benchmark"
    ~internal_name:"main"
    ~path:"etherlink/tezt/benchmarks"
    ~synopsis:"Run EVM node benchmark"
    ~bisect_ppx:No
    ~static:false
    ~deps:
      [
        bls12_381_archive;
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
        floodgate_lib;
        tezt_benchmark_lib;
      ]
    ~with_macos_security_framework:true
    ~dep_globs:
      ["evm_kernel_inputs/*"; "../../tezos_contracts/*"; "../../config/*"]
    ~dep_globs_rec:["../../kernel_latest/*"]

let _tezt_etherlink_benchmark_producer =
  public_exe
    "etherlink-benchmark-producer"
    ~internal_name:"main"
    ~path:"etherlink/bin_benchmark_producer"
    ~synopsis:"Produce benchmark data to test etherlink primitives"
    ~bisect_ppx:No
    ~static:false
    ~deps:
      [
        bls12_381_archive;
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
        octez_base |> open_ ~m:"TzPervasives";
      ]

let _etherlink_governance_observer =
  public_exe
    "etherlink-governance-observer"
    ~internal_name:"governance_observer"
    ~path:"etherlink/governance-metrics/src"
    ~opam:"etherlink-governance-observer"
    ~synopsis:
      "A binary to observe, scrap and store Etherlink's governance contracts \
       informations"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_version_value;
        octez_clic;
        prometheus_app;
        cohttp_lwt_unix;
        cohttp_lwt;
        octez_rpc_http |> open_;
        octez_rpc_http_client_unix;
      ]
    ~release_status:Experimental
    ~bisect_ppx:Yes

(* Back-register the websocket library. *)
let () =
  Sub_lib.add_doc_link
    registered_octez_evm_node_libs
    ~target:"!module-Websocket_cohttp_lwt"
    ~text:"Websocket_cohttp_lwt"

let _floodgate_bin =
  public_exe
    "floodgate"
    ~path:"etherlink/bin_floodgate"
    ~with_macos_security_framework:true
    ~internal_name:"main"
    ~release_status:Unreleased
    ~opam:"floodgate"
    ~synopsis:"Tool to flood an EVM chain with transaction batches"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        evm_node_config |> open_;
        floodgate_lib |> open_;
      ]

let _outbox_monitor =
  public_exe
    "etherlink-outbox-monitor"
    ~internal_name:"main"
    ~path:"etherlink/bin_outbox_monitor"
    ~opam:"etherlink-outbox-monitor"
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~synopsis:
      "A binary to monitor withdrawals in the outbox and their execution"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_version_value;
        octez_clic;
        octez_rpc_http |> open_;
        octez_rpc_http_client_unix;
        caqti_lwt;
        crunch;
        re;
        octez_sqlite |> open_;
        evm_node_lib_dev_encoding |> open_;
        evm_node_lib_dev |> open_;
        octez_smart_rollup_lib;
      ]
    ~dune:
      Dune.
        [
          [
            S "rule";
            [S "target"; S "migrations.ml"];
            [S "deps"; [S "glob_files"; S "migrations/*.sql"]];
            [
              S "action";
              [
                S "run";
                S "ocaml-crunch";
                S "-e";
                S "sql";
                S "-m";
                S "plain";
                S "-o";
                S "%{target}";
                S "-s";
                S ".";
              ];
            ];
          ];
        ]

let _fa_bridge_watchtower =
  public_exe
    "fa-bridge-watchtower"
    ~internal_name:"main"
    ~path:"etherlink/fa-bridge-watchtower"
    ~opam:"fa-bridge-watchtower"
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~synopsis:"A binary to claim FA deposits sent by the bridge"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_version_value;
        octez_clic;
        octez_rpc_http |> open_;
        octez_rpc_http_client_unix;
        octez_stdlib_unix |> open_;
        caqti_lwt;
        crunch;
        re;
        octez_sqlite |> open_;
        evm_node_lib_dev_encoding |> open_;
        evm_node_lib_dev |> open_;
        efunc_core;
        dream;
        octez_openapi;
      ]
    ~dune:
      Dune.
        [
          [
            S "rule";
            [S "target"; S "migrations.ml"];
            [S "deps"; [S "glob_files"; S "migrations/*.sql"]];
            [
              S "action";
              [
                S "run";
                S "ocaml-crunch";
                S "-e";
                S "sql";
                S "-m";
                S "plain";
                S "-o";
                S "%{target}";
                S "-s";
                S ".";
              ];
            ];
          ];
        ]
