(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals
open Internals
open Product_octez
open Product_prometheus

include Product (struct
  let name = "etherlink"

  let source = ["etherlink"; "src"]
end)

let tezt_etherlink =
  private_lib
    "tezt_etherlink"
    ~path:"etherlink/tezt/lib"
    ~opam:"tezt-etherlink"
    ~bisect_ppx:No
    ~deps:
      [
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_performance_regression |> open_;
        octez_crypto;
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_cloud |> open_;
        octez_test_helpers |> open_;
      ]
    ~release_status:Unreleased

let _tezt_tests_cloud =
  private_exe
    "main"
    ~path:"tezt/tests/cloud"
    ~opam:"tezt-tests-cloud"
    ~synopsis:"Tezt tests using Tezt cloud"
    ~bisect_ppx:No
    ~deps:
      [
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_cloud |> open_;
        tezt_etherlink;
      ]
    ~release_status:Unreleased
    ~with_macos_security_framework:true

(* Container of the registered sublibraries of [octez-evm-node] *)
let registered_octez_evm_node_libs = Sub_lib.make_container ()

(* Registers a sub-library in the [octez-evm-node] package. *)
let octez_evm_node_lib =
  Sub_lib.sub_lib
    ~package_synopsis:"Octez EVM node libraries"
    ~container:registered_octez_evm_node_libs
    ~package:"octez-evm-node-libs"

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
      ]

let evm_node_migrations =
  octez_evm_node_lib
    "evm_node_migrations"
    ~path:"etherlink/bin_node/migrations"
    ~synopsis:"SQL migrations for the EVM node store"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; caqti_lwt; crunch; re]
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
        re;
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
        octez_workers |> open_;
        octez_rpc_http_client_unix;
        octez_version_value;
        octez_stdlib_unix |> open_;
        evm_node_lib_dev_encoding |> open_;
        lwt_watcher;
        lwt_exit;
        caqti;
        caqti_lwt;
        caqti_lwt_unix;
        caqti_sqlite;
        octez_client_base |> open_;
        evm_node_config |> open_;
        octez_context_sigs;
        octez_context_disk;
        octez_context_encoding;
        octez_scoru_wasm;
        octez_scoru_wasm_helpers |> open_;
        octez_scoru_wasm_debugger_lib |> open_;
        octez_layer2_store |> open_;
        octez_smart_rollup_lib |> open_;
        evm_node_migrations;
        prometheus_app;
        octez_dal_node_services;
      ]

let _octez_evm_node_tests =
  tezt
    ["test_rlp"; "test_ethbloom"; "test_call_tracer_algo"]
    ~path:"etherlink/bin_node/test"
    ~opam:"octez-evm-node-tests"
    ~synopsis:"Tests for the EVM Node"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_base_test_helpers |> open_;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
        evm_node_lib_dev;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        Protocol.(main alpha);
      ]

let _tezt_etherlink =
  tezt
    ["evm_rollup"; "evm_sequencer"; "validate"; "dal_sequencer"; "eth_call"]
    ~path:"etherlink/tezt/tests"
    ~opam:"tezt-etherlink"
    ~synopsis:"Tezt integration tests for Etherlink"
    ~deps:
      [
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
        evm_node_lib_dev_encoding;
        Protocol.(main alpha);
      ]
    ~with_macos_security_framework:true
    ~dep_globs:
      ["evm_kernel_inputs/*"; "../../tezos_contracts/*"; "../../config/*"]
    ~dep_globs_rec:["../../kernel_evm/*"]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let _evm_node =
  public_exe
    (sf "octez-evm-node")
    ~internal_name:(sf "main")
    ~path:"etherlink/bin_node"
    ~opam:"octez-evm-node"
    ~synopsis:
      "An implementation of a subset of Ethereum JSON-RPC API for the EVM \
       rollup"
    ~release_status:Experimental
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_clic;
        octez_rpc_http |> open_;
        octez_rpc_http_server;
        octez_version_value;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
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
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
      ]
