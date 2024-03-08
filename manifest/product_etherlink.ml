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
        tezt_tezos |> open_;
      ]
    ~release_status:Unreleased

let tezt_risc_v_sandbox =
  private_lib
    "tezt_risc_v_sandbox"
    ~path:"tezt/lib_risc_v_sandbox"
    ~opam:"tezt-risc-v-sandbox"
    ~synopsis:"Test framework for RISC-V sandbox"
    ~bisect_ppx:No
    ~deps:[tezt_wrapper |> open_ |> open_ ~m:"Base"; tezt_tezos]
    ~release_status:Unreleased

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
        octez_stdlib_unix |> open_;
      ]

let evm_node_lib_prod_encoding =
  octez_evm_node_lib
    "evm_node_lib_prod_encoding"
    ~path:"etherlink/bin_node/lib_prod/encodings"
    ~synopsis:
      "EVM encodings for the EVM node and plugin for the WASM Debugger [prod \
       version]"
    ~deps:
      [octez_base |> open_ ~m:"TzPervasives"; octez_scoru_wasm_debugger_plugin]

let _evm_node_sequencer_protobuf =
  let protobuf_rules =
    Dune.[protobuf_rule "narwhal"; protobuf_rule "exporter"]
  in
  octez_evm_node_lib
    "evm_node_sequencer_protobuf"
    ~path:"etherlink/bin_node/lib_sequencer_protobuf"
    ~synopsis:
      "gRPC libraries for interacting with a consensus node, generated from \
       protobuf definitions"
    ~deps:[ocaml_protoc_compiler]
    ~dune:protobuf_rules

let evm_node_lib_prod =
  octez_evm_node_lib
    "evm_node_lib_prod"
    ~path:"etherlink/bin_node/lib_prod"
    ~synopsis:
      "An implementation of a subset of Ethereum JSON-RPC API for the EVM \
       rollup [prod version]"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc_http |> open_;
        octez_rpc_http_server;
        octez_workers |> open_;
        octez_rpc_http_client_unix;
        octez_version_value;
        octez_stdlib_unix |> open_;
        evm_node_lib_prod_encoding |> open_;
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
      ]

let evm_node_lib_dev_encoding =
  octez_evm_node_lib
    "evm_node_lib_dev_encoding"
    ~path:"etherlink/bin_node/lib_dev/encodings"
    ~synopsis:
      "EVM encodings for the EVM node and plugin for the WASM Debugger [dev \
       version]"
    ~deps:
      [octez_base |> open_ ~m:"TzPervasives"; octez_scoru_wasm_debugger_plugin]

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
      ]

let _octez_evm_node_tests =
  tezt
    ["test_rlp"; "test_ethbloom"]
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
        evm_node_lib_prod;
        evm_node_lib_dev;
      ]

let _tezt_etherlink =
  tezt
    ["evm_rollup"; "evm_sequencer"]
    ~path:"etherlink/tezt/tests"
    ~opam:"tezt-etherlink"
    ~synopsis:"Tezt integration tests for Etherlink"
    ~deps:
      [
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_etherlink |> open_;
        Protocol.(main alpha);
      ]
    ~with_macos_security_framework:true
    ~dep_globs:
      ["evm_kernel_inputs/*"; "../../tezos_contracts/*"; "../../config/*"]
    ~dep_globs_rec:["../../kernel_evm/*"]
    ~preprocess:[staged_pps [ppx_import; ppx_deriving_show]]

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
        evm_node_lib_prod;
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
