(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2025 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2025 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals
open Internals
open Product_data_encoding
open Product_prometheus
open Product_resto
open Product_lwt_domain

include Product (struct
  let name = "octez"

  (** This source will be used to generate the Octez tarball,
      This should include everything required to run [make octez].

      We also include the source of data-encoding. See data-encoding product
      file for the rationale behind this design decision. *)
  let source =
    [
      "src/";
      "tezt/";
      "Makefile";
      "dune";
      "dune-project";
      "dune-workspace";
      "CHANGES.rst";
      "LICENSES";
      "opam";
      "script-inputs";
      "scripts";
      "sdk/";
      "irmin/";
      "brassaia/";
      "brassaia-eio/";
      "rust-toolchain";
      "lwt_domain/";
    ]
    @ Product_data_encoding.product_source @ Product_prometheus.product_source
    @ Product_resto.product_source
end)

module String_set = Set.Make (String)

(** [~enabled_if] clause that disables tests in the CI. *)
let not_in_ci = Dune.[S "="; S "false"; S "%{env:CI=false}"]

let final_protocol_versions =
  let path = "src/lib_protocol_compiler/final_protocol_versions" in
  let ic = open_in path in
  let rec loop acc =
    try
      let s = input_line ic in
      let acc' = if String.equal s "" then acc else String_set.add s acc in
      loop acc'
    with End_of_file ->
      close_in ic ;
      acc
  in
  loop String_set.empty

(* Fork of https://github.com/essdotteedot/distributed, used for plonk.
   uwt has been removed. The directories examples and tests have been dropped.
*)
let distributed_internal =
  public_lib
    "octez-distributed-internal"
    ~internal_name:"distributed"
    ~path:"src/lib_distributed_internal/src"
    ~synopsis:"Fork of distributed. Use for Octez only"
    ~deps:[unix]

let distributed_internal_lwt =
  public_lib
    "octez-distributed-lwt-internal"
    ~internal_name:"distributed_lwt"
    ~path:"src/lib_distributed_internal/lwt"
    ~synopsis:"Fork of distributed-lwt. Use for Octez only"
    ~deps:[unix; distributed_internal; lwt; lwt_unix; logs_lwt]

(* The main module [Octez_alcotest] is [open_] so that one can replace
   the [alcotest] dependency with [alcotezt] and it just works.
   If we use [~internal_name:"alcotest"] here, it would also work,
   except in cases where the real Alcotest is also a dependency. *)
let alcotezt =
  public_lib
    "octez-alcotezt"
    ~path:"tezt/lib_alcotezt"
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4727

         we mark "octez-alcotezt" as released but the real solution is to
         modify the manifest to add build instructions for dune to be
         used `with-test` *)
    ~release_status:Released
    ~synopsis:
      "Provide the interface of Alcotest for Octez, but with Tezt as backend"
    ~deps:[tezt_core_lib]
  |> open_

(* Container of the registered sublibraries of [octez-libs] *)
let registered_octez_libs = Sub_lib.make_container ()

(* Back-register the data-encoding library which is currently maintained as its
   own product but still attached to octez-libs. *)
let () =
  Sub_lib.add_doc_link
    registered_octez_libs
    ~target:"!module-Data_encoding"
    ~text:"Data_encoding"

(* Back-register the cohttp library which is currently maintained as its
         own product but still attached to octez-libs. *)
let () =
  Sub_lib.add_doc_link
    registered_octez_libs
    ~target:"!module-Cohttp"
    ~text:"Cohttp"

(* Back-register the prometheus library which is currently maintained as its
         own product but still attached to octez-libs. *)
let () =
  Sub_lib.add_doc_link
    registered_octez_libs
    ~target:"!module-Prometheus"
    ~text:"Prometheus"

(* Back-register the resto library which is currently maintained as its
         own product but still attached to octez-libs. *)
let () =
  Sub_lib.add_doc_link
    registered_octez_libs
    ~target:"!module-Resto"
    ~text:"Resto"

(* Container of the registered sublibraries of [octez-shell-libs] *)
let registered_octez_shell_libs = Sub_lib.make_container ()

(* Container of the registered sublibraries of [octez-proto-libs] *)
let registered_octez_proto_libs = Sub_lib.make_container ()

(* Container of the registered sublibraries of [octez-l2-libs] *)
let registered_octez_l2_libs = Sub_lib.make_container ()

(* Container of the registered sublibraries of [octez-internal-libs] *)
let octez_internal_libs = Sub_lib.make_container ()

(* Registers a sub-library in [octez-libs] packages.
   This package should contain all Octez basic libraries. *)
let octez_lib =
  Sub_lib.sub_lib
    ~package_synopsis:
      "A package that contains multiple base libraries used by the Octez suite"
    ~container:registered_octez_libs
    ~package:"octez-libs"

(* Registers a sub-library in the [octez-shell-libs] package.
   This package should contain all the libraries related to the shell. *)
let octez_shell_lib =
  Sub_lib.sub_lib
    ~package_synopsis:"Octez shell libraries"
    ~container:registered_octez_shell_libs
    ~package:"octez-shell-libs"

(* Registers a sub-library in the [octez-proto-libs] package.
   This package should contain all the libraries related to the protocol. *)
let octez_proto_lib =
  Sub_lib.sub_lib
    ~package_synopsis:"Octez protocol libraries"
    ~container:registered_octez_proto_libs
    ~package:"octez-proto-libs"

(* Registers a sub-library in the [octez-l2-libs] package.
   This package should contain all the libraries related to layer 2. *)
let octez_l2_lib =
  Sub_lib.sub_lib
    ~package_synopsis:"Octez layer2 libraries"
    ~container:registered_octez_l2_libs
    ~package:"octez-l2-libs"

(* Registers a sub-library in the [octez-internal-libs] package.

   This package should contain Octez dependencies that are under the
   ISC license (as of December 2023, these are exactly the Irmin
   packages). *)
let octez_internal_lib =
  Sub_lib.sub_lib
    ~package_synopsis:
      "A package that contains some libraries used by the Octez suite"
    ~container:octez_internal_libs
    ~package:"octez-internal-libs"
    ~license:"ISC"
    ~extra_authors:["Thomas Gazagnaire"; "Thomas Leonard"; "Craig Ferguson"]

let tezt_wrapper =
  octez_lib "tezt-wrapper" ~path:"tezt/lib_wrapper" ~deps:[tezt_lib]

let qcheck_tezt =
  octez_lib
    "qcheck-tezt"
    ~path:"tezt/lib_qcheck"
    ~synopsis:"Wrapper to register QCheck tests as Tezt tests"
    ~deps:[tezt_core_lib; qcheck_core]

let octez_test_helpers =
  octez_lib
    "test-helpers"
    ~path:"src/lib_test"
    ~internal_name:"tezos_test_helpers"
    ~deps:[uri; fmt; qcheck_alcotest; lwt; pure_splitmix; data_encoding]
    ~linkall:true
    ~release_status:Released

let octez_expect_helper =
  octez_lib
    "expect-helper"
    ~internal_name:"tezos_expect_helper"
    ~path:"src/lib_expect_helper"

let _octez_expect_helper_test =
  private_lib
    "tezos_expect_helper_test"
    ~opam:"octez-libs"
    ~path:"src/lib_expect_helper/test"
    ~deps:[octez_expect_helper]
    ~inline_tests:ppx_expect

let octez_stdlib =
  octez_lib
    "stdlib"
    ~internal_name:"tezos_stdlib"
    ~path:"src/lib_stdlib"
    ~synopsis:"Yet-another local-extension of the OCaml standard library"
    ~deps:[hex; zarith; lwt; aches]
    ~inline_tests:ppx_expect
    ~foreign_stubs:
      {language = C; flags = []; names = ["tzBytes_c"]; include_dirs = []}

let _octez_stdlib_tests =
  tezt
    [
      "test_bits";
      "test_tzList";
      "test_bounded_heap";
      "Test_bounded_min_heap";
      "test_tzString";
      "test_fallbackArray";
      "test_functionalArray";
      "test_hash_queue";
      "test_tzBytes";
      "test_arrays";
    ]
    ~path:"src/lib_stdlib/test"
    ~with_macos_security_framework:true
    ~opam:"octez-libs"
    ~deps:
      [
        octez_stdlib |> open_;
        alcotezt;
        bigstring;
        octez_test_helpers |> open_;
        qcheck_alcotest;
      ]

let _octez_stdlib_test_unix =
  tezt
    [
      "test_lwt_pipe";
      "test_circular_buffer";
      "test_circular_buffer_fuzzy";
      "test_hash_queue_lwt";
      "test_lwt_utils";
    ]
    ~path:"src/lib_stdlib/test-unix"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_stdlib |> open_;
        alcotezt;
        bigstring;
        lwt_unix;
        octez_test_helpers |> open_;
        qcheck_alcotest;
      ]

let octez_lwt_result_stdlib_bare_functor_outputs =
  octez_lib
    "lwt-result-stdlib.bare.functor-outputs"
    ~path:"src/lib_lwt_result_stdlib/bare/functor_outputs"
    ~internal_name:"bare_functor_outputs"
    ~deps:[lwt]

let octez_lwt_result_stdlib_bare_sigs =
  octez_lib
    "lwt-result-stdlib.bare.sigs"
    ~path:"src/lib_lwt_result_stdlib/bare/sigs"
    ~internal_name:"bare_sigs"
    ~deps:[seqes; lwt; octez_lwt_result_stdlib_bare_functor_outputs]

let octez_lwt_result_stdlib_bare_structs =
  octez_lib
    "lwt-result-stdlib.bare.structs"
    ~path:"src/lib_lwt_result_stdlib/bare/structs"
    ~internal_name:"bare_structs"
    ~deps:[seqes; lwt; octez_lwt_result_stdlib_bare_sigs]

let octez_lwt_result_stdlib_traced_functor_outputs =
  octez_lib
    "lwt-result-stdlib.traced.functor-outputs"
    ~path:"src/lib_lwt_result_stdlib/traced/functor_outputs"
    ~internal_name:"traced_functor_outputs"
    ~deps:[lwt; octez_lwt_result_stdlib_bare_sigs]

let octez_lwt_result_stdlib_traced_sigs =
  octez_lib
    "lwt-result-stdlib.traced.sigs"
    ~path:"src/lib_lwt_result_stdlib/traced/sigs"
    ~internal_name:"traced_sigs"
    ~deps:
      [
        lwt;
        octez_lwt_result_stdlib_bare_sigs;
        octez_lwt_result_stdlib_bare_structs;
        octez_lwt_result_stdlib_traced_functor_outputs;
      ]

let octez_lwt_result_stdlib_traced_structs =
  octez_lib
    "lwt-result-stdlib.traced.structs"
    ~path:"src/lib_lwt_result_stdlib/traced/structs"
    ~internal_name:"traced_structs"
    ~deps:
      [
        lwt;
        octez_lwt_result_stdlib_traced_sigs;
        octez_lwt_result_stdlib_bare_structs;
      ]

let octez_lwt_result_stdlib =
  octez_lib
    "lwt-result-stdlib"
    ~path:"src/lib_lwt_result_stdlib"
    ~internal_name:"tezos_lwt_result_stdlib"
    ~synopsis:"error-aware stdlib replacement"
    ~documentation:
      Dune.
        [
          [S "package"; S "octez-libs"];
          [S "mld_files"; S "tezos_lwt_result_stdlib"];
        ]
    ~deps:
      [
        lwt;
        octez_lwt_result_stdlib_bare_sigs;
        octez_lwt_result_stdlib_bare_structs;
        octez_lwt_result_stdlib_traced_sigs;
        octez_lwt_result_stdlib_traced_structs;
      ]

let octez_lwt_result_stdlib_examples_traces =
  octez_lib
    "lwt-result-stdlib.examples.traces"
    ~path:"src/lib_lwt_result_stdlib/examples/traces"
    ~internal_name:"traces"
    ~deps:
      [
        lwt;
        octez_lwt_result_stdlib_bare_structs;
        octez_lwt_result_stdlib_traced_sigs;
      ]

let _octez_lwt_result_stdlib_tests =
  tezt
    [
      "support";
      "traits_tiered";
      "test_hashtbl";
      "test_list_basic";
      "test_list_basic_lwt";
      "test_seq_basic";
      "test_fuzzing_lib";
      "test_fuzzing_list_against_stdlib";
      "test_fuzzing_option_against_stdlib";
      "test_fuzzing_set_against_stdlib";
      "test_fuzzing_map_against_stdlib";
    ]
    ~path:"src/lib_lwt_result_stdlib/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_lwt_result_stdlib |> open_;
        octez_lwt_result_stdlib_examples_traces;
        lwt_unix;
        alcotezt;
        qcheck_alcotest;
        octez_test_helpers |> open_;
      ]
    ~dune_with_test:Only_on_64_arch

let octez_error_monad =
  octez_lib
    "error-monad"
    ~internal_name:"tezos_error_monad"
    ~path:"src/lib_error_monad"
    ~synopsis:"Error monad"
    ~deps:
      [
        octez_stdlib |> open_;
        data_encoding |> open_;
        lwt_canceler;
        lwt;
        octez_lwt_result_stdlib;
      ]
    ~conflicts:[external_lib "result" V.(less_than "1.5")]

let octez_error_monad_legacy =
  octez_lib
    "error-monad-legacy"
    ~internal_name:"tezos_error_monad_legacy"
    ~path:"src/lib_error_monad_legacy"
    ~synopsis:"Error monad (legacy)"
    ~deps:[lwt; octez_error_monad]
    ~conflicts:[external_lib "result" V.(less_than "1.5")]

let octez_hacl =
  octez_lib
    "hacl"
    ~internal_name:"tezos_hacl"
    ~path:"src/lib_hacl"
    ~synopsis:"Thin layer around hacl-star"
    ~deps:[hacl_star; hacl_star_raw]
    ~conflicts:[Conflicts.hacl_x25519]

let _octez_hacl_tests =
  tezt
    [
      "test_prop_signature_pk";
      "test_hacl";
      "test_prop_hacl_hash";
      "test";
      "vectors_p256";
      "vectors_ed25519";
    ]
    ~path:"src/lib_hacl/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib |> open_;
        zarith;
        data_encoding |> open_;
        octez_hacl |> open_;
        qcheck_alcotest;
        alcotezt;
        octez_test_helpers |> open_;
      ]

let _octez_error_monad_tests =
  tezt
    ["test_registration"; "test_splitted_error_encoding"]
    ~path:"src/lib_error_monad/test"
    ~with_macos_security_framework:true
    ~opam:"octez-libs"
    ~deps:[octez_error_monad |> open_; data_encoding; alcotezt]

let octez_rpc =
  octez_lib
    "rpc"
    ~internal_name:"tezos_rpc"
    ~path:"src/lib_rpc"
    ~synopsis:
      "Library of auto-documented RPCs (service and hierarchy descriptions)"
    ~deps:
      [
        data_encoding |> open_;
        octez_error_monad |> open_;
        resto;
        resto_directory;
        uri;
      ]

let lib_rustzcash =
  rust_archive
    Manifest_link_deps.LinkTypes.[RustDep Rustzcash]
    ~inline_tests_link_flags:["-cclib"; "-loctez_rustzcash_deps"]
    (public_lib
       "octez-rustzcash-deps"
       ~path:"src/rustzcash_deps"
       ~synopsis:"Octez Rustzcash Dependencies"
       ~foreign_archives:["octez_rustzcash_deps"]
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
                 S "liboctez_rustzcash_deps.a";
                 S "dlloctez_rustzcash_deps.so";
               ];
               [
                 S "deps";
                 [S "file"; S "build.sh"];
                 [S "file"; S "Cargo.toml"];
                 [S "file"; S "Cargo.lock"];
                 [S "file"; S "../../rust-toolchain"];
                 [S "source_tree"; S ".cargo"];
                 [S "source_tree"; S "bin"];
                 [S "source_tree"; S "include"];
                 [S "source_tree"; S "src"];
               ];
               [S "action"; [S "no-infer"; [S "system"; S "./build.sh"]]];
             ];
           ])

(* TODO: We temporarily also return the target octez_rust_deps so that inline tests will continue work.
   They will be removed in the MR !17226 *)
let octez_rust_deps, lib_wasmer_riscv =
  let octez_rust_deps =
    public_lib
      "octez-rust-deps"
      ~path:"src/rust_deps"
      ~synopsis:"Octez Rust Dependencies"
      ~foreign_archives:["octez_rust_deps"]
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
              [S "targets"; S "liboctez_rust_deps.a"; S "dlloctez_rust_deps.so"];
              [
                S "deps";
                [S "file"; S "build.sh"];
                [S "file"; S "Cargo.toml"];
                [S "file"; S "Cargo.lock"];
                [S "file"; S "../../rust-toolchain"];
                [S "source_tree"; S ".cargo"];
                [S "source_tree"; S "../rustzcash_deps"];
                [S "source_tree"; S "wasmer-3.3.0"];
                [S "source_tree"; S "rust_igd_next"];
                [S "source_tree"; S "rust_tezos_context"];
                [S "source_tree"; S "src"];
                [S "source_tree"; S "../riscv"];
                [S "source_tree"; S "../kernel_sdk"];
                [S "source_tree"; S "../../sdk/rust"];
              ];
              [S "action"; [S "no-infer"; [S "system"; S "./build.sh"]]];
            ];
          ]
  in
  ( octez_rust_deps,
    rust_archive
      Manifest_link_deps.LinkTypes.
        [RustDep Rustzcash; RustDep Wasmer; RustDep Riscv_pvm]
      ~inline_tests_link_flags:["-cclib"; "-loctez_rust_deps"]
      octez_rust_deps )

let octez_rust_igd_next =
  public_lib
    "octez-igd-next"
    ~path:"src/rust_deps/rust_igd_next/ocaml-api"
    ~synopsis:"OCaml API to igd-next"
    ~flags:
      (Flags.standard
         ~disable_warnings:[66]
         (* We disable the warnings emitted by the files generated by [ocaml-rs]. *)
         ())

let bls12_381_archive =
  public_lib
    "bls12-381.archive"
    ~modules:["archive"]
    ~path:"src/lib_bls12_381"
    ~foreign_archives:["blst"]
    ~dune:
      Dune.
        [
          [
            S "rule";
            [S "target"; S "archive.ml"];
            [S "action"; [S "run"; S "touch"; S "archive.ml"]];
          ];
        ]

let bls12_381 =
  public_lib
    "bls12-381"
    ~path:"src/lib_bls12_381"
    ~available:(N_ary_and [No_32; No_ppc; No_s390x])
    ~synopsis:
      "Implementation of the BLS12-381 curve (wrapper for the Blst library)"
    ~modules:
      [
        "bls12_381";
        "ff_sig";
        "fr";
        "fq12";
        "g1";
        "g2";
        "gt";
        "pairing";
        "fq";
        "fq2";
      ]
    ~private_modules:["fq"; "fq2"]
    ~linkall:true
    ~c_library_flags:["-Wall"; "-Wextra"; ":standard"; "-lpthread"]
    ~deps:[integers; zarith; hex]
    ~foreign_stubs:
      {
        language = C;
        flags =
          [
            S "-Wall";
            S "-Wextra";
            S ":standard";
            [S ":include"; S "c_flags_blst.sexp"];
          ];
        include_dirs = [];
        names = ["blst_wrapper"; "blst_bindings_stubs"];
      }
    ~dune:
      Dune.
        [
          [S "copy_files"; S "libblst/bindings/blst.h"];
          [S "copy_files"; S "libblst/bindings/blst_extended.h"];
          [S "copy_files"; S "libblst/bindings/blst_aux.h"];
          [S "data_only_dirs"; S "libblst"];
          [
            S "rule";
            [
              S "deps";
              [S "source_tree"; S "libblst"];
              S "build_blst.sh";
              S "blst_extended.c";
              S "blst_extended.h";
            ];
            [S "targets"; S "libblst.a"; S "dllblst.so"; S "c_flags_blst.sexp"];
            [
              S "action";
              [
                S "no-infer";
                [
                  S "progn";
                  [
                    S "run";
                    S "cp";
                    S "blst_extended.c";
                    S "libblst/src/blst_extended.c";
                  ];
                  [
                    S "run";
                    S "cp";
                    S "blst_extended.h";
                    S "libblst/bindings/blst_extended.h";
                  ];
                  [S "run"; S "sh"; S "build_blst.sh"];
                  [S "run"; S "cp"; S "libblst/libblst.a"; S "libblst.a"];
                  [
                    S "ignore-stderr";
                    [
                      S "with-accepted-exit-codes";
                      [S "or"; S "0"; S "1"];
                      [S "run"; S "cp"; S "libblst/libblst.so"; S "dllblst.so"];
                    ];
                  ];
                  [
                    S "ignore-stderr";
                    [
                      S "with-accepted-exit-codes";
                      [S "or"; S "0"; S "1"];
                      [
                        S "run";
                        S "cp";
                        S "libblst/libblst.dylib";
                        S "dllblst.so";
                      ];
                    ];
                  ];
                ];
              ];
            ];
          ];
          [
            S "install";
            [
              S "files";
              S "libblst/bindings/blst.h";
              S "libblst/bindings/blst_aux.h";
              S "blst_extended.h";
              S "blst_misc.h";
              S "caml_bls12_381_stubs.h";
            ];
            [S "section"; S "lib"];
            [S "package"; S "bls12-381"];
          ];
        ]

let tezt ?(deps = []) = tezt ~deps:(bls12_381_archive :: deps)

let _bls12_381_tests =
  tezt
    [
      "test_fr";
      "test_g1";
      "test_g2";
      "test_pairing";
      "test_hash_to_curve";
      "test_random_state";
      "test_fq12";
      "test_gt";
      "utils";
      "ff_pbt";
      "test_ec_make";
    ]
    ~path:"src/lib_bls12_381/test"
    ~opam:"bls12-381"
    ~deps:[alcotezt; qcheck_alcotest; bls12_381]
    ~dep_globs_rec:["test_vectors/*"]

let _octez_bls12_381_utils =
  let names =
    [
      "generate_pairing_vectors";
      "generate_miller_loop_vectors";
      "generate_final_exponentiation_vectors";
      "generate_g1_test_vectors";
      "generate_g2_test_vectors";
      "generate_fr_test_vectors";
      "generate_hash_to_curve_vectors";
    ]
  in
  private_exes
    names
    ~path:"src/lib_bls12_381/utils"
    ~opam:"bls12-381"
    ~bisect_ppx:No
    ~modules:names
    ~deps:[hex; bls12_381_archive; bls12_381]

let octez_bls12_381_signature =
  octez_lib
    "bls12-381-signature"
    ~path:"src/lib_bls12_381_signature"
    ~internal_name:"bls12_381_signature"
    ~deps:[bls12_381]
    ~modules:["bls12_381_signature"]
    ~foreign_stubs:
      {
        language = C;
        flags = [S "-Wall"; S "-Wextra"; S ":standard"];
        include_dirs = [];
        names = ["blst_bindings_stubs"];
      }
    ~c_library_flags:["-Wall"; "-Wextra"; ":standard"; "-lpthread"]
    ~linkall:true

(* TODO: dep_globs aren't added to the rules for JS tests *)
let _octez_bls12_381_signature_tests =
  tezt
    [
      "test_threshold_signature";
      "test_aggregated_signature";
      "test_signature";
      "utils";
    ]
    ~path:"src/lib_bls12_381_signature/test"
    ~opam:"octez-libs"
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5377
         This test is affected by the [FinalizationRegistry] hangs in JS,
         so although JS compatible, we only test in [Native] mode *)
    ~modes:[Native]
    ~deps:[bls12_381; octez_bls12_381_signature; alcotezt]
    ~dep_globs_rec:["test_vectors/*"]
(* See above *)

let octez_event_logging =
  octez_lib
    "event-logging"
    ~internal_name:"tezos_event_logging"
    ~path:"src/lib_event_logging"
    ~synopsis:"Octez event logging library"
    ~deps:
      [
        octez_stdlib |> open_;
        data_encoding |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib;
        uri;
      ]

let octez_event_logging_test_helpers =
  octez_lib
    "event-logging-test-helpers"
    ~internal_name:"tezos_event_logging_test_helpers"
    ~path:"src/lib_event_logging/test_helpers"
    ~synopsis:"Test helpers for the event logging library"
    ~deps:
      [
        octez_stdlib;
        octez_lwt_result_stdlib |> open_;
        data_encoding;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_event_logging |> open_;
        octez_test_helpers |> open_;
        tezt_core_lib |> open_;
        alcotezt;
      ]
    ~linkall:true
    ~bisect_ppx:No

let octez_libcrux_ml_dsa =
  public_lib
    "octez-libcrux-ml-dsa"
    ~internal_name:"octez_libcrux_ml_dsa"
    ~path:"src/rust_libcrux/src"
    ~opam:"octez-libcrux-ml-dsa"
    ~synopsis:"Rust foreign archive for libcrux_ml_dsa"
    ~foreign_archives:["octez_libcrux_ml_dsa"]
    ~flags:(Flags.standard ~disable_warnings:[9; 27; 66] ())
    ~dune:
      Dune.
        [
          [
            S "rule";
            [
              S "targets";
              S "liboctez_libcrux_ml_dsa.a";
              S "dlloctez_libcrux_ml_dsa.so";
            ];
            [
              S "deps";
              [S "glob_files"; S "*.rs"];
              [S "file"; S "../Cargo.toml"];
              [S "file"; S "../Cargo.lock"];
              [S "file"; S "../../../rust-toolchain"];
              [S "source_tree"; S "../.cargo"];
            ];
            [
              S "action";
              [
                S "progn";
                [S "run"; S "sh"; S "-c"; S "cargo build --release"];
                [
                  S "run";
                  S "sh";
                  S "-c";
                  S
                    "mv ../target/release/liboctez_libcrux_ml_dsa.so \
                     ./dlloctez_libcrux_ml_dsa.so 2> /dev/null || mv \
                     ../target/release/liboctez_libcrux_ml_dsa.dylib \
                     ./dlloctez_libcrux_ml_dsa.so";
                ];
                [
                  S "run";
                  S "mv";
                  S "../target/release/liboctez_libcrux_ml_dsa.a";
                  S "liboctez_libcrux_ml_dsa.a";
                ];
              ];
            ];
          ];
        ]

let octez_ml_dsa =
  public_lib
    "octez-ml-dsa"
    ~path:"src/lib_ml_dsa"
    ~synopsis:"ML-DSA-44 signature scheme"
    ~deps:[octez_libcrux_ml_dsa]

let octez_crypto =
  octez_lib
    "crypto"
    ~internal_name:"tezos_crypto"
    ~path:"src/lib_crypto"
    ~synopsis:"Library with all the cryptographic primitives used by Tezos"
    ~deps:
      [
        octez_stdlib |> open_;
        data_encoding |> open_;
        octez_lwt_result_stdlib;
        lwt;
        octez_hacl;
        octez_ml_dsa;
        secp256k1_internal;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_rpc;
        aches;
        zarith;
        bls12_381;
        octez_bls12_381_signature;
        octez_event_logging;
      ]

let _octez_crypto_tests =
  tezt
    [
      "test_run";
      "test_prop_signature";
      "roundtrips";
      "key_encoding_vectors";
      "test_base58";
      "test_blake2b";
      "test_crypto_box";
      "test_deterministic_nonce";
      "test_merkle";
      "test_signature";
      "test_signature_encodings";
      "test_timelock_legacy";
      "test_timelock";
      "test_context_hash";
      "vectors_secp256k1_keccak256";
    ]
    ~path:"src/lib_crypto/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_crypto |> open_;
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        zarith;
        octez_hacl;
        data_encoding |> open_;
        alcotezt;
        qcheck_alcotest;
        octez_test_helpers |> open_;
      ]

let _octez_crypto_tests_unix =
  tezt
    ["test_crypto_box"]
    ~path:"src/lib_crypto/test-unix"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_crypto |> open_;
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        zarith;
        octez_hacl;
        data_encoding |> open_;
        alcotezt;
        lwt_unix;
        qcheck_alcotest;
        octez_test_helpers |> open_;
      ]

let octez_bls12_381_hash =
  octez_lib
    "bls12-381-hash"
    ~path:"src/lib_bls12_381_hash"
    ~internal_name:"bls12_381_hash"
    ~c_library_flags:["-Wall"; "-Wextra"; ":standard"; "-lpthread"]
    ~deps:[bls12_381]
    ~foreign_stubs:
      {
        language = C;
        flags = [];
        include_dirs = [];
        names =
          [
            "caml_rescue_stubs";
            "caml_anemoi_stubs";
            "caml_poseidon_stubs";
            "caml_griffin_stubs";
            "rescue";
            "anemoi";
            "poseidon";
            "griffin";
          ];
      }
    ~linkall:true

let _octez_bls12_381_hash_tests =
  tezt
    ["test_poseidon"; "test_rescue"; "test_anemoi"; "test_griffin"; "test_jive"]
    ~path:"src/lib_bls12_381_hash/test"
    ~opam:"octez-libs"
    ~deps:[alcotezt; bls12_381; octez_bls12_381_hash]
    ~flags:(Flags.standard ~disable_warnings:[3] ())

let octez_mec =
  octez_lib
    "mec"
    ~path:"src/lib_mec"
    ~internal_name:"mec"
    ~deps:[alcotest; bls12_381; bigarray_compat; eqaf]

let _octez_mec_tests =
  tezt
    [
      "ark_poseidon128";
      "ark_pobls";
      "mds_pobls";
      "mds_poseidon128";
      "poseidon128_linear_trick_expected_output";
      "test_vector_pedersen_hash";
      "test_neptunus";
      "test_orchard";
      "test_pedersen_hash";
      "test_poseidon128";
      "test_poseidon252";
      "test_sinsemilla";
      "test_babyjubjub";
      "test_babyjubjub_reduced";
      "test_bandersnatch_affine_montgomery";
      "test_bandersnatch_affine_weierstrass";
      "test_bandersnatch_affine_edwards";
      (* FIXME: test_bandersnatch_all has been removed from the repository, see
         https://gitlab.com/tezos/tezos/-/issues/5147 *)
      "test_bls12_381_affine";
      "test_bls12_381_projective";
      "test_bn254_affine";
      "test_bn254_jacobian";
      "test_bn254_projective";
      "test_curve25519_affine_edwards";
      "test_curve25519_conversions_between_forms";
      "test_curve25519_montgomery";
      "test_curve448";
      "test_ec_functor";
      "test_iso_pallas_affine";
      "test_jubjub";
      "test_jubjub_conversions_between_forms";
      "test_jubjub_weierstrass";
      "test_pallas_affine";
      "test_pallas_jacobian";
      "test_pallas_projective";
      "test_secp256k1_affine";
      "test_secp256k1_jacobian";
      "test_secp256k1_projective";
      "test_secp256r1_affine";
      "test_secp256r1_jacobian";
      "test_secp256r1_projective";
      "test_tweedledee_affine";
      "test_tweedledee_jacobian";
      "test_tweedledee_projective";
      "test_tweedledum_affine";
      "test_tweedledum_jacobian";
      "test_tweedledum_projective";
      "test_vesta_affine";
      "test_vesta_jacobian";
      "test_vesta_projective";
      "test_digestif";
      "test_linear_trick";
      "test_marvellous";
      "test_redjubjub";
      "test_find_group_hash";
      "test_iterator";
    ]
    ~path:"src/lib_mec/test"
    ~opam:"octez-libs"
    ~deps:[alcotezt; octez_mec |> open_]

let octez_polynomial =
  octez_lib
    "polynomial"
    ~path:"src/lib_polynomial"
    ~internal_name:"polynomial"
    ~synopsis:"Polynomials over finite fields"
    ~deps:[bls12_381; zarith]

let _octez_polynomial_tests =
  tezt
    ["test_with_finite_field"; "test_utils"; "polynomial_pbt"]
    ~path:"src/lib_polynomial/test"
    ~opam:"octez-libs"
    ~deps:[bls12_381; octez_mec; alcotezt; octez_polynomial]

let octez_bls12_381_polynomial =
  octez_lib
    "bls12-381-polynomial"
    ~internal_name:"octez_bls12_381_polynomial"
    ~path:"src/lib_bls12_381_polynomial"
    ~synopsis:
      "Polynomials over BLS12-381 finite field - Temporary vendored version of \
       Octez"
    ~c_library_flags:["-Wall"; "-Wextra"; ":standard"]
    ~preprocess:(pps ppx_repr)
    ~deps:[bls12_381; ppx_repr; bigstringaf]
    ~foreign_stubs:
      {
        language = C;
        flags = [];
        include_dirs = [];
        names =
          [
            "caml_bls12_381_polynomial_polynomial_stubs";
            "caml_bls12_381_polynomial_srs_stubs";
            "caml_bls12_381_polynomial_ec_array_stubs";
            "caml_bls12_381_polynomial_fft_stubs";
            "bls12_381_polynomial_polynomial";
            "bls12_381_polynomial_fft";
          ];
      }

let _octez_bls12_381_polynomial_tests =
  tezt
    [
      "test_main";
      "helpers";
      "test_coefficients";
      "test_domains";
      "test_evaluations";
      "test_pbt";
      "test_polynomial";
      "test_srs";
    ]
    ~path:"src/lib_bls12_381_polynomial/test"
    ~opam:"octez-libs"
    ~deps:
      [
        alcotezt;
        qcheck_alcotest;
        octez_polynomial;
        bls12_381;
        octez_bls12_381_polynomial;
      ]
    ~dep_files:["srs_zcash_g1_5"]

let octez_srs_extraction =
  octez_lib
    "srs-extraction"
    ~internal_name:"octez_srs_extraction"
    ~path:"src/lib_srs_extraction"
    ~modules:["libsrs"]
    ~bisect_ppx:No
    ~deps:[bls12_381; octez_bls12_381_polynomial |> open_]

let _octez_srs_extraction_main =
  private_exe
    "srs_extraction_main"
    ~path:"src/lib_srs_extraction"
    ~opam:"octez-libs"
    ~modules:["srs_extraction_main"]
    ~bisect_ppx:No
    ~deps:
      [
        bls12_381_archive;
        octez_srs_extraction |> open_;
        cmdliner;
        unix;
        bls12_381;
        octez_bls12_381_polynomial |> open_;
      ]

let _octez_srs_extraction_tests =
  tezt
    ["test_main"]
    ~path:"src/lib_srs_extraction/test"
    ~opam:"octez-libs"
    ~deps:[octez_srs_extraction |> open_; alcotezt]
    ~dep_files:["phase1radix2m5"]
    ~dune:
      Dune.(
        let extract curve srs_file =
          let generated_srs_file = srs_file ^ ".generated" in
          [
            S "rule";
            [S "alias"; S "runtest"];
            [S "package"; S "octez-libs"];
            [S "deps"; S srs_file; S "phase1radix2m5"];
            [
              S "action";
              [
                S "progn";
                [
                  S "run";
                  S "%{exe:../srs_extraction_main.exe}";
                  S "extract";
                  S "zcash";
                  S curve;
                  S "phase1radix2m5";
                  S "-o";
                  S generated_srs_file;
                ];
                [S "diff"; S generated_srs_file; S srs_file];
              ];
            ];
          ]
        in
        [
          extract "g1" "srs_zcash_g1_5";
          extract "g2" "srs_zcash_g2_5";
          alias_rule
            "runtest"
            ~package:"octez-libs"
            ~deps:["srs_zcash_g1_5"; "srs_zcash_g2_5"]
            ~action:
              (run
                 "../srs_extraction_main.exe"
                 ["check"; "srs_zcash_g1_5"; "srs_zcash_g2_5"]);
          alias_rule
            "runtest"
            ~package:"octez-libs"
            ~deps:["srs_filecoin_g1_6"; "srs_filecoin_g2_6"]
            ~action:
              (run
                 "../srs_extraction_main.exe"
                 ["check"; "srs_filecoin_g1_6"; "srs_filecoin_g2_6"]);
        ])

let octez_kzg =
  octez_lib
    "kzg"
    ~path:"src/lib_kzg"
    ~synopsis:"Toolbox for KZG polynomial commitment"
    ~deps:
      [
        repr;
        data_encoding |> open_;
        octez_bls12_381_polynomial |> open_;
        octez_crypto;
      ]
    ~preprocess:(pps ppx_repr)

let octez_plompiler =
  octez_lib
    "plompiler"
    ~internal_name:"plompiler"
    ~path:"src/lib_plompiler"
    ~deps:
      [
        stdint;
        hacl_star;
        octez_bls12_381_hash;
        octez_polynomial;
        octez_mec;
        octez_kzg;
      ]
    ~preprocess:(staged_pps [ppx_repr; ppx_deriving_show])

(* Deactivating z3 tests. z3 is not installed in the CI *)
(* ~dune: *)
(*   Dune. *)
(*     [ *)
(*       alias_rule *)
(*         "runtest" *)
(*         ~deps_dune:[S "z3/run_z3_tests.sh"; [S "glob_files"; S "z3/*.z3"]] *)
(*         ~action:[S "chdir"; S "z3"; [S "run"; S "sh"; S "run_z3_tests.sh"]]; *)
(*     ] *)

(* Tests of this form are run in multiple PlonK-related packages. *)
let make_plonk_runtest_invocation ~package =
  Dune.
    [
      alias_rule
        "runtest"
        ~package
        ~action:
          (G
             [
               setenv
                 "RANDOM_SEED"
                 "42"
                 (progn
                    [
                      run_exe "main" ["-q"];
                      [S "diff?"; S "test-quick.expected"; S "test.output"];
                    ]);
             ])
        ~enabled_if:not_in_ci;
      alias_rule "runtest_slow" ~package ~action:(run_exe "main" []);
      alias_rule
        "runtest_slow_with_regression"
        ~package
        ~action:
          (G
             [
               setenv
                 "RANDOM_SEED"
                 "42"
                 (progn
                    [
                      run_exe "main" [];
                      [S "diff?"; S "test-slow.expected"; S "test.output"];
                    ]);
             ]);
    ]

let octez_plonk =
  octez_lib
    "plonk"
    ~path:"src/lib_plonk"
    ~synopsis:"Plonk zero-knowledge proving system"
    ~deps:[octez_kzg; octez_plompiler |> open_; str]
    ~preprocess:(pps ppx_repr)

let octez_plonk_aggregation =
  octez_lib
    "plonk.aggregation"
    ~path:"src/lib_aplonk/plonk-aggregation"
    ~internal_name:"aggregation"
    ~preprocess:(pps ppx_repr)
    ~deps:[octez_plonk; octez_bls12_381_polynomial |> open_]

let octez_aplonk =
  octez_lib
    "aplonk"
    ~internal_name:"aplonk"
    ~path:"src/lib_aplonk"
    ~preprocess:(pps ppx_repr)
    ~deps:[octez_plonk_aggregation]

let octez_plonk_distribution =
  octez_lib
    "plonk.distribution"
    ~internal_name:"distribution"
    ~path:"src/lib_distributed_plonk/distribution"
    ~deps:[octez_plonk; octez_plonk_aggregation]
    ~preprocess:(pps ppx_repr)

let octez_plonk_communication =
  octez_lib
    "plonk.communication"
    ~internal_name:"communication"
    ~path:"src/lib_distributed_plonk/communication"
    ~deps:[logs; distributed_internal_lwt; octez_plonk_distribution |> open_]
    ~preprocess:(pps ppx_repr)

let octez_plonk_test_helpers =
  octez_lib
    "plonk.plonk-test"
    ~path:"src/lib_plonk/test"
    ~internal_name:"plonk_test"
    ~deps:[octez_plonk; octez_plonk_aggregation; octez_plonk_distribution]
    ~modules:["helpers"; "cases"]
    ~preprocess:(pps ppx_repr)
    ~dune:(make_plonk_runtest_invocation ~package:"octez-libs")

let _octez_plonk_test_helpers_main =
  private_exe
    "main"
    ~path:"src/lib_plonk/test"
    ~opam:"octez-libs"
    ~modules:
      [
        "main";
        "test_circuit";
        "test_cq";
        "test_evaluations";
        "test_main_protocol";
        "test_pack";
        "test_permutations";
        "test_plookup";
        "test_polynomial_commitment";
        "test_polynomial_protocol";
        "test_range_checks";
        "test_utils";
      ]
    ~bisect_ppx:No
    ~deps:
      [
        bls12_381_archive;
        octez_plonk_test_helpers;
        qcheck_alcotest;
        octez_bls12_381_polynomial |> open_;
      ]

let _octez_plonk_distribution_test =
  private_exe
    "main"
    ~path:"src/lib_distributed_plonk/distribution/test"
    ~opam:"octez-libs"
    ~deps:[bls12_381_archive; octez_plonk_aggregation; octez_plonk_test_helpers]
    ~modules:["main"; "test_polynomial_commitment"]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-libs")

let _octez_plonk_test_helpers_bench =
  private_exe
    "bench"
    ~path:"src/lib_plonk/test"
    ~opam:"octez-libs"
    ~modules:["bench"]
    ~bisect_ppx:No
    ~deps:[bls12_381_archive; octez_plonk_test_helpers]

let _octez_plonk_test_plompiler_afl =
  private_exe
    "afl"
    ~path:"src/lib_plonk/test_plompiler"
    ~opam:"octez-libs"
    ~modules:["afl"]
    ~bisect_ppx:No
    ~deps:[bls12_381_archive; octez_plompiler; octez_plonk; bls12_381]

let _octez_plonk_test_plompiler_main =
  private_exe
    "main"
    ~path:"src/lib_plonk/test_plompiler"
    ~opam:"octez-libs"
    ~modules:
      [
        "bench_poseidon";
        "benchmark";
        "main";
        "test_anemoi";
        "test_blake";
        "test_sha2";
        "test_core";
        "test_edwards";
        "test_encoding";
        "test_enum";
        "test_input_com";
        "test_linear_algebra";
        "test_lookup";
        "test_merkle";
        "test_merkle_narity";
        "test_mod_arith";
        "test_optimizer";
        "test_poseidon";
        "test_range_checks";
        "test_schnorr";
        "test_ed25519";
        "test_edwards25519";
        "test_serialization";
        "test_weierstrass";
        "test_utils";
      ]
    ~bisect_ppx:No
    ~deps:[bls12_381_archive; octez_plonk_test_helpers]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-libs")

let octez_distributed_plonk =
  octez_lib
    "distributed-plonk"
    ~internal_name:"distributed_plonk"
    ~path:"src/lib_distributed_plonk"
    ~deps:
      [
        octez_aplonk;
        octez_plonk_communication;
        octez_plonk |> open_;
        octez_plonk_test_helpers;
      ]
    ~modules:
      [
        "distributed_prover";
        "filenames";
        "master_runner";
        "distribution_helpers";
        "worker";
      ]
    ~preprocess:(pps ppx_repr)
    ~bisect_ppx:Yes

let _octez_distributed_plonk_test_main =
  test
    "test_distribution"
    (* This test is disabled in the CI since the test is flaky and
       development of distributed plonk is on hiatus. As the
       dependencies of distributed plonk have significant load-time,
       we do not integrate it in the main tezt entrypoint using the
       [tezt] function. *)
    ~enabled_if:not_in_ci
    ~opam:"octez-libs"
    ~path:"src/lib_distributed_plonk/test"
    ~deps:
      [
        bls12_381_archive;
        octez_distributed_plonk;
        octez_plonk;
        octez_plonk_aggregation;
        octez_plonk_distribution;
        octez_aplonk;
        octez_plonk_test_helpers;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _octez_distributed_plonk_worker_runner =
  private_exe
    "worker_runner"
    ~path:"src/lib_distributed_plonk"
    ~opam:"octez-libs"
    ~bisect_ppx:No
    ~deps:[bls12_381_archive; octez_distributed_plonk; octez_plonk_distribution]
    ~modules:["worker_runner"]

let _octez_aplonk_test_main =
  private_exe
    "main"
    ~path:"src/lib_aplonk/test"
    ~opam:"octez-libs"
    ~deps:[bls12_381_archive; octez_plonk_test_helpers; octez_aplonk]
    ~modules:["main"; "test_aplonk"; "test_main_protocol"]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-libs")

let _octez_distributed_plonk_executable =
  private_exe
    "distribution"
    ~path:"src/lib_distributed_plonk"
    ~opam:"octez-libs"
    ~bisect_ppx:No
    ~deps:[bls12_381_archive; octez_distributed_plonk |> open_]
    ~modules:["distribution"]

let _octez_distributed_plonk_executable_meta =
  private_exe
    "distribution_meta"
    ~path:"src/lib_distributed_plonk"
    ~opam:"octez-libs"
    ~bisect_ppx:No
    ~deps:[bls12_381_archive; octez_distributed_plonk |> open_]
    ~modules:["distribution_meta"]

let _octez_aplonk_test_helpers_bench =
  private_exe
    "bench"
    ~path:"src/lib_aplonk/test"
    ~opam:"octez-libs"
    ~modules:["bench"]
    ~bisect_ppx:No
    ~deps:[bls12_381_archive; octez_plonk_test_helpers; octez_aplonk]

let octez_epoxy_tx =
  octez_lib
    "epoxy-tx"
    ~path:"src/lib_epoxy_tx"
    ~internal_name:"epoxy_tx"
    ~deps:[octez_plompiler; hex; stdint; octez_plonk; octez_mec]

let _octez_epoxy_tx_tests =
  private_exe
    "main"
    ~path:"src/lib_epoxy_tx/test"
    ~opam:"octez-libs"
    ~deps:
      [
        bls12_381_archive; octez_epoxy_tx; octez_plonk_test_helpers; octez_aplonk;
      ]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-libs")

let octez_stdlib_unix =
  octez_lib
    "stdlib-unix"
    ~internal_name:"tezos_stdlib_unix"
    ~path:"src/lib_stdlib_unix"
    ~synopsis:
      "Yet-another local-extension of the OCaml standard library \
       (unix-specific fragment)"
    ~deps:
      [
        unix;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib;
        octez_event_logging |> open_;
        octez_stdlib |> open_;
        data_encoding |> open_;
        aches_lwt;
        lwt_unix;
        lwt_exit;
        ipaddr_unix;
        re;
        ezjsonm;
        ptime;
        ptime_clock_os;
        mtime;
        mtime_clock_os;
        conf_libev;
        uri;
        progress;
        camlp_streams;
        lwt_domain;
      ]

let _octez_stdlib_unix_test =
  tezt
    [
      "test_key_value_store";
      "test_key_value_store_fuzzy";
      "test_log_config_rules";
    ]
    ~path:"src/lib_stdlib_unix/test/"
    ~opam:"octez-libs"
    ~preprocess:(ppses [ppx_hash])
    ~deps:
      [
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_stdlib_unix |> open_;
        octez_event_logging |> open_;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
        tezt_bam;
      ]

let octez_dal_config =
  octez_lib
    "crypto-dal.dal-config"
    ~internal_name:"tezos_crypto_dal_octez_dal_config"
    ~path:"src/lib_crypto_dal/dal_config"
    ~deps:[data_encoding |> open_]

let ppx_irmin =
  octez_internal_lib
    "ppx_irmin"
    ~path:"irmin/lib_ppx_irmin"
    ~deps:[ppx_repr_lib]
    ~ppx_kind:Ppx_deriver

let ppx_irmin_internal_lib =
  octez_internal_lib
    "ppx_irmin.internal_lib"
    ~path:"irmin/lib_ppx_irmin/internal"
    ~modules:["ppx_irmin_internal_lib"]
    ~deps:[logs]

let ppx_irmin_internal =
  octez_internal_lib
    "ppx_irmin.internal"
    ~path:"irmin/lib_ppx_irmin/internal"
    ~modules:["ppx_irmin_internal"]
    ~deps:[ppxlib; ppx_irmin_internal_lib; ppx_irmin]
    ~ppx_kind:Ppx_rewriter
    ~ppx_runtime_libraries:[logs; ppx_irmin_internal_lib]
    ~preprocess:(pps ppxlib_metaquot)

let irmin_data =
  octez_internal_lib
    "irmin.data"
    ~path:"irmin/lib_irmin/data"
    ~deps:[bigstringaf; fmt]

let irmin =
  octez_internal_lib
    "irmin"
    ~path:"irmin/lib_irmin"
    ~deps:
      [
        irmin_data;
        astring;
        bheap;
        digestif;
        fmt;
        jsonm;
        logs;
        logs_fmt;
        lwt;
        mtime;
        ocamlgraph;
        uri;
        uutf;
        re_export repr;
      ]
    ~preprocess:(pps ~args:["--"; "--lib"; "Type"] ppx_irmin_internal)

let irmin_mem =
  octez_internal_lib
    "irmin.mem"
    ~path:"irmin/lib_irmin/mem"
    ~deps:[irmin; logs; lwt]
    ~preprocess:(pps ppx_irmin_internal)
    ~flags:(Flags.standard ~disable_warnings:[68] ())

let irmin_pack =
  octez_internal_lib
    "irmin_pack"
    ~path:"irmin/lib_irmin_pack"
    ~deps:[fmt; irmin; irmin_data; logs; lwt; optint]
    ~preprocess:(pps ppx_irmin_internal)
    ~flags:(Flags.standard ~disable_warnings:[66] ())

let irmin_pack_mem =
  octez_internal_lib
    "irmin_pack.mem"
    ~path:"irmin/lib_irmin_pack/mem"
    ~deps:[irmin_pack; irmin_mem]
    ~preprocess:(pps ppx_irmin_internal)

let irmin_pack_unix =
  octez_internal_lib
    "irmin_pack.unix"
    ~path:"irmin/lib_irmin_pack/unix"
    ~deps:
      [
        fmt;
        index;
        index_unix;
        irmin;
        irmin_pack;
        logs;
        lwt;
        lwt_unix;
        mtime;
        cmdliner;
        optint;
        checkseum;
        checkseum_ocaml;
        rusage;
      ]
    ~preprocess:(pps ppx_irmin_internal)
    ~flags:(Flags.standard ~disable_warnings:[66; 68] ())

let irmin_tezos =
  octez_internal_lib
    "irmin_tezos"
    ~path:"irmin/lib_irmin_tezos"
    ~deps:[fmt; zarith; digestif; irmin; irmin_pack; irmin_pack_unix]
    ~preprocess:(pps ppx_irmin_internal)

let irmin_test_helpers =
  octez_internal_lib
    "irmin_test_helpers"
    ~path:"irmin/test/helpers"
    ~deps:
      [alcotezt; astring; fmt; irmin; jsonm; logs; lwt; mtime; mtime_clock_os]
    ~preprocess:(pps ppx_irmin_internal)
    ~flags:(Flags.standard ~disable_warnings:[66; 68] ())

let octez_clic =
  octez_lib
    "clic"
    ~internal_name:"tezos_clic"
    ~path:"src/lib_clic"
    ~deps:
      [
        octez_stdlib |> open_;
        lwt;
        re;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib;
        terminal;
      ]

let octez_clic_unix =
  octez_lib
    "clic.unix"
    ~internal_name:"tezos_clic_unix"
    ~path:"src/lib_clic/unix"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_clic |> open_;
        octez_stdlib_unix;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib;
      ]

let _octez_clic_tests =
  tezt
    ["test_clic"]
    ~path:"src/lib_clic/test"
    ~opam:"octez-libs"
    ~deps:[octez_stdlib |> open_; octez_clic |> open_; lwt_unix; alcotezt]

let _octez_clic_example =
  private_exe
    "clic_example"
    ~path:"src/lib_clic/examples"
    ~opam:""
    ~deps:[octez_clic; lwt_unix]
    ~bisect_ppx:No
    ~static:false

let octez_micheline =
  octez_lib
    "micheline"
    ~internal_name:"tezos_micheline"
    ~path:"src/lib_micheline"
    ~synopsis:"Internal AST and parser for the Michelson language"
    ~deps:
      [
        uutf;
        zarith;
        octez_stdlib |> open_;
        octez_error_monad |> open_;
        data_encoding |> open_;
      ]
    ~inline_tests:ppx_expect

let _octez_micheline_tests =
  private_lib
    "test_parser"
    ~path:"src/lib_micheline/test"
    ~opam:"octez-libs"
    ~inline_tests:ppx_expect
    ~modules:["test_parser"]
    ~deps:[octez_micheline |> open_]

let _octez_micheline_tests =
  private_lib
    "test_diff"
    ~path:"src/lib_micheline/test"
    ~opam:"octez-libs"
    ~inline_tests:ppx_expect
    ~modules:["test_diff"]
    ~deps:[octez_micheline |> open_]

let octez_version_parser =
  octez_lib
    "version.parser"
    ~internal_name:"tezos_version_parser"
    ~path:"src/lib_version/parser"
    ~dune:Dune.[ocamllex "tezos_version_parser"]
    ~preprocess:(pps ppx_deriving_show)

let octez_version =
  octez_lib
    "version"
    ~internal_name:"tezos_version"
    ~path:"src/lib_version"
    ~synopsis:"Version information generated from Git"
    ~deps:
      [
        data_encoding;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_version_parser;
      ]

let octez_version_value =
  public_lib
    "octez-version.value"
    ~internal_name:"tezos_version_value"
    ~path:"src/lib_version/value/"
    ~synopsis:"Tezos: version value generated from Git"
    ~deps:[octez_version; octez_version_parser]
      (* We want generated_git_info.cmi to be compiled with -opaque so
         that a change in the implementation doesn't force rebuilding all
         the reverse dependencies. *)
    ~flags:(Flags.standard ~opaque:true ())
    ~dune:
      Dune.
        [
          (* Ensures the hash updates whenever a source file is modified. *)
          targets_rule
            ["generated_git_info.ml"]
            ~deps:[[S "universe"]]
            ~action:[S "run"; S "../exe/get_git_info.exe"];
        ]

let _octez_version_get_git_info =
  private_exe
    "get_git_info"
    ~path:"src/lib_version/exe"
    ~opam:"octez-version"
    ~deps:[bls12_381_archive; dune_configurator; octez_version_parser]
    ~modules:["get_git_info"]
    ~bisect_ppx:No

let octez_print_version =
  public_lib
    "octez-version.print"
    ~path:"src/lib_version/print"
    ~deps:[octez_version |> open_; octez_version_value |> open_]

let _octez_print_version_exe =
  public_exe
    "octez-version"
    ~internal_name:"octez_print_version"
    ~path:"src/lib_version/exe"
    ~opam:"octez-version"
    ~deps:[octez_version_value |> open_; octez_print_version |> open_]
    ~modules:["octez_print_version"]
    ~bisect_ppx:No

let octez_profiler =
  octez_lib
    "octez-profiler"
    ~internal_name:"tezos_profiler"
    ~path:"src/lib_profiler"
    ~synopsis:"The Octez Profiler"
    ~deps:[data_encoding; lwt]

let octez_base =
  octez_lib
    "base"
    ~internal_name:"tezos_base"
    ~path:"src/lib_base"
    ~synopsis:"Meta-package and pervasive type definitions for Tezos"
    ~deps:
      [
        octez_version;
        octez_stdlib |> open_;
        octez_crypto;
        data_encoding |> open_;
        octez_error_monad_legacy |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_rpc;
        octez_micheline |> open_;
        octez_event_logging |> open_;
        ptime;
        ptime_clock_os;
        mtime;
        ezjsonm;
        lwt;
        ipaddr;
        uri;
      ]
    ~documentation:[Dune.[S "package"; S "octez-libs"]]
    ~dune:Dune.[ocamllex "point_parser"]
    ~ocaml:
      V.(
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6112
           Should be in sync with scripts/version.sh *)
        at_least "5.3.0" && less_than "5.4.0")
    ~license:"Apache-2.0"

let octez_base_unix =
  octez_lib
    "base.unix"
    ~internal_name:"tezos_base_unix"
    ~path:"src/lib_base/unix"
    ~foreign_stubs:
      {
        language = C;
        flags = [S ":standard"];
        include_dirs = [];
        names = ["socket"];
      }
    ~deps:
      [
        octez_error_monad |> open_;
        octez_crypto;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_hacl;
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        octez_profiler |> open_;
        data_encoding |> open_;
        uri;
        octez_event_logging |> open_;
        lwt_exit;
        terminal;
        eio;
        eio_posix;
        lwt_eio;
      ]
    ~inline_tests:ppx_expect
    ~inline_tests_libraries:[bls12_381_archive]
    ~inline_tests_link_flags:["-cclib"; "-lblst"]

(* Alcotezt process isolation library - defined here after octez_base/octez_base_unix
   to avoid dependency cycles. Tests that need process isolation with tzresult support
   should depend on this library. *)
let alcotezt_process =
  octez_lib
    "alcotezt-process"
    ~path:"tezt/lib_alcotezt_process"
    ~synopsis:
      "Process-isolated test execution for Alcotezt with Tezos error monad \
       support"
    ~deps:
      [
        tezt_core_lib;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_error_monad |> open_;
        lwt_unix;
        lwt_eio;
      ]

let octez_lib_upnp =
  public_lib
    "octez-lib-upnp"
    ~path:"src/lib_upnp"
    ~synopsis:"UPNP support"
    ~deps:
      [
        octez_rust_igd_next |> open_;
        octez_stdlib |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib |> open_;
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
      ]

let octez_lib_upnp_args =
  public_lib
    "octez-lib-upnp-args"
    ~path:"src/lib_upnp/args"
    ~synopsis:"UPNP support: Cmdliner shared args"
    ~deps:[cmdliner]

let octez_base_p2p_identity_file =
  octez_lib
    "base.p2p-identity-file"
    ~internal_name:"tezos_base_p2p_identity_file"
    ~path:"src/lib_base/p2p_identity_file"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_stdlib_unix |> open_]

let _octez_base_tests =
  tezt
    [
      "test_bounded";
      "test_time";
      "test_protocol";
      "test_p2p_addr";
      "test_sized";
      "test_skip_list";
      "test_bitset";
    ]
    ~path:"src/lib_base/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_base |> open_;
        octez_error_monad |> open_;
        data_encoding;
        octez_test_helpers |> open_;
        qcheck_tezt;
        qcheck_alcotest;
        alcotezt;
      ]
    ~dep_files:
      [
        (* Note: those files are only actually needed by test_p2p_addr. *)
        "points.ok";
        "points.ko";
      ]

let _octez_base_unix_tests =
  tezt
    [
      "test_unix_error";
      "test_syslog";
      "test_event_loop";
      "test_external_process";
    ]
    ~path:"src/lib_base/unix/test"
    ~with_macos_security_framework:true
    ~opam:"octez-libs"
    ~modes:[Native]
    ~deps:
      [
        octez_base |> open_;
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_error_monad |> open_;
        data_encoding;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
        tezt_lib;
        eio;
      ]

let octez_base_test_helpers =
  octez_lib
    "base-test-helpers"
    ~internal_name:"tezos_base_test_helpers"
    ~path:"src/lib_base/test_helpers"
    ~synopsis:"Octez base test helpers"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix;
        octez_event_logging_test_helpers;
        octez_test_helpers |> open_;
        alcotezt;
        qcheck_alcotest;
        tezt_core_lib;
      ]
    ~linkall:true
    ~bisect_ppx:No
    ~release_status:Released

let ppx_profiler =
  octez_lib
    "ppx_profiler"
    ~path:"src/lib_ppx_profiler"
    ~deps:[ppxlib]
    ~ppx_kind:Ppx_rewriter
    ~ppx_runtime_libraries:[logs]
    ~preprocess:(pps ppxlib_metaquot)

let ppx_profiler =
  make_ppx ~env_var:"TEZOS_PPX_PROFILER" ~preprocess:ppx_profiler

let octez_profiler_unix =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_lib
    "octez-profiler.unix"
    ~internal_name:"tezos_profiler_unix"
    ~path:"src/lib_profiler/unix"
    ~synopsis:"The Octez Profiler"
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib |> open_;
        octez_stdlib_unix;
        octez_profiler |> open_;
        data_encoding |> open_;
        octez_event_logging;
        octez_error_monad |> open_;
      ]

let octez_profiler_backends =
  octez_lib
    "octez-profiler.backends"
    ~internal_name:"tezos_profiler_backends"
    ~path:"src/lib_profiler/backends"
    ~synopsis:"Backends for the Octez Profiler"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix |> open_;
        octez_profiler |> open_;
        octez_profiler_unix |> open_;
        octez_stdlib |> open_;
        data_encoding |> open_;
      ]

let _octez_profiler_backends_tests =
  tezt
    ["test_simple_profiler"]
    ~path:"src/lib_profiler/backends/test"
    ~with_macos_security_framework:true
    ~opam:"octez-libs"
    ~modes:[Native]
    ~deps:
      [
        octez_base |> open_;
        octez_base_unix |> open_;
        octez_profiler |> open_;
        octez_profiler_backends |> open_;
        octez_stdlib_unix |> open_;
        octez_error_monad |> open_;
        data_encoding;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
        tezt_lib;
      ]

let octez_profiler_complex_backends =
  octez_lib
    "octez-profiler.complex_backends"
    ~internal_name:"tezos_profiler_complex_backends"
    ~path:"src/lib_profiler/backends/complex"
    ~synopsis:
      "Complex backends for the Octez Profiler that need more dependencies"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix |> open_;
        octez_profiler |> open_;
        octez_profiler_unix |> open_;
        octez_profiler_backends |> open_;
        octez_stdlib |> open_;
        data_encoding |> open_;
        prometheus;
        opentelemetry;
        ambient_context_lwt;
        opentelemetry_client_cohttp_lwt;
      ]

let octez_crypto_dal =
  octez_lib
    "crypto-dal"
    ~internal_name:"tezos_crypto_dal"
    ~path:"src/lib_crypto_dal"
    ~synopsis:"DAL cryptographic primitives"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        octez_error_monad |> open_;
        data_encoding |> open_;
        octez_dal_config |> open_;
        octez_bls12_381_polynomial;
        cohttp_lwt_unix;
        octez_lwt_result_stdlib;
        lwt_unix;
        octez_kzg;
        octez_event_logging;
        octez_base;
      ]

let _octez_crypto_dal_tests =
  tezt
    ["test_dal_cryptobox"; "test_trap"]
    ~path:"src/lib_crypto_dal/test"
    ~opam:"octez-libs"
    ~dep_files:["srs_zcash_g1_5"; "srs_zcash_g2_5"]
    ~deps:
      [
        octez_stdlib |> open_;
        octez_crypto_dal |> open_;
        octez_dal_config |> open_;
        octez_error_monad |> open_;
        data_encoding |> open_;
        alcotezt;
        qcheck_alcotest;
        octez_bls12_381_polynomial;
        octez_base_test_helpers |> open_;
        octez_base |> open_ ~m:"TzPervasives";
      ]

let ppx_brassaia =
  octez_lib
    "ppx_brassaia"
    ~path:"brassaia/lib_ppx_brassaia"
    ~deps:[ppx_repr_lib]
    ~ppx_kind:Ppx_deriver

let ppx_brassaia_internal_lib =
  octez_lib
    "ppx_brassaia.internal_lib"
    ~path:"brassaia/lib_ppx_brassaia/internal"
    ~modules:["ppx_brassaia_internal_lib"]
    ~deps:[logs]

let ppx_brassaia_internal =
  octez_lib
    "ppx_brassaia.internal"
    ~path:"brassaia/lib_ppx_brassaia/internal"
    ~modules:["ppx_brassaia_internal"]
    ~deps:[ppxlib; ppx_brassaia_internal_lib; ppx_brassaia]
    ~ppx_kind:Ppx_rewriter
    ~ppx_runtime_libraries:[logs; ppx_brassaia_internal_lib]
    ~preprocess:(pps ppxlib_metaquot)

let brassaia_data =
  octez_lib
    "brassaia.data"
    ~path:"brassaia/lib_brassaia/data"
    ~deps:[bigstringaf; fmt]

let brassaia =
  octez_lib
    "brassaia"
    ~path:"brassaia/lib_brassaia"
    ~deps:
      [
        octez_event_logging |> open_;
        data_encoding;
        brassaia_data;
        astring;
        bheap;
        digestif;
        fmt;
        jsonm;
        logs;
        logs_fmt;
        lwt;
        mtime;
        ocamlgraph;
        uri;
        uutf;
        re_export repr;
      ]
    ~preprocess:(pps ~args:["--"; "--lib"; "Type"] ppx_brassaia_internal)

let brassaia_mem =
  octez_lib
    "brassaia.mem"
    ~path:"brassaia/lib_brassaia/mem"
    ~deps:[brassaia; logs; lwt]
    ~preprocess:(pps ppx_brassaia_internal)
    ~flags:(Flags.standard ~disable_warnings:[68] ())

let brassaia_pack =
  octez_lib
    "brassaia_pack"
    ~path:"brassaia/lib_brassaia_pack"
    ~deps:
      [
        octez_event_logging |> open_;
        data_encoding;
        fmt;
        brassaia;
        brassaia_data;
        logs;
        lwt;
        optint;
      ]
    ~preprocess:(pps ppx_brassaia_internal)
    ~flags:(Flags.standard ~disable_warnings:[66] ())

let brassaia_pack_mem =
  octez_lib
    "brassaia_pack.mem"
    ~path:"brassaia/lib_brassaia_pack/mem"
    ~deps:[brassaia_pack; brassaia_mem; brassaia]
    ~preprocess:(pps ppx_brassaia_internal)

let brassaia_index =
  octez_lib
    "brassaia.index"
    ~path:"brassaia/index/src/"
    ~deps:
      [
        optint;
        mtime;
        fmt;
        fmt_cli;
        logs;
        logs_cli;
        rusage;
        lwt;
        progress;
        cmdliner;
        repr;
        lru;
      ]
    ~preprocess:(pps ppx_repr)
    ~flags:(Flags.standard ~disable_warnings:[66; 68] ())

let brassaia_index_unix =
  octez_lib
    "brassaia.index.unix"
    ~path:"brassaia/index/src/unix"
    ~deps:
      [
        optint;
        mtime;
        rusage;
        fmt_tty;
        lwt;
        lwt_unix;
        brassaia_index;
        semaphore_compat;
      ]
    ~flags:(Flags.standard ~disable_warnings:[66; 68] ())
    ~foreign_stubs:
      {language = C; flags = []; include_dirs = []; names = ["pread"; "pwrite"]}

let brassaia_pack_unix =
  octez_lib
    "brassaia_pack.unix"
    ~path:"brassaia/lib_brassaia_pack/unix"
    ~deps:
      [
        octez_event_logging |> open_;
        fmt;
        brassaia_index;
        brassaia_index_unix;
        brassaia;
        brassaia_pack;
        logs;
        lwt;
        lwt_unix;
        mtime;
        cmdliner;
        optint;
        checkseum;
        checkseum_ocaml;
        rusage;
      ]
    ~preprocess:(pps ppx_brassaia_internal)
    ~flags:(Flags.standard ~disable_warnings:[66; 68] ())

let brassaia_tezos =
  octez_lib
    "brassaia_tezos"
    ~path:"brassaia/lib_brassaia_tezos"
    ~deps:[fmt; zarith; digestif; brassaia; brassaia_pack; brassaia_pack_unix]
    ~preprocess:(pps ppx_brassaia_internal)

let brassaia_test_helpers =
  octez_lib
    "brassaia_test_helpers"
    ~path:"brassaia/test/helpers"
    ~deps:
      [
        alcotezt; astring; fmt; brassaia; jsonm; logs; lwt; mtime; mtime_clock_os;
      ]
    ~preprocess:(pps ppx_brassaia_internal)
    ~flags:(Flags.standard ~disable_warnings:[66; 68] ())

let ppx_brassaia_eio =
  octez_lib
    "ppx_brassaia_eio"
    ~path:"brassaia-eio/lib_ppx_brassaia"
    ~deps:[ppx_repr_lib]
    ~ppx_kind:Ppx_deriver

let ppx_brassaia_eio_internal_lib =
  octez_lib
    "ppx_brassaia_eio.internal_lib"
    ~path:"brassaia-eio/lib_ppx_brassaia/internal"
    ~modules:["ppx_brassaia_internal_lib"]
    ~deps:[logs]

let ppx_brassaia_eio_internal =
  octez_lib
    "ppx_brassaia_eio.internal"
    ~path:"brassaia-eio/lib_ppx_brassaia/internal"
    ~modules:["ppx_brassaia_internal"]
    ~deps:[ppxlib; ppx_brassaia_eio_internal_lib; ppx_brassaia_eio]
    ~ppx_kind:Ppx_rewriter
    ~ppx_runtime_libraries:[logs; ppx_brassaia_eio_internal_lib]
    ~preprocess:(pps ppxlib_metaquot)

let brassaia_eio_data =
  octez_lib
    "brassaia_eio.data"
    ~path:"brassaia-eio/lib_brassaia/data"
    ~deps:[bigstringaf; fmt]

let brassaia_eio =
  octez_lib
    "brassaia_eio"
    ~path:"brassaia-eio/lib_brassaia"
    ~deps:
      [
        octez_event_logging |> open_;
        data_encoding;
        brassaia_eio_data;
        astring;
        bheap;
        digestif;
        fmt;
        jsonm;
        logs;
        logs_fmt;
        eio;
        mtime;
        ocamlgraph;
        uri;
        uutf;
        re_export repr;
      ]
    ~preprocess:(pps ~args:["--"; "--lib"; "Type"] ppx_brassaia_eio_internal)
    ~flags:(Flags.standard ~disable_warnings:[66] ())

let brassaia_eio_mem =
  octez_lib
    "brassaia_eio.mem"
    ~path:"brassaia-eio/lib_brassaia/mem"
    ~deps:[brassaia_eio; logs; eio]
    ~preprocess:(pps ppx_brassaia_eio_internal)
    ~flags:(Flags.standard ~disable_warnings:[68] ())

let brassaia_eio_pack =
  octez_lib
    "brassaia_eio_pack"
    ~path:"brassaia-eio/lib_brassaia_pack"
    ~deps:
      [
        octez_event_logging |> open_;
        data_encoding;
        fmt;
        brassaia_eio;
        brassaia_eio_data;
        logs;
        eio;
        optint;
      ]
    ~preprocess:(pps ppx_brassaia_eio_internal)
    ~flags:(Flags.standard ~disable_warnings:[66] ())

let brassaia_eio_pack_mem =
  octez_lib
    "brassaia_eio_pack.mem"
    ~path:"brassaia-eio/lib_brassaia_pack/mem"
    ~deps:[brassaia_eio_pack; brassaia_eio_mem; brassaia_eio]
    ~preprocess:(pps ppx_brassaia_eio_internal)

let brassaia_eio_pack_io =
  octez_lib
    "brassaia_eio_pack.io"
    ~path:"brassaia-eio/lib_brassaia_pack/io"
    ~deps:
      [
        octez_event_logging |> open_;
        fmt;
        brassaia_index;
        brassaia_index_unix;
        brassaia_eio;
        brassaia_eio_pack;
        logs;
        eio;
        mtime;
        cmdliner;
        optint;
        checkseum;
        checkseum_ocaml;
        rusage;
      ]
    ~preprocess:(pps ppx_brassaia_eio_internal)
    ~flags:(Flags.standard ~disable_warnings:[66; 68] ())

let brassaia_eio_tezos =
  octez_lib
    "brassaia_eio_tezos"
    ~path:"brassaia-eio/lib_brassaia_tezos"
    ~deps:
      [
        fmt;
        zarith;
        digestif;
        brassaia_eio;
        brassaia_eio_pack;
        brassaia_eio_pack_io;
      ]
    ~preprocess:(pps ppx_brassaia_eio_internal)

let brassaia_eio_test_helpers =
  octez_lib
    "brassaia_eio_test_helpers"
    ~path:"brassaia-eio/test/helpers"
    ~deps:
      [
        alcotezt;
        astring;
        fmt;
        brassaia_eio;
        octez_base_unix |> open_;
        jsonm;
        logs;
        eio;
        mtime;
        mtime_clock_os;
      ]
    ~preprocess:(pps ppx_brassaia_eio_internal)
    ~flags:(Flags.standard ~disable_warnings:[66; 68] ())

let octez_context_sigs =
  octez_lib
    "context.sigs"
    ~internal_name:"tezos_context_sigs"
    ~path:"src/lib_context/sigs"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_stdlib |> open_]

let tree_encoding =
  octez_lib
    "tree-encoding"
    ~internal_name:"tezos_tree_encoding"
    ~path:"src/lib_tree_encoding"
    ~synopsis:
      "A general-purpose library to encode arbitrary data in Merkle trees"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_context_sigs;
        octez_lwt_result_stdlib;
        data_encoding;
      ]

let lazy_containers =
  octez_lib
    "lazy-containers"
    ~internal_name:"tezos_lazy_containers"
    ~path:"src/lib_lazy_containers"
    ~synopsis:
      "A collection of lazy containers whose contents is fetched from \
       arbitrary backend on-demand"
    ~deps:[zarith; tree_encoding]

let _lazy_containers_tests =
  tezt
    ["chunked_byte_vector_tests"; "lazy_vector_tests"]
    ~path:"src/lib_lazy_containers/test"
    ~opam:"tezos-lazy-containers-tests"
    ~synopsis:"Various tests for the lazy containers library"
    ~deps:
      [
        lazy_containers |> open_;
        qcheck_core;
        qcheck_alcotest;
        lwt_unix;
        alcotezt;
      ]

let octez_webassembly_interpreter =
  octez_lib
    "webassembly-interpreter"
    ~internal_name:"tezos_webassembly_interpreter"
    ~path:"src/lib_webassembly"
    ~dune:Dune.[[S "include_subdirs"; S "unqualified"]]
    ~deps:
      [
        octez_lwt_result_stdlib;
        octez_stdlib;
        octez_error_monad;
        zarith;
        lazy_containers |> open_;
      ]
    ~preprocess:(pps ppx_deriving_show)

let octez_webassembly_interpreter_extra =
  octez_l2_lib
    "webassembly-interpreter-extra"
    ~internal_name:"tezos_webassembly_interpreter_extra"
    ~path:"src/lib_webassembly/extra"
    ~license:"Apache-2.0"
    ~extra_authors:["WebAssembly Authors"]
    ~synopsis:"Additional modules from the WebAssembly REPL used in testing"
    ~dune:
      Dune.[[S "include_subdirs"; S "unqualified"]; [S "include"; S "dune.inc"]]
    ~deps:
      [
        octez_webassembly_interpreter |> open_;
        lwt_unix;
        lazy_containers |> open_;
      ]

let _octez_webassembly_repl =
  private_exe
    "main"
    ~path:"src/lib_webassembly/bin"
    ~opam:""
    ~dune:Dune.[[S "include"; S "dune.inc"]]
    ~deps:
      [
        bls12_381_archive;
        octez_webassembly_interpreter |> open_;
        octez_webassembly_interpreter_extra |> open_;
        lwt_unix;
        tree_encoding |> open_;
        lazy_containers |> open_;
      ]

let _octez_webassembly_test =
  tezt
    ["smallint"]
    ~path:"src/lib_webassembly/tests"
    ~opam:"octez-l2-libs"
    ~dune:Dune.[[S "include_subdirs"; S "no"]]
    ~deps:[octez_webassembly_interpreter |> open_; alcotezt]

let _octez_evm_node_print_version_exe =
  public_exe
    "octez-evm-node-version"
    ~internal_name:"octez_evm_node_print_version"
    ~path:"src/lib_version/exe"
    ~opam:"octez-version"
    ~deps:
      [
        bls12_381_archive;
        octez_version_value |> open_;
        octez_version |> open_;
        octez_base_unix;
        octez_print_version |> open_;
      ]
    ~modules:["octez_evm_node_print_version"]
    ~bisect_ppx:No

let _octez_version_tests =
  tezt
    ["test_parser"; "test_octez_node_version"]
    ~path:"src/lib_version/test"
    ~opam:"octez-libs"
    ~deps:[octez_version |> open_; octez_version_parser; qcheck_tezt]

let octez_p2p_services =
  octez_lib
    "tezos-p2p-services"
    ~path:"src/lib_p2p_services"
    ~synopsis:"Descriptions of RPCs exported by [tezos-p2p]"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_rpc]
    ~linkall:true

let octez_workers =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_lib
    "tezos-workers"
    ~path:"src/lib_workers"
    ~synopsis:"Worker library"
    ~documentation:
      Dune.[[S "package"; S "octez-libs"]; [S "mld_files"; S "tezos_workers"]]
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_stdlib_unix |> open_;
        octez_profiler_complex_backends |> open_;
        opentelemetry;
      ]

let _octez_workers_tests =
  tezt
    ["mocked_worker"; "test_workers_unit"]
    ~path:"src/lib_workers/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives"
        |> open_ ~m:"Worker_types";
        octez_workers |> open_;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        alcotezt;
      ]

let octez_bees =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_lib
    "tezos-bees"
    ~path:"src/lib_bees"
    ~synopsis:"Parallel worker library"
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_stdlib_unix |> open_;
        octez_profiler_backends;
        octez_profiler_complex_backends;
        saturn;
      ]

let _octez_bees_tests =
  tezt
    [
      "mocked_worker";
      "test_bees_unit";
      "test_bees_unit_eio";
      "test_bees_task_worker";
      "test_bees_lwt_async";
    ]
    ~path:"src/lib_bees/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives"
        |> open_ ~m:"Worker_types";
        octez_bees |> open_;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        alcotezt;
        alcotezt_process;
      ]

let octez_merkle_proof_encoding =
  octez_lib
    "tezos-context.merkle_proof_encoding"
    ~path:"src/lib_context/merkle_proof_encoding"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        octez_context_sigs;
      ]

let octez_brassaia_merkle_proof_encoding =
  octez_lib
    "tezos-context-brassaia.merkle_proof_encoding"
    ~path:"src/lib_context_brassaia/merkle_proof_encoding"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        octez_context_sigs;
      ]

let octez_shell_services =
  octez_shell_lib
    "shell-services"
    ~internal_name:"tezos_shell_services"
    ~path:"src/lib_shell_services"
    ~synopsis:"Descriptions of RPCs exported by [tezos-shell]"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_rpc;
        octez_p2p_services |> open_;
        octez_version |> open_;
        octez_context_sigs;
        octez_merkle_proof_encoding;
        octez_profiler |> open_;
        octez_dal_config |> open_;
      ]
    ~linkall:true

let _octez_shell_services_tests =
  tezt
    ["test_block_services"]
    ~path:"src/lib_shell_services/test"
    ~opam:"octez-shell-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_shell_services |> open_;
        octez_test_helpers;
      ]

let octez_p2p =
  octez_shell_lib
    "p2p"
    ~internal_name:"tezos_p2p"
    ~path:"src/lib_p2p"
    ~synopsis:"Library for a pool of P2P connections"
    ~deps:
      [
        lwt_watcher;
        lwt_canceler;
        ringo;
        aches;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_p2p_services |> open_;
        octez_version;
        octez_workers;
        prometheus;
        octez_base_p2p_identity_file |> open_;
      ]

let tezt_performance_regression =
  public_lib
    "tezt-tezos.tezt-performance-regression"
    ~path:"tezt/lib_performance_regression"
    ~opam:"tezt-tezos"
    ~bisect_ppx:No
    ~deps:[tezt_wrapper |> open_ |> open_ ~m:"Base"; uri; cohttp_lwt_unix]

let tezt_cloud =
  public_lib
    "tezt-cloud"
    ~path:"tezt/lib_cloud"
    ~bisect_ppx:No
    ~synopsis:
      "Automatic of Cloud deployment for complex scenarios on top of Tezt"
    ~deps:
      [
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_performance_regression |> open_;
        jingoo;
        dream;
        data_encoding;
      ]

let tezt_tezos =
  public_lib
    "tezt-tezos"
    ~path:"tezt/lib_tezos"
    ~opam:"tezt-tezos"
    ~synopsis:"Octez test framework based on Tezt"
    ~bisect_ppx:No
    ~deps:
      [
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_performance_regression |> open_;
        uri;
        hex;
        octez_crypto_dal;
        octez_base;
        octez_base_unix;
        cohttp_lwt_unix;
        yaml;
      ]
    ~cram:true
    ~release_status:Released

let tezt_migration_registry =
  public_lib
    "tezt-migration-registry"
    ~path:"tezt/lib_migration_test_registry"
    ~opam:"tezt-migration-registry"
    ~synopsis:"Octez Protocol migration checks registration"
    ~bisect_ppx:No
    ~deps:
      [
        tezt_tezos;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        octez_base;
        octez_base_unix;
        cohttp_lwt_unix;
      ]
    ~cram:true
    ~release_status:Released

let octez_p2p_test_common =
  octez_shell_lib
    "p2p_test_common"
    ~internal_name:"tezos_p2p_test_common"
    ~path:"src/lib_p2p/test/common"
    ~deps:
      [
        tezt_lib |> open_ |> open_ ~m:"Base";
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_p2p |> open_;
        octez_p2p_services |> open_;
      ]

let _octez_p2p_tezt =
  tezt
    [
      "test_p2p_fd";
      "test_p2p_socket";
      "test_p2p_conn";
      "test_p2p_node";
      "test_p2p_pool";
    ]
    ~path:"src/lib_p2p/tezt"
    ~opam:"octez-shell-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_p2p |> open_;
        octez_p2p_services |> open_;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        octez_event_logging_test_helpers |> open_;
        octez_p2p_test_common |> open_;
      ]

let _octez_p2p_tests =
  tests
    [
      "test_p2p_socket";
      "test_p2p_broadcast";
      "test_p2p_io_scheduler";
      "test_p2p_peerset";
      "test_p2p_buffer_reader";
      "test_p2p_banned_peers";
      "test_p2p_connect_handler";
      "test_p2p_maintenance";
    ]
    ~bisect_ppx:With_sigterm
    ~path:"src/lib_p2p/test"
    ~opam:"octez-shell-libs"
    ~locks:"/locks/p2p"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_p2p |> open_;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        octez_event_logging_test_helpers |> open_;
        octez_p2p_test_common |> open_;
        octez_p2p_services |> open_;
        tezt_tezos;
        tezt_lib;
        alcotezt;
        astring;
      ]

let _tezt_self_tests =
  tezt
    ["test_michelson_script"; "test_daemon"]
    ~opam:"tezt-tezos"
    ~path:"tezt/self_tests"
    ~deps:[tezt_lib |> open_ |> open_ ~m:"Base"; tezt_tezos |> open_]

let octez_gossipsub =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_lib
    "tezos-gossipsub"
    ~path:"src/lib_gossipsub"
    ~deps:
      [
        ringo;
        aches;
        fmt;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_version;
        octez_profiler |> open_;
      ]
    ~preprocess
    ~preprocessor_deps

let _octez_gossipsub_test =
  tezt
    [
      "test_gossipsub";
      "test_integration_worker";
      "test_message_cache";
      "test_pbt";
      "test_unit";
      "test_gossipsub_shared";
      "gossipsub_pbt_generators";
      "default_limits";
    ]
    ~path:"src/lib_gossipsub/test"
    ~opam:"octez-libs"
    ~deps:
      [
        fmt;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_gossipsub |> open_;
        tezt_lib;
        qcheck_core;
        octez_test_helpers |> open_;
      ]

let octez_wasmer =
  octez_l2_lib
    "wasmer"
    ~internal_name:"tezos_wasmer"
    ~path:"src/lib_wasmer"
    ~synopsis:"Wasmer bindings for SCORU WASM"
    ~deps:[ctypes; ctypes_foreign; lwt; lwt_unix]
    ~link_deps:lib_wasmer_riscv
    ~dep_globs_rec:["../rust_deps/wasmer-3.3.0/*"]
    ~preprocess:(pps ppx_deriving_show)
    ~flags:(Flags.standard ~disable_warnings:[9; 27] ())
    ~ctypes:
      Ctypes.
        {
          external_library_name = "octez_rust_deps";
          include_header = "wasmer.h";
          extra_search_dir = "../rust_deps/wasmer-3.3.0/lib/c-api";
          type_description = {instance = "Types"; functor_ = "Api_types_desc"};
          function_description =
            {instance = "Functions"; functor_ = "Api_funcs_desc"};
          generated_types = "Api_types";
          generated_entry_point = "Api";
          c_flags = ["-Wno-incompatible-pointer-types"];
          c_library_flags = [];
          deps =
            [
              "../rust_deps/wasmer-3.3.0/lib/c-api/wasmer.h";
              "../rust_deps/wasmer-3.3.0/lib/c-api/wasm.h";
            ];
        }

let _octez_wasmer_test =
  tezt
    ["test_wasmer"]
    ~path:"src/lib_wasmer/test"
    ~opam:"octez-l2-libs"
    ~deps:[octez_wasmer; alcotezt]

let octez_context_encoding =
  octez_lib
    "tezos-context.encoding"
    ~path:"src/lib_context/encoding"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        irmin;
        irmin_pack;
      ]
    ~conflicts:[Conflicts.checkseum]

let octez_context_brassaia_encoding =
  octez_lib
    "tezos-context-brassaia.encoding"
    ~path:"src/lib_context_brassaia/encoding"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        brassaia;
        brassaia_pack;
      ]
    ~conflicts:[Conflicts.checkseum]

let octez_context_helpers =
  octez_lib
    "tezos-context.helpers"
    ~path:"src/lib_context/helpers"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        octez_context_encoding;
        octez_context_sigs;
        octez_merkle_proof_encoding;
        irmin;
        irmin_pack;
      ]
    ~conflicts:[Conflicts.checkseum]

let octez_context_brassaia_helpers =
  octez_lib
    "tezos-context-brassaia.helpers"
    ~path:"src/lib_context_brassaia/helpers"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        octez_context_brassaia_encoding;
        octez_context_sigs;
        octez_brassaia_merkle_proof_encoding;
        brassaia;
        brassaia_pack;
      ]
    ~conflicts:[Conflicts.checkseum]

(* Should not be used directly in most cases, use context_ops abstraction instead *)
let octez_context_memory =
  octez_lib
    "tezos-context.memory"
    ~path:"src/lib_context/memory"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        irmin_pack;
        irmin_pack_mem;
        brassaia_pack;
        brassaia_pack_mem;
        octez_context_sigs;
        octez_context_encoding;
        octez_context_helpers;
      ]
    ~conflicts:[Conflicts.checkseum]

(* Should not be used directly in most cases, use context_ops abstraction instead *)
let octez_context_brassaia_memory =
  octez_lib
    "tezos-context-brassaia.memory"
    ~path:"src/lib_context_brassaia/memory"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        brassaia_pack;
        brassaia_pack_mem;
        octez_context_sigs;
        octez_context_brassaia_encoding;
        octez_context_brassaia_helpers;
      ]
    ~conflicts:[Conflicts.checkseum]

let octez_scoru_wasm =
  octez_lib
    "scoru-wasm"
    ~internal_name:"tezos_scoru_wasm"
    ~path:"src/lib_scoru_wasm"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        lazy_containers;
        octez_webassembly_interpreter;
        octez_context_sigs;
        octez_context_memory;
        octez_lwt_result_stdlib;
        data_encoding;
      ]

let octez_scoru_wasm_fast =
  octez_l2_lib
    "scoru-wasm-fast"
    ~internal_name:"tezos_scoru_wasm_fast"
    ~path:"src/lib_scoru_wasm/fast"
    ~synopsis:"WASM functionality for SCORU Fast Execution"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_webassembly_interpreter;
        lazy_containers;
        octez_scoru_wasm;
        octez_wasmer;
      ]

let octez_context_disk =
  octez_lib
    "tezos-context.disk"
    ~path:"src/lib_context/disk"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        bigstringaf;
        fmt;
        irmin;
        irmin_pack;
        irmin_pack_unix;
        logs_fmt;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_context_sigs;
        octez_context_helpers;
        octez_context_encoding;
        octez_context_memory;
      ]
    ~conflicts:[Conflicts.checkseum]

let _tree_encoding_tests =
  tezt
    ["test_proofs"; "test_encoding"]
    ~path:"src/lib_tree_encoding/test"
    ~opam:"tezos-tree-encoding-test"
    ~synopsis:"Tests for the tree encoding library"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_context_disk;
        octez_base_test_helpers |> open_;
        octez_test_helpers |> open_;
        octez_webassembly_interpreter;
        qcheck_alcotest;
        alcotezt;
      ]

let _octez_context_tests =
  tezt
    ["test_context"; "test_merkle_proof"; "tezt_main"]
    ~path:"src/lib_context/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_context_sigs;
        octez_context_disk;
        octez_context_memory;
        octez_context_encoding;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
        qcheck_alcotest;
        alcotezt;
      ]

let _octez_context_memory_tests =
  tezt
    ["test"]
    ~path:"src/lib_context/memory/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_context_disk;
        octez_context_memory;
        octez_stdlib_unix |> open_;
        alcotezt;
      ]

let _irmin_tests =
  tezt
    ["tezt_main"; "test_lib_irmin_store"; "test_utils"]
    ~path:"irmin/test"
    ~opam:"tezos_internal_irmin_tests"
    ~synopsis:"Tezos internal irmin tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        irmin_test_helpers;
        octez_context_disk;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _irmin_data_tests =
  tezt
    ["test"; "import"; "test_fixed_size_string_set"]
    ~path:"irmin/test/irmin/data"
    ~opam:"tezos_internal_irmin_tests"
    ~synopsis:"Tezos internal irmin tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        alcotezt;
        irmin_test_helpers;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _brassaia_data_tests =
  tezt
    ["test"; "import"; "test_fixed_size_string_set"]
    ~path:"brassaia/test/brassaia/data"
    ~opam:"tezos_internal_brassaia_tests"
    ~synopsis:"Tezos internal brassaia tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        alcotezt;
        brassaia_test_helpers;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
        vector;
      ]

let _brassaia_eio_data_tests =
  tezt
    ["test"; "import"; "test_fixed_size_string_set"]
    ~path:"brassaia-eio/test/brassaia/data"
    ~opam:"tezos_internal_brassaia_eio_tests"
    ~synopsis:"Tezos internal brassaia eio tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        alcotezt;
        brassaia_eio_test_helpers |> open_;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
        vector;
      ]

let _irmin_generic_key_tests =
  tezt
    ["test"; "test_store_offset"; "test_inlined_contents"]
    ~path:"irmin/test/irmin/generic-key"
    ~opam:"tezos_internal_irmin_tests"
    ~synopsis:"Tezos internal irmin tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        irmin_test_helpers;
        irmin_mem;
        vector;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_irmin_internal)

let _brassaia_generic_key_tests =
  tezt
    ["test"; "test_store_offset"; "test_inlined_contents"]
    ~path:"brassaia/test/brassaia/generic-key"
    ~opam:"tezos_internal_brassaia_tests"
    ~synopsis:"Tezos internal brassaia tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        brassaia_test_helpers;
        brassaia_mem;
        vector;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_brassaia_internal)

let _brassaia_eio_generic_key_tests =
  tezt
    ["test"; "test_store_offset"; "test_inlined_contents"]
    ~path:"brassaia-eio/test/brassaia/generic-key"
    ~opam:"tezos_internal_brassaia_eio_tests"
    ~synopsis:"Tezos internal brassaia eio tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        brassaia_eio_test_helpers |> open_;
        brassaia_eio_mem;
        eio_posix;
        vector;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_brassaia_eio_internal)

let _irmin_tests =
  tezt
    ["test"; "test_lru"; "test_hash"; "test_tree"; "test_conf"]
    ~path:"irmin/test/irmin"
    ~opam:"tezos_internal_irmin_tests"
    ~synopsis:"Tezos internal irmin tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        irmin_test_helpers |> open_;
        irmin_mem;
        alcotezt;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_irmin_internal)

let _brassaia_tests =
  tezt
    ["test"; "test_lru"; "test_hash"; "test_tree"; "test_conf"]
    ~path:"brassaia/test/brassaia"
    ~opam:"tezos_internal_brassaia_tests"
    ~synopsis:"Tezos internal brassaia tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        brassaia_test_helpers |> open_;
        brassaia_mem;
        alcotezt;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_brassaia_internal)

let _brassaia_eio_tests =
  tezt
    ["test"; "test_lru"; "test_hash"; "test_tree"; "test_conf"]
    ~path:"brassaia-eio/test/brassaia"
    ~opam:"tezos_internal_brassaia_eio_tests"
    ~synopsis:"Tezos internal brassaia eio tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        brassaia_eio_test_helpers |> open_;
        brassaia_eio_mem;
        alcotezt;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        eio;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_brassaia_eio_internal)

let _irmin_mem_tests =
  tezt
    ["test"; "test_mem"]
    ~path:"irmin/test/irmin-mem"
    ~opam:"tezos_internal_irmin_tests"
    ~synopsis:"Tezos internal irmin tests"
    ~deps:
      [
        octez_context_memory;
        irmin_test_helpers;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _brassaia_mem_tests =
  tezt
    ["test"; "test_mem"]
    ~path:"brassaia/test/brassaia-mem"
    ~opam:"tezos_internal_brassaia_tests"
    ~synopsis:"Tezos internal brassaia tests"
    ~deps:
      [
        octez_context_brassaia_memory;
        brassaia_test_helpers;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _brassaia_eio_mem_tests =
  tezt
    ["test"; "test_mem"]
    ~path:"brassaia-eio/test/brassaia-mem"
    ~opam:"tezos_internal_brassaia_eio_tests"
    ~synopsis:"Tezos internal brassaia eio tests"
    ~deps:
      [
        octez_context_brassaia_memory;
        brassaia_eio_mem;
        brassaia_eio_test_helpers;
        octez_test_helpers |> open_;
        eio_posix;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _irmin_tezos_tests =
  tezt
    ["tezt_main"; "generate"]
    ~path:"irmin/test/irmin-tezos"
    ~opam:"tezos_internal_irmin_tests"
    ~synopsis:"Tezos internal irmin tests"
    ~deps:
      [
        irmin_test_helpers;
        irmin_tezos;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _brassaia_tezos_tests =
  tezt
    ["tezt_main"; "generate"]
    ~path:"brassaia/test/brassaia-tezos"
    ~opam:"tezos_internal_brassaia_tests"
    ~synopsis:"Tezos internal brassaia tests"
    ~deps:
      [
        brassaia_test_helpers;
        brassaia_tezos;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _brassaia_eio_tezos_tests =
  tezt
    ["tezt_main"; "generate"]
    ~path:"brassaia-eio/test/brassaia-tezos"
    ~opam:"tezos_internal_brassaia_eio_tests"
    ~synopsis:"Tezos internal brassaia eio tests"
    ~deps:
      [
        brassaia_eio_test_helpers;
        brassaia_eio_tezos;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _irmin_pack_tests =
  tezt
    [
      "common";
      "test";
      "test_async";
      "test_corrupted";
      "test_dispatcher";
      "test_existing_stores";
      "test_flush_reload";
      "test_gc";
      "test_hashes";
      "test_indexing_strategy";
      "test_inode";
      "test_lower";
      "test_mapping";
      "test_nearest_geq";
      "test_pack";
      "test_pack_version_bump";
      "test_readonly";
      "test_ranges";
      "test_snapshot";
      "test_tree";
      "test_upgrade";
    ]
    ~path:"irmin/test/irmin-pack"
    ~opam:"tezos_internal_irmin_tests"
    ~synopsis:"Tezos internal irmin tests"
    ~deps:
      [
        octez_context_memory;
        irmin;
        irmin_test_helpers |> open_;
        irmin_pack;
        irmin_pack_unix;
        irmin_tezos;
        octez_test_helpers |> open_;
        alcotezt;
        fpath;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_irmin_internal)

let _brassaia_pack_tests =
  tezt
    [
      "common";
      "test";
      "test_async";
      "test_corrupted";
      "test_dispatcher";
      "test_existing_stores";
      "test_flush_reload";
      "test_gc";
      "test_hashes";
      "test_indexing_strategy";
      "test_inode";
      "test_lower";
      "test_mapping";
      "test_nearest_geq";
      "test_pack";
      "test_pack_version_bump";
      "test_readonly";
      "test_ranges";
      "test_snapshot";
      "test_tree";
      "test_upgrade";
    ]
    ~path:"brassaia/test/brassaia-pack"
    ~opam:"tezos_internal_brassaia_tests"
    ~synopsis:"Tezos internal brassaia tests"
    ~deps:
      [
        octez_context_memory;
        brassaia;
        brassaia_test_helpers |> open_;
        brassaia_pack;
        brassaia_pack_unix;
        brassaia_tezos;
        octez_test_helpers |> open_;
        alcotezt;
        fpath;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_brassaia_internal)

let _brassaia_eio_pack_tests =
  tezt
    [
      "common";
      "test";
      "test_async";
      "test_corrupted";
      "test_dispatcher";
      "test_existing_stores";
      "test_flush_reload";
      "test_gc";
      "test_hashes";
      "test_indexing_strategy";
      "test_inode";
      "test_lower";
      "test_mapping";
      "test_nearest_geq";
      "test_pack";
      "test_pack_version_bump";
      "test_readonly";
      "test_ranges";
      "test_snapshot";
      "test_tree";
      "test_upgrade";
      "test_multicore";
    ]
    ~path:"brassaia-eio/test/brassaia-pack"
    ~opam:"tezos_internal_brassaia_eio_tests"
    ~synopsis:"Tezos internal brassaia eio tests"
    ~deps:
      [
        octez_context_memory;
        brassaia_eio |> open_;
        brassaia_eio_test_helpers |> open_;
        brassaia_eio_pack |> open_;
        brassaia_eio_pack_io |> open_;
        brassaia_eio_tezos |> open_;
        brassaia_eio_pack_mem |> open_;
        octez_test_helpers |> open_;
        alcotezt;
        fpath;
        eio_posix;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:(pps ppx_brassaia_eio_internal)
    ~flags:(Flags.standard ~disable_warnings:[66] ())

(* This binding assumes that librustzcash.a is installed in the system default
   directories or in: $OPAM_SWITCH_PREFIX/lib

   Tests are disabled in the .opam because the tests require zcash parameter files. *)
let octez_sapling =
  octez_lib
    "tezos-sapling"
    ~path:"src/lib_sapling"
    ~deps:
      [
        conf_rust;
        integers;
        ctypes;
        data_encoding;
        octez_stdlib |> open_;
        octez_crypto;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_sapling_parameters;
        octez_lwt_result_stdlib;
      ]
    ~link_deps:lib_rustzcash
    ~dep_globs_rec:["../rustzcash_deps/*"]
    ~foreign_stubs:
      {
        language = C;
        flags = [S ":standard"];
        include_dirs = ["../rustzcash_deps/include"];
        names = ["rustzcash_ctypes_c_stubs"];
      }
    ~c_library_flags:["-lpthread"]
    ~dune:
      Dune.
        [
          [S "copy_files"; S "bindings/rustzcash_ctypes_bindings.ml"];
          [
            S "rule";
            [
              S "targets";
              S "rustzcash_ctypes_stubs.ml";
              S "rustzcash_ctypes_c_stubs.c";
            ];
            [S "deps"; [S ":gen"; S "./bindings/rustzcash_ctypes_gen.exe"]];
            [S "action"; run "%{gen}" ["%{targets}"]];
          ];
        ]

let _octez_sapling_tests =
  tezt
    [
      "test_rustzcash";
      "test_keys";
      "test_merkle";
      "test_roots";
      "test_sapling";
      "keys";
      "example";
    ]
    ~path:"src/lib_sapling/test"
    ~with_macos_security_framework:true
    ~opam:"octez-libs"
    ~dep_files:["vectors.csv"; "vectors-zip32.csv"]
    ~deps:
      [
        octez_sapling |> open_;
        octez_crypto;
        str;
        octez_base;
        octez_base_unix;
        octez_stdlib |> open_;
        octez_stdlib_unix;
        data_encoding |> open_;
        octez_base_test_helpers |> open_;
        alcotezt;
      ]
    ~dune_with_test:Never

let _octez_sapling_ctypes_gen =
  private_exes
    ["rustzcash_ctypes_gen"]
    ~path:"src/lib_sapling/bindings"
    ~opam:"octez-libs"
    ~bisect_ppx:No
    ~deps:[ctypes_stubs; ctypes]
    ~modules:["rustzcash_ctypes_gen"; "rustzcash_ctypes_bindings"]

let octez_riscv_api =
  public_lib
    "octez-riscv-api"
    ~path:"src/lib_riscv/api"
    ~synopsis:"OCaml API of the RISC-V Rust components"
    ~flags:(Flags.standard ~disable_warnings:[9; 27; 66] ())
    ~dep_globs_rec:["../../riscv/*"]
    ~link_deps:lib_wasmer_riscv
    ~modules:["octez_riscv_api"]
    ~dune:Dune.[[S "copy_files"; S "../../riscv/api/octez_riscv_api.*"]]

let octez_riscv_pvm =
  public_lib
    "octez-riscv-pvm"
    ~path:"src/lib_riscv/pvm"
    ~synopsis:"RISC-V PVM"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_riscv_api]

let tezos_protocol_environment_sigs_internals =
  octez_proto_lib
    "protocol-environment.sigs-internals"
    ~internal_name:"tezos_protocol_environment_sigs_internals"
    ~path:"src/lib_protocol_environment/sigs-internals"

let tezos_protocol_environment_sigs =
  octez_proto_lib
    "protocol-environment.sigs"
    ~internal_name:"tezos_protocol_environment_sigs"
    ~path:"src/lib_protocol_environment/sigs"
    ~deps:[tezos_protocol_environment_sigs_internals]
    ~flags:(Flags.standard ~nopervasives:true ~nostdlib:true ())
    ~dune:
      (let gen n =
         Dune.(
           targets_rule
             [sf "v%d.ml" n]
             ~deps:
               [
                 Dune.(S (sf "v%d.in.ml" n));
                 Dune.(H [[S "glob_files"; S (sf "v%n/*.mli" n)]]);
               ]
             ~mode:Promote
             ~action:
               [
                 S "with-stdout-to";
                 S "%{targets}";
                 [
                   S "run";
                   S "%{dep:../ppinclude/ppinclude.exe}";
                   S (sf "v%d.in.ml" n);
                 ];
               ])
       in
       let latest_environment_number = 16 in
       List.init (latest_environment_number + 1) gen |> Dune.of_list)

let octez_protocol_environment_structs =
  octez_proto_lib
    "protocol-environment.structs"
    ~internal_name:"tezos_protocol_environment_structs"
    ~path:"src/lib_protocol_environment/structs"
    ~deps:
      [
        octez_stdlib;
        octez_crypto;
        octez_lwt_result_stdlib;
        octez_scoru_wasm;
        data_encoding;
        bls12_381;
        octez_plonk |> open_;
      ]

let octez_protocol_environment =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_proto_lib
    "protocol-environment"
    ~internal_name:"tezos_protocol_environment"
    ~path:"src/lib_protocol_environment"
    ~documentation:[Dune.[S "package"; S "octez-proto-libs"]]
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        zarith;
        bls12_381;
        octez_plonk |> open_;
        octez_crypto_dal;
        vdf;
        aches;
        aches_lwt;
        octez_base |> open_ ~m:"TzPervasives";
        octez_sapling;
        tezos_protocol_environment_sigs;
        octez_protocol_environment_structs;
        octez_micheline |> open_;
        octez_context_memory;
        octez_context_brassaia_memory;
        octez_scoru_wasm;
        octez_riscv_pvm;
        octez_event_logging;
        octez_profiler |> open_;
      ]

let _octez_protocol_environment_tests =
  tezt
    [
      "test_mem_context";
      "test_mem_context_array_theory";
      "test_mem_context_common";
      "test_cache";
      "test_data_encoding";
    ]
    ~path:"src/lib_protocol_environment/test"
    ~opam:"octez-proto-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_protocol_environment |> open_;
        alcotezt;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        lwt_unix;
      ]

let octez_rust_tezos_context =
  public_lib
    "octez-rust-tezos-context"
    ~path:"src/rust_deps/rust_tezos_context/ocaml-api"
    ~synopsis:"OCaml API to Tezedge context"
    ~flags:
      (Flags.standard
         ~disable_warnings:[66]
         (* We disable the warnings emitted by the files generated by [ocaml-rs]. *)
         ())

let octez_context_tezedge =
  octez_lib
    "tezos-context-tezedge"
    ~path:"src/lib_context_tezedge"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        octez_context_sigs;
        octez_rust_tezos_context;
      ]

let octez_context_ops =
  let octez_context_brassaia_disk =
    octez_lib
      "tezos-context-brassaia.disk"
      ~path:"src/lib_context_brassaia/disk"
      ~deps:
        [
          octez_base |> open_ ~m:"TzPervasives";
          bigstringaf;
          fmt;
          brassaia;
          brassaia_pack;
          brassaia_pack_unix;
          logs_fmt;
          octez_stdlib_unix |> open_;
          octez_stdlib |> open_;
          octez_context_sigs;
          octez_context_brassaia_helpers;
          octez_context_brassaia_encoding;
          octez_context_brassaia_memory |> open_;
        ]
      ~conflicts:[Conflicts.checkseum]
  in
  let octez_context_brassaia =
    (* Should not be used directly in most cases, use context_ops abstraction instead *)
    octez_lib
      "tezos-context-brassaia"
      ~path:"src/lib_context_brassaia"
      ~synopsis:"On-disk context abstraction for [octez-node] (brassaia)"
      ~deps:[octez_context_brassaia_disk; octez_context_brassaia_memory]
  in
  let octez_brassaia_context =
    octez_shell_lib
      "brassaia-context"
      ~internal_name:"tezos_brassaia_context"
      ~path:"src/lib_protocol_environment/brassaia_context"
      ~deps:
        [
          octez_base |> open_ ~m:"TzPervasives";
          octez_protocol_environment;
          octez_context_brassaia;
        ]
  in
  let octez_context =
    octez_lib
      "tezos-context"
      ~path:"src/lib_context"
      ~synopsis:"On-disk context abstraction for [octez-node]"
      ~deps:[octez_context_disk; octez_context_memory]
  in
  (* in general this library should not be used directly, context_ops should be
     used instead *)
  let octez_shell_context =
    octez_shell_lib
      "shell-context"
      ~internal_name:"tezos_shell_context"
      ~path:"src/lib_protocol_environment/shell_context"
      ~deps:
        [
          octez_base |> open_ ~m:"TzPervasives";
          octez_protocol_environment;
          octez_context;
        ]
  in
  let octez_tezedge_context =
    octez_shell_lib
      "tezedge-context"
      ~internal_name:"tezos_tezedge_context"
      ~path:"src/lib_protocol_environment/tezedge_context"
      ~deps:
        [
          octez_base |> open_ ~m:"TzPervasives";
          octez_protocol_environment;
          octez_context_tezedge;
        ]
  in
  let octez_duo_context_lib =
    let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
    octez_shell_lib
      "duo-context-lib"
      ~internal_name:"tezos_duo_context_lib"
      ~path:"src/lib_protocol_environment/duo_context_lib"
      ~preprocess
      ~preprocessor_deps
      ~deps:
        [
          octez_base |> open_ ~m:"TzPervasives";
          octez_protocol_environment;
          octez_context_brassaia;
          octez_context;
          octez_tezedge_context;
        ]
  in
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_shell_lib
    "context-ops"
    ~internal_name:"tezos_context_ops"
    ~path:"src/lib_protocol_environment/context_ops"
    ~synopsis:"Backend-agnostic operations on contexts"
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_error_monad |> open_;
        octez_protocol_environment;
        octez_context |> open_;
        octez_context_brassaia |> open_;
        octez_shell_context |> open_;
        octez_stdlib_unix |> open_;
        octez_brassaia_context |> open_;
        octez_tezedge_context |> open_;
        octez_duo_context_lib |> open_;
      ]

let _brassaia_tests =
  tezt
    ["tezt_brassaia"; "test_lib_brassaia_store"; "test_utils"]
    ~path:"brassaia/test"
    ~opam:"tezos_internal_brassaia_tests"
    ~synopsis:"Tezos internal brassaia tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        brassaia_test_helpers;
        octez_context_ops;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _brassaia_eio_tests =
  tezt
    ["tezt_brassaia"; "test_lib_brassaia_store"; "test_utils"]
    ~path:"brassaia-eio/test"
    ~opam:"tezos_internal_brassaia_eio_tests"
    ~synopsis:"Tezos internal brassaia eio tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        brassaia_eio_test_helpers |> open_;
        octez_context_ops;
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]

let _octez_protocol_shell_context_tests =
  tezt
    ["test_proxy_context"]
    ~path:"src/lib_protocol_environment/test_shell_context"
    ~opam:"octez-shell-libs"
    ~deps:
      [
        octez_context_ops;
        alcotezt;
        octez_test_helpers |> open_;
        octez_base |> open_ ~m:"TzPervasives";
        octez_protocol_environment |> open_;
      ]

let octez_protocol_compiler_registerer =
  public_lib
    "octez-protocol-compiler.registerer"
    ~path:"src/lib_protocol_compiler/registerer"
    ~internal_name:"tezos_protocol_registerer"
    ~deps:
      [octez_base |> open_ ~m:"TzPervasives"; tezos_protocol_environment_sigs]
    ~flags:(Flags.standard ~opaque:true ())

let octez_protocol_compiler_alternative_hashes =
  public_lib
    "octez-protocol-compiler.alternative-hashes"
    ~path:"src/lib_protocol_compiler/hashes"
    ~internal_name:"octez_protocol_alternative_hashes"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"]

let octez_protocol_compiler_compat =
  public_lib
    "octez-protocol-compiler-compat"
    ~path:"src/lib_protocol_compiler/compat"
    ~opam:"octez-protocol-compiler-compat"
    ~synopsis:"Octez Protocol Compiler compatibility layer with OCaml versions"
    ~deps:[compiler_libs_common; compiler_libs_optcomp; compiler_libs_toplevel]
    ~dune:
      Dune.
        [
          target_rule
            "protocol_compiler_env.ml"
            ~action:
              [
                S "copy";
                S "compat_files/protocol_compiler_env_ocaml4.ml";
                S "%{target}";
              ]
            ~enabled_if:[S "<"; S "%{ocaml_version}"; S "5"];
          target_rule
            "protocol_compiler_env.ml"
            ~action:
              [
                S "copy";
                S "compat_files/protocol_compiler_env_ocaml5.ml";
                S "%{target}";
              ]
            ~enabled_if:[S ">="; S "%{ocaml_version}"; S "5"];
          target_rule
            "compiler_libs.ml"
            ~action:
              [
                S "copy"; S "compat_files/compiler_libs_ocaml4.ml"; S "%{target}";
              ]
            ~enabled_if:[S "<"; S "%{ocaml_version}"; S "5"];
          target_rule
            "compiler_libs.ml"
            ~action:
              [
                S "copy"; S "compat_files/compiler_libs_ocaml5.ml"; S "%{target}";
              ]
            ~enabled_if:
              [
                S "and";
                [S ">="; S "%{ocaml_version}"; S "5"];
                [S "<"; S "%{ocaml_version}"; S "5.2"];
              ];
          target_rule
            "compiler_libs.ml"
            ~action:
              [
                S "copy";
                S "compat_files/compiler_libs_ocaml5_2.ml";
                S "%{target}";
              ]
            ~enabled_if:[S ">="; S "%{ocaml_version}"; S "5.2"];
        ]

let _octez_protocol_compiler_cmis_of_cma =
  private_exe
    "cmis_of_cma"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"octez-protocol-compiler"
    ~deps:[bls12_381_archive; octez_protocol_compiler_compat]
    ~modules:["cmis_of_cma"]

let octez_protocol_compiler_lib =
  public_lib
    "octez-protocol-compiler"
    ~path:"src/lib_protocol_compiler"
    ~synopsis:"Tezos: economic-protocol compiler"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        tezos_protocol_environment_sigs;
        octez_stdlib_unix |> open_;
        octez_protocol_compiler_compat |> open_;
        octez_protocol_compiler_alternative_hashes |> open_;
        lwt_unix;
        ocplib_ocamlres;
        unix;
      ]
    ~opam_only_deps:[octez_protocol_environment]
    ~modules:
      [
        "Embedded_cmis_env";
        "Embedded_cmis_register";
        "Packer";
        "Compiler";
        "Defaults";
      ]
    ~dune:
      Dune.
        [
          targets_rule
            ["embedded-interfaces-env"]
            ~deps:[Dune.(H [[S "package"; S "octez-proto-libs"]])]
            ~action:
              [
                S "with-stdout-to";
                S "%{targets}";
                [
                  S "run";
                  S "bin/cmis_of_cma.exe";
                  V
                    [
                      S
                        "%{lib:octez-proto-libs.protocol-environment.sigs:tezos_protocol_environment_sigs.cmxa}";
                    ];
                ];
              ];
          targets_rule
            ["embedded_cmis_env.ml"]
            ~deps:[Dune.(H [[S "package"; S "octez-proto-libs"]])]
            ~action:
              [
                S "run";
                G
                  [
                    S "%{bin:ocp-ocamlres}";
                    S "-format";
                    S "variants";
                    S "-o";
                    S "%{targets}";
                  ];
                S "%{read-strings:embedded-interfaces-env}";
              ];
          targets_rule
            ["embedded_cmis_register.ml"]
            ~action:
              [
                S "run";
                G
                  [
                    S "%{bin:ocp-ocamlres}";
                    S "-format";
                    S "variants";
                    S "-o";
                    S "%{targets}";
                  ];
                S "%{cmi:registerer/tezos_protocol_registerer}";
              ];
          targets_rule
            ["defaults.ml"]
            ~action:
              [
                S "write-file";
                S "%{targets}";
                S
                  (sf
                     "let warnings = %S"
                     ("+a"
                     ^ Flags.disabled_warnings_to_string
                         warnings_disabled_by_default));
              ];
        ]

let octez_protocol_compiler_native =
  public_lib
    "octez-protocol-compiler.native"
    ~path:"src/lib_protocol_compiler"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_protocol_compiler_lib |> open_;
        octez_protocol_compiler_compat |> open_;
        compiler_libs_optcomp;
      ]
    ~modules:["Native"]
    ~dune:
      Dune.
        [
          install
            [V [S "final_protocol_versions"]]
            ~package:"octez-protocol-compiler"
            ~section:"libexec";
        ]

let octez_protocol_updater =
  octez_shell_lib
    "protocol-updater"
    ~internal_name:"tezos_protocol_updater"
    ~path:"src/lib_protocol_updater"
    ~synopsis:"Economic-protocol dynamic loading for `octez-node`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_micheline |> open_;
        octez_shell_services |> open_;
        octez_protocol_environment;
        octez_protocol_compiler_alternative_hashes;
        octez_protocol_compiler_registerer;
        octez_protocol_compiler_native;
        octez_context_ops |> open_;
        lwt_exit;
        dynlink;
      ]

let octez_client_base =
  octez_shell_lib
    "client-base"
    ~internal_name:"tezos_client_base"
    ~path:"src/lib_client_base"
    ~synopsis:"Tezos: common helpers for `octez-client`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_clic;
        octez_rpc;
        octez_shell_services |> open_;
        octez_sapling;
        uri;
      ]
    ~modules:[":standard"; "bip39_english"]
    ~linkall:true
    ~dune:
      Dune.
        [
          targets_rule
            ["bip39_english.ml"]
            ~deps:
              [
                [S ":exe"; S "gen/bip39_generator.exe"];
                S "gen/bip39_english.txt";
              ]
            ~action:[S "run"; S "%{exe}"; S "%{targets}"];
        ]

let _octez_client_base_tests =
  tezt
    ["bip39_tests"; "pbkdf_tests"]
    ~path:"src/lib_client_base/test"
    ~opam:"octez-shell-libs"
    ~with_macos_security_framework:true
    ~deps:[octez_base; octez_client_base |> open_; alcotezt]

let octez_validation =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_shell_lib
    "validation"
    ~internal_name:"tezos_validation"
    ~path:"src/lib_validation"
    ~synopsis:"Library for block validation"
    ~time_measurement_ppx:true
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_client_base |> open_;
        octez_crypto |> open_;
        octez_rpc;
        octez_clic;
        octez_context_ops |> open_;
        octez_shell_services |> open_;
        octez_protocol_updater |> open_;
        octez_stdlib_unix |> open_;
        octez_profiler |> open_;
        octez_profiler_backends |> open_;
        octez_version_value;
      ]

let octez_store_shared =
  octez_shell_lib
    "store.shared"
    ~internal_name:"tezos_store_shared"
    ~path:"src/lib_store/shared"
    ~deps:
      [
        octez_stdlib_unix |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_crypto |> open_;
        octez_shell_services |> open_;
        aches;
        aches_lwt;
        octez_validation |> open_;
      ]
    ~modules:
      [
        "naming";
        "block_repr";
        "store_types";
        "store_events";
        "block_key";
        "block_level";
      ]

let octez_store_unix =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_shell_lib
    "store.unix"
    ~internal_name:"tezos_store_unix"
    ~path:"src/lib_store/unix"
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_shell_services |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_version;
        index;
        octez_store_shared |> open_;
        octez_protocol_environment |> open_;
        octez_context_ops |> open_;
        octez_validation |> open_;
        octez_profiler |> open_;
        octez_protocol_updater |> open_;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_crypto |> open_;
        lwt_watcher;
        aches;
        aches_lwt;
        camlzip;
        tar;
        tar_unix;
        prometheus;
      ]
    ~modules:
      [
        "block_repr_unix";
        "block_store";
        "cemented_block_store";
        "consistency";
        "floating_block_index";
        "floating_block_store";
        "protocol_store";
        "store_metrics";
        "store";
      ]
    ~conflicts:[Conflicts.checkseum]

let octez_store_unix_reconstruction =
  octez_shell_lib
    "store.unix-reconstruction"
    ~internal_name:"tezos_store_unix_reconstruction"
    ~path:"src/lib_store/unix"
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_crypto |> open_;
        octez_shell_services |> open_;
        octez_protocol_updater |> open_;
        octez_validation |> open_;
        octez_context_ops |> open_;
        octez_store_shared |> open_;
        octez_store_unix |> open_;
      ]
    ~modules:["reconstruction"; "reconstruction_events"]

let octez_store_unix_snapshots =
  octez_shell_lib
    "store.unix-snapshots"
    ~internal_name:"tezos_store_unix_snapshots"
    ~path:"src/lib_store/unix"
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_crypto |> open_;
        octez_shell_services |> open_;
        octez_context_ops |> open_;
        octez_validation |> open_;
        octez_store_shared |> open_;
        octez_store_unix |> open_;
      ]
    ~modules:["snapshots"; "snapshots_events"]

let octez_store =
  octez_shell_lib
    "store"
    ~internal_name:"tezos_store"
    ~path:"src/lib_store"
    ~synopsis:"Store for `octez-node`"
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_crypto |> open_;
        octez_rpc;
        lwt_watcher;
        octez_shell_services |> open_;
        octez_validation |> open_;
        octez_context_ops |> open_;
        octez_store_shared |> open_;
        octez_profiler |> open_;
      ]
    ~virtual_modules:["store"]
    ~default_implementation:"octez-shell-libs.store.real"

let _octez_store_real =
  octez_shell_lib
    "store.real"
    ~internal_name:"tezos_store_real"
    ~path:"src/lib_store/real"
    ~deps:[octez_store_unix |> open_]
    ~implements:octez_store

let _octez_store_mocked =
  octez_shell_lib
    "mocked"
    ~path:"src/lib_store/mocked"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_crypto |> open_;
        octez_shell_services |> open_;
        octez_context_ops |> open_;
        octez_validation |> open_;
        octez_protocol_environment;
        octez_store_shared |> open_;
      ]
    ~private_modules:["block_store"; "protocol_store"; "stored_data"]
    ~implements:octez_store

let octez_requester =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_shell_lib
    "requester"
    ~internal_name:"tezos_requester"
    ~path:"src/lib_requester"
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_shell_services |> open_;
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_profiler |> open_;
        lwt_watcher;
      ]

let octez_requester_tests =
  tezt
    ["requester_impl"; "test_requester"; "test_fuzzing_requester"; "shared"]
    ~path:"src/lib_requester/test"
    ~opam:"octez-requester-tests"
    ~synopsis:"Requester tests"
    ~deps:
      [
        octez_shell_services |> open_;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        octez_stdlib |> open_;
        octez_stdlib_unix;
        octez_requester |> open_;
        qcheck_tezt;
      ]

let octez_shell =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_shell_lib
    "shell"
    ~internal_name:"tezos_shell"
    ~path:"src/lib_shell"
    ~synopsis:
      "Core of `octez-node` (gossip, validation scheduling, mempool, ...)"
    ~documentation:
      Dune.
        [[S "package"; S "octez-shell-libs"]; [S "mld_files"; S "octez_shell"]]
    ~inline_tests:ppx_expect
    ~inline_tests_libraries:[bls12_381_archive]
    ~inline_tests_link_flags:["-cclib"; "-lblst"]
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        lwt_watcher;
        lwt_canceler;
        prometheus;
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix |> open_;
        octez_rpc;
        octez_store |> open_;
        octez_store_shared |> open_;
        octez_protocol_environment |> open_;
        octez_context_ops |> open_;
        octez_p2p |> open_;
        octez_stdlib_unix |> open_;
        octez_shell_services |> open_;
        octez_p2p_services |> open_;
        octez_profiler |> open_;
        octez_profiler_complex_backends |> open_;
        octez_protocol_compiler_alternative_hashes;
        octez_protocol_updater |> open_;
        octez_requester |> open_;
        octez_workers |> open_;
        octez_validation |> open_;
        octez_version |> open_;
        octez_dal_config |> open_;
        lwt_exit;
        octez_bees;
      ]

let octez_connpool =
  public_lib
    "octez-connpool"
    ~opam:"octez-connpool"
    ~path:"src/lib_connpool"
    ~synopsis:"Persistent HTTP connection pool for Octez built with Cohttp-lwt"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        cohttp_lwt_unix;
        uri;
        opentelemetry_lwt;
      ]

let _octez_connpool_tests =
  tezt
    ~path:"src/lib_connpool/test"
    ~opam:"octez-connpool"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_;
        octez_connpool;
      ]
    ["test_connpool"]

let octez_rpc_http =
  octez_lib
    "rpc-http"
    ~internal_name:"tezos-rpc-http"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Library of auto-documented RPCs (http server and client)"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_rpc; resto_cohttp; uri]
    ~modules:["RPC_client_errors"; "media_type"]

let octez_rpc_http_client =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  octez_lib
    "rpc-http-client"
    ~internal_name:"tezos-rpc-http-client"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Library of auto-documented RPCs (http client)"
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_profiler |> open_;
        resto_cohttp_client;
        octez_rpc;
        octez_rpc_http |> open_;
      ]
    ~modules:["RPC_client"; "RPC_profiler"]

let octez_rpc_http_client_unix =
  octez_lib
    "rpc-http-client-unix"
    ~internal_name:"tezos_rpc_http_client_unix"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Unix implementation of the RPC client"
    ~deps:
      [
        octez_stdlib_unix;
        octez_base |> open_ ~m:"TzPervasives";
        cohttp_lwt_unix;
        resto_cohttp_client;
        octez_rpc;
        octez_rpc_http_client |> open_;
      ]
    ~modules:["RPC_client_unix"]

let octez_rpc_http_server =
  octez_lib
    "rpc-http-server"
    ~internal_name:"tezos_rpc_http_server"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Library of auto-documented RPCs (http server)"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        cohttp_lwt_unix;
        resto_cohttp_server;
        resto_acl;
        octez_rpc;
        octez_rpc_http |> open_;
      ]
    ~modules:["RPC_server"; "RPC_middleware"]

let _octez_rpc_http_tests =
  tezt
    ["test_rpc_http"]
    ~path:"src/lib_rpc_http/test"
    ~opam:"octez-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib |> open_;
        octez_stdlib_unix;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        octez_rpc_http_server |> open_;
        resto_cohttp_client;
        qcheck_alcotest;
        alcotezt;
      ]

let octez_telemetry =
  octez_lib
    "octez-telemetry"
    ~internal_name:"Octez_telemetry"
    ~path:"src/lib_telemetry"
    ~synopsis:"Convenient wrappers to trace Octez libraries with Opentelemetry"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        opentelemetry;
        opentelemetry_lwt;
        resto;
        octez_rpc_http;
        octez_rpc_http_server;
        octez_rpc_http_client_unix;
        opentelemetry_ambient_context_lwt;
        octez_workers;
        octez_stdlib_unix |> open_;
      ]

let _bip39_generator =
  private_exe
    "bip39_generator"
    ~path:"src/lib_client_base/gen"
    ~opam:"octez-shell-libs"
    ~bisect_ppx:No

let octez_signer_services =
  octez_shell_lib
    "signer-services"
    ~internal_name:"tezos_signer_services"
    ~path:"src/lib_signer_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-signer`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc;
        octez_client_base |> open_;
      ]
    ~linkall:true

let octez_signer_backends =
  octez_shell_lib
    "signer-backends"
    ~internal_name:"tezos_signer_backends"
    ~path:"src/lib_signer_backends"
    ~synopsis:"Tezos: remote-signature backends for `octez-client`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        octez_client_base |> open_;
        octez_rpc_http |> open_;
        octez_rpc_http_client |> open_;
        octez_signer_services |> open_;
        octez_shell_services |> open_;
        uri;
      ]

let _octez_signer_backends_tests =
  tezt
    ["test_encrypted"]
    ~path:"src/lib_signer_backends/test"
    ~opam:"octez-shell-libs"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base;
        octez_base_unix;
        octez_stdlib |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_crypto;
        octez_client_base |> open_;
        octez_signer_backends |> open_;
        alcotezt;
        uri;
      ]

let octez_signer_backends_unix =
  octez_shell_lib
    "signer-backends.unix"
    ~internal_name:"tezos_signer_backends_unix"
    ~path:"src/lib_signer_backends/unix"
    ~deps:
      [
        ocplib_endian_bigstring;
        fmt;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_clic;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_client_base |> open_;
        octez_rpc_http |> open_;
        octez_rpc_http_client |> open_;
        octez_rpc_http_client_unix |> open_;
        octez_signer_services |> open_;
        octez_signer_backends |> open_;
        octez_shell_services |> open_;
        uri;
        select
          ~package:ledgerwallet_tezos
          ~source_if_present:"ledger.available.ml"
          ~source_if_absent:"ledger.none.ml"
          ~target:"ledger.ml";
      ]

let _octez_signer_backends_unix_tests =
  tezt
    ["test_crouching"]
    ~path:"src/lib_signer_backends/unix/test"
    ~opam:"octez-shell-libs"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_error_monad |> open_;
        octez_stdlib |> open_;
        octez_crypto;
        octez_client_base |> open_;
        octez_signer_backends_unix |> open_;
        alcotezt;
      ]

let octez_client_commands =
  octez_shell_lib
    "client-commands"
    ~internal_name:"tezos_client_commands"
    ~path:"src/lib_client_commands"
    ~synopsis:"Tezos: protocol agnostic commands for `octez-client`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc;
        octez_clic;
        octez_clic_unix |> open_;
        octez_client_base |> open_;
        octez_shell_services |> open_;
        octez_p2p_services |> open_;
        octez_stdlib_unix;
        octez_base_unix |> open_;
        octez_signer_backends;
        data_encoding |> open_;
        uri;
      ]
    ~linkall:true

let octez_mockup_registration =
  octez_shell_lib
    "mockup-registration"
    ~internal_name:"tezos_mockup_registration"
    ~path:"src/lib_mockup"
    ~synopsis:"Tezos: protocol registration for the mockup mode"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_client_base;
        octez_shell_services;
        octez_protocol_environment;
        uri;
      ]
    ~modules:["registration"; "registration_intf"; "mockup_args"]

let octez_mockup_proxy =
  octez_shell_lib
    "mockup-proxy"
    ~internal_name:"tezos_mockup_proxy"
    ~path:"src/lib_mockup_proxy"
    ~synopsis:"Tezos: local RPCs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_client_base;
        octez_protocol_environment;
        octez_rpc_http;
        resto_cohttp_self_serving_client;
        octez_rpc_http_client;
        octez_shell_services;
        uri;
      ]

(* Depends on tezos_p2p to register the relevant RPCs. *)
let octez_mockup =
  octez_shell_lib
    "mockup"
    ~internal_name:"tezos_mockup"
    ~path:"src/lib_mockup"
    ~synopsis:"Tezos: library of auto-documented RPCs (mockup mode)"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_client_base;
        octez_mockup_proxy;
        resto_cohttp_self_serving_client;
        octez_rpc;
        octez_p2p_services;
        octez_p2p;
        octez_protocol_environment;
        octez_stdlib_unix;
        octez_rpc_http;
        octez_rpc_http_client;
        octez_mockup_registration |> open_;
      ]
    ~modules:
      [
        "files";
        "local_services";
        "persistence";
        "persistence_intf";
        "RPC_client";
        "migration";
      ]

let octez_mockup_commands =
  octez_shell_lib
    "mockup-commands"
    ~internal_name:"tezos_mockup_commands"
    ~path:"src/lib_mockup"
    ~synopsis:"Tezos: library of auto-documented RPCs (commands)"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_clic;
        octez_client_commands;
        octez_client_base;
        octez_mockup |> open_;
        octez_mockup_registration |> open_;
      ]
    ~modules:["mockup_wallet"; "mockup_commands"]

let _octez_mockup_tests =
  tezt
    ["test_mockup_args"; "test_fuzzing_mockup_args"; "test_persistence"]
    ~path:"src/lib_mockup/test"
    ~opam:"octez-shell-libs"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_test_helpers |> open_;
        octez_test_helpers |> open_;
        octez_rpc;
        octez_mockup;
        octez_mockup_registration;
        octez_client_base;
        qcheck_alcotest;
        alcotezt;
      ]

let octez_proxy =
  octez_shell_lib
    "proxy"
    ~internal_name:"tezos_proxy"
    ~path:"src/lib_proxy"
    ~synopsis:"Tezos: proxy"
    ~deps:
      [
        aches;
        aches_lwt;
        octez_base |> open_ ~m:"TzPervasives";
        octez_clic;
        octez_client_base;
        octez_protocol_environment;
        octez_rpc;
        octez_shell_services;
        uri;
      ]

let octez_proxy_rpc =
  octez_shell_lib
    "proxy.rpc"
    ~internal_name:"tezos_proxy_rpc"
    ~path:"src/lib_proxy/rpc"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_client_base;
        octez_mockup_proxy;
        octez_rpc;
        octez_proxy;
        uri;
      ]

let octez_proxy_test_helpers_shell_services =
  private_lib
    "tezos_proxy_test_helpers_shell_services"
    ~path:"src/lib_proxy/test_helpers/shell_services"
    ~opam:""
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_shell_services;
        octez_test_helpers |> open_;
        qcheck_core;
        octez_context_memory;
        lwt_unix;
        alcotezt;
      ]
    ~bisect_ppx:No
    ~linkall:true
    ~release_status:Released

let _octez_shell_service_test_helpers_tests =
  tezt
    ["test_block_services"]
    ~path:"src/lib_proxy/test_helpers/shell_services/test"
    ~opam:"octez-shell-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_test_helpers |> open_;
        octez_shell_services;
        octez_proxy_test_helpers_shell_services;
        qcheck_alcotest;
        alcotezt;
      ]

let _octez_proxy_tests =
  tezt
    [
      "test_proxy";
      "test_fuzzing_proxy_getter";
      "test_light";
      "test_fuzzing_light";
      "light_lib";
    ]
    ~path:"src/lib_proxy/test"
    ~with_macos_security_framework:true
    ~opam:"octez-shell-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix;
        octez_proxy;
        octez_base_test_helpers |> open_;
        octez_test_helpers |> open_;
        octez_proxy_test_helpers_shell_services;
        qcheck_alcotest;
        alcotezt;
        uri;
      ]

let octez_client_base_unix =
  octez_shell_lib
    "client-base-unix"
    ~internal_name:"tezos_client_base_unix"
    ~path:"src/lib_client_base_unix"
    ~synopsis:
      "Tezos: common helpers for `octez-client` (unix-specific fragment)"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_clic;
        octez_rpc;
        octez_rpc_http |> open_;
        octez_rpc_http_client_unix |> open_;
        octez_shell_services |> open_;
        octez_stdlib_unix |> open_;
        octez_client_base |> open_;
        octez_client_commands |> open_;
        octez_mockup;
        octez_mockup_registration;
        octez_mockup_commands |> open_;
        octez_proxy;
        octez_proxy_rpc;
        octez_signer_backends_unix;
        octez_version_value;
        lwt_exit;
        uri;
      ]
    ~linkall:true

let _octez_client_base_unix_tests =
  tezt
    ["test_mockup_wallet"]
    ~path:"src/lib_client_base_unix/test"
    ~opam:"octez-shell-libs"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_mockup_commands;
        octez_client_base_unix;
        octez_base_test_helpers |> open_;
        alcotezt;
      ]

let octez_benchmark =
  public_lib
    "tezos-benchmark"
    ~path:"src/lib_benchmark"
    ~synopsis:
      "Tezos: library for writing benchmarks and performing simple parameter \
       inference"
    ~foreign_stubs:
      {
        language = C;
        flags = [S ":standard"];
        include_dirs = [];
        names = ["snoop_stubs"];
      }
    ~private_modules:["builtin_models"; "builtin_benchmarks"]
    ~deps:
      [
        str;
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_crypto;
        octez_micheline;
        octez_clic;
        data_encoding;
        prbnmcn_linalg;
        prbnmcn_stats;
        pringo;
        pyml;
        ocamlgraph;
        opam_only "hashcons" V.True;
      ]
    ~inline_tests:ppx_expect
    ~inline_tests_deps:[S "%{workspace_root}/.ocamlformat"]
    ~inline_tests_libraries:[bls12_381_archive]
    ~inline_tests_link_flags:["-cclib"; "-lblst"]
      (* We disable tests for this package as they require Python, which is not
           installed in the image of the opam jobs. *)
    ~opam_with_test:Never

let octez_benchmark_examples =
  public_lib
    "tezos-benchmark-examples"
    ~path:"src/lib_benchmark/example"
    ~synopsis:"Tezos: examples for lib-benchmarks"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix;
        octez_crypto;
        octez_benchmark;
      ]

let _octez_benchmark_tests =
  tezt
    [
      "test";
      "test_sparse_vec";
      "test_costlang";
      "test_model";
      "test_probe";
      "test_measure";
      "test_benchmark_helpers";
    ]
    ~path:"src/lib_benchmark/test"
    ~opam:"tezos-benchmark-tests"
    ~synopsis:"Tezos: tests for lib-benchmarks"
    ~deps:
      [
        alcotezt;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix;
        octez_micheline;
        octez_crypto;
        octez_benchmark;
        octez_benchmark_examples;
      ]

(* unused lib? *)
let octez_micheline_rewriting =
  public_lib
    "tezos-micheline-rewriting"
    ~path:"src/lib_benchmark/lib_micheline_rewriting"
    ~synopsis:"Tezos: library for rewriting Micheline expressions"
    ~deps:
      [
        zarith;
        octez_stdlib |> open_;
        octez_crypto;
        octez_error_monad |> open_;
        octez_micheline |> open_;
      ]

let octez_shell_benchmarks =
  octez_shell_lib
    "shell-benchmarks"
    ~internal_name:"tezos_shell_benchmarks"
    ~path:"src/lib_shell_benchmarks"
    ~synopsis:"Tezos: shell benchmarks"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_error_monad |> open_;
        octez_benchmark |> open_;
        octez_crypto;
        octez_store;
        octez_micheline;
      ]
    ~linkall:true
    ~foreign_stubs:
      {language = C; flags = []; include_dirs = []; names = ["alloc_mmap"]}

let octogram =
  public_lib
    "octogram"
    ~path:"src/lib_octogram"
    ~synopsis:"An Ansible-inspired environment to run scenarios and experiments"
    ~deps:
      [
        octez_shell_services |> open_;
        octez_rpc_http_client_unix |> open_;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        jingoo;
        dmap;
      ]

let _octogram_bin =
  public_exe
    "octogram"
    ~path:"src/bin_octogram"
    ~internal_name:"octogram_main"
    ~release_status:Unreleased
    ~opam:"octogram"
    ~deps:
      [
        bls12_381_archive;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        octogram;
        yaml;
      ]

let octez_openapi =
  public_lib
    "tezos-openapi"
    ~path:"src/lib_openapi"
    ~synopsis:
      "Tezos: a library for querying RPCs and converting into the OpenAPI \
       format"
    ~deps:[ezjsonm; json_data_encoding; tezt_json_lib]

let _octez_protocol_compiler_bin =
  public_exe
    "octez-protocol-compiler"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"octez-protocol-compiler"
    ~internal_name:"main_native"
    ~modes:[Native]
    ~deps:
      [bls12_381_archive; octez_protocol_compiler_native; octez_version_value]
    ~linkall:true
    ~modules:["Main_native"]

let octez_protocol_compiler_tezos_protocol_packer =
  public_exe
    "octez-protocol-compiler.octez-protocol-packer"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"octez-protocol-compiler"
    ~internal_name:"main_packer"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_protocol_compiler_lib |> open_;
      ]
    ~modules:["Main_packer"]

let _octez_embedded_protocol_packer =
  public_exe
    "octez-embedded-protocol-packer"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"octez-protocol-compiler"
    ~internal_name:"main_embedded_packer"
    ~modes:[Native]
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
      ]
    ~linkall:true
    ~modules:["Main_embedded_packer"]

let _octez_riscv_pvm_test =
  tezt
    ["test_main"; "test_backend"; "test_storage"; "test_rust_bindings"; "utils"]
    ~path:"src/lib_riscv/pvm/test"
    ~opam:"octez-riscv-pvm-test"
    ~synopsis:"Tests for RISC-V OCaml API"
    ~dep_globs:
      [
        "../../../riscv/assets/riscv-dummy.elf";
        "../../../riscv/assets/riscv-dummy.elf.checksum";
        "../../../riscv/assets/jstz_proof_initial";
      ]
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_base_test_helpers |> open_;
        alcotezt;
        octez_riscv_pvm;
      ]

let _octez_libcrux_ml_dsa_tests =
  tezt
    ["test_main"; "test_bindings"]
    ~path:"src/rust_libcrux/test"
    ~opam:"octez-libcrux-ml-dsa-test"
    ~synopsis:"Tests for the libcrux ML-DSA bindings"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        alcotezt;
        octez_ml_dsa;
      ]

let octez_layer2_store =
  octez_l2_lib
    "layer2_store"
    ~internal_name:"tezos_layer2_store"
    ~path:"src/lib_layer2_store"
    ~synopsis:"layer2 storage utils"
    ~deps:
      [
        index;
        octez_base |> open_ ~m:"TzPervasives";
        irmin_pack;
        irmin_pack_unix;
        irmin;
        aches_lwt;
        octez_stdlib_unix |> open_;
        octez_context_encoding;
        octez_context_sigs;
        octez_context_helpers;
        cohttp_lwt_unix;
        camlzip;
        tar;
        tar_unix;
      ]
    ~linkall:true
    ~conflicts:[Conflicts.checkseum]

let octez_layer2_irmin_context =
  octez_l2_lib
    "irmin_context"
    ~internal_name:"tezos_layer2_irmin_context"
    ~path:"src/lib_layer2_irmin_context"
    ~synopsis:"Irmin context for the smart rollup PVMs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        irmin_pack;
        irmin_pack_unix;
        irmin;
        octez_context_encoding;
        octez_context_sigs;
        octez_context_helpers;
        octez_layer2_store |> open_;
        opentelemetry_lwt;
      ]
    ~linkall:true
    ~conflicts:[Conflicts.checkseum]

let octez_layer2_shell =
  octez_l2_lib
    "layer2_shell"
    ~internal_name:"Tezos_layer2_shell"
    ~path:"src/lib_layer2_shell"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_layer2_irmin_context;
        lambda_term;
      ]

let octez_layer2_riscv_context =
  octez_l2_lib
    "riscv_context"
    ~internal_name:"tezos_layer2_riscv_context"
    ~path:"src/lib_layer2_riscv_context"
    ~synopsis:"RiscV implementation of the context for Layer2"
    ~deps:
      [
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib"
        |> open_ ~m:"Error_monad";
        octez_lwt_result_stdlib |> open_;
        octez_layer2_store |> open_;
        octez_riscv_pvm;
      ]
    ~linkall:true

let octez_sqlite =
  octez_l2_lib
    "sqlite"
    ~internal_name:"octez_sqlite"
    ~path:"src/lib_sqlite"
    ~synopsis:"SQLite wrappers and helpers"
    ~deps:
      [
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib"
        |> open_ ~m:"Error_monad";
        caqti;
        caqti_lwt;
        caqti_lwt_unix;
        caqti_sqlite;
        opentelemetry_lwt;
      ]
    ~linkall:true
    ~conflicts:[Conflicts.checkseum]

let _octez_layer2_indexed_store_test =
  tezt
    ["test_indexed_store"]
    ~path:"src/lib_layer2_store/test/"
    ~opam:"octez-l2-libs"
    ~deps:
      [
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_layer2_store |> open_;
        qcheck_alcotest;
        alcotezt;
        tezt_lib;
      ]

let octez_dal_node_services =
  public_lib
    "tezos-dal-node-services"
    ~path:"src/lib_dal_node_services"
    ~opam:"tezos-dal-node-services"
    ~synopsis:"Tezos: `tezos-dal-node` RPC services"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_rpc;
        octez_crypto_dal;
        lwt_watcher;
      ]
    ~linkall:true

let octez_dal_node_migrations =
  public_lib
    "dal_node_migrations"
    ~path:"src/lib_dal_node/migrations"
    ~synopsis:"Tezos: SQL migrations for the DAL node store"
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

let octez_dal_node_gossipsub_lib =
  public_lib
    "tezos-dal-node-lib.gossipsub"
    ~path:"src/lib_dal_node/gossipsub"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_crypto_dal |> open_;
        octez_gossipsub |> open_;
        octez_p2p |> open_;
        octez_p2p_services |> open_;
        octez_dal_node_services |> open_;
        octez_crypto |> open_;
      ]

let octez_crawler =
  public_lib
    "octez-crawler"
    ~internal_name:"octez_crawler"
    ~path:"src/lib_crawler"
    ~synopsis:"Octez: library to crawl blocks of the L1 chain"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_rpc_http |> open_;
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_client_base |> open_;
        octez_shell;
        octez_telemetry;
      ]

let octez_dal_node_lib =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  public_lib
    "tezos-dal-node-lib"
    ~path:"src/lib_dal_node"
    ~opam:"tezos-dal-node-lib"
    ~synopsis:"Tezos: `tezos-dal-node` library"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_bees;
        octez_dal_node_services |> open_;
        octez_dal_node_migrations;
        octez_protocol_updater |> open_;
        octez_rpc_http_client_unix;
        octez_stdlib_unix |> open_;
        octez_crypto_dal |> open_;
        octez_p2p |> open_;
        octez_p2p_services |> open_;
        octez_sqlite |> open_;
        octez_version;
        cmdliner;
        octez_client_base |> open_;
        octez_rpc_http |> open_;
        octez_rpc_http_server;
        octez_stdlib |> open_;
        octez_layer2_store |> open_;
        octez_store_unix;
        octez_store_shared |> open_;
        octez_gossipsub |> open_;
        octez_dal_node_gossipsub_lib |> open_;
        octez_crypto |> open_;
        octez_base_p2p_identity_file |> open_;
        octez_shell_services |> open_;
        opentelemetry;
        opentelemetry_client_cohttp_lwt;
        octez_telemetry;
        prometheus_app;
        prometheus;
        octez_crawler |> open_;
        octez_profiler |> open_;
        octez_profiler_complex_backends |> open_;
      ]
    ~preprocess
    ~preprocessor_deps

let _octez_dal_node_lib_test =
  tezt
    ["test_storage"]
    ~path:"src/lib_dal_node/test/"
    ~opam:"tezos-dal-node-lib"
    ~preprocess:(ppses [ppx_hash])
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        octez_base |> open_ ~m:"TzPervasives";
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_dal_node_lib |> open_;
        octez_test_helpers |> open_;
        tezt_bam;
      ]

let octez_node_config =
  public_lib
    "octez-node-config"
    ~path:"src/lib_node_config"
    ~synopsis:"Octez: `octez-node-config` library"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_shell_services |> open_;
        octez_rpc_http |> open_;
        octez_rpc_http_server |> open_;
        octez_context_ops |> open_;
        octez_store |> open_;
        octez_validation |> open_;
      ]

let _octez_p2p_node =
  private_exe
    "main_p2p_node"
    ~path:"src/bin_p2p_node"
    ~opam:""
    ~synopsis:"Tezos: node with only the P2P layer used for testing"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_clic |> open_;
        octez_version_value |> open_;
        octez_node_config |> open_;
        octez_p2p_services |> open_;
        octez_p2p |> open_;
        octez_shell |> open_;
      ]
    ~linkall:true
    ~with_macos_security_framework:true

let octez_rpc_process =
  public_lib
    "octez-rpc-process"
    ~path:"src/lib_rpc_process"
    ~synopsis:"Tezos: RPC process"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_shell |> open_;
        octez_base_unix |> open_;
        octez_node_config |> open_;
        octez_protocol_updater |> open_;
        octez_rpc_http |> open_;
        octez_rpc_http_server |> open_;
        octez_rpc_http_client_unix |> open_;
        octez_rpc_http_client |> open_;
        octez_shell_services;
        octez_stdlib_unix |> open_;
        octez_store |> open_;
        octez_store_shared |> open_;
        lwt_unix;
        lwt_exit;
        prometheus_app;
      ]

let octez_injector_lib =
  public_lib
    "octez-injector"
    ~path:"src/lib_injector"
    ~synopsis:"Octez: library for building injectors"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        logs_lwt;
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_crypto;
        octez_micheline |> open_;
        octez_client_base |> open_;
        octez_workers |> open_;
        octez_shell;
        octez_crawler |> open_;
        octez_signer_backends;
        octez_telemetry;
      ]

let performance_metrics =
  public_lib
    "octez-performance-metrics"
    ~path:"src/lib_performance_metrics"
    ~deps:
      [
        prometheus;
        octez_version;
        octez_base |> open_ ~m:"TzPervasives";
        lwt_unix;
      ]
    ~synopsis:"Set of metrics to measure binary performances"

let octez_smart_rollup_lib =
  octez_l2_lib
    "smart-rollup"
    ~internal_name:"octez_smart_rollup"
    ~path:"src/lib_smart_rollup"
    ~synopsis:"Library for Smart Rollups"
    ~documentation:[Dune.[S "package"; S "octez-l2-libs"]]
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_crypto |> open_;
        octez_layer2_store |> open_;
        octez_crypto_dal;
        yaml;
        opentelemetry;
      ]

let rollup_node_sqlite_migrations =
  octez_l2_lib
    "rollup_node_sqlite_migrations"
    ~path:"src/lib_smart_rollup_node/migrations"
    ~synopsis:"SQL migrations for the Rollup node store"
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

let octez_smart_rollup_node_store_lib_modules =
  ["store_version"; "sql_store"; "store_sig"; "store"]

let octez_smart_rollup_node_store_lib =
  octez_l2_lib
    "octez-smart-rollup-node-lib.store"
    ~internal_name:"octez_smart_rollup_node_store"
    ~path:"src/lib_smart_rollup_node"
    ~synopsis:"Octez: library for accessing the store of the Smart Rollup node"
    ~modules:octez_smart_rollup_node_store_lib_modules
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_layer2_store |> open_;
        rollup_node_sqlite_migrations;
        octez_sqlite |> open_;
        octez_smart_rollup_lib |> open_;
        performance_metrics;
      ]

let octez_smart_rollup_node_lib =
  public_lib
    "octez-smart-rollup-node-lib"
    ~internal_name:"octez_smart_rollup_node"
    ~path:"src/lib_smart_rollup_node"
    ~synopsis:"Octez: library for Smart Rollup node"
    ~all_modules_except:octez_smart_rollup_node_store_lib_modules
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_crypto |> open_;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        cohttp_lwt_unix;
        octez_openapi;
        octez_node_config;
        prometheus_app;
        octez_dal_node_lib;
        octez_injector_lib |> open_;
        octez_version_value |> open_;
        octez_layer2_store |> open_;
        octez_layer2_irmin_context |> open_;
        octez_layer2_riscv_context |> open_;
        octez_smart_rollup_node_store_lib |> open_;
        octez_crawler |> open_;
        octez_workers |> open_;
        octez_smart_rollup_lib |> open_;
        octez_scoru_wasm_fast;
        opentelemetry_lwt;
        octez_telemetry;
      ]

let wasm_helpers_intf_modules = ["wasm_utils_intf"]

let octez_scoru_wasm_helpers_intf =
  octez_l2_lib
    "scoru-wasm-helpers-intf"
    ~internal_name:"tezos_scoru_wasm_helpers_intf"
    ~path:"src/lib_scoru_wasm/helpers"
    ~synopsis:
      "Helpers for the smart rollup wasm functionality and debugger \
       (signatures)"
    ~modules:wasm_helpers_intf_modules
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_scoru_wasm;
        octez_webassembly_interpreter_extra |> open_;
      ]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let wasm_helpers_functor_modules = ["wasm_utils_functor"]

let octez_scoru_wasm_helpers_functor =
  octez_l2_lib
    "scoru-wasm-helpers-functor"
    ~internal_name:"tezos_scoru_wasm_helpers_functor"
    ~path:"src/lib_scoru_wasm/helpers"
    ~synopsis:
      "Helpers for the smart rollup wasm functionality and debugger (functor)"
    ~modules:wasm_helpers_functor_modules
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_scoru_wasm;
        octez_webassembly_interpreter_extra |> open_;
        octez_scoru_wasm_helpers_intf |> open_;
      ]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let octez_scoru_wasm_helpers =
  octez_l2_lib
    "scoru-wasm-helpers"
    ~internal_name:"tezos_scoru_wasm_helpers"
    ~path:"src/lib_scoru_wasm/helpers"
    ~synopsis:"Helpers for the smart rollup wasm functionality and debugger"
    ~all_modules_except:
      (wasm_helpers_intf_modules @ wasm_helpers_functor_modules)
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_scoru_wasm;
        octez_scoru_wasm_fast;
        octez_scoru_wasm_helpers_intf |> open_;
        octez_scoru_wasm_helpers_functor |> open_;
        octez_webassembly_interpreter_extra |> open_;
      ]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let octez_scoru_wasm_durable_snapshot =
  octez_l2_lib
    "scoru_wasm_durable_snapshot"
    ~internal_name:"tezos_scoru_wasm_durable_snapshot"
    ~path:"src/lib_scoru_wasm/test/durable_snapshot"
    ~synopsis:"Durable storage reference implementation"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_webassembly_interpreter_extra |> open_;
      ]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let octez_scoru_wasm_tests_helpers =
  octez_l2_lib
    "scoru_wasm_test_helpers"
    ~internal_name:"tezos_scoru_wasm_test_helpers"
    ~path:"src/lib_scoru_wasm/test/helpers"
    ~synopsis:"Helpers for test of the smart rollup wasm functionality"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_base_test_helpers |> open_;
        octez_test_helpers;
        octez_scoru_wasm;
        octez_scoru_wasm_durable_snapshot;
        octez_scoru_wasm_fast;
        octez_scoru_wasm_helpers;
        qcheck_alcotest;
        alcotezt;
        octez_webassembly_interpreter_extra |> open_;
      ]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let octez_scoru_wasm_benchmark =
  octez_l2_lib
    "smart_rollup_wasm_benchmark_lib"
    ~internal_name:"octez_smart_rollup_wasm_benchmark_lib"
    ~path:"src/lib_scoru_wasm/bench"
    ~synopsis:"Smart Rollup WASM benchmark library"
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        tezt_lib;
        octez_webassembly_interpreter;
        octez_scoru_wasm;
        octez_scoru_wasm_helpers;
        lwt_unix;
      ]
    ~preprocess:(pps ppx_deriving_show)

let _octez_scoru_wasm_benchmark_exe =
  private_exe
    "octez_smart_rollup_wasm_benchmark"
    ~path:"src/lib_scoru_wasm/bench/executable"
    ~opam:"octez-l2-libs"
    ~preprocess:(pps ppx_deriving_show)
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_scoru_wasm_benchmark;
      ]

let _octez_scoru_wasm_tests =
  tezt
    [
      "test_ast_generators";
      "test_debug";
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5028
         Beware: there is a weird test failure when
         Durable snapshot test doesn't go first *)
      "test_durable_shapshot";
      "test_durable_storage";
      "test_fixed_nb_ticks";
      "test_get_set";
      "test_hash_consistency";
      "test_host_functions_ticks";
      "test_init";
      "test_input";
      "test_output";
      "test_parser_encoding";
      "test_protocol_migration";
      "test_reveal";
      "test_wasm_encoding";
      "test_wasm_pvm_encodings";
      "test_wasm_pvm";
      "test_wasm_vm";
      "test_crypto";
    ]
    ~path:"src/lib_scoru_wasm/test"
    ~opam:"octez-l2-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_base_test_helpers |> open_;
        octez_test_helpers |> open_;
        octez_scoru_wasm;
        qcheck_alcotest;
        alcotezt;
        octez_scoru_wasm_helpers |> open_;
        octez_scoru_wasm_tests_helpers |> open_;
        octez_webassembly_interpreter_extra |> open_;
      ]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let _octez_scoru_wasm_fast_tests =
  tezt
    [
      "gen";
      "partial_memory";
      "qcheck_helpers";
      "test_fast_cache";
      "test_fast";
      "test_memory_access";
    ]
    ~path:"src/lib_scoru_wasm/fast/test"
    ~opam:"octez-l2-libs"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_base_test_helpers |> open_;
        octez_scoru_wasm_helpers |> open_;
        octez_scoru_wasm_tests_helpers |> open_;
        octez_test_helpers |> open_;
        octez_scoru_wasm;
        octez_scoru_wasm_fast;
        qcheck_alcotest;
        alcotezt;
      ]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let octez_baking_common_lib =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  public_lib
    "octez-baking-common-lib"
    ~path:"src/lib_baking_common"
    ~internal_name:"octez_baking_common"
    ~synopsis:
      "Common Library for protocol-dependent bakers and protocol-agnostic baker"
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix |> open_;
        octez_client_base_unix |> open_;
        octez_profiler |> open_;
      ]

let octez_agnostic_baker_lib =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  public_lib
    "octez-baker-lib"
    ~path:"src/lib_agnostic_baker"
    ~internal_name:"octez_agnostic_baker"
    ~synopsis:"Octez: library for Agnostic Baker"
    ~preprocess
    ~preprocessor_deps
    ~deps:
      [
        bls12_381_archive;
        data_encoding |> open_;
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix |> open_;
        octez_client_base_unix |> open_;
        octez_node_config;
        octez_client_commands |> open_;
        octez_profiler |> open_;
        octez_stdlib_unix |> open_;
        octez_shell_services |> open_;
        octez_dal_node_services;
        octez_dal_node_lib;
        octez_baking_common_lib;
      ]

(* PROTOCOL PACKAGES *)

module Protocol : sig
  type number = Dev | V of int | Other

  type status = Active | Frozen | Overridden | Not_mainnet

  type t

  val number : t -> number

  val short_hash : t -> string

  val status : t -> status

  val name_dash : t -> string

  val name_underscore : t -> string

  val base_path : t -> string

  val main : t -> target

  val embedded : t -> target

  (** [embedded] does not fail, it's just that the optional version
      composes better with [all_optionally]. *)
  val embedded_opt : t -> target option

  val client : t -> target option

  val client_exn : t -> target

  val client_commands_exn : t -> target

  val client_commands_registration : t -> target option

  val baking_commands_registration : t -> target option

  val plugin : t -> target option

  val plugin_exn : t -> target

  val plugin_registerer : t -> target option

  val plugin_registerer_exn : t -> target

  val dal : t -> target option

  val agnostic_baker : t -> target option

  val parameters : t -> target option

  val parameters_exn : t -> target

  val benchmarks_proto_exn : t -> target

  val octez_sc_rollup : t -> target option

  val octez_sc_rollup_node : t -> target option

  val octez_injector : t -> target option

  val baking_exn : t -> target

  val test_helpers_exn : t -> target

  val test_migration_exn : t -> target

  val genesis : t

  val demo_noops : t

  val alpha : t

  val all : t list

  val active : t list

  val all_optionally : (t -> target option) list -> target list
end = struct
  type number = Dev | V of int | Other

  module Name : sig
    type t

    (** [dev] is a named protocol name with protocol number [Dev] *)
    val dev : string -> t

    (** [v name num] constuct a protocol name with protocol number [V num] *)
    val v : string -> int -> t

    (** [other name] constuct a protocol name with protocol number [Other] *)
    val other : string -> t

    val number : t -> number

    val name_underscore : t -> string

    val name_dash : t -> string

    val base_path : t -> string

    val short_hash : t -> string
  end = struct
    type t = {
      short_hash : string;
      name_underscore : string;
      name_dash : string;
      number : number;
    }

    let make name number =
      if
        not
          (String.for_all
             (function
               | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
               | _ -> false)
             name)
      then
        invalid_arg
          (sf
             "Protocol.Name.make: %s is not a valid protocol name: should be \
              of the form [A-Za-z0-9-]+"
             name) ;
      let make_full_name sep name =
        match number with
        | Dev | Other -> name
        | V number -> sf "%03d%c%s" number sep name
      in
      let short_hash = Str.replace_first (Str.regexp "^[0-9]+_") "" name in
      let name_dash =
        make_full_name '-' (String.map (function '_' -> '-' | c -> c) name)
      in
      let name_underscore =
        make_full_name '_' (String.map (function '-' -> '_' | c -> c) name)
      in
      {short_hash; number; name_dash; name_underscore}

    let v name number = make name (V number)

    let dev name = make name Dev

    let other name = make name Other

    let short_hash t = t.short_hash

    let number t = t.number

    let name_underscore t = t.name_underscore

    let name_dash t = t.name_dash

    let base_path t = Format.sprintf "src/proto_%s" (name_underscore t)
  end

  type status = Active | Frozen | Overridden | Not_mainnet

  type t = {
    status : status;
    name : Name.t;
    main : target;
    embedded : target;
    client : target option;
    client_commands : target option;
    client_commands_registration : target option;
    baking_commands_registration : target option;
    plugin : target option;
    plugin_registerer : target option;
    dal : target option;
    agnostic_baker : target option;
    test_helpers : target option;
    test_migration : target option;
    parameters : target option;
    benchmarks_proto : target option;
    baking : target option;
    octez_sc_rollup : target option;
    octez_sc_rollup_node : target option;
    octez_injector : target option;
  }

  let make ?client ?client_commands ?client_commands_registration
      ?baking_commands_registration ?plugin ?plugin_registerer ?dal
      ?agnostic_baker ?test_helpers ?test_migration ?parameters
      ?benchmarks_proto ?octez_sc_rollup ?octez_sc_rollup_node ?octez_injector
      ?baking ~status ~name ~main ~embedded () =
    {
      status;
      name;
      main;
      embedded;
      client;
      client_commands;
      client_commands_registration;
      baking_commands_registration;
      plugin;
      plugin_registerer;
      dal;
      agnostic_baker;
      test_helpers;
      test_migration;
      parameters;
      benchmarks_proto;
      baking;
      octez_sc_rollup;
      octez_sc_rollup_node;
      octez_injector;
    }

  let all_rev : t list ref = ref []

  (* Add to the [Protocol.add] list used to link in the node, client, etc.
     Returns the protocol for easier composability. *)
  let register protocol =
    all_rev := protocol :: !all_rev ;
    protocol

  let mandatory what {main; _} = function
    | None ->
        failwith
          ("protocol " ^ name_for_errors main ^ " has no " ^ what ^ " package")
    | Some x -> x

  let number p = Name.number p.name

  let short_hash p = Name.short_hash p.name

  let status p = p.status

  let name_dash p = Name.name_dash p.name

  let name_underscore p = Name.name_underscore p.name

  let base_path p = Name.base_path p.name

  let main p = p.main

  let embedded p = p.embedded

  let embedded_opt p = Some p.embedded

  let client p = p.client

  let client_exn p = mandatory "client" p p.client

  let client_commands_exn p = mandatory "client-commands" p p.client_commands

  let client_commands_registration p = p.client_commands_registration

  let baking_commands_registration p = p.baking_commands_registration

  let plugin p = p.plugin

  let plugin_exn p = mandatory "plugin" p p.plugin

  let plugin_registerer p = p.plugin_registerer

  let plugin_registerer_exn p =
    mandatory "plugin_registerer" p p.plugin_registerer

  let dal p = p.dal

  let agnostic_baker p = p.agnostic_baker

  let parameters p = p.parameters

  let parameters_exn p = mandatory "parameters" p p.parameters

  let benchmarks_proto_exn p = mandatory "benchmarks_proto" p p.benchmarks_proto

  let baking_exn p = mandatory "baking" p p.baking

  let octez_sc_rollup p = p.octez_sc_rollup

  let octez_sc_rollup_node p = p.octez_sc_rollup_node

  let octez_injector p = p.octez_injector

  let test_helpers_exn p = mandatory "test_helpers" p p.test_helpers

  let test_migration_exn p = mandatory "test_migration" p p.test_migration

  (* N as in "protocol number in the Dev family". *)
  module N = struct
    (* This function is asymmetrical on purpose: we don't want to compare
       numbers with [Dev] because such comparisons would break when snapshotting.
       So the left-hand side is the number of the protocol being built,
       but the right-hand side is an integer.

       We could instead have defined functions with one argument [number_le], [number_ge],
       [version_ne] and [version_eq] in [register_alpha_family] directly.
       We chose to use a module instead because [number_le 013] is not as readable as
       [N.(number <= 013)]. Indeed, is [number_le 013] equivalent to [(<=) 013],
       meaning "greater than 013", or is [number_le 013] equivalent to [fun x -> x <= 013],
       meaning the opposite? *)
    let compare_asymmetric a b =
      match a with
      | Dev -> 1
      | V a -> Int.compare a b
      | Other ->
          invalid_arg "cannot use N.compare_asymmetric on Other protocols"

    let ( <= ) a b = compare_asymmetric a b <= 0

    let ( >= ) a b = compare_asymmetric a b >= 0

    let ( == ) a b = compare_asymmetric a b == 0
  end

  let signature_version_number ~protocol_number =
    if N.(protocol_number <= 022) then 1
    else if N.(protocol_number <= 024) then 2
    else 3

  let only_if condition make = if condition then Some (make ()) else None

  let conditional_list =
    List.filter_map (fun (x, b) -> if b then Some x else None)

  let error_monad_module should_use_infix =
    if should_use_infix then open_ ~m:"TzPervasives.Error_monad_legacy"
    else fun target -> target

  module Lib_protocol = struct
    type t = {main : target; lifted : target; embedded : target}

    let make_tests ?test_helpers ?parameters ?plugin ?client ?benchmark
        ?benchmark_type_inference ?octez_sc_rollup ~main ~name () =
      let name_dash = Name.name_dash name in
      let number = Name.number name in
      let path = Name.base_path name in
      let modules =
        [
          ("test_baking", true);
          ("test_consensus_key", true);
          ("test_deactivation", true);
          ("test_delegation", true);
          ("test_double_baking", true);
          ("test_double_attestation", N.(number >= 018));
          ("test_double_endorsement", N.(number <= 017));
          ("test_double_preattestation", N.(number >= 018));
          ("test_double_preendorsement", N.(number <= 017));
          ("test_attestation", N.(number >= 018));
          ("test_endorsement", N.(number <= 017));
          ("test_frozen_deposits", true);
          ("test_helpers_rpcs", true);
          ("test_participation", true);
          ("test_preattestation_functor", N.(number >= 018));
          ("test_preendorsement_functor", N.(number <= 017));
          ("test_preattestation", N.(number >= 018));
          ("test_preendorsement", N.(number <= 017));
          ("test_seed", true);
          ("test_aggregate", N.(number >= 022));
          ("test_dal_entrapment", N.(number >= 022));
          ("test_companion_key", N.(number >= 023));
          ("test_scenario_attestation", N.(number >= 023));
          ("test_all_bakers_attest", N.(number >= 024));
        ]
        |> conditional_list
      in
      let _integration_consensus =
        tezt
          modules
          ~path:(path // "lib_protocol/test/integration/consensus")
          ~with_macos_security_framework:true
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~deps:
            [
              alcotezt;
              octez_base |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              main |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
              parameters |> if_some |> open_;
              plugin |> if_some |> open_;
            ]
      in
      let _integration_gas =
        tezt
          ["test_gas_costs"; "test_gas_levels"]
          ~path:(path // "lib_protocol/test/integration/gas")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              alcotezt;
              octez_base |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              main |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
            ]
      in
      let _integration_michelson =
        let modules =
          [
            ("test_annotations", true);
            ("test_block_time_instructions", true);
            ("test_contract_event", true);
            ("test_global_constants_storage", true);
            ("test_interpretation", true);
            ("test_lazy_storage_diff", true);
            ("test_patched_contracts", true);
            ("test_sapling", true);
            ("test_script_cache", true);
            ("test_script_typed_ir_size", true);
            ("test_temp_big_maps", true);
            ("test_ticket_accounting", true);
            ("test_ticket_balance_key", true);
            ("test_ticket_balance", true);
            ("test_ticket_lazy_storage_diff", true);
            ("test_ticket_manager", true);
            ("test_ticket_operations_diff", true);
            ("test_ticket_scanner", true);
            ("test_ticket_storage", true);
            ("test_ticket_direct_spending", N.(number >= 019));
            ("test_typechecking", true);
            ("test_lambda_normalization", N.(number >= 016));
          ]
          |> conditional_list
        in
        tezt
          modules
          ~path:(path // "lib_protocol/test/integration/michelson")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~dep_globs:
            (conditional_list
               [
                 ("contracts/*", true);
                 ("patched_contracts/*", N.(number >= 013));
               ])
          ~dep_globs_rec:
            (conditional_list
               [
                 ( "../../../../../../michelson_test_scripts/*",
                   N.(number >= 014) );
               ])
          ~deps:
            [
              alcotezt;
              octez_base |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              main |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
              octez_client_base |> if_ N.(number <= 012);
              client |> if_some |> open_;
              octez_benchmark;
              octez_micheline |> open_;
              benchmark |> if_some |> open_;
              benchmark_type_inference |> if_some |> open_;
              plugin |> if_some |> open_;
              parameters |> if_some |> if_ N.(number >= 013);
            ]
      in
      let _integration_operations =
        let modules =
          [
            ("test_activation", true);
            ("test_combined_operations", true);
            ("test_failing_noop", true);
            ("test_origination", true);
            ("test_paid_storage_increase", true);
            ("test_reveal", true);
            ("test_sc_rollup_transfer", N.(number >= 016));
            ("test_sc_rollup", N.(number >= 016));
            ("test_transfer", true);
            ("test_voting", true);
            ("test_zk_rollup", true);
            ("test_transfer_ticket", N.(number >= 016));
          ]
          |> conditional_list
        in
        tezt
          modules
          ~path:(path // "lib_protocol/test/integration/operations")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~dep_globs:(conditional_list [("contracts/*", N.(number >= 013))])
          ~deps:
            [
              alcotezt;
              octez_base |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              main |> open_;
              client |> if_some |> if_ N.(number >= 012) |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
              plugin |> if_some |> open_;
            ]
      in
      let _integration_validate =
        only_if N.(number >= 014) @@ fun () ->
        tezt
          [
            "generator_descriptors";
            "generators";
            "manager_operation_helpers";
            "test_1m_restriction";
            "test_covalidity";
            "test_manager_operation_validation";
            "test_mempool";
            "test_sanity";
            "test_validation_batch";
            "valid_operations_generators";
            "validate_helpers";
          ]
          ~path:(path // "lib_protocol/test/integration/validate")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              alcotezt |> if_ N.(number <= 018);
              octez_base |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              main |> open_;
              qcheck_alcotest;
              client |> if_some |> open_;
              octez_test_helpers |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
              parameters |> if_some |> if_ N.(number >= 019) |> open_;
              plugin |> if_some |> open_;
            ]
      in
      let _integration =
        let modules =
          [
            ("test_constants", true);
            ("test_frozen_bonds", true);
            ("test_adaptive_issuance_launch", N.(number >= 018 && number <= 021));
            ("test_adaptive_issuance_roundtrip", N.(number == 018));
            ("test_scenario_base", N.(number >= 019));
            ("test_scenario_stake", N.(number >= 019));
            ("test_scenario_rewards", N.(number >= 019));
            ("test_scenario_autostaking", N.(number >= 019 && number <= 021));
            ("test_scenario_slashing", N.(number >= 019));
            ("test_scenario_slashing_stakers", N.(number >= 019));
            ("test_scenario_deactivation", N.(number >= 019));
            ("test_liquidity_baking", true);
            ("test_storage_functions", true);
            ("test_storage", true);
            ("test_token", true);
            ("test_native_contracts", N.(number >= 025));
            ("test_clst", N.(number >= 025));
          ]
          |> conditional_list
        in
        tezt
          modules
          ~path:(path // "lib_protocol/test/integration")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              (if N.(number >= 015) then Some tezt_lib else None) |> if_some;
              alcotezt |> if_ N.(number <= 018);
              octez_base |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              client |> if_some |> open_;
              octez_client_base |> if_ N.(number <= 012);
              main |> open_;
              parameters |> if_some |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
              plugin |> if_some |> open_;
            ]
          ~dep_globs:(if N.(number >= 015) then ["wasm_kernel/*.wasm"] else [])
      in
      let _pbt =
        let list =
          [
            ("liquidity_baking_pbt", true);
            ("saturation_fuzzing", true);
            ("test_merkle_list", N.(number >= 013 && number <= 023));
            ("test_gas_properties", true);
            ("test_sampler", N.(number >= 012));
            ("test_script_comparison", true);
            ("test_script_roundtrip", N.(number >= 019));
            ("test_script_normalization", true);
            ("test_tez_repr", true);
            ("test_sc_rollup_tick_repr", N.(number >= 016));
            ("test_sc_rollup_encoding", N.(number >= 016));
            ("test_sc_rollup_inbox", N.(number >= 017));
            ("refutation_game_pbt", N.(number == 013));
            ("test_refutation_game", N.(number >= 016));
            ("test_carbonated_map", N.(number >= 013));
            ("test_zk_rollup_encoding", N.(number >= 015));
            ("test_dal_slot_proof", N.(number >= 016));
            ("test_compare_operations", N.(number >= 015));
            ("test_operation_encoding", N.(number >= 016));
            ("test_balance_updates_encoding", N.(number >= 018));
            ("test_bytes_conversion", N.(number >= 016));
          ]
          |> conditional_list
        in
        tezt
          list
          ~synopsis:"Tezos/Protocol: tests for economic-protocol definition"
          ~path:(path // "lib_protocol/test/pbt")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              octez_base
              |> if_ N.(number <= 14)
              |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              octez_base |> if_ N.(number >= 15) |> open_ ~m:"TzPervasives";
              octez_micheline |> open_;
              client |> if_some |> open_;
              main |> open_;
              octez_merkle_proof_encoding;
              octez_test_helpers |> open_;
              test_helpers |> if_some |> open_;
              alcotezt;
              qcheck_alcotest;
              octez_client_base |> if_ N.(number <= 012);
              octez_benchmark;
              benchmark |> if_some |> open_;
              benchmark_type_inference |> if_some |> open_;
              octez_sc_rollup |> if_some |> if_ N.(number >= 016) |> open_;
              octez_crypto_dal |> if_ N.(number >= 016) |> open_;
              octez_base_test_helpers |> if_ N.(number >= 016) |> open_;
              parameters |> if_some |> if_ N.(number >= 016) |> open_;
              plugin |> if_some |> open_;
            ]
      in
      let _unit =
        let modules =
          [
            ("test_bond_id_repr", true);
            ("test_consensus_key", true);
            ("test_contract_repr", true);
            ("test_destination_repr", true);
            ("test_fitness", true);
            ("test_fixed_point", true);
            ("test_gas_monad", true);
            ("test_global_constants_storage", true);
            ("test_level_module", true);
            ("test_liquidity_baking_repr", true);
            ("test_merkle_list", N.(number <= 023));
            ("test_operation_repr", true);
            ("test_qty", true);
            ("test_receipt", true);
            ("test_round_repr", true);
            ("test_saturation", true);
            ("test_sc_rollup_arith", N.(number >= 016));
            ("test_sc_rollup_game", N.(number >= 016));
            ("test_sc_rollup_inbox", N.(number >= 016));
            ("test_sc_rollup_management_protocol", N.(number >= 016));
            ("test_sc_rollup_storage", N.(number >= 016));
            ("test_skip_list_repr", true);
            ("test_tez_repr", true);
            ("test_time_repr", true);
            ("test_zk_rollup_storage", true);
            ("test_sc_rollup_inbox_legacy", N.(number >= 016));
            ("test_sc_rollup_wasm", N.(number >= 016));
            ("test_local_contexts", N.(number >= 016));
            ("test_dal_slot_proof", N.(number >= 016));
            ("test_dal_past_parameters_storage", N.(number >= 025));
            ("test_dal_attestations_repr", N.(number >= 025));
            ("test_adaptive_issuance", N.(number >= 018));
            ("test_adaptive_issuance_ema", N.(number <= 023));
            ("test_percentage", N.(number >= 019));
            ("test_full_staking_balance_repr", N.(number >= 019));
            ("test_slashing_percentage", N.(number >= 019));
            ("test_staking_operations", N.(number >= 021));
            ("test_consecutive_round_zero", N.(number >= 022));
            ("test_address_registry", N.(number >= 024));
            ( "test_native_contracts",
              N.(number >= 025)
              (* This condition is temporary: as proto_024 is still in `dev`
                 it doesn't have a number and it would be targeted as valid. *)
            );
          ]
          |> conditional_list
        in
        tezt
          modules
          ~path:(path // "lib_protocol/test/unit")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              octez_base |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              octez_base_test_helpers |> open_;
              octez_micheline |> open_;
              client |> if_some |> open_;
              octez_client_base;
              parameters |> if_some |> open_if N.(number >= 016);
              octez_protocol_environment;
              octez_stdlib_unix;
              main |> open_;
              octez_test_helpers |> open_;
              test_helpers |> if_some |> open_;
              alcotezt;
              octez_scoru_wasm_helpers |> if_ N.(number >= 016) |> open_;
              octez_stdlib |> if_ N.(number >= 013) |> open_;
              octez_crypto_dal |> if_ N.(number >= 016) |> open_;
              octez_scoru_wasm;
              octez_webassembly_interpreter_extra
              |> if_ N.(number >= 016)
              |> open_;
            ]
      in
      let _regresssion =
        if N.(number >= 014) then
          (* About [~dep_globs]: this is only needed so that dune re-runs the tests
             if those files are modified. Dune will also copy those files in [_build],
             but the test uses absolute paths to find those files
             (thanks to [DUNE_SOURCEROOT] and [Filename.dirname __FILE__]),
             so those copies are not actually used. This is needed so that the test
             can be run either with [dune build @runtest],
             with [dune exec src/proto_alpha/lib_protocol/test/regression/main.exe],
             or with [dune exec tezt/tests/main.exe -- -f test_logging.ml]. *)
          let _ =
            tezt
              ["test_logging"]
              ~path:(path // "lib_protocol/test/regression")
              ~with_macos_security_framework:true
              ~opam:(sf "tezos-protocol-%s-tests" name_dash)
              ~deps:
                [
                  octez_base |> open_ ~m:"TzPervasives";
                  tezt_tezos |> open_;
                  main |> open_;
                  client |> if_some |> open_;
                  plugin |> if_some |> open_;
                  test_helpers |> if_some |> open_;
                  octez_micheline |> open_;
                ]
              ~dep_globs:["contracts/*.tz"; "expected/test_logging.ml/*.out"]
          in
          ()
      in
      ()

    let make ~name ~status =
      let name_underscore = Name.name_underscore name in
      let name_dash = Name.name_dash name in
      let number = Name.number name in
      let path = Name.base_path name in
      let dirname = path // "lib_protocol" in
      let tezos_protocol_filename = dirname // "TEZOS_PROTOCOL" in
      let tezos_protocol = Tezos_protocol.of_file_exn tezos_protocol_filename in
      (* Container of the registered sublibraries of [octez-protocol-libs] *)
      let registered_tezos_protocol = Sub_lib.make_container () in
      let tezos_protocol_sub_lib =
        Sub_lib.sub_lib
          ~package_synopsis:(sf "Tezos protocol %s package" name_dash)
          ~container:registered_tezos_protocol
          ~package:(sf "tezos-protocol-%s" name_dash)
      in
      let modules_as_deps =
        let basenames_of_module module_ =
          [".ml"; ".mli"]
          |> List.filter_map (fun ext ->
                 let basename = String.uncapitalize_ascii module_ ^ ext in
                 if Sys.file_exists (dirname // basename) then Some basename
                 else None)
        in
        let s_expr =
          tezos_protocol.Tezos_protocol.modules
          |> List.map (fun module_ ->
                 match basenames_of_module module_ with
                 | _ :: _ as basenames -> Dune.(G (of_atom_list basenames))
                 | [] ->
                     failwith
                       (sf
                          "In %s a module %s was declared, but no \
                           corresponding .ml or .mli files were found in \
                           directory %s"
                          tezos_protocol_filename
                          module_
                          dirname))
          |> Dune.of_list
        in
        Dune.V s_expr
      in
      let disable_warnings =
        match number with
        (* [Other] and [Dev] protocols can be edited and should be
           fixed whenever a warning that we care about triggers. We
           only want to disable a limited set of warnings *)
        | Other | Dev -> []
        (* [V _] protocols can't be edited to accomodate warnings, we need to disable warnings instead. *)
        | V _ as number ->
            if N.(number >= 024) then []
            else if N.(number >= 014) then [32; 34]
            else if N.(number >= 011) then [32; 34; 51]
            else [6; 7; 9; 16; 29; 32; 34; 51; 68]
      in
      let environment =
        tezos_protocol_sub_lib
          "protocol.environment"
          ~internal_name:(sf "tezos_protocol_environment_%s" name_underscore)
          ~path:(path // "lib_protocol")
          ~modules:[sf "Tezos_protocol_environment_%s" name_underscore]
          ~linkall:true
          ~deps:[octez_protocol_environment]
          ~dune:
            Dune.
              [
                targets_rule
                  [sf "tezos_protocol_environment_%s.ml" name_underscore]
                  ~action:
                    [
                      S "write-file";
                      S "%{targets}";
                      S
                        (sf
                           {|module Name = struct let name = "%s" end
include Tezos_protocol_environment.V%d.Make(Name)()
|}
                           name_dash
                           tezos_protocol.expected_env_version);
                    ];
              ]
      in
      let raw_protocol =
        tezos_protocol_sub_lib
          "protocol.raw"
          ~internal_name:(sf "tezos_raw_protocol_%s" name_underscore)
          ~path:(path // "lib_protocol")
          ~linkall:true
          ~modules:tezos_protocol.modules
          ~flags:
            (Flags.standard
               ~nopervasives:true
               ~nostdlib:true
               ~disable_warnings
               ())
          ~deps:
            [
              environment |> open_ |> open_ ~m:"Pervasives"
              |> open_ ~m:"Error_monad";
            ]
      in
      let main =
        tezos_protocol_sub_lib
          "protocol"
          ~internal_name:(sf "tezos_protocol-%s" name_dash)
          ~path:(path // "lib_protocol")
          ~synopsis:
            (match number with
            | V _ as number when N.(number <= 003) ->
                sf
                  "Tezos/Protocol: %s (economic-protocol definition, functor \
                   version)"
                  name_underscore
            | Other ->
                sf
                  "Tezos/Protocol: %s economic-protocol definition"
                  name_underscore
            | Dev | V _ -> "Tezos/Protocol: economic-protocol definition")
          ~modules:["Protocol"; sf "Tezos_protocol_%s" name_underscore]
          ~flags:(Flags.standard ~nopervasives:true ~disable_warnings ())
          ~deps:
            [
              octez_protocol_environment;
              tezos_protocol_environment_sigs;
              raw_protocol;
            ]
          ~dune:
            Dune.
              [
                install
                  [as_ "TEZOS_PROTOCOL" "protocol/raw/TEZOS_PROTOCOL"]
                  ~package:(sf "tezos-protocol-%s" name_dash)
                  ~section:"lib";
                targets_rule
                  ["protocol.ml"]
                  ~action:
                    [
                      S "write-file";
                      S "%{targets}";
                      S
                        (sf
                           {|
let hash = Tezos_crypto.Hashed.Protocol_hash.of_b58check_exn "%s"
let name = Tezos_protocol_environment_%s.Name.name
include Tezos_raw_protocol_%s
include Tezos_raw_protocol_%s.Main
|}
                           tezos_protocol.hash
                           name_underscore
                           name_underscore
                           name_underscore);
                    ];
                targets_rule
                  [sf "tezos_protocol_%s.ml" name_underscore]
                  ~action:
                    [
                      S "write-file";
                      S "%{targets}";
                      S
                        (sf
                           {|
module Environment = Tezos_protocol_environment_%s
module Protocol = Protocol
|}
                           name_underscore);
                    ];
                alias_rule
                  "runtest_compile_protocol"
                  ~deps_dune:
                    [modules_as_deps; [S ":src_dir"; S "TEZOS_PROTOCOL"]]
                  ~action:
                    [
                      S "run";
                      S "%{bin:octez-protocol-compiler}";
                      (if
                         (match number with Dev -> false | _ -> true)
                         && String_set.mem
                              tezos_protocol.Tezos_protocol.hash
                              final_protocol_versions
                       then E
                       else S "-no-hash-check");
                      (match disable_warnings with
                      | [] -> E
                      | l ->
                          H
                            [
                              S "-warning";
                              S (Flags.disabled_warnings_to_string l);
                            ]);
                      H [S "-warn-error"; S "+a"];
                      S ".";
                    ];
              ]
      in
      let lifted =
        tezos_protocol_sub_lib
          "protocol.lifted"
          ~internal_name:(sf "tezos_protocol-%s.lifted" name_dash)
          ~path:(path // "lib_protocol")
          ~modules:["Lifted_protocol"]
          ~flags:(Flags.standard ~nopervasives:true ~disable_warnings ())
          ~deps:
            [
              octez_protocol_environment;
              tezos_protocol_environment_sigs;
              main |> open_;
            ]
          ~dune:
            Dune.
              [
                targets_rule
                  ["lifted_protocol.ml"]
                  ~action:
                    [
                      S "write-file";
                      S "%{targets}";
                      S
                        {|
include Environment.Lift (Protocol)
let hash = Protocol.hash
|};
                    ];
              ]
      in
      let _functor =
        private_lib
          (sf "tezos_protocol_%s_functor" name_underscore)
          ~path:(path // "lib_protocol")
          ~opam:""
          ~synopsis:
            (match number with
            | V _ as number when N.(number <= 003) ->
                sf
                  "Tezos/Protocol: %s (economic-protocol definition \
                   parameterized by its environment implementation)"
                  (if N.(number == 000) then name_dash else name_underscore)
            | Other ->
                sf
                  "Tezos/Protocol: %s (economic-protocol definition \
                   parameterized by its environment implementation)"
                  name_underscore
            | Dev | V _ ->
                "Tezos/Protocol: economic-protocol definition parameterized by \
                 its environment implementation")
          ~modules:["Functor"]
            (* The instrumentation is removed as it can lead to a stack overflow *)
            (* https://gitlab.com/tezos/tezos/-/issues/1927 *)
          ~bisect_ppx:No
          ~flags:(Flags.standard ~nopervasives:true ~disable_warnings ())
          ~opam_only_deps:[octez_protocol_compiler_tezos_protocol_packer]
          ~deps:[octez_protocol_environment; tezos_protocol_environment_sigs]
          ~dune:
            Dune.
              [
                targets_rule
                  ["functor.ml"]
                  ~deps:[modules_as_deps; [S ":src_dir"; S "TEZOS_PROTOCOL"]]
                  ~action:
                    [
                      S "with-stdout-to";
                      S "%{targets}";
                      [
                        S "chdir";
                        S "%{workspace_root}";
                        [
                          S "run";
                          S
                            "%{bin:octez-protocol-compiler.octez-protocol-packer}";
                          S "%{src_dir}";
                        ];
                      ];
                    ];
              ]
      in
      let embedded =
        tezos_protocol_sub_lib
          "embedded-protocol"
          ~internal_name:(sf "tezos_embedded_protocol_%s" name_underscore)
          ~path:(path // "lib_protocol")
          ~synopsis:
            (match number with
            | V _ as number when N.(number <= 003) ->
                sf
                  "Tezos/Protocol: %s (economic-protocol definition, embedded \
                   in `octez-node`)"
                  (if N.(number == 000) then name_dash else name_underscore)
            | Other ->
                sf
                  "Tezos/Protocol: %s (economic-protocol definition, embedded \
                   in `octez-node`)"
                  name_underscore
            | Dev | V _ ->
                "Tezos/Protocol: economic-protocol definition, embedded in \
                 `octez-node`")
          ~modules:["Registerer"]
          ~linkall:true
          ~flags:(Flags.standard ~disable_warnings ())
          ~release_status:
            (match (number, status) with
            | V _, (Active | Frozen | Overridden) ->
                (* Contrary to client libs and protocol plugin registerers,
                   embedded protocols are useful even when the protocol was overridden. *)
                Released
            | V _, Not_mainnet | (Dev | Other), _ ->
                (* Ideally we would not release the opam packages but this would require
                   removing the dependencies when releasing, both from .opam files
                   and dune files. *)
                Auto_opam)
          ~deps:[main; octez_protocol_updater; octez_protocol_environment]
          ~dune:
            Dune.
              [
                targets_rule
                  ["registerer.ml"]
                  ~deps:[modules_as_deps; [S ":src_dir"; S "TEZOS_PROTOCOL"]]
                  ~action:
                    [
                      S "with-stdout-to";
                      S "%{targets}";
                      [
                        S "chdir";
                        S "%{workspace_root}";
                        [
                          S "run";
                          S "%{bin:octez-embedded-protocol-packer}";
                          S "%{src_dir}";
                          S name_underscore;
                        ];
                      ];
                    ];
              ]
      in
      {main; lifted; embedded}
  end

  let genesis =
    let name = Name.other "genesis" in
    let {Lib_protocol.main; lifted; embedded} =
      Lib_protocol.make ~name ~status:Not_mainnet
    in
    let client =
      public_lib
        (sf "tezos-client-%s" (Name.name_dash name))
        ~path:(Name.base_path name // "lib_client")
        ~synopsis:"Tezos/Protocol: protocol specific library for `octez-client`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad";
            octez_shell_services |> open_;
            octez_client_base |> open_;
            octez_protocol_environment;
            main |> open_;
            lifted;
            octez_client_commands |> open_;
            octez_proxy;
            octez_stdlib_unix;
          ]
        ~linkall:true
    in
    register @@ make ~name ~status:Not_mainnet ~main ~embedded ~client ()

  let demo_noops =
    let name = Name.other "demo-noops" in
    let {Lib_protocol.main; lifted = _; embedded} =
      Lib_protocol.make ~name ~status:Not_mainnet
    in
    register @@ make ~name ~status:Not_mainnet ~main ~embedded ()

  let _demo_counter =
    let name = Name.other "demo-counter" in
    let {Lib_protocol.main; lifted; embedded} =
      Lib_protocol.make ~name ~status:Not_mainnet
    in
    let client =
      public_lib
        (sf "tezos-client-%s" (Name.name_dash name))
        ~path:(Name.base_path name // "lib_client")
        ~synopsis:"Tezos/Protocol: protocol specific library for `octez-client`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad";
            octez_shell_services |> open_;
            octez_client_base |> open_;
            octez_client_commands |> open_;
            main |> open_;
            lifted;
          ]
        ~linkall:true
    in
    register @@ make ~name ~status:Not_mainnet ~main ~embedded ~client ()

  let register_alpha_family status name =
    let short_hash = Name.short_hash name in
    let name_dash = Name.name_dash name in
    let name_underscore = Name.name_underscore name in
    let number = Name.number name in
    let path = Name.base_path name in
    (* Container of the registered sublibraries of [octez-protocol-libs] *)
    let registered_octez_protocol_libs = Sub_lib.make_container () in
    let octez_protocol_lib =
      Sub_lib.sub_lib
        ~package_synopsis:(sf "Octez protocol %s libraries" name_dash)
        ~container:registered_octez_protocol_libs
        ~package:(sf "octez-protocol-%s-libs" name_dash)
    in
    let active =
      match status with
      | Frozen | Overridden | Not_mainnet -> false
      | Active -> true
    in
    let not_overridden =
      match status with
      | Frozen | Active | Not_mainnet -> true
      | Overridden -> false
    in
    let executable_release_status =
      match (number, status) with
      | V _, (Active | Frozen) -> Released
      | V _, (Overridden | Not_mainnet) -> Unreleased
      | Dev, _ -> Experimental
      | Other, _ -> Unreleased
    in
    let optional_library_release_status =
      match (number, status) with
      | V _, (Active | Frozen) ->
          (* Put explicit dependency in meta-package octez.opam to force the optional
             dependency to be installed. *)
          Released
      | V _, (Overridden | Not_mainnet) | (Dev | Other), _ ->
          (* Ideally we would not release the opam packages but this would require
             removing the dependencies when releasing, both from .opam files
             and dune files. *)
          Auto_opam
    in
    let opt_map l f = Option.map f l in
    let both o1 o2 =
      match (o1, o2) with Some x, Some y -> Some (x, y) | _, _ -> None
    in
    let {Lib_protocol.main; lifted; embedded} =
      Lib_protocol.make ~name ~status
    in
    let dune_signatures_version_rule =
      Dune.targets_rule
        ["signature.ml"]
        ~action:
          [
            S "write-file";
            S "%{targets}";
            S
              (sf
                 {|  module Bls = Tezos_crypto.Signature.Bls
  module Ed25519 = Tezos_crypto.Signature.Ed25519
  module P256 = Tezos_crypto.Signature.P256
  module Secp256k1 = Tezos_crypto.Signature.Secp256k1%s
  include Tezos_crypto.Signature.V%d|}
                 (if N.(number <= 024) then ""
                  else {|
module Mldsa44 = Tezos_crypto.Signature.Mldsa44|})
                 (signature_version_number ~protocol_number:number));
          ]
    in
    let dune_client_keys_version_rule =
      Dune.(
        targets_rule
          ["client_keys.ml"]
          ~action:
            [
              S "write-file";
              S "%{targets}";
              S
                (sf
                   {|include Tezos_client_base.Client_keys_v%d|}
                   (signature_version_number ~protocol_number:number));
            ])
    in
    let parameters =
      only_if (N.(number >= 011) && not_overridden) @@ fun () ->
      public_lib
        (sf "tezos-protocol-%s.parameters" name_dash)
        ~path:(path // "lib_parameters")
        ~all_modules_except:["gen"]
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            octez_protocol_environment;
            main |> open_;
          ]
        ~linkall:true
        ~dune:(if N.(number >= 016) then [dune_signatures_version_rule] else [])
    in
    let _parameters_exe =
      opt_map parameters @@ fun parameters ->
      private_exe
        "gen"
        ~path:(path // "lib_parameters")
        ~opam:(sf "tezos-protocol-%s" name_dash)
        ~deps:
          [
            bls12_381_archive;
            octez_base |> open_ ~m:"TzPervasives";
            parameters |> open_;
            main |> if_ N.(number >= 012) |> open_;
          ]
        ~modules:["gen"]
        ~linkall:true
        ~with_macos_security_framework:true
        ~dune:
          Dune.(
            let gen_json name =
              targets_rule
                [name ^ "-parameters.json"]
                ~deps:[S "gen.exe"]
                ~action:[S "run"; S "%{deps}"; S ("--" ^ name)]
            in
            let networks = List.["sandbox"; "test"; "mainnet"] in
            let networks =
              if N.(number >= 017) then
                networks @ List.["mainnet-with-chain-id"]
              else networks
            in
            of_list
              (List.map gen_json networks
              @
              (* TODO: why do we install these files? *)
              List.
                [
                  install
                    (List.map (fun n -> S (n ^ "-parameters.json")) networks)
                    ~package:(sf "tezos-protocol-%s" name_dash)
                    ~section:"lib";
                ]))
        ~bisect_ppx:No
    in
    let octez_sc_rollup =
      only_if N.(number >= 016) @@ fun () ->
      octez_protocol_lib
        "smart-rollup"
        ~internal_name:(sf "tezos_smart_rollup_%s" name_dash)
        ~path:(path // "lib_sc_rollup")
        ~synopsis:
          "Protocol specific library of helpers for `tezos-smart-rollup`"
        ~deps:[octez_base |> open_ ~m:"TzPervasives"; main |> open_]
        ~inline_tests:ppx_expect
        ~inline_tests_libraries:[bls12_381_archive]
        ~inline_tests_link_flags:["-cclib"; "-lblst"]
        ~linkall:true
    in
    let plugin =
      only_if (N.(number >= 007) && not_overridden) @@ fun () ->
      octez_protocol_lib
        "plugin"
        ~internal_name:(sf "tezos_protocol_plugin_%s" name_dash)
        ~path:(path // "lib_plugin")
        ~synopsis:"Protocol plugin"
        ~documentation:
          [Dune.[S "package"; S (sf "octez-protocol-%s-libs" name_dash)]]
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            main |> open_;
            octez_sc_rollup |> if_some |> if_ N.(number >= 016) |> open_;
          ]
        ~all_modules_except:["Plugin_registerer"]
        ~bisect_ppx:(if N.(number >= 008) then Yes else No)
        ~dune:(if N.(number >= 016) then [dune_signatures_version_rule] else [])
    in
    let plugin_registerer =
      opt_map plugin @@ fun plugin ->
      octez_protocol_lib
        "plugin-registerer"
        ~internal_name:(sf "tezos_protocol_plugin_%s_registerer" name_dash)
        ~path:(path // "lib_plugin")
        ~synopsis:"Protocol plugin registerer"
        ~release_status:optional_library_release_status
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            embedded |> open_;
            plugin |> open_;
            octez_validation |> open_;
          ]
        ~modules:["Plugin_registerer"]
        ~bisect_ppx:(if N.(number >= 008) then Yes else No)
    in
    let client_name =
      if N.(number >= 015) then "`octez-client`" else "`tezos-client`"
    in
    let client =
      only_if not_overridden @@ fun () ->
      octez_protocol_lib
        "client"
        ~internal_name:(sf "tezos_client_%s" name_dash)
        ~path:(path // "lib_client")
        ~synopsis:("Protocol specific library for " ^ client_name)
        ~release_status:optional_library_release_status
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_clic;
            octez_shell_services |> open_;
            octez_client_base |> open_;
            main |> open_;
            lifted |> open_if N.(number >= 018);
            octez_mockup_registration |> if_ N.(number >= 011);
            octez_proxy |> if_ N.(number >= 011);
            octez_signer_backends |> if_ N.(number >= 001);
            plugin |> if_some |> open_if N.(number >= 008);
            parameters |> if_some |> if_ N.(number >= 011) |> open_;
            octez_rpc;
            octez_client_commands |> if_ N.(number == 000) |> open_;
            octez_stdlib_unix |> if_ N.(number == 000);
            octez_sc_rollup |> if_some |> if_ N.(number >= 016) |> open_;
            uri |> if_ N.(number >= 001);
          ]
        ~bisect_ppx:(if N.(number >= 008) then Yes else No)
        ?inline_tests:(if N.(number >= 009) then Some ppx_expect else None)
        ?inline_tests_libraries:
          (only_if N.(number >= 009) @@ fun () -> [bls12_381_archive])
        ?inline_tests_link_flags:
          (only_if N.(number >= 009) @@ fun () -> ["-cclib"; "-lblst"])
        ~linkall:true
        ~dune:
          (if N.(number >= 016) then
             [dune_client_keys_version_rule; dune_signatures_version_rule]
           else [])
    in
    let test_helpers =
      only_if active @@ fun () ->
      octez_protocol_lib
        "test-helpers"
        ~path:(path // "lib_protocol/test/helpers")
        ~internal_name:(sf "tezos_%s_test_helpers" name_underscore)
        ~synopsis:"Protocol testing framework"
        ~opam_only_deps:[octez_protocol_environment; parameters |> if_some]
        ~deps:
          [
            tezt_core_lib |> if_ N.(number >= 019) |> open_ |> open_ ~m:"Base";
            tezt_tezos |> if_ N.(number >= 019);
            tezt_lib |> if_ N.(number >= 019);
            qcheck_alcotest;
            octez_test_helpers;
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_micheline |> open_;
            octez_stdlib_unix |> open_;
            main |> open_;
            client |> if_some |> open_;
            parameters |> if_some |> open_;
            octez_protocol_environment;
            plugin |> if_some |> open_;
            octez_shell_services |> open_;
            octez_plompiler |> if_ N.(number >= 015);
            octez_crypto_dal |> if_ N.(number >= 016) |> open_;
            octez_sc_rollup |> if_some |> if_ N.(number >= 018) |> open_;
          ]
        ~dune:[dune_signatures_version_rule]
    in
    let test_migration =
      only_if active @@ fun () ->
      octez_protocol_lib
        "test-migration"
        ~path:(path // "lib_protocol/test/migration")
        ~internal_name:(sf "tezos_%s_test_migration" name_underscore)
        ~synopsis:"Protocol migration testing checks"
        ~opam_only_deps:[octez_protocol_environment; parameters |> if_some]
        ~deps:
          [
            tezt_migration_registry;
            alcotezt;
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            main |> open_;
            test_helpers |> if_some |> open_;
            octez_base_test_helpers |> open_;
            parameters |> if_some |> open_;
            plugin |> if_some |> open_;
          ]
        ~linkall:true
    in
    let _plugin_tests =
      opt_map (both plugin test_helpers) @@ fun (plugin, test_helpers) ->
      only_if active @@ fun () ->
      tezt
        [
          "helpers";
          "test_conflict_handler";
          "test_consensus_filter";
          "test_fee_needed_to_overtake";
          "test_fee_needed_to_replace_by_fee";
        ]
        ~path:(path // "lib_plugin/test")
        ~with_macos_security_framework:true
        ~opam:(sf "octez-protocol-%s-libs" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_base_test_helpers |> open_;
            octez_base_unix |> if_ N.(number >= 013);
            alcotezt;
            octez_test_helpers |> open_;
            qcheck_alcotest;
            octez_stdlib_unix;
            octez_micheline |> open_;
            plugin |> open_;
            main |> open_ |> open_ ~m:"Protocol";
            parameters |> if_some |> open_;
            test_helpers |> open_;
          ]
    in
    let _client_tests =
      only_if active @@ fun () ->
      tezt
        [
          "test_michelson_v1_macros";
          "test_client_proto_contracts";
          "test_client_proto_context";
          "test_proxy";
        ]
        ~path:(path // "lib_client/test")
        ~opam:(sf "octez-protocol-%s-libs" name_dash)
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_micheline |> open_;
            client |> if_some |> open_;
            main |> open_;
            octez_base_test_helpers |> open_;
            octez_test_helpers |> open_;
            alcotezt;
            qcheck_alcotest;
          ]
    in
    let client_commands =
      only_if (N.(number >= 001) && not_overridden) @@ fun () ->
      octez_protocol_lib
        "client.commands"
        ~internal_name:(sf "tezos_client_%s_commands" name_dash)
        ~path:(path // "lib_client_commands")
        ~deps:
          [
            octez_base
            |> if_ N.(number <= 14)
            |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_base |> if_ N.(number >= 15) |> open_ ~m:"TzPervasives";
            octez_clic;
            main |> open_;
            parameters |> if_some |> if_ N.(number >= 013) |> open_;
            octez_stdlib_unix |> open_;
            octez_protocol_environment;
            octez_shell_services |> open_;
            octez_mockup |> if_ N.(number >= 011);
            octez_mockup_registration |> if_ N.(number >= 011);
            octez_mockup_commands |> if_ N.(number >= 011);
            octez_client_base |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            octez_rpc;
            octez_client_base_unix |> if_ N.(number >= 009) |> open_;
            plugin |> if_some |> if_ N.(number >= 008) |> open_;
            (* uri used by the stresstest command introduced in 011 *)
            uri |> if_ N.(number >= 011);
          ]
        ~bisect_ppx:(if N.(number >= 008) then Yes else No)
        ~linkall:true
        ~all_modules_except:["alpha_commands_registration"]
        ~dune:
          (if N.(number >= 016) then
             [dune_signatures_version_rule; dune_client_keys_version_rule]
           else [])
    in
    let client_sapling =
      only_if (N.(number >= 011) && not_overridden) @@ fun () ->
      octez_protocol_lib
        "client.sapling"
        ~internal_name:(sf "tezos_client_sapling_%s" name_underscore)
        ~path:(path // "lib_client_sapling")
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_clic;
            octez_crypto;
            octez_stdlib_unix |> open_;
            octez_client_base |> open_;
            octez_signer_backends;
            client |> if_some |> open_;
            client_commands |> if_some |> open_;
            main |> open_;
            plugin |> if_some |> if_ N.(number >= 013) |> open_;
          ]
        ~linkall:true
    in
    let client_commands_registration =
      only_if (N.(number >= 001) && not_overridden) @@ fun () ->
      octez_protocol_lib
        "client.commands-registration"
        ~internal_name:(sf "tezos_client_%s_commands_registration" name_dash)
        ~path:(path // "lib_client_commands")
        ~deps:
          [
            octez_base
            |> if_ N.(number <= 14)
            |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_base |> if_ N.(number >= 15) |> open_ ~m:"TzPervasives";
            octez_clic;
            main |> open_;
            parameters |> if_some |> if_ N.(number >= 013) |> open_;
            octez_protocol_environment;
            octez_shell_services |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            client_commands |> if_some |> open_;
            client_sapling |> if_some |> if_ N.(number >= 011) |> open_;
            octez_rpc;
            plugin |> if_some |> if_ N.(number >= 008) |> open_;
          ]
        ~bisect_ppx:(if N.(number >= 008) then Yes else No)
        ~linkall:true
        ~modules:["alpha_commands_registration"]
    in
    let baking =
      only_if active @@ fun () ->
      let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
      octez_protocol_lib
        "baking"
        ~internal_name:("tezos_baking_" ^ name_dash)
        ~path:(path // "lib_delegate")
        ~synopsis:
          (if N.(number <= 011) then
             "Base library for `tezos-baker/endorser/accuser`"
           else "Base library for `tezos-baker/accuser`")
        ~preprocess
        ~preprocessor_deps
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_clic;
            octez_version_value;
            main |> open_;
            lifted |> if_ N.(number >= 018) |> open_;
            plugin |> if_some |> open_;
            octez_protocol_environment;
            octez_shell_services |> open_;
            octez_node_config;
            octez_client_base |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            octez_stdlib |> open_;
            octez_stdlib_unix |> open_;
            octez_context_memory |> if_ (N.(number >= 012) && N.(number <= 019));
            octez_profiler |> if_ N.(number >= 021) |> open_;
            octez_rpc_http_client_unix |> if_ N.(number >= 011);
            octez_rpc_http_client |> if_ N.(number >= 011) |> open_;
            octez_context_ops |> if_ N.(number >= 011) |> open_;
            octez_rpc;
            octez_rpc_http |> open_;
            octez_crypto_dal |> open_;
            octez_dal_node_services |> if_ N.(number >= 017);
            lwt_canceler;
            lwt_exit;
            uri;
            memtrace;
            octez_agnostic_baker_lib |> open_;
            octez_baking_common_lib;
          ]
        ~linkall:true
        ~all_modules_except:
          (if N.(number <= 011) then
             ["Delegate_commands"; "Delegate_commands_registration"]
           else ["Baking_commands"; "Baking_commands_registration"])
        ~dune:[dune_signatures_version_rule; dune_client_keys_version_rule]
    in
    let tenderbrute =
      only_if active @@ fun () ->
      octez_protocol_lib
        "baking.tenderbrute"
        ~internal_name:(sf "tenderbrute_%s" name_underscore)
        ~path:(path // "lib_delegate/test/tenderbrute/lib")
        ~deps:
          [
            data_encoding |> open_;
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018)
            |> open_;
            octez_base_unix;
            main |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
          ]
        ~bisect_ppx:No
        ~dune:[dune_signatures_version_rule]
    in
    let _tenderbrute_exe =
      only_if (active && N.(number >= 013)) @@ fun () ->
      test
        "tenderbrute_main"
        ~alias:""
        ~path:(path // "lib_delegate/test/tenderbrute")
        ~with_macos_security_framework:true
        ~opam:(sf "octez-protocol-%s-libs" name_dash)
        ~deps:
          [
            bls12_381_archive;
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018)
            |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
            main |> open_;
            tenderbrute |> if_some |> open_;
          ]
        ~linkall:true
    in
    let _baking_tests =
      opt_map (both baking test_helpers) @@ fun (baking, test_helpers) ->
      only_if active @@ fun () ->
      let mockup_simulator =
        only_if N.(number >= 012) @@ fun () ->
        octez_protocol_lib
          "bakings.mockup-simulator"
          ~internal_name:(sf "tezos_%s_mockup_simulator" name_underscore)
          ~path:(path // "lib_delegate/test/mockup_simulator")
          ~deps:
            [
              octez_base |> open_ ~m:"TzPervasives"
              |> error_monad_module N.(number <= 018);
              main |> open_ |> open_ ~m:"Protocol";
              client |> if_some |> open_;
              octez_client_commands |> open_;
              baking |> open_;
              octez_stdlib_unix |> open_;
              octez_client_base_unix |> open_;
              parameters |> if_some |> open_;
              octez_mockup;
              octez_mockup_proxy;
              octez_mockup_commands;
              tenderbrute |> if_some |> if_ N.(number >= 013) |> open_;
              tezt_core_lib |> open_;
            ]
          ~bisect_ppx:No
          ~dune:[dune_client_keys_version_rule]
      in
      tezt
        ["test_scenario"]
        ~path:(path // "lib_delegate/test")
        ~with_macos_security_framework:true
        ~opam:(sf "octez-protocol-%s-libs" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_protocol_environment |> if_ N.(number <= 011);
            octez_test_helpers |> open_;
            octez_micheline |> open_;
            client |> if_some |> open_;
            main |> open_;
            test_helpers |> if_ N.(number <= 011) |> open_;
            octez_base_test_helpers |> open_;
            mockup_simulator |> if_some |> open_;
            octez_client_base |> if_ N.(number <= 011);
            baking |> open_;
            parameters |> if_some |> if_ N.(number >= 012);
            octez_crypto |> if_ N.(number >= 012);
            octez_event_logging_test_helpers |> open_;
            uri;
          ]
    in
    let baking_commands =
      only_if active @@ fun () ->
      octez_protocol_lib
        "baking-commands"
        ~internal_name:(sf "tezos_baking_%s_commands" name_dash)
        ~path:(path // "lib_delegate")
        ~synopsis:"Protocol-specific commands for baking"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            main |> open_;
            parameters |> if_some |> if_ N.(number >= 18) |> open_;
            octez_stdlib_unix |> open_;
            octez_protocol_environment;
            octez_shell_services |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            baking |> if_some |> open_;
            octez_rpc;
            uri;
          ]
        ~linkall:true
        ~modules:
          [
            (if N.(number <= 011) then "Delegate_commands" else "Baking_commands");
          ]
    in
    let baking_commands_registration =
      only_if active @@ fun () ->
      octez_protocol_lib
        "baking-commands.registration"
        ~internal_name:(sf "tezos_baking_%s_commands_registration" name_dash)
        ~path:(path // "lib_delegate")
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            main |> open_;
            octez_protocol_environment;
            octez_shell_services |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            baking |> if_some |> open_;
            baking_commands |> if_some |> open_;
            octez_rpc;
          ]
        ~linkall:true
        ~modules:
          [
            (if N.(number <= 011) then "Delegate_commands_registration"
             else "Baking_commands_registration");
          ]
    in
    let daemon daemon =
      only_if active @@ fun () ->
      public_exe
        (sf "octez-%s-%s" daemon short_hash)
        ~internal_name:(sf "main_%s_%s" daemon name_underscore)
        ~path:(path // sf "bin_%s" daemon)
        ~synopsis:(sf "Tezos/Protocol: %s binary" daemon)
        ~release_status:executable_release_status
        ~with_macos_security_framework:true
        ~deps:
          [
            bls12_381_archive;
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_clic;
            main |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            baking_commands |> if_some |> open_;
            octez_stdlib_unix |> open_;
            octez_client_base_unix |> open_;
            octez_profiler_backends |> open_;
          ]
    in
    let _baker = daemon "baker" in
    let _accuser = daemon "accuser" in
    let _endorser = only_if N.(number <= 011) @@ fun () -> daemon "endorser" in
    let layer2_utils =
      only_if N.(number >= 016) @@ fun () ->
      octez_protocol_lib
        "layer2-utils"
        ~internal_name:(sf "tezos_layer2_utils_%s" name_dash)
        ~path:(path // "lib_layer2_utils")
        ~synopsis:"Protocol specific library for Layer 2 utils"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            main |> open_;
            client |> if_some |> open_;
          ]
        ~inline_tests:ppx_expect
        ~inline_tests_libraries:[bls12_381_archive]
        ~inline_tests_link_flags:["-cclib"; "-lblst"]
        ~linkall:true
    in
    let dal =
      only_if N.(number >= 021) @@ fun () ->
      octez_protocol_lib
        "dal"
        ~internal_name:(sf "tezos_dal_%s" name_dash)
        ~path:(path // "lib_dal")
        ~synopsis:"Protocol specific library for the Data availability Layer"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_protocol_compiler_registerer |> open_;
            octez_stdlib_unix |> open_;
            octez_shell_services |> open_;
            octez_dal_node_lib |> open_;
            client |> if_some |> open_;
            plugin |> if_some |> open_;
            embedded |> open_;
            layer2_utils |> if_some |> open_;
            main |> open_;
          ]
        ~inline_tests:ppx_expect
        ~inline_tests_libraries:[bls12_381_archive]
        ~inline_tests_link_flags:["-cclib"; "-lblst"]
        ~linkall:true
        ~dune:[dune_signatures_version_rule]
    in
    let _dal_tests =
      only_if (active && N.(number >= 016)) @@ fun () ->
      tezt
        ["test_dal_slot_frame_encoding"; "test_helpers"]
        ~path:(path // "lib_dal/test")
        ~opam:(sf "octez-protocol-%s-libs" name_dash)
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            dal |> if_some |> open_;
            main |> open_;
            octez_base_test_helpers |> open_;
            test_helpers |> if_some |> open_;
            alcotezt;
          ]
    in
    let agnostic_baker =
      only_if (active && N.(number >= 021)) @@ fun () ->
      octez_protocol_lib
        "agnostic-baker"
        ~internal_name:(sf "tezos_agnostic_baker_%s" name_dash)
        ~path:(path // "lib_agnostic_baker")
        ~synopsis:"Protocol specific library for the Agnostic Baker"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            client |> if_some |> open_;
            main |> open_;
            baking |> if_some |> open_;
            baking_commands |> if_some |> open_;
            octez_agnostic_baker_lib |> open_;
          ]
        ~linkall:true
    in
    let octez_injector =
      only_if N.(active && number >= 017) @@ fun () ->
      private_lib
        (sf "octez_injector_%s" short_hash)
        ~path:(path // "lib_injector")
        ~synopsis:
          "Tezos/Protocol: protocol-specific library for the injector binary"
        ~opam:(sf "tezos-injector-%s" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            main |> open_;
            octez_injector_lib |> open_;
            client |> if_some |> open_;
            octez_client_base |> open_;
            plugin |> if_some |> open_;
          ]
        ~linkall:true
        ~dune:[dune_client_keys_version_rule; dune_signatures_version_rule]
    in
    let octez_sc_rollup_layer2 =
      only_if N.(number >= 016) @@ fun () ->
      octez_protocol_lib
        "smart-rollup-layer2"
        ~internal_name:(sf "tezos_smart_rollup_layer2_%s" name_dash)
        ~path:(path // "lib_sc_rollup_layer2")
        ~synopsis:"Protocol specific library for `tezos-smart-rollup`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            main |> open_;
            octez_injector_lib |> open_;
            octez_smart_rollup_lib |> open_;
          ]
        ~inline_tests:ppx_expect
        ~inline_tests_libraries:[bls12_381_archive]
        ~inline_tests_link_flags:["-cclib"; "-lblst"]
        ~linkall:true
    in
    let octez_sc_rollup_node =
      (* For now, we want to keep this for Oxford and above to be able to
         replay Etherlink Mainnet from genesis. *)
      only_if N.(number >= 018) @@ fun () ->
      private_lib
        (sf "octez_smart_rollup_node_%s" short_hash)
        ~path:(path // "lib_sc_rollup_node")
        ~opam:(sf "octez-smart-rollup-node-%s" short_hash)
        ~synopsis:
          (sf
             "Protocol specific (for %s) library for smart rollup node"
             name_dash)
        ~linkall:true
        ~deps:
          [
            octez_base |> open_ |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_stdlib_unix |> open_;
            octez_client_base |> open_;
            octez_client_base_unix |> open_;
            client |> if_some |> open_;
            dal |> if_some |> if_ N.(number >= 019) |> open_;
            octez_context_encoding;
            octez_context_helpers;
            main |> open_;
            plugin |> if_some |> open_;
            parameters |> if_some |> open_;
            octez_rpc;
            octez_rpc_http;
            octez_rpc_http_server;
            octez_workers |> open_;
            octez_dal_node_services;
            octez_dal_node_lib |> open_;
            octez_shell_services |> open_;
            octez_smart_rollup_lib |> open_;
            octez_sc_rollup |> if_some |> open_;
            octez_sc_rollup_layer2 |> if_some |> open_;
            layer2_utils |> if_some |> open_;
            octez_layer2_store |> open_;
            octez_layer2_riscv_context |> open_;
            octez_layer2_irmin_context |> open_;
            octez_crawler |> open_;
            tree_encoding;
            data_encoding;
            irmin_pack;
            irmin_pack_unix;
            irmin;
            aches;
            aches_lwt;
            octez_injector_lib |> open_;
            octez_smart_rollup_node_lib |> open_;
            octez_scoru_wasm;
            octez_scoru_wasm_fast;
            octez_riscv_pvm |> if_ N.(number >= 021);
            octez_crypto_dal |> if_ N.(number >= 016) |> open_;
            octez_version_value;
          ]
        ~conflicts:[Conflicts.checkseum]
        ~dune:[dune_signatures_version_rule]
    in
    let _octez_sc_rollup_node_test =
      only_if (active && N.(number >= 016)) @@ fun () ->
      tezt
        ["serialized_proofs"; "test_octez_conversions"]
        ~path:(path // "lib_sc_rollup_node/test")
        ~opam:"tezos-sc-rollup-node-test"
        ~synopsis:"Tests for the smart rollup node library"
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            main |> open_;
            octez_test_helpers |> open_;
            octez_sc_rollup_layer2 |> if_some |> open_;
            octez_sc_rollup_node |> if_some |> open_;
            alcotezt;
          ]
    in
    let benchmark_type_inference =
      only_if active @@ fun () ->
      public_lib
        (sf "tezos-benchmark-type-inference-%s" name_dash)
        ~path:(path // "lib_benchmark/lib_benchmark_type_inference")
        ~synopsis:"Tezos: type inference for partial Michelson expressions"
        ~deps:
          [
            octez_stdlib |> open_;
            octez_error_monad |> open_;
            octez_crypto |> open_;
            octez_micheline |> open_;
            octez_micheline_rewriting |> open_;
            main |> open_;
            hashcons;
          ]
    in
    let _benchmark_type_inference_tests =
      only_if active @@ fun () ->
      tests
        ["test_uf"; "test_inference"]
        ~path:(path // "lib_benchmark/lib_benchmark_type_inference/test")
        ~opam:(sf "tezos-benchmark-type-inference-%s" name_dash)
        ~with_macos_security_framework:true
        ~deps:
          [
            bls12_381_archive;
            octez_micheline |> open_;
            octez_micheline_rewriting;
            benchmark_type_inference |> if_some |> open_;
            main;
            octez_error_monad;
            client |> if_some;
          ]
    in
    let benchmark =
      opt_map test_helpers @@ fun test_helpers ->
      only_if active @@ fun () ->
      public_lib
        (sf "tezos-benchmark-%s" name_dash)
        ~path:(path // "lib_benchmark")
        ~synopsis:
          "Tezos/Protocol: library for writing benchmarks (protocol-specific \
           part)"
        ~deps:
          [
            octez_stdlib |> open_;
            octez_base |> open_ |> error_monad_module N.(number <= 018);
            octez_error_monad |> open_;
            octez_micheline |> open_;
            octez_micheline_rewriting |> open_;
            octez_benchmark |> open_;
            benchmark_type_inference |> if_some |> open_;
            main |> open_;
            octez_crypto;
            parameters |> if_some;
            hashcons;
            test_helpers |> open_;
            prbnmcn_stats;
          ]
        ~linkall:true
        ~private_modules:["kernel"; "rules"; "state_space"]
        ~bisect_ppx:(if N.(number <= 012) then Yes else No)
        ~dune:
          [
            Dune.targets_rule
              ["crypto_samplers.mli"]
              ~action:
                [
                  S "write-file";
                  S "%{targets}";
                  S
                    (sf
                       "include module type of \
                        Tezos_benchmark.Crypto_samplers.V%d"
                       (signature_version_number ~protocol_number:number));
                ];
            Dune.targets_rule
              ["crypto_samplers.ml"]
              ~action:
                [
                  S "write-file";
                  S "%{targets}";
                  S
                    (sf
                       "include Tezos_benchmark.Crypto_samplers.V%d"
                       (signature_version_number ~protocol_number:number));
                ];
          ]
    in
    let _benchmark_tests =
      opt_map (both benchmark test_helpers) @@ fun (benchmark, test_helpers) ->
      only_if active @@ fun () ->
      (* Note: to enable gprof profiling,
         manually add the following stanza to lib_benchmark/test/dune:
         (ocamlopt_flags (:standard -p -ccopt -no-pie)) *)
      tests
        [
          "test_sampling_data";
          "test_sampling_code";
          "test_autocompletion";
          "test_distribution";
        ]
        ~path:(path // "lib_benchmark/test")
        ~with_macos_security_framework:true
        ~opam:(sf "tezos-benchmark-%s" name_dash)
        ~deps:
          [
            bls12_381_archive;
            octez_base |> error_monad_module N.(number <= 018);
            octez_micheline |> open_;
            octez_micheline_rewriting;
            main |> open_;
            octez_benchmark |> open_;
            benchmark_type_inference |> if_some |> open_;
            benchmark |> if_some |> open_;
            test_helpers |> open_;
            octez_error_monad;
            prbnmcn_stats;
          ]
        ~alias:""
        ~dune:
          Dune.
            [
              alias_rule
                "runtest_micheline_rewriting_data"
                ~action:(run_exe "test_sampling_data" ["1234"]);
              alias_rule
                "runtest_micheline_rewriting_code"
                ~action:(run_exe "test_sampling_code" ["1234"]);
            ]
    in
    let benchmarks_proto : Manifest.target option =
      Option.bind (both benchmark test_helpers)
      @@ fun (benchmark, test_helpers) ->
      only_if active @@ fun () ->
      public_lib
        (sf "tezos-benchmarks-proto-%s" name_dash)
        ~path:(path // "lib_benchmarks_proto")
        ~synopsis:"Tezos/Protocol: protocol benchmarks"
        ~deps:
          [
            str;
            octez_stdlib |> open_;
            octez_base |> open_ |> open_ ~m:"TzPervasives"
            |> error_monad_module N.(number <= 018);
            octez_error_monad |> open_;
            parameters |> if_some |> open_;
            lazy_containers |> open_;
            octez_benchmark |> open_;
            benchmark |> if_some |> open_;
            benchmark_type_inference |> if_some |> open_;
            main |> open_ |> open_ ~m:"Protocol";
            octez_crypto;
            octez_shell_benchmarks;
            octez_micheline |> open_;
            test_helpers |> open_;
            octez_sapling;
            client |> if_some |> open_;
            plugin |> if_some |> open_;
            octez_protocol_environment;
          ]
        ~linkall:true
        ~dune:[dune_signatures_version_rule]
    in
    let _ =
      if active then
        Lib_protocol.make_tests
          ?test_helpers
          ?parameters
          ?plugin
          ?client
          ?benchmark:(Option.bind benchmark Fun.id)
          ?benchmark_type_inference
          ?octez_sc_rollup
          ~main
          ~name
          ()
    in
    (* Generate documentation index for [octez-protocol-%s-libs] *)
    let () =
      write (path // "lib_plugin/index.mld") @@ fun fmt ->
      let header =
        sf
          "{0 Octez-protocol-%s-libs: octez protocol %s libraries}\n\n\
           This is a package containing some libraries related to the Tezos %s \
           protocol.\n\n\
           It contains the following libraries:\n\n"
          name_dash
          name_dash
          name_dash
      in
      Manifest.Sub_lib.pp_documentation_of_container
        ~header
        fmt
        registered_octez_protocol_libs
    in
    register
    @@ make
         ~status
         ~name
         ~main
         ~embedded
         ?client
         ?client_commands
         ?client_commands_registration
         ?baking_commands_registration
         ?plugin
         ?plugin_registerer
         ?dal
         ?agnostic_baker
         ?test_helpers
         ?test_migration
         ?parameters
         ?benchmarks_proto
         ?baking
         ?octez_sc_rollup
         ?octez_sc_rollup_node
         ?octez_injector
         ()

  let active = register_alpha_family Active

  let frozen = register_alpha_family Frozen

  let overridden = register_alpha_family Overridden

  let _000_Ps9mPmXa = frozen (Name.v "Ps9mPmXa" 000)

  let _001_PtCJ7pwo = frozen (Name.v "PtCJ7pwo" 001)

  let _002_PsYLVpVv = frozen (Name.v "PsYLVpVv" 002)

  let _003_PsddFKi3 = frozen (Name.v "PsddFKi3" 003)

  let _004_Pt24m4xi = frozen (Name.v "Pt24m4xi" 004)

  let _005_PsBABY5H = overridden (Name.v "PsBABY5H" 005)

  let _005_PsBabyM1 = frozen (Name.v "PsBabyM1" 005)

  let _006_PsCARTHA = frozen (Name.v "PsCARTHA" 006)

  let _007_PsDELPH1 = frozen (Name.v "PsDELPH1" 007)

  let _008_PtEdoTez = overridden (Name.v "PtEdoTez" 008)

  let _008_PtEdo2Zk = frozen (Name.v "PtEdo2Zk" 008)

  let _009_PsFLoren = frozen (Name.v "PsFLoren" 009)

  let _010_PtGRANAD = frozen (Name.v "PtGRANAD" 010)

  let _011_PtHangz2 = frozen (Name.v "PtHangz2" 011)

  let _012_Psithaca = frozen (Name.v "Psithaca" 012)

  let _013_PtJakart = frozen (Name.v "PtJakart" 013)

  let _014_PtKathma = frozen (Name.v "PtKathma" 014)

  let _015_PtLimaPt = frozen (Name.v "PtLimaPt" 015)

  let _016_PtMumbai = frozen (Name.v "PtMumbai" 016)

  let _017_PtNairob = frozen (Name.v "PtNairob" 017)

  let _018_Proxford = frozen (Name.v "Proxford" 018)

  let _019_PtParisB = frozen (Name.v "PtParisB" 019)

  let _020_PsParisC = frozen (Name.v "PsParisC" 020)

  let _021_PsQuebec = frozen (Name.v "PsQuebec" 021)

  let _022_PsRiotum = frozen (Name.v "PsRiotum" 022)

  let _023_PtSeouLo = active (Name.v "PtSeouLo" 023)

  let _024_PtTALLiN = active (Name.v "PtTALLiN" 024)

  let alpha = active (Name.dev "alpha")

  let all = List.rev !all_rev

  let active = List.filter (fun p -> p.baking_commands_registration <> None) all

  let all_optionally (get_packages : (t -> target option) list) =
    let get_targets_for_protocol protocol =
      List.filter_map (fun get_package -> get_package protocol) get_packages
    in
    List.map get_targets_for_protocol all |> List.flatten |> List.map optional
end

(* TESTS THAT USE PROTOCOLS *)

let _octez_micheline_rewriting_tests =
  tezt
    ["test_rewriting"]
    ~path:"src/lib_benchmark/lib_micheline_rewriting/test"
    ~with_macos_security_framework:true
    ~opam:"tezos-micheline-rewriting"
    ~deps:
      [
        octez_micheline |> open_;
        octez_micheline_rewriting;
        Protocol.(main alpha);
        octez_error_monad;
        Protocol.(client_exn alpha);
      ]

let octez_store_tests =
  tezt
    [
      "test";
      "test_snapshots";
      "test_reconstruct";
      "test_history_mode_switch";
      "alpha_utils";
      "test_consistency";
      "test_locator";
      "test_cemented_store";
      "test_block_store";
      "test_protocol_store";
      "test_store";
      "test_testchain";
      "test_utils";
      "assert_lib";
    ]
    ~path:"src/lib_store/unix/test"
    ~with_macos_security_framework:true
    ~opam:"octez-store-tests"
    ~synopsis:"Store tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_crypto |> open_;
        octez_context_ops |> open_;
        octez_store_shared |> open_;
        octez_store_unix |> open_;
        octez_store_unix_reconstruction |> open_;
        octez_store_unix_snapshots |> open_;
        octez_shell_services |> open_;
        octez_stdlib_unix |> open_;
        octez_validation |> open_;
        octez_protocol_updater |> open_;
        Protocol.(embedded demo_noops);
        Protocol.(embedded genesis);
        Protocol.(embedded alpha);
        Protocol.(parameters_exn alpha |> open_);
        Protocol.(plugin_exn alpha) |> open_;
        alcotezt;
        tezt_lib;
        octez_test_helpers |> open_;
        octez_event_logging_test_helpers |> open_;
      ]

(* [_octez_bench_store_lib_tests_exe] is a bench for the store locator,
   We do not run these tests in the CI. *)
let _octez_bench_store_lib_tests_exe =
  private_exe
    "bench"
    ~path:"src/lib_store/unix/test/bench"
    ~with_macos_security_framework:true
    ~synopsis:"Bench store lib tests"
    ~opam:""
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tezt_lib;
        alcotezt;
        octez_store_tests |> open_;
      ]

(* [_octez_slow_store_lib_tests_exe] is a very long test, running a huge
   combination of tests that are useful for local testing for a
   given test suite. In addition to that, there is a memory leak
   is the tests (that could be in alcotest) which makes the test
   to consumes like > 10Gb of ram. For these reasons, we do not
   run these tests in the CI. *)
let _octez_slow_store_lib_tests_exe =
  private_exe
    "test_slow"
    ~path:"src/lib_store/unix/test/slow"
    ~with_macos_security_framework:true
    ~synopsis:"Slow store lib tests"
    ~modules:["test_slow"]
    ~opam:""
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tezt_lib;
        alcotezt;
        octez_store_tests |> open_;
      ]

let _octez_shell_tests =
  tezt
    [
      "generators";
      "generators_tree";
      "shell_test_helpers";
      "test_consensus_heuristic";
      "test_node";
      "test_peer_validator";
      "test_prevalidator";
      "test_prevalidation";
      "test_prevalidator_bounding";
      "test_prevalidator_classification";
      "test_prevalidator_classification_operations";
      "test_prevalidator_pending_operations";
      "test_protocol_validator";
      "test_shell_operation";
      "test_synchronisation_heuristic";
      "test_synchronisation_heuristic_fuzzy";
      "test_validator";
    ]
    ~path:"src/lib_shell/test"
    ~with_macos_security_framework:true
    ~opam:"octez-shell-tests"
    ~synopsis:"Shell tests"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_test_helpers |> open_;
        octez_store |> open_;
        octez_store_shared |> open_;
        octez_context_ops |> open_;
        octez_protocol_updater |> open_;
        octez_p2p |> open_;
        octez_p2p_services |> open_;
        octez_requester;
        octez_shell |> open_;
        octez_shell_services |> open_;
        Protocol.(embedded demo_noops);
        octez_stdlib_unix |> open_;
        octez_validation |> open_;
        octez_event_logging_test_helpers |> open_;
        octez_test_helpers |> open_;
        alcotezt;
        octez_version_value;
        octez_requester_tests |> open_;
      ]

(* INTERNAL EXES *)

(* Not released, so no ~opam. *)
let remove_if_exists fname = if Sys.file_exists fname then Sys.remove fname

let get_contracts_lib =
  let get_contracts_module proto =
    "devtools" // "get_contracts"
    // (sf "get_contracts_%s.ml" @@ Protocol.name_underscore proto)
  in
  let protocols =
    List.filter_map
      (fun proto ->
        let get_contracts_ml = get_contracts_module proto in
        match (Protocol.status proto, Protocol.client proto) with
        | Active, Some client ->
            (if not @@ Sys.file_exists get_contracts_ml then
               let contents =
                 file_content @@ get_contracts_module Protocol.alpha
               in
               let contents =
                 Str.global_replace
                   (Str.regexp_string "open Tezos_protocol_alpha")
                   ("open Tezos_protocol_" ^ Protocol.name_underscore proto)
                   contents
               in
               let contents =
                 Str.global_replace
                   (Str.regexp_string "open Tezos_client_alpha")
                   ("open Tezos_client_" ^ Protocol.name_underscore proto)
                   contents
               in
               write get_contracts_ml (fun fmt ->
                   Format.pp_print_string fmt contents)) ;
            Some [Protocol.main proto; client]
        | _ ->
            remove_if_exists get_contracts_ml ;
            None)
      Protocol.all
  in
  private_lib
    "get_contracts_lib"
    ~path:("devtools" // "get_contracts")
    ~release_status:Unreleased
    ~synopsis:"Generic tool to extract smart contracts from node's context."
    ~opam:""
    ~deps:
      ([
         octez_micheline |> open_;
         octez_base |> open_ ~m:"TzPervasives";
         octez_store;
       ]
      @ List.flatten protocols)
    ~all_modules_except:["get_contracts"]
    ~bisect_ppx:No
    ~linkall:true

let _get_contracts =
  private_exe
    "get_contracts"
    ~path:("devtools" // "get_contracts")
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~synopsis:"A script to extract smart contracts from a node."
    ~opam:""
    ~deps:
      [
        bls12_381_archive;
        octez_micheline |> open_;
        octez_base |> open_ ~m:"TzPervasives";
        get_contracts_lib |> open_;
      ]
    ~modules:["get_contracts"]
    ~bisect_ppx:No

let _proto_context_du =
  public_exe
    "proto_context_du"
    ~internal_name:"main"
    ~path:("devtools" // "proto_context_du")
    ~with_macos_security_framework:true
    ~release_status:Unreleased
    ~synopsis:"A script to print protocol context disk usage"
    ~opam:"internal-devtools_proto-context-du"
    ~deps:
      [
        bls12_381_archive;
        octez_clic;
        octez_base |> open_ ~m:"TzPervasives";
        octez_node_config;
        octez_store;
        Protocol.(main alpha);
        Protocol.(client_exn alpha);
      ]
    ~bisect_ppx:No

let yes_wallet_lib =
  let get_delegates_module proto =
    "devtools" // "yes_wallet"
    // (sf "get_delegates_%s.ml" @@ Protocol.name_underscore proto)
  in
  let protocols =
    List.filter_map
      (fun proto ->
        let get_delegates_ml = get_delegates_module proto in
        match Protocol.status proto with
        | Active ->
            (if not @@ Sys.file_exists get_delegates_ml then
               let contents =
                 file_content @@ get_delegates_module Protocol.alpha
               in
               let contents =
                 Str.global_replace
                   (Str.regexp_string "open Tezos_protocol_alpha")
                   ("open Tezos_protocol_" ^ Protocol.name_underscore proto)
                   contents
               in
               write get_delegates_ml (fun fmt ->
                   Format.pp_print_string fmt contents)) ;
            Some (Protocol.main proto)
        | _ ->
            remove_if_exists get_delegates_ml ;
            None)
      Protocol.all
  in
  private_lib
    "yes_wallet_lib"
    ~path:("devtools" // "yes_wallet")
    ~release_status:Unreleased
    ~synopsis:"A development tool for extracting baker keys from a context."
    ~opam:""
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         lwt_unix;
         ezjsonm;
         octez_node_config;
         octez_store;
       ]
      @ protocols)
    ~all_modules_except:["yes_wallet"]
    ~bisect_ppx:No
    ~linkall:true

let _yes_wallet =
  private_exe
    "yes_wallet"
    ~path:("devtools" // "yes_wallet")
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~synopsis:
      "A script extracting delegates' keys from a context into a wallet."
    ~opam:""
    ~deps:[bls12_381_archive; yes_wallet_lib |> open_]
    ~modules:["yes_wallet"]
    ~bisect_ppx:No

let _yes_wallet_test =
  private_exe
    "bench_signature_perf"
    ~path:("devtools" // "yes_wallet" // "test")
    ~release_status:Unreleased
    ~synopsis:"Tests for yes_wallet tool"
    ~opam:""
    ~deps:
      [
        bls12_381_archive;
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        octez_crypto;
        zarith;
        data_encoding |> open_;
        lwt_unix;
        ptime;
      ]
    ~bisect_ppx:No

let _testnet_experiment_tools =
  private_exe
    "testnet_experiment_tools"
    ~path:("devtools" // "testnet_experiment_tools")
    ~release_status:Unreleased
    ~synopsis:
      "Suite of tools to support the execution of stresstests on testnets"
    ~bisect_ppx:No
    ~static:false
    ~with_macos_security_framework:true
    ~opam:""
    ~deps:
      [
        bls12_381_archive;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos;
        octez_client_base_unix |> open_;
        octez_node_config;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        Protocol.(client_exn alpha);
        Protocol.(main alpha);
        Protocol.(plugin alpha) |> if_some;
      ]
    ~modules:["testnet_experiment_tools"; "format_baker_accounts"]

let simulation_scenario_lib =
  let proto_deps, proto_tools =
    let proto_tool proto = sf "tool_%s" @@ Protocol.name_underscore proto in
    let get_tool_module proto =
      "devtools" // "testnet_experiment_tools" // (proto_tool proto ^ ".ml")
    in
    let alpha_tool = get_tool_module Protocol.alpha in
    List.filter_map
      (fun proto ->
        let tool_path = get_tool_module proto in
        match (Protocol.status proto, Protocol.client proto) with
        | Active, Some _client ->
            (if not @@ Sys.file_exists tool_path then
               let contents = file_content @@ alpha_tool in
               let contents =
                 List.fold_left
                   (fun contents (re, replace) ->
                     Str.global_replace re replace contents)
                   contents
                   [
                     ( Str.regexp_string "open Tezos_client_alpha",
                       "open Tezos_client_" ^ Protocol.name_underscore proto );
                     ( Str.regexp_string "open Tezos_baking_alpha",
                       "open Tezos_baking_" ^ Protocol.name_underscore proto );
                     ( Str.regexp_string "open Tezos_protocol_alpha",
                       "open Tezos_protocol_" ^ Protocol.name_underscore proto
                     );
                     ( Str.regexp_string
                         "module Alpha_services = \
                          Tezos_protocol_plugin_alpha.Plugin.Alpha_services",
                       "module Alpha_services = Tezos_protocol_plugin_"
                       ^ Protocol.name_underscore proto
                       ^ ".Plugin.Alpha_services" );
                   ]
               in
               write tool_path (fun fmt -> Format.pp_print_string fmt contents)) ;
            let proto_deps =
              Protocol.
                [
                  baking_exn proto;
                  client_exn proto;
                  client_commands_exn proto;
                  main proto;
                ]
            in
            Some (proto_deps, proto_tool proto)
        | _ ->
            remove_if_exists tool_path ;
            None)
      Protocol.all
    |> List.split
  in
  private_lib
    "simulation_scenario_lib"
    ~path:("devtools" // "testnet_experiment_tools")
    ~release_status:Unreleased
    ~synopsis:"Simulation scenario lib"
    ~opam:""
    ~deps:
      ([
         octez_stdlib_unix |> open_;
         octez_base |> open_ |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         octez_store |> open_;
         octez_store_shared |> open_;
         octez_context_ops |> open_;
       ]
      @ List.flatten proto_deps)
    ~modules:("sigs" :: proto_tools)
    ~bisect_ppx:No
    ~linkall:true

let _simulation_scenario =
  private_exe
    "simulation_scenario"
    ~path:("devtools" // "testnet_experiment_tools")
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~synopsis:
      "A script creating a simulation scenario from a tezos node directory."
    ~opam:""
    ~deps:
      [
        bls12_381_archive;
        octez_stdlib_unix |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_store |> open_;
        octez_clic;
        octez_store_unix_snapshots |> open_;
        octez_store_shared |> open_;
        octez_node_config |> open_;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        simulation_scenario_lib |> open_;
      ]
    ~modules:["simulation_scenario"]
    ~bisect_ppx:No
    ~linkall:true

let _extract_data =
  private_exe
    "extract_data"
    ~path:("devtools" // "testnet_experiment_tools")
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~synopsis:"A script to extract data from profiling."
    ~opam:""
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_store |> open_;
        octez_clic;
        octez_client_base_unix |> open_;
      ]
    ~modules:["extract_data"]
    ~bisect_ppx:No
    ~linkall:true

let _safety_checker =
  private_exe
    "safety_checker"
    ~path:("devtools" // "testnet_experiment_tools")
    ~with_macos_security_framework:true
    ~release_status:Unreleased
    ~synopsis:
      "A script for checking the safety of the reducing block time experiment."
    ~opam:""
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_store |> open_;
        octez_clic;
        octez_node_config |> open_;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
      ]
    ~modules:["safety_checker"]
    ~bisect_ppx:No
    ~linkall:true

let _get_teztale_data =
  private_exe
    "get_teztale_data"
    ~path:("devtools" // "testnet_experiment_tools")
    ~synopsis:"Script to obtain missed attestations from experiment"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~release_status:Unreleased
    ~opam:""
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_clic;
        caqti_lwt_unix;
        caqti_dynload;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
      ]
    ~modules:["get_teztale_data"; "teztale_sql_queries"]

let simdal_lib =
  private_lib
    "simdal"
    ~path:("devtools" // "simdal" // "lib")
    ~release_status:Unreleased
    ~synopsis:"P2P simulator library"
    ~opam:""
    ~deps:[ocamlgraph; prbnmcn_stats; unix]
    ~static:false
    ~bisect_ppx:No

let _simdal =
  private_exes
    ["sim"; "concat"]
    ~path:("devtools" // "simdal" // "bin")
    ~release_status:Unreleased
    ~synopsis:"DAL/P2P simulator"
    ~opam:""
    ~deps:[simdal_lib]
    ~static:false
    ~bisect_ppx:No

let tezt_tx_kernel =
  private_lib
    "tezt_tx_kernel"
    ~path:"tezt/lib_tx_kernel"
    ~opam:"tezt-tx-kernel"
    ~synopsis:"Tx kernel test framework based on Tezt"
    ~bisect_ppx:No
    ~deps:
      [
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        Protocol.(main alpha);
        octez_crypto;
      ]
    ~release_status:Unreleased

let _ppinclude =
  private_exe
    "ppinclude"
    ~path:"src/lib_protocol_environment/ppinclude"
    ~opam:"octez-libs"
    ~bisect_ppx:No
    ~deps:[compiler_libs_common]

let _octez_node =
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | _, V 000 ->
            (* The node always needs to be linked with this protocol for Mainnet. *)
            false
        | Active, V _ ->
            (* Active protocols cannot be optional because of a bug
               that results in inconsistent hashes. Once this bug is fixed,
               this exception can be removed. *)
            false
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Dev | Other) ->
            (* Other protocols are optional. *)
            true
      in
      let targets =
        List.filter_map
          Fun.id
          [Protocol.embedded_opt protocol; Protocol.plugin_registerer protocol]
      in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  public_exe
    "octez-node"
    ~path:"src/bin_node"
    ~internal_name:"main"
    ~synopsis:"Tezos: `octez-node` binary"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~preprocess
    ~preprocessor_deps
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives" |> open_;
         octez_base_unix |> open_;
         octez_version;
         octez_version_value;
         octez_node_config |> open_;
         octez_stdlib_unix |> open_;
         octez_shell_services |> open_;
         octez_rpc_http |> open_;
         octez_rpc_http_server |> open_;
         octez_rpc_process |> open_;
         octez_p2p |> open_;
         octez_profiler |> open_;
         octez_profiler_backends |> open_;
         octez_profiler_complex_backends |> open_;
         octez_shell |> open_;
         octez_store |> open_;
         octez_store_unix_reconstruction |> open_;
         octez_store_unix_snapshots |> open_;
         octez_validation |> open_;
         octez_workers |> open_;
         octez_protocol_updater |> open_;
         cmdliner;
         fmt_cli;
         fmt_tty;
         tls_lwt;
         prometheus_app_unix;
         lwt_exit;
         uri;
         octez_base_p2p_identity_file |> open_;
         memtrace;
         octez_lib_upnp;
         octez_lib_upnp_args;
       ]
      @ protocol_deps)
    ~linkall:true
    ~dune:
      Dune.
        [
          install
            [as_ "octez-sandboxed-node.sh" "octez-sandboxed-node.sh"]
            ~package:"octez-node"
            ~section:"bin";
        ]

let _octez_agnostic_baker =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | Active, V _ -> false
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Dev | Other) -> true
      in
      let targets =
        List.filter_map
          Fun.id
          [Protocol.agnostic_baker protocol; Protocol.dal protocol]
      in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  public_exe
    "octez-baker"
    ~path:"src/bin_agnostic_baker"
    ~internal_name:"main_agnostic_baker"
    ~synopsis:"Tezos: `octez-baker` binary for baking"
    ~preprocess
    ~preprocessor_deps
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives" |> open_;
         octez_base_unix |> open_;
         octez_client_base_unix |> open_;
         octez_agnostic_baker_lib |> open_;
         octez_profiler |> open_;
       ]
      @ protocol_deps)
    ~linkall:true

let _octez_agnostic_accuser =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | Active, V _ -> false
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Dev | Other) -> true
      in
      let targets = List.filter_map Fun.id [Protocol.agnostic_baker protocol] in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  public_exe
    "octez-accuser"
    ~path:"src/bin_agnostic_accuser"
    ~internal_name:"main_agnostic_accuser"
    ~synopsis:"Tezos: `octez-accuser` binary for denunciation"
    ~preprocess
    ~preprocessor_deps
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives" |> open_;
         octez_base_unix |> open_;
         octez_client_base_unix |> open_;
         octez_agnostic_baker_lib |> open_;
       ]
      @ protocol_deps)
    ~linkall:true

let _octez_client =
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | Active, V _ -> false
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Dev | Other) -> true
      in
      let targets =
        List.filter_map
          Fun.id
          [
            (match Protocol.client_commands_registration protocol with
            | None -> Protocol.client protocol
            | x -> x);
            Protocol.baking_commands_registration protocol;
            Protocol.plugin protocol;
          ]
      in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  public_exes
    ["octez-client"; "octez-admin-client"]
    ~path:"src/bin_client"
    ~internal_names:["main_client"; "main_admin"]
    ~opam:"octez-client"
    ~synopsis:"Tezos: `octez-client` binary"
    ~release_status:Released
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_clic;
         octez_rpc_http_client |> open_;
         octez_stdlib_unix |> open_;
         octez_shell_services |> open_;
         octez_client_base |> open_;
         octez_client_commands |> open_;
         octez_mockup_commands |> open_;
         octez_proxy;
         octez_client_base_unix |> open_;
         octez_signer_backends_unix;
         uri;
       ]
      @ protocol_deps)
    ~linkall:true
    ~with_macos_security_framework:true
    ~dune:
      Dune.
        [
          install
            [
              as_
                "octez-init-sandboxed-client.sh"
                "octez-init-sandboxed-client.sh";
            ]
            ~package:"octez-client"
            ~section:"bin";
          alias_rule
            "runtest_compile_protocol"
            ~deps_dune:[[S "source_tree"; S "test/proto_test_injection"]]
            ~action:
              [
                S "run";
                S "%{bin:octez-protocol-compiler}";
                S "-no-hash-check";
                H [S "-warn-error"; S "+a"];
                S "test/proto_test_injection/";
              ];
        ]

let _octez_codec =
  public_exe
    "octez-codec"
    ~path:"src/bin_codec"
    ~internal_name:"codec"
    ~synopsis:"Tezos: `octez-codec` binary to encode and decode values"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      ([
         bls12_381_archive;
         data_encoding |> open_;
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_client_base_unix |> open_;
         octez_client_base |> open_;
         octez_node_config;
         octez_clic;
         octez_stdlib_unix |> open_;
         octez_event_logging |> open_;
         octez_signer_services;
         octez_version_value;
         octez_dal_node_gossipsub_lib;
       ]
      @ Protocol.all_optionally
      @@ [
           (fun protocol ->
             let link =
               match Protocol.number protocol with
               | Dev -> true
               | V number -> number >= 005
               | Other -> false
             in
             if link then Protocol.client protocol else None);
         ])
    ~linkall:true

let _octez_snoop =
  public_exe
    "octez-snoop"
    ~path:"src/bin_snoop"
    ~internal_name:"main_snoop"
    ~synopsis:"Tezos: `octez-snoop` binary"
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_clic;
        octez_benchmark |> open_;
        octez_benchmark_examples;
        octez_shell_benchmarks;
        Protocol.(benchmarks_proto_exn alpha);
        str;
        pyml;
        prbnmcn_stats;
        octez_version_value;
      ]
    ~linkall:true
    ~dune:
      Dune.
        [
          S "cram"
          :: G [S "deps" :: [S "main_snoop.exe"]]
          :: [S "package" :: [S "octez-snoop"]];
        ]

let _octez_injector_server =
  public_exe
    "octez-injector-server"
    ~internal_name:"octez_injector_server"
    ~path:"contrib/octez_injector_server"
    ~synopsis:"Octez injector"
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~linkall:true
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives";
         octez_injector_lib |> open_;
         octez_stdlib_unix |> open_;
         octez_rpc_http_server |> open_;
         octez_rpc_http |> open_;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         data_encoding;
       ]
      (* No code from octez_injector_alpha is used, but it's imported in order to *)
      (* run the protocol registration code *)
      @ Protocol.(all_optionally [octez_injector]))

let _RPC_toy =
  public_exe
    "RPC-toy"
    ~internal_name:"RPC_toy"
    ~path:"contrib/RPC_toy"
    ~synopsis:"RPC toy"
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~linkall:true
    ~deps:
      [
        bls12_381_archive;
        octez_stdlib_unix;
        octez_base |> open_ ~m:"TzPervasives";
        cohttp_lwt_unix;
      ]

(* We use Dune's select statement and keep uTop optional. Keeping uTop
   optional lets `make build` succeed. This prevents having to add
   dev-dependency to images/ci unnecessarily. We set [~static] to
   false because we don't release this as a static binary. *)
let _tztop =
  public_exe
    "tztop"
    ~path:"devtools/tztop"
    ~release_status:Unreleased
    ~internal_name:"tztop_main"
    ~synopsis:"Internal dev tools"
    ~opam:"internal-devtools"
    ~modes:[Byte]
    ~bisect_ppx:No
    ~static:false
    ~profile:"octez-dev-deps"
    ~deps:
      [
        bls12_381_archive;
        (* The following deps come from the original dune file. *)
        octez_protocol_compiler_lib;
        octez_base;
        compiler_libs_toplevel;
        select
          ~package:utop
          ~source_if_present:"tztop.utop.ml"
          ~source_if_absent:"tztop.vanilla.ml"
          ~target:"tztop.ml";
      ]

let _octez_signer =
  public_exe
    "octez-signer"
    ~path:"src/bin_signer"
    ~internal_name:"main_signer"
    ~synopsis:"Tezos: `octez-signer` binary"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_clic;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        octez_client_commands |> open_;
        octez_signer_services |> open_;
        octez_rpc_http |> open_;
        octez_rpc_http_server |> open_;
        octez_rpc_http_client_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_signer_backends_unix;
      ]

let _rpc_openapi =
  private_exe
    "rpc_openapi"
    ~path:"src/bin_openapi"
    ~opam:""
    ~deps:[octez_openapi]

let _octez_tps_evaluation =
  public_exe
    "tezos-tps-evaluation"
    ~internal_name:"main_tps_evaluation"
    ~path:"src/bin_tps_evaluation"
    ~synopsis:"Tezos TPS evaluation tool"
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        caqti;
        caqti_dynload;
        caqti_lwt_unix;
        data_encoding;
        lwt;
        Protocol.(baking_exn alpha);
        Protocol.(client_commands_exn alpha);
        octez_client_base_unix;
        Protocol.(main alpha);
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_;
        tezt_performance_regression |> open_;
        uri;
        octez_test_helpers |> open_;
      ]
    ~static:false
    ~dune:
      Dune.
        [
          targets_rule
            ["sql.ml"]
            ~action:
              [
                S "run";
                G
                  [
                    S "%{bin:ocp-ocamlres}";
                    S "-format";
                    S "ocaml";
                    S "-o";
                    S "%{targets}";
                  ];
                S "%{dep:sql/get_all_operations.sql}";
              ];
        ]

let _octez_dal_node =
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | _, V 000 ->
            (* The node always needs to be linked with this protocol for Mainnet. *)
            false
        | Active, V _ ->
            (* Active protocols cannot be optional because of a bug
               that results in inconsistent hashes. Once this bug is fixed,
               this exception can be removed. *)
            false
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Dev | Other) ->
            (* Other protocols are optional. *)
            true
      in
      let targets = List.filter_map Fun.id [Protocol.dal protocol] in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  public_exe
    "octez-dal-node"
    ~path:"src/bin_dal_node"
    ~internal_name:"main"
    ~synopsis:"Tezos: `octez-dal-node` binary"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives";
         octez_stdlib_unix |> open_;
         octez_dal_node_lib |> open_;
         memtrace;
         octez_profiler_backends |> open_;
         octez_profiler_complex_backends |> open_;
       ]
      @ protocol_deps)
    ~conflicts:[Conflicts.checkseum]

let _octez_smart_rollup_node =
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | Active, V _ -> false
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Dev | Other) -> true
      in
      let targets =
        List.filter_map Fun.id [Protocol.octez_sc_rollup_node protocol]
      in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  public_exe
    "octez-smart-rollup-node"
    ~internal_name:"main_smart_rollup_node"
    ~path:"src/bin_smart_rollup_node"
    ~synopsis:"Octez: Smart rollup node"
    ~release_status:Released
    ~linkall:true
    ~with_macos_security_framework:true
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ |> open_ ~m:"TzPervasives"
         |> open_ ~m:"TzPervasives.Error_monad";
         octez_clic;
         octez_shell_services |> open_;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         octez_client_commands |> open_;
         octez_smart_rollup_lib |> open_;
         octez_smart_rollup_node_lib |> open_;
       ]
      @ protocol_deps)

let _octez_smart_rollup_node_lib_tests =
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | Active, V _ -> false
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Dev | Other) -> true
      in
      let targets =
        List.filter_map Fun.id [Protocol.octez_sc_rollup_node protocol]
      in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  let helpers =
    private_lib
      "octez_smart_rollup_node_test_helpers"
      ~path:"src/lib_smart_rollup_node/test/helpers"
      ~opam:""
      ~deps:
        ([
           octez_base |> open_ ~m:"TzPervasives"
           |> open_ ~m:"TzPervasives.Error_monad";
           octez_test_helpers |> open_;
           qcheck_alcotest;
           qcheck_core;
           logs_lwt;
           alcotezt;
           tezt_lib;
           octez_client_base_unix |> open_;
           octez_smart_rollup_lib |> open_;
           octez_smart_rollup_node_lib |> open_;
           octez_layer2_store |> open_;
         ]
        @ protocol_deps)
  in
  tezt
    ["canary"; "test_context_gc"; "test_store"]
    ~path:"src/lib_smart_rollup_node/test/"
    ~opam:"tezos-smart-rollup-node-lib-test"
    ~synopsis:"Tests for the smart rollup node library"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives"
        |> open_ ~m:"TzPervasives.Error_monad";
        octez_stdlib_unix |> open_;
        octez_test_helpers |> open_;
        octez_layer2_store |> open_;
        octez_layer2_irmin_context |> open_;
        octez_smart_rollup_lib |> open_;
        octez_smart_rollup_node_store_lib |> open_;
        octez_smart_rollup_node_lib |> open_;
        helpers |> open_;
        alcotezt;
      ]

let octez_scoru_wasm_debugger_plugin =
  public_lib
    "octez-smart-rollup-wasm-debugger-plugin"
    ~path:"src/bin_wasm_debugger/plugin"
    ~release_status:Unreleased
    ~deps:[]
    ~synopsis:"Plugin interface for the Octez Smart Rollup WASM Debugger"

let octez_scoru_wasm_debugger_lib =
  public_lib
    "octez-smart-rollup-wasm-debugger-lib"
    ~path:"src/lib_wasm_debugger"
    ~synopsis:"Tezos: Library used for the Smart Rollups' WASM debugger"
    ~release_status:Unreleased
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_clic;
        tree_encoding;
        octez_base_unix;
        (* The debugger always rely on proto_alpha, as such the client is always
           available. *)
        Protocol.(client_exn alpha);
        cohttp_lwt_unix;
        octez_scoru_wasm;
        octez_scoru_wasm_helpers_intf |> open_;
        octez_smart_rollup_lib;
        octez_webassembly_interpreter |> open_;
        octez_webassembly_interpreter_extra |> open_;
        octez_version_value;
        octez_scoru_wasm_debugger_plugin;
        dynlink;
        lambda_term;
      ]

let _octez_scoru_wasm_debugger =
  public_exe
    (sf "octez-smart-rollup-wasm-debugger")
    ~internal_name:(sf "main_wasm_debugger")
    ~path:"src/bin_wasm_debugger"
    ~opam:"octez-smart-rollup-wasm-debugger"
    ~synopsis:"Tezos: Debugger for the smart rollups’ WASM kernels"
    ~release_status:Unreleased
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        octez_scoru_wasm_helpers |> open_;
        octez_scoru_wasm_debugger_lib |> open_;
      ]

let _octez_scoru_wasm_regressions =
  tezt
    ["tezos_scoru_wasm_regressions"]
    ~path:"src/lib_scoru_wasm/regressions"
    ~with_macos_security_framework:true
    ~opam:"tezos-scoru-wasm-regressions"
    ~synopsis:"WASM PVM regressions"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_scoru_wasm |> open_;
        octez_scoru_wasm_helpers;
        octez_test_helpers;
        Protocol.(main alpha);
        Protocol.(octez_sc_rollup alpha) |> if_some |> open_;
        Protocol.(parameters_exn alpha);
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~dep_files:
      [
        "../../proto_alpha/lib_protocol/test/integration/wasm_kernel/echo.wast";
        "../test/wasm_kernels/tx-kernel-no-verif.wasm";
        "../test/messages/deposit.out";
        "../test/messages/withdrawal.out";
      ]
    ~preprocess:(staged_pps [ppx_import; ppx_deriving_show])

let tezos_time_measurement =
  external_lib ~opam:"" "tezos-time-measurement" V.True

let _tezt_long_tests =
  private_exe
    "main"
    ~opam:""
    ~path:"tezt/long_tests"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_performance_regression |> open_;
        octez_lwt_result_stdlib |> open_;
        Protocol.(test_helpers_exn alpha);
        octez_micheline;
        octez_openapi;
        Protocol.(main alpha);
        qcheck_core;
        tezos_time_measurement;
        data_encoding;
        octez_event_logging |> open_;
        octez_test_helpers |> open_;
      ]

let _tezt_manual_tests =
  private_exe
    "main"
    ~opam:""
    ~path:"tezt/manual_tests"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_;
        yes_wallet_lib;
      ]

let _tezt_eio_benchmarks =
  let (PPX {preprocess; preprocessor_deps}) = ppx_profiler in
  private_exe
    "test"
    ~opam:""
    ~path:"tezt/manual_tests/eio_benchmarks"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~preprocess
    ~preprocessor_deps
    ~linkall:true
    ~deps:
      [
        bls12_381_archive;
        bls12_381;
        octez_profiler_backends;
        octez_crypto;
        octez_bees;
        octez_client_base_unix;
      ]

let _tezt_remote_tests =
  private_exe
    "main"
    ~opam:""
    ~path:"tezt/remote_tests"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_;
      ]

let _tezt_snoop =
  private_exe
    "main"
    ~opam:""
    ~path:"tezt/snoop"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_;
      ]

let _tezt_vesting_contract_test =
  private_exe
    "main"
    ~opam:""
    ~path:"tezt/vesting_contract_test"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~deps:
      [
        bls12_381_archive;
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_;
        octez_stdlib;
        octez_test_helpers;
        octez_micheline;
        Protocol.(main alpha);
        ptime;
      ]

let _tezt_stats =
  private_exe
    "main"
    ~opam:""
    ~path:"tezt/stats"
    ~bisect_ppx:No
    ~deps:[clap; tezt_core_lib]
    ~release_status:Unreleased

let _docs_doc_gen =
  private_exes
    ["rpc_doc"; "p2p_doc"]
    ~opam:""
    ~path:"docs/doc_gen"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~release_status:Unreleased
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives";
         octez_rpc;
         octez_stdlib_unix |> open_;
         octez_shell |> open_;
         octez_rpc_http_server;
         octez_store |> open_;
         octez_protocol_updater |> open_;
         octez_node_config |> open_;
         data_encoding;
         re;
       ]
      @ List.map Protocol.embedded (Protocol.genesis :: Protocol.active)
      @ List.map Protocol.plugin_registerer_exn Protocol.active)
    ~linkall:true

let _docs_doc_gen_errors =
  private_exe
    "error_doc"
    ~opam:""
    ~path:"docs/doc_gen/errors"
    ~bisect_ppx:No
    ~with_macos_security_framework:true
    ~release_status:Unreleased
    ~linkall:true
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_;
        octez_error_monad |> open_;
        data_encoding |> open_;
        Protocol.(client_exn alpha) |> open_;
      ]

let () = generate_content_input ()
