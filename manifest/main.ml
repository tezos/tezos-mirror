(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Manifest
module V = Version

let sf = Printf.sprintf

(* EXTERNAL LIBS *)

let alcotest = external_lib ~js_compatible:true "alcotest" V.(at_least "1.5.0")

let alcotest_lwt = external_lib "alcotest-lwt" V.(at_least "1.5.0")

let astring = external_lib ~js_compatible:true "astring" V.True

let bigstring = external_lib ~js_compatible:true "bigstring" V.True

let bigstringaf =
  external_lib ~js_compatible:true "bigstringaf" V.(at_least "0.2.0")

let bisect_ppx = opam_only "bisect_ppx" V.(at_least "2.7.0")

let bls12_381 =
  let version = V.(at_least "3.0.0" && less_than "3.1.0") in
  external_lib
    ~js_compatible:true
    ~npm_deps:
      [
        Npm.make
          ~node_wrapper_flags:["--bls12-381"]
          "@dannywillems/ocaml-bls12-381"
          version;
      ]
    "bls12-381"
    version

let camlzip = external_lib "camlzip" V.(at_least "1.11" && less_than "1.12")

let caqti = external_lib "caqti" V.True

let caqti_lwt = external_lib "caqti-lwt" V.True

let caqti_driver_postgresql = external_lib "caqti-driver-postgresql" V.True

let cmdliner = external_lib "cmdliner" V.True

let cohttp_lwt_unix = external_lib "cohttp-lwt-unix" V.(at_least "2.2.0")

let compiler_libs_common = external_lib "compiler-libs.common" V.True ~opam:""

let compiler_libs_optcomp = external_lib "compiler-libs.optcomp" V.True ~opam:""

let compiler_libs_toplevel =
  external_lib "compiler-libs.toplevel" V.True ~opam:""

let conf_libev = opam_only "conf-libev" V.True

let conf_rust = opam_only "conf-rust" V.True

let ctypes = external_lib ~js_compatible:true "ctypes" V.(at_least "0.18.0")

let ctypes_stubs = external_sublib ctypes "ctypes.stubs"

let ctypes_stubs_js = external_lib ~js_compatible:true "ctypes_stubs_js" V.True

let data_encoding =
  external_lib
    ~js_compatible:true
    ~main_module:"Data_encoding"
    "data-encoding"
    V.(at_least "0.5.3" && less_than "0.6")

let digestif = external_lib ~js_compatible:true "digestif" V.(at_least "0.7.3")

let digestif_c = external_sublib digestif "digestif.c"

let dune_configurator = external_lib "dune-configurator" V.True

let dynlink = external_lib "dynlink" V.True ~opam:""

let ezjsonm = external_lib ~js_compatible:true "ezjsonm" V.(at_least "1.1.0")

let fmt = external_lib ~js_compatible:true "fmt" V.(at_least "0.8.7")

let fmt_cli = external_sublib fmt "fmt.cli"

let fmt_tty = external_sublib fmt "fmt.tty"

let hacl_star_npm =
  Npm.make ~node_wrapper_flags:["--hacl"] "hacl-wasm" V.(exactly "1.1.0")

let hacl_star =
  external_lib
    ~js_compatible:true
    ~npm_deps:[hacl_star_npm]
    "hacl-star"
    V.(at_least "0.4.2" && less_than "0.5")

let hacl_star_raw = external_lib ~js_compatible:true "hacl-star-raw" V.True

let hacl_x25519 = external_lib "hacl_x25519" V.True

let hashcons = external_lib "hashcons" V.True

let hex = external_lib ~js_compatible:true "hex" V.(at_least "1.3.0")

let index = external_lib "index" V.(at_least "1.6.0" && less_than "1.7.0")

let integers = external_lib ~js_compatible:true "integers" V.True

let integers_stubs_js =
  external_lib ~js_compatible:true "integers_stubs_js" V.True

let ipaddr =
  external_lib
    ~js_compatible:true
    "ipaddr"
    V.(at_least "5.0.0" && less_than "6.0.0")

let ipaddr_unix = external_sublib ipaddr "ipaddr.unix"

let irmin = external_lib "irmin" V.(at_least "3.2.0" && less_than "3.3.0")

let irmin_pack =
  external_lib "irmin-pack" V.(at_least "3.2.0" && less_than "3.3.0")

let irmin_pack_unix = external_sublib irmin_pack "irmin-pack.unix"

let irmin_pack_mem = external_sublib irmin_pack "irmin-pack.mem"

let json_data_encoding =
  external_lib
    ~js_compatible:true
    "json-data-encoding"
    V.(at_least "0.11" && less_than "0.12")

let logs = external_lib "logs" V.True

let logs_fmt = external_sublib logs "logs.fmt"

let lwt = external_lib ~js_compatible:true "lwt" V.(at_least "5.4.0")

let lwt_canceler =
  external_lib
    ~js_compatible:true
    "lwt-canceler"
    V.(at_least "0.3" && less_than "0.4")

let lwt_exit = external_lib "lwt-exit" V.True

let lwt_log = external_lib "lwt_log" V.True

let lwt_log_core = external_sublib ~js_compatible:true lwt_log "lwt_log.core"

let lwt_unix = external_sublib lwt "lwt.unix"

let lwt_watcher = external_lib "lwt-watcher" V.(exactly "0.2")

let mtime = external_lib ~js_compatible:true "mtime" V.(at_least "1.0.0")

let mtime_clock_os = external_sublib mtime "mtime.clock.os"

let ocaml_migrate_parsetree = external_lib "ocaml-migrate-parsetree" V.True

let ocamlformat = opam_only "ocamlformat" V.(exactly "0.18.0")

let ocamlgraph = external_lib "ocamlgraph" V.True

let ocplib_endian = external_lib ~js_compatible:true "ocplib-endian" V.True

let ocplib_endian_bigstring =
  external_sublib ocplib_endian "ocplib-endian.bigstring"

let ocplib_ocamlres =
  external_lib ~opam:"ocp-ocamlres" "ocplib-ocamlres" V.(at_least "0.4")

let ometrics = opam_only "ometrics" V.(at_least "0.1.3")

let parsexp = external_lib ~js_compatible:true "parsexp" V.True

let ppx_blob = external_lib "ppx_blob" V.True

let ppx_inline_test =
  inline_tests_backend (external_lib "ppx_inline_test" V.True)

let ptime = external_lib ~js_compatible:true "ptime" V.(at_least "1.0.0")

let ppx_deriving = external_lib "ppx_deriving" V.True

let ppx_deriving_show = external_sublib ppx_deriving "ppx_deriving.show"

let ptime_clock_os = external_sublib ~js_compatible:true ptime "ptime.clock.os"

let pure_splitmix =
  external_lib ~js_compatible:true "pure-splitmix" V.(exactly "0.3")

let prbnmcn_cgrph = external_lib "prbnmcn-cgrph" V.(exactly "0.0.2")

let prbnmcn_dagger = external_lib "prbnmcn-dagger" V.(exactly "0.0.2")

let prbnmcn_dagger_stats =
  external_lib "prbnmcn-dagger-stats" V.(exactly "0.0.2")

let prbnmcn_stats = external_lib "prbnmcn-stats" V.(exactly "0.0.4")

let pringo = external_lib "pringo" V.(exactly "1.3")

let prometheus = external_lib "prometheus" V.True

let prometheus_app = external_lib "prometheus-app" V.True

let prometheus_app_unix = external_sublib prometheus_app "prometheus-app.unix"

let pyml = external_lib "pyml" V.True

let qcheck_alcotest =
  external_lib ~js_compatible:true "qcheck-alcotest" V.(at_least "0.15")

let qcheck_core = external_lib "qcheck-core" V.True

let re = external_lib ~js_compatible:true "re" V.(at_least "1.7.2")

let re_str = external_sublib ~js_compatible:true re "re.str"

let resto_version = V.(at_least "0.6" && less_than "0.7")

let resto = external_lib ~js_compatible:true "resto" resto_version

let resto_acl = external_lib "resto-acl" resto_version

let resto_cohttp = external_lib "resto-cohttp" resto_version

let resto_cohttp_client = external_lib "resto-cohttp-client" resto_version

let resto_cohttp_self_serving_client =
  external_lib "resto-cohttp-self-serving-client" resto_version

let resto_cohttp_server = external_lib "resto-cohttp-server" resto_version

let resto_directory =
  external_lib ~js_compatible:true "resto-directory" resto_version

let ringo = external_lib ~js_compatible:true "ringo" V.(exactly "0.8")

let ringo_lwt = external_lib "ringo-lwt" V.(exactly "0.8")

let secp256k1_internal =
  let version = V.(at_least "0.3.0") in
  external_lib
    ~npm_deps:
      [
        Npm.make
          ~node_wrapper_flags:["--secp256k1"]
          "@nomadic-labs/secp256k1-wasm"
          version;
      ]
    ~js_compatible:true
    "secp256k1-internal"
    version

let str = external_lib ~js_compatible:true "str" ~opam:"" V.True

let tar = external_lib "tar" V.True

let tar_unix = external_lib "tar-unix" V.(exactly "2.0.0")

let tezos_rust_lib =
  opam_only ~can_vendor:false "tezos-rust-libs" V.(exactly "1.1")

let tls = external_lib "tls" V.(at_least "0.10")

let unix = external_lib ~opam:"base-unix" "unix" V.True

let uri = external_lib ~js_compatible:true "uri" V.True

let utop = external_lib "utop" V.(at_least "2.8")

let uutf = external_lib ~js_compatible:true "uutf" V.True

(* The signature of the [Z] module has changed in 1.12. *)
let zarith =
  external_lib
    ~js_compatible:true
    "zarith"
    V.(at_least "1.12" && less_than "1.13")

let zarith_stubs_js = external_lib ~js_compatible:true "zarith_stubs_js" V.True

(* VENDORED LIBS *)

let benchmark_utils = vendored_lib "benchmark-utils"

let flextesa = vendored_lib "flextesa"

let ledgerwallet_tezos = vendored_lib "ledgerwallet-tezos"

let pyml_plot = vendored_lib "pyml-plot"

(* INTERNAL LIBS *)

let tezos_test_helpers =
  public_lib
    "tezos-test-helpers"
    ~path:"src/lib_test"
    ~internal_name:"lib_test"
    ~synopsis:"Tezos-agnostic test helpers"
    ~deps:
      [uri; fmt; qcheck_alcotest; alcotest; lwt; pure_splitmix; data_encoding]
    ~js_compatible:true
    ~ocaml:V.(at_least "4.08")
    ~linkall:true
    ~dune:
      Dune.
        [
          (* This rule is necessary for `make lint-tests-pkg`, without it dune
             complains that the alias is empty. *)
          alias_rule "runtest_js" ~action:(S "progn");
        ]
    ~modules:
      [
        "assert";
        "lwt_assert";
        "qcheck2_helpers";
        "qcheck_extra";
        "qcheck_helpers";
        "random_pure";
        "roundtrip";
        "testable";
      ]

let tezos_stdlib =
  public_lib
    "tezos-stdlib"
    ~path:"src/lib_stdlib"
    ~synopsis:"Tezos: yet-another local-extension of the OCaml standard library"
    ~deps:[hex; zarith; zarith_stubs_js; lwt; ringo]
    ~ocaml:V.(at_least "4.08")
    ~js_compatible:true
    ~inline_tests:ppx_inline_test

let _tezos_stdlib_tests =
  tests
    [
      "test_bits";
      "test_tzList";
      "test_bounded_heap";
      "test_tzString";
      "test_fallbackArray";
      "test_functionalArray";
      "test_hash_queue";
    ]
    ~path:"src/lib_stdlib/test"
    ~opam:"src/lib_stdlib/tezos-stdlib"
    ~modes:[Native; JS]
    ~deps:
      [
        tezos_stdlib |> open_;
        alcotest;
        bigstring;
        tezos_test_helpers;
        qcheck_alcotest;
      ]
    ~js_compatible:true

let _tezos_stdlib_unix_tests =
  tests
    [
      "test_lwt_pipe";
      "test_circular_buffer";
      "test_circular_buffer_fuzzy";
      "test_hash_queue_lwt";
    ]
    ~path:"src/lib_stdlib/test-unix"
    ~opam:"src/lib_stdlib/tezos-stdlib"
    ~deps:
      [
        tezos_stdlib |> open_;
        alcotest;
        alcotest_lwt;
        lwt_log;
        bigstring;
        lwt_unix;
        tezos_test_helpers;
        qcheck_alcotest;
      ]

let tezos_lwt_result_stdlib_bare_functor_outputs =
  public_lib
    "tezos-lwt-result-stdlib.bare.functor-outputs"
    ~path:"src/lib_lwt_result_stdlib/bare/functor_outputs"
    ~opam:"src/lib_lwt_result_stdlib/tezos-lwt-result-stdlib"
    ~internal_name:"bare_functor_outputs"
    ~js_compatible:true
    ~deps:[lwt]

let tezos_lwt_result_stdlib_bare_sigs =
  public_lib
    "tezos-lwt-result-stdlib.bare.sigs"
    ~path:"src/lib_lwt_result_stdlib/bare/sigs"
    ~opam:"src/lib_lwt_result_stdlib/tezos-lwt-result-stdlib"
    ~internal_name:"bare_sigs"
    ~js_compatible:true
    ~deps:[lwt; tezos_lwt_result_stdlib_bare_functor_outputs]

let tezos_lwt_result_stdlib_bare_structs =
  public_lib
    "tezos-lwt-result-stdlib.bare.structs"
    ~path:"src/lib_lwt_result_stdlib/bare/structs"
    ~opam:"src/lib_lwt_result_stdlib/tezos-lwt-result-stdlib"
    ~internal_name:"bare_structs"
    ~js_compatible:true
    ~deps:[lwt; tezos_lwt_result_stdlib_bare_sigs]

let tezos_lwt_result_stdlib_traced_functor_outputs =
  public_lib
    "tezos-lwt-result-stdlib.traced.functor-outputs"
    ~path:"src/lib_lwt_result_stdlib/traced/functor_outputs"
    ~opam:"src/lib_lwt_result_stdlib/tezos-lwt-result-stdlib"
    ~internal_name:"traced_functor_outputs"
    ~js_compatible:true
    ~deps:[lwt; tezos_lwt_result_stdlib_bare_sigs]

let tezos_lwt_result_stdlib_traced_sigs =
  public_lib
    "tezos-lwt-result-stdlib.traced.sigs"
    ~path:"src/lib_lwt_result_stdlib/traced/sigs"
    ~opam:"src/lib_lwt_result_stdlib/tezos-lwt-result-stdlib"
    ~internal_name:"traced_sigs"
    ~js_compatible:true
    ~deps:
      [
        lwt;
        tezos_lwt_result_stdlib_bare_sigs;
        tezos_lwt_result_stdlib_bare_structs;
        tezos_lwt_result_stdlib_traced_functor_outputs;
      ]

let tezos_lwt_result_stdlib_traced_structs =
  public_lib
    "tezos-lwt-result-stdlib.traced.structs"
    ~path:"src/lib_lwt_result_stdlib/traced/structs"
    ~opam:"src/lib_lwt_result_stdlib/tezos-lwt-result-stdlib"
    ~internal_name:"traced_structs"
    ~js_compatible:true
    ~deps:
      [
        lwt;
        tezos_lwt_result_stdlib_traced_sigs;
        tezos_lwt_result_stdlib_bare_structs;
      ]

let tezos_lwt_result_stdlib =
  public_lib
    "tezos-lwt-result-stdlib"
    ~path:"src/lib_lwt_result_stdlib"
    ~synopsis:"Tezos: error-aware stdlib replacement"
    ~ocaml:V.(at_least "4.12")
    ~js_compatible:true
    ~documentation:[]
    ~deps:
      [
        lwt;
        tezos_lwt_result_stdlib_bare_sigs;
        tezos_lwt_result_stdlib_bare_structs;
        tezos_lwt_result_stdlib_traced_sigs;
        tezos_lwt_result_stdlib_traced_structs;
      ]

let tezos_lwt_result_stdlib_examples_traces =
  public_lib
    "tezos-lwt-result-stdlib.examples.traces"
    ~path:"src/lib_lwt_result_stdlib/examples/traces"
    ~opam:"src/lib_lwt_result_stdlib/tezos-lwt-result-stdlib"
    ~internal_name:"traces"
    ~deps:
      [
        lwt;
        tezos_lwt_result_stdlib_bare_structs;
        tezos_lwt_result_stdlib_traced_sigs;
      ]

let _tezos_lwt_result_stdlib_tests =
  tests
    [
      "test_hashtbl";
      "test_list_basic";
      "test_list_basic_lwt";
      "test_seq_basic";
      "test_generic";
      "test_fuzzing_seq";
      "test_fuzzing_list";
      "test_fuzzing_set";
      "test_fuzzing_seq_tiered";
      "test_fuzzing_option";
    ]
    ~path:"src/lib_lwt_result_stdlib/test"
    ~opam:"src/lib_lwt_result_stdlib/tezos-lwt-result-stdlib"
    ~deps:
      [
        tezos_lwt_result_stdlib |> open_;
        tezos_lwt_result_stdlib_examples_traces;
        lwt_unix;
        alcotest_lwt;
        qcheck_alcotest;
        tezos_test_helpers;
      ]

let tezos_error_monad =
  public_lib
    "tezos-error-monad"
    ~path:"src/lib_error_monad"
    ~synopsis:"Tezos: error monad"
    ~ocaml:V.(at_least "4.07")
    ~deps:
      [
        tezos_stdlib |> open_;
        data_encoding |> open_;
        lwt_canceler;
        lwt;
        tezos_lwt_result_stdlib;
      ]
    ~js_compatible:true

let tezos_hacl =
  let js_stubs = ["random.js"; "evercrypt.js"] in
  let js_generated = "runtime-generated.js" in
  let js_helper = "helper.js" in
  public_lib
    "tezos-hacl"
    ~path:"src/lib_hacl"
    ~synopsis:"Tezos: thin layer around hacl-star"
    ~ocaml:V.(at_least "4.08")
    ~deps:[hacl_star; hacl_star_raw; ctypes_stubs_js]
    ~js_of_ocaml:
      [
        [
          S "javascript_files";
          G (Dune.of_atom_list (js_generated :: js_helper :: js_stubs));
        ];
      ]
    ~conflicts:[hacl_x25519]
    ~dune:
      Dune.
        [
          [
            S "rule";
            [S "targets"; S js_generated];
            [
              S "deps";
              S "gen/api.json";
              S "gen/gen.exe";
              G (of_atom_list js_stubs);
            ];
            [S "mode"; S "promote"];
            [
              S "action";
              [
                S "with-stdout-to";
                S "%{targets}";
                of_list
                  (List.map
                     (fun l -> H (of_atom_list l))
                     Stdlib.List.(
                       ["run"; "gen/gen.exe"]
                       ::
                       ["-api"; "gen/api.json"]
                       :: List.map (fun s -> ["-stubs"; s]) js_stubs));
              ];
            ];
          ];
        ]

let _tezos_hacl_gen =
  private_exe
    "gen"
    ~path:"src/lib_hacl/gen/"
    ~opam:"src/lib_hacl/tezos-hacl"
    ~bisect_ppx:false
    ~deps:[ctypes_stubs; ctypes; hacl_star_raw; ezjsonm]
    ~dune:
      (let package = "tezos-hacl" in
       Dune.
         [
           [
             S "rule";
             [S "alias"; S "runtest_js"];
             [S "target"; S "api.json.corrected"];
             [S "package"; S package];
             [
               S "action";
               [
                 S "run";
                 S "%{dep:../../tooling/node_wrapper.exe}";
                 H (of_atom_list (Npm.node_wrapper_flags hacl_star_npm));
                 S "%{dep:./check-api.js}";
               ];
             ];
           ];
           alias_rule
             ~package
             "runtest_js"
             ~action:[S "diff"; S "api.json"; S "api.json.corrected"];
         ])

let _tezos_hacl_tests =
  tests
    ["test_hacl"; "test_prop_hacl_hash"; "test_prop_signature_pk"]
    ~path:"src/lib_hacl/test"
    ~opam:"src/lib_hacl/tezos-hacl"
    ~deps:
      [
        tezos_stdlib |> open_;
        tezos_error_monad |> open_ ~m:"TzLwtreslib";
        zarith;
        zarith_stubs_js;
        data_encoding |> open_;
        tezos_hacl |> open_;
        qcheck_alcotest;
        tezos_test_helpers;
      ]
    ~all_modules_except:["test"]
    ~modes:[Native; JS]
    ~js_compatible:true

let _tezos_hacl_tests_1 =
  test
    "test"
    ~path:"src/lib_hacl/test"
    ~opam:"src/lib_hacl/tezos-hacl"
    ~deps:
      [
        tezos_stdlib;
        tezos_error_monad;
        zarith;
        zarith_stubs_js;
        data_encoding;
        tezos_hacl;
        qcheck_alcotest;
        tezos_test_helpers;
      ]
    ~modules:["test"]
    ~modes:[Native; JS]
    ~js_compatible:true

let _tezos_error_monad_tests =
  tests
    ["test_registration"; "test_splitted_error_encoding"]
    ~path:"src/lib_error_monad/test"
    ~opam:"src/lib_error_monad/tezos-error-monad"
    ~modes:[Native; JS]
    ~deps:[tezos_error_monad |> open_; data_encoding; alcotest]
    ~js_compatible:true

let tezos_rpc =
  public_lib
    "tezos-rpc"
    ~path:"src/lib_rpc"
    ~synopsis:
      "Tezos: library of auto-documented RPCs (service and hierarchy \
       descriptions)"
    ~deps:
      [
        data_encoding |> open_;
        tezos_error_monad |> open_;
        resto;
        resto_directory;
      ]
    ~js_compatible:true

let tezos_crypto =
  public_lib
    "tezos-crypto"
    ~path:"src/lib_crypto"
    ~synopsis:
      "Tezos: library with all the cryptographic primitives used by Tezos"
    ~deps:
      [
        tezos_stdlib |> open_;
        data_encoding |> open_;
        tezos_lwt_result_stdlib;
        lwt;
        tezos_hacl;
        secp256k1_internal;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_rpc |> open_;
        ringo;
        zarith;
        zarith_stubs_js;
        bls12_381;
      ]
    ~js_compatible:true

let _tezos_crypto_tests =
  tests
    ["test_run"; "test_prop_signature"]
    ~path:"src/lib_crypto/test"
    ~opam:"src/lib_crypto/tezos-crypto"
    ~deps:
      [
        tezos_stdlib |> open_;
        tezos_crypto |> open_;
        tezos_error_monad |> open_ ~m:"TzLwtreslib";
        zarith;
        zarith_stubs_js;
        tezos_hacl;
        data_encoding |> open_;
        alcotest;
        qcheck_alcotest;
        tezos_test_helpers;
      ]
    ~modes:[Native; JS]
    ~js_compatible:true

let _tezos_crypto_tests_unix =
  tests
    ["test_crypto_box"]
    ~path:"src/lib_crypto/test-unix"
    ~opam:"src/lib_crypto/tezos-crypto"
    ~deps:
      [
        tezos_stdlib |> open_;
        tezos_crypto |> open_;
        tezos_error_monad |> open_ ~m:"TzLwtreslib";
        zarith;
        zarith_stubs_js;
        tezos_hacl;
        data_encoding |> open_;
        alcotest;
        alcotest_lwt;
        lwt_unix;
        qcheck_alcotest;
        tezos_test_helpers;
      ]

let tezos_event_logging =
  public_lib
    "tezos-event-logging"
    ~path:"src/lib_event_logging"
    ~synopsis:"Tezos event logging library"
    ~deps:
      [
        tezos_stdlib |> open_;
        data_encoding |> open_;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_lwt_result_stdlib;
        lwt_log_core;
      ]
    ~js_compatible:true

let tezos_event_logging_test_helpers =
  public_lib
    "tezos-event-logging-test-helpers"
    ~path:"src/lib_event_logging/test_helpers"
    ~synopsis:"Tezos: test helpers for the event logging library"
    ~deps:
      [
        tezos_stdlib;
        tezos_lwt_result_stdlib;
        data_encoding;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_event_logging |> open_;
        tezos_test_helpers;
        alcotest;
      ]
    ~js_compatible:true
    ~linkall:true
    ~bisect_ppx:false

let tezos_stdlib_unix =
  public_lib
    "tezos-stdlib-unix"
    ~path:"src/lib_stdlib_unix"
    ~synopsis:
      "Tezos: yet-another local-extension of the OCaml standard library \
       (unix-specific fragment)"
    ~deps:
      [
        unix;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_lwt_result_stdlib;
        tezos_event_logging |> open_;
        tezos_stdlib |> open_;
        data_encoding |> open_;
        lwt_unix;
        ipaddr_unix;
        re;
        ezjsonm;
        ptime;
        mtime;
        mtime_clock_os;
        lwt_log;
        conf_libev;
      ]

let tezos_clic =
  public_lib
    "tezos-clic"
    ~path:"src/lib_clic"
    ~synopsis:
      "Tezos: library of auto-documented command-line-parsing combinators"
    ~deps:
      [
        tezos_stdlib |> open_;
        lwt;
        re;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_lwt_result_stdlib;
      ]
    ~js_compatible:true

let tezos_clic_unix =
  public_lib
    "tezos-clic.unix"
    ~path:"src/lib_clic/unix"
    ~opam:"src/lib_clic/tezos-clic"
    ~deps:
      [
        tezos_stdlib |> open_;
        tezos_clic |> open_;
        tezos_stdlib_unix;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_lwt_result_stdlib;
      ]

let _tezos_clic_tests =
  test
    "test_clic"
    ~path:"src/lib_clic/test"
    ~opam:"src/lib_clic/tezos-clic"
    ~deps:[tezos_stdlib |> open_; tezos_clic |> open_; alcotest_lwt]

let _tezos_clic_example =
  private_exe
    "clic_example"
    ~path:"src/lib_clic/examples"
    ~opam:""
    ~deps:[tezos_clic; lwt_unix]
    ~bisect_ppx:false
    ~static:false

let tezos_micheline =
  public_lib
    "tezos-micheline"
    ~path:"src/lib_micheline"
    ~synopsis:"Tezos: internal AST and parser for the Michelson language"
    ~deps:
      [
        uutf;
        zarith;
        zarith_stubs_js;
        tezos_stdlib |> open_;
        tezos_error_monad |> open_;
        data_encoding |> open_;
      ]
    ~js_compatible:true
    ~inline_tests:ppx_inline_test

let _tezos_micheline_tests =
  tests
    ["test_parser"; "test_diff"]
    ~path:"src/lib_micheline/test"
    ~opam:"src/lib_micheline/tezos-micheline"
    ~modes:[Native; JS]
    ~deps:[tezos_micheline |> open_; alcotest]
    ~js_compatible:true

let tezos_base =
  public_lib
    "tezos-base"
    ~path:"src/lib_base"
    ~synopsis:"Tezos: meta-package and pervasive type definitions for Tezos"
    ~deps:
      [
        tezos_stdlib |> open_;
        tezos_crypto |> open_;
        data_encoding |> open_;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_rpc |> open_;
        tezos_clic |> open_;
        tezos_micheline |> open_;
        tezos_event_logging |> open_;
        ptime;
        ptime_clock_os;
        ezjsonm;
        lwt;
        ipaddr;
      ]
    ~js_compatible:true
    ~js_of_ocaml:[[S "javascript_files"; S "ptime.js"]]
    ~dune:
      Dune.
        [
          ocamllex "point_parser";
          (* ptime is currently sligtly broken and I don't think a fix will land soon.
             See https://github.com/dbuenzli/ptime/pull/27.
             Meanwhile, we can just copy the runtime file and let dune knows about it our-selves.
          *)
          targets_rule
            ["ptime.js"]
            ~action:
              [S "copy"; S "%{lib:ptime.clock.os:runtime.js}"; S "ptime.js"];
        ]

let tezos_base_unix =
  public_lib
    "tezos-base.unix"
    ~path:"src/lib_base/unix"
    ~opam:"src/lib_base/tezos-base"
    ~deps:
      [
        tezos_error_monad |> open_;
        tezos_crypto |> open_;
        tezos_base |> open_;
        tezos_hacl;
        tezos_stdlib |> open_;
        tezos_stdlib_unix |> open_;
        data_encoding |> open_;
      ]

let lib_base_tests ?dep_files names =
  tests
    names
    ~path:"src/lib_base/test"
    ~opam:"src/lib_base/tezos-base"
    ~deps:
      [
        tezos_base |> open_;
        tezos_error_monad |> open_;
        data_encoding;
        qcheck_alcotest;
        tezos_test_helpers;
      ]
    ?dep_files
    ~modes:[Native; JS]
    ~js_compatible:true
    ~modules:names

let _tezos_base_tests_1 =
  lib_base_tests ["test_bounded"; "test_time"; "test_protocol"]

let _tezos_base_tests_2 =
  lib_base_tests ["test_p2p_addr"] ~dep_files:["points.ok"; "points.ko"]

let _tezos_base_tests_3 = lib_base_tests ["test_sized"]

let _tezos_base_unix_tests =
  test
    "test_unix_error"
    ~path:"src/lib_base/unix/test"
    ~opam:"src/lib_base/tezos-base"
    ~deps:
      [
        tezos_base |> open_;
        tezos_base_unix |> open_;
        tezos_error_monad |> open_;
        data_encoding;
        tezos_test_helpers;
        qcheck_alcotest;
      ]

let tezos_base_test_helpers =
  public_lib
    "tezos-base-test-helpers"
    ~path:"src/lib_base/test_helpers"
    ~synopsis:"Tezos: Tezos base test helpers"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_stdlib_unix;
        tezos_event_logging_test_helpers;
        tezos_test_helpers;
        alcotest;
        alcotest_lwt;
        qcheck_alcotest;
      ]
    ~linkall:true
    ~bisect_ppx:false

let tezos_version_parser =
  public_lib
    "tezos-version.parser"
    ~path:"src/lib_version/parser"
    ~opam:"src/lib_version/tezos-version"
    ~dune:Dune.[ocamllex "tezos_version_parser"]
    ~js_compatible:true
    ~preprocess:[pps ppx_deriving_show]

let tezos_version =
  public_lib
    "tezos-version"
    ~path:"src/lib_version"
    ~opam:"src/lib_version/tezos-version"
    ~synopsis:"Tezos: version information generated from Git"
    ~deps:[tezos_base |> open_ ~m:"TzPervasives"; tezos_version_parser]
    ~js_compatible:true
    ~dune:
      Dune.
        [
          (* Ensures the hash updates whenever a source file is modified. *)
          targets_rule
            ["generated_git_info.ml"]
            ~deps:[[S "universe"]]
            ~action:[S "run"; S "./exe/get_git_info.exe"];
        ]

let _tezos_version_get_git_info =
  private_exe
    "get_git_info"
    ~path:"src/lib_version/exe"
    ~opam:"src/lib_version/tezos-version"
    ~deps:[dune_configurator; tezos_version_parser]
    ~modules:["get_git_info"]
    ~bisect_ppx:false

let _tezos_print_version_exe =
  public_exe
    "tezos-version"
    ~internal_name:"tezos_print_version"
    ~path:"src/lib_version/exe"
    ~opam:"src/lib_version/tezos-version"
    ~deps:[tezos_version |> open_; tezos_base_unix]
    ~modules:["tezos_print_version"]
    ~bisect_ppx:false

let _tezos_version_tests =
  test
    "test_parser"
    ~path:"src/lib_version/test"
    ~opam:"src/lib_version/tezos-version"
    ~js_compatible:true
    ~modes:[Native; JS]
    ~deps:[tezos_version |> open_; tezos_version_parser; alcotest]

let tezos_p2p_services =
  public_lib
    "tezos-p2p-services"
    ~path:"src/lib_p2p_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-p2p`"
    ~deps:[tezos_base |> open_ ~m:"TzPervasives"]
    ~linkall:true
    ~js_compatible:true

let tezos_workers =
  public_lib
    "tezos-workers"
    ~path:"src/lib_workers"
    ~synopsis:"Tezos: worker library"
    ~documentation:[]
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives" |> open_;
        tezos_stdlib_unix |> open_;
        ringo;
      ]

let tezos_shell_services =
  public_lib
    "tezos-shell-services"
    ~path:"src/lib_shell_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-shell`"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives" |> open_;
        tezos_p2p_services |> open_;
        tezos_version |> open_;
      ]
    ~linkall:true
    ~js_compatible:true

let _tezos_shell_services_tests =
  test
    "test"
    ~path:"src/lib_shell_services/test"
    ~opam:"src/lib_shell_services/tezos-shell-services"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_shell_services |> open_;
        alcotest;
      ]
    ~modes:[Native; JS]
    ~js_compatible:true

let tezos_shell_services_test_helpers =
  public_lib
    "tezos-shell-services-test-helpers"
    ~path:"src/lib_shell_services/test_helpers"
    ~synopsis:"Tezos: Tezos shell_services test helpers"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_shell_services;
        tezos_test_helpers;
        qcheck_core;
      ]
    ~bisect_ppx:false
    ~linkall:true

let _tezos_shell_service_test_helpers_tests =
  test
    "test_block_services"
    ~path:"src/lib_shell_services/test_helpers/test"
    ~opam:
      "src/lib_shell_services/test_helpers/tezos-shell-services-test-helpers"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_test_helpers;
        tezos_shell_services;
        tezos_shell_services_test_helpers;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let tezos_tooling =
  public_lib
    "tezos-tooling"
    ~path:"src/tooling"
    ~synopsis:"Tezos: tooling for the project"
    ~modules:[]
    ~deps:
      [
        bisect_ppx;
        (* These next are only used in the CI, we add this dependency so that
           it is added to tezos/opam-repository. *)
        ocamlformat;
        ometrics;
      ]
    ~dune:
      Dune.
        [
          install
            [as_ "lint.sh" "lint.sh"]
            ~package:"tezos-tooling"
            ~section:"libexec";
        ]

let _tezos_tooling_js_inline_tests =
  test
    "run_js_inline_tests"
    ~runtest:false
    ~path:"src/tooling"
    ~opam:"src/tooling/tezos-tooling"
    ~modules:["run_js_inline_tests"]
    ~deps:[parsexp; unix]

let tezos_p2p =
  public_lib
    "tezos-p2p"
    ~path:"src/lib_p2p"
    ~synopsis:"Tezos: library for a pool of P2P connections"
    ~deps:
      [
        lwt_watcher;
        lwt_canceler;
        ringo;
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix |> open_;
        tezos_stdlib_unix |> open_;
        tezos_stdlib |> open_;
        tezos_p2p_services |> open_;
        tezos_version;
        prometheus;
      ]

let _tezos_p2p_tests =
  tests
    [
      "test_p2p_socket";
      "test_p2p_pool";
      "test_p2p_io_scheduler";
      "test_p2p_peerset";
      "test_p2p_buffer_reader";
      "test_p2p_banned_peers";
      "test_p2p_node";
      (* Deactivated because it fails on CI (but not locally) *)
      (* See https://gitlab.com/tezos/tezos/-/issues/1184 *)
      (* "test_p2p_logging"; *)
      "test_p2p_connect_handler";
    ]
    ~path:"src/lib_p2p/test"
    ~opam:"src/lib_p2p/tezos-p2p"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_stdlib_unix |> open_;
        tezos_stdlib |> open_;
        tezos_p2p |> open_;
        tezos_test_helpers;
        tezos_base_test_helpers |> open_;
        tezos_event_logging_test_helpers |> open_;
        tezos_p2p_services |> open_;
        alcotest_lwt;
        astring;
      ]
    ~opam_only_deps:[tezos_tooling]
    ~linkall:true
    ~preprocess:[pps bisect_ppx ~args:["--bisect-sigterm"]]
    ~runtest:false
    ~dune:
      Dune.
        [
          alias_rule
            "runtest_p2p_socket"
            ~locks:"/ports/49152-65535"
            ~action:(run_exe "test_p2p_socket" []);
          alias_rule
            "runtest_p2p_pool"
            ~locks:"/ports/49152-65535"
            ~action:
              (run_exe "test_p2p_pool" ["--clients"; "10"; "--repeat"; "5"]);
          alias_rule
            "runtest_p2p_io_scheduler"
            ~locks:"/ports/49152-65535"
            ~action:
              (run_exe
                 "test_p2p_io_scheduler"
                 [
                   "--delay";
                   "5";
                   "--clients";
                   "8";
                   "--max-upload-speed";
                   "262144";
                   (* 1 << 18 = 256kB *)
                   "--max-download-speed";
                   "1048576";
                   (* 1 << 20 = 1MB *)
                 ]);
          alias_rule
            "runtest_p2p_socket_ipv4"
            ~locks:"/ports/49152-65535"
            ~action:(run_exe "test_p2p_socket" ["--addr"; "::ffff:127.0.0.1"]);
          alias_rule
            "runtest_p2p_pool_ipv4"
            ~locks:"/ports/49152-65535"
            ~action:
              (run_exe
                 "test_p2p_pool"
                 [
                   "--clients";
                   "10";
                   "--repeat";
                   "5";
                   "--addr";
                   "::ffff:127.0.0.1";
                 ]);
          alias_rule
            "runtest_p2p_io_scheduler_ipv4"
            ~locks:"/ports/49152-65535"
            ~action:
              (run_exe
                 "test_p2p_io_scheduler"
                 [
                   "--delay";
                   "5";
                   "--clients";
                   "8";
                   "--max-upload-speed";
                   "262144";
                   (* 1 << 18 = 256kB *)
                   "--max-download-speed";
                   "1048576";
                   (* 1 << 20 = 1MB *)
                   "--addr";
                   "::ffff:127.0.0.1";
                 ]);
          alias_rule
            "runtest_p2p_peerset"
            ~action:(run_exe "test_p2p_peerset" []);
          alias_rule
            "runtest_p2p_buffer_reader"
            ~action:(run_exe "test_p2p_buffer_reader" []);
          alias_rule
            "runtest_p2p_banned_peers"
            ~action:(run_exe "test_p2p_banned_peers" []);
          alias_rule
            "runtest_p2p_node"
            ~locks:"/ports/49152-65535"
            ~action:(run_exe "test_p2p_node" []);
          alias_rule
            "runtest_p2p_connect_handler"
            ~action:(run_exe "test_p2p_connect_handler" []);
          alias_rule
            "runtest"
            ~package:"tezos-p2p"
            ~alias_deps:
              [
                "runtest_p2p_socket_ipv4";
                "runtest_p2p_pool_ipv4";
                "runtest_p2p_io_scheduler_ipv4";
                "runtest_p2p_peerset";
                "runtest_p2p_buffer_reader";
                "runtest_p2p_banned_peers";
                "runtest_p2p_node";
                "runtest_p2p_connect_handler";
              ];
        ]

let tezos_context_sigs =
  public_lib
    "tezos-context.sigs"
    ~path:"src/lib_context/sigs"
    ~opam:"src/lib_context/tezos-context"
    ~deps:[tezos_base |> open_ ~m:"TzPervasives"; tezos_stdlib |> open_]

let tezos_context_encoding =
  public_lib
    "tezos-context.encoding"
    ~path:"src/lib_context/encoding"
    ~opam:"src/lib_context/tezos-context"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib |> open_;
        irmin;
        irmin_pack;
      ]

let tezos_context_helpers =
  public_lib
    "tezos-context.helpers"
    ~path:"src/lib_context/helpers"
    ~opam:"src/lib_context/tezos-context"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib |> open_;
        tezos_context_encoding;
        tezos_context_sigs;
        irmin;
        irmin_pack;
      ]

let tezos_context_memory =
  public_lib
    "tezos-context.memory"
    ~path:"src/lib_context/memory"
    ~opam:"src/lib_context/tezos-context"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib |> open_;
        irmin_pack;
        irmin_pack_mem;
        tezos_context_sigs;
        tezos_context_encoding;
        tezos_context_helpers;
      ]

let tezos_context =
  public_lib
    "tezos-context"
    ~path:"src/lib_context"
    ~synopsis:"Tezos: on-disk context abstraction for `tezos-node`"
    ~deps:
      [
        tezos_shell_services |> open_;
        tezos_base |> open_ ~m:"TzPervasives";
        bigstringaf;
        fmt;
        logs_fmt;
        digestif_c;
        irmin;
        irmin_pack;
        irmin_pack_unix;
        tezos_stdlib_unix |> open_;
        tezos_stdlib |> open_;
        tezos_context_sigs;
        tezos_context_helpers;
        tezos_context_encoding;
        tezos_context_memory;
      ]

let _tezos_context_tests =
  test
    "test"
    ~path:"src/lib_context/test"
    ~opam:"src/lib_context/tezos-context"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_context |> open_;
        tezos_stdlib_unix |> open_;
        alcotest_lwt;
      ]
    ~modules:["assert"; "test_context"; "test"]

let _tezos_context_memory_tests =
  test
    "test"
    ~path:"src/lib_context/memory/test"
    ~opam:"src/lib_context/tezos-context"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_context;
        tezos_context_memory;
        tezos_stdlib_unix |> open_;
        alcotest_lwt;
      ]

(* This binding assumes that librustzcash.a is installed in the system default
   directories or in: $OPAM_SWITCH_PREFIX/lib *)
let tezos_sapling =
  public_lib
    "tezos-sapling"
    ~path:"src/lib_sapling"
    ~synopsis:"OCaml library for the Sapling protocol, using librustzcash"
    ~deps:
      [
        conf_rust;
        integers;
        integers_stubs_js;
        ctypes;
        ctypes_stubs_js;
        data_encoding;
        tezos_stdlib |> open_;
        tezos_crypto |> open_;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_rust_lib;
        tezos_lwt_result_stdlib;
      ]
    ~js_of_ocaml:[[S "javascript_files"; S "runtime.js"]]
    ~foreign_stubs:
      {
        language = C;
        flags =
          [":standard"; "-I%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs"];
        names = ["rustzcash_ctypes_c_stubs"];
      }
    ~c_library_flags:
      [
        "-L%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs";
        "-lrustzcash";
        "-lpthread";
      ]
    ~dune:
      Dune.
        [
          [S "copy_files"; S "bindings/rustzcash_ctypes_bindings.ml"];
          [
            S "rule";
            [S "target"; S "runtime.js"];
            [S "deps"; [S ":gen"; S "./bindings/gen_runtime_js.exe"]];
            [
              S "action";
              [S "with-stdout-to"; S "%{target}"; run "%{gen}" ["%{target}"]];
            ];
          ];
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

let _tezos_sapling_tests =
  tests
    ["test_rustzcash"; "test_keys"; "test_merkle"; "test_roots"; "test_sapling"]
    ~path:"src/lib_sapling/test"
    ~opam:"src/lib_sapling/tezos-sapling"
    ~dep_files:["vectors.csv"; "vectors-zip32.csv"]
    ~deps:
      [
        tezos_sapling |> open_;
        tezos_crypto |> open_;
        str;
        tezos_base;
        tezos_base_unix;
        tezos_stdlib |> open_;
        tezos_stdlib_unix;
        data_encoding |> open_;
        tezos_base_test_helpers |> open_;
        alcotest_lwt;
      ]
    ~all_modules_except:["test_js"]

let _tezos_sapling_js_tests =
  test
    "test_js"
    ~path:"src/lib_sapling/test"
    ~opam:"src/lib_sapling/tezos-sapling"
    ~deps:[tezos_sapling; tezos_hacl]
    ~modules:["test_js"]
    ~linkall:true
    ~modes:[JS]
    ~js_compatible:true

let _tezos_sapling_ctypes_gen =
  private_exes
    ["rustzcash_ctypes_gen"; "gen_runtime_js"]
    ~path:"src/lib_sapling/bindings"
    ~opam:"src/lib_sapling/tezos-sapling"
    ~bisect_ppx:false
    ~deps:[ctypes_stubs; ctypes]
    ~modules:
      ["rustzcash_ctypes_gen"; "rustzcash_ctypes_bindings"; "gen_runtime_js"]

let tezos_protocol_environment_sigs_stdlib_compat =
  public_lib
    "tezos-protocol-environment.sigs.stdlib-compat"
    ~path:"src/lib_protocol_environment/sigs/stdlib_compat"
    ~opam:"src/lib_protocol_environment/tezos-protocol-environment"
    ~modules_without_implementation:["V_all"; "V2"; "V3"; "V4"]

let tezos_protocol_environment_sigs =
  public_lib
    "tezos-protocol-environment.sigs"
    ~path:"src/lib_protocol_environment/sigs"
    ~opam:"src/lib_protocol_environment/tezos-protocol-environment"
    ~ocaml:V.(at_least "4.12")
    ~deps:[tezos_protocol_environment_sigs_stdlib_compat]
    ~nopervasives:true
    ~nostdlib:true
    ~modules:["V0"; "V1"; "V2"; "V3"; "V4"; "V5"]
    ~dune:
      Dune.
        [
          include_ "v0.dune.inc";
          include_ "v1.dune.inc";
          include_ "v2.dune.inc";
          include_ "v3.dune.inc";
          include_ "v4.dune.inc";
          include_ "v5.dune.inc";
        ]

let tezos_protocol_environment_structs =
  public_lib
    "tezos-protocol-environment.structs"
    ~path:"src/lib_protocol_environment/structs"
    ~opam:"src/lib_protocol_environment/tezos-protocol-environment"
    ~deps:
      [
        tezos_stdlib;
        tezos_crypto;
        tezos_lwt_result_stdlib;
        data_encoding;
        bls12_381;
      ]
    ~modules:["V0"; "V1"; "V2"; "V3"; "V4"; "V5"]
    ~dune:
      Dune.
        [
          include_ "v0.dune.inc";
          include_ "v1.dune.inc";
          include_ "v2.dune.inc";
          include_ "v3.dune.inc";
          include_ "v4.dune.inc";
          include_ "v5.dune.inc";
        ]

let tezos_protocol_environment =
  public_lib
    "tezos-protocol-environment"
    ~path:"src/lib_protocol_environment"
    ~synopsis:"Interface layer between the protocols and the shell"
    ~description:
      {|The protocol-environment is a two-sided component sitting between the shell and
the protocols.

On one side, it provides a restricted typing environment to compile the
protocols against. This is a series of modules which replace the standard
library of OCaml. These modules purposefully omit many functionalities, thus
preventing the protocols from, say, directly writing to disk.

On the other side, it provides the shell with specific call-sites in the
protocols. These are the only entry-points into the otherwise black-box
protocols.|}
    ~deps:
      [
        zarith;
        zarith_stubs_js;
        bls12_381;
        ringo;
        ringo_lwt;
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_sapling;
        tezos_protocol_environment_sigs;
        tezos_protocol_environment_structs;
        tezos_micheline |> open_;
        tezos_context_memory;
        tezos_event_logging;
      ]
    ~wrapped:false
    ~modules:
      [
        "Tezos_protocol_environment";
        "Environment_V0";
        "Environment_V1";
        "Environment_V2";
        "Environment_V3";
        "Environment_V4";
        "Environment_V5";
        "Environment_cache";
        "Environment_context";
        "Environment_context_intf";
        "Environment_protocol_T";
        "Environment_protocol_T_V0";
        "Environment_protocol_T_V3";
        "Environment_protocol_T_test";
        "Dummy_context";
        "Memory_context";
        "Proxy_context";
        "Proxy_delegate";
      ]

let tezos_shell_context =
  public_lib
    "tezos-shell-context"
    ~path:"src/lib_protocol_environment"
    ~synopsis:
      "Tezos: economic-protocols environment implementation for `tezos-node`"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_protocol_environment;
        tezos_context;
      ]
    ~modules:["Proxy_delegate_maker"; "Shell_context"]

let _tezos_protocol_environment_tests =
  tests
    ["test"; "test_mem_context_array_theory"; "test_cache"]
    ~path:"src/lib_protocol_environment/test"
    ~opam:"src/lib_protocol_environment/tezos-protocol-environment"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_protocol_environment |> open_;
        alcotest_lwt;
        tezos_test_helpers;
        qcheck_alcotest;
        lwt_unix;
      ]

let _tezos_protocol_shell_context_tests =
  tests
    ["test_proxy_context"]
    ~path:"src/lib_protocol_environment/test_shell_context"
    ~opam:"src/lib_protocol_environment/tezos-shell-context-test"
    ~synopsis:"Testing the Shell Context"
    ~deps:
      [
        tezos_shell_context;
        alcotest_lwt;
        tezos_test_helpers |> open_;
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_protocol_environment |> open_;
      ]

let tezos_protocol_compiler_registerer =
  public_lib
    "tezos-protocol-compiler.registerer"
    ~path:"src/lib_protocol_compiler"
    ~opam:"src/lib_protocol_compiler/tezos-protocol-compiler"
    ~internal_name:"tezos_protocol_registerer"
    ~deps:
      [tezos_base |> open_ ~m:"TzPervasives"; tezos_protocol_environment_sigs]
    ~modules:["Registerer"]
    ~opaque:true
    ~dune:
      Dune.
        [
          targets_rule
            ["embedded_cmis.ml"]
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
                S "%{lib:stdlib:camlinternalFormatBasics.cmi}";
                S
                  "%{dep:.tezos_protocol_registerer.objs/byte/tezos_protocol_registerer__Registerer.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs.stdlib-compat:tezos_protocol_environment_sigs_stdlib_compat.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs.stdlib-compat:tezos_protocol_environment_sigs_stdlib_compat__V_all.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs.stdlib-compat:tezos_protocol_environment_sigs_stdlib_compat__V2.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs.stdlib-compat:tezos_protocol_environment_sigs_stdlib_compat__V3.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs.stdlib-compat:tezos_protocol_environment_sigs_stdlib_compat__V4.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs:tezos_protocol_environment_sigs.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs:tezos_protocol_environment_sigs__V0.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs:tezos_protocol_environment_sigs__V1.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs:tezos_protocol_environment_sigs__V2.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs:tezos_protocol_environment_sigs__V3.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs:tezos_protocol_environment_sigs__V4.cmi}";
                S
                  "%{lib:tezos-protocol-environment.sigs:tezos_protocol_environment_sigs__V5.cmi}";
              ];
        ]

let tezos_protocol_compiler_lib =
  public_lib
    "tezos-protocol-compiler"
    ~path:"src/lib_protocol_compiler"
    ~synopsis:"Tezos: economic-protocol compiler"
    ~ocaml:
      V.(
        (* Should be in sync with scripts/version.sh *)
        at_least "4.12.1" && less_than "4.13")
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix |> open_;
        tezos_version;
        tezos_protocol_environment_sigs;
        tezos_stdlib_unix |> open_;
        compiler_libs_common;
        lwt_unix;
        ocplib_ocamlres;
        unix;
      ]
    ~opam_only_deps:[tezos_protocol_environment]
    ~modules:["Embedded_cmis"; "Packer"; "Compiler"]

let tezos_protocol_compiler_native =
  public_lib
    "tezos-protocol-compiler.native"
    ~path:"src/lib_protocol_compiler"
    ~opam:"src/lib_protocol_compiler/tezos-protocol-compiler"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_protocol_compiler_lib |> open_;
        compiler_libs_optcomp;
      ]
    ~modules:["Native"]
    ~dune:
      Dune.
        [
          install
            [
              V
                [
                  S "dune_protocol.v0";
                  S "dune_protocol.v1";
                  S "dune_protocol.template.v0";
                  S "dune_protocol.template.v1";
                  S "final_protocol_versions";
                ];
            ]
            ~section:"libexec";
        ]

let tezos_protocol_updater =
  public_lib
    "tezos-protocol-updater"
    ~path:"src/lib_protocol_updater"
    ~synopsis:"Tezos: economic-protocol dynamic loading for `tezos-node`"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib_unix |> open_;
        tezos_micheline |> open_;
        tezos_shell_services |> open_;
        tezos_protocol_environment;
        tezos_shell_context;
        tezos_protocol_compiler_registerer;
        tezos_protocol_compiler_native;
        tezos_context |> open_;
        lwt_exit;
        dynlink;
      ]

let tezos_validation =
  public_lib
    "tezos-validation"
    ~path:"src/lib_validation"
    ~synopsis:"Tezos: library for blocks validation"
    ~time_measurement_ppx:true
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_context |> open_;
        tezos_shell_context |> open_;
        tezos_shell_services |> open_;
        tezos_protocol_updater |> open_;
        tezos_stdlib_unix |> open_;
      ]

let tezos_store =
  public_lib
    "tezos-store"
    ~path:"src/lib_store"
    ~synopsis:"Tezos: store for `tezos-node`"
    ~deps:
      [
        tezos_shell_services |> open_;
        tezos_base |> open_ |> open_ ~m:"TzPervasives";
        tezos_version;
        index;
        irmin_pack;
        tezos_context |> open_;
        tezos_validation |> open_;
        tezos_protocol_updater |> open_;
        tezos_stdlib_unix |> open_;
        tezos_stdlib |> open_;
        lwt_watcher;
        ringo_lwt;
        camlzip;
        tar;
        tar_unix;
        prometheus;
      ]

let tezos_test_helpers_extra =
  public_lib
    "tezos-test-helpers-extra"
    ~path:"src/lib_test"
    ~internal_name:"lib_test_extra"
    ~synopsis:"Test helpers dependent on tezos-base"
    ~deps:[tezos_base; tezos_crypto; tezos_test_helpers; tezos_shell_services]
    ~ocaml:V.(at_least "4.08")
    ~dune:
      Dune.
        [
          (* This rule is necessary for `make lint-tests-pkg`, without it dune
             complains that the alias is empty. *)
          alias_rule "runtest_js_base" ~action:(S "progn");
        ]
    ~modules:["assert_lib"]

let tezos_requester =
  public_lib
    "tezos-requester"
    ~path:"src/lib_requester"
    ~synopsis:"Tezos: generic resource fetching service"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib_unix |> open_;
        lwt_watcher;
      ]

let _tezos_requester_tests =
  tests
    ["test_requester"; "test_fuzzing_requester"]
    ~path:"src/lib_requester/test"
    ~opam:"src/lib_requester/tezos-requester"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_test_helpers;
        tezos_base_test_helpers |> open_;
        tezos_stdlib |> open_;
        tezos_stdlib_unix;
        tezos_requester |> open_;
        alcotest_lwt;
        qcheck_alcotest;
      ]

let tezos_shell =
  public_lib
    "tezos-shell"
    ~path:"src/lib_shell"
    ~synopsis:
      "Tezos: core of `tezos-node` (gossip, validation scheduling, mempool, \
       ...)"
    ~documentation:[]
    ~deps:
      [
        lwt_watcher;
        lwt_canceler;
        prometheus;
        tezos_base |> open_ ~m:"TzPervasives" |> open_;
        tezos_base_unix |> open_;
        tezos_context |> open_;
        tezos_store |> open_;
        tezos_shell_context |> open_;
        tezos_p2p |> open_;
        tezos_stdlib_unix |> open_;
        tezos_shell_services |> open_;
        tezos_p2p_services |> open_;
        tezos_protocol_updater |> open_;
        tezos_requester |> open_;
        tezos_workers |> open_;
        tezos_validation |> open_;
        tezos_version |> open_;
        lwt_exit;
      ]

let tezos_rpc_http =
  public_lib
    "tezos-rpc-http"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: library of auto-documented RPCs (http server and client)"
    ~deps:[tezos_base |> open_ ~m:"TzPervasives"; resto_cohttp]
    ~modules:["RPC_client_errors"; "media_type"]

let tezos_rpc_http_client =
  public_lib
    "tezos-rpc-http-client"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: library of auto-documented RPCs (http client)"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        resto_cohttp_client;
        tezos_rpc_http |> open_;
      ]
    ~modules:["RPC_client"]

let tezos_rpc_http_client_unix =
  public_lib
    "tezos-rpc-http-client-unix"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: unix implementation of the RPC client"
    ~deps:
      [
        tezos_stdlib_unix;
        tezos_base |> open_ ~m:"TzPervasives";
        cohttp_lwt_unix;
        resto_cohttp_client;
        tezos_rpc_http_client |> open_;
      ]
    ~modules:["RPC_client_unix"]

let tezos_rpc_http_server =
  public_lib
    "tezos-rpc-http-server"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: library of auto-documented RPCs (http server)"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib_unix |> open_;
        resto_cohttp_server;
        resto_acl;
        tezos_rpc |> open_;
        tezos_rpc_http |> open_;
      ]
    ~modules:["RPC_server"; "RPC_logging"]
    ~private_modules:["RPC_logging"]

let _tezos_rpc_http_server_tests =
  test
    "test_rpc_http"
    ~path:"src/lib_rpc_http/test"
    ~opam:"src/lib_rpc_http/tezos-rpc-http-server"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_stdlib |> open_;
        tezos_stdlib_unix;
        tezos_test_helpers |> open_;
        tezos_base_test_helpers |> open_;
        tezos_rpc_http_server |> open_;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let _tezos_context_merkle_proof_tests =
  test
    "test_merkle_proof"
    ~path:"src/lib_context/test"
    ~opam:"src/lib_context/tezos-context"
    ~deps:
      [
        tezos_base;
        tezos_base_unix;
        tezos_context;
        tezos_context_encoding;
        tezos_stdlib_unix;
        qcheck_alcotest;
        tezos_test_helpers;
      ]
    ~opens:["Tezos_base__TzPervasives"; "Tezos_context"; "Tezos_stdlib_unix"]
    ~modules:["test_merkle_proof"]

let tezos_validator_lib =
  public_lib
    "tezos-validator"
    ~path:"src/bin_validation"
    ~synopsis:
      "Tezos: `tezos-validator` binary for external validation of blocks"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_context |> open_;
        tezos_stdlib_unix |> open_;
        tezos_protocol_environment;
        tezos_shell |> open_;
        tezos_shell_services |> open_;
        tezos_validation |> open_;
        tezos_protocol_updater |> open_;
        tezos_shell_context |> open_;
      ]

let tezos_client_base =
  public_lib
    "tezos-client-base"
    ~path:"src/lib_client_base"
    ~synopsis:"Tezos: common helpers for `tezos-client`"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_rpc |> open_;
        tezos_shell_services |> open_;
        tezos_sapling;
      ]
    ~modules:[":standard"; "bip39_english"]
    ~linkall:true
    ~js_compatible:true
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

let _tezos_client_base_tests =
  tests
    ["bip39_tests"; "pbkdf_tests"]
    ~path:"src/lib_client_base/test"
    ~opam:"src/lib_client_base/tezos-client-base"
    ~deps:[tezos_base; tezos_client_base |> open_; alcotest]
    ~js_compatible:true
    ~modes:[Native; JS]

let _bip39_generator =
  private_exe
    "bip39_generator"
    ~path:"src/lib_client_base/gen"
    ~opam:"src/lib_client_base/tezos-client-base"
    ~bisect_ppx:false

let tezos_signer_services =
  public_lib
    "tezos-signer-services"
    ~path:"src/lib_signer_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-signer`"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_rpc |> open_;
        tezos_client_base |> open_;
      ]
    ~linkall:true
    ~js_compatible:true

let tezos_signer_backends =
  public_lib
    "tezos-signer-backends"
    ~path:"src/lib_signer_backends"
    ~synopsis:"Tezos: remote-signature backends for `tezos-client`"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib |> open_;
        tezos_client_base |> open_;
        tezos_rpc_http |> open_;
        tezos_rpc_http_client |> open_;
        tezos_signer_services |> open_;
        tezos_shell_services |> open_;
      ]

let _tezos_signer_backends_tests =
  test
    "test_encrypted"
    ~path:"src/lib_signer_backends/test"
    ~opam:"src/lib_signer_backends/tezos-signer-backends"
    ~deps:
      [
        tezos_base;
        tezos_base_unix;
        tezos_stdlib |> open_;
        tezos_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_crypto |> open_;
        tezos_client_base |> open_;
        tezos_signer_backends |> open_;
        alcotest_lwt;
      ]

let tezos_signer_backends_unix =
  public_lib
    "tezos-signer-backends.unix"
    ~path:"src/lib_signer_backends/unix"
    ~opam:"src/lib_signer_backends/tezos-signer-backends"
    ~deps:
      [
        ocplib_endian_bigstring;
        fmt;
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_stdlib_unix |> open_;
        tezos_stdlib |> open_;
        tezos_client_base |> open_;
        tezos_rpc_http |> open_;
        tezos_rpc_http_client |> open_;
        tezos_rpc_http_client_unix |> open_;
        tezos_signer_services |> open_;
        tezos_signer_backends |> open_;
        tezos_shell_services |> open_;
        select
          ~package:ledgerwallet_tezos
          ~source_if_present:"ledger.available.ml"
          ~source_if_absent:"ledger.none.ml"
          ~target:"ledger.ml";
      ]

let _tezos_signer_backends_unix_tests =
  test
    "test_crouching"
    ~path:"src/lib_signer_backends/unix/test"
    ~opam:"src/lib_signer_backends/tezos-signer-backends"
    ~deps:
      [
        tezos_error_monad |> open_;
        tezos_stdlib |> open_;
        tezos_crypto |> open_;
        tezos_client_base |> open_;
        tezos_signer_backends_unix |> open_;
        alcotest_lwt;
      ]

let tezos_client_commands =
  public_lib
    "tezos-client-commands"
    ~path:"src/lib_client_commands"
    ~synopsis:"Tezos: protocol agnostic commands for `tezos-client`"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_rpc |> open_;
        tezos_clic_unix |> open_;
        tezos_client_base |> open_;
        tezos_shell_services |> open_;
        tezos_p2p_services |> open_;
        tezos_stdlib_unix;
        tezos_signer_backends;
        data_encoding |> open_;
      ]
    ~linkall:true

let tezos_mockup_registration =
  public_lib
    "tezos-mockup-registration"
    ~path:"src/lib_mockup"
    ~synopsis:"Tezos: protocol registration for the mockup mode"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_client_base;
        tezos_shell_services;
        tezos_protocol_environment;
      ]
    ~modules:["registration"; "registration_intf"; "mockup_args"]

let tezos_mockup_proxy =
  public_lib
    "tezos-mockup-proxy"
    ~path:"src/lib_mockup_proxy"
    ~synopsis:"Tezos: local RPCs"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_client_base;
        tezos_protocol_environment;
        tezos_rpc_http;
        resto_cohttp_self_serving_client;
        tezos_rpc_http_client;
        tezos_shell_services;
      ]

(* Depends on tezos_p2p to register the relevant RPCs. *)
let tezos_mockup =
  public_lib
    "tezos-mockup"
    ~path:"src/lib_mockup"
    ~synopsis:"Tezos: library of auto-documented RPCs (mockup mode)"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_client_base;
        tezos_mockup_proxy;
        resto_cohttp_self_serving_client;
        tezos_rpc;
        tezos_p2p_services;
        tezos_p2p;
        tezos_protocol_environment;
        tezos_stdlib_unix;
        tezos_rpc_http;
        tezos_rpc_http_client;
        tezos_mockup_registration |> open_;
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

let tezos_mockup_commands =
  public_lib
    "tezos-mockup-commands"
    ~path:"src/lib_mockup"
    ~synopsis:"Tezos: library of auto-documented RPCs (commands)"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_client_commands;
        tezos_client_base;
        tezos_mockup |> open_;
        tezos_mockup_registration |> open_;
      ]
    ~modules:["mockup_wallet"; "mockup_commands"]

let _tezos_mockup_tests =
  tests
    ["test_mockup_args"; "test_fuzzing_mockup_args"; "test_persistence"]
    ~path:"src/lib_mockup/test"
    ~opam:"src/lib_mockup/tezos-mockup"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_test_helpers |> open_;
        tezos_mockup;
        tezos_mockup_registration;
        tezos_client_base;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let tezos_proxy =
  public_lib
    "tezos-proxy"
    ~path:"src/lib_proxy"
    ~synopsis:"Tezos: proxy"
    ~deps:
      [
        ringo_lwt;
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_clic;
        tezos_client_base;
        tezos_protocol_environment;
        tezos_rpc;
        tezos_shell_services;
        tezos_context_memory;
      ]

let tezos_proxy_rpc =
  public_lib
    "tezos-proxy.rpc"
    ~path:"src/lib_proxy/rpc"
    ~opam:"src/lib_proxy/tezos-proxy"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_client_base;
        tezos_mockup_proxy;
        tezos_rpc;
        tezos_proxy;
      ]

let _tezos_proxy_tests =
  tests
    [
      "test_proxy";
      "test_fuzzing_proxy_getter";
      "test_light";
      "test_fuzzing_light";
    ]
    ~path:"src/lib_proxy/test"
    ~opam:"src/lib_proxy/tezos-proxy"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_stdlib_unix;
        tezos_proxy;
        tezos_base_test_helpers |> open_;
        tezos_test_helpers;
        tezos_shell_services_test_helpers;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let tezos_proxy_server_config =
  public_lib
    "tezos-proxy-server-config"
    ~path:"src/lib_proxy_server_config"
    ~synopsis:"Tezos: proxy server configuration"
    ~deps:[tezos_base |> open_ ~m:"TzPervasives"; tezos_stdlib_unix]

let _tezos_proxy_server_config_tests =
  test
    "test_proxy_server_config"
    ~path:"src/lib_proxy_server_config/test"
    ~opam:"src/lib_proxy_server_config/tezos-proxy-server-config"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_proxy_server_config;
        tezos_test_helpers;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let tezos_client_base_unix =
  public_lib
    "tezos-client-base-unix"
    ~path:"src/lib_client_base_unix"
    ~synopsis:
      "Tezos: common helpers for `tezos-client` (unix-specific fragment)"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_rpc_http |> open_;
        tezos_rpc_http_client_unix |> open_;
        tezos_shell_services |> open_;
        tezos_stdlib_unix |> open_;
        tezos_client_base |> open_;
        tezos_client_commands |> open_;
        tezos_mockup;
        tezos_mockup_registration;
        tezos_mockup_commands |> open_;
        tezos_proxy;
        tezos_proxy_rpc;
        tezos_signer_backends_unix;
        lwt_exit;
      ]
    ~linkall:true

let _tezos_client_base_unix_tests =
  test
    "test_mockup_wallet"
    ~path:"src/lib_client_base_unix/test"
    ~opam:"src/lib_client_base_unix/tezos-client-base-unix"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_mockup_commands;
        tezos_client_base_unix;
        tezos_base_test_helpers |> open_;
        alcotest;
        alcotest_lwt;
      ]

(* Depends on pyml-plop because of Matrix module... pyml-plot should be split further. *)
let tezos_benchmark =
  public_lib
    "tezos-benchmark"
    ~path:"src/lib_benchmark"
    ~synopsis:
      "Tezos: library for writing benchmarks and performing simple parameter \
       inference"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib_unix |> open_;
        tezos_micheline;
        tezos_clic;
        data_encoding;
        prbnmcn_cgrph;
        prbnmcn_dagger;
        prbnmcn_dagger_stats;
        prbnmcn_stats;
        pringo;
        benchmark_utils;
        pyml_plot;
        ocaml_migrate_parsetree;
        opam_only "hashcons" V.True;
      ]

let tezos_benchmark_examples =
  public_lib
    "tezos-benchmark-examples"
    ~path:"src/lib_benchmark/example"
    ~opam:"src/lib_benchmark/tezos-benchmark-examples"
    ~synopsis:"Tezos: examples for lib-benchmarks"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib_unix;
        tezos_crypto;
        tezos_benchmark;
      ]

let _tezos_benchmark_tests =
  test
    "main_ci"
    ~path:"src/lib_benchmark/test"
    ~opam:"src/lib_benchmark/tezos-benchmark-tests"
    ~synopsis:"Tezos: tests for lib-benchmarks"
    ~deps:
      [
        alcotest_lwt;
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_stdlib_unix;
        tezos_micheline;
        tezos_crypto;
        tezos_benchmark;
        tezos_benchmark_examples;
      ]

(* unused lib? *)
let tezos_micheline_rewriting =
  public_lib
    "tezos-micheline-rewriting"
    ~path:"src/lib_benchmark/lib_micheline_rewriting"
    ~synopsis:"Tezos: library for rewriting Micheline expressions"
    ~deps:
      [
        zarith;
        zarith_stubs_js;
        tezos_stdlib |> open_;
        tezos_error_monad |> open_;
        tezos_micheline |> open_;
      ]

let tezos_shell_benchmarks =
  public_lib
    "tezos-shell-benchmarks"
    ~path:"src/lib_shell_benchmarks"
    ~synopsis:"Tezos: shell benchmarks"
    ~deps:
      [
        tezos_stdlib |> open_;
        tezos_base |> open_ |> open_ ~m:"TzPervasives";
        tezos_error_monad |> open_;
        tezos_benchmark |> open_;
        tezos_crypto |> open_;
        tezos_context;
        tezos_shell_context;
        tezos_micheline;
      ]
    ~linkall:true

let tezt =
  public_lib
    "tezt"
    ~path:"tezt/lib"
    ~synopsis:
      "Test framework for unit tests, integration tests, and regression tests"
    ~ocaml:V.(at_least "4.08")
    ~bisect_ppx:false
    ~deps:[re; lwt_unix; ezjsonm]

let tezt_performance_regression =
  public_lib
    "tezt-performance-regression"
    ~path:"tezt/lib_performance_regression"
    ~synopsis:"Performance regression test framework based on Tezt"
    ~bisect_ppx:false
    ~deps:[tezt |> open_ |> open_ ~m:"Base"; uri; cohttp_lwt_unix]

let tezt_tezos =
  public_lib
    "tezt-tezos"
    ~path:"tezt/lib_tezos"
    ~synopsis:"Tezos test framework based on Tezt"
    ~bisect_ppx:false
    ~deps:
      [
        tezt |> open_ |> open_ ~m:"Base";
        tezt_performance_regression |> open_;
        uri;
        hex;
        tezos_base;
        tezos_base_unix;
      ]
    ~cram:true

let _tezt_self_tests =
  public_exe
    "tezt-self-tests"
    ~internal_name:"main"
    ~path:"tezt/self_tests"
    ~synopsis:"Tests for the Tezos test framework based on Tezt"
    ~bisect_ppx:false
    ~deps:[tezt |> open_ |> open_ ~m:"Base"; tezt_tezos |> open_]
    ~cram:true
    ~dune:
      Dune.
        [
          [
            S "cram";
            [S "package"; S "tezt-self-tests"];
            [S "deps"; S "tezt.sh"; S "main.exe"];
          ];
        ]

let tezos_openapi =
  public_lib
    "tezos-openapi"
    ~path:"src/lib_openapi"
    ~synopsis:
      "Tezos: a library for querying RPCs and converting into the OpenAPI \
       format"
    ~deps:[ezjsonm; json_data_encoding; tezt]

(* PROTOCOL PACKAGES *)

module Protocol : sig
  type number = Alpha | V of int | Other

  (** Status of the protocol on Mainnet.

      - [Active]: the protocol is the current protocol on Mainnet, is being proposed,
        or was active recently and was not deleted or frozen yet.
        Or, it is protocol Alpha.
      - [Frozen]: the protocol is an old protocol of Mainnet which was frozen
        (its tests, daemons etc. have been removed).
      - [Overridden]: the protocol has been replaced using a user-activated protocol override.
      - [Not_mainnet]: this protocol was never on Mainnet (e.g. demo protocols). *)
  type status = Active | Frozen | Overridden | Not_mainnet

  type t

  val number : t -> number

  val status : t -> status

  (** Name without the number, e.g. "alpha" or "PsDELPH1". *)
  val name : t -> string

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

  val parameters_exn : t -> target

  val benchmarks_proto_exn : t -> target

  val baking_exn : t -> target

  val genesis : t

  val demo_noops : t

  val alpha : t

  (** List of all protocols. *)
  val all : t list

  (** List of active protocols. *)
  val active : t list

  (** Get packages to link.

      This takes a function that selects packages from a protocol.
      For instance, the node wants the embedded protocol and the plugin registerer,
      while the client wants the client commands etc.

      The result is the list of all such packages that exist.
      All of them are optional dependencies. *)
  val all_optionally : (t -> target option) list -> target list
end = struct
  type number = Alpha | V of int | Other

  type status = Active | Frozen | Overridden | Not_mainnet

  type t = {
    number : number;
    status : status;
    name : string;
    main : target;
    embedded : target;
    client : target option;
    client_commands : target option;
    client_commands_registration : target option;
    baking_commands_registration : target option;
    plugin : target option;
    plugin_registerer : target option;
    test_helpers : target option;
    parameters : target option;
    benchmarks_proto : target option;
    baking : target option;
  }

  let make ?(number = Other) ?client ?client_commands
      ?client_commands_registration ?baking_commands_registration ?plugin
      ?plugin_registerer ?test_helpers ?parameters ?benchmarks_proto ?baking
      ~status ~name ~main ~embedded () =
    {
      number;
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
      test_helpers;
      parameters;
      benchmarks_proto;
      baking;
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

  let number p = p.number

  let status p = p.status

  let name p = p.name

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

  let parameters_exn p = mandatory "parameters" p p.parameters

  let benchmarks_proto_exn p = mandatory "benchmarks_proto" p p.benchmarks_proto

  let baking_exn p = mandatory "baking" p p.baking

  (* For now we declare some packages as external packages just so
     that we can depend on them, but their dune and .opam files are
     not yet generated. *)
  let todo ?opam ?main_module x =
    Printf.ksprintf (fun name -> external_lib ?opam ?main_module name V.True) x

  let genesis =
    let name_dash = "genesis" in
    let name_underscore = "genesis" in
    let main =
      todo
        ~main_module:(sf "Tezos_protocol_%s" name_underscore)
        "tezos-protocol-%s"
        name_dash
    in
    let client =
      public_lib
        (sf "tezos-client-%s" name_dash)
        ~path:(sf "src/proto_%s/lib_client" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_shell_services |> open_;
            tezos_client_base |> open_;
            tezos_protocol_environment;
            main |> open_;
            tezos_client_commands |> open_;
            tezos_proxy;
            tezos_stdlib_unix;
          ]
        ~linkall:true
    in
    register
    @@ make
         ~name:"genesis"
         ~status:Not_mainnet
         ~main:(todo "tezos-protocol-genesis")
         ~embedded:(todo "tezos-embedded-protocol-genesis")
         ~client
         ()

  let demo_noops =
    register
    @@ make
         ~name:"demo-noops"
         ~status:Not_mainnet
         ~main:(todo "tezos-protocol-demo-noops")
         ~embedded:(todo "tezos-embedded-protocol-demo-noops")
         ()

  let _demo_counter =
    let name_dash = "demo-counter" in
    let name_underscore = "demo_counter" in
    let main =
      todo
        ~main_module:(sf "Tezos_protocol_%s" name_underscore)
        "tezos-protocol-%s"
        name_dash
    in
    let client =
      public_lib
        (sf "tezos-client-%s" name_dash)
        ~path:(sf "src/proto_%s/lib_client" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_shell_services |> open_;
            tezos_client_base |> open_;
            tezos_client_commands |> open_;
            main |> open_;
          ]
        ~linkall:true
        ~warnings:"-9+27-30-32-40@8"
    in
    register
    @@ make
         ~name:"demo-counter"
         ~status:Not_mainnet
         ~main:(todo "tezos-protocol-demo-counter")
         ~embedded:(todo "tezos-embedded-protocol-demo-counter")
         ~client
         ()

  (* N as in "protocol number in the Alpha family". *)
  module N = struct
    (* This function is asymmetrical on purpose: we don't want to compare
       numbers with [Alpha] because such comparisons would break when snapshotting.
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
      | Alpha -> 1
      | V a -> Int.compare a b
      | Other -> invalid_arg "cannot use N.compare on Other protocols"

    let ( <= ) a b = compare_asymmetric a b <= 0

    let ( >= ) a b = compare_asymmetric a b >= 0

    let ( <> ) a b = compare_asymmetric a b <> 0

    let ( == ) a b = compare_asymmetric a b == 0
  end

  let register_alpha_family status number name =
    let make_full_name sep =
      match number with
      | Alpha -> name
      | V number -> sf "%03d%c%s" number sep name
      | Other ->
          invalid_arg
            "cannot use register_alpha_family to register Other protocols"
    in
    let name_dash = make_full_name '-' in
    let name_underscore = make_full_name '_' in
    let some_if condition make = if condition then Some (make ()) else None in
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
    let opt_map l f = Option.map f l in
    let warnings =
      if N.(number == 002 || number == 001) then Some "-9+27-30-32-40@8"
      else None
    in
    let main =
      todo
        ~main_module:(sf "Tezos_protocol_%s" name_underscore)
        "tezos-protocol-%s"
        name_dash
    in
    let embedded =
      todo
        ~main_module:(sf "Tezos_embedded_protocol_%s" name_underscore)
        "tezos-embedded-protocol-%s"
        name_dash
    in
    let environment =
      todo
        ~opam:(sf "tezos-protocol-%s" name_dash)
        ~main_module:(sf "Tezos_protocol_environment_%s" name_underscore)
        "tezos-protocol-%s.environment"
        name_dash
    in
    let raw_protocol =
      todo
        ~opam:(sf "tezos-protocol-%s" name_dash)
        ~main_module:(sf "Tezos_raw_protocol_%s" name_underscore)
        "tezos-protocol-%s.raw"
        name_dash
    in
    let parameters =
      some_if (N.(number >= 008) && not_overridden) @@ fun () ->
      public_lib
        (sf "tezos-protocol-%s-parameters" name_dash)
        ~path:(sf "src/proto_%s/lib_parameters" name_underscore)
        ~synopsis:"Tezos/Protocol: parameters"
        ~all_modules_except:["gen"]
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives";
            tezos_protocol_environment;
            main |> open_;
          ]
        ~linkall:true
    in
    let _parameters_exe =
      opt_map parameters @@ fun parameters ->
      private_exe
        "gen"
        ~path:(sf "src/proto_%s/lib_parameters" name_underscore)
        ~opam:
          (sf
             "src/proto_%s/lib_parameters/tezos-protocol-%s-parameters"
             name_underscore
             name_dash)
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives";
            parameters |> open_;
            main |> if_ N.(number >= 012) |> open_;
          ]
        ~modules:["gen"]
        ~linkall:true
        ~dune:
          Dune.(
            let gen_json name =
              targets_rule
                [name ^ "-parameters.json"]
                ~deps:[S "gen.exe"]
                ~action:[S "run"; S "%{deps}"; S ("--" ^ name)]
            in
            [
              gen_json "sandbox";
              gen_json "test";
              gen_json "mainnet";
              (* TODO: why do we install these files? *)
              install
                [
                  S "sandbox-parameters.json";
                  S "test-parameters.json";
                  S "mainnet-parameters.json";
                ]
                ~section:"lib";
            ])
        ~bisect_ppx:false
    in
    let test_helpers =
      todo
        "tezos-%s-test-helpers"
        name_dash
        ~main_module:(sf "Tezos_%s_test_helpers" name_underscore)
    in
    let plugin =
      some_if (N.(number >= 007) && not_overridden) @@ fun () ->
      public_lib
        (sf "tezos-protocol-plugin-%s" name_dash)
        ~path:(sf "src/proto_%s/lib_plugin" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol plugin"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
          ]
        ~all_modules_except:["Plugin_registerer"]
        ~bisect_ppx:N.(number >= 008)
    in
    let plugin_registerer =
      opt_map plugin @@ fun plugin ->
      public_lib
        (sf "tezos-protocol-plugin-%s-registerer" name_dash)
        ~path:(sf "src/proto_%s/lib_plugin" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol plugin registerer"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            embedded |> open_;
            plugin |> open_;
            tezos_shell |> open_;
          ]
        ~modules:["Plugin_registerer"]
        ~bisect_ppx:N.(number >= 008)
    in
    let _plugin_tests =
      opt_map plugin @@ fun plugin ->
      some_if (active && N.(number <> 011)) @@ fun () ->
      tests
        ["test_consensus_filter"; "test_filter_state"; "test_plugin"]
        ~path:(sf "src/proto_%s/lib_plugin/test" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol plugin tests"
        ~opam:
          (sf
             "src/proto_%s/lib_plugin/tezos-protocol-plugin-%s-tests"
             name_underscore
             name_dash)
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_base_test_helpers |> open_;
            tezos_base_unix |> if_ N.(number >= 013);
            alcotest_lwt;
            tezos_test_helpers;
            qcheck_alcotest;
            tezos_stdlib_unix;
            tezos_micheline |> open_;
            plugin |> open_;
            environment |> open_;
            main |> open_ |> open_ ~m:"Protocol";
            parameters |> if_some |> open_;
            test_helpers |> open_;
          ]
    in
    let client =
      some_if not_overridden @@ fun () ->
      public_lib
        (sf "tezos-client-%s" name_dash)
        ~path:(sf "src/proto_%s/lib_client" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_shell_services |> open_;
            tezos_client_base |> open_;
            main |> open_;
            tezos_mockup_registration |> if_ N.(number >= 007);
            tezos_proxy |> if_ N.(number >= 007);
            tezos_signer_backends |> if_ N.(number >= 001);
            plugin |> if_some |> open_if N.(number >= 008);
            parameters |> if_some |> if_ N.(number >= 008) |> open_;
            tezos_rpc |> if_ N.(number >= 001) |> open_;
            tezos_client_commands |> if_ N.(number == 000) |> open_;
            tezos_stdlib_unix |> if_ N.(number == 000);
          ]
        ~bisect_ppx:N.(number >= 008)
        ?inline_tests:(if N.(number >= 009) then Some ppx_inline_test else None)
        ~linkall:true
        ?warnings
    in
    let _client_tests =
      some_if N.(number >= 011) @@ fun () ->
      tests
        [
          "test_michelson_v1_macros";
          "test_client_proto_contracts";
          "test_client_proto_context";
          "test_proxy";
        ]
        ~path:(sf "src/proto_%s/lib_client/test" name_underscore)
        ~opam:
          (sf
             "src/proto_%s/lib_client/tezos-client-%s"
             name_underscore
             name_dash)
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_micheline |> open_;
            client |> if_some |> open_;
            main |> open_;
            tezos_base_test_helpers |> open_;
            tezos_test_helpers |> open_;
            alcotest_lwt;
            qcheck_alcotest;
          ]
    in
    let client_commands =
      some_if (N.(number >= 001) && not_overridden) @@ fun () ->
      public_lib
        (sf "tezos-client-%s-commands" name_dash)
        ~path:(sf "src/proto_%s/lib_client_commands" name_underscore)
        ~synopsis:
          "Tezos/Protocol: protocol-specific commands for `tezos-client`"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
            parameters |> if_some |> if_ N.(number >= 013) |> open_;
            tezos_stdlib_unix |> open_;
            tezos_protocol_environment;
            tezos_shell_services |> open_;
            tezos_mockup |> if_ N.(number >= 007);
            tezos_mockup_registration |> if_ N.(number >= 007);
            tezos_mockup_commands |> if_ N.(number >= 007);
            tezos_client_base |> open_;
            client |> if_some |> open_;
            tezos_client_commands |> open_;
            tezos_rpc |> open_;
            tezos_client_base_unix |> if_ N.(number >= 009) |> open_;
            plugin |> if_some |> if_ N.(number >= 008) |> open_;
          ]
        ~bisect_ppx:N.(number >= 008)
        ~linkall:true
        ~all_modules_except:["alpha_commands_registration"]
        ?warnings
    in
    let client_sapling =
      some_if (N.(number >= 008) && not_overridden) @@ fun () ->
      public_lib
        (sf "tezos-client-sapling-%s" name_dash)
        ~path:(sf "src/proto_%s/lib_client_sapling" name_underscore)
        ~synopsis:"Tezos: sapling support for `tezos-client`"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_crypto;
            tezos_stdlib_unix |> open_;
            tezos_client_base |> open_;
            tezos_signer_backends;
            client |> if_some |> open_;
            client_commands |> if_some |> open_;
            main |> open_;
            environment |> open_;
            plugin |> if_some |> if_ N.(number >= 013) |> open_;
          ]
        ~linkall:true
    in
    let client_commands_registration =
      some_if (N.(number >= 001) && not_overridden) @@ fun () ->
      let is_sublib = N.(number <= 006) in
      public_lib
        (if is_sublib then sf "tezos-client-%s-commands.registration" name_dash
        else sf "tezos-client-%s-commands-registration" name_dash)
        ~path:(sf "src/proto_%s/lib_client_commands" name_underscore)
        ?opam:
          ( some_if is_sublib @@ fun () ->
            sf
              "src/proto_%s/lib_client_commands/tezos-client-%s-commands"
              name_underscore
              name_dash )
        ?synopsis:
          (if is_sublib then None
          else
            Some "Tezos/Protocol: protocol-specific commands for `tezos-client`")
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
            parameters |> if_some |> if_ N.(number >= 013) |> open_;
            tezos_protocol_environment;
            tezos_shell_services |> open_;
            tezos_client_base |> open_;
            client |> if_some |> open_;
            tezos_client_commands |> open_;
            client_commands |> if_some |> open_;
            client_sapling |> if_some |> if_ N.(number >= 008) |> open_;
            tezos_rpc |> open_;
            plugin |> if_some |> if_ N.(number >= 008) |> open_;
          ]
        ~bisect_ppx:N.(number >= 008)
        ~linkall:true
        ~modules:["alpha_commands_registration"]
        ?warnings
    in
    let baking =
      some_if active @@ fun () ->
      public_lib
        ("tezos-baking-" ^ name_dash)
        ~path:(sf "src/proto_%s/lib_delegate" name_underscore)
        ~synopsis:
          (if N.(number <= 011) then
           "Tezos/Protocol: base library for `tezos-baker/endorser/accuser`"
          else "Tezos/Protocol: base library for `tezos-baker/accuser`")
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_version;
            main |> open_;
            plugin |> if_some |> open_;
            tezos_protocol_environment;
            tezos_shell_services |> open_;
            tezos_client_base |> open_;
            client |> if_some |> open_;
            tezos_client_commands |> open_;
            tezos_stdlib |> open_;
            tezos_stdlib_unix |> open_;
            tezos_shell_context |> open_;
            tezos_context |> open_;
            tezos_context_memory |> if_ N.(number >= 012);
            tezos_rpc_http_client_unix |> if_ N.(number >= 011);
            tezos_rpc |> open_;
            tezos_rpc_http |> open_;
            lwt_canceler;
            lwt_exit;
          ]
        ~opam_only_deps:(if N.(number <= 011) then [] else [tezos_tooling])
        ~linkall:true
        ~all_modules_except:
          (if N.(number <= 011) then
           ["Delegate_commands"; "Delegate_commands_registration"]
          else ["Baking_commands"; "Baking_commands_registration"])
    in
    let tenderbrute =
      some_if (active && N.(number >= 013)) @@ fun () ->
      public_lib
        (sf "tezos-baking-%s.tenderbrute" name_dash)
        ~internal_name:"tenderbrute"
        ~path:
          (sf "src/proto_%s/lib_delegate/test/tenderbrute/lib" name_underscore)
        ~opam:
          (sf
             "src/proto_%s/lib_delegate/tezos-baking-%s"
             name_underscore
             name_dash)
        ~deps:
          [
            data_encoding |> open_;
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
            |> open_;
            tezos_base_unix;
            main |> open_;
            tezos_client_base |> open_;
            client |> if_some |> open_;
          ]
        ~bisect_ppx:false
    in
    let _tenderbrute_exe =
      some_if (active && N.(number >= 013)) @@ fun () ->
      test
        "tenderbrute_main"
        ~runtest:false
        ~path:(sf "src/proto_%s/lib_delegate/test/tenderbrute" name_underscore)
        ~opam:
          (sf
             "src/proto_%s/lib_delegate/tezos-baking-%s"
             name_underscore
             name_dash)
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
            |> open_;
            tezos_client_base |> open_;
            client |> if_some |> open_;
            main |> open_;
            tenderbrute |> if_some;
          ]
        ~linkall:true
    in
    let _baking_tests =
      opt_map baking @@ fun baking ->
      some_if N.(number >= 011) @@ fun () ->
      let mockup_simulator =
        some_if N.(number >= 012) @@ fun () ->
        public_lib
          (sf "tezos-baking-%s.mockup-simulator" name_dash)
          ~internal_name:(sf "tezos_%s_mockup_simulator" name_underscore)
          ~path:
            (sf
               "src/proto_%s/lib_delegate/test/mockup_simulator"
               name_underscore)
          ~opam:
            (sf
               "src/proto_%s/lib_delegate/tezos-baking-%s"
               name_underscore
               name_dash)
          ~deps:
            [
              tezos_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              main |> open_;
              client |> if_some |> open_;
              tezos_client_commands |> open_;
              baking |> open_;
              tezos_stdlib_unix |> open_;
              tezos_client_base_unix |> open_;
              parameters |> if_some |> open_;
              tezos_mockup;
              tezos_mockup_proxy;
              tezos_mockup_commands;
              tenderbrute |> if_some |> if_ N.(number >= 013);
            ]
          ~opens:[sf "Tezos_protocol_%s.Protocol" name_underscore]
          ~bisect_ppx:false
      in
      test
        "main"
        ~path:(sf "src/proto_%s/lib_delegate/test" name_underscore)
        ~opam:
          (sf
             "src/proto_%s/lib_delegate/tezos-baking-%s"
             name_underscore
             name_dash)
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_protocol_environment |> if_ N.(number <= 011);
            tezos_test_helpers |> if_ N.(number <= 011);
            tezos_micheline |> open_;
            client |> if_some |> open_;
            main |> open_;
            environment |> open_;
            test_helpers |> if_ N.(number <= 011) |> open_;
            tezos_base_test_helpers |> open_;
            mockup_simulator |> if_some |> open_;
            tezos_client_base |> if_ N.(number <= 011);
            baking |> open_;
            parameters |> if_some |> if_ N.(number >= 012);
            tezos_crypto |> if_ N.(number >= 012);
            alcotest_lwt;
          ]
    in
    let baking_commands =
      some_if active @@ fun () ->
      public_lib
        (sf "tezos-baking-%s-commands" name_dash)
        ~path:(sf "src/proto_%s/lib_delegate" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol-specific commands for baking"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
            tezos_stdlib_unix |> open_;
            tezos_protocol_environment;
            tezos_shell_services |> open_;
            tezos_client_base |> open_;
            client |> if_some |> open_;
            tezos_client_commands |> open_;
            baking |> if_some |> open_;
            tezos_rpc |> open_;
          ]
        ~linkall:true
        ~modules:
          [
            (if N.(number <= 011) then "Delegate_commands"
            else "Baking_commands");
          ]
    in
    let baking_commands_registration =
      some_if active @@ fun () ->
      public_lib
        (sf "tezos-baking-%s-commands.registration" name_dash)
        ~opam:
          (sf
             "src/proto_%s/lib_delegate/tezos-baking-%s-commands"
             name_underscore
             name_dash)
        ~path:(sf "src/proto_%s/lib_delegate" name_underscore)
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives";
            main |> open_;
            tezos_protocol_environment;
            tezos_shell_services |> open_;
            tezos_client_base |> open_;
            client |> if_some |> open_;
            tezos_client_commands |> open_;
            baking |> if_some |> open_;
            baking_commands |> if_some |> open_;
            tezos_rpc |> open_;
          ]
        ~linkall:true
        ~modules:
          [
            (if N.(number <= 011) then "Delegate_commands_registration"
            else "Baking_commands_registration");
          ]
    in
    let daemon daemon =
      some_if active @@ fun () ->
      public_exe
        (sf "tezos-%s-%s" daemon name_dash)
        ~internal_name:(sf "main_%s_%s" daemon name_underscore)
        ~path:(sf "src/proto_%s/bin_%s" name_underscore daemon)
        ~synopsis:(sf "Tezos/Protocol: %s binary" daemon)
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
            client |> if_some |> open_;
            tezos_client_commands |> open_;
            baking_commands |> if_some |> open_;
            tezos_stdlib_unix |> open_;
            tezos_client_base_unix |> open_;
          ]
    in
    let _baker = daemon "baker" in
    let _accuser = daemon "accuser" in
    let _endorser = some_if N.(number <= 011) @@ fun () -> daemon "endorser" in
    let sc_rollup =
      some_if N.(number >= 013) @@ fun () ->
      public_lib
        (sf "tezos-sc-rollup-%s" name_dash)
        ~path:(sf "src/proto_%s/lib_sc_rollup" name_underscore)
        ~synopsis:
          "Tezos/Protocol: protocol specific library for `tezos-sc-rollup`"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives";
            main |> open_;
            plugin |> if_some |> open_;
            parameters |> if_some |> open_;
            tezos_rpc |> open_;
          ]
        ~inline_tests:ppx_inline_test
        ~linkall:true
    in
    let _sc_rollup_client =
      some_if (active && N.(number >= 013)) @@ fun () ->
      public_exe
        (sf "tezos-sc-rollup-client-%s" name_dash)
        ~internal_name:(sf "main_sc_rollup_client_%s" name_underscore)
        ~path:(sf "src/proto_%s/bin_sc_rollup_client" name_underscore)
        ~synopsis:"Tezos/Protocol: `tezos-sc-rollup-client-alpha` client binary"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_client_base;
            client |> if_some |> open_;
            tezos_client_commands |> open_;
            tezos_stdlib_unix |> open_;
            tezos_client_base_unix |> open_;
            tezos_rpc_http;
            tezos_rpc_http_client_unix |> open_;
            main |> open_;
            sc_rollup |> if_some |> open_;
          ]
    in
    let _sc_rollup_node =
      some_if (active && N.(number >= 013)) @@ fun () ->
      public_exe
        (sf "tezos-sc-rollup-node-%s" name_dash)
        ~internal_name:(sf "main_sc_rollup_node_%s" name_underscore)
        ~path:(sf "src/proto_%s/bin_sc_rollup_node" name_underscore)
        ~synopsis:"Tezos/Protocol: Smart Contract Rollup node binary"
        ~deps:
          [
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_client_commands |> open_;
            tezos_stdlib_unix |> open_;
            tezos_client_base |> open_;
            tezos_client_base_unix |> open_;
            client |> if_some |> open_;
            tezos_context_encoding;
            tezos_context_helpers;
            main |> open_;
            plugin |> if_some |> open_;
            parameters |> if_some |> open_;
            tezos_rpc |> open_;
            tezos_rpc_http;
            tezos_rpc_http_server;
            tezos_shell_services |> open_;
            sc_rollup |> if_some |> open_;
            data_encoding;
            irmin_pack;
            irmin_pack_unix;
            irmin;
            ringo;
            ringo_lwt;
          ]
    in
    let _tx_rollup_node =
      some_if (active && N.(number >= 013)) @@ fun () ->
      public_exe
        (sf "tezos-tx-rollup-node-%s" name_dash)
        ~internal_name:(sf "main_tx_rollup_node_%s" name_underscore)
        ~path:(sf "src/proto_%s/bin_tx_rollup_node" name_underscore)
        ~synopsis:"Tezos/Protocol: Transaction Rollup node binary"
        ~deps:
          [
            index;
            tezos_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
            |> open_;
            tezos_crypto |> open_;
            main |> open_;
            environment |> open_;
            client |> if_some |> open_;
            tezos_client_commands |> open_;
            tezos_context_encoding;
            baking_commands |> if_some |> open_;
            tezos_stdlib_unix |> open_;
            tezos_rpc |> open_;
            tezos_rpc_http |> open_;
            tezos_rpc_http_client_unix |> open_;
            tezos_rpc_http_server |> open_;
            tezos_micheline |> open_;
            tezos_client_base |> open_;
            tezos_client_base_unix |> open_;
            tezos_shell;
            tezos_store;
            tezos_workers |> open_;
          ]
    in
    let benchmark_type_inference =
      some_if active @@ fun () ->
      public_lib
        (sf "tezos-benchmark-type-inference-%s" name_dash)
        ~path:
          (sf
             "src/proto_%s/lib_benchmark/lib_benchmark_type_inference"
             name_underscore)
        ~synopsis:"Tezos: type inference for partial Michelson expressions"
        ~deps:
          [
            tezos_stdlib |> open_;
            tezos_error_monad |> open_;
            tezos_crypto;
            tezos_micheline |> open_;
            tezos_micheline_rewriting |> open_;
            main |> open_;
            hashcons;
          ]
    in
    let _benchmark_type_inference_tests =
      some_if active @@ fun () ->
      tests
        ["test_uf"; "test_inference"]
        ~path:
          (sf
             "src/proto_%s/lib_benchmark/lib_benchmark_type_inference/test"
             name_underscore)
        ~opam:
          (sf
             "src/proto_%s/lib_benchmark/lib_benchmark_type_inference/tezos-benchmark-type-inference-%s"
             name_underscore
             name_dash)
        ~deps:
          [
            tezos_micheline |> open_;
            tezos_micheline_rewriting;
            benchmark_type_inference |> if_some |> open_;
            main;
            tezos_error_monad;
            client |> if_some;
          ]
    in
    let benchmark =
      some_if active @@ fun () ->
      public_lib
        (sf "tezos-benchmark-%s" name_dash)
        ~path:(sf "src/proto_%s/lib_benchmark" name_underscore)
        ~synopsis:
          "Tezos/Protocol: library for writing benchmarks (protocol-specific \
           part)"
        ~deps:
          [
            tezos_stdlib |> open_;
            tezos_base |> open_
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_error_monad |> open_;
            tezos_micheline |> open_;
            tezos_micheline_rewriting |> open_;
            tezos_benchmark |> open_;
            benchmark_type_inference |> if_some |> open_;
            main |> open_;
            tezos_crypto |> open_;
            parameters |> if_some;
            hashcons;
            benchmark_utils;
            test_helpers |> open_;
            prbnmcn_stats;
          ]
        ~linkall:true
        ~private_modules:["kernel"; "rules"; "state_space"]
        ~bisect_ppx:N.(number <= 012)
    in
    let _benchmark_tests =
      some_if active @@ fun () ->
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
        ~path:(sf "src/proto_%s/lib_benchmark/test" name_underscore)
        ~opam:
          (sf
             "src/proto_%s/lib_benchmark/tezos-benchmark-%s"
             name_underscore
             name_dash)
        ~deps:
          [
            tezos_base
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_micheline |> open_;
            tezos_micheline_rewriting;
            main |> open_;
            tezos_benchmark |> open_;
            benchmark_type_inference |> if_some |> open_;
            benchmark |> if_some |> open_;
            test_helpers |> open_;
            tezos_error_monad;
            alcotest_lwt;
            prbnmcn_stats;
          ]
        ~runtest:false
        ~dune:
          Dune.
            [
              alias_rule
                "runtest_micheline_rewriting_data"
                ~action:(run_exe "test_sampling_data" ["1234"]);
              alias_rule
                "runtest_micheline_rewriting_code"
                ~action:(run_exe "test_sampling_code" ["1234"]);
              alias_rule
                "runtest"
                ~package:(sf "tezos-benchmark-%s" name_dash)
                ~alias_deps:
                  [
                    "runtest_micheline_rewriting_data";
                    "runtest_micheline_rewriting_code";
                  ];
            ]
    in
    let benchmarks_proto =
      some_if active @@ fun () ->
      public_lib
        (sf "tezos-benchmarks-proto-%s" name_dash)
        ~path:(sf "src/proto_%s/lib_benchmarks_proto" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol benchmarks"
        ~deps:
          [
            str;
            tezos_stdlib |> open_;
            tezos_base |> open_ |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            tezos_error_monad |> open_;
            parameters |> if_some;
            tezos_benchmark |> open_;
            benchmark |> if_some |> open_;
            benchmark_type_inference |> if_some |> open_;
            main |> open_;
            raw_protocol |> open_;
            tezos_crypto |> open_;
            tezos_shell_benchmarks;
            tezos_micheline |> open_;
            test_helpers |> open_;
            tezos_sapling;
            client |> if_some |> open_;
            tezos_tooling;
            tezos_protocol_environment;
          ]
        ~linkall:true
    in
    register
    @@ make
         ~number
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
         ~test_helpers
         ?parameters
         ?benchmarks_proto
         ?baking
         ()

  let active = register_alpha_family Active

  let frozen = register_alpha_family Frozen

  let overridden = register_alpha_family Overridden

  let _000_Ps9mPmXa = frozen (V 000) "Ps9mPmXa"

  let _001_PtCJ7pwo = frozen (V 001) "PtCJ7pwo"

  let _002_PsYLVpVv = frozen (V 002) "PsYLVpVv"

  let _003_PsddFKi3 = frozen (V 003) "PsddFKi3"

  let _004_Pt24m4xi = frozen (V 004) "Pt24m4xi"

  let _005_PsBABY5H = overridden (V 005) "PsBABY5H"

  let _005_PsBabyM1 = frozen (V 005) "PsBabyM1"

  let _006_PsCARTHA = frozen (V 006) "PsCARTHA"

  let _007_PsDELPH1 = frozen (V 007) "PsDELPH1"

  let _008_PtEdoTez = overridden (V 008) "PtEdoTez"

  let _008_PtEdo2Zk = frozen (V 008) "PtEdo2Zk"

  let _009_PsFLoren = frozen (V 009) "PsFLoren"

  let _010_PtGRANAD = frozen (V 010) "PtGRANAD"

  let _011_PtHangz2 = frozen (V 011) "PtHangz2"

  let _012_Psithaca = active (V 012) "Psithaca"

  let _013_PtJakart = active (V 013) "PtJakart"

  let alpha = active Alpha "alpha"

  let all = List.rev !all_rev

  let active = List.filter (fun p -> p.baking_commands_registration <> None) all

  let all_optionally (get_packages : (t -> target option) list) =
    let get_targets_for_protocol protocol =
      List.filter_map (fun get_package -> get_package protocol) get_packages
    in
    List.map get_targets_for_protocol all |> List.flatten |> List.map optional
end

(* TESTS THAT USE PROTOCOLS *)

let _tezos_micheline_rewriting_tests =
  test
    "test_rewriting"
    ~path:"src/lib_benchmark/lib_micheline_rewriting/test"
    ~opam:"src/lib_benchmark/lib_micheline_rewriting/tezos-micheline-rewriting"
    ~deps:
      [
        tezos_micheline |> open_;
        tezos_micheline_rewriting;
        Protocol.(main alpha);
        tezos_error_monad;
        Protocol.(client_exn alpha);
        alcotest_lwt;
      ]

let _tezos_store_tests =
  test
    "test"
    ~path:"src/lib_store/test"
    ~opam:"src/lib_store/tezos-store"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_store |> open_;
        tezos_shell_services |> open_;
        tezos_stdlib_unix |> open_;
        tezos_validation |> open_;
        Protocol.(embedded demo_noops);
        Protocol.(embedded genesis);
        Protocol.(embedded alpha);
        Protocol.(parameters_exn alpha |> open_);
        Protocol.(plugin_exn alpha) |> open_;
        alcotest_lwt;
        tezos_test_helpers;
        tezos_test_helpers_extra;
      ]
    ~runtest:false
    ~dune:
      (* [test_slow_manual] is a very long test, running a huge
         combination of tests that are useful for local testing for a
         given test suite. In addition to that, there is a memory leak
         is the tests (that could be in alcotest) which makes the test
         to consumes like > 10Gb of ram. For these reasons, we do not
         run these tests in the CI. *)
      Dune.
        [
          alias_rule
            "runtest"
            ~package:"tezos-store"
            ~action:(setenv "SLOW_TEST" "false" @@ run_exe "test" []);
          alias_rule
            "test_slow_manual"
            ~action:(setenv "SLOW_TEST" "true" @@ run_exe "test" []);
        ]

let _tezos_shell_tests =
  tests
    [
      "test_shell";
      "test_locator";
      "test_synchronisation_heuristic_fuzzy";
      "test_prevalidation";
      "test_prevalidation_t";
      "test_prevalidator_classification";
      "test_prevalidator_classification_operations";
      "test_prevalidator_pending_operations";
    ]
    ~path:"src/lib_shell/test"
    ~opam:"src/lib_shell/tezos-shell"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_test_helpers |> open_;
        tezos_store |> open_;
        tezos_context |> open_;
        tezos_shell_context |> open_;
        tezos_protocol_updater |> open_;
        tezos_p2p |> open_;
        tezos_p2p_services |> open_;
        tezos_requester;
        tezos_shell |> open_;
        tezos_shell_services |> open_;
        Protocol.(embedded demo_noops);
        tezos_stdlib_unix |> open_;
        tezos_validation |> open_;
        tezos_event_logging_test_helpers |> open_;
        tezos_test_helpers;
        alcotest_lwt;
      ]
    ~dune:
      Dune.
        [
          alias_rule
            "runtest_locator_bench"
            ~package:"tezos-shell"
            ~action:(run_exe "test_locator" ["--bench"]);
        ]

(* INTERNAL EXES *)

(* Not released, so no ~opam. *)
let _node_wrapper =
  private_exe
    "node_wrapper"
    ~path:"src/tooling"
    ~opam:""
    ~deps:[unix]
    ~modules:["node_wrapper"]
    ~bisect_ppx:false

let _tezos_protocol_compiler_bin =
  public_exe
    "tezos-protocol-compiler"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"src/lib_protocol_compiler/tezos-protocol-compiler"
    ~internal_name:"main_native"
    ~modes:[Native]
    ~deps:[tezos_protocol_compiler_native]
    ~linkall:true
    ~modules:["Main_native"]

let _tezos_protocol_compiler_tezos_protocol_packer =
  public_exe
    "tezos-protocol-compiler.tezos-protocol-packer"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"src/lib_protocol_compiler/tezos-protocol-compiler"
    ~internal_name:"main_packer"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_stdlib_unix |> open_;
        tezos_protocol_compiler_lib |> open_;
      ]
    ~modules:["Main_packer"]

let _tezos_embedded_protocol_packer =
  public_exe
    "tezos-embedded-protocol-packer"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"src/lib_protocol_compiler/tezos-protocol-compiler"
    ~internal_name:"main_embedded_packer"
    ~modes:[Native]
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix |> open_;
        tezos_stdlib_unix |> open_;
      ]
    ~linkall:true
    ~modules:["Main_embedded_packer"]

let _s_packer =
  private_exe
    "s_packer"
    ~path:"src/lib_protocol_environment/s_packer"
    ~opam:"src/lib_protocol_environment/tezos-protocol-environment"
    ~bisect_ppx:false
    ~dune:
      Dune.
        [
          install
            [as_ "s_packer.exe" "s_packer"]
            ~package:"tezos-protocol-environment"
            ~section:"libexec";
        ]

let _replace =
  private_exe
    "replace"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"src/lib_protocol_compiler/tezos-protocol-compiler"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_stdlib_unix |> open_;
        re_str;
      ]
    ~modules:["Replace"]
    ~static:true
    ~dune:Dune.[install [as_ "replace.exe" "replace"] ~section:"libexec"]

let _tezos_validator_bin =
  public_exe
    "tezos-validator"
    ~path:"src/bin_validation/bin"
    ~opam:"src/bin_validation/tezos-validator"
    ~internal_name:"main_validator"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_context |> open_;
        tezos_stdlib_unix |> open_;
        tezos_shell |> open_;
        tezos_shell_services |> open_;
        tezos_validation |> open_;
        tezos_protocol_updater |> open_;
        tezos_validator_lib |> open_;
      ]
    ~linkall:true

let _tezos_node =
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | (_, V 000) ->
            (* The node always needs to be linked with this protocol for Mainnet. *)
            false
        | (Active, V _) ->
            (* Active protocols cannot be optional because of a bug
               that results in inconsistent hashes. Once this bug is fixed,
               this exception can be removed. *)
            false
        | ((Frozen | Overridden | Not_mainnet), _) | (Active, (Alpha | Other))
          ->
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
  public_exe
    "tezos-node"
    ~path:"src/bin_node"
    ~internal_name:"main"
    ~synopsis:"Tezos: `tezos-node` binary"
    ~deps:
      ([
         tezos_base |> open_ ~m:"TzPervasives" |> open_;
         tezos_base_unix;
         tezos_version;
         tezos_stdlib_unix |> open_;
         tezos_shell_services |> open_;
         tezos_rpc_http |> open_;
         tezos_rpc_http_server |> open_;
         tezos_p2p |> open_;
         tezos_shell |> open_;
         tezos_store |> open_;
         tezos_context |> open_;
         tezos_validator_lib |> open_;
         tezos_validation |> open_;
         tezos_shell_context |> open_;
         tezos_workers |> open_;
         tezos_protocol_updater |> open_;
         cmdliner;
         fmt_cli;
         fmt_tty;
         tls;
         prometheus_app_unix;
         lwt_exit;
       ]
      @ protocol_deps)
    ~linkall:true
    ~dune:
      Dune.
        [
          install
            [as_ "tezos-sandboxed-node.sh" "tezos-sandboxed-node.sh"]
            ~package:"tezos-node"
            ~section:"bin";
        ]

let _tezos_client =
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | (Active, V _) -> false
        | ((Frozen | Overridden | Not_mainnet), _) | (Active, (Alpha | Other))
          ->
            true
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
    ["tezos-client"; "tezos-admin-client"]
    ~path:"src/bin_client"
    ~internal_names:["main_client"; "main_admin"]
    ~opam:"src/bin_client/tezos-client"
    ~synopsis:"Tezos: `tezos-client` binary"
    ~deps:
      ([
         tezos_base |> open_ ~m:"TzPervasives";
         tezos_base_unix;
         tezos_rpc_http_client |> open_;
         tezos_stdlib_unix |> open_;
         tezos_shell_services |> open_;
         tezos_client_base |> open_;
         tezos_client_commands |> open_;
         tezos_mockup_commands |> open_;
         tezos_proxy;
         tezos_client_base_unix |> open_;
         tezos_signer_backends_unix;
       ]
      @ protocol_deps)
    ~linkall:true
    ~dune:
      Dune.
        [
          install
            [
              as_
                "tezos-init-sandboxed-client.sh"
                "tezos-init-sandboxed-client.sh";
            ]
            ~package:"tezos-client"
            ~section:"bin";
        ]

let _tezos_codec =
  public_exe
    "tezos-codec"
    ~path:"src/bin_codec"
    ~internal_name:"codec"
    ~synopsis:"Tezos: `tezos-codec` binary to encode and decode values"
    ~deps:
      ([
         data_encoding |> open_;
         tezos_base |> open_ ~m:"TzPervasives";
         tezos_base_unix;
         tezos_client_base_unix |> open_;
         tezos_client_base |> open_;
         tezos_clic |> open_;
         tezos_stdlib_unix |> open_;
         tezos_event_logging |> open_;
         tezos_signer_services;
       ]
      @ Protocol.all_optionally
      @@ [
           (fun protocol ->
             let link =
               match Protocol.number protocol with
               | Alpha -> true
               | V number -> number >= 005
               | Other -> false
             in
             if link then Protocol.client protocol else None);
         ])
    ~linkall:true

let _tezos_sandbox =
  private_exe
    "main"
    ~path:"src/bin_sandbox"
    ~opam:""
    ~bisect_ppx:false
    ~deps:[tezos_base; tezos_base_unix; fmt; flextesa]

let _tezos_proxy_server =
  public_exe
    "tezos-proxy-server"
    ~path:"src/bin_proxy_server"
    ~internal_name:"main_proxy_server"
    ~synopsis:"Tezos: `tezos-proxy-server` binary"
    ~deps:
      ([
         tezos_base |> open_ ~m:"TzPervasives" |> open_;
         tezos_base_unix;
         tezos_stdlib_unix |> open_;
         cmdliner;
         lwt_exit;
         lwt_unix;
         tezos_proxy;
         tezos_proxy_server_config;
         tezos_rpc_http_client_unix;
         tezos_rpc_http_server;
         tezos_shell_services;
         tezos_shell_context;
         tezos_version;
       ]
      @ Protocol.all_optionally [Protocol.client; Protocol.plugin])
    ~linkall:true

let _tezos_snoop =
  public_exe
    "tezos-snoop"
    ~path:"src/bin_snoop"
    ~internal_name:"main_snoop"
    ~synopsis:"Tezos: `tezos-snoop` binary"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_stdlib_unix |> open_;
        tezos_clic;
        tezos_benchmark |> open_;
        tezos_benchmark_examples;
        tezos_shell_benchmarks;
        Protocol.(benchmarks_proto_exn alpha);
        str;
        ocamlgraph;
        pyml;
        pyml_plot;
        prbnmcn_stats;
      ]
    ~linkall:true

(* We use Dune's select statement and keep uTop optional *)
(* Keeping uTop optional lets `make build` succeed, *)
(* which uses tezos/opam-repository to resolve dependencies, *)
(* on the CI. This prevents having to add dev-dependency to *)
(* tezos/opam-repository unnecessarily *)
(* We set [~static] to false because we don't release this as a static binary. *)
let _tztop =
  public_exe
    "tztop"
    ~path:"src/tooling/tztop"
    ~internal_name:"tztop_main"
    ~opam:"src/tooling/tezos-tooling"
    ~modes:[Byte]
    ~bisect_ppx:false
    ~release:false
    ~static:false
    ~deps:
      [
        (* The following deps come from the original dune file. *)
        tezos_protocol_compiler_lib;
        tezos_base;
        compiler_libs_toplevel;
        select
          ~package:utop
          ~source_if_present:"tztop.utop.ml"
          ~source_if_absent:"tztop.vanilla.ml"
          ~target:"tztop.ml";
      ]

let _tezos_signer =
  public_exe
    "tezos-signer"
    ~path:"src/bin_signer"
    ~internal_name:"main_signer"
    ~synopsis:"Tezos: `tezos-signer` binary"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        tezos_base_unix;
        tezos_client_base |> open_;
        tezos_client_base_unix |> open_;
        tezos_client_commands |> open_;
        tezos_signer_services |> open_;
        tezos_rpc_http |> open_;
        tezos_rpc_http_server |> open_;
        tezos_rpc_http_client_unix |> open_;
        tezos_stdlib_unix |> open_;
        tezos_stdlib |> open_;
        tezos_signer_backends_unix;
      ]

let _rpc_openapi =
  private_exe
    "rpc_openapi"
    ~path:"src/bin_openapi"
    ~opam:""
    ~deps:[tezos_openapi]

let _tezos_tps_evaluation =
  public_exe
    "tezos-tps-evaluation"
    ~internal_name:"main_tps_evaluation"
    ~path:"src/bin_tps_evaluation"
    ~synopsis:"Tezos TPS evaluation tool"
    ~deps:
      [
        tezos_base |> open_ ~m:"TzPervasives";
        caqti;
        caqti_driver_postgresql;
        caqti_lwt;
        data_encoding;
        lwt;
        ppx_blob;
        Protocol.(baking_exn alpha);
        Protocol.(client_commands_exn alpha);
        tezos_client_base_unix;
        Protocol.(main alpha);
        tezt |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_;
        tezt_performance_regression |> open_;
      ]
    ~preprocess:[pps ppx_blob]
    ~preprocessor_deps:[File "./sql/get_all_operations.sql"]
    ~static:false
    ~release:false

(* For now we don't generate:
   - lib_protocol files (that's a TODO);
   - proto_/parameters/dune (it only has a (copy_files) stanza);
   - lib_time_measurement (its dune structure is *very* specific);
   - src/lib_protocol_compiler/test/dune (it does not define any library,
     executable or test stanza, it only defines aliases).

   Note that [filename] is relative to the manifest directory,
   i.e. it starts with "../". *)
let exclude filename =
  let is_in_lib_protocol =
    match String.split_on_char '/' filename with
    | ".." :: "src" :: maybe_proto :: "lib_protocol" :: _ ->
        has_prefix ~prefix:"proto_" maybe_proto
    | _ -> false
  in
  let is_protocol_parameters =
    match String.split_on_char '/' filename with
    | ".." :: "src" :: maybe_proto :: "parameters" :: _ ->
        has_prefix ~prefix:"proto_" maybe_proto
    | _ -> false
  in
  is_in_lib_protocol || is_protocol_parameters
  || has_prefix ~prefix:"../src/lib_time_measurement/" filename
  ||
  match filename with
  | "../src/lib_protocol_compiler/test/dune" -> true
  | _ -> false

(* Generate dune and opam files. *)
let () = generate ~exclude ()

(* Generate active_protocol_versions. *)
let () =
  let ch = open_out "../active_protocol_versions" in
  Fun.protect ~finally:(fun () -> close_out ch) @@ fun () ->
  let write_protocol protocol =
    match Protocol.number protocol with
    | Alpha -> Printf.fprintf ch "%s\n" (Protocol.name protocol)
    | V number -> Printf.fprintf ch "%03d-%s\n" number (Protocol.name protocol)
    | Other -> ()
  in
  List.iter write_protocol Protocol.active
