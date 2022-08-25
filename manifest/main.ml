(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
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

let warnings_disabled_by_default = [4; 40; 41; 42; 44; 45; 48; 60; 67; 69; 70]
(*
   4 [fragile-match]
  40 [name-out-of-scope]
  41 [ambiguous-name]
  42 [disambiguated-name]
  44 [open-shadow-identifier]
  45 [open-shadow-label-constructor]
  48 [eliminated-optional-arguments]
  60 [unused-module]
  67 [unused-functor-parameter]
  69 [unused-field]
  70 [missing-mli]
*)

let tezt_without_tezt_lib_dependency = tezt

(* Prevent using [tezt] until we define [tezt_lib]. *)
let tezt = () [@@warning "-unused-value-declaration"]

module V = Version

let sf = Printf.sprintf

module String_set = Set.Make (String)

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

(* EXTERNAL LIBS *)

let alcotest = external_lib ~js_compatible:true "alcotest" V.(at_least "1.5.0")

let alcotest_lwt = external_lib "alcotest-lwt" V.(at_least "1.5.0")

let astring = external_lib ~js_compatible:true "astring" V.True

let bigstring = external_lib ~js_compatible:true "bigstring" V.True

let bigstringaf =
  external_lib ~js_compatible:true "bigstringaf" V.(at_least "0.2.0")

let bisect_ppx = opam_only "bisect_ppx" V.(at_least "2.7.0")

let bls12_381 =
  let version = V.(at_least "4.0.0" && less_than "4.1.0") in
  external_lib
    ~js_compatible:true
    ~npm_deps:[Npm.make "@dannywillems/ocaml-bls12-381" version]
    "bls12-381"
    version

let bls12_381_polynomial =
  let version = V.at_least "0.1.0" in
  external_lib ~js_compatible:false "tezos-bls12-381-polynomial" version

let camlzip = external_lib "camlzip" V.(at_least "1.11" && less_than "1.12")

let caqti = external_lib "caqti" V.True

let caqti_lwt = external_lib "caqti-lwt" V.True

let caqti_dynload = external_lib "caqti-dynload" V.True

let cmdliner = external_lib "cmdliner" V.(at_least "1.1.0")

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
    V.(at_least "0.6" && less_than "0.7")

let digestif = external_lib ~js_compatible:true "digestif" V.(at_least "0.7.3")

let digestif_c = external_sublib digestif "digestif.c"

let dune_configurator = external_lib "dune-configurator" V.True

let dynlink = external_lib "dynlink" V.True ~opam:""

let ezjsonm = external_lib ~js_compatible:true "ezjsonm" V.(at_least "1.1.0")

let fmt = external_lib ~js_compatible:true "fmt" V.(at_least "0.8.7")

let fmt_cli = external_sublib fmt "fmt.cli"

let fmt_tty = external_sublib fmt "fmt.tty"

let hacl_star =
  external_lib
    ~js_compatible:true
    ~npm_deps:[Npm.make "hacl-wasm" V.(exactly "1.1.0")]
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

let irmin = external_lib "irmin" V.(at_least "3.3.2" && less_than "3.4.0")

let irmin_pack =
  external_lib "irmin-pack" V.(at_least "3.3.1" && less_than "3.4.0")

let irmin_pack_unix = external_sublib irmin_pack "irmin-pack.unix"

let irmin_pack_mem = external_sublib irmin_pack "irmin-pack.mem"

let json_data_encoding =
  external_lib
    ~js_compatible:true
    "json-data-encoding"
    V.(at_least "0.11" && less_than "0.13")

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

let ocamlformat = opam_only "ocamlformat" V.(exactly "0.21.0")

let ocamlgraph = external_lib "ocamlgraph" V.True

let ocplib_endian = external_lib ~js_compatible:true "ocplib-endian" V.True

let ocplib_endian_bigstring =
  external_sublib ocplib_endian "ocplib-endian.bigstring"

let ocplib_ocamlres =
  external_lib ~opam:"ocp-ocamlres" "ocplib-ocamlres" V.(at_least "0.4")

let ometrics = opam_only "ometrics" V.(at_least "0.2.1")

let ppx_expect = inline_tests_backend (external_lib "ppx_expect" V.True)

let plompiler = external_lib "tezos-plompiler" V.(at_least "0.1.2")

let plonk = external_lib "tezos-plonk" V.(at_least "0.1.2")

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

let pringo = external_lib "pringo" V.(at_least "1.3" && less_than "1.4")

let prometheus = external_lib "prometheus" V.(at_least "1.2")

let prometheus_app = external_lib "prometheus-app" V.(at_least "1.2")

let prometheus_app_unix = external_sublib prometheus_app "prometheus-app.unix"

let pyml = external_lib "pyml" V.True

let qcheck_alcotest =
  external_lib ~js_compatible:true "qcheck-alcotest" V.(at_least "0.18")

let qcheck_core = external_lib "qcheck-core" V.True

let re = external_lib ~js_compatible:true "re" V.(at_least "1.7.2")

let resto_version = V.(at_least "0.8" && less_than "0.9")

let resto = external_lib ~js_compatible:true "resto" resto_version

let resto_acl = external_lib "resto-acl" resto_version

let resto_cohttp = external_lib "resto-cohttp" resto_version

let resto_cohttp_client = external_lib "resto-cohttp-client" resto_version

let resto_cohttp_self_serving_client =
  external_lib "resto-cohttp-self-serving-client" resto_version

let resto_cohttp_server = external_lib "resto-cohttp-server" resto_version

let resto_directory =
  external_lib ~js_compatible:true "resto-directory" resto_version

let ringo = external_lib ~js_compatible:true "ringo" V.(at_least "0.9")

let ringo_lwt = external_lib "ringo-lwt" V.(at_least "0.9")

let secp256k1_internal =
  let version = V.(at_least "0.3.0") in
  external_lib
    ~npm_deps:[Npm.make "@nomadic-labs/secp256k1-wasm" version]
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

let uri = external_lib ~js_compatible:true "uri" V.(at_least "2.2.0")

let utop = external_lib "utop" V.(at_least "2.8")

let uutf = external_lib ~js_compatible:true "uutf" V.True

let vdf =
  external_lib ~js_compatible:true "class_group_vdf" V.(at_least "0.0.4")

(* The signature of the [Z] module has changed in 1.12. *)
let zarith =
  external_lib
    ~js_compatible:true
    "zarith"
    V.(at_least "1.12" && less_than "1.13")

let zarith_stubs_js = external_lib ~js_compatible:true "zarith_stubs_js" V.True

let ledgerwallet_tezos = external_lib "ledgerwallet-tezos" V.(at_least "0.2.1")

(* INTERNAL LIBS *)

let octez_test_helpers =
  public_lib
    "tezos-test-helpers"
    ~path:"src/lib_test"
    ~internal_name:"lib_test"
    ~synopsis:"Tezos-agnostic test helpers"
    ~deps:
      [uri; fmt; qcheck_alcotest; alcotest; lwt; pure_splitmix; data_encoding]
    ~js_compatible:true
    ~ocaml:V.(at_least "4.12")
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

let octez_expect_helper =
  public_lib
    "tezos-expect-helper"
    ~path:"src/lib_expect_helper"
    ~synopsis:"Tezos: helper for writing expect tests for Tezos"

let _octez_expect_helper_test =
  private_lib
    "tezos_expect_helper_test"
    ~opam:"tezos-expect-helper"
    ~path:"src/lib_expect_helper/test"
    ~deps:[octez_expect_helper]
    ~inline_tests:ppx_expect

let octez_stdlib =
  public_lib
    "tezos-stdlib"
    ~path:"src/lib_stdlib"
    ~synopsis:"Tezos: yet-another local-extension of the OCaml standard library"
    ~deps:[hex; zarith; zarith_stubs_js; lwt; ringo]
    ~ocaml:V.(at_least "4.12")
    ~js_compatible:true
    ~inline_tests:ppx_expect

let _octez_stdlib_tests =
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
    ~opam:"tezos-stdlib"
    ~modes:[Native; JS]
    ~deps:
      [
        octez_stdlib |> open_;
        alcotest;
        bigstring;
        octez_test_helpers;
        qcheck_alcotest;
      ]
    ~js_compatible:true

let _octez_stdlib_unix_tests =
  tests
    [
      "test_lwt_pipe";
      "test_circular_buffer";
      "test_circular_buffer_fuzzy";
      "test_hash_queue_lwt";
    ]
    ~path:"src/lib_stdlib/test-unix"
    ~opam:"tezos-stdlib"
    ~deps:
      [
        octez_stdlib |> open_;
        alcotest;
        alcotest_lwt;
        lwt_log;
        bigstring;
        lwt_unix;
        octez_test_helpers;
        qcheck_alcotest;
      ]

let octez_lwt_result_stdlib_bare_functor_outputs =
  public_lib
    "tezos-lwt-result-stdlib.bare.functor-outputs"
    ~path:"src/lib_lwt_result_stdlib/bare/functor_outputs"
    ~internal_name:"bare_functor_outputs"
    ~js_compatible:true
    ~deps:[lwt]
    ~opam_with_test:Only_on_64_arch

let octez_lwt_result_stdlib_bare_sigs =
  public_lib
    "tezos-lwt-result-stdlib.bare.sigs"
    ~path:"src/lib_lwt_result_stdlib/bare/sigs"
    ~internal_name:"bare_sigs"
    ~js_compatible:true
    ~deps:[lwt; octez_lwt_result_stdlib_bare_functor_outputs]
    ~opam_with_test:Only_on_64_arch

let octez_lwt_result_stdlib_bare_structs =
  public_lib
    "tezos-lwt-result-stdlib.bare.structs"
    ~path:"src/lib_lwt_result_stdlib/bare/structs"
    ~internal_name:"bare_structs"
    ~js_compatible:true
    ~deps:[lwt; octez_lwt_result_stdlib_bare_sigs]
    ~opam_with_test:Only_on_64_arch

let octez_lwt_result_stdlib_traced_functor_outputs =
  public_lib
    "tezos-lwt-result-stdlib.traced.functor-outputs"
    ~path:"src/lib_lwt_result_stdlib/traced/functor_outputs"
    ~internal_name:"traced_functor_outputs"
    ~js_compatible:true
    ~deps:[lwt; octez_lwt_result_stdlib_bare_sigs]
    ~opam_with_test:Only_on_64_arch

let octez_lwt_result_stdlib_traced_sigs =
  public_lib
    "tezos-lwt-result-stdlib.traced.sigs"
    ~path:"src/lib_lwt_result_stdlib/traced/sigs"
    ~internal_name:"traced_sigs"
    ~js_compatible:true
    ~deps:
      [
        lwt;
        octez_lwt_result_stdlib_bare_sigs;
        octez_lwt_result_stdlib_bare_structs;
        octez_lwt_result_stdlib_traced_functor_outputs;
      ]
    ~opam_with_test:Only_on_64_arch

let octez_lwt_result_stdlib_traced_structs =
  public_lib
    "tezos-lwt-result-stdlib.traced.structs"
    ~path:"src/lib_lwt_result_stdlib/traced/structs"
    ~internal_name:"traced_structs"
    ~js_compatible:true
    ~deps:
      [
        lwt;
        octez_lwt_result_stdlib_traced_sigs;
        octez_lwt_result_stdlib_bare_structs;
      ]
    ~opam_with_test:Only_on_64_arch

let octez_lwt_result_stdlib =
  public_lib
    "tezos-lwt-result-stdlib"
    ~path:"src/lib_lwt_result_stdlib"
    ~synopsis:"Tezos: error-aware stdlib replacement"
    ~ocaml:V.(at_least "4.12")
    ~js_compatible:true
    ~documentation:[Dune.[S "package"; S "tezos-lwt-result-stdlib"]]
    ~deps:
      [
        lwt;
        octez_lwt_result_stdlib_bare_sigs;
        octez_lwt_result_stdlib_bare_structs;
        octez_lwt_result_stdlib_traced_sigs;
        octez_lwt_result_stdlib_traced_structs;
      ]
    ~opam_with_test:Only_on_64_arch

let octez_lwt_result_stdlib_examples_traces =
  public_lib
    "tezos-lwt-result-stdlib.examples.traces"
    ~path:"src/lib_lwt_result_stdlib/examples/traces"
    ~internal_name:"traces"
    ~deps:
      [
        lwt;
        octez_lwt_result_stdlib_bare_structs;
        octez_lwt_result_stdlib_traced_sigs;
      ]
    ~opam_with_test:Only_on_64_arch

let _octez_lwt_result_stdlib_tests =
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
    ~opam:"tezos-lwt-result-stdlib"
    ~deps:
      [
        octez_lwt_result_stdlib |> open_;
        octez_lwt_result_stdlib_examples_traces;
        lwt_unix;
        alcotest_lwt;
        qcheck_alcotest;
        octez_test_helpers;
      ]
    ~opam_with_test:Only_on_64_arch

let octez_error_monad =
  public_lib
    "tezos-error-monad"
    ~path:"src/lib_error_monad"
    ~synopsis:"Tezos: error monad"
    ~ocaml:V.(at_least "4.07")
    ~deps:
      [
        octez_stdlib |> open_;
        data_encoding |> open_;
        lwt_canceler;
        lwt;
        octez_lwt_result_stdlib;
      ]
    ~conflicts:[external_lib "result" V.(less_than "1.5")]
    ~js_compatible:true

let octez_hacl =
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
                       ["run"; "gen/gen.exe"] :: ["-api"; "gen/api.json"]
                       :: List.map (fun s -> ["-stubs"; s]) js_stubs));
              ];
            ];
          ];
        ]

let _octez_hacl_gen0 =
  private_exe
    "gen0"
    ~path:"src/lib_hacl/gen/"
    ~opam:"tezos-hacl"
    ~bisect_ppx:false
    ~modules:["gen0"]
    ~deps:[compiler_libs_common]

let _octez_hacl_gen =
  private_exe
    "gen"
    ~path:"src/lib_hacl/gen/"
    ~opam:"tezos-hacl"
    ~bisect_ppx:false
    ~deps:[ctypes_stubs; ctypes; hacl_star_raw; ezjsonm]
    ~modules:["gen"; "bindings"; "api_json"]
    ~dune:
      (let package = "tezos-hacl" in
       Dune.
         [
           targets_rule
             ["bindings.ml"]
             ~deps:[Dune.(H [[S "package"; S "hacl-star-raw"]])]
             ~action:
               [
                 S "with-stdout-to";
                 S "%{targets}";
                 [
                   S "run";
                   S "./gen0.exe";
                   S "%{lib:hacl-star-raw:ocamlevercrypt.cma}";
                 ];
               ];
           [
             S "rule";
             [S "alias"; S "runtest_js"];
             [S "target"; S "api.json.corrected"];
             [S "package"; S package];
             [
               S "action";
               [
                 S "setenv";
                 S "NODE_PRELOAD";
                 S "hacl-wasm";
                 [S "run"; S "node"; S "%{dep:./check-api.js}"];
               ];
             ];
           ];
           alias_rule
             ~package
             "runtest_js"
             ~action:[S "diff"; S "api.json"; S "api.json.corrected"];
         ])

let _octez_hacl_tests =
  tests
    ["test_hacl"; "test_prop_hacl_hash"; "test_prop_signature_pk"]
    ~path:"src/lib_hacl/test"
    ~opam:"tezos-hacl"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        zarith;
        zarith_stubs_js;
        data_encoding |> open_;
        octez_hacl |> open_;
        qcheck_alcotest;
        octez_test_helpers;
      ]
    ~all_modules_except:["test"]
    ~modes:[Native; JS]
    ~js_compatible:true

let _octez_hacl_tests_1 =
  test
    "test"
    ~path:"src/lib_hacl/test"
    ~opam:"tezos-hacl"
    ~deps:
      [
        octez_stdlib;
        octez_error_monad;
        zarith;
        zarith_stubs_js;
        data_encoding;
        octez_hacl;
        qcheck_alcotest;
        octez_test_helpers;
      ]
    ~modules:["test"]
    ~modes:[Native; JS]
    ~js_compatible:true

let _octez_error_monad_tests =
  tests
    ["test_registration"; "test_splitted_error_encoding"]
    ~path:"src/lib_error_monad/test"
    ~opam:"tezos-error-monad"
    ~modes:[Native; JS]
    ~deps:[octez_error_monad |> open_; data_encoding; alcotest]
    ~js_compatible:true

let octez_rpc =
  public_lib
    "tezos-rpc"
    ~path:"src/lib_rpc"
    ~synopsis:
      "Tezos: library of auto-documented RPCs (service and hierarchy \
       descriptions)"
    ~deps:
      [
        data_encoding |> open_;
        octez_error_monad |> open_;
        resto;
        resto_directory;
        uri;
      ]
    ~js_compatible:true

let octez_crypto =
  public_lib
    "tezos-crypto"
    ~path:"src/lib_crypto"
    ~synopsis:
      "Tezos: library with all the cryptographic primitives used by Tezos"
    ~deps:
      [
        octez_stdlib |> open_;
        data_encoding |> open_;
        octez_lwt_result_stdlib;
        lwt;
        octez_hacl;
        secp256k1_internal;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_rpc |> open_;
        ringo;
        zarith;
        zarith_stubs_js;
        bls12_381;
      ]
    ~js_compatible:true

let _octez_crypto_tests =
  tests
    ["test_run"; "test_prop_signature"]
    ~path:"src/lib_crypto/test"
    ~opam:"tezos-crypto"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_crypto |> open_;
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        zarith;
        zarith_stubs_js;
        octez_hacl;
        data_encoding |> open_;
        alcotest;
        qcheck_alcotest;
        octez_test_helpers;
      ]
    ~modes:[Native; JS]
    ~js_compatible:true

let _octez_crypto_tests_unix =
  tests
    ["test_crypto_box"]
    ~path:"src/lib_crypto/test-unix"
    ~opam:"tezos-crypto"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_crypto |> open_;
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        zarith;
        zarith_stubs_js;
        octez_hacl;
        data_encoding |> open_;
        alcotest;
        alcotest_lwt;
        lwt_unix;
        qcheck_alcotest;
        octez_test_helpers;
      ]

let octez_crypto_dal =
  public_lib
    "tezos-crypto-dal"
    ~path:"src/lib_crypto_dal"
    ~synopsis:"DAL cryptographic primitives"
    ~opam:"tezos-crypto-dal"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_error_monad |> open_;
        data_encoding |> open_;
        octez_crypto;
        bls12_381_polynomial;
        lwt_unix;
      ]

let _octez_crypto_dal_tests =
  tests
    ["test_dal_cryptobox"]
    ~path:"src/lib_crypto_dal/test"
    ~opam:"tezos-crypto-dal"
    ~dep_files:["shard_proofs_precomp"]
    ~deps:
      [
        octez_stdlib |> open_;
        octez_crypto_dal |> open_;
        octez_error_monad |> open_;
        data_encoding |> open_;
        alcotest;
        qcheck_alcotest;
        bls12_381_polynomial;
      ]

let octez_event_logging =
  public_lib
    "tezos-event-logging"
    ~path:"src/lib_event_logging"
    ~synopsis:"Tezos event logging library"
    ~deps:
      [
        octez_stdlib |> open_;
        data_encoding |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib;
        lwt_log_core;
        uri;
      ]
    ~js_compatible:true

let octez_event_logging_test_helpers =
  public_lib
    "tezos-event-logging-test-helpers"
    ~path:"src/lib_event_logging/test_helpers"
    ~synopsis:"Tezos: test helpers for the event logging library"
    ~deps:
      [
        octez_stdlib;
        octez_lwt_result_stdlib;
        data_encoding;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_event_logging |> open_;
        octez_test_helpers;
        alcotest;
      ]
    ~js_compatible:true
    ~linkall:true
    ~bisect_ppx:false

let octez_stdlib_unix =
  public_lib
    "tezos-stdlib-unix"
    ~path:"src/lib_stdlib_unix"
    ~synopsis:
      "Tezos: yet-another local-extension of the OCaml standard library \
       (unix-specific fragment)"
    ~deps:
      [
        unix;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib;
        octez_event_logging |> open_;
        octez_stdlib |> open_;
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
        uri;
      ]

let octez_clic =
  public_lib
    "tezos-clic"
    ~path:"src/lib_clic"
    ~synopsis:
      "Tezos: library of auto-documented command-line-parsing combinators"
    ~deps:
      [
        octez_stdlib |> open_;
        lwt;
        re;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib;
      ]
    ~js_compatible:true

let octez_clic_unix =
  public_lib
    "tezos-clic.unix"
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
  test
    "test_clic"
    ~path:"src/lib_clic/test"
    ~opam:"tezos-clic"
    ~deps:[octez_stdlib |> open_; octez_clic |> open_; alcotest_lwt]

let _octez_clic_example =
  private_exe
    "clic_example"
    ~path:"src/lib_clic/examples"
    ~opam:""
    ~deps:[octez_clic; lwt_unix]
    ~bisect_ppx:false
    ~static:false

let octez_micheline =
  public_lib
    "tezos-micheline"
    ~path:"src/lib_micheline"
    ~synopsis:"Tezos: internal AST and parser for the Michelson language"
    ~deps:
      [
        uutf;
        zarith;
        zarith_stubs_js;
        octez_stdlib |> open_;
        octez_error_monad |> open_;
        data_encoding |> open_;
      ]
    ~js_compatible:true
    ~inline_tests:ppx_expect

let _octez_micheline_tests =
  private_lib
    "test_parser"
    ~path:"src/lib_micheline/test"
    ~opam:"tezos-micheline"
    ~inline_tests:ppx_expect
    ~modules:["test_parser"]
    ~deps:[octez_micheline |> open_]
    ~js_compatible:true

let _octez_micheline_tests =
  private_lib
    "test_diff"
    ~path:"src/lib_micheline/test"
    ~opam:"tezos-micheline"
    ~inline_tests:ppx_expect
    ~modules:["test_diff"]
    ~deps:[octez_micheline |> open_]
    ~js_compatible:true

let octez_base =
  public_lib
    "tezos-base"
    ~path:"src/lib_base"
    ~synopsis:"Tezos: meta-package and pervasive type definitions for Tezos"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_crypto |> open_;
        data_encoding |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_rpc |> open_;
        octez_clic |> open_;
        octez_micheline |> open_;
        octez_event_logging |> open_;
        ptime;
        ptime_clock_os;
        ezjsonm;
        lwt;
        ipaddr;
        uri;
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

let octez_base_unix =
  public_lib
    "tezos-base.unix"
    ~path:"src/lib_base/unix"
    ~deps:
      [
        octez_error_monad |> open_;
        octez_crypto |> open_;
        octez_base |> open_;
        octez_hacl;
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        data_encoding |> open_;
        uri;
      ]

let lib_base_tests ?dep_files names =
  tests
    names
    ~path:"src/lib_base/test"
    ~opam:"tezos-base"
    ~deps:
      [
        octez_base |> open_;
        octez_error_monad |> open_;
        data_encoding;
        qcheck_alcotest;
        octez_test_helpers;
      ]
    ?dep_files
    ~modes:[Native; JS]
    ~js_compatible:true
    ~modules:names

let _octez_base_tests_1 =
  lib_base_tests ["test_bounded"; "test_time"; "test_protocol"]

let _octez_base_tests_2 =
  lib_base_tests ["test_p2p_addr"] ~dep_files:["points.ok"; "points.ko"]

let _octez_base_tests_3 = lib_base_tests ["test_sized"]

let _octez_base_unix_tests =
  test
    "test_unix_error"
    ~path:"src/lib_base/unix/test"
    ~opam:"tezos-base"
    ~deps:
      [
        octez_base |> open_;
        octez_base_unix |> open_;
        octez_error_monad |> open_;
        data_encoding;
        octez_test_helpers;
        qcheck_alcotest;
      ]

let octez_base_test_helpers =
  public_lib
    "tezos-base-test-helpers"
    ~path:"src/lib_base/test_helpers"
    ~synopsis:"Tezos: Tezos base test helpers"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix;
        octez_event_logging_test_helpers;
        octez_test_helpers;
        alcotest;
        alcotest_lwt;
        qcheck_alcotest;
      ]
    ~linkall:true
    ~bisect_ppx:false

let lazy_containers =
  public_lib
    "lazy-containers"
    ~path:"src/lib_lazy_containers"
    ~synopsis:
      "A collection of lazy containers whose contents is fetched from \
       arbitrary backend on-demand"
    ~deps:[octez_lwt_result_stdlib; zarith]

let _lazy_containers_tests =
  test
    "main"
    ~path:"src/lib_lazy_containers/test"
    ~opam:"lazy-containers-tests"
    ~synopsis:"Various tests for the lazy containers library"
    ~dune:Dune.[[S "include_subdirs"; S "no"]]
    ~deps:
      [
        lazy_containers |> open_;
        qcheck_core;
        qcheck_alcotest;
        alcotest;
        lwt_unix;
      ]

let tree_encoding =
  public_lib
    "tree-encoding"
    ~path:"src/lib_tree_encoding"
    ~synopsis:
      "A general-purpose library to encode arbitrary data in Merkle trees"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        lazy_containers;
        octez_lwt_result_stdlib;
        data_encoding;
      ]

let octez_webassembly_interpreter =
  public_lib
    "tezos-webassembly-interpreter"
    ~path:"src/lib_webassembly"
    ~license:"Apache-2.0"
    ~extra_authors:["WebAssembly Authors"]
    ~synopsis:"WebAssembly reference interpreter with tweaks for Tezos"
    ~dune:Dune.[[S "include_subdirs"; S "unqualified"]]
    ~deps:[octez_lwt_result_stdlib; zarith; lazy_containers |> open_]

let _octez_webassembly_repl =
  private_exe
    "main"
    ~path:"src/lib_webassembly/bin"
    ~opam:""
    ~dune:Dune.[[S "include"; S "dune.inc"]]
    ~deps:
      [
        octez_webassembly_interpreter |> open_;
        lwt_unix;
        tree_encoding |> open_;
        lazy_containers |> open_;
      ]

let _octez_webassembly_test =
  test
    "main"
    ~path:"src/lib_webassembly/tests"
    ~opam:"tezos-webassembly-interpreter"
    ~dune:Dune.[[S "include_subdirs"; S "no"]]
    ~deps:[octez_webassembly_interpreter |> open_; alcotest]

let octez_version_parser =
  public_lib
    "tezos-version.parser"
    ~path:"src/lib_version/parser"
    ~dune:Dune.[ocamllex "tezos_version_parser"]
    ~js_compatible:true
    ~preprocess:[pps ppx_deriving_show]

let octez_version =
  public_lib
    "tezos-version"
    ~path:"src/lib_version"
    ~synopsis:"Tezos: version information generated from Git"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_version_parser]
    ~js_compatible:true
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
            ~action:[S "run"; S "./exe/get_git_info.exe"];
        ]

let _octez_version_get_git_info =
  private_exe
    "get_git_info"
    ~path:"src/lib_version/exe"
    ~opam:"tezos-version"
    ~deps:[dune_configurator; octez_version_parser]
    ~modules:["get_git_info"]
    ~bisect_ppx:false

let _octez_print_version_exe =
  public_exe
    "tezos-version"
    ~internal_name:"tezos_print_version"
    ~path:"src/lib_version/exe"
    ~deps:[octez_version |> open_; octez_base_unix]
    ~modules:["tezos_print_version"]
    ~bisect_ppx:false

let _octez_version_tests =
  test
    "test_parser"
    ~path:"src/lib_version/test"
    ~opam:"tezos-version"
    ~js_compatible:true
    ~modes:[Native; JS]
    ~deps:[octez_version |> open_; octez_version_parser; alcotest]

let octez_p2p_services =
  public_lib
    "tezos-p2p-services"
    ~path:"src/lib_p2p_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-p2p`"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"]
    ~linkall:true
    ~js_compatible:true

let octez_workers =
  public_lib
    "tezos-workers"
    ~path:"src/lib_workers"
    ~synopsis:"Tezos: worker library"
    ~documentation:[Dune.[S "package"; S "tezos-workers"]]
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_stdlib_unix |> open_;
        ringo;
      ]

let _octez_workers_tests =
  tests
    ["test_workers_unit"]
    ~path:"src/lib_workers/test"
    ~opam:"tezos-workers"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives"
        |> open_ ~m:"Worker_types";
        octez_workers |> open_;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        alcotest_lwt;
      ]

let octez_context_sigs =
  public_lib
    "tezos-context.sigs"
    ~path:"src/lib_context/sigs"
    ~opam:"tezos-context"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_stdlib |> open_]
    ~js_compatible:true

let octez_merkle_proof_encoding =
  public_lib
    "tezos-context.merkle_proof_encoding"
    ~path:"src/lib_context/merkle_proof_encoding"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        octez_context_sigs;
      ]
    ~js_compatible:true

let octez_shell_services =
  public_lib
    "tezos-shell-services"
    ~path:"src/lib_shell_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-shell`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_p2p_services |> open_;
        octez_version |> open_;
        octez_context_sigs;
      ]
    ~linkall:true
    ~js_compatible:true

let octez_test_helpers_extra =
  public_lib
    "tezos-test-helpers-extra"
    ~path:"src/lib_test"
    ~internal_name:"lib_test_extra"
    ~synopsis:"Test helpers dependent on tezos-base"
    ~deps:[octez_base; octez_crypto; octez_test_helpers]
    ~ocaml:V.(at_least "4.08")
    ~dune:
      Dune.
        [
          (* This rule is necessary for `make lint-tests-pkg`, without it dune
             complains that the alias is empty. *)
          alias_rule "runtest_js_base" ~action:(S "progn");
        ]
    ~modules:["assert_lib"]

let _octez_shell_services_tests =
  test
    "test"
    ~path:"src/lib_shell_services/test"
    ~opam:"tezos-shell-services"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_shell_services |> open_;
        alcotest;
      ]
    ~modes:[Native; JS]
    ~js_compatible:true

let octez_shell_services_test_helpers =
  public_lib
    "tezos-shell-services-test-helpers"
    ~path:"src/lib_shell_services/test_helpers"
    ~synopsis:"Tezos: Tezos shell_services test helpers"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_shell_services;
        octez_test_helpers;
        qcheck_core;
      ]
    ~bisect_ppx:false
    ~linkall:true

let _octez_shell_service_test_helpers_tests =
  test
    "test_block_services"
    ~path:"src/lib_shell_services/test_helpers/test"
    ~opam:"tezos-shell-services-test-helpers"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_test_helpers;
        octez_shell_services;
        octez_shell_services_test_helpers;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let _octez_tooling =
  public_lib
    "tezos-tooling"
    ~path:"src/tooling"
    ~synopsis:"Tezos: tooling for the project"
    ~modules:[]
    ~opam_only_deps:
      [
        bisect_ppx;
        (* These next are only used in the CI, we add this dependency so that
           it is added to tezos/opam-repository. *)
        ocamlformat;
        ometrics;
      ]

let octez_tooling_opam_file_format =
  private_lib
    "opam_file_format"
    ~opam:"tezos-tooling"
    ~path:"src/tooling/opam-lint/opam-file-format-src"
    ~deps:[unix]
    ~dune:Dune.[ocamllex "opamLexer"; ocamlyacc "opamBaseParser"]

let _octez_tooling_opam_lint =
  test
    "opam_lint"
    ~alias:""
    ~path:"src/tooling/opam-lint"
    ~opam:"tezos-tooling"
    ~deps:[octez_tooling_opam_file_format; unix]

let octez_p2p =
  public_lib
    "tezos-p2p"
    ~path:"src/lib_p2p"
    ~synopsis:"Tezos: library for a pool of P2P connections"
    ~deps:
      [
        lwt_watcher;
        lwt_canceler;
        ringo;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_p2p_services |> open_;
        octez_version;
        prometheus;
      ]

let _octez_p2p_tests =
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
      "test_p2p_maintenance";
    ]
    ~path:"src/lib_p2p/test"
    ~opam:"tezos-p2p"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_p2p |> open_;
        octez_test_helpers;
        octez_base_test_helpers |> open_;
        octez_event_logging_test_helpers |> open_;
        octez_p2p_services |> open_;
        alcotest_lwt;
        astring;
      ]
    ~linkall:true
    ~alias:""
    ~dune:
      Dune.(
        (* At the termination of the tests, or if an unexpected
           error occurs, detached processes are terminated through a
           SIGKILL.
           See https://github.com/aantron/bisect_ppx/blob/master/doc/advanced.md#SIGTERM
           See https://gitlab.com/tezos/tezos/-/issues/1946 *)
        let run_exe prog args =
          setenv "BISECT_SIGTERM" "yes" @@ run_exe prog args
        in
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
                   "1048576" (* 1 << 20 = 1MB *);
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
            "runtest_p2p_maintenance"
            ~action:(run_exe "test_p2p_maintenance" []);
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
                "runtest_p2p_maintenance";
              ];
        ])

let octez_scoru_wasm =
  public_lib
    "tezos-scoru-wasm"
    ~path:"src/lib_scoru_wasm"
    ~synopsis:
      "Protocol environment dependency providing WASM functionality for SCORU"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        lazy_containers;
        octez_webassembly_interpreter;
        octez_context_sigs;
        octez_lwt_result_stdlib;
        data_encoding;
      ]

let octez_context_encoding =
  public_lib
    "tezos-context.encoding"
    ~path:"src/lib_context/encoding"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        irmin;
        irmin_pack;
      ]

let octez_context_helpers =
  public_lib
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

let octez_context_dump =
  public_lib
    "tezos-context.dump"
    ~path:"src/lib_context/dump"
    ~deps:
      [octez_base |> open_ ~m:"TzPervasives"; octez_stdlib_unix |> open_; fmt]

let octez_context_memory =
  public_lib
    "tezos-context.memory"
    ~path:"src/lib_context/memory"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib |> open_;
        irmin_pack;
        irmin_pack_mem;
        octez_context_sigs;
        octez_context_encoding;
        octez_context_helpers;
      ]

let octez_context_disk =
  public_lib
    "tezos-context.disk"
    ~path:"src/lib_context/disk"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        bigstringaf;
        fmt;
        logs_fmt;
        digestif_c;
        irmin;
        irmin_pack;
        irmin_pack_unix;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_context_sigs;
        octez_context_helpers;
        octez_context_encoding;
        octez_context_memory;
        octez_context_dump;
      ]

let _tree_encoding_tests =
  test
    "test_tree_encoding"
    ~path:"src/lib_tree_encoding/test"
    ~opam:"tree-encoding-test"
    ~synopsis:"Tests for the tree encoding library"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_context_disk;
        octez_base_test_helpers |> open_;
        octez_test_helpers;
        octez_webassembly_interpreter;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let _octez_scoru_wasm_tests =
  test
    "test_scoru_wasm"
    ~path:"src/lib_scoru_wasm/test"
    ~opam:"tezos-scoru-wasm-test"
    ~synopsis:"Tests for the scoru-wasm functionality"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_context_disk;
        octez_base_test_helpers |> open_;
        octez_test_helpers;
        octez_scoru_wasm;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let octez_context =
  public_lib
    "tezos-context"
    ~path:"src/lib_context"
    ~synopsis:"Tezos: on-disk context abstraction for `tezos-node`"
    ~deps:[octez_context_disk; octez_context_memory]

let _octez_context_tests =
  test
    "test"
    ~path:"src/lib_context/test"
    ~opam:"tezos-context"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_context_sigs;
        octez_context_disk;
        octez_context_memory;
        octez_stdlib_unix |> open_;
        octez_test_helpers;
        octez_test_helpers_extra;
        alcotest_lwt;
      ]
    ~modules:["test_context"; "test"]

let _octez_context_memory_tests =
  test
    "test"
    ~path:"src/lib_context/memory/test"
    ~opam:"tezos-context"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_context_disk;
        octez_context_memory;
        octez_stdlib_unix |> open_;
        alcotest_lwt;
      ]

(* This binding assumes that librustzcash.a is installed in the system default
   directories or in: $OPAM_SWITCH_PREFIX/lib

   Tests are disabled in the .opam because the tests require zcash parameter files. *)
let octez_sapling =
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
        octez_stdlib |> open_;
        octez_crypto |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_rust_lib;
        octez_lwt_result_stdlib;
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
    ~opam_with_test:Never
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

let _octez_sapling_tests =
  tests
    ["test_rustzcash"; "test_keys"; "test_merkle"; "test_roots"; "test_sapling"]
    ~path:"src/lib_sapling/test"
    ~opam:"tezos-sapling"
    ~dep_files:["vectors.csv"; "vectors-zip32.csv"]
    ~deps:
      [
        octez_sapling |> open_;
        octez_crypto |> open_;
        str;
        octez_base;
        octez_base_unix;
        octez_stdlib |> open_;
        octez_stdlib_unix;
        data_encoding |> open_;
        octez_base_test_helpers |> open_;
        alcotest_lwt;
      ]
    ~all_modules_except:["test_js"]
    ~opam_with_test:Never

let _octez_sapling_js_tests =
  test
    "test_js"
    ~path:"src/lib_sapling/test"
    ~opam:"tezos-sapling"
    ~deps:[octez_sapling; octez_hacl]
    ~modules:["test_js"]
    ~linkall:true
    ~modes:[JS]
    ~js_compatible:true
    ~opam_with_test:Never

let _octez_sapling_ctypes_gen =
  private_exes
    ["rustzcash_ctypes_gen"; "gen_runtime_js"]
    ~path:"src/lib_sapling/bindings"
    ~opam:"tezos-sapling"
    ~bisect_ppx:false
    ~deps:[ctypes_stubs; ctypes]
    ~modules:
      ["rustzcash_ctypes_gen"; "rustzcash_ctypes_bindings"; "gen_runtime_js"]
    ~opam_with_test:Never

let tezos_protocol_environment_sigs_internals =
  public_lib
    "tezos-protocol-environment.sigs-internals"
    ~path:"src/lib_protocol_environment/sigs-internals"

let tezos_protocol_environment_sigs =
  public_lib
    "tezos-protocol-environment.sigs"
    ~path:"src/lib_protocol_environment/sigs"
    ~ocaml:V.(at_least "4.12")
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
             ~promote:true
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
       let latest_environment_number = 7 in
       List.init (latest_environment_number + 1) gen |> Dune.of_list)

let octez_protocol_environment_structs =
  public_lib
    "tezos-protocol-environment.structs"
    ~path:"src/lib_protocol_environment/structs"
    ~deps:
      [
        octez_stdlib;
        octez_crypto;
        octez_lwt_result_stdlib;
        octez_scoru_wasm;
        data_encoding;
        bls12_381;
        plonk;
      ]

let octez_protocol_environment =
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
        plonk;
        octez_crypto_dal;
        vdf;
        ringo;
        ringo_lwt;
        octez_base |> open_ ~m:"TzPervasives";
        octez_sapling;
        tezos_protocol_environment_sigs;
        octez_protocol_environment_structs;
        octez_micheline |> open_;
        octez_context_memory;
        octez_scoru_wasm;
        octez_event_logging;
      ]

let octez_shell_context =
  public_lib
    "tezos-shell-context"
    ~path:"src/lib_protocol_environment/shell_context"
    ~synopsis:
      "Tezos: economic-protocols environment implementation for `tezos-node`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_protocol_environment;
        octez_context;
      ]

let _octez_protocol_environment_tests =
  tests
    ["test"; "test_mem_context_array_theory"; "test_cache"]
    ~path:"src/lib_protocol_environment/test"
    ~opam:"tezos-protocol-environment"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_protocol_environment |> open_;
        alcotest_lwt;
        octez_test_helpers;
        qcheck_alcotest;
        lwt_unix;
      ]

let octez_context_ops =
  public_lib
    "tezos-context-ops"
    ~path:"src/lib_protocol_environment/context_ops"
    ~synopsis:"Tezos: backend-agnostic operations on contexts"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_error_monad |> open_;
        octez_protocol_environment;
        octez_context |> open_;
        octez_shell_context |> open_;
      ]

let _octez_protocol_shell_context_tests =
  tests
    ["test_proxy_context"]
    ~path:"src/lib_protocol_environment/test_shell_context"
    ~opam:"tezos-shell-context-test"
    ~synopsis:"Testing the Shell Context"
    ~deps:
      [
        octez_shell_context;
        alcotest_lwt;
        octez_test_helpers |> open_;
        octez_base |> open_ ~m:"TzPervasives";
        octez_protocol_environment |> open_;
      ]

let octez_protocol_compiler_registerer =
  public_lib
    "tezos-protocol-compiler.registerer"
    ~path:"src/lib_protocol_compiler/registerer"
    ~internal_name:"tezos_protocol_registerer"
    ~deps:
      [octez_base |> open_ ~m:"TzPervasives"; tezos_protocol_environment_sigs]
    ~flags:(Flags.standard ~opaque:true ())

let _octez_protocol_compiler_cmis_of_cma =
  private_exe
    "cmis_of_cma"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"tezos-protocol-compiler"
    ~deps:[compiler_libs_common]
    ~modules:["cmis_of_cma"]

let octez_protocol_compiler_lib =
  public_lib
    "tezos-protocol-compiler"
    ~path:"src/lib_protocol_compiler"
    ~synopsis:"Tezos: economic-protocol compiler"
    ~ocaml:
      V.(
        (* Should be in sync with scripts/version.sh *)
        at_least "4.14.0" && less_than "4.15")
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_version;
        tezos_protocol_environment_sigs;
        octez_stdlib_unix |> open_;
        compiler_libs_common;
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
            ~deps:[Dune.(H [[S "package"; S "tezos-protocol-environment"]])]
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
                        "%{lib:tezos-protocol-environment.sigs:tezos_protocol_environment_sigs.cmxa}";
                    ];
                ];
              ];
          targets_rule
            ["embedded_cmis_env.ml"]
            ~deps:[Dune.(H [[S "package"; S "tezos-protocol-environment"]])]
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
    "tezos-protocol-compiler.native"
    ~path:"src/lib_protocol_compiler"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_protocol_compiler_lib |> open_;
        compiler_libs_optcomp;
      ]
    ~modules:["Native"]
    ~dune:
      Dune.
        [
          install
            [V [S "final_protocol_versions"]]
            ~package:"tezos-protocol-compiler"
            ~section:"libexec";
        ]

let octez_protocol_updater =
  public_lib
    "tezos-protocol-updater"
    ~path:"src/lib_protocol_updater"
    ~synopsis:"Tezos: economic-protocol dynamic loading for `tezos-node`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_micheline |> open_;
        octez_shell_services |> open_;
        octez_protocol_environment;
        octez_shell_context;
        octez_protocol_compiler_registerer;
        octez_protocol_compiler_native;
        octez_context |> open_;
        lwt_exit;
        dynlink;
      ]

let octez_validation =
  public_lib
    "tezos-validation"
    ~path:"src/lib_validation"
    ~synopsis:"Tezos: library for blocks validation"
    ~time_measurement_ppx:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_context |> open_;
        octez_context_ops |> open_;
        octez_shell_services |> open_;
        octez_protocol_updater |> open_;
        octez_stdlib_unix |> open_;
      ]

let octez_store_shared =
  public_lib
    "tezos-store.shared"
    ~path:"src/lib_store/shared"
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_shell_services |> open_;
        ringo_lwt;
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
  public_lib
    "tezos-store.unix"
    ~path:"src/lib_store/unix"
    ~deps:
      [
        octez_shell_services |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_version;
        index;
        irmin_pack;
        octez_store_shared |> open_;
        octez_protocol_environment |> open_;
        octez_context |> open_;
        octez_context_ops |> open_;
        octez_shell_context;
        octez_validation |> open_;
        octez_protocol_updater |> open_;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        lwt_watcher;
        ringo_lwt;
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
        "stored_data";
        "store_metrics";
        "store";
      ]

let octez_store_unix_reconstruction =
  public_lib
    "tezos-store.unix-reconstruction"
    ~path:"src/lib_store/unix"
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_shell_services |> open_;
        octez_protocol_updater |> open_;
        octez_validation |> open_;
        octez_context_ops |> open_;
        octez_store_shared |> open_;
        octez_store_unix |> open_;
      ]
    ~modules:["reconstruction"; "reconstruction_events"]

let octez_store_unix_snapshots =
  public_lib
    "tezos-store.unix-snapshots"
    ~path:"src/lib_store/unix"
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_shell_services |> open_;
        octez_context |> open_;
        octez_validation |> open_;
        octez_store_shared |> open_;
        octez_store_unix |> open_;
      ]
    ~modules:["snapshots"; "snapshots_events"]

let octez_store =
  public_lib
    "tezos-store"
    ~path:"src/lib_store"
    ~synopsis:"Tezos: store for `tezos-node`"
    ~description:
      {|This library provides abstraction for storing and iterating over blocks.
tezos-store is a virtual library that provides two implementations:
- tezos-store.real is the default implementation, used in production
- tezos-store.mocked is used for testing purposes.|}
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        lwt_watcher;
        octez_shell_services |> open_;
        octez_validation |> open_;
        octez_context_ops |> open_;
        octez_store_shared |> open_;
      ]
    ~virtual_modules:["store"]
    ~default_implementation:"tezos-store.real"

let _octez_store_real =
  public_lib
    "tezos-store.real"
    ~path:"src/lib_store/real"
    ~deps:[octez_store_unix |> open_]
    ~implements:octez_store

let _octez_store_mocked =
  public_lib
    "tezos-store.mocked"
    ~path:"src/lib_store/mocked"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_shell_services |> open_;
        octez_context_memory |> open_;
        octez_context_ops |> open_;
        octez_validation |> open_;
        octez_protocol_environment;
        octez_store_shared |> open_;
      ]
    ~private_modules:["block_store"; "protocol_store"; "stored_data"]
    ~implements:octez_store

let octez_requester =
  public_lib
    "tezos-requester"
    ~path:"src/lib_requester"
    ~synopsis:"Tezos: generic resource fetching service"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        lwt_watcher;
      ]

let _octez_requester_tests =
  tests
    ["test_requester"; "test_fuzzing_requester"]
    ~path:"src/lib_requester/test"
    ~opam:"tezos-requester"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_test_helpers;
        octez_base_test_helpers |> open_;
        octez_stdlib |> open_;
        octez_stdlib_unix;
        octez_requester |> open_;
        alcotest_lwt;
        qcheck_alcotest;
      ]

let octez_shell =
  public_lib
    "tezos-shell"
    ~path:"src/lib_shell"
    ~synopsis:
      "Tezos: core of `tezos-node` (gossip, validation scheduling, mempool, \
       ...)"
    ~documentation:[Dune.[S "package"; S "tezos-shell"]]
    ~inline_tests:ppx_expect
    ~deps:
      [
        lwt_watcher;
        lwt_canceler;
        prometheus;
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_base_unix |> open_;
        octez_context |> open_;
        octez_store |> open_;
        octez_store_shared |> open_;
        octez_protocol_environment |> open_;
        octez_context_ops |> open_;
        octez_shell_context |> open_;
        octez_p2p |> open_;
        octez_stdlib_unix |> open_;
        octez_shell_services |> open_;
        octez_p2p_services |> open_;
        octez_protocol_updater |> open_;
        octez_requester |> open_;
        octez_workers |> open_;
        octez_validation |> open_;
        octez_version |> open_;
        lwt_exit;
      ]

let octez_rpc_http =
  public_lib
    "tezos-rpc-http"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: library of auto-documented RPCs (http server and client)"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; resto_cohttp; uri]
    ~modules:["RPC_client_errors"; "media_type"]

let octez_rpc_http_client =
  public_lib
    "tezos-rpc-http-client"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: library of auto-documented RPCs (http client)"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        resto_cohttp_client;
        octez_rpc_http |> open_;
      ]
    ~modules:["RPC_client"]

let octez_rpc_http_client_unix =
  public_lib
    "tezos-rpc-http-client-unix"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: unix implementation of the RPC client"
    ~deps:
      [
        octez_stdlib_unix;
        octez_base |> open_ ~m:"TzPervasives";
        cohttp_lwt_unix;
        resto_cohttp_client;
        octez_rpc_http_client |> open_;
      ]
    ~modules:["RPC_client_unix"]

let octez_rpc_http_server =
  public_lib
    "tezos-rpc-http-server"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: library of auto-documented RPCs (http server)"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        resto_cohttp_server;
        resto_acl;
        octez_rpc |> open_;
        octez_rpc_http |> open_;
      ]
    ~modules:["RPC_server"; "RPC_logging"; "RPC_middleware"]
    ~private_modules:["RPC_logging"]

let _octez_rpc_http_server_tests =
  test
    "test_rpc_http"
    ~path:"src/lib_rpc_http/test"
    ~opam:"tezos-rpc-http-server"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib |> open_;
        octez_stdlib_unix;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        octez_rpc_http_server |> open_;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let _octez_context_merkle_proof_tests =
  test
    "test_merkle_proof"
    ~path:"src/lib_context/test"
    ~opam:"tezos-context"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_context_disk |> open_;
        octez_context_encoding;
        octez_stdlib_unix |> open_;
        qcheck_alcotest;
        octez_test_helpers;
      ]
    ~modules:["test_merkle_proof"]

let octez_validator_lib =
  public_lib
    "tezos-validator"
    ~path:"src/bin_validation"
    ~synopsis:
      "Tezos: `tezos-validator` binary for external validation of blocks"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_context |> open_;
        octez_context_ops |> open_;
        octez_stdlib_unix |> open_;
        octez_protocol_environment;
        octez_shell |> open_;
        octez_shell_services |> open_;
        octez_validation |> open_;
        octez_protocol_updater |> open_;
        octez_shell_context |> open_;
      ]

let octez_client_base =
  public_lib
    "tezos-client-base"
    ~path:"src/lib_client_base"
    ~synopsis:"Tezos: common helpers for `tezos-client`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc |> open_;
        octez_shell_services |> open_;
        octez_sapling;
        uri;
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

let _octez_client_base_tests =
  tests
    ["bip39_tests"; "pbkdf_tests"]
    ~path:"src/lib_client_base/test"
    ~opam:"tezos-client-base"
    ~deps:[octez_base; octez_client_base |> open_; alcotest]
    ~js_compatible:true
    ~modes:[Native; JS]

let _bip39_generator =
  private_exe
    "bip39_generator"
    ~path:"src/lib_client_base/gen"
    ~opam:"tezos-client-base"
    ~bisect_ppx:false

let octez_signer_services =
  public_lib
    "tezos-signer-services"
    ~path:"src/lib_signer_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-signer`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc |> open_;
        octez_client_base |> open_;
      ]
    ~linkall:true
    ~js_compatible:true

let octez_signer_backends =
  public_lib
    "tezos-signer-backends"
    ~path:"src/lib_signer_backends"
    ~synopsis:"Tezos: remote-signature backends for `tezos-client`"
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
  test
    "test_encrypted"
    ~path:"src/lib_signer_backends/test"
    ~opam:"tezos-signer-backends"
    ~deps:
      [
        octez_base;
        octez_base_unix;
        octez_stdlib |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_crypto |> open_;
        octez_client_base |> open_;
        octez_signer_backends |> open_;
        alcotest_lwt;
        uri;
      ]

let octez_signer_backends_unix =
  public_lib
    "tezos-signer-backends.unix"
    ~path:"src/lib_signer_backends/unix"
    ~deps:
      [
        ocplib_endian_bigstring;
        fmt;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
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
  test
    "test_crouching"
    ~path:"src/lib_signer_backends/unix/test"
    ~opam:"tezos-signer-backends"
    ~deps:
      [
        octez_error_monad |> open_;
        octez_stdlib |> open_;
        octez_crypto |> open_;
        octez_client_base |> open_;
        octez_signer_backends_unix |> open_;
        alcotest_lwt;
      ]

let octez_client_commands =
  public_lib
    "tezos-client-commands"
    ~path:"src/lib_client_commands"
    ~synopsis:"Tezos: protocol agnostic commands for `tezos-client`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc |> open_;
        octez_clic_unix |> open_;
        octez_client_base |> open_;
        octez_shell_services |> open_;
        octez_p2p_services |> open_;
        octez_stdlib_unix;
        octez_signer_backends;
        data_encoding |> open_;
        uri;
      ]
    ~linkall:true

let octez_mockup_registration =
  public_lib
    "tezos-mockup-registration"
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
  public_lib
    "tezos-mockup-proxy"
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
  public_lib
    "tezos-mockup"
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
  public_lib
    "tezos-mockup-commands"
    ~path:"src/lib_mockup"
    ~synopsis:"Tezos: library of auto-documented RPCs (commands)"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_client_commands;
        octez_client_base;
        octez_mockup |> open_;
        octez_mockup_registration |> open_;
      ]
    ~modules:["mockup_wallet"; "mockup_commands"]

let _octez_mockup_tests =
  tests
    ["test_mockup_args"; "test_fuzzing_mockup_args"; "test_persistence"]
    ~path:"src/lib_mockup/test"
    ~opam:"tezos-mockup"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_test_helpers |> open_;
        octez_mockup;
        octez_mockup_registration;
        octez_client_base;
        qcheck_alcotest;
        alcotest_lwt;
      ]

let octez_proxy =
  public_lib
    "tezos-proxy"
    ~path:"src/lib_proxy"
    ~synopsis:"Tezos: proxy"
    ~deps:
      [
        ringo_lwt;
        octez_base |> open_ ~m:"TzPervasives";
        octez_clic;
        octez_client_base;
        octez_protocol_environment;
        octez_rpc;
        octez_shell_services;
        octez_context_memory;
        uri;
      ]

let octez_proxy_rpc =
  public_lib
    "tezos-proxy.rpc"
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

let _octez_proxy_tests =
  tests
    [
      "test_proxy";
      "test_fuzzing_proxy_getter";
      "test_light";
      "test_fuzzing_light";
    ]
    ~path:"src/lib_proxy/test"
    ~opam:"tezos-proxy"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix;
        octez_proxy;
        octez_base_test_helpers |> open_;
        octez_test_helpers;
        octez_shell_services_test_helpers;
        qcheck_alcotest;
        alcotest_lwt;
        uri;
      ]

let octez_proxy_server_config =
  public_lib
    "tezos-proxy-server-config"
    ~path:"src/lib_proxy_server_config"
    ~synopsis:"Tezos: proxy server configuration"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_stdlib_unix; uri]

let _octez_proxy_server_config_tests =
  test
    "test_proxy_server_config"
    ~path:"src/lib_proxy_server_config/test"
    ~opam:"tezos-proxy-server-config"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_proxy_server_config;
        octez_test_helpers;
        qcheck_alcotest;
        alcotest_lwt;
        uri;
      ]

let octez_client_base_unix =
  public_lib
    "tezos-client-base-unix"
    ~path:"src/lib_client_base_unix"
    ~synopsis:
      "Tezos: common helpers for `tezos-client` (unix-specific fragment)"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
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
        lwt_exit;
        uri;
      ]
    ~linkall:true

let _octez_client_base_unix_tests =
  test
    "test_mockup_wallet"
    ~path:"src/lib_client_base_unix/test"
    ~opam:"tezos-client-base-unix"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_mockup_commands;
        octez_client_base_unix;
        octez_base_test_helpers |> open_;
        alcotest;
        alcotest_lwt;
      ]

let octez_benchmark =
  public_lib
    "tezos-benchmark"
    ~path:"src/lib_benchmark"
    ~synopsis:
      "Tezos: library for writing benchmarks and performing simple parameter \
       inference"
    ~foreign_stubs:
      {language = C; flags = [":standard"]; names = ["snoop_stubs"]}
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_micheline;
        octez_clic;
        data_encoding;
        prbnmcn_cgrph;
        prbnmcn_dagger;
        prbnmcn_dagger_stats;
        prbnmcn_stats;
        pringo;
        pyml;
        ocaml_migrate_parsetree;
        opam_only "hashcons" V.True;
      ]

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
  test
    "main_ci"
    ~path:"src/lib_benchmark/test"
    ~opam:"tezos-benchmark-tests"
    ~synopsis:"Tezos: tests for lib-benchmarks"
    ~deps:
      [
        alcotest_lwt;
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
        zarith_stubs_js;
        octez_stdlib |> open_;
        octez_error_monad |> open_;
        octez_micheline |> open_;
      ]

let octez_shell_benchmarks =
  public_lib
    "tezos-shell-benchmarks"
    ~path:"src/lib_shell_benchmarks"
    ~synopsis:"Tezos: shell benchmarks"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_error_monad |> open_;
        octez_benchmark |> open_;
        octez_crypto |> open_;
        octez_context;
        octez_shell_context;
        octez_micheline;
      ]
    ~linkall:true

let tezt_lib =
  public_lib
    "tezt"
    ~path:"tezt/lib"
    ~synopsis:
      "Test framework for unit tests, integration tests, and regression tests"
    ~opam_homepage:"https://gitlab.com/tezos/tezos/-/tree/master/tezt/lib#tezt"
    ~opam_doc:"https://tezos.gitlab.io/api/odoc/_html/tezt/Tezt/index.html"
    ~ocaml:V.(at_least "4.12")
    ~bisect_ppx:false
    ~deps:[re; lwt_unix; ezjsonm]

let tezt ~opam ~path ?(deps = []) ?dep_globs l =
  tezt_without_tezt_lib_dependency
    ~opam
    ~path
    ~deps:((tezt_lib |> open_ |> open_ ~m:"Base") :: deps)
    ?dep_globs
    l

let tezt_performance_regression =
  public_lib
    "tezt-performance-regression"
    ~path:"tezt/lib_performance_regression"
    ~synopsis:"Performance regression test framework based on Tezt"
    ~bisect_ppx:false
    ~deps:[tezt_lib |> open_ |> open_ ~m:"Base"; uri; cohttp_lwt_unix]

let tezt_tezos =
  public_lib
    "tezt-tezos"
    ~path:"tezt/lib_tezos"
    ~synopsis:"Tezos test framework based on Tezt"
    ~bisect_ppx:false
    ~deps:
      [
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_performance_regression |> open_;
        uri;
        hex;
        octez_crypto_dal;
        octez_base;
        octez_base_unix;
        cohttp_lwt_unix;
      ]
    ~cram:true

let _tezt_self_tests =
  public_exe
    "tezt-self-tests"
    ~internal_name:"main"
    ~path:"tezt/self_tests"
    ~synopsis:"Tests for the Tezos test framework based on Tezt"
    ~bisect_ppx:false
    ~static:false
    ~deps:[tezt_lib |> open_ |> open_ ~m:"Base"; tezt_tezos |> open_]
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

let octez_openapi =
  public_lib
    "tezos-openapi"
    ~path:"src/lib_openapi"
    ~synopsis:
      "Tezos: a library for querying RPCs and converting into the OpenAPI \
       format"
    ~deps:[ezjsonm; json_data_encoding; tezt_lib]

let _octez_protocol_compiler_bin =
  public_exe
    "tezos-protocol-compiler"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"tezos-protocol-compiler"
    ~internal_name:"main_native"
    ~modes:[Native]
    ~deps:[octez_protocol_compiler_native]
    ~linkall:true
    ~modules:["Main_native"]

let octez_protocol_compiler_tezos_protocol_packer =
  public_exe
    "tezos-protocol-compiler.tezos-protocol-packer"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"tezos-protocol-compiler"
    ~internal_name:"main_packer"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_protocol_compiler_lib |> open_;
      ]
    ~modules:["Main_packer"]

let _octez_embedded_protocol_packer =
  public_exe
    "tezos-embedded-protocol-packer"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"tezos-protocol-compiler"
    ~internal_name:"main_embedded_packer"
    ~modes:[Native]
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
      ]
    ~linkall:true
    ~modules:["Main_embedded_packer"]

let octez_dal_node_services =
  private_lib
    "tezos_dal_node_services"
    ~path:"src/lib_dal_node_services"
    ~opam:"tezos-dal-node-services"
    ~synopsis:"Tezos: `tezos-dal-node` RPC services"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives" |> open_;
        octez_rpc |> open_;
        octez_crypto_dal;
      ]
    ~linkall:true

let octez_dal_node_lib =
  private_lib
    "tezos_dal_node_lib"
    ~path:"src/lib_dal_node"
    ~opam:"tezos-dal-node-lib"
    ~synopsis:"Tezos: `tezos-dal-node` library"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_client_base |> open_;
        octez_protocol_updater |> open_;
      ]

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

  val name_dash : t -> string

  val name_underscore : t -> string

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

  val dal : t -> target option

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

  module Name : sig
    type t

    (** [alpha] is a protocol name with protocol number [Alpha] *)
    val alpha : t

    (** [v name num] constuct a protocol name with protocol number [V num] *)
    val v : string -> int -> t

    (** [other name] constuct a protocol name with protocol number [Other] *)
    val other : string -> t

    val number : t -> number

    val name_underscore : t -> string

    val name_dash : t -> string

    val base_path : t -> string
  end = struct
    type t = {name_underscore : string; name_dash : string; number : number}

    let make name number =
      if
        not
          (String.for_all
             (function
               | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true | _ -> false)
             name)
      then
        invalid_arg
          (sf
             "Protocol.Name.make: %s is not a valid protocol name: should be \
              of the form [A-Za-z0-9-]+"
             name) ;
      let make_full_name sep name =
        match number with
        | Alpha | Other -> name
        | V number -> sf "%03d%c%s" number sep name
      in
      let name_dash = make_full_name '-' name in
      let name_underscore =
        make_full_name '_' (String.map (function '-' -> '_' | c -> c) name)
      in
      {number; name_dash; name_underscore}

    let v name number = make name (V number)

    let alpha = make "alpha" Alpha

    let other name = make name Other

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
    test_helpers : target option;
    parameters : target option;
    benchmarks_proto : target option;
    baking : target option;
  }

  let make ?client ?client_commands ?client_commands_registration
      ?baking_commands_registration ?plugin ?plugin_registerer ?dal
      ?test_helpers ?parameters ?benchmarks_proto ?baking ~status ~name ~main
      ~embedded () =
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

  let number p = Name.number p.name

  let status p = p.status

  let name_dash p = Name.name_dash p.name

  let name_underscore p = Name.name_underscore p.name

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

  let dal p = p.dal

  let parameters_exn p = mandatory "parameters" p p.parameters

  let benchmarks_proto_exn p = mandatory "benchmarks_proto" p p.benchmarks_proto

  let baking_exn p = mandatory "baking" p p.baking

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
      | Other ->
          invalid_arg "cannot use N.compare_asymmetric on Other protocols"

    let ( <= ) a b = compare_asymmetric a b <= 0

    let ( >= ) a b = compare_asymmetric a b >= 0

    let ( <> ) a b = compare_asymmetric a b <> 0

    let ( == ) a b = compare_asymmetric a b == 0
  end

  let only_if condition make = if condition then Some (make ()) else None

  let conditional_list =
    List.filter_map (fun (x, b) -> if b then Some x else None)

  module Lib_protocol = struct
    type t = {main : target; embedded : target}

    let make_tests ?test_helpers ?parameters ?plugin ?client ?benchmark
        ?benchmark_type_inference ?sc_rollup ~main ~name () =
      let name_dash = Name.name_dash name in
      let number = Name.number name in
      let path = Name.base_path name in
      let _integration_consensus =
        test
          "main"
          ~path:(path // "lib_protocol/test/integration/consensus")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~deps:
            [
              alcotest_lwt;
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              main |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
              parameters |> if_some |> open_;
              plugin |> if_some |> open_;
            ]
      in
      let _integration_gas =
        test
          "main"
          ~path:(path // "lib_protocol/test/integration/gas")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~deps:
            [
              alcotest_lwt;
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              main |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
            ]
      in
      let _integration_michelson =
        test
          "main"
          ~path:(path // "lib_protocol/test/integration/michelson")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~dep_globs:
            (conditional_list
               [
                 ("contracts/*", true);
                 ("patched_contracts/*", N.(number >= 013));
               ])
          ~deps:
            [
              alcotest_lwt;
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              main |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
              octez_client_base |> if_ N.(number <= 012);
              client |> if_some |> open_;
              octez_benchmark;
              octez_micheline |> open_;
              benchmark |> if_some |> open_;
              benchmark_type_inference |> if_some |> open_;
              parameters |> if_some |> if_ N.(number >= 013);
            ]
      in
      let _integration_operations =
        test
          "main"
          ~path:(path // "lib_protocol/test/integration/operations")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~dep_globs:(conditional_list [("contracts/*", N.(number >= 013))])
          ~deps:
            [
              alcotest_lwt;
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              main |> open_;
              client |> if_some |> if_ N.(number >= 012) |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
            ]
      in
      let _integration_validate =
        only_if N.(number >= 014) @@ fun () ->
        tests
          ["main"; "test_1m_restriction"]
          ~path:(path // "lib_protocol/test/integration/validate")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~deps:
            [
              alcotest_lwt;
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              main |> open_;
              qcheck_alcotest;
              client |> if_some |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
            ]
      in
      let _integration =
        test
          "main"
          ~path:(path // "lib_protocol/test/integration")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~deps:
            [
              (if N.(number >= 015) then Some tezt_lib else None) |> if_some;
              octez_context;
              alcotest_lwt;
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              client |> if_some |> open_;
              octez_client_base |> if_ N.(number <= 012);
              main |> open_;
              parameters |> if_some |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
            ]
          ~dep_globs:(if N.(number >= 015) then ["wasm_kernel/*.wasm"] else [])
      in
      let _pbt =
        tests
          (conditional_list
             [
               ("liquidity_baking_pbt", true);
               ("saturation_fuzzing", true);
               ("test_merkle_list", N.(number >= 013));
               ("test_gas_properties", true);
               ("test_sampler", N.(number >= 012));
               ("test_script_comparison", true);
               ("test_tez_repr", true);
               ("test_tx_rollup_l2_encoding", N.(number >= 013));
               ("test_tx_rollup_l2_withdraw_storage", N.(number <= 010));
               ("test_bitset", N.(number >= 013));
               ("test_sc_rollup_tick_repr", N.(number >= 013));
               ("test_sc_rollup_encoding", N.(number >= 015));
               ("refutation_game_pbt", N.(number == 013));
               ("test_refutation_game", N.(number >= 014));
               ("test_carbonated_map", N.(number >= 013));
               ("test_zk_rollup_encoding", N.(number >= 015));
             ])
          ~synopsis:"Tezos/Protocol: tests for economic-protocol definition"
          ~path:(path // "lib_protocol/test/pbt")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~deps:
            [
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              octez_micheline |> open_;
              client |> if_some |> open_;
              main |> open_;
              octez_merkle_proof_encoding;
              octez_test_helpers;
              test_helpers |> if_some |> open_;
              alcotest;
              qcheck_alcotest;
              octez_client_base |> if_ N.(number <= 012);
              octez_benchmark;
              benchmark |> if_some |> open_;
              benchmark_type_inference |> if_some |> open_;
              sc_rollup |> if_some |> if_ N.(number >= 015) |> open_;
            ]
      in
      let _unit =
        test
          "main"
          ~path:(path // "lib_protocol/test/unit")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~alias:""
          ~deps:
            [
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              octez_base_test_helpers |> open_;
              octez_micheline |> open_;
              client |> if_some |> open_;
              octez_client_base;
              parameters |> if_some;
              octez_protocol_environment;
              octez_stdlib_unix;
              main |> open_;
              test_helpers |> if_some |> open_;
              alcotest_lwt;
              octez_stdlib |> if_ N.(number >= 013) |> open_;
            ]
          ~dune:
            Dune.
              [
                alias_rule
                  "runtest"
                  ~package:(sf "tezos-protocol-%s-tests" name_dash)
                  ~action:(run_exe "main" ["test"; "Unit"]);
              ]
      in
      let _regresssion =
        if N.(number >= 014) then
          (* About [~dep_globs]: this is only needed so that dune re-runs the tests
             if those files are modified. Dune will also copy those files in [_build],
             but the test uses absolute paths to find those files
             (thanks to [DUNE_SOURCEROOT] and [Filename.dirname __FILE__]),
             so those copies are not actually used. This is needed so that the test
             can be run either with [dune build @runtezt],
             with [dune exec src/proto_alpha/lib_protocol/test/regression/main.exe],
             or with [dune exec tezt/tests/main.exe -- -f test_logging.ml]. *)
          tezt
            ["test_logging"]
            ~path:(path // "lib_protocol/test/regression")
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

    let make ~name =
      let name_underscore = Name.name_underscore name in
      let name_dash = Name.name_dash name in
      let number = Name.number name in
      let path = Name.base_path name in
      let dirname = path // "lib_protocol" in
      let tezos_protocol_filename = dirname // "TEZOS_PROTOCOL" in
      let tezos_protocol = Tezos_protocol.of_file_exn tezos_protocol_filename in
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
        (* [Other] and [Alpha] protocols can be edited and should be
           fixed whenever a warning that we care about triggers. We
           only want to disable a limited set of warnings *)
        | Other | Alpha -> []
        (* [V _] protocols can't be edited to accomodate warnings, we need to disable warnings instead. *)
        | V _ as number ->
            if N.(number >= 014) then []
            else if N.(number >= 011) then [51]
            else [6; 7; 9; 16; 29; 32; 51; 68]
      in
      let environment =
        public_lib
          (sf "tezos-protocol-%s.environment" name_dash)
          ~internal_name:(sf "tezos_protocol_environment_%s" name_underscore)
          ~path:(path // "lib_protocol")
          ~opam:(sf "tezos-protocol-%s" name_dash)
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
        public_lib
          (sf "tezos-protocol-%s.raw" name_dash)
          ~internal_name:(sf "tezos_raw_protocol_%s" name_underscore)
          ~path:(path // "lib_protocol")
          ~opam:(sf "tezos-protocol-%s" name_dash)
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
        public_lib
          (sf "tezos-protocol-%s" name_dash)
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
            | Alpha | V _ -> "Tezos/Protocol: economic-protocol definition")
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
                  [as_ "TEZOS_PROTOCOL" "raw/TEZOS_PROTOCOL"]
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
let hash = Tezos_crypto.Protocol_hash.of_b58check_exn "%s"
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
                      S "%{bin:tezos-protocol-compiler}";
                      (if
                       String_set.mem
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
            | Alpha | V _ ->
                "Tezos/Protocol: economic-protocol definition parameterized by \
                 its environment implementation")
          ~modules:["Functor"]
            (* The instrumentation is removed as it can lead to a stack overflow *)
            (* https://gitlab.com/tezos/tezos/-/issues/1927 *)
          ~bisect_ppx:false
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
                            "%{bin:tezos-protocol-compiler.tezos-protocol-packer}";
                          S "%{src_dir}";
                        ];
                      ];
                    ];
              ]
      in
      let embedded =
        public_lib
          (sf "tezos-embedded-protocol-%s" name_dash)
          ~internal_name:(sf "tezos_embedded_protocol_%s" name_underscore)
          ~path:(path // "lib_protocol")
          ~synopsis:
            (match number with
            | V _ as number when N.(number <= 003) ->
                sf
                  "Tezos/Protocol: %s (economic-protocol definition, embedded \
                   in `tezos-node`)"
                  (if N.(number == 000) then name_dash else name_underscore)
            | Other ->
                sf
                  "Tezos/Protocol: %s (economic-protocol definition, embedded \
                   in `tezos-node`)"
                  name_underscore
            | Alpha | V _ ->
                "Tezos/Protocol: economic-protocol definition, embedded in \
                 `tezos-node`")
          ~modules:["Registerer"]
          ~linkall:true
          ~flags:(Flags.standard ~disable_warnings ())
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
                          S "%{bin:tezos-embedded-protocol-packer}";
                          S "%{src_dir}";
                          S name_underscore;
                        ];
                      ];
                    ];
              ]
      in
      {main; embedded}
  end

  let genesis =
    let name = Name.other "genesis" in
    let {Lib_protocol.main; embedded} = Lib_protocol.make ~name in
    let client =
      public_lib
        (sf "tezos-client-%s" (Name.name_dash name))
        ~path:(Name.base_path name // "lib_client")
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_shell_services |> open_;
            octez_client_base |> open_;
            octez_protocol_environment;
            main |> open_;
            octez_client_commands |> open_;
            octez_proxy;
            octez_stdlib_unix;
          ]
        ~linkall:true
    in
    register @@ make ~name ~status:Not_mainnet ~main ~embedded ~client ()

  let demo_noops =
    let name = Name.other "demo-noops" in
    let {Lib_protocol.main; embedded} = Lib_protocol.make ~name in
    register @@ make ~name ~status:Not_mainnet ~main ~embedded ()

  let _demo_counter =
    let name = Name.other "demo-counter" in
    let {Lib_protocol.main; embedded} = Lib_protocol.make ~name in
    let client =
      public_lib
        (sf "tezos-client-%s" (Name.name_dash name))
        ~path:(Name.base_path name // "lib_client")
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_shell_services |> open_;
            octez_client_base |> open_;
            octez_client_commands |> open_;
            main |> open_;
          ]
        ~linkall:true
    in
    register @@ make ~name ~status:Not_mainnet ~main ~embedded ~client ()

  let register_alpha_family status name =
    let name_dash = Name.name_dash name in
    let name_underscore = Name.name_underscore name in
    let number = Name.number name in
    let path = Name.base_path name in
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
    let both o1 o2 =
      match (o1, o2) with Some x, Some y -> Some (x, y) | _, _ -> None
    in
    let {Lib_protocol.main; embedded} = Lib_protocol.make ~name in
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
    in
    let _parameters_exe =
      opt_map parameters @@ fun parameters ->
      private_exe
        "gen"
        ~path:(path // "lib_parameters")
        ~opam:(sf "tezos-protocol-%s" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
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
                ~package:(sf "tezos-protocol-%s" name_dash)
                ~section:"lib";
            ])
        ~bisect_ppx:false
    in
    let plugin =
      only_if (N.(number >= 007) && not_overridden) @@ fun () ->
      public_lib
        (sf "tezos-protocol-plugin-%s" name_dash)
        ~path:(path // "lib_plugin")
        ~synopsis:"Tezos/Protocol: protocol plugin"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
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
        ~path:(path // "lib_plugin")
        ~synopsis:"Tezos/Protocol: protocol plugin registerer"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            embedded |> open_;
            plugin |> open_;
            octez_shell |> open_;
          ]
        ~modules:["Plugin_registerer"]
        ~bisect_ppx:N.(number >= 008)
    in
    let client =
      only_if not_overridden @@ fun () ->
      public_lib
        (sf "tezos-client-%s" name_dash)
        ~path:(path // "lib_client")
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_shell_services |> open_;
            octez_client_base |> open_;
            main |> open_;
            octez_mockup_registration |> if_ N.(number >= 011);
            octez_proxy |> if_ N.(number >= 011);
            octez_signer_backends |> if_ N.(number >= 001);
            plugin |> if_some |> open_if N.(number >= 008);
            parameters |> if_some |> if_ N.(number >= 011) |> open_;
            octez_rpc |> if_ N.(number >= 001) |> open_;
            octez_client_commands |> if_ N.(number == 000) |> open_;
            octez_stdlib_unix |> if_ N.(number == 000);
            uri |> if_ N.(number >= 001);
          ]
        ~bisect_ppx:N.(number >= 008)
        ?inline_tests:(if N.(number >= 009) then Some ppx_expect else None)
        ~linkall:true
    in
    let test_helpers =
      only_if active @@ fun () ->
      public_lib
        (sf "tezos-%s-test-helpers" name_dash)
        ~path:
          (if active then path // "lib_protocol/test/helpers"
          else path // "lib_protocol")
        ~opam:
          (if active then sf "tezos-%s-test-helpers" name_dash
          else sf "tezos-%s-test-helpers" name_dash)
        ~internal_name:(sf "tezos_%s_test_helpers" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol testing framework"
        ~opam_only_deps:[octez_protocol_environment; parameters |> if_some]
        ~deps:
          [
            alcotest_lwt;
            qcheck_alcotest;
            octez_test_helpers;
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_micheline |> open_;
            octez_stdlib_unix |> open_;
            main |> open_;
            client |> if_some |> open_;
            parameters |> if_some;
            octez_protocol_environment;
            plugin |> if_some |> open_;
            octez_shell_services |> open_;
            plompiler |> if_ N.(number >= 015);
          ]
    in
    let _plugin_tests =
      opt_map (both plugin test_helpers) @@ fun (plugin, test_helpers) ->
      only_if (active && N.(number <> 011)) @@ fun () ->
      tests
        ["test_consensus_filter"; "test_filter_state"; "test_plugin"]
        ~path:(path // "lib_plugin/test")
        ~synopsis:"Tezos/Protocol: protocol plugin tests"
        ~opam:(sf "tezos-protocol-plugin-%s-tests" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_base_test_helpers |> open_;
            octez_base_unix |> if_ N.(number >= 013);
            alcotest_lwt;
            octez_test_helpers;
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
      only_if N.(number >= 011) @@ fun () ->
      tests
        [
          "test_michelson_v1_macros";
          "test_client_proto_contracts";
          "test_client_proto_context";
          "test_proxy";
        ]
        ~path:(path // "lib_client/test")
        ~opam:(sf "tezos-client-%s" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_micheline |> open_;
            client |> if_some |> open_;
            main |> open_;
            octez_base_test_helpers |> open_;
            octez_test_helpers |> open_;
            alcotest_lwt;
            qcheck_alcotest;
          ]
    in
    let client_commands =
      only_if (N.(number >= 001) && not_overridden) @@ fun () ->
      public_lib
        (sf "tezos-client-%s.commands" name_dash)
        ~path:(path // "lib_client_commands")
        ~deps:
          [
            octez_base
            |> if_ N.(number <= 14)
            |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_base |> if_ N.(number >= 15) |> open_ ~m:"TzPervasives";
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
            octez_rpc |> open_;
            octez_client_base_unix |> if_ N.(number >= 009) |> open_;
            plugin |> if_some |> if_ N.(number >= 008) |> open_;
            (* uri used by the stresstest command introduced in 011 *)
            uri |> if_ N.(number >= 011);
          ]
        ~bisect_ppx:N.(number >= 008)
        ~linkall:true
        ~all_modules_except:["alpha_commands_registration"]
    in
    let client_sapling =
      only_if (N.(number >= 011) && not_overridden) @@ fun () ->
      public_lib
        (sf "tezos-client-%s.sapling" name_dash)
        ~internal_name:(sf "tezos_client_sapling_%s" name_underscore)
        ~path:(path // "lib_client_sapling")
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
      public_lib
        (sf "tezos-client-%s.commands-registration" name_dash)
        ~path:(path // "lib_client_commands")
        ~opam:(sf "tezos-client-%s" name_dash)
        ~deps:
          [
            octez_base
            |> if_ N.(number <= 14)
            |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_base |> if_ N.(number >= 15) |> open_ ~m:"TzPervasives";
            main |> open_;
            parameters |> if_some |> if_ N.(number >= 013) |> open_;
            octez_protocol_environment;
            octez_shell_services |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            client_commands |> if_some |> open_;
            client_sapling |> if_some |> if_ N.(number >= 011) |> open_;
            octez_rpc |> open_;
            plugin |> if_some |> if_ N.(number >= 008) |> open_;
          ]
        ~bisect_ppx:N.(number >= 008)
        ~linkall:true
        ~modules:["alpha_commands_registration"]
    in
    let baking =
      only_if active @@ fun () ->
      public_lib
        ("tezos-baking-" ^ name_dash)
        ~path:(path // "lib_delegate")
        ~synopsis:
          (if N.(number <= 011) then
           "Tezos/Protocol: base library for `tezos-baker/endorser/accuser`"
          else "Tezos/Protocol: base library for `tezos-baker/accuser`")
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_version;
            main |> open_;
            plugin |> if_some |> open_;
            octez_protocol_environment;
            octez_shell_services |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            octez_stdlib |> open_;
            octez_stdlib_unix |> open_;
            octez_shell_context |> open_;
            octez_context |> open_;
            octez_context_memory |> if_ N.(number >= 012);
            octez_rpc_http_client_unix |> if_ N.(number >= 011);
            octez_context_ops |> if_ N.(number >= 011) |> open_;
            octez_rpc |> open_;
            octez_rpc_http |> open_;
            lwt_canceler;
            lwt_exit;
            uri;
          ]
        ~linkall:true
        ~all_modules_except:
          (if N.(number <= 011) then
           ["Delegate_commands"; "Delegate_commands_registration"]
          else ["Baking_commands"; "Baking_commands_registration"])
    in
    let tenderbrute =
      only_if (active && N.(number >= 013)) @@ fun () ->
      public_lib
        (sf "tezos-baking-%s.tenderbrute" name_dash)
        ~internal_name:(sf "tenderbrute_%s" name_underscore)
        ~path:(path // "lib_delegate/test/tenderbrute/lib")
        ~deps:
          [
            data_encoding |> open_;
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
            |> open_;
            octez_base_unix;
            main |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
          ]
        ~bisect_ppx:false
    in
    let _tenderbrute_exe =
      only_if (active && N.(number >= 013)) @@ fun () ->
      test
        "tenderbrute_main"
        ~alias:""
        ~path:(path // "lib_delegate/test/tenderbrute")
        ~opam:(sf "tezos-baking-%s" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
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
      only_if N.(number >= 011) @@ fun () ->
      let mockup_simulator =
        only_if N.(number >= 012) @@ fun () ->
        public_lib
          (sf "tezos-baking-%s.mockup-simulator" name_dash)
          ~internal_name:(sf "tezos_%s_mockup_simulator" name_underscore)
          ~path:(path // "lib_delegate/test/mockup_simulator")
          ~deps:
            [
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
            ]
          ~bisect_ppx:false
      in
      test
        "main"
        ~path:(path // "lib_delegate/test")
        ~opam:(sf "tezos-baking-%s" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_protocol_environment |> if_ N.(number <= 011);
            octez_test_helpers |> if_ N.(number <= 011);
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
            alcotest_lwt;
            uri;
          ]
    in
    let baking_commands =
      only_if active @@ fun () ->
      public_lib
        (sf "tezos-baking-%s-commands" name_dash)
        ~path:(path // "lib_delegate")
        ~synopsis:"Tezos/Protocol: protocol-specific commands for baking"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
            octez_stdlib_unix |> open_;
            octez_protocol_environment;
            octez_shell_services |> open_;
            octez_client_base |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            baking |> if_some |> open_;
            octez_rpc |> open_;
            uri;
          ]
        ~linkall:true
        ~modules:
          [
            (if N.(number <= 011) then "Delegate_commands"
            else "Baking_commands");
          ]
    in
    let baking_commands_registration =
      only_if active @@ fun () ->
      public_lib
        (sf "tezos-baking-%s-commands.registration" name_dash)
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
            octez_rpc |> open_;
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
        (sf "tezos-%s-%s" daemon name_dash)
        ~internal_name:(sf "main_%s_%s" daemon name_underscore)
        ~path:(path // sf "bin_%s" daemon)
        ~synopsis:(sf "Tezos/Protocol: %s binary" daemon)
        ~release:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            baking_commands |> if_some |> open_;
            octez_stdlib_unix |> open_;
            octez_client_base_unix |> open_;
          ]
    in
    let _baker = daemon "baker" in
    let _accuser = daemon "accuser" in
    let _endorser = only_if N.(number <= 011) @@ fun () -> daemon "endorser" in
    let injector =
      only_if N.(number >= 013) @@ fun () ->
      public_lib
        (sf "tezos-injector-%s" name_dash)
        ~path:(path // "lib_injector")
        ~synopsis:"Tezos/Protocol: protocol specific library building injectors"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
            |> open_;
            octez_base_unix;
            octez_stdlib_unix |> open_;
            octez_crypto |> open_;
            main |> open_;
            octez_micheline |> open_;
            client |> if_some |> open_;
            octez_client_base |> open_;
            octez_workers |> open_;
            octez_shell;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let sc_rollup =
      only_if N.(number >= 013) @@ fun () ->
      public_lib
        (sf "tezos-sc-rollup-%s" name_dash)
        ~path:(path // "lib_sc_rollup")
        ~synopsis:
          "Tezos/Protocol: protocol specific library for `tezos-sc-rollup`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            main |> open_;
            plugin |> if_some |> open_;
            parameters |> if_some |> open_;
            octez_rpc |> open_;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let _sc_rollup_client =
      only_if (active && N.(number >= 013)) @@ fun () ->
      public_exe
        (sf "tezos-sc-rollup-client-%s" name_dash)
        ~internal_name:(sf "main_sc_rollup_client_%s" name_underscore)
        ~path:(path // "bin_sc_rollup_client")
        ~synopsis:"Tezos/Protocol: `tezos-sc-rollup-client-alpha` client binary"
        ~release:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_client_base;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            octez_stdlib_unix |> open_;
            octez_client_base_unix |> open_;
            octez_rpc_http;
            octez_rpc_http_client_unix |> open_;
            main |> open_;
            sc_rollup |> if_some |> open_;
            uri;
          ]
    in
    let _sc_rollup_node =
      only_if (active && N.(number >= 013)) @@ fun () ->
      public_exe
        (sf "tezos-sc-rollup-node-%s" name_dash)
        ~internal_name:(sf "main_sc_rollup_node_%s" name_underscore)
        ~path:(path // "bin_sc_rollup_node")
        ~synopsis:"Tezos/Protocol: Smart Contract Rollup node binary"
        ~release:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_client_commands |> open_;
            octez_stdlib_unix |> open_;
            octez_client_base |> open_;
            octez_client_base_unix |> open_;
            client |> if_some |> open_;
            octez_context_encoding;
            octez_context_helpers;
            main |> open_;
            plugin |> if_some |> open_;
            parameters |> if_some |> open_;
            octez_rpc |> open_;
            octez_rpc_http;
            octez_rpc_http_server;
            octez_shell_services |> open_;
            sc_rollup |> if_some |> open_;
            data_encoding;
            irmin_pack;
            irmin_pack_unix;
            irmin;
            ringo;
            ringo_lwt;
            injector |> if_some |> open_;
          ]
    in
    let tx_rollup =
      only_if N.(number >= 013) @@ fun () ->
      public_lib
        (sf "tezos-tx-rollup-%s" name_dash)
        ~path:(path // "lib_tx_rollup")
        ~synopsis:
          "Tezos/Protocol: protocol specific library for `tezos-tx-rollup`"
        ~deps:
          [
            index;
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
            |> open_;
            octez_crypto |> open_;
            main |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            octez_context_encoding;
            baking_commands |> if_some |> open_;
            octez_stdlib_unix |> open_;
            octez_rpc |> open_;
            octez_rpc_http |> open_;
            octez_rpc_http_client_unix |> open_;
            octez_rpc_http_server |> open_;
            octez_micheline |> open_;
            octez_client_base |> open_;
            octez_client_base_unix |> open_;
            octez_shell;
            octez_store;
            octez_workers |> open_;
            plugin |> if_some |> open_;
            injector |> if_some |> open_;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let _tx_rollup_client =
      only_if (active && N.(number >= 013)) @@ fun () ->
      public_exe
        (sf "tezos-tx-rollup-client-%s" name_dash)
        ~internal_name:(sf "main_tx_rollup_client_%s" name_underscore)
        ~path:(path // "bin_tx_rollup_client")
        ~synopsis:"Tezos/Protocol: `tezos-tx-rollup-client-alpha` client binary"
        ~release:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_ |> open_ ~m:"Protocol";
            client |> if_some |> open_;
            client_commands |> if_some |> open_;
            octez_client_base_unix |> open_;
            octez_stdlib_unix |> open_;
            tx_rollup |> if_some |> open_;
            uri;
          ]
    in
    let _tx_rollup_node =
      only_if (active && N.(number >= 013)) @@ fun () ->
      public_exe
        (sf "tezos-tx-rollup-node-%s" name_dash)
        ~internal_name:(sf "main_tx_rollup_node_%s" name_underscore)
        ~path:(path // "bin_tx_rollup_node")
        ~synopsis:"Tezos/Protocol: Transaction Rollup node binary"
        ~release:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
            |> open_;
            main |> open_;
            client |> if_some |> open_;
            octez_client_base |> open_;
            octez_client_base_unix |> open_;
            tx_rollup |> if_some |> open_;
          ]
    in
    let dal =
      only_if (active && N.(number >= 015)) @@ fun () ->
      public_lib
        (sf "tezos-dal-%s" name_dash)
        ~path:(path // "lib_dal")
        ~synopsis:
          "Tezos/Protocol: protocol specific library for the Data availability \
           Layer"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_protocol_compiler_registerer |> open_;
            octez_dal_node_lib |> open_;
            client |> if_some |> open_;
            embedded |> open_;
            main |> open_;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let _dal_tests =
      only_if (active && N.(number >= 015)) @@ fun () ->
      test
        "main"
        ~path:(path // "lib_dal/test")
        ~opam:(sf "tezos-dal-%s" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            dal |> if_some |> open_;
            main |> open_;
            octez_base_test_helpers |> open_;
            test_helpers |> if_some |> open_;
            alcotest_lwt;
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
            octez_crypto;
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
        ~deps:
          [
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
            octez_base |> open_
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_error_monad |> open_;
            octez_micheline |> open_;
            octez_micheline_rewriting |> open_;
            octez_benchmark |> open_;
            benchmark_type_inference |> if_some |> open_;
            main |> open_;
            octez_crypto |> open_;
            parameters |> if_some;
            hashcons;
            test_helpers |> open_;
            prbnmcn_stats;
          ]
        ~linkall:true
        ~private_modules:["kernel"; "rules"; "state_space"]
        ~bisect_ppx:N.(number <= 012)
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
        ~opam:(sf "tezos-benchmark-%s" name_dash)
        ~deps:
          [
            octez_base
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_micheline |> open_;
            octez_micheline_rewriting;
            main |> open_;
            octez_benchmark |> open_;
            benchmark_type_inference |> if_some |> open_;
            benchmark |> if_some |> open_;
            test_helpers |> open_;
            octez_error_monad;
            alcotest_lwt;
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
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_error_monad |> open_;
            parameters |> if_some |> open_;
            octez_benchmark |> open_;
            benchmark |> if_some |> open_;
            benchmark_type_inference |> if_some |> open_;
            main |> open_ |> open_ ~m:"Protocol";
            octez_crypto |> open_;
            octez_shell_benchmarks;
            octez_micheline |> open_;
            test_helpers |> open_;
            octez_sapling;
            client |> if_some |> open_;
            octez_protocol_environment;
          ]
        ~linkall:true
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
          ?sc_rollup
          ~main
          ~name
          ()
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
         ?test_helpers
         ?parameters
         ?benchmarks_proto
         ?baking
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

  let _012_Psithaca = active (Name.v "Psithaca" 012)

  let _013_PtJakart = active (Name.v "PtJakart" 013)

  let _014_PtKathma = active (Name.v "PtKathma" 014)

  let alpha = active Name.alpha

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
  test
    "test_rewriting"
    ~path:"src/lib_benchmark/lib_micheline_rewriting/test"
    ~opam:"tezos-micheline-rewriting"
    ~deps:
      [
        octez_micheline |> open_;
        octez_micheline_rewriting;
        Protocol.(main alpha);
        octez_error_monad;
        Protocol.(client_exn alpha);
        alcotest_lwt;
      ]

let _octez_store_tests =
  tests
    ["test"; "test_locator"]
    ~path:"src/lib_store/unix/test"
    ~opam:"tezos-store"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_context_ops |> open_;
        octez_store_shared |> open_;
        octez_store_unix |> open_;
        octez_store_unix_reconstruction |> open_;
        octez_store_unix_snapshots |> open_;
        octez_shell_services |> open_;
        octez_stdlib_unix |> open_;
        octez_validation |> open_;
        Protocol.(embedded demo_noops);
        Protocol.(embedded genesis);
        Protocol.(embedded alpha);
        Protocol.(parameters_exn alpha |> open_);
        Protocol.(plugin_exn alpha) |> open_;
        alcotest_lwt;
        octez_test_helpers;
        octez_test_helpers_extra;
      ]
    ~alias:""
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
          alias_rule
            "runtest_locator_bench"
            ~package:"tezos-store"
            ~action:(run_exe "test_locator" ["--bench"]);
        ]

let _octez_shell_tests =
  tests
    [
      "test_shell";
      "test_synchronisation_heuristic_fuzzy";
      "test_prevalidation";
      "test_prevalidation_t";
      "test_prevalidator_classification";
      "test_prevalidator_classification_operations";
      "test_prevalidator_pending_operations";
      "test_peer_validator";
    ]
    ~path:"src/lib_shell/test"
    ~opam:"tezos-shell"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_test_helpers |> open_;
        octez_store |> open_;
        octez_store_shared |> open_;
        octez_context |> open_;
        octez_context_ops |> open_;
        octez_shell_context |> open_;
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
        octez_test_helpers;
        alcotest_lwt;
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

let _git_gas_diff =
  public_exe
    "git-gas-diff"
    ~path:"devtools/git-gas-diff/bin"
    ~synopsis:"Internal dev tools"
    ~internal_name:"main"
    ~opam:"internal-devtools"
    ~deps:[external_lib "num" V.True; re]
    ~static:false
    ~bisect_ppx:false

let remove_if_exists fname = if Sys.file_exists fname then Sys.remove fname

let get_contracts_lib =
  private_lib
    "get_contracts"
    ~path:("devtools" // "get_contracts")
    ~synopsis:"Generic tool to extract smart contracts from node's context."
    ~opam:""
    ~deps:
      [
        octez_micheline |> open_;
        octez_base |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_client_base_unix;
        octez_store;
      ]
    ~modules:["get_contracts"; "sigs"; "storage_helpers"; "contract_size"]
    ~bisect_ppx:false

let _get_contracts =
  let path = "devtools" // "get_contracts" in
  let get_contracts_module proto =
    path // (sf "get_contracts_%s.ml" @@ Protocol.name_underscore proto)
  in
  List.filter_map
    (fun proto ->
      let name = "get_contracts_" ^ Protocol.name_underscore proto in
      let main_module = get_contracts_module proto in
      match (Protocol.status proto, Protocol.client proto) with
      | Active, Some client ->
          let main = Protocol.main proto in
          if not @@ Sys.file_exists main_module then
            ignore @@ Sys.command
            @@ Format.sprintf
                 "cp %s %s"
                 (get_contracts_module Protocol.alpha)
                 main_module ;
          Some
            (private_exe
               name
               ~path
               ~synopsis:"A script to extract smart contracts from a node."
               ~opam:""
               ~deps:
                 [
                   octez_base |> open_ ~m:"TzPervasives";
                   main |> open_ |> open_ ~m:"Protocol";
                   client |> open_;
                   get_contracts_lib;
                 ]
               ~modules:[name]
               ~bisect_ppx:false)
      | _ ->
          remove_if_exists main_module ;
          None)
    Protocol.all

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
    ~synopsis:"A development tool for extracting baker keys from a context."
    ~opam:""
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         lwt_unix;
         ezjsonm;
         octez_store;
         octez_shell_context;
         octez_context;
       ]
      @ protocols)
    ~all_modules_except:["yes_wallet"]
    ~bisect_ppx:false
    ~linkall:true

let _yes_wallet =
  private_exe
    "yes_wallet"
    ~path:("devtools" // "yes_wallet")
    ~synopsis:
      "A script extracting delegates' keys from a context into a wallet."
    ~opam:""
    ~deps:[yes_wallet_lib |> open_]
    ~modules:["yes_wallet"]
    ~bisect_ppx:false

let _yes_wallet_test =
  private_exe
    "bench_signature_perf"
    ~path:("devtools" // "yes_wallet" // "test")
    ~synopsis:"Tests for yes_wallet tool"
    ~opam:""
    ~deps:
      [
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        octez_crypto |> open_;
        zarith;
        zarith_stubs_js;
        data_encoding |> open_;
        alcotest;
        alcotest_lwt;
        lwt_unix;
        qcheck_alcotest;
        octez_test_helpers;
        ptime;
      ]
    ~bisect_ppx:false

let _ppinclude =
  private_exe
    "ppinclude"
    ~path:"src/lib_protocol_environment/ppinclude"
    ~opam:"tezos-protocol-environment"
    ~bisect_ppx:false
    ~deps:[compiler_libs_common]

let _octez_validator_bin =
  public_exe
    "tezos-validator"
    ~path:"src/bin_validation/bin"
    ~opam:"tezos-validator"
    ~internal_name:"main_validator"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_context |> open_;
        octez_stdlib_unix |> open_;
        octez_shell |> open_;
        octez_shell_services |> open_;
        octez_validation |> open_;
        octez_protocol_updater |> open_;
        octez_validator_lib |> open_;
      ]
    ~linkall:true

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
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Alpha | Other) ->
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
    ~release:true
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives" |> open_;
         octez_base_unix;
         octez_version;
         octez_stdlib_unix |> open_;
         octez_shell_services |> open_;
         octez_rpc_http |> open_;
         octez_rpc_http_server |> open_;
         octez_p2p |> open_;
         octez_shell |> open_;
         octez_store |> open_;
         octez_store_unix_reconstruction |> open_;
         octez_store_unix_snapshots |> open_;
         octez_context |> open_;
         octez_validator_lib |> open_;
         octez_validation |> open_;
         octez_shell_context |> open_;
         octez_workers |> open_;
         octez_protocol_updater |> open_;
         cmdliner;
         fmt_cli;
         fmt_tty;
         tls;
         prometheus_app_unix;
         lwt_exit;
         uri;
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

let _octez_client =
  let protocol_deps =
    let deps_for_protocol protocol =
      let is_optional =
        match (Protocol.status protocol, Protocol.number protocol) with
        | Active, V _ -> false
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Alpha | Other) ->
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
    ~opam:"tezos-client"
    ~synopsis:"Tezos: `tezos-client` binary"
    ~release:true
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
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
          alias_rule
            "runtest_compile_protocol"
            ~deps_dune:[[S "source_tree"; S "test/proto_test_injection"]]
            ~action:
              [
                S "run";
                S "%{bin:tezos-protocol-compiler}";
                S "-no-hash-check";
                H [S "-warn-error"; S "+a"];
                S "test/proto_test_injection/";
              ];
        ]

let _octez_codec =
  public_exe
    "tezos-codec"
    ~path:"src/bin_codec"
    ~internal_name:"codec"
    ~synopsis:"Tezos: `tezos-codec` binary to encode and decode values"
    ~release:true
    ~deps:
      ([
         data_encoding |> open_;
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_client_base_unix |> open_;
         octez_client_base |> open_;
         octez_clic |> open_;
         octez_stdlib_unix |> open_;
         octez_event_logging |> open_;
         octez_signer_services;
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

let _octez_proxy_server =
  public_exe
    "tezos-proxy-server"
    ~path:"src/bin_proxy_server"
    ~internal_name:"main_proxy_server"
    ~synopsis:"Tezos: `tezos-proxy-server` binary"
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives" |> open_;
         octez_base_unix;
         octez_stdlib_unix |> open_;
         cmdliner;
         lwt_exit;
         lwt_unix;
         octez_proxy;
         octez_proxy_server_config;
         octez_rpc_http_client_unix;
         octez_rpc_http_server;
         octez_shell_services;
         octez_shell_context;
         octez_version;
         uri;
       ]
      @ Protocol.all_optionally [Protocol.client; Protocol.plugin])
    ~linkall:true

let _octez_snoop =
  public_exe
    "tezos-snoop"
    ~path:"src/bin_snoop"
    ~internal_name:"main_snoop"
    ~synopsis:"Tezos: `tezos-snoop` binary"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_clic;
        octez_benchmark |> open_;
        octez_benchmark_examples;
        octez_shell_benchmarks;
        Protocol.(benchmarks_proto_exn alpha);
        str;
        ocamlgraph;
        pyml;
        prbnmcn_stats;
      ]
    ~linkall:true
    ~dune:
      Dune.
        [
          S "cram"
          :: G [S "deps" :: [S "main_snoop.exe"]]
          :: [S "package" :: [S "tezos-snoop"]];
        ]

(* We use Dune's select statement and keep uTop optional *)
(* Keeping uTop optional lets `make build` succeed, *)
(* which uses tezos/opam-repository to resolve dependencies, *)
(* on the CI. This prevents having to add dev-dependency to *)
(* tezos/opam-repository unnecessarily *)
(* We set [~static] to false because we don't release this as a static binary. *)
let _tztop =
  public_exe
    "tztop"
    ~path:"devtools/tztop"
    ~internal_name:"tztop_main"
    ~opam:"internal-devtools"
    ~modes:[Byte]
    ~bisect_ppx:false
    ~static:false
    ~deps:
      [
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
    "tezos-signer"
    ~path:"src/bin_signer"
    ~internal_name:"main_signer"
    ~synopsis:"Tezos: `tezos-signer` binary"
    ~release:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
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
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        caqti;
        caqti_dynload;
        caqti_lwt;
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
        | (Frozen | Overridden | Not_mainnet), _ | Active, (Alpha | Other) ->
            (* Other protocols are optional. *)
            true
      in
      let targets = List.filter_map Fun.id [Protocol.dal protocol] in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  public_exe
    "tezos-dal-node"
    ~path:"src/bin_dal_node"
    ~internal_name:"main_dal"
    ~synopsis:"Tezos: `tezos-dal-node` binary"
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         octez_client_commands |> open_;
         octez_rpc_http |> open_;
         octez_rpc_http_server;
         octez_protocol_updater;
         octez_rpc_http_client_unix;
         octez_stdlib_unix |> open_;
         octez_stdlib |> open_;
         octez_dal_node_lib |> open_;
         octez_dal_node_services |> open_;
         octez_crypto_dal |> open_;
         irmin_pack;
         irmin_pack_unix;
         irmin;
       ]
      @ protocol_deps)

(* Add entries to this function to declare that some dune and .opam files are
   not generated by the manifest on purpose.

   - DO NOT add packages to this list without a good reason.
   - DO NOT add released packages to this list even with a good reason.
     Instead, you need a VERY good reason, because those packages prevent
     automatic opam releases.
   - ALWAYS add a comment to explain why a package is excluded. *)
let exclude filename =
  let is_proto_ name = String.starts_with ~prefix:"proto_" name in
  match String.split_on_char '/' filename with
  (* Dune files in src/proto_*/parameters only have a (copy_files) stanza
     (no library / executable / test). *)
  | "src" :: maybe_proto :: "parameters" :: _ when is_proto_ maybe_proto -> true
  (* This dune file does not contain any targets, only a dirs stanza. *)
  | ["src"; maybe_proto; "lib_protocol"; "test"; "regression"; "tezt"; "dune"]
    when is_proto_ maybe_proto ->
      true
  (* The following directory has a very specific structure that would be hard
     to port to the manifest. Also, it is not released, and is not a dependency
     for releases as it is an opt-in instrumentation. *)
  | "src" :: "lib_time_measurement" :: _ -> true
  (* The following file only defines aliases (no library / executable / test). *)
  | ["src"; "lib_protocol_compiler"; "test"; "dune"] -> true
  (* We don't generate the toplevel dune file. *)
  | ["dune"] -> true
  (* The following directories do not contain packages that we release on opam. *)
  | "vendors" :: _ -> true
  | "scripts" :: _ -> true
  | "docs" :: _ -> true
  (* opam-repository is used by scripts/opam-release.sh *)
  | "opam-repository" :: _ -> true
  (* Tezt is only partially managed by the manifest.
     There is no real good reason for that but only the core Tezt library is released. *)
  | "tezt" :: "long_tests" :: _ -> true
  | "tezt" :: "manual_tests" :: _ -> true
  | "tezt" :: "records" :: _ -> true
  | "tezt" :: "remote_tests" :: _ -> true
  | "tezt" :: "snoop" :: _ -> true
  | "tezt" :: "vesting_contract_test" :: _ -> true
  | _ -> false

let () =
  (* [make_tezt_exe] makes the global executable that contains all tests.
     [generate] gives it the list of libraries that register Tezt tests
     so that it can link all of them. *)
  let make_tezt_exe test_libs =
    let deps =
      [
        tezt_lib |> open_ |> open_ ~m:"Base";
        str;
        bls12_381;
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        data_encoding;
        octez_crypto_dal;
        octez_base;
        octez_base_unix;
        octez_stdlib_unix;
        Protocol.(main alpha);
      ]
    in
    test "main" ~alias:"" ~path:"tezt/tests" ~opam:"" ~deps:(deps @ test_libs)
  in
  generate ~make_tezt_exe

(* Generate a dunw-workspace file at the root of the repo *)
let () =
  let p_dev = Env.Profile "dev" in
  let p_static = Env.Profile "static" in
  let p_release = Env.Profile "release" in
  let warnings_for_dev =
    (* We mark all warnings as error and disable the few ones we don't want *)
    (* The last warning number 72 should be revisited when we move from OCaml.4.14 *)
    "@1..72" ^ Flags.disabled_warnings_to_string warnings_disabled_by_default
  in
  let env =
    Env.empty
    |> Env.add p_static ~key:"ocamlopt_flags" Dune.[S ":standard"; S "-O3"]
    |> Env.add p_release ~key:"ocamlopt_flags" Dune.[S ":standard"; S "-O3"]
    |> Env.add
         p_dev
         ~key:"flags"
         Dune.[S ":standard"; S "-w"; S warnings_for_dev]
    |> Env.add
         Env.Any
         ~key:"js_of_ocaml"
         Dune.[S "runtest_alias"; S "runtest_js"]
  in
  let dune =
    Dune.
      [
        [
          S "context";
          [S "default"; [S "paths"; [S "ORIGINAL_PATH"; S ":standard"]]];
        ];
      ]
  in
  generate_workspace env dune

(* Generate active_protocol_versions. *)
let () =
  let write_protocol fmt protocol =
    match Protocol.number protocol with
    | Alpha | V _ -> Format.fprintf fmt "%s\n" (Protocol.name_dash protocol)
    | Other -> ()
  in
  write "script-inputs/active_protocol_versions" @@ fun fmt ->
  List.iter (write_protocol fmt) Protocol.active

let () = check ~exclude ()
