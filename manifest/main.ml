(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
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

let warnings_disabled_by_default =
  [4; 40; 41; 42; 44; 45; 48; 58; 60; 67; 69; 70]
(*
   4 [fragile-match]
  40 [name-out-of-scope]
  41 [ambiguous-name]
  42 [disambiguated-name]
  44 [open-shadow-identifier]
  45 [open-shadow-label-constructor]
  48 [eliminated-optional-arguments]
  58 [no-cmx-file] (flambda specific, necessary as long as Zarith_version.cmx is not installed)
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

let astring = external_lib ~js_compatible:true "astring" V.True

let bigarray_compat = external_lib ~js_compatible:true "bigarray-compat" V.True

let bigstring = external_lib ~js_compatible:true "bigstring" V.True

let bigstringaf =
  external_lib ~js_compatible:true "bigstringaf" V.(at_least "0.5.0")

let bisect_ppx = opam_only "bisect_ppx" V.(at_least "2.7.0")

let bls12_381 =
  let version = V.(at_least "6.1.0" && less_than "6.2.0") in
  external_lib
    ~js_compatible:true
    ~npm_deps:[Npm.make "@nomadic-labs/ocaml-bls12-381" version]
    "bls12-381"
    version

let camlzip = external_lib "camlzip" V.(at_least "1.11" && less_than "1.12")

let caqti = external_lib "caqti" V.True

let caqti_lwt = external_lib "caqti-lwt" V.True

let caqti_dynload = external_lib "caqti-dynload" V.True

let cmdliner = external_lib "cmdliner" V.(at_least "1.1.0")

let cohttp_lwt_unix =
  external_lib "cohttp-lwt-unix" V.(at_least "4.0.0" && different_from "5.1.0")

let compiler_libs_common = external_lib "compiler-libs.common" V.True ~opam:""

let compiler_libs_optcomp = external_lib "compiler-libs.optcomp" V.True ~opam:""

let compiler_libs_toplevel =
  external_lib "compiler-libs.toplevel" V.True ~opam:""

let conf_libev = opam_only "conf-libev" V.True

let conf_rust = opam_only "conf-rust" V.True

let ctypes = external_lib ~js_compatible:true "ctypes" V.(at_least "0.18.0")

let ctypes_foreign =
  external_lib
    ~js_compatible:true
    ~opam:"ctypes-foreign"
    "ctypes.foreign"
    V.(at_least "0.18.0")

let ctypes_stubs = external_sublib ctypes "ctypes.stubs"

let ctypes_stubs_js = external_lib ~js_compatible:true "ctypes_stubs_js" V.True

let data_encoding =
  external_lib
    ~js_compatible:true
    ~main_module:"Data_encoding"
    "data-encoding"
    V.(at_least "0.7.1" && less_than "1.0.0")

let dune_configurator = external_lib "dune-configurator" V.True

let dynlink = external_lib "dynlink" V.True ~opam:""

let eqaf = external_lib "eqaf" V.True

let ezjsonm = external_lib ~js_compatible:true "ezjsonm" V.(at_least "1.1.0")

let fmt = external_lib ~js_compatible:true "fmt" V.(at_least "0.8.7")

let fmt_cli = external_sublib fmt "fmt.cli"

let fmt_tty = external_sublib fmt "fmt.tty"

let hacl_star =
  external_lib
    ~js_compatible:true
    ~npm_deps:[Npm.make "hacl-wasm" V.(at_least "1.4.0" && less_than "1.5.0")]
    "hacl-star"
    V.(at_least "0.7.1" && less_than "0.8")

let hacl_star_raw = external_lib ~js_compatible:true "hacl-star-raw" V.True

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

let irmin = external_lib "irmin" V.(at_least "3.7.2" && less_than "3.8.0")

let irmin_pack =
  external_lib "irmin-pack" V.(at_least "3.7.2" && less_than "3.8.0")

let irmin_pack_unix = external_sublib irmin_pack "irmin-pack.unix"

let irmin_pack_mem = external_sublib irmin_pack "irmin-pack.mem"

let json_data_encoding =
  external_lib
    ~js_compatible:true
    "json-data-encoding"
    V.(at_least "0.12" && less_than "0.13")

let logs = external_lib "logs" V.True

let logs_fmt = external_sublib logs "logs.fmt"

let logs_lwt = external_sublib logs "logs.lwt"

let lwt = external_lib ~js_compatible:true "lwt" V.(at_least "5.6.0")

let lwt_canceler =
  external_lib
    ~js_compatible:true
    "lwt-canceler"
    V.(at_least "0.3" && less_than "0.4")

let lwt_exit = external_lib "lwt-exit" V.True

let lwt_unix = external_sublib lwt "lwt.unix"

let lwt_watcher = external_lib "lwt-watcher" V.(exactly "0.2")

let mtime =
  external_lib
    ~js_compatible:true
    "mtime"
    V.(at_least "1.4.0" && less_than "2.0.0")

let mtime_clock_os = external_sublib mtime "mtime.clock.os"

let ocaml_migrate_parsetree = external_lib "ocaml-migrate-parsetree" V.True

let ocamlformat = opam_only "ocamlformat" V.(exactly "0.24.1")

let ocamlgraph = external_lib "ocamlgraph" V.True

let ocplib_endian = external_lib ~js_compatible:true "ocplib-endian" V.True

let ocplib_endian_bigstring =
  external_sublib ocplib_endian "ocplib-endian.bigstring"

let ocplib_ocamlres =
  external_lib ~opam:"ocp-ocamlres" "ocplib-ocamlres" V.(at_least "0.4")

let ometrics = opam_only "ometrics" V.(at_least "0.2.1")

let ppx_expect = inline_tests_backend (external_lib "ppx_expect" V.True)

let ptime = external_lib ~js_compatible:true "ptime" V.(at_least "1.1.0")

let ppx_import = external_lib "ppx_import" V.True

let ppx_deriving = external_lib "ppx_deriving" V.True

let ppx_deriving_show = external_sublib ppx_deriving "ppx_deriving.show"

let ppx_repr = external_lib "ppx_repr" V.(at_least "0.6.0")

let ptime_clock_os = external_sublib ~js_compatible:true ptime "ptime.clock.os"

let pure_splitmix =
  external_lib ~js_compatible:true "pure-splitmix" V.(exactly "0.3")

let prbnmcn_linalg = external_lib "prbnmcn-linalg" V.(exactly "0.0.1")

let prbnmcn_stats = external_lib "prbnmcn-stats" V.(exactly "0.0.6")

let pringo = external_lib "pringo" V.(at_least "1.3" && less_than "1.4")

let prometheus = external_lib "prometheus" V.(at_least "1.2")

let prometheus_app = external_lib "prometheus-app" V.(at_least "1.2")

let prometheus_app_unix = external_sublib prometheus_app "prometheus-app.unix"

let pyml = external_lib "pyml" V.True

let qcheck_alcotest =
  external_lib ~js_compatible:true "qcheck-alcotest" V.(at_least "0.20")

let qcheck_core = external_lib "qcheck-core" V.True

let re = external_lib ~js_compatible:true "re" V.(at_least "1.9.0")

let repr = external_lib "repr" V.True

let resto_version = V.(at_least "1.0")

let resto = external_lib ~js_compatible:true "resto" resto_version

let resto_acl = external_lib "resto-acl" resto_version

let resto_cohttp = external_lib "resto-cohttp" resto_version

let resto_cohttp_client = external_lib "resto-cohttp-client" resto_version

let resto_cohttp_self_serving_client =
  external_lib "resto-cohttp-self-serving-client" resto_version

let resto_cohttp_server = external_lib "resto-cohttp-server" resto_version

let resto_directory =
  external_lib ~js_compatible:true "resto-directory" resto_version

let ringo = external_lib ~js_compatible:true "ringo" V.(at_least "1.0.0")

let rlp = external_lib "rlp" V.(at_least "0.1")

let rope = external_lib "rope" V.(at_least "0.6.2")

let aches = external_lib ~js_compatible:true "aches" V.(at_least "1.0.0")

let aches_lwt = external_lib "aches-lwt" V.(at_least "1.0.0")

let secp256k1_internal =
  let version = V.(at_least "0.4.0") in
  external_lib
    ~npm_deps:[Npm.make "@nomadic-labs/secp256k1-wasm" version]
    ~js_compatible:true
    "secp256k1-internal"
    version

let seqes = external_lib ~js_compatible:true "seqes" V.(at_least "0.2")

let stdint = external_lib "stdint" V.True

let str = external_lib ~js_compatible:true "str" ~opam:"" V.True

let tar = external_lib "tar" V.True

let tar_unix = external_lib "tar-unix" V.(at_least "2.0.1" && less_than "3.0.0")

let tezos_rust_lib =
  opam_only ~can_vendor:false "tezos-rust-libs" V.(exactly "1.5")

let tezos_sapling_parameters =
  opam_only ~can_vendor:false "tezos-sapling-parameters" V.(at_least "1.1.0")

let tls_lwt = external_lib "tls-lwt" V.(at_least "0.16.0")

let unix = external_lib ~opam:"base-unix" "unix" V.True

let uri = external_lib ~js_compatible:true "uri" V.(at_least "3.1.0")

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

let ledgerwallet_tezos = external_lib "ledgerwallet-tezos" V.(at_least "0.3.0")

(* This modules aims to define the list of packages versions that
   generate conflicts. *)
module Conflicts = struct
  (* Checkseum is an irmin-pack dependency, and thus a transitive
     dependency for Octez. Version 0.5.0 is known to be bugged and
     this release was disabled. *)
  let checkseum = external_lib "checkseum" V.(exactly "0.5.0")

  let hacl_x25519 = external_lib "hacl_x25519" V.True
end

(* DEVELOPMENT-ONLY DEPENDENCIES *)

let () =
  List.iter
    (add_dep_to_profile "octez-dev-deps")
    [
      external_lib "merlin" V.True;
      external_lib "odoc" V.True;
      external_lib "ocp-indent" V.True;
      external_lib "ocaml-lsp-server" V.(at_least "1.6.1");
      external_lib "merge-fmt" V.True;
      external_lib "js_of_ocaml-lwt" V.(at_least "5.2.0");
    ]

(* INTERNAL LIBS *)

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

let tezt_lib =
  external_lib
    ~js_compatible:false
    "tezt"
    V.(at_least "3.1.1")
    ~main_module:"Tezt"

let tezt_core_lib =
  external_sublib
    tezt_lib
    ~js_compatible:true
    "tezt.core"
    ~main_module:"Tezt_core"

let tezt_js_lib = external_sublib tezt_lib ~js_compatible:true "tezt.js"

let tezt ~opam ~path ?js_compatible ?modes ?(deps = []) ?dep_globs
    ?dep_globs_rec ?dep_files ?opam_with_test ?synopsis
    ?(with_macos_security_framework = false) ?flags ?dune ?preprocess
    ?preprocessor_deps l =
  tezt_without_tezt_lib_dependency
    ~with_macos_security_framework
    ~opam
    ~path
    ?synopsis
    ?js_compatible
    ?modes
    ~lib_deps:((tezt_core_lib |> open_ |> open_ ~m:"Base") :: deps)
    ~exe_deps:[tezt_lib]
    ~js_deps:[tezt_js_lib]
    ?dep_globs
    ?dep_globs_rec
    ?dep_files
    ?opam_with_test
    ?flags
    ?dune
    ?preprocess
    ?preprocessor_deps
    l

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
    ~js_compatible:true
    ~deps:[tezt_core_lib]
  |> open_

let octez_test_helpers =
  public_lib
    "tezos-test-helpers"
    ~path:"src/lib_test"
    ~internal_name:"tezos_test_helpers"
    ~synopsis:"Tezos-agnostic test helpers"
    ~deps:[uri; fmt; qcheck_alcotest; lwt; pure_splitmix; data_encoding]
    ~js_compatible:true
    ~linkall:true
    ~release_status:Released
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
    ~deps:[hex; zarith; zarith_stubs_js; lwt; aches]
    ~js_compatible:true
    ~js_of_ocaml:
      [[S "javascript_files"; G (Dune.of_atom_list ["tzBytes_js.js"])]]
    ~inline_tests:ppx_expect
    ~foreign_stubs:{language = C; flags = []; names = ["tzBytes_c"]}

let _octez_stdlib_tests =
  tezt
    [
      "test_bits";
      "test_tzList";
      "test_bounded_heap";
      "test_tzString";
      "test_fallbackArray";
      "test_functionalArray";
      "test_hash_queue";
      "test_tzBytes";
      "test_arrays";
    ]
    ~path:"src/lib_stdlib/test"
    ~with_macos_security_framework:true
    ~opam:"tezos-stdlib"
    ~modes:[Native; JS]
    ~deps:
      [
        octez_stdlib |> open_;
        alcotezt;
        bigstring;
        octez_test_helpers |> open_;
        qcheck_alcotest;
      ]
    ~js_compatible:true

let _octez_stdlib_test_unix =
  tezt
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
        alcotezt;
        bigstring;
        lwt_unix;
        octez_test_helpers |> open_;
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
    ~deps:[seqes; lwt; octez_lwt_result_stdlib_bare_functor_outputs]
    ~opam_with_test:Only_on_64_arch

let octez_lwt_result_stdlib_bare_structs =
  public_lib
    "tezos-lwt-result-stdlib.bare.structs"
    ~path:"src/lib_lwt_result_stdlib/bare/structs"
    ~internal_name:"bare_structs"
    ~js_compatible:true
    ~deps:[seqes; lwt; octez_lwt_result_stdlib_bare_sigs]
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
    ~opam:"tezos-lwt-result-stdlib"
    ~deps:
      [
        octez_lwt_result_stdlib |> open_;
        octez_lwt_result_stdlib_examples_traces;
        lwt_unix;
        alcotezt;
        qcheck_alcotest;
        octez_test_helpers |> open_;
      ]
    ~opam_with_test:Only_on_64_arch

let octez_error_monad =
  public_lib
    "tezos-error-monad"
    ~path:"src/lib_error_monad"
    ~synopsis:"Tezos: error monad"
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
    ~deps:[hacl_star; hacl_star_raw; ctypes_stubs_js]
    ~js_of_ocaml:
      [
        [
          S "javascript_files";
          G (Dune.of_atom_list (js_generated :: js_helper :: js_stubs));
        ];
      ]
    ~conflicts:[Conflicts.hacl_x25519]
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
    ~with_macos_security_framework:true
    ~bisect_ppx:No
    ~modules:["gen0"]
    ~deps:[compiler_libs_common]

let _octez_hacl_gen =
  private_exe
    "gen"
    ~path:"src/lib_hacl/gen/"
    ~opam:"tezos-hacl"
    ~bisect_ppx:No
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
    ~opam:"tezos-hacl"
    ~deps:
      [
        octez_stdlib |> open_;
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        octez_lwt_result_stdlib |> open_;
        zarith;
        zarith_stubs_js;
        data_encoding |> open_;
        octez_hacl |> open_;
        qcheck_alcotest;
        alcotezt;
        octez_test_helpers |> open_;
      ]
    ~modes:[Native; JS]
    ~js_compatible:true

let _octez_error_monad_tests =
  tezt
    ["test_registration"; "test_splitted_error_encoding"]
    ~path:"src/lib_error_monad/test"
    ~with_macos_security_framework:true
    ~opam:"tezos-error-monad"
    ~modes:[Native; JS]
    ~deps:[octez_error_monad |> open_; data_encoding; alcotezt]
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

let octez_bls12_381_signature =
  public_lib
    "octez-bls12-381-signature"
    ~path:"src/lib_bls12_381_signature"
    ~internal_name:"bls12_381_signature"
    ~synopsis:
      "Implementation of BLS signatures for the pairing-friendly curve \
       BLS12-381"
    ~deps:[bls12_381]
    ~modules:["bls12_381_signature"]
    ~js_compatible:true
    ~foreign_stubs:
      {
        language = C;
        flags = ["-Wall"; "-Wextra"; ":standard"];
        names = ["blst_bindings_stubs"];
      }
    ~c_library_flags:["-Wall"; "-Wextra"; ":standard"; "-lpthread"]
    ~js_of_ocaml:[[S "javascript_files"; S "blst_bindings_stubs.js"]]
    ~linkall:true
    ~dune:
      Dune.
        [
          targets_rule
            ["needed-wasm-names"]
            ~promote:true
            ~action:
              [
                S "with-outputs-to";
                S "%{targets}";
                [S "run"; S "./gen_wasm_needed_names.exe"; S "%{files}"];
              ]
            ~deps:[[S ":files"; S "blst_bindings_stubs.js"]];
        ]

(* TODO: dep_globs aren't added to the rules for JS tests *)
let _octez_bls12_381_signature_tests =
  tezt
    ["test_aggregated_signature"; "test_signature"; "utils"]
    ~path:"src/lib_bls12_381_signature/test"
    ~opam:"octez-bls12-381-signature"
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5377
         This test is affected by the [FinalizationRegistry] hangs in JS,
         so although JS compatible, we only test in [Native] mode *)
    ~modes:[Native]
    ~deps:[bls12_381; octez_bls12_381_signature; alcotezt; integers_stubs_js]
    ~dep_globs_rec:["test_vectors/*"] (* See above *)
    ~js_compatible:false

let _octez_bls12_381_signature_gen_wasm_needed_names =
  private_exe
    "gen_wasm_needed_names"
    ~path:"src/lib_bls12_381_signature"
    ~opam:"octez-bls12-381-signature"
    ~bisect_ppx:No
    ~modules:["gen_wasm_needed_names"]
    ~deps:[re]

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
        octez_rpc;
        aches;
        zarith;
        zarith_stubs_js;
        bls12_381;
        octez_bls12_381_signature;
      ]
    ~js_compatible:true

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
    ]
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
        alcotezt;
        qcheck_alcotest;
        octez_test_helpers |> open_;
      ]
    ~modes:[Native; JS]
    ~js_compatible:true

let _octez_crypto_tests_unix =
  tezt
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
        alcotezt;
        lwt_unix;
        qcheck_alcotest;
        octez_test_helpers |> open_;
      ]

let octez_bls12_381_hash =
  public_lib
    "octez-bls12-381-hash"
    ~path:"src/lib_bls12_381_hash"
    ~internal_name:"bls12_381_hash"
    ~synopsis:
      "Implementation of some cryptographic hash primitives using the scalar \
       field of BLS12-381"
    ~c_library_flags:["-Wall"; "-Wextra"; ":standard"; "-lpthread"]
    ~deps:[bls12_381; bisect_ppx]
    ~js_compatible:false
    ~foreign_stubs:
      {
        language = C;
        flags = [];
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
    ~opam:"octez-bls12-381-hash"
    ~deps:[alcotezt; bls12_381; octez_bls12_381_hash]
    ~flags:(Flags.standard ~disable_warnings:[3] ())

let octez_mec =
  public_lib
    "octez-mec"
    ~path:"src/lib_mec"
    ~internal_name:"mec"
    ~synopsis:"Modular Experimental Cryptography library"
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
    ~opam:"octez-mec"
    ~deps:[alcotezt; octez_mec |> open_]

let octez_polynomial =
  public_lib
    "octez-polynomial"
    ~path:"src/lib_polynomial"
    ~internal_name:"polynomial"
    ~synopsis:"Polynomials over finite fields"
    ~deps:[bls12_381; bisect_ppx; zarith]

let _octez_polynomial_tests =
  tezt
    ["test_with_finite_field"; "test_utils"; "polynomial_pbt"]
    ~path:"src/lib_polynomial/test"
    ~opam:"octez-polynomial"
    ~deps:[bls12_381; octez_mec; alcotezt; octez_polynomial]

let octez_bls12_381_polynomial =
  public_lib
    "octez-bls12-381-polynomial"
    ~path:"src/lib_bls12_381_polynomial"
    ~synopsis:
      "Polynomials over BLS12-381 finite field - Temporary vendored version of \
       Octez"
    ~c_library_flags:["-Wall"; "-Wextra"; ":standard"]
    ~preprocess:[pps ppx_repr]
    ~deps:[bls12_381; ppx_repr; bigstringaf]
    ~js_compatible:false
    ~foreign_stubs:
      {
        language = C;
        flags = [];
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
    ~opam:"octez-bls12-381-polynomial"
    ~deps:
      [
        alcotezt;
        qcheck_alcotest;
        octez_polynomial;
        bisect_ppx;
        bls12_381;
        octez_bls12_381_polynomial;
      ]
    ~dep_files:["srs_zcash_g1_5"]

let octez_srs_extraction =
  public_lib
    "octez-srs-extraction"
    ~path:"src/lib_srs_extraction"
    ~synopsis:
      "Extracts SRS for G1 and G2 from powers-of-tau generated by the ZCash and\n\
       Filecoin MPC ceremonies"
    ~modules:["libsrs"]
    ~bisect_ppx:No
    ~deps:[bls12_381; octez_bls12_381_polynomial |> open_]

let _octez_srs_extraction_main =
  private_exe
    "srs_extraction_main"
    ~path:"src/lib_srs_extraction"
    ~opam:"octez-srs-extraction"
    ~modules:["srs_extraction_main"]
    ~bisect_ppx:No
    ~deps:
      [
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
    ~opam:"octez-srs-extraction"
    ~deps:[octez_srs_extraction |> open_; alcotezt]
    ~dep_files:["phase1radix2m5"]
    ~dune:
      Dune.(
        let extract curve srs_file =
          let generated_srs_file = srs_file ^ ".generated" in
          [
            S "rule";
            [S "alias"; S "runtest"];
            [S "package"; S "octez-srs-extraction"];
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
            ~package:"octez-srs-extraction"
            ~deps:["srs_zcash_g1_5"; "srs_zcash_g2_5"]
            ~action:
              (run
                 "../srs_extraction_main.exe"
                 ["check"; "srs_zcash_g1_5"; "srs_zcash_g2_5"]);
          alias_rule
            "runtest"
            ~package:"octez-srs-extraction"
            ~deps:["srs_filecoin_g1_6"; "srs_filecoin_g2_6"]
            ~action:
              (run
                 "../srs_extraction_main.exe"
                 ["check"; "srs_filecoin_g1_6"; "srs_filecoin_g2_6"]);
        ])

let octez_plompiler =
  public_lib
    "octez-plompiler"
    ~internal_name:"plompiler"
    ~path:"src/lib_plompiler"
    ~synopsis:"Library to write arithmetic circuits for Plonk"
    ~deps:
      [
        repr;
        stdint;
        hacl_star;
        octez_bls12_381_hash;
        octez_polynomial;
        octez_mec;
      ]
    ~preprocess:[staged_pps [ppx_repr; ppx_deriving_show]]

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
             ]);
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
  public_lib
    "octez-plonk"
    ~internal_name:"plonk"
    ~path:"src/lib_plonk"
    ~synopsis:"Plonk zero-knowledge proving system"
    ~deps:
      [
        repr;
        hacl_star;
        data_encoding;
        octez_bls12_381_polynomial |> open_;
        octez_plompiler |> open_;
        str;
      ]
    ~preprocess:[pps ppx_repr]

let octez_plonk_aggregation =
  public_lib
    "octez-plonk.aggregation"
    ~path:"src/lib_aplonk/plonk-aggregation"
    ~internal_name:"aggregation"
    ~preprocess:[pps ppx_repr]
    ~deps:[octez_plonk; octez_bls12_381_polynomial |> open_]

let octez_aplonk =
  public_lib
    "octez-aplonk"
    ~internal_name:"aplonk"
    ~path:"src/lib_aplonk"
    ~synopsis:
      "Zero-knowledge proving system based on PlonK optimized for proofs \
       aggregation"
    ~preprocess:[pps ppx_repr]
    ~deps:[octez_plonk_aggregation]

let octez_plonk_distribution =
  public_lib
    "octez-plonk.distribution"
    ~internal_name:"distribution"
    ~path:"src/lib_distributed_plonk/distribution"
    ~deps:[octez_plonk; octez_plonk_aggregation]
    ~preprocess:[pps ppx_repr]

let octez_plonk_communication =
  public_lib
    "octez-plonk.communication"
    ~internal_name:"communication"
    ~path:"src/lib_distributed_plonk/communication"
    ~deps:[logs; distributed_internal_lwt; octez_plonk_distribution |> open_]
    ~preprocess:[pps ppx_repr]

let octez_plonk_test_helpers =
  public_lib
    "octez-plonk.plonk-test"
    ~path:"src/lib_plonk/test"
    ~internal_name:"plonk_test"
    ~deps:[octez_plonk; octez_plonk_aggregation; octez_plonk_distribution]
    ~modules:["helpers"; "cases"]
    ~preprocess:[pps ppx_repr]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-plonk")

let _octez_plonk_test_helpers_main =
  private_exe
    "main"
    ~path:"src/lib_plonk/test"
    ~opam:"octez-plonk"
    ~modules:
      [
        "main";
        "test_circuit";
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
        octez_plonk_test_helpers;
        qcheck_alcotest;
        octez_bls12_381_polynomial |> open_;
      ]

let _octez_plonk_distribution_test =
  private_exe
    "main"
    ~path:"src/lib_distributed_plonk/distribution/test"
    ~opam:"octez-plonk"
    ~deps:[octez_plonk_aggregation; octez_plonk_test_helpers]
    ~modules:["main"; "test_polynomial_commitment"]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-plonk")

let _octez_plonk_test_helpers_bench =
  private_exe
    "bench"
    ~path:"src/lib_plonk/test"
    ~opam:"octez-plonk"
    ~modules:["bench"]
    ~bisect_ppx:No
    ~deps:[octez_plonk_test_helpers]

let _octez_plonk_test_plompiler_afl =
  private_exe
    "afl"
    ~path:"src/lib_plonk/test_plompiler"
    ~opam:"octez-plonk"
    ~modules:["afl"]
    ~bisect_ppx:No
    ~deps:[octez_plompiler; octez_plonk; bls12_381]

let _octez_plonk_test_plompiler_main =
  private_exe
    "main"
    ~path:"src/lib_plonk/test_plompiler"
    ~opam:"octez-plonk"
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
        "test_serialization";
        "test_weierstrass";
        "test_utils";
      ]
    ~bisect_ppx:No
    ~deps:[octez_plonk_test_helpers]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-plonk")

let octez_distributed_plonk =
  public_lib
    "octez-distributed-plonk"
    ~internal_name:"distributed_plonk"
    ~path:"src/lib_distributed_plonk"
    ~synopsis:"Distributed version of the proving system"
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
    ~preprocess:[pps ppx_repr]
    ~bisect_ppx:Yes

let _octez_distributed_plonk_test_main =
  private_exe
    "main"
    ~opam:"octez-distributed-plonk"
    ~path:"src/lib_distributed_plonk/test"
    ~deps:
      [
        octez_distributed_plonk;
        octez_plonk;
        octez_plonk_aggregation;
        octez_plonk_distribution;
        octez_aplonk;
        octez_plonk_test_helpers;
      ]
    ~bisect_ppx:No
    ~dune:(make_plonk_runtest_invocation ~package:"octez-distributed-plonk")

let _octez_distributed_plonk_worker_runner =
  private_exe
    "worker_runner"
    ~path:"src/lib_distributed_plonk"
    ~opam:"octez-distributed-plonk"
    ~bisect_ppx:No
    ~deps:[octez_distributed_plonk; octez_plonk_distribution]
    ~modules:["worker_runner"]

let _octez_aplonk_test_main =
  private_exe
    "main"
    ~path:"src/lib_aplonk/test"
    ~opam:"octez-aplonk"
    ~deps:[octez_plonk_test_helpers; octez_aplonk]
    ~modules:["main"; "test_aplonk"; "test_main_protocol"]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-aplonk")

let _octez_distributed_plonk_executable =
  private_exe
    "distribution"
    ~path:"src/lib_distributed_plonk"
    ~opam:"octez-distributed-plonk"
    ~bisect_ppx:No
    ~deps:[octez_distributed_plonk |> open_]
    ~modules:["distribution"]

let _octez_distributed_plonk_executable_meta =
  private_exe
    "distribution_meta"
    ~path:"src/lib_distributed_plonk"
    ~opam:"octez-distributed-plonk"
    ~bisect_ppx:No
    ~deps:[octez_distributed_plonk |> open_]
    ~modules:["distribution_meta"]

let _octez_aplonk_test_helpers_bench =
  private_exe
    "bench"
    ~path:"src/lib_aplonk/test"
    ~opam:"octez-aplonk"
    ~modules:["bench"]
    ~bisect_ppx:No
    ~deps:[octez_plonk_test_helpers; octez_aplonk]

let octez_epoxy_tx =
  public_lib
    "octez-epoxy-tx"
    ~path:"src/lib_epoxy_tx"
    ~synopsis:"Circuits for transaction Epoxy rollup"
    ~internal_name:"epoxy_tx"
    ~deps:[octez_plompiler; hex; stdint; octez_plonk; octez_mec]

let _octez_epoxy_tx_tests =
  private_exe
    "main"
    ~path:"src/lib_epoxy_tx/test"
    ~opam:"octez-epoxy-tx"
    ~deps:[octez_epoxy_tx; octez_plonk_test_helpers; octez_aplonk]
    ~dune:(make_plonk_runtest_invocation ~package:"octez-epoxy-tx")

let octez_dal_config =
  public_lib
    "tezos-crypto-dal.octez-dal-config"
    ~path:"src/lib_crypto_dal/dal_config"
    ~deps:[data_encoding |> open_]
    ~js_compatible:true

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
        octez_dal_config |> open_;
        octez_crypto;
        octez_bls12_381_polynomial;
        lwt_unix;
      ]

let _octez_crypto_dal_tests =
  tezt
    ["test_dal_cryptobox"]
    ~path:"src/lib_crypto_dal/test"
    ~opam:"tezos-crypto-dal"
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
        octez_test_helpers;
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
        octez_test_helpers |> open_;
        alcotezt;
      ]
    ~js_compatible:true
    ~linkall:true
    ~bisect_ppx:No

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
        aches_lwt;
        lwt_unix;
        ipaddr_unix;
        re;
        ezjsonm;
        ptime;
        ptime_clock_os;
        mtime;
        mtime_clock_os;
        conf_libev;
        uri;
      ]

let _octez_stdlib_unix_test =
  tezt
    ["test_key_value_store_fuzzy"; "test_log_config_rules"]
    ~path:"src/lib_stdlib_unix/test/"
    ~opam:"tezos-stdlib-unix"
    ~deps:
      [
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_stdlib_unix |> open_;
        octez_event_logging |> open_;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
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
  tezt
    ["test_clic"]
    ~path:"src/lib_clic/test"
    ~opam:"tezos-clic"
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
        octez_crypto;
        data_encoding |> open_;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_rpc;
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
    ~dune:Dune.[ocamllex "point_parser"]

let octez_base_unix =
  public_lib
    "tezos-base.unix"
    ~path:"src/lib_base/unix"
    ~deps:
      [
        octez_error_monad |> open_;
        octez_crypto;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_hacl;
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        data_encoding |> open_;
        uri;
        octez_event_logging |> open_;
      ]
    ~inline_tests:ppx_expect

let octez_base_p2p_identity_file =
  public_lib
    "tezos-base.p2p-identity-file"
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
    ]
    ~path:"src/lib_base/test"
    ~opam:"tezos-base"
    ~deps:
      [
        octez_base |> open_;
        octez_error_monad |> open_;
        data_encoding;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
      ]
    ~dep_files:
      [
        (* Note: those files are only actually needed by test_p2p_addr. *)
        "points.ok";
        "points.ko";
      ]
    ~modes:[Native; JS]
    ~js_compatible:true

let _octez_base_unix_tests =
  tezt
    ["test_unix_error"; "test_syslog"]
    ~path:"src/lib_base/unix/test"
    ~with_macos_security_framework:true
    ~opam:"tezos-base"
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
        octez_test_helpers |> open_;
        alcotezt;
        qcheck_alcotest;
      ]
    ~linkall:true
    ~bisect_ppx:No
    ~release_status:Released

let octez_context_sigs =
  public_lib
    "tezos-context.sigs"
    ~path:"src/lib_context/sigs"
    ~opam:"tezos-context"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_stdlib |> open_]
    ~js_compatible:true

let tree_encoding =
  public_lib
    "tezos-tree-encoding"
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
  public_lib
    "tezos-lazy-containers"
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
  public_lib
    "tezos-webassembly-interpreter"
    ~path:"src/lib_webassembly"
    ~license:"Apache-2.0"
    ~extra_authors:["WebAssembly Authors"]
    ~synopsis:"WebAssembly reference interpreter with tweaks for Tezos"
    ~dune:Dune.[[S "include_subdirs"; S "unqualified"]]
    ~deps:
      [
        octez_lwt_result_stdlib;
        octez_stdlib;
        octez_error_monad;
        zarith;
        lazy_containers |> open_;
      ]
    ~preprocess:[pps ppx_deriving_show]

let octez_webassembly_interpreter_extra =
  public_lib
    "tezos-webassembly-interpreter-extra"
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
    ~opam:"tezos-webassembly-interpreter"
    ~dune:Dune.[[S "include_subdirs"; S "no"]]
    ~deps:[octez_webassembly_interpreter |> open_; alcotezt]

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

let octez_version_value =
  public_lib
    "tezos-version.value"
    ~path:"src/lib_version/value/"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_version;
        octez_version_parser;
      ]
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
            ~action:[S "run"; S "../exe/get_git_info.exe"];
        ]

let _octez_version_get_git_info =
  private_exe
    "get_git_info"
    ~path:"src/lib_version/exe"
    ~opam:"tezos-version"
    ~deps:[dune_configurator; octez_version_parser]
    ~modules:["get_git_info"]
    ~bisect_ppx:No

let _octez_print_version_exe =
  public_exe
    "tezos-version"
    ~internal_name:"tezos_print_version"
    ~path:"src/lib_version/exe"
    ~opam:"tezos-version"
    ~deps:
      [octez_version_value |> open_; octez_version |> open_; octez_base_unix]
    ~modules:["tezos_print_version"]
    ~bisect_ppx:No

let _octez_version_tests =
  tezt
    ["test_parser"]
    ~path:"src/lib_version/test"
    ~opam:"tezos-version"
    ~js_compatible:true
    ~modes:[Native; JS]
    ~deps:[octez_version |> open_; octez_version_parser; alcotezt]

let octez_p2p_services =
  public_lib
    "tezos-p2p-services"
    ~path:"src/lib_p2p_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-p2p`"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_rpc]
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
      ]

let _octez_workers_tests =
  tezt
    ["mocked_worker"; "test_workers_unit"]
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
        alcotezt;
      ]

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
        octez_rpc;
        octez_p2p_services |> open_;
        octez_version |> open_;
        octez_context_sigs;
        octez_merkle_proof_encoding;
        octez_dal_config |> open_;
      ]
    ~linkall:true
    ~js_compatible:true

let _octez_shell_services_tests =
  tezt
    ["test_block_services"]
    ~path:"src/lib_shell_services/test"
    ~opam:"tezos-shell-services"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_shell_services |> open_;
        alcotezt;
      ]
    ~modes:[Native; JS]
    ~js_compatible:true

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
        aches;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_stdlib |> open_;
        octez_p2p_services |> open_;
        octez_version;
        prometheus;
        octez_base_p2p_identity_file |> open_;
      ]

let tezt_performance_regression =
  public_lib
    "tezt-performance-regression"
    ~path:"tezt/lib_performance_regression"
    ~synopsis:"Performance regression test framework based on Tezt"
    ~bisect_ppx:No
    ~deps:[tezt_lib |> open_ |> open_ ~m:"Base"; uri; cohttp_lwt_unix]

let tezt_tezos =
  public_lib
    "tezt-tezos"
    ~path:"tezt/lib_tezos"
    ~synopsis:"Tezos test framework based on Tezt"
    ~bisect_ppx:No
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
    ~release_status:Released

let tezt_ethereum =
  private_lib
    "tezt_ethereum"
    ~path:"tezt/lib_ethereum"
    ~opam:"tezt-ethereum"
    ~synopsis:"Ethereum test framework based on Tezt"
    ~bisect_ppx:No
    ~deps:
      [
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_performance_regression |> open_;
      ]
    ~release_status:Unreleased

let _tezt_self_tests =
  tezt
    ["test_michelson_script"; "test_daemon"]
    ~opam:"tezt-tezos"
    ~path:"tezt/self_tests"
    ~deps:[tezt_lib |> open_ |> open_ ~m:"Base"; tezt_tezos |> open_]

let octez_p2p_test_common =
  private_lib
    "tezos_p2p_test_common"
    ~path:"src/lib_p2p/test/common"
    ~opam:"tezos-p2p"
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
    ["test_p2p_socket"; "test_p2p_conn"]
    ~path:"src/lib_p2p/tezt"
    ~opam:"tezos-p2p"
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
      "test_p2p_pool";
      "test_p2p_broadcast";
      "test_p2p_io_scheduler";
      "test_p2p_peerset";
      "test_p2p_buffer_reader";
      "test_p2p_banned_peers";
      "test_p2p_node";
      "test_p2p_connect_handler";
      "test_p2p_maintenance";
    ]
    ~bisect_ppx:With_sigterm
    ~path:"src/lib_p2p/test"
    ~opam:"tezos-p2p"
    ~locks:"/locks/p2p"
    ~deps:
      [
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
        alcotezt;
        astring;
      ]

let octez_gossipsub =
  public_lib
    "tezos-gossipsub"
    ~path:"src/lib_gossipsub"
    ~synopsis:"Tezos: An implementation of gossipsub"
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
      ]

let _octez_gossipsub_test =
  test
    "test_gossipsub"
    ~path:"src/lib_gossipsub/test"
    ~opam:"tezos-gossipsub-test"
    ~synopsis:"Tests for the gossipsub algorithm"
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
  public_lib
    "tezos-wasmer"
    ~path:"src/lib_wasmer"
    ~synopsis:"Wasmer bindings for SCORU WASM"
    ~deps:[ctypes; ctypes_foreign; lwt; lwt_unix; tezos_rust_lib]
    ~preprocess:[pps ppx_deriving_show]
    ~flags:(Flags.standard ~disable_warnings:[9; 27] ())
    ~ctypes:
      Ctypes.
        {
          external_library_name = "wasmer";
          include_header = "wasmer.h";
          extra_search_dir = "%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs";
          type_description = {instance = "Types"; functor_ = "Api_types_desc"};
          function_description =
            {instance = "Functions"; functor_ = "Api_funcs_desc"};
          generated_types = "Api_types";
          generated_entry_point = "Api";
          c_flags = ["-Wno-incompatible-pointer-types"];
          c_library_flags = [];
        }

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
    ~conflicts:[Conflicts.checkseum]

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
    ~conflicts:[Conflicts.checkseum]

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
    ~conflicts:[Conflicts.checkseum]

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
        octez_context_memory;
        octez_lwt_result_stdlib;
        data_encoding;
      ]

let octez_scoru_wasm_fast =
  public_lib
    "tezos-scoru-wasm-fast"
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

let octez_context_dump =
  public_lib
    "tezos-context.dump"
    ~path:"src/lib_context/dump"
    ~deps:
      [octez_base |> open_ ~m:"TzPervasives"; octez_stdlib_unix |> open_; fmt]

let octez_context_disk =
  public_lib
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
        octez_context_dump;
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

let octez_context =
  public_lib
    "tezos-context"
    ~path:"src/lib_context"
    ~synopsis:"Tezos: on-disk context abstraction for `octez-node`"
    ~deps:[octez_context_disk; octez_context_memory]

let _octez_context_tests =
  tezt
    ["test_context"; "test_merkle_proof"]
    ~path:"src/lib_context/test"
    ~opam:"tezos-context"
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
        qcheck_alcotest;
        alcotezt;
      ]

let _octez_context_memory_tests =
  tezt
    ["test"]
    ~path:"src/lib_context/memory/test"
    ~opam:"tezos-context"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_context_disk;
        octez_context_memory;
        octez_stdlib_unix |> open_;
        alcotezt;
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
        octez_crypto;
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        tezos_rust_lib;
        tezos_sapling_parameters;
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
    ~opam:"tezos-sapling"
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
    ~bisect_ppx:No
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
       let latest_environment_number = 10 in
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
        octez_plonk |> open_;
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
    ~ocaml:
      V.(
        (* Should be in sync with scripts/version.sh *)
        at_least "4.14.0" && less_than "4.15")
    ~deps:
      [
        zarith;
        zarith_stubs_js;
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
        octez_scoru_wasm;
        octez_event_logging;
      ]

let octez_shell_context =
  public_lib
    "tezos-shell-context"
    ~path:"src/lib_protocol_environment/shell_context"
    ~synopsis:
      "Tezos: economic-protocols environment implementation for `octez-node`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_protocol_environment;
        octez_context;
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
    ~opam:"tezos-protocol-environment"
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
  tezt
    ["test_proxy_context"]
    ~path:"src/lib_protocol_environment/test_shell_context"
    ~opam:"tezos-shell-context-test"
    ~synopsis:"Testing the Shell Context"
    ~deps:
      [
        octez_shell_context;
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

let _octez_protocol_compiler_cmis_of_cma =
  private_exe
    "cmis_of_cma"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"octez-protocol-compiler"
    ~deps:[compiler_libs_common]
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
    "octez-protocol-compiler.native"
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
            ~package:"octez-protocol-compiler"
            ~section:"libexec";
        ]

let octez_protocol_updater =
  public_lib
    "tezos-protocol-updater"
    ~path:"src/lib_protocol_updater"
    ~synopsis:"Tezos: economic-protocol dynamic loading for `octez-node`"
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
    ~synopsis:"Tezos: library for block validation"
    ~time_measurement_ppx:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_crypto |> open_;
        octez_rpc;
        octez_context |> open_;
        octez_context_ops |> open_;
        octez_shell_context |> open_;
        octez_shell_services |> open_;
        octez_protocol_updater |> open_;
        octez_stdlib_unix |> open_;
        octez_version_value;
      ]

let octez_store_shared =
  public_lib
    "tezos-store.shared"
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
  public_lib
    "tezos-store.unix-reconstruction"
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
  public_lib
    "tezos-store.unix-snapshots"
    ~path:"src/lib_store/unix"
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_stdlib_unix |> open_;
        octez_crypto |> open_;
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
    ~synopsis:"Tezos: store for `octez-node`"
    ~description:
      {|This library provides abstraction for storing and iterating over blocks.
tezos-store is a virtual library that provides two implementations:
- tezos-store.real is the default implementation, used in production
- tezos-store.mocked is used for testing purposes.|}
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
        octez_crypto |> open_;
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
  tezt
    ["test_requester"; "test_fuzzing_requester"; "shared"]
    ~path:"src/lib_requester/test"
    ~opam:"tezos-requester"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        octez_stdlib |> open_;
        octez_stdlib_unix;
        octez_requester |> open_;
        alcotezt;
        qcheck_alcotest;
      ]

let octez_shell =
  public_lib
    "tezos-shell"
    ~path:"src/lib_shell"
    ~synopsis:
      "Tezos: core of `octez-node` (gossip, validation scheduling, mempool, \
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
        octez_rpc;
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
        octez_dal_config |> open_;
        lwt_exit;
      ]

let octez_rpc_http =
  public_lib
    "tezos-rpc-http"
    ~path:"src/lib_rpc_http"
    ~synopsis:"Tezos: library of auto-documented RPCs (http server and client)"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_rpc; resto_cohttp; uri]
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
        octez_rpc;
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
        octez_rpc;
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
        cohttp_lwt_unix;
        resto_cohttp_server;
        resto_acl;
        octez_rpc;
        octez_rpc_http |> open_;
      ]
    ~modules:["RPC_server"; "RPC_middleware"]

let _octez_rpc_http_server_tests =
  tezt
    ["test_rpc_http"]
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
        alcotezt;
      ]

let octez_client_base =
  public_lib
    "tezos-client-base"
    ~path:"src/lib_client_base"
    ~synopsis:"Tezos: common helpers for `tezos-client`"
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
  tezt
    ["bip39_tests"; "pbkdf_tests"]
    ~path:"src/lib_client_base/test"
    ~opam:"tezos-client-base"
    ~with_macos_security_framework:true
    ~deps:[octez_base; octez_client_base |> open_; alcotezt]
    ~js_compatible:true
    ~modes:[Native; JS]

let _bip39_generator =
  private_exe
    "bip39_generator"
    ~path:"src/lib_client_base/gen"
    ~opam:"tezos-client-base"
    ~bisect_ppx:No

let octez_signer_services =
  public_lib
    "tezos-signer-services"
    ~path:"src/lib_signer_services"
    ~synopsis:"Tezos: descriptions of RPCs exported by `tezos-signer`"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc;
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
  tezt
    ["test_encrypted"]
    ~path:"src/lib_signer_backends/test"
    ~opam:"tezos-signer-backends"
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
  public_lib
    "tezos-signer-backends.unix"
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
    ~opam:"tezos-signer-backends"
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
  public_lib
    "tezos-client-commands"
    ~path:"src/lib_client_commands"
    ~synopsis:"Tezos: protocol agnostic commands for `tezos-client`"
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
    ~opam:"tezos-mockup"
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
  public_lib
    "tezos-proxy"
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
    ~opam:"tezos-proxy"
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
    ~opam:"tezos-proxy"
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

let octez_proxy_server_config =
  public_lib
    "tezos-proxy-server-config"
    ~path:"src/lib_proxy_server_config"
    ~synopsis:"Tezos: proxy server configuration"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_stdlib_unix; uri]

let _octez_proxy_server_config_tests =
  tezt
    ["test_proxy_server_config"]
    ~path:"src/lib_proxy_server_config/test"
    ~opam:"tezos-proxy-server-config"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_proxy_server_config;
        octez_test_helpers |> open_;
        qcheck_alcotest;
        alcotezt;
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
    ~opam:"tezos-client-base-unix"
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
      {language = C; flags = [":standard"]; names = ["snoop_stubs"]}
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
        ocaml_migrate_parsetree;
        opam_only "hashcons" V.True;
      ]
    ~inline_tests:ppx_expect

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
        zarith_stubs_js;
        octez_stdlib |> open_;
        octez_crypto;
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
        octez_crypto;
        octez_context;
        octez_shell_context;
        octez_micheline;
      ]
    ~linkall:true

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
        tezt_lib |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_ethereum |> open_;
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
    "octez-protocol-compiler"
    ~path:"src/lib_protocol_compiler/bin"
    ~opam:"octez-protocol-compiler"
    ~internal_name:"main_native"
    ~modes:[Native]
    ~deps:[octez_protocol_compiler_native; octez_version_value]
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
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_stdlib_unix |> open_;
      ]
    ~linkall:true
    ~modules:["Main_embedded_packer"]

let octez_layer2_store =
  private_lib
    "tezos_layer2_store"
    ~path:"src/lib_layer2_store"
    ~opam:"tezos-layer2-store"
    ~synopsis:"Tezos: layer2 storage utils"
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
      ]
    ~linkall:true
    ~conflicts:[Conflicts.checkseum]

let _octez_layer2_indexed_store_test =
  tezt
    ["test_indexed_store"]
    ~path:"src/lib_layer2_store/test/"
    ~opam:"tezos-layer2-store"
    ~deps:
      [
        octez_error_monad |> open_ |> open_ ~m:"TzLwtreslib";
        octez_layer2_store |> open_;
        qcheck_alcotest;
        alcotezt;
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
      ]
    ~linkall:true

let octez_dal_node_lib =
  public_lib
    "tezos-dal-node-lib"
    ~path:"src/lib_dal_node"
    ~opam:"tezos-dal-node-lib"
    ~synopsis:"Tezos: `tezos-dal-node` library"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_dal_node_services;
        octez_client_base |> open_;
        octez_protocol_updater |> open_;
        octez_client_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_crypto_dal |> open_;
        octez_p2p |> open_;
        octez_p2p_services |> open_;
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
        octez_crypto |> open_;
      ]

let octez_dac_lib =
  public_lib
    "tezos-dac-lib"
    ~path:"src/lib_dac"
    ~opam:"tezos-dac-lib"
    ~synopsis:"Tezos: `tezos-dac` library"
    ~deps:
      [octez_base |> open_ ~m:"TzPervasives"; octez_protocol_updater |> open_]

let octez_dac_client_lib =
  public_lib
    "tezos-dac-client-lib"
    ~path:"src/lib_dac_client"
    ~opam:"tezos-dac-client-lib"
    ~synopsis:"Tezos: `tezos-dac-client` library"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_dac_lib |> open_;
      ]

let octez_dac_node_lib =
  private_lib
    "tezos_dac_node_lib"
    ~path:"src/lib_dac_node"
    ~opam:"tezos-dac-node-lib"
    ~synopsis:"Tezos: `tezos-dac-node` library"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_layer2_store |> open_;
        octez_rpc_http_server;
        octez_dac_lib |> open_;
        octez_dac_client_lib |> open_;
      ]

let _octez_dac_node_lib_tests =
  tezt
    ["test_data_streamer"]
    ~path:"src/lib_dac_node/test"
    ~opam:"tezos-dac-node-lib-test"
    ~synopsis:"Test for dac node lib"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        octez_dac_node_lib |> open_;
        alcotezt;
      ]

let _octez_dac_lib_tests =
  tezt
    ["test_certificate"; "test_dac_plugin"]
    ~path:"src/lib_dac/test"
    ~opam:"tezos-dac-lib-test"
    ~synopsis:"Test for dac lib"
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_stdlib |> open_;
        octez_stdlib_unix |> open_;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_test_helpers |> open_;
        octez_base_test_helpers |> open_;
        octez_dac_lib |> open_;
        alcotezt;
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
        octez_context |> open_;
        octez_store |> open_;
        octez_validation |> open_;
      ]

let octez_crawler =
  public_lib
    "octez-crawler"
    ~internal_name:"octez_crawler"
    ~path:"src/lib_crawler"
    ~synopsis:"Octez: library to crawl blocks of the L1 chain"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives"
        |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
        |> open_;
        octez_rpc_http |> open_;
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_client_base |> open_;
        octez_shell;
      ]

let octez_injector =
  public_lib
    "octez-injector"
    ~path:"src/lib_injector"
    ~synopsis:"Octez: library for building injectors"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives"
        |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
        |> open_;
        logs_lwt;
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_crypto;
        octez_micheline |> open_;
        octez_client_base |> open_;
        octez_workers |> open_;
        octez_shell;
        octez_crawler |> open_;
      ]

let octez_smart_rollup_lib =
  public_lib
    "octez-smart-rollup"
    ~path:"src/lib_smart_rollup"
    ~synopsis:"Octez: library for Smart Rollups "
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives"
        |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
        |> open_;
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_crypto |> open_;
        octez_crypto_dal;
      ]

let octez_smart_rollup_node_lib =
  public_lib
    "octez-smart-rollup-node"
    ~path:"src/lib_smart_rollup_node"
    ~synopsis:"Octez: library for Smart Rollup node"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_stdlib_unix |> open_;
        octez_crypto |> open_;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        cohttp_lwt_unix;
        octez_node_config;
        prometheus_app;
        octez_injector |> open_;
        octez_version_value |> open_;
        octez_layer2_store |> open_;
        octez_crawler |> open_;
        octez_smart_rollup_lib |> open_;
      ]

let octez_scoru_wasm_helpers =
  public_lib
    "tezos-scoru-wasm-helpers"
    ~path:"src/lib_scoru_wasm/helpers"
    ~opam:"tezos-scoru-wasm-helpers"
    ~synopsis:"Helpers for the smart rollup wasm functionality and debugger"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_context_disk;
        octez_scoru_wasm;
        octez_scoru_wasm_fast;
        octez_webassembly_interpreter_extra |> open_;
      ]
    ~preprocess:[staged_pps [ppx_import; ppx_deriving_show]]

let octez_scoru_wasm_durable_snapshot =
  private_lib
    "tezos_scoru_wasm_durable_snapshot"
    ~path:"src/lib_scoru_wasm/test/durable_snapshot"
    ~opam:"tezos-scoru-wasm-durable-snapshot"
    ~synopsis:"Durable storage reference implementation"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_webassembly_interpreter_extra |> open_;
      ]
    ~preprocess:[staged_pps [ppx_import; ppx_deriving_show]]

let octez_scoru_wasm_tests_helpers =
  private_lib
    "tezos_scoru_wasm_test_helpers"
    ~path:"src/lib_scoru_wasm/test/helpers"
    ~opam:"tezos-scoru-wasm-test-helpers"
    ~synopsis:"Helpers for test of the smart rollup wasm functionality"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_context_disk;
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
    ~preprocess:[staged_pps [ppx_import; ppx_deriving_show]]

let octez_scoru_wasm_benchmark =
  private_lib
    "octez_smart_rollup_wasm_benchmark_lib"
    ~path:"src/lib_scoru_wasm/bench"
    ~synopsis:"Smart Rollup WASM benchmark library"
    ~opam:"octez-smart-rollup-wasm-benchmark-lib"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tezt_lib;
        octez_webassembly_interpreter;
        octez_context_memory;
        octez_scoru_wasm;
        octez_scoru_wasm_helpers;
        lwt_unix;
      ]
    ~preprocess:[pps ppx_deriving_show]

let _octez_scoru_wasm_benchmark_exe =
  private_exe
    "octez_smart_rollup_wasm_benchmark"
    ~path:"src/lib_scoru_wasm/bench/executable"
    ~synopsis:"Smart rollup WASM benchmark library"
    ~opam:"octez-smart-rollup-wasm-benchmark"
    ~preprocess:[pps ppx_deriving_show]
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; octez_scoru_wasm_benchmark]

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
    ]
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
        octez_test_helpers |> open_;
        octez_scoru_wasm;
        qcheck_alcotest;
        alcotezt;
        octez_scoru_wasm_helpers |> open_;
        octez_scoru_wasm_tests_helpers |> open_;
        octez_webassembly_interpreter_extra |> open_;
      ]
    ~preprocess:[staged_pps [ppx_import; ppx_deriving_show]]

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
    ~opam:"tezos-scoru-wasm-fast-test"
    ~synopsis:"Tests for the scoru-wasm-fast functionality"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tree_encoding;
        octez_base_unix;
        octez_context_disk;
        octez_base_test_helpers |> open_;
        octez_scoru_wasm_helpers |> open_;
        octez_scoru_wasm_tests_helpers |> open_;
        octez_test_helpers |> open_;
        octez_scoru_wasm;
        octez_scoru_wasm_fast;
        qcheck_alcotest;
        alcotezt;
      ]
    ~preprocess:[staged_pps [ppx_import; ppx_deriving_show]]

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

  val short_hash : t -> string

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

  val dac : t -> target option

  val parameters_exn : t -> target

  val benchmarks_proto_exn : t -> target

  val octez_sc_rollup_layer2 : t -> target option

  val octez_sc_rollup_node : t -> target option

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
      {short_hash = name; number; name_dash; name_underscore}

    let v name number = make name (V number)

    let alpha = make "alpha" Alpha

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
    dac : target option;
    test_helpers : target option;
    parameters : target option;
    benchmarks_proto : target option;
    baking : target option;
    octez_sc_rollup_layer2 : target option;
    octez_sc_rollup_node : target option;
  }

  let make ?client ?client_commands ?client_commands_registration
      ?baking_commands_registration ?plugin ?plugin_registerer ?dal ?dac
      ?test_helpers ?parameters ?benchmarks_proto ?octez_sc_rollup_layer2
      ?octez_sc_rollup_node ?baking ~status ~name ~main ~embedded () =
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
      dac;
      test_helpers;
      parameters;
      benchmarks_proto;
      baking;
      octez_sc_rollup_layer2;
      octez_sc_rollup_node;
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

  let dac p = p.dac

  let parameters_exn p = mandatory "parameters" p p.parameters

  let benchmarks_proto_exn p = mandatory "benchmarks_proto" p p.benchmarks_proto

  let baking_exn p = mandatory "baking" p p.baking

  let octez_sc_rollup_layer2 p = p.octez_sc_rollup_layer2

  let octez_sc_rollup_node p = p.octez_sc_rollup_node

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

    let ( == ) a b = compare_asymmetric a b == 0
  end

  let only_if condition make = if condition then Some (make ()) else None

  let conditional_list =
    List.filter_map (fun (x, b) -> if b then Some x else None)

  module Lib_protocol = struct
    type t = {main : target; lifted : target; embedded : target}

    let make_tests ?test_helpers ?parameters ?plugin ?client ?benchmark
        ?benchmark_type_inference ?octez_sc_rollup ~main ~name () =
      let name_dash = Name.name_dash name in
      let number = Name.number name in
      let path = Name.base_path name in
      let _integration_consensus =
        tezt
          [
            "test_baking";
            "test_consensus_key";
            "test_deactivation";
            "test_delegation";
            "test_double_baking";
            "test_double_endorsement";
            "test_double_preendorsement";
            "test_endorsement";
            "test_frozen_deposits";
            "test_helpers_rpcs";
            "test_participation";
            "test_preendorsement_functor";
            "test_preendorsement";
            "test_seed";
          ]
          ~path:(path // "lib_protocol/test/integration/consensus")
          ~with_macos_security_framework:true
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~deps:
            [
              alcotezt;
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
        tezt
          ["test_gas_costs"; "test_gas_levels"]
          ~path:(path // "lib_protocol/test/integration/gas")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              alcotezt;
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
            ("test_typechecking", true);
            ("test_lambda_normalization", N.(number >= 016));
          ]
          |> List.filter_map (fun (n, b) -> if b then Some n else None)
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
            ("test_tx_rollup", N.(number <= 016));
          ]
          |> List.filter_map (fun (n, b) -> if b then Some n else None)
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
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
              alcotezt;
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              main |> open_;
              qcheck_alcotest;
              client |> if_some |> open_;
              octez_test_helpers |> open_;
              test_helpers |> if_some |> open_;
              octez_base_test_helpers |> open_;
              plugin |> if_some |> open_;
            ]
      in
      let _integration =
        let modules =
          [
            ("test_constants", true);
            ("test_frozen_bonds", true);
            ("test_adaptive_inflation_launch", N.(number >= 018));
            ("test_liquidity_baking", true);
            ("test_storage_functions", true);
            ("test_storage", true);
            ("test_token", true);
          ]
          |> List.filter_map (fun (n, b) -> if b then Some n else None)
        in
        tezt
          modules
          ~path:(path // "lib_protocol/test/integration")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              (if N.(number >= 015) then Some tezt_lib else None) |> if_some;
              octez_context;
              alcotezt;
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
        let list =
          (* The first item of each tuple is the index N for the runtestN alias.
             Those aliases are used to split into multiple CI jobs. *)
          [
            (1, "liquidity_baking_pbt", true);
            (1, "saturation_fuzzing", true);
            (1, "test_merkle_list", N.(number >= 013));
            (1, "test_gas_properties", true);
            (2, "test_sampler", N.(number >= 012));
            (2, "test_script_comparison", true);
            (2, "test_tez_repr", true);
            (2, "test_tx_rollup_l2_encoding", N.(number >= 013 && number <= 016));
            (2, "test_bitset", N.(number >= 013));
            (2, "test_sc_rollup_tick_repr", N.(number >= 016));
            (2, "test_sc_rollup_encoding", N.(number >= 016));
            (2, "test_sc_rollup_inbox", N.(number >= 017));
            (3, "refutation_game_pbt", N.(number == 013));
            (3, "test_refutation_game", N.(number >= 016));
            (3, "test_carbonated_map", N.(number >= 013));
            (3, "test_zk_rollup_encoding", N.(number >= 015));
            (3, "test_dal_slot_proof", N.(number >= 016));
            (3, "test_compare_operations", N.(number >= 015));
            (3, "test_operation_encoding", N.(number >= 016));
            (3, "test_bytes_conversion", N.(number >= 016));
          ]
          |> List.filter_map (fun (i, n, b) -> if b then Some (i, n) else None)
        in
        tezt
          (List.map snd list)
          ~synopsis:"Tezos/Protocol: tests for economic-protocol definition"
          ~path:(path // "lib_protocol/test/pbt")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              octez_base
              |> if_ N.(number <= 14)
              |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
            ("test_merkle_list", true);
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
            ("test_tx_rollup_l2_apply", N.(number >= 015 && number <= 016));
            ("test_tx_rollup_l2", N.(number >= 015 && number <= 016));
            ("test_adaptive_inflation", N.(number >= 018));
            ("test_adaptive_inflation_ema", N.(number >= 018));
          ]
          |> List.filter_map (fun (n, b) -> if b then Some n else None)
        in
        tezt
          modules
          ~path:(path // "lib_protocol/test/unit")
          ~opam:(sf "tezos-protocol-%s-tests" name_dash)
          ~with_macos_security_framework:true
          ~deps:
            [
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
      let lifted =
        public_lib
          (sf "tezos-protocol-%s.lifted" name_dash)
          ~path:(path // "lib_protocol")
          ~opam:(sf "tezos-protocol-%s" name_dash)
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
            | Alpha | V _ ->
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
        public_lib
          (sf "tezos-embedded-protocol-%s" name_dash)
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
            | Alpha | V _ ->
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
            | V _, Not_mainnet | (Alpha | Other), _ ->
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
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
      | Alpha, _ -> Experimental
      | Other, _ -> Unreleased
    in
    let optional_library_release_status =
      match (number, status) with
      | V _, (Active | Frozen) ->
          (* Put explicit dependency in meta-package octez.opam to force the optional
             dependency to be installed. *)
          Released
      | V _, (Overridden | Not_mainnet) | (Alpha | Other), _ ->
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
        ~with_macos_security_framework:true
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
        ~bisect_ppx:No
    in
    let octez_sc_rollup =
      only_if N.(number >= 016) @@ fun () ->
      public_lib
        (sf "tezos-smart-rollup-%s" name_dash)
        ~path:(path // "lib_sc_rollup")
        ~synopsis:
          "Tezos/Protocol: protocol specific library of helpers for \
           `tezos-smart-rollup`"
        ~deps:[octez_base |> open_ ~m:"TzPervasives"; main |> open_]
        ~inline_tests:ppx_expect
        ~linkall:true
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
            octez_sc_rollup |> if_some |> if_ N.(number >= 016) |> open_;
          ]
        ~all_modules_except:["Plugin_registerer"]
        ~bisect_ppx:(if N.(number >= 008) then Yes else No)
    in
    let plugin_registerer =
      opt_map plugin @@ fun plugin ->
      public_lib
        (sf "tezos-protocol-plugin-%s-registerer" name_dash)
        ~path:(path // "lib_plugin")
        ~synopsis:"Tezos/Protocol: protocol plugin registerer"
        ~release_status:optional_library_release_status
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            embedded |> open_;
            plugin |> open_;
            octez_shell |> open_;
          ]
        ~modules:["Plugin_registerer"]
        ~bisect_ppx:(if N.(number >= 008) then Yes else No)
    in
    let client =
      only_if not_overridden @@ fun () ->
      public_lib
        (sf "tezos-client-%s" name_dash)
        ~path:(path // "lib_client")
        ~synopsis:"Tezos/Protocol: protocol specific library for `tezos-client`"
        ~release_status:optional_library_release_status
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
        ~linkall:true
    in
    let test_helpers =
      only_if active @@ fun () ->
      let name = sf "tezos-%s-test-helpers" name_dash in
      public_lib
        name
        ~path:
          (if active then path // "lib_protocol/test/helpers"
          else path // "lib_protocol")
        ~opam:name
        ~internal_name:(sf "tezos_%s_test_helpers" name_underscore)
        ~synopsis:"Tezos/Protocol: protocol testing framework"
        ~opam_only_deps:[octez_protocol_environment; parameters |> if_some]
        ~deps:
          [
            qcheck_alcotest;
            octez_test_helpers |> open_;
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
            octez_plompiler |> if_ N.(number >= 015);
            octez_crypto_dal |> if_ N.(number >= 016) |> open_;
          ]
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
        ~synopsis:"Tezos/Protocol: protocol plugin tests"
        ~opam:(sf "tezos-protocol-plugin-%s-tests" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
        ~opam:(sf "tezos-client-%s" name_dash)
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
            octez_clic;
            octez_version_value;
            main |> open_;
            lifted |> if_ N.(number >= 018) |> open_;
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
            octez_rpc;
            octez_rpc_http |> open_;
            octez_dal_node_services |> if_ N.(number >= 017);
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
        ~bisect_ppx:No
    in
    let _tenderbrute_exe =
      only_if (active && N.(number >= 013)) @@ fun () ->
      test
        "tenderbrute_main"
        ~alias:""
        ~path:(path // "lib_delegate/test/tenderbrute")
        ~with_macos_security_framework:true
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
          ~bisect_ppx:No
      in
      tezt
        ["test_scenario"]
        ~path:(path // "lib_delegate/test")
        ~with_macos_security_framework:true
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
            alcotezt;
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
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_clic;
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
    let layer2_utils =
      only_if N.(number >= 016) @@ fun () ->
      public_lib
        (sf "tezos-layer2-utils-%s" name_dash)
        ~path:(path // "lib_layer2_utils")
        ~synopsis:"Tezos/Protocol: protocol specific library for Layer 2 utils"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            main |> open_;
            client |> if_some |> open_;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let injector =
      only_if (active && N.(number >= 013 && number <= 015)) @@ fun () ->
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
            octez_crypto;
            main |> open_;
            octez_micheline |> open_;
            client |> if_some |> open_;
            octez_client_base |> open_;
            octez_workers |> open_;
            octez_shell;
            layer2_utils |> if_some |> open_;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let dal =
      only_if (active && N.(number >= 016)) @@ fun () ->
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
            octez_stdlib_unix |> open_;
            octez_dal_node_lib |> open_;
            client |> if_some |> open_;
            plugin |> if_some |> open_;
            embedded |> open_;
            layer2_utils |> if_some |> open_;
            main |> open_;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let _dal_tests =
      only_if (active && N.(number >= 016)) @@ fun () ->
      tezt
        (* test [test_dac_pages_encoding] was removed after 016 *)
        (if N.(number == 016) then
         [
           "test_dal_slot_frame_encoding";
           "test_dac_pages_encoding";
           "test_helpers";
         ]
        else ["test_dal_slot_frame_encoding"; "test_helpers"])
        ~path:(path // "lib_dal/test")
        ~opam:(sf "tezos-dal-%s" name_dash)
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            dal |> if_some |> open_;
            main |> open_;
            octez_base_test_helpers |> open_;
            test_helpers |> if_some |> open_;
            alcotezt;
          ]
    in
    let dac =
      (* [~link_all:true] is necessary to ensure that the dac plugin
         registration happens when running the dal node. Removing this
         option would cause DAL related tezts to fail because the DAC
         plugin cannot be resolved. *)
      only_if (active && N.(number >= 017)) @@ fun () ->
      public_lib
        (sf "tezos-dac-%s" name_dash)
        ~path:(path // "lib_dac_plugin")
        ~synopsis:
          "Tezos/Protocol: protocol specific library for the Data availability \
           Committee"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_protocol_compiler_registerer |> open_;
            octez_stdlib_unix |> open_;
            octez_dac_lib |> open_;
            octez_dac_client_lib |> open_;
            client |> if_some |> open_;
            embedded |> open_;
            main |> open_;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let _dac_tests =
      only_if (active && N.(number >= 017)) @@ fun () ->
      tezt
        [
          "test_dac_pages_encoding";
          "test_dac_plugin_registration";
          "test_helpers";
        ]
        ~path:(path // "lib_dac_plugin/test")
        ~with_macos_security_framework:true
        ~opam:(sf "tezos-dac-%s" name_dash)
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            dac |> if_some |> open_;
            main |> open_;
            octez_base_test_helpers |> open_;
            test_helpers |> if_some |> open_;
            octez_dac_lib |> open_;
            octez_dac_node_lib |> open_;
            alcotezt;
          ]
    in
    let octez_sc_rollup_layer2 =
      only_if N.(number >= 016) @@ fun () ->
      public_lib
        (sf "tezos-smart-rollup-layer2-%s" name_dash)
        ~path:(path // "lib_sc_rollup_layer2")
        ~synopsis:
          "Tezos/Protocol: protocol specific library for `tezos-smart-rollup`"
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives";
            main |> open_;
            octez_injector |> open_;
            octez_smart_rollup_lib |> open_;
          ]
        ~inline_tests:ppx_expect
        ~linkall:true
    in
    let octez_sc_rollup_node =
      only_if (active && N.(number >= 016)) @@ fun () ->
      private_lib
        (sf "octez_smart_rollup_node_%s" short_hash)
        ~path:(path // "lib_sc_rollup_node")
        ~opam:(sf "octez-smart-rollup-node-%s" short_hash)
        ~deps:
          [
            octez_base |> open_ |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_stdlib_unix |> open_;
            octez_client_base |> open_;
            octez_client_base_unix |> open_;
            client |> if_some |> open_;
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
            (* [dac] is needed for the DAC observer client which is not
               available in Nairobi and earlier. *)
            dac |> if_some |> if_ N.(number >= 018) |> open_;
            octez_dac_lib |> if_ N.(number >= 018) |> open_;
            octez_dac_client_lib |> if_ N.(number >= 018) |> open_;
            octez_shell_services |> open_;
            octez_smart_rollup_lib |> open_;
            octez_sc_rollup |> if_some |> open_;
            octez_sc_rollup_layer2 |> if_some |> open_;
            layer2_utils |> if_some |> open_;
            octez_layer2_store |> open_;
            octez_crawler |> open_;
            tree_encoding;
            data_encoding;
            irmin_pack;
            irmin_pack_unix;
            irmin;
            aches;
            aches_lwt;
            octez_injector |> open_;
            octez_smart_rollup_node_lib |> open_;
            octez_scoru_wasm;
            octez_scoru_wasm_fast;
            octez_crypto_dal |> if_ N.(number >= 016) |> open_;
            octez_version_value;
          ]
        ~conflicts:[Conflicts.checkseum]
    in
    let _octez_sc_rollup_node_test =
      only_if N.(number >= 016) @@ fun () ->
      let helpers =
        private_lib
          (sf "octez_smart_rollup_node_%s_test_helpers" short_hash)
          ~path:(path // "lib_sc_rollup_node/test/helpers")
          ~opam:"tezos-sc-rollup-node-test"
          ~deps:
            [
              octez_base |> open_ ~m:"TzPervasives"
              |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
              main |> open_;
              parameters |> if_some |> open_;
              client |> if_some |> open_;
              octez_test_helpers |> open_;
              qcheck_alcotest;
              qcheck_core;
              logs_lwt;
              octez_sc_rollup_layer2 |> if_some |> open_;
              octez_sc_rollup_node |> if_some |> open_;
              alcotezt;
            ]
      in
      tezt
        ["canary"; "test_octez_conversions"]
        ~path:(path // "lib_sc_rollup_node/test")
        ~opam:"tezos-sc-rollup-node-test"
        ~synopsis:"Tests for the smart rollup node library"
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
            octez_test_helpers |> open_;
            octez_sc_rollup_layer2 |> if_some |> open_;
            octez_sc_rollup_node |> if_some |> open_;
            alcotezt;
            helpers |> open_;
          ]
    in
    let octez_sc_rollup_client =
      only_if (active && N.(number >= 016)) @@ fun () ->
      private_lib
        (sf "octez_smart_rollup_client_%s" short_hash)
        ~path:(path // "lib_sc_rollup_client")
        ~opam:(sf "octez-smart-rollup-client-%s" short_hash)
        ~deps:
          [
            octez_base |> open_ |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            main |> open_;
            octez_client_commands |> open_;
            octez_client_base |> open_;
            octez_client_base_unix |> open_;
            client |> if_some |> open_;
            octez_sc_rollup |> if_some |> open_;
            octez_sc_rollup_layer2 |> if_some |> open_;
          ]
    in
    let _sc_rollup_client =
      only_if (active && N.(number >= 016)) @@ fun () ->
      public_exe
        (sf "octez-smart-rollup-client-%s" short_hash)
        ~internal_name:(sf "main_sc_rollup_client_%s" name_underscore)
        ~path:(path // "bin_sc_rollup_client")
        ~synopsis:"Tezos/Protocol: Smart rollup client"
        ~release_status:executable_release_status
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_clic;
            main |> open_;
            octez_sc_rollup_client |> if_some |> open_;
            octez_version_value;
          ]
    in
    let _sc_rollup_node =
      only_if (active && N.(number >= 016)) @@ fun () ->
      public_exe
        (sf "octez-smart-rollup-node-%s" short_hash)
        ~internal_name:(sf "main_sc_rollup_node_%s" name_underscore)
        ~path:(path // "bin_sc_rollup_node")
        ~synopsis:"Tezos/Protocol: protocol specific Smart rollup node"
        ~release_status:executable_release_status
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_clic;
            main |> open_;
            octez_shell_services |> open_;
            octez_client_base |> open_;
            octez_client_base_unix |> open_;
            octez_client_commands |> open_;
            client |> if_some |> open_;
            octez_smart_rollup_node_lib |> open_;
            octez_sc_rollup_node |> if_some |> open_;
          ]
    in
    let tx_rollup =
      only_if (active && N.(number >= 013 && number <= 015)) @@ fun () ->
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
            octez_crypto;
            main |> open_;
            client |> if_some |> open_;
            octez_client_commands |> open_;
            octez_context_encoding;
            baking_commands |> if_some |> open_;
            octez_stdlib_unix |> open_;
            octez_rpc;
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
      only_if (active && N.(number >= 013 && number <= 015)) @@ fun () ->
      public_exe
        (sf "octez-tx-rollup-client-%s" short_hash)
        ~internal_name:(sf "main_tx_rollup_client_%s" name_underscore)
        ~path:(path // "bin_tx_rollup_client")
        ~synopsis:"Tezos/Protocol: `octez-tx-rollup-client-alpha` client binary"
        ~release_status:executable_release_status
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
            octez_clic;
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
      only_if (active && N.(number >= 013 && number <= 015)) @@ fun () ->
      public_exe
        (sf "octez-tx-rollup-node-%s" short_hash)
        ~internal_name:(sf "main_tx_rollup_node_%s" name_underscore)
        ~path:(path // "bin_tx_rollup_node")
        ~synopsis:"Tezos/Protocol: Transaction Rollup node binary"
        ~release_status:executable_release_status
        ~with_macos_security_framework:true
        ~deps:
          [
            octez_base |> open_ ~m:"TzPervasives"
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals"
            |> open_;
            octez_clic;
            main |> open_;
            client |> if_some |> open_;
            octez_client_base |> open_;
            octez_client_base_unix |> open_;
            tx_rollup |> if_some |> open_;
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
            octez_crypto;
            parameters |> if_some;
            hashcons;
            test_helpers |> open_;
            prbnmcn_stats;
          ]
        ~linkall:true
        ~private_modules:["kernel"; "rules"; "state_space"]
        ~bisect_ppx:(if N.(number <= 012) then Yes else No)
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
            |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
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
         ?dac
         ?test_helpers
         ?parameters
         ?benchmarks_proto
         ?baking
         ?octez_sc_rollup_layer2
         ?octez_sc_rollup_node
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

  let _016_PtMumbai = active (Name.v "PtMumbai" 016)

  let _017_PtNairob = active (Name.v "PtNairob" 017)

  let _018_Proxford = active (Name.v "Proxford" 018)

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

let _octez_store_tests =
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
    ~opam:"tezos-store"
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
        octez_test_helpers |> open_;
      ]

(* [_octez_bench_store_lib_tests_exe] is a bench for the store locator,
   We do not run these tests in the CI. *)
let _octez_bench_store_lib_tests_exe =
  private_exe
    "bench"
    ~path:"src/lib_store/unix/test/bench"
    ~synopsis:"Bench store lib tests"
    ~opam:""
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tezt_lib;
        alcotezt;
        _octez_store_tests |> open_;
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
    ~synopsis:"Slow store lib tests"
    ~modules:["test_slow"]
    ~opam:""
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        tezt_lib;
        alcotezt;
        _octez_store_tests |> open_;
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
        octez_test_helpers |> open_;
        alcotezt;
        octez_version_value;
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
    ~bisect_ppx:No

let _git_gas_diff =
  public_exe
    "git-gas-diff"
    ~path:"devtools/git-gas-diff/bin"
    ~synopsis:"Internal dev tools"
    ~internal_name:"main"
    ~opam:"internal-devtools"
    ~deps:[external_lib "num" V.True; re]
    ~static:false
    ~bisect_ppx:No

let _gas_parameter_diff =
  public_exe
    "gas_parameter_diff"
    ~path:"devtools/gas_parameter_diff/bin"
    ~synopsis:"Internal dev tools"
    ~internal_name:"main"
    ~opam:"internal-devtools"
    ~deps:[]
    ~static:false
    ~bisect_ppx:No

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
    ~with_macos_security_framework:true
    ~synopsis:"A script to extract smart contracts from a node."
    ~opam:""
    ~deps:
      [
        octez_micheline |> open_;
        octez_base |> open_ ~m:"TzPervasives";
        get_contracts_lib |> open_;
      ]
    ~modules:["get_contracts"]
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
    ~bisect_ppx:No
    ~linkall:true

let _yes_wallet =
  private_exe
    "yes_wallet"
    ~path:("devtools" // "yes_wallet")
    ~with_macos_security_framework:true
    ~synopsis:
      "A script extracting delegates' keys from a context into a wallet."
    ~opam:""
    ~deps:[yes_wallet_lib |> open_]
    ~modules:["yes_wallet"]
    ~bisect_ppx:No

let _yes_wallet_test =
  private_exe
    "bench_signature_perf"
    ~path:("devtools" // "yes_wallet" // "test")
    ~synopsis:"Tests for yes_wallet tool"
    ~opam:""
    ~deps:
      [
        octez_error_monad |> open_ ~m:"TzLwtreslib";
        octez_crypto;
        zarith;
        zarith_stubs_js;
        data_encoding |> open_;
        lwt_unix;
        ptime;
      ]
    ~bisect_ppx:No

let simdal_lib =
  private_lib
    "simdal"
    ~path:("devtools" // "simdal" // "lib")
    ~synopsis:"P2P simulator library"
    ~opam:""
    ~deps:[ocamlgraph; prbnmcn_stats; unix]
    ~static:false
    ~bisect_ppx:No

let _simdal =
  private_exes
    ["sim"; "concat"]
    ~path:("devtools" // "simdal" // "bin")
    ~synopsis:"DAL/P2P simulator"
    ~opam:""
    ~deps:[simdal_lib]
    ~static:false
    ~bisect_ppx:No

let _ppinclude =
  private_exe
    "ppinclude"
    ~path:"src/lib_protocol_environment/ppinclude"
    ~opam:"tezos-protocol-environment"
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
    "octez-node"
    ~path:"src/bin_node"
    ~internal_name:"main"
    ~synopsis:"Tezos: `octez-node` binary"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives" |> open_;
         octez_base_unix;
         octez_version;
         octez_version_value;
         octez_node_config |> open_;
         octez_stdlib_unix |> open_;
         octez_shell_services |> open_;
         octez_rpc_http |> open_;
         octez_rpc_http_server |> open_;
         octez_p2p |> open_;
         octez_shell |> open_;
         octez_store |> open_;
         octez_store_unix_reconstruction |> open_;
         octez_store_unix_snapshots |> open_;
         octez_context;
         octez_validation |> open_;
         octez_shell_context |> open_;
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
    ["octez-client"; "octez-admin-client"]
    ~path:"src/bin_client"
    ~internal_names:["main_client"; "main_admin"]
    ~opam:"octez-client"
    ~synopsis:"Tezos: `octez-client` binary"
    ~release_status:Released
    ~deps:
      ([
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
         data_encoding |> open_;
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_client_base_unix |> open_;
         octez_client_base |> open_;
         octez_clic;
         octez_stdlib_unix |> open_;
         octez_event_logging |> open_;
         octez_signer_services;
         octez_version_value;
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
    "octez-proxy-server"
    ~path:"src/bin_proxy_server"
    ~internal_name:"main_proxy_server"
    ~synopsis:"Octez: `octez-proxy-server` binary"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives" |> open_;
         octez_base_unix;
         octez_stdlib_unix |> open_;
         octez_rpc;
         cmdliner;
         lwt_exit;
         lwt_unix;
         octez_proxy;
         octez_proxy_server_config;
         octez_rpc_http_client_unix;
         octez_rpc_http_server;
         octez_shell_services;
         octez_shell_context;
         octez_version_value;
         uri;
       ]
      @ Protocol.all_optionally [Protocol.client; Protocol.plugin])
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
    ~bisect_ppx:No
    ~static:false
    ~profile:"octez-dev-deps"
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
    "octez-signer"
    ~path:"src/bin_signer"
    ~internal_name:"main_signer"
    ~synopsis:"Tezos: `octez-signer` binary"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      [
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
    "octez-dal-node"
    ~path:"src/bin_dal_node"
    ~internal_name:"main"
    ~synopsis:"Tezos: `octez-dal-node` binary"
    ~release_status:Experimental
    ~with_macos_security_framework:true
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_version;
         cmdliner;
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
         octez_layer2_store |> open_;
         octez_crypto_dal |> open_;
         octez_store_unix;
         octez_store_shared |> open_;
         octez_gossipsub |> open_;
         octez_dal_node_gossipsub_lib |> open_;
         octez_p2p |> open_;
         octez_p2p_services |> open_;
         octez_crypto |> open_;
         octez_base_p2p_identity_file |> open_;
         octez_shell_services |> open_;
         irmin_pack;
         irmin_pack_unix;
         irmin;
       ]
      @ protocol_deps)
    ~conflicts:[Conflicts.checkseum]

let _octez_dac_node =
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
      let targets = List.filter_map Fun.id [Protocol.dac protocol] in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  public_exe
    "octez-dac-node"
    ~path:"src/bin_dac_node"
    ~internal_name:"main_dac"
    ~synopsis:"Tezos: `octez-dac-node` binary"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_clic;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         octez_client_commands |> open_;
         octez_rpc_http |> open_;
         octez_rpc_http_server;
         octez_protocol_updater;
         octez_rpc_http_client_unix;
         octez_stdlib_unix |> open_;
         octez_stdlib |> open_;
         octez_dac_node_lib |> open_;
         octez_layer2_store |> open_;
         irmin_pack;
         irmin_pack_unix;
         irmin;
       ]
      @ protocol_deps)
    ~conflicts:[Conflicts.checkseum]

let _octez_dac_client =
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
      let targets = List.filter_map Fun.id [Protocol.dac protocol] in
      if is_optional then List.map optional targets else targets
    in
    List.map deps_for_protocol Protocol.all |> List.flatten
  in
  public_exe
    "octez-dac-client"
    ~path:"src/bin_dac_client"
    ~internal_name:"main_dac_client"
    ~synopsis:"Tezos: `octez-dac-client` binary"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix;
         octez_clic;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         octez_client_commands |> open_;
         octez_stdlib_unix |> open_;
         octez_stdlib |> open_;
         octez_dac_lib |> open_;
         octez_dac_client_lib |> open_;
       ]
      @ protocol_deps)

let _octez_scoru_wasm_debugger =
  public_exe
    (sf "octez-smart-rollup-wasm-debugger")
    ~internal_name:(sf "main_wasm_debugger")
    ~path:"src/bin_wasm_debugger"
    ~opam:"octez-smart-rollup-wasm-debugger"
    ~synopsis:"Tezos: Debugger for the smart rollups’ WASM kernels"
    ~release_status:Released
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_clic;
        tree_encoding;
        octez_base_unix;
        (* The debugger always rely on proto_alpha, as such the client is always
           available. *)
        Protocol.(client_exn alpha);
        octez_scoru_wasm;
        octez_scoru_wasm_benchmark;
        octez_scoru_wasm_helpers |> open_;
        octez_webassembly_interpreter |> open_;
        octez_webassembly_interpreter_extra |> open_;
        octez_version_value;
      ]

let evm_proxy_lib =
  private_lib
    "evm_proxy_lib"
    ~path:"src/bin_evm_proxy/lib"
    ~opam:"octez-evm-proxy-lib"
    ~synopsis:
      "An implementation of a subset of Ethereum JSON-RPC API for the EVM \
       rollup"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_rpc_http |> open_;
        octez_rpc_http_client_unix;
        octez_version_value;
        lwt_exit;
        rlp;
        rope;
      ]

let octez_scoru_sequencer =
  private_lib
    "octez_smart_rollup_sequencer"
    ~path:"src/lib_scoru_sequencer"
    ~opam:"octez-smart-rollup-sequencer"
    ~synopsis:"Sequencer library for smart contract rollup"
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives"
        |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
        Protocol.(octez_sc_rollup_layer2 alpha |> if_some |> open_);
        Protocol.(main alpha) |> open_;
        Protocol.(octez_sc_rollup_node alpha) |> if_some;
        octez_rpc;
        octez_rpc_http;
        octez_rpc_http_server;
      ]

let _sc_sequencer_node =
  public_exe
    "octez-smart-rollup-sequencer-node"
    ~internal_name:"main_sequencer_node"
    ~path:"src/bin_sequencer_node"
    ~synopsis:"Smart rollup sequencer node (low-latency node)"
    ~release_status:Experimental
    ~with_macos_security_framework:true
    ~deps:
      [
        octez_base |> open_ |> open_ ~m:"TzPervasives"
        |> open_ ~m:"TzPervasives.Error_monad.Legacy_monad_globals";
        octez_clic;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        octez_client_commands |> open_;
        octez_smart_rollup_node_lib |> open_;
        Protocol.(client alpha) |> if_some |> open_;
        Protocol.(octez_sc_rollup_node alpha) |> if_some |> open_;
        octez_scoru_sequencer |> open_;
      ]

let _evm_proxy =
  public_exe
    (sf "octez-evm-proxy-server")
    ~internal_name:(sf "evm_proxy")
    ~path:("src" // "bin_evm_proxy")
    ~opam:"octez-evm-proxy"
    ~synopsis:
      "An implementation of a subset of Ethereum JSON-RPC API for the EVM \
       rollup"
    ~release_status:Experimental
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix;
        octez_clic;
        octez_rpc_http |> open_;
        octez_rpc_http_server;
        octez_version_value;
        evm_proxy_lib;
      ]
    ~bisect_ppx:Yes

let _octez_evm_chunker_exe =
  private_exe
    "octez_evm_chunker"
    ~path:"src/bin_evm_proxy/chunker"
    ~synopsis:"EVM kernel transaction chunker"
    ~opam:"octez-evm-chunker"
    ~deps:[octez_base |> open_ ~m:"TzPervasives"; evm_proxy_lib]

let octez_scoru_wasm_regressions =
  private_lib
    "tezos_scoru_wasm_regressions"
    ~path:"src/lib_scoru_wasm/regressions"
    ~opam:"tezos-scoru-wasm-regressions"
    ~synopsis:"WASM PVM regressions"
    ~deps:
      [
        octez_base |> open_ ~m:"TzPervasives";
        octez_scoru_wasm |> open_;
        octez_scoru_wasm_helpers;
        octez_test_helpers;
        Protocol.(main alpha);
        tezt_lib |> open_ |> open_ ~m:"Base";
      ]
    ~preprocess:[staged_pps [ppx_import; ppx_deriving_show]]

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
  (* opam/mandatory-for-make.opam is a trick to have [make build-deps]
     install some optional dependencies in a mandatory way.
     Once we use lock files, we can remove this, probably. *)
  | ["opam"; "mandatory-for-make.opam"] -> true
  (* opam-repository is used by scripts/opam-release.sh *)
  | "opam-repository" :: _ -> true
  (* Tezt is only partially managed by the manifest.
     There is no real good reason for that but only the core Tezt library is released. *)
  | "tezt" :: "long_tests" :: _ -> true
  | "tezt" :: "manual_tests" :: _ -> true
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
        tezt_ethereum |> open_;
        data_encoding;
        octez_base;
        octez_base_unix;
        octez_stdlib_unix;
        Protocol.(main alpha);
        octez_scoru_wasm_regressions;
      ]
    in
    test
      "main"
      ~with_macos_security_framework:true
      ~alias:""
      ~path:"tezt/tests"
        (* Instrument with sigterm handler, to ensure that coverage from
           Tezt worker processes are collected. *)
      ~bisect_ppx:With_sigterm
      ~opam:""
      ~deps:(deps @ test_libs)
  in
  generate
    ~make_tezt_exe
    ~default_profile:"octez-deps"
    ~add_to_meta_package:
      [
        (* [ledgerwallet_tezos] is an optional dependency, but we want
           [opam install octez] to always install it. *)
        ledgerwallet_tezos;
      ]

(* Generate a dune-workspace file at the root of the repo *)
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

(* Generate active_protocol_versions_without_number. *)
let () =
  let write_protocol fmt protocol =
    match Protocol.number protocol with
    | Alpha | V _ -> Format.fprintf fmt "%s\n" (Protocol.short_hash protocol)
    | Other -> ()
  in
  write "script-inputs/active_protocol_versions_without_number" @@ fun fmt ->
  List.iter (write_protocol fmt) Protocol.active

let () = postcheck ~exclude ()
