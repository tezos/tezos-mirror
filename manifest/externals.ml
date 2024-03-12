(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

module V = Version

let sf = Printf.sprintf

(* EXTERNAL LIBS *)

let alcotest = external_lib ~js_compatible:true "alcotest" V.(at_least "1.5.0")

let astring = external_lib ~js_compatible:true "astring" V.True

let bheap = external_lib "bheap" V.True

let bigarray_compat = external_lib ~js_compatible:true "bigarray-compat" V.True

let bigstring = external_lib ~js_compatible:true "bigstring" V.True

let bigstringaf =
  external_lib ~js_compatible:true "bigstringaf" V.(at_least "0.5.0")

let bisect_ppx = opam_only "bisect_ppx" V.(at_least "2.7.0")

let camlzip = external_lib "camlzip" V.(at_least "1.11" && less_than "1.12")

let caqti = external_lib "caqti" V.True

let caqti_lwt = external_lib "caqti-lwt" V.(at_least "2.0.1")

let caqti_lwt_unix = external_sublib caqti_lwt "caqti-lwt.unix"

let caqti_sqlite = external_lib "caqti-driver-sqlite3" V.(at_least "2.0.1")

let caqti_dynload = external_lib "caqti-dynload" V.True

(* Checkseum is an irmin-pack dependency. Version 0.5.0 is known to be
   bugged and this release was disabled. *)
let checkseum = external_lib "checkseum" (V.different_from "0.5.0")

let checkseum_ocaml = external_sublib checkseum "checkseum.ocaml"

let cmdliner = external_lib "cmdliner" V.(at_least "1.1.0")

let cohttp_lwt_unix = external_lib "cohttp-lwt-unix" V.(at_least "5.2.0")

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

let digestif = external_lib "digestif" V.True

let dune_configurator = external_lib "dune-configurator" V.True

let dynlink = external_lib "dynlink" V.True ~opam:""

let eqaf = external_lib "eqaf" V.True

let ezjsonm = external_lib ~js_compatible:true "ezjsonm" V.(at_least "1.3.0")

let fmt = external_lib ~js_compatible:true "fmt" V.(at_least "0.8.7")

let fmt_cli = external_sublib fmt "fmt.cli"

let fmt_tty = external_sublib fmt "fmt.tty"

let hacl_star =
  external_lib
    ~js_compatible:true
    ~npm_deps:
      [Npm.make "hacl-wasm" (Version V.(at_least "1.4.0" && less_than "1.5.0"))]
    "hacl-star"
    V.(at_least "0.7.1" && less_than "0.8")

let hacl_star_raw = external_lib ~js_compatible:true "hacl-star-raw" V.True

let hashcons = external_lib "hashcons" V.True

let hex = external_lib ~js_compatible:true "hex" V.(at_least "1.3.0")

let index = external_lib "index" V.(at_least "1.6.0" && less_than "1.7.0")

let index_unix = external_sublib index "index.unix"

let integers = external_lib ~js_compatible:true "integers" V.True

let integers_stubs_js =
  external_lib ~js_compatible:true "integers_stubs_js" V.True

let ipaddr =
  external_lib
    ~js_compatible:true
    "ipaddr"
    V.(at_least "5.3.0" && less_than "6.0.0")

let ipaddr_unix = external_sublib ipaddr "ipaddr.unix"

let json_data_encoding =
  external_lib
    ~js_compatible:true
    "json-data-encoding"
    V.(at_least "0.12" && less_than "0.13")

let jsonm = external_lib "jsonm" V.True

let logs = external_lib "logs" V.True

let logs_fmt = external_sublib logs "logs.fmt"

let logs_lwt = external_sublib logs "logs.lwt"

let lwt = external_lib ~js_compatible:true "lwt" V.(at_least "5.7.0")

let lwt_canceler =
  external_lib
    ~js_compatible:true
    "lwt-canceler"
    V.(at_least "0.3" && less_than "0.4")

let lwt_exit = external_lib "lwt-exit" V.True

let lwt_unix = external_sublib lwt "lwt.unix"

let lwt_watcher = external_lib "lwt-watcher" V.(exactly "0.2")

let mtime = external_lib ~js_compatible:true "mtime" V.(at_least "2.0.0")

let mtime_clock_os = external_sublib mtime "mtime.clock.os"

let ocaml_migrate_parsetree = external_lib "ocaml-migrate-parsetree" V.True

let ocaml_protoc_compiler =
  external_lib "ocaml-protoc-plugin" V.(at_least "4.5.0")

let ocamlformat = opam_only "ocamlformat" V.(exactly "0.24.1")

let ocamlgraph = external_lib "ocamlgraph" V.True

let ocplib_endian = external_lib ~js_compatible:true "ocplib-endian" V.True

let ocplib_endian_bigstring =
  external_sublib ocplib_endian "ocplib-endian.bigstring"

let ocplib_ocamlres =
  external_lib ~opam:"ocp-ocamlres" "ocplib-ocamlres" V.(at_least "0.4")

let optint = external_lib "optint" V.True

let ppx_expect = inline_tests_backend (external_lib "ppx_expect" V.True)

let ppxlib = external_lib "ppxlib" V.True

let ppxlib_metaquot = external_sublib ppxlib "ppxlib.metaquot"

let ptime = external_lib ~js_compatible:true "ptime" V.(at_least "1.1.0")

let ppx_import = external_lib "ppx_import" V.True

let ppx_deriving = external_lib "ppx_deriving" V.True

let ppx_deriving_show = external_sublib ppx_deriving "ppx_deriving.show"

let ppx_repr = external_lib "ppx_repr" V.(at_least "0.6.0")

let ppx_repr_lib = external_sublib ppx_repr "ppx_repr.lib"

let ppx_sexp_conv = external_lib "ppx_sexp_conv" V.True

let ptime_clock_os = external_sublib ~js_compatible:true ptime "ptime.clock.os"

let pure_splitmix =
  external_lib ~js_compatible:true "pure-splitmix" V.(exactly "0.3")

let prbnmcn_linalg = external_lib "prbnmcn-linalg" V.(exactly "0.0.1")

let prbnmcn_stats = external_lib "prbnmcn-stats" V.(exactly "0.0.6")

let pringo = external_lib "pringo" V.(at_least "1.3" && less_than "1.4")

let prometheus = external_lib "prometheus" V.(at_least "1.2")

let prometheus_app = external_lib "prometheus-app" V.(at_least "1.2")

let prometheus_app_unix = external_sublib prometheus_app "prometheus-app.unix"

let pyml = external_lib "pyml" V.(at_least "20220905")

let qcheck_alcotest =
  external_lib ~js_compatible:true "qcheck-alcotest" V.(at_least "0.20")

let qcheck_core = external_lib "qcheck-core" V.True

let re = external_lib ~js_compatible:true "re" V.(at_least "1.10.0")

let repr = external_lib "repr" V.True

let resto_version = V.(at_least "1.2")

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

let rusage = external_lib "rusage" V.True

let aches = external_lib ~js_compatible:true "aches" V.(at_least "1.0.0")

let aches_lwt = external_lib "aches-lwt" V.(at_least "1.0.0")

let secp256k1_internal =
  let version = V.(at_least "0.4.0") in
  external_lib
    ~npm_deps:[Npm.make "@nomadic-labs/secp256k1-wasm" (Version version)]
    ~js_compatible:true
    "secp256k1-internal"
    version

let seqes = external_lib ~js_compatible:true "seqes" V.(at_least "0.2")

let sexplib = external_lib "sexplib" V.True

let stdint = external_lib "stdint" V.True

let str = external_lib ~js_compatible:true "str" ~opam:"" V.True

let tar = external_lib "tar" V.True

let tar_unix = external_lib "tar-unix" V.(at_least "2.0.1" && less_than "3.0.0")

let tezos_rust_lib =
  opam_only ~can_vendor:false "tezos-rust-libs" V.(exactly "1.6")

let tezos_sapling_parameters =
  opam_only ~can_vendor:false "tezos-sapling-parameters" V.(at_least "1.1.0")

let tls_lwt = external_lib "tls-lwt" V.(at_least "0.16.0")

let unix = external_lib ~opam:"base-unix" "unix" V.True

let uri = external_lib ~js_compatible:true "uri" V.(at_least "3.1.0")

let lambda_term = external_lib "lambda-term" V.(at_least "3.3.1")

let utop = external_lib "utop" V.(at_least "2.8")

let uutf = external_lib ~js_compatible:true "uutf" V.True

let vdf =
  external_lib ~js_compatible:true "class_group_vdf" V.(at_least "0.0.4")

let yaml = external_lib "yaml" V.(at_least "3.1.0")

let jingoo = external_lib "jingoo" V.True

let dmap = external_lib "dmap" V.True

(* The signature of the [Z] module has changed in 1.12. *)
let zarith =
  external_lib
    ~js_compatible:true
    "zarith"
    V.(at_least "1.13" && less_than "1.14")

(* The 0.16.1 release matches of the stubs matches the 1.13 release of zarith *)
let zarith_stubs_js =
  external_lib ~js_compatible:true "zarith_stubs_js" V.(at_least "0.16.1")

let ledgerwallet_tezos = external_lib "ledgerwallet-tezos" V.(at_least "0.4.0")

let tezt_lib =
  external_lib
    ~js_compatible:false
    "tezt"
    V.(at_least "4.0.0" && less_than "5.0.0")
    ~main_module:"Tezt"

let tezt_core_lib =
  external_sublib
    tezt_lib
    ~js_compatible:true
    "tezt.core"
    ~main_module:"Tezt_core"

let tezt_js_lib = external_sublib tezt_lib ~js_compatible:true "tezt.js"

let tezt_json_lib =
  external_sublib tezt_lib ~js_compatible:true "tezt.json" ~main_module:"JSON"

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
      external_lib "ocaml-lsp-server" V.(at_least "1.16.2");
      external_lib "merge-fmt" V.True;
      external_lib "js_of_ocaml-lwt" V.(at_least "5.2.0");
    ]
