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

let rope = external_lib "rope" V.(at_least "0.6.2")

let alcotest = external_lib "alcotest" V.(at_least "1.5.0")

let alcotest_lwt = external_lib "alcotest-lwt" V.(at_least "1.5.0")

let ambient_context_lwt = external_lib "ambient-context-lwt" V.(exactly "0.1.0")

let asetmap = external_lib "asetmap" V.(at_least "0.8.1")

let astring = external_lib "astring" V.True

let base64 = external_lib "base64" V.(at_least "3.3.0")

let bheap = external_lib "bheap" V.(at_least "2.0.0")

let bigarray_compat = external_lib "bigarray-compat" V.True

let bigstring = external_lib "bigstring" V.True

let bigstringaf = external_lib "bigstringaf" V.(at_least "0.5.0")

let bisect_ppx = opam_only "bisect_ppx" V.(at_least "2.7.0")

let camlp_streams = external_lib "camlp-streams" V.(at_least "5.0.1")

let camlzip = external_lib "camlzip" V.(at_least "1.13")

let caqti = external_lib "caqti" V.True

let caqti_lwt = external_lib "caqti-lwt" V.(at_least "2.0.1")

let caqti_lwt_unix = external_sublib caqti_lwt "caqti-lwt.unix"

let caqti_sqlite =
  (* We have evidence that latest version of Caqti Sqlite introduces
     flakiness/hanged connections, at least for the EVM Node. We’ll revert the
     upperbound once we have identified the issue. *)
  external_lib "caqti-driver-sqlite3" V.(at_least "2.0.1" && less_than "2.2.0")

let caqti_postgresql =
  external_lib "caqti-driver-postgresql" V.(at_least "2.0.1")

let caqti_dynload = external_lib "caqti-dynload" V.True

(* Checkseum is an irmin-pack dependency. Version 0.5.0 is known to be
   bugged and this release was disabled. *)
let checkseum = external_lib "checkseum" (V.different_from "0.5.0")

let checkseum_ocaml = external_sublib checkseum "checkseum.ocaml"

let clap = external_lib "clap" V.(at_least "0.3.0")

let cmdliner = external_lib "cmdliner" V.(at_least "1.1.0")

let cohttp = external_lib "cohttp" V.(exactly "5.3.1~octez")

let cohttp_lwt = external_lib "cohttp-lwt" V.(exactly "5.3.1~octez")

let cohttp_lwt_unix = external_lib "cohttp-lwt-unix" V.(exactly "5.3.1~octez")

let conduit = external_lib "conduit-lwt" V.(at_least "7.1.0")

let conduit_lwt = external_lib "conduit-lwt" V.(at_least "7.1.0")

let conduit_lwt_unix = external_lib "conduit-lwt-unix" V.(at_least "7.1.0")

let compiler_libs_common = external_lib "compiler-libs.common" V.True ~opam:""

let compiler_libs_optcomp = external_lib "compiler-libs.optcomp" V.True ~opam:""

let compiler_libs_toplevel =
  external_lib "compiler-libs.toplevel" V.True ~opam:""

let conf_libev = opam_only "conf-libev" V.True

let conf_rust = opam_only "conf-rust" V.True

let crunch = opam_only "crunch" V.(at_least "3.3.0")

let ctypes = external_lib "ctypes" V.(at_least "0.18.0")

let ctypes_foreign =
  external_lib ~opam:"ctypes-foreign" "ctypes.foreign" V.(at_least "0.18.0")

let ctypes_stubs = external_sublib ctypes "ctypes.stubs"

let digestif = external_lib "digestif" V.(at_least "0.9.0")

let domainslib = external_lib "domainslib" V.(at_least "0.5.2")

let dream = external_lib "dream" V.(at_least "1.0.0~alpha8-octez")

let dune_configurator = external_lib "dune-configurator" V.True

let dynlink = external_lib "dynlink" V.True ~opam:""

let eio = external_lib "eio" V.True

let eio_posix = external_lib "eio_posix" V.True

let lwt_eio = external_lib "lwt_eio" V.True

let eqaf = external_lib "eqaf" V.True

let ezgzip = external_lib "ezgzip" V.True

let ezjsonm = external_lib "ezjsonm" V.(at_least "1.3.0")

let fmt = external_lib "fmt" V.(at_least "0.8.7")

let fmt_cli = external_sublib fmt "fmt.cli"

let fmt_tty = external_sublib fmt "fmt.tty"

let fpath = external_lib "fpath" V.True

let hacl_star = external_lib "hacl-star" V.(at_least "0.7.1" && less_than "0.8")

let hacl_star_raw = external_lib "hacl-star-raw" V.True

let hashcons = external_lib "hashcons" V.True

let hex = external_lib "hex" V.(at_least "1.3.0")

let index = external_lib "index" V.(at_least "1.6.0" && less_than "1.7.0")

let index_unix = external_sublib index "index.unix"

let integers = external_lib "integers" V.True

let ipaddr = external_lib "ipaddr" V.(at_least "5.3.0" && less_than "6.0.0")

let ipaddr_unix = external_sublib ipaddr "ipaddr.unix"

let jsonm = external_lib "jsonm" V.True

let logs = external_lib "logs" V.True

let logs_fmt = external_sublib logs "logs.fmt"

let logs_cli = external_sublib fmt "logs.cli"

let logs_lwt = external_sublib logs "logs.lwt"

let lru = external_lib "lru" V.(at_least "0.3.0")

(* 5.9.0 deprecates Lwt_stream.junk_old, which we use *)
let lwt = external_lib "lwt" V.(at_least "5.7.0" && less_than "5.9.0")

let lwt_canceler =
  external_lib "lwt-canceler" V.(at_least "0.3" && less_than "0.4")

let lwt_exit = external_lib "lwt-exit" V.True

let lwt_log = external_lib "lwt_log" V.True

let lwt_ppx = external_lib "lwt_ppx" V.True

let lwt_unix = external_sublib lwt "lwt.unix"

let lwt_watcher = external_lib "lwt-watcher" V.(exactly "0.2")

let magic_mime = external_lib "magic-mime" V.(at_least "1.3.1")

let memtrace = external_lib "memtrace" V.True

let mirage_crypto_rng = external_lib "mirage-crypto-rng" V.(at_least "1.0.0")

let mtime = external_lib "mtime" V.(at_least "2.0.0")

let mtime_clock_os = external_sublib mtime "mtime.clock.os"

let ocaml_protoc_compiler =
  external_lib "ocaml-protoc-plugin" V.(at_least "4.5.0")

let ocamlformat = opam_only "ocamlformat" V.(exactly "0.27.0")

let ocamlgraph = external_lib "ocamlgraph" V.True

let ocplib_endian = external_lib "ocplib-endian" V.True

let ocplib_endian_bigstring =
  external_sublib ocplib_endian "ocplib-endian.bigstring"

let ocplib_ocamlres =
  external_lib ~opam:"ocp-ocamlres" "ocplib-ocamlres" V.(at_least "0.4")

let opam_file_format = external_lib "opam-file-format" V.(at_least "2.1.6")

let opentelemetry = external_lib "opentelemetry" V.(at_least "0.12")

let opentelemetry_ambient_context_lwt =
  external_sublib opentelemetry "opentelemetry.ambient-context.lwt"

let opentelemetry_lwt = external_lib "opentelemetry-lwt" V.True

let opentelemetry_client_cohttp_lwt =
  external_lib "opentelemetry-client-cohttp-lwt" V.True

let optint = external_lib "optint" V.True

let ppx_expect = inline_tests_backend (external_lib "ppx_expect" V.True)

let ppxlib = external_lib "ppxlib" V.(at_least "0.34.0")

let ppxlib_metaquot = external_sublib ppxlib "ppxlib.metaquot"

let ptime = external_lib "ptime" V.(at_least "1.1.0")

let ppx_import = external_lib "ppx_import" V.True

let ppx_deriving = external_lib "ppx_deriving" V.True

let ppx_deriving_show = external_sublib ppx_deriving "ppx_deriving.show"

let ppx_repr = external_lib "ppx_repr" V.(at_least "0.6.0")

let ppx_repr_lib = external_sublib ppx_repr "ppx_repr.lib"

let ppx_sexp_conv = external_lib "ppx_sexp_conv" V.True

let ptime_clock_os = external_sublib ptime "ptime.clock.os"

let pure_splitmix = external_lib "pure-splitmix" V.(exactly "0.3")

let prbnmcn_linalg = external_lib "prbnmcn-linalg" V.(exactly "0.0.1")

let prbnmcn_stats = external_lib "prbnmcn-stats" V.(exactly "0.0.6")

let pringo = external_lib "pringo" V.(at_least "1.3" && less_than "1.4")

let progress = external_lib "progress" V.(at_least "0.1.0")

let pyml = external_lib "pyml" V.(at_least "20220905")

let qcheck_alcotest = external_lib "qcheck-alcotest" V.(at_least "0.20")

let qcheck_core = external_lib "qcheck-core" V.True

let re = external_lib "re" V.(at_least "1.10.0")

let repr = external_lib "repr" V.True

let ringo = external_lib "ringo" V.(at_least "1.1.0")

let rss = external_lib "rss" V.True

let rusage = external_lib "rusage" V.True

let aches = external_lib "aches" V.(at_least "1.1.0")

let aches_lwt = external_lib "aches-lwt" V.(at_least "1.1.0")

let safepass = external_lib "safepass" V.True

let saturn = external_lib "saturn" V.(at_least "0.5.0")

let secp256k1_internal =
  let version = V.(at_least "0.4.0") in
  external_lib "secp256k1-internal" version

let semaphore_compat = external_lib "semaphore-compat" (V.at_least "1.0.1")

let seqes = external_lib "seqes" V.(at_least "0.2")

let sexplib = external_lib "sexplib" V.True

(* WARNING: SQLite extension in etherlink/bin_node/lib_dev/sqlite_receipt_bloom
   re-export some types of the ocaml sqlite3 interface which is not guaranteed
   to be stable. Verify compatibility before updating sqlite3 dependency. *)
let sqlite3 = external_lib "sqlite3" V.(exactly "5.3.1")

let stdint = external_lib "stdint" V.True

let str = external_lib "str" ~opam:"" V.True

let tar = external_lib "tar" V.True

let tar_unix = external_lib "tar-unix" V.(at_least "2.0.1" && less_than "3.0.0")

let terminal = external_lib "terminal" V.True

let tezos_sapling_parameters =
  opam_only ~can_vendor:false "tezos-sapling-parameters" V.(at_least "1.1.0")

let tls_lwt = external_lib "tls-lwt" V.(at_least "1.0.4")

let trace = external_lib "trace" V.True

let unix = external_lib ~opam:"base-unix" "unix" V.True

let uri = external_lib "uri" V.(at_least "3.1.0")

let lambda_term = external_lib "lambda-term" V.(at_least "3.3.1")

let utop = external_lib "utop" V.(at_least "2.8")

let uuidm = external_lib "uuidm" V.(at_least "0.9.9")

let uutf = external_lib "uutf" V.True

let vector = external_lib "vector" V.True

let vdf = external_lib "class_group_vdf" V.(at_least "0.0.4")

let yaml = external_lib "yaml" V.(at_least "3.1.0")

let jingoo = external_lib "jingoo" V.True

let dmap = external_lib "dmap" V.True

(* The signature of the [Z] module has changed in 1.12. *)
let zarith = external_lib "zarith" V.(at_least "1.13" && less_than "1.14")

let ledgerwallet_tezos = external_lib "ledgerwallet-tezos" V.(at_least "0.4.1")

let crowbar = external_lib "crowbar" V.(at_least "0.2")

let ppx_hash = external_lib "ppx_hash" V.True

let tezt_bam = external_lib "tezt-bam" V.(at_least "0.3")

let tezt_lib =
  external_lib
    "tezt"
    V.(at_least "4.1.0" && less_than "5.0.0")
    ~main_module:"Tezt"

let tezt_core_lib =
  external_sublib tezt_lib "tezt.core" ~main_module:"Tezt_core"

let x509 = external_lib "x509" V.(at_least "1.0.0")

let tezt_json_lib = external_sublib tezt_lib "tezt.json" ~main_module:"JSON"

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
      external_lib "merlin" V.(at_least "4.18");
      external_lib "ocaml-lsp-server" V.(at_least "1.23.0");
      external_lib "odoc" V.(at_least "3.0.0");
      external_lib "ocp-indent" V.True;
      external_lib "merge-fmt" V.True;
    ]
