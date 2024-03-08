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

include Product (struct
  let name = "tooling"
end)

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
      ]
    ~npm_deps:
      [
        Npm.make "kaitai-struct" (Version (V.exactly "0.10.0"))
        (* Client-libs project requires Javascript Kaitai runtime. *);
      ]

let _node_wrapper =
  private_exe
    "node_wrapper"
    ~path:"src/tooling"
    ~opam:""
    ~deps:[unix]
    ~modules:["node_wrapper"]
    ~bisect_ppx:No

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
