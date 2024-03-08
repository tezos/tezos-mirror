(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

open Externals

(* tezt targets depend on the external [tezt_core_lib] library which is declared
   an [external_lib] in [Externals]. But target [maker]s are defined in
   [Manifest] (before the externals).

   As a result, we have to do a little dance here where we inject the external
   dependencies (declared in [Externals]) into the tezt maker (declared in
   [Manifest]). *)
let tezt ~product ~opam ~path ?js_compatible ?modes ?(deps = []) ?dep_globs
    ?dep_globs_rec ?dep_files ?opam_with_test ?dune_with_test ?synopsis
    ?(with_macos_security_framework = false) ?flags ?dune ?preprocess
    ?preprocessor_deps l =
  Manifest.tezt
    ~product
    ~with_macos_security_framework
    ~opam
    ~path
    ?synopsis
    ?js_compatible
    ?modes
    ~lib_deps:
      ((tezt_core_lib |> Manifest.open_ |> Manifest.open_ ~m:"Base") :: deps)
    ~exe_deps:[tezt_lib]
    ~js_deps:[tezt_js_lib]
    ?dep_globs
    ?dep_globs_rec
    ?dep_files
    ?opam_with_test
    ?dune_with_test
    ?flags
    ?dune
    ?preprocess
    ?preprocessor_deps
    l

module Product (M : sig
  val name : string
end) =
struct
  include Manifest.Product (M)

  let tezt = tezt ~product:M.name
end
