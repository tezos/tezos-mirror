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
let tezt ~product ~opam ~path ?modes ?(deps = []) ?dep_globs ?dep_globs_rec
    ?dep_files ?opam_with_test ?dune_with_test ?synopsis
    ?(with_macos_security_framework = false) ?flags ?dune ?preprocess
    ?preprocessor_deps ?include_in_main_tezt_exe ?source l =
  Manifest.tezt
    ~product
    ~with_macos_security_framework
    ~opam
    ~path
    ?synopsis
    ?modes
    ~lib_deps:
      ((tezt_core_lib |> Manifest.open_ |> Manifest.open_ ~m:"Base") :: deps)
    ~exe_deps:[tezt_lib]
    ?dep_globs
    ?dep_globs_rec
    ?dep_files
    ?opam_with_test
    ?dune_with_test
    ?flags
    ?dune
    ?preprocess
    ?preprocessor_deps
    ?include_in_main_tezt_exe
    ?source
    l

module Product (M : sig
  val name : string

  val source : string list
end) =
struct
  include Manifest.Product (M)

  let tezt = tezt ~product:M.name ~source:M.source
end
