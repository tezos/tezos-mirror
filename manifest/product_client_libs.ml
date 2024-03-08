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
open Product_octez

let kaitai =
  public_lib
    "kaitai"
    ~path:"contrib/kaitai-ocaml/src"
    ~release_status:Unreleased
    ~preprocess:[pps ppx_sexp_conv]
    ~deps:[yaml; sexplib]
    ~dune:Dune.[ocamllex "lexer"; menhir "parser"]
    ~synopsis:"OCaml library for reading Kaitai spec files"

(* We use a private-lib with inline-tests in order to run the tests normally,
   but without placing all the code for the tests within the main kaitai
   library. *)
let _kaitai_test =
  private_lib
    "kaitai_test"
    ~opam:"kaitai"
    ~path:"contrib/kaitai-ocaml/test"
    ~release_status:Unreleased
    ~inline_tests:ppx_expect
    ~deps:[kaitai]

let kaitai_of_data_encoding =
  public_lib
    "kaitai-of-data-encoding"
    ~path:"contrib/lib_kaitai_of_data_encoding"
    ~release_status:Unreleased
    ~synopsis:"Kaitai spec generator for data-encoding library"
    ~deps:[yaml; data_encoding; kaitai]
    ~bisect_ppx:No

let _kaitai_of_data_encoding_test =
  private_lib
    "kaitai_of_data_encoding_test"
    ~opam:"kaitai-of-data-encoding"
    ~path:"contrib/lib_kaitai_of_data_encoding/test"
    ~release_status:Unreleased
    ~deps:[yaml; data_encoding; kaitai; kaitai_of_data_encoding]
    ~bisect_ppx:No
    ~inline_tests:ppx_expect

let _octez_codec_kaitai =
  public_exe
    "octez-codec-kaitai"
    ~path:"contrib/bin_codec_kaitai"
    ~release_status:Unreleased
    ~internal_name:"codec"
    ~synopsis:
      "Tezos: `octez-codec-kaitai` binary to generate kaitai descriptions"
    ~with_macos_security_framework:true
    ~deps:
      ([
         data_encoding |> open_;
         kaitai_of_data_encoding;
         kaitai;
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
    ~dune:
      Dune.
        [
          S "cram"
          :: G [S "deps" :: [S "codec.exe"]]
          :: [S "package" :: [S "octez-codec"]];
        ]
