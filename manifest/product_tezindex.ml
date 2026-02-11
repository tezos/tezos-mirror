(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals
open Internals
open Product_octez

include Product (struct
  let name = "tezindex"

  let source = ["tezindex/"]
end)

let link_flags =
  [Dune.[S ":include"; S "%{workspace_root}/static-link-flags-tezindex.sexp"]]

let release_status = Experimental

let _tezindex =
  public_exe
    (sf "octez-tezindex")
    ~internal_name:(sf "tezindex_main")
    ~path:"tezindex/bin_tezindex"
    ~opam:"octez-tezindex"
    ~synopsis:"Tezos: tezindex, a delegate operations monitor"
    ~with_macos_security_framework:true
    ~static:false
    ~link_flags
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_base_unix |> open_;
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        octez_stdlib_unix |> open_;
        octez_shell_services |> open_;
        x509;
      ]
    ~release_status

let () = generate_content_input ()
