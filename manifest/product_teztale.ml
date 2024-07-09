(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals
open Internals
open Product_octez
open Product_prometheus

include Product (struct
  let name = "teztale"

  let source = ["teztale/"]
end)

let teztale_library_base =
  private_lib
    "lib_teztale_base"
    ~path:"teztale/lib_teztale_base"
    ~opam:""
    ~deps:[octez_base |> open_ |> open_ ~m:"TzPervasives"; lwt_unix]

let teztale_server_library =
  private_lib
    "lib_teztale_server"
    ~path:"teztale/lib_teztale_server"
    ~opam:""
    ~deps:
      [
        prometheus_app;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        lwt_unix;
        teztale_library_base |> open_;
      ]

let _teztale_server =
  public_exe
    (sf "octez-teztale-server")
    ~internal_name:(sf "teztale_server_main")
    ~path:"teztale/bin_teztale_server"
    ~opam:"octez-teztale"
    ~synopsis:"Tezos: teztale, a delegate operations monitor"
    ~with_macos_security_framework:true
    ~static:false
    ~link_flags:
      [
        Dune.[S ":include"; S "%{workspace_root}/static-link-flags-teztale.sexp"];
      ]
    ~deps:
      [
        aches;
        caqti_postgresql;
        caqti_sqlite;
        caqti_lwt_unix;
        Product_cohttp.cohttp_lwt_unix;
        ezgzip;
        safepass;
        octez_version_value;
        teztale_library_base |> open_;
        teztale_server_library |> open_;
      ]
    ~release_status:Experimental

let protocols =
  List.filter_map
    (fun protocol ->
      match Protocol.number protocol with
      | Other | Dev | V 000 -> None
      | V _ -> (
          match Protocol.client protocol with
          | Some package -> Some (Protocol.short_hash protocol, package)
          | None -> None))
    Protocol.all

let generate_file fname fcontent protocol =
  Dune.
    [S "rule"; [S "action"; [S "write-file"; S (fname protocol); S fcontent]]]

let generated_empty_machines =
  let generate (protocol, _package) =
    generate_file
      (Printf.sprintf "%s_machine.no.ml")
      "module M = struct end"
      protocol
  in
  List.fold_left (fun acc x -> Dune.(generate x :: acc)) Dune.[] protocols

let _teztale_archiver =
  let protocol_deps =
    let deps_for_protocol (protocol, package) =
      let source_if_present = Printf.sprintf "%s_machine.real.ml" protocol in
      let source_if_absent = Printf.sprintf "%s_machine.no.ml" protocol in
      let target = Printf.sprintf "%s_machine.ml" protocol in
      select ~package ~source_if_present ~source_if_absent ~target
    in
    List.map deps_for_protocol protocols
  in
  public_exe
    (sf "octez-teztale-archiver")
    ~internal_name:(sf "teztale_archiver_main")
    ~path:"teztale/bin_teztale_archiver"
    ~opam:"octez-teztale"
    ~synopsis:"Tezos: teztale, a delegate operations monitor"
    ~with_macos_security_framework:true
    ~static:false
    ~link_flags:
      [
        Dune.[S ":include"; S "%{workspace_root}/static-link-flags-teztale.sexp"];
      ]
    ~deps:
      ([
         octez_base |> open_ ~m:"TzPervasives";
         octez_stdlib_unix |> open_;
         octez_shell_services |> open_;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         octez_rpc_http_client_unix |> open_;
         octez_rpc_http |> open_;
         octez_version_value;
         teztale_library_base |> open_;
         x509;
       ]
      @ protocol_deps)
    ~release_status:Experimental
    ~dune:generated_empty_machines

let () = generate_content_input ()
