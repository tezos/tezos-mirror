(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 TriliTech <contact@trili.tech>                         *)
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

let link_flags =
  [Dune.[S ":include"; S "%{workspace_root}/static-link-flags-teztale.sexp"]]

let release_status = Experimental

let _teztale_server =
  public_exe
    (sf "octez-teztale-server")
    ~internal_name:(sf "teztale_server_main")
    ~path:"teztale/bin_teztale_server"
    ~opam:"octez-teztale"
    ~synopsis:"Tezos: teztale, a delegate operations monitor"
    ~with_macos_security_framework:true
    ~static:false
    ~link_flags
    ~deps:
      [
        bls12_381_archive;
        aches;
        caqti_postgresql;
        caqti_sqlite;
        caqti_lwt_unix;
        cohttp_lwt_unix;
        ezgzip;
        safepass;
        octez_version_value;
        teztale_library_base |> open_;
        teztale_server_library |> open_;
        cmdliner;
      ]
    ~release_status

let protocols =
  List.filter_map
    (fun protocol ->
      match Protocol.number protocol with
      | Other | V 000 -> None
      | Dev | V _ -> (
          match Protocol.client protocol with
          | Some package -> Some (Protocol.short_hash protocol, package)
          | None -> None))
    Protocol.all

let write_file fname fcontent protocol =
  Dune.
    [S "rule"; [S "action"; [S "write-file"; S (fname protocol); S fcontent]]]

(* TODO: upate copyright header date *)
let generate_protocol_machine src dst =
  let src_fname = Printf.sprintf "%s_machine.real.ml" src in
  let dst_fname = Printf.sprintf "%s_machine.real.ml" dst in
  let substitute prefix =
    Printf.sprintf "s/%s%s/%s%s/g" prefix src prefix dst
  in
  Dune.target_rule
    ~mode:Fallback
    ~deps:[S src_fname]
    ~action:
      (Dune.progn
         [
           [S "copy"; S src_fname; S dst_fname];
           Dune.run
             "sed"
             [
               "-i.bak";
               "-e";
               substitute "Tezos_client_";
               "-e";
               substitute "Tezos_protocol_";
               "-e";
               substitute "Tezos_protocol_plugin_";
               dst_fname;
             ];
         ])
    dst_fname

let generated_machines =
  (* These ones are used when building without the full list protocol support.
     In that case, protocol-related libraries will be asbsent and dune will select
     the empty X_machine.no.ml file instead of the actual X_machine.real.ml one *)
  let generated_empty_machines =
    let generate (protocol, _package) =
      write_file
        (Printf.sprintf "%s_machine.no.ml")
        "module M = struct end"
        protocol
    in
    List.fold_left (fun acc x -> Dune.(generate x :: acc)) Dune.[] protocols
  in
  (* The process in order to create a new protocol machine implementation is basically
     to clone the "parent" protocol of the new one and replace some module names.
     If a new protocol is added in octez (either next or a release candidate version
     of next), you should find the corresponding file automatically produced into
     dune's build directory. Copy it in /teztale/bin_teztale_archiver and add it to git. *)
  List.fold_left
    (fun acc protocol ->
      match Protocol.number protocol with
      | V _ ->
          Dune.(
            generate_protocol_machine "next" (Protocol.short_hash protocol)
            :: acc)
      | _ -> acc)
    Dune.(generate_protocol_machine "alpha" "next" :: generated_empty_machines)
    Protocol.active

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
    ~link_flags
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives";
         octez_stdlib_unix |> open_;
         octez_shell_services |> open_;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         octez_rpc_http_client_unix |> open_;
         octez_rpc_http |> open_;
         octez_version_value;
         tls_lwt
         (* tls-lwt is depopt of conduit-lwt-unix, but we need conduit-lwt-unix
            to be compiled with it in teztale *);
         conduit_lwt_unix;
         teztale_library_base |> open_;
         x509;
       ]
      @ protocol_deps)
    ~release_status
    ~dune:generated_machines

let _teztale_snitch =
  public_exe
    (sf "octez-teztale-snitch")
    ~internal_name:(sf "teztale_snitch_main")
    ~path:"teztale/bin_teztale_snitch"
    ~opam:"octez-teztale"
    ~synopsis:"Tezos: teztale, a delegate operations monitor"
    ~with_macos_security_framework:true
    ~static:false
    ~link_flags
    ~deps:
      [
        bls12_381_archive;
        octez_base |> open_ ~m:"TzPervasives";
        octez_version_value;
        teztale_library_base |> open_;
        cohttp_lwt;
        cohttp_lwt_unix;
        cmdliner;
      ]
    ~release_status

let () = generate_content_input ()
