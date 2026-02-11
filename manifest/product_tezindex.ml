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
open Product_prometheus

include Product (struct
  let name = "tezindex"

  let source = ["tezindex/"]
end)

let link_flags =
  [Dune.[S ":include"; S "%{workspace_root}/static-link-flags-tezindex.sexp"]]

let release_status = Experimental

let tezindex_library =
  private_lib
    "lib_tezindex"
    ~path:"tezindex/lib_tezindex"
    ~opam:""
    ~deps:
      [
        prometheus_app;
        caqti_postgresql;
        caqti_sqlite;
        caqti_lwt_unix;
        cohttp_lwt_unix;
        octez_base |> open_ |> open_ ~m:"TzPervasives";
        octez_client_base |> open_;
        octez_client_base_unix |> open_;
        octez_rpc_http_client_unix |> open_;
        octez_rpc_http |> open_;
        octez_rpc_http_server |> open_;
        safepass;
        lwt_unix;
      ]

let protocols =
  List.filter_map
    (fun protocol ->
      match Protocol.number protocol with
      | Other | V 000 -> None
      | V v when v <= 023 -> None
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
     dune's build directory. Copy it in /tezindex/bin_tezindex_archiver and add it to git. *)
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

let _tezindex =
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
    (sf "octez-tezindex")
    ~internal_name:(sf "tezindex_main")
    ~path:"tezindex/bin_tezindex"
    ~opam:"octez-tezindex"
    ~synopsis:"Tezos: tezindex, a delegate operations monitor"
    ~with_macos_security_framework:true
    ~static:false
    ~link_flags
    ~deps:
      ([
         bls12_381_archive;
         octez_base |> open_ ~m:"TzPervasives";
         octez_base_unix |> open_;
         octez_client_base |> open_;
         octez_client_base_unix |> open_;
         octez_stdlib_unix |> open_;
         octez_shell_services |> open_;
         tezindex_library |> open_;
         x509;
       ]
      @ protocol_deps)
    ~release_status
    ~dune:generated_machines

let () = generate_content_input ()
