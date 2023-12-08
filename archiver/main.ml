(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 - 2021 Nomadic Labs, <contact@nomadic-labs.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Lwt_result_syntax

let group =
  {
    Tezos_clic.name = "teztale";
    Tezos_clic.title = "A delegate operation monitor";
  }

let starting_block_arg =
  Tezos_clic.default_arg
    ~doc:"Starting block"
    ~short:'b'
    ~long:"block"
    ~placeholder:"int"
    ~default:"1"
    (Tezos_clic.parameter (fun _ p -> return (Int32.of_string p)))

let endpoint_param =
  Tezos_clic.param
    ~name:"server_endpoint"
    ~desc:"Teztale server to feed"
    (Tezos_clic.parameter (fun _ p -> return (Uri.of_string p)))

let directory_parameter =
  Tezos_clic.parameter (fun _ p ->
      if not (Sys.file_exists p && Sys.is_directory p) then
        failwith "Directory doesn't exist: '%s'" p
      else return p)

let main_json cctxt prefix =
  let logger = Teztale_lib.Log.logger () in
  let* () = Client_confirmations.wait_for_bootstrapped cctxt in
  let () = Teztale_lib.Log.info logger (fun () -> "Node bootstrapped") in
  (* let* () = await_protocol_activation cctxt Loops.protocol_hash in *)
  let dumper = Json_archiver.launch cctxt prefix in
  let main =
    let*! () =
      Lwt.Infix.(
        General_archiver.Json_loops.blocks_loop cctxt
        <&> General_archiver.Json_loops.endorsements_loop cctxt)
    in
    let () = Json_archiver.stop () in
    Lwt.return_unit
  in
  let*! out = Lwt.join [dumper; main] in
  return out

let main_server state cctxt =
  let logger = Teztale_lib.Log.logger () in
  let* () = Client_confirmations.wait_for_bootstrapped cctxt in
  let () = Teztale_lib.Log.info logger (fun () -> "Node bootstrapped") in
  (* let* () = await_protocol_activation cctxt Loops.protocol_hash in *)
  let dumper = Server_archiver.launch state "source-not-used" in
  let main =
    let*! () =
      Lwt.Infix.(
        General_archiver.Server_loops.blocks_loop cctxt
        <&> General_archiver.Server_loops.endorsements_loop cctxt)
    in
    let () = Server_archiver.stop () in
    Lwt.return_unit
  in
  let*! out = Lwt.join [dumper; main] in
  return out

let server_to_json_chunk : Server_archiver.chunk -> Json_archiver.chunk option =
  function
  | Block (level, (block, cycle_info, (endos, preendos))) ->
      Some
        (Block
           ( level,
             block.Data.Block.hash,
             block.Data.Block.predecessor,
             block.round,
             block.timestamp,
             block.reception_times,
             block.delegate,
             cycle_info,
             endos @ preendos ))
  | Mempool (level, ops) -> Some (Mempool (None, level, ops))
  | Rights (_, _) -> None

let select_commands _ctxt Client_config.{chain; _} =
  return
    [
      Tezos_clic.command
        ~group
        ~desc:"run the json archiver"
        Tezos_clic.no_options
        (Tezos_clic.prefixes ["run"; "json-archiver"; "in"]
        @@ Tezos_clic.param
             ~name:"archive_path"
             ~desc:"folder in which to dump files"
             directory_parameter
        @@ Tezos_clic.stop)
        (fun () prefix cctxt -> main_json cctxt prefix);
      Tezos_clic.command
        ~group
        ~desc:"upload a file hierarchy to a teztale_server"
        Tezos_clic.no_options
        (Tezos_clic.prefixes ["convert"; "from"]
        @@ Tezos_clic.string ~name:"archive_path" ~desc:"folder where files are"
        @@ Tezos_clic.prefix "to"
        @@ Tezos_clic.param
             ~name:"server_endpoint"
             ~desc:"Teztale server to feed"
             (Tezos_clic.parameter (fun _ p -> return (Uri.of_string p)))
        @@ Tezos_clic.stop)
        (fun () prefix endpoint _cctxt ->
          let Server_archiver.{auth = source, pass; endpoint} =
            Server_archiver.extract_auth endpoint
          in
          Converter.main source pass endpoint prefix);
      Tezos_clic.command
        ~group
        ~desc:"inject endorsing rights in a teztale_server"
        (Tezos_clic.args1 starting_block_arg)
        (Tezos_clic.prefixes ["insert"; "rights"; "in"]
        @@ endpoint_param @@ Tezos_clic.stop)
        (fun starting endpoint cctxt ->
          let*! ctx =
            match X509.Authenticator.of_string "none" with
            | Error _ -> Conduit_lwt_unix.init ()
            | Ok f ->
                let tls_authenticator =
                  f (fun () -> Some (Time.System.now ()))
                in
                Conduit_lwt_unix.init ~tls_authenticator ()
          in
          let cohttp_ctx = Cohttp_lwt_unix.Net.init ~ctx () in
          let endpoints = [Server_archiver.extract_auth endpoint] in
          let state =
            Server_archiver.
              {cohttp_ctx; endpoints; backup = (fun _ -> Lwt.return_unit)}
          in
          let dumper = Server_archiver.launch state "source-not-used" in
          let main =
            let logger = Log.logger () in
            General_archiver.print_failures
              logger
              (General_archiver.Server_loops.rights chain starting cctxt)
          in
          let*! out = Lwt.join [dumper; main] in
          return out);
      Tezos_clic.command
        ~group
        ~desc:"inject past blocks in a teztale_server"
        (Tezos_clic.args1 starting_block_arg)
        (Tezos_clic.prefixes ["insert"; "blocks"; "in"]
        @@ endpoint_param @@ Tezos_clic.stop)
        (fun starting endpoint cctxt ->
          let*! ctx =
            match X509.Authenticator.of_string "none" with
            | Error _ -> Conduit_lwt_unix.init ()
            | Ok f ->
                let tls_authenticator =
                  f (fun () -> Some (Time.System.now ()))
                in
                Conduit_lwt_unix.init ~tls_authenticator ()
          in
          let cohttp_ctx = Cohttp_lwt_unix.Net.init ~ctx () in
          let endpoints = [Server_archiver.extract_auth endpoint] in
          let state =
            Server_archiver.
              {cohttp_ctx; endpoints; backup = (fun _ -> Lwt.return_unit)}
          in
          let dumper = Server_archiver.launch state "source-not-used" in
          let logger = Log.logger () in
          let main =
            General_archiver.print_failures
              logger
              (General_archiver.Server_loops.applied_blocks
                 chain
                 starting
                 cctxt)
          in
          let*! out = Lwt.join [dumper; main] in
          return out);
      Tezos_clic.command
        ~group
        ~desc:"run the archiver to a teztale_server"
        (Tezos_clic.args2
           (Tezos_clic.arg
              ~doc:"dump failed post data"
              ~long:"backup-dir"
              ~placeholder:"path"
              (Tezos_clic.parameter (fun _ p -> return p)))
           (Tezos_clic.arg
              ~doc:"set verbosity level"
              ~long:"verbosity"
              ~placeholder:"FATAL|ERROR|WARNING|INFO|DEBUG"
              (Tezos_clic.parameter (fun _ arg ->
                   match Teztale_lib.Log.level_of_string arg with
                   | Some level -> return level
                   | None -> fail []))))
        (Tezos_clic.prefixes ["feed"] @@ Tezos_clic.seq_of_param endpoint_param)
        (fun (backup_path, level) endpoints cctxt ->
          Option.iter (fun level -> Teztale_lib.Log.verbosity := level) level ;
          let logger = Teztale_lib.Log.logger () in
          let*! ctx =
            match X509.Authenticator.of_string "none" with
            | Error _ -> Conduit_lwt_unix.init ()
            | Ok f ->
                let tls_authenticator =
                  f (fun () -> Some (Time.System.now ()))
                in
                Conduit_lwt_unix.init ~tls_authenticator ()
          in
          let cohttp_ctx = Cohttp_lwt_unix.Net.init ~ctx () in
          let state =
            Server_archiver.
              {
                cohttp_ctx;
                endpoints = List.map Server_archiver.extract_auth endpoints;
                backup =
                  Option.fold
                    ~none:(fun chunk ->
                      Teztale_lib.Log.error logger (fun () ->
                          let level, kind =
                            match chunk with
                            | Block (level, _) -> (level, "/block")
                            | Mempool (level, _) -> (level, "/mempool")
                            | Rights (level, _) -> (level, "/rights")
                          in
                          Printf.sprintf
                            "(%ld) Failed to send %s data and --backup-dir is \
                             not set."
                            level
                            kind) ;
                      Lwt.return_unit)
                    ~some:(fun prefix chunk ->
                      match server_to_json_chunk chunk with
                      | None -> Lwt.return_unit
                      | Some chunk -> Json_archiver.dump prefix chunk)
                    backup_path;
              }
          in
          main_server state cctxt);
    ]

module M001 = PtCJ7pwo_machine.M
module M002 = PsYLVpVv_machine.M
module M003 = PsddFKi3_machine.M
module M004 = Pt24m4xi_machine.M
module M005 = PsBabyM1_machine.M
module M006 = PsCARTHA_machine.M
module M007 = PsDELPH1_machine.M
module M008 = PtEdo2Zk_machine.M
module M009 = PsFLoren_machine.M
module M010 = PtGRANAD_machine.M
module M011 = PtHangz2_machine.M
module M012 = Psithaca_machine.M
module M013 = PtJakart_machine.M
module M014 = PtKathma_machine.M
module M015 = PtLimaPt_machine.M
module M016 = PtMumbai_machine.M
module M017 = PtNairob_machine.M
module M018 = Proxford_machine.M

module Client_config = struct
  include Client_config

  let default_media_type = Media_type.Command_line.Binary
end

let () = Client_main_run.run (module Client_config) ~select_commands
