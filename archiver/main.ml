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
  {Clic.name = "generic"; Clic.title = "Protocol agnostic teztale command"}

let user_arg =
  Clic.default_arg
    ~doc:"Name of the feeder"
    ~short:'u'
    ~long:"user"
    ~placeholder:"name"
    ~default:"archiver"
    (Clic.parameter (fun _ p -> return p))

let password_arg =
  Clic.default_arg
    ~doc:"Authentification to the endpoint"
    ~short:'p'
    ~long:"password"
    ~placeholder:"secret"
    ~default:""
    (Clic.parameter (fun _ p -> return p))

let starting_block_arg =
  Clic.default_arg
    ~doc:"Starting block"
    ~short:'b'
    ~long:"block"
    ~placeholder:"int"
    ~default:"1"
    (Clic.parameter (fun _ p -> return (Int32.of_string p)))

let endpoint_param =
  Clic.param
    ~name:"server_endpoint"
    ~desc:"Teztale server to feed"
    (Clic.parameter (fun _ p -> return (Uri.of_string p)))

let new_file_parameter =
  Clic.parameter (fun _ p ->
      if Sys.file_exists p then failwith "File already exist: '%s'" p
      else return p)

let path_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p) then failwith "File does not exist: '%s'" p
      else return p)

let directory_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p && Sys.is_directory p) then
        failwith "Directory doesn't exist: '%s'" p
      else return p)

let main_db cctxt db_path source =
  let* () = Client_confirmations.wait_for_bootstrapped cctxt in
  (* let* () = await_protocol_activation cctxt Loops.protocol_hash in *)
  let db = Sqlite3.db_open db_path in
  let () = Db.set_pragma_use_foreign_keys db in
  let dumper = Db_archiver.launch db source in
  let main =
    let*! () =
      Lwt.Infix.(
        General_archiver.Db_loops.blocks_loop cctxt
        <&> General_archiver.Db_loops.endorsements_loop cctxt)
    in
    let () = Db_archiver.stop () in
    Lwt.return_unit
  in
  let*! out = Lwt.join [dumper; main] in
  return out

let main_json cctxt prefix =
  let* () = Client_confirmations.wait_for_bootstrapped cctxt in
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
  let* () = Client_confirmations.wait_for_bootstrapped cctxt in
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

let select_commands _ctxt Client_config.{chain; _} =
  return
    [
      Clic.command
        ~group
        ~desc:"create empty Sqlite3 database"
        Clic.no_options
        (Clic.prefixes ["create"; "database"; "in"]
        @@ Clic.param
             ~name:"db_path"
             ~desc:"path to file in which to store the Sqlite3 database"
             new_file_parameter
        @@ Clic.stop)
        (fun () db_path _cctxt ->
          Db.create_db db_path ;
          return_unit);
      Clic.command
        ~group
        ~desc:"run the db archiver"
        Clic.no_options
        (Clic.prefixes ["run"; "db-archiver"; "on"]
        @@ Clic.param
             ~name:"db_path"
             ~desc:"path to Sqlite3 database file where to store the data"
             path_parameter
        @@ Clic.prefix "for"
        @@ Clic.param
             ~name:"source"
             ~desc:"name of the data source (i.e. of the node)"
             (Clic.parameter (fun _ p -> return p))
        @@ Clic.stop)
        (fun () db_path source cctxt -> main_db cctxt db_path source);
      Clic.command
        ~group
        ~desc:"run the json archiver"
        Clic.no_options
        (Clic.prefixes ["run"; "json-archiver"; "in"]
        @@ Clic.param
             ~name:"archive_path"
             ~desc:"folder in which to dump files"
             directory_parameter
        @@ Clic.stop)
        (fun () prefix cctxt -> main_json cctxt prefix);
      Clic.command
        ~group
        ~desc:"upload a file hierarchy to a server"
        (Clic.args2
           (Clic.default_arg
              ~doc:"Name of the feeder"
              ~short:'u'
              ~long:"user"
              ~placeholder:"name"
              ~default:"archiver"
              (Clic.parameter (fun _ p -> return p)))
           (Clic.default_arg
              ~doc:"Name of the feeder"
              ~short:'p'
              ~long:"password"
              ~placeholder:"secret"
              ~default:""
              (Clic.parameter (fun _ p -> return p))))
        (Clic.prefixes ["convert"; "from"]
        @@ Clic.string ~name:"archive_path" ~desc:"folder where files are"
        @@ Clic.prefix "to"
        @@ Clic.param
             ~name:"server_endpoint"
             ~desc:"Teztale server to feed"
             (Clic.parameter (fun _ p -> return (Uri.of_string p)))
        @@ Clic.stop)
        (fun (source, pass) prefix endpoint _cctxt ->
          Converter.main source pass endpoint prefix);
      Clic.command
        ~group
        ~desc:"inject endorsing rights in a teztale_server"
        (Clic.args3 user_arg password_arg starting_block_arg)
        (Clic.prefixes ["insert"; "rights"; "in"] @@ endpoint_param @@ Clic.stop)
        (fun (source, pass, starting) endpoint cctxt ->
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
            Server_archiver.{cohttp_ctx; auth = (source, pass); endpoint}
          in
          let dumper = Server_archiver.launch state "source-not-used" in
          let main =
            General_archiver.print_failures
              (General_archiver.Server_loops.rights chain starting cctxt)
          in
          let*! out = Lwt.join [dumper; main] in
          return out);
      Clic.command
        ~group
        ~desc:"inject past blocks in a teztale_server"
        (Clic.args3 user_arg password_arg starting_block_arg)
        (Clic.prefixes ["insert"; "blocks"; "in"] @@ endpoint_param @@ Clic.stop)
        (fun (source, pass, starting) endpoint cctxt ->
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
            Server_archiver.{cohttp_ctx; auth = (source, pass); endpoint}
          in
          let dumper = Server_archiver.launch state "source-not-used" in
          let main =
            General_archiver.print_failures
              (General_archiver.Server_loops.blocks chain starting cctxt)
          in
          let*! out = Lwt.join [dumper; main] in
          return out);
      Clic.command
        ~group
        ~desc:"run the archiver and feed an aggregator"
        (Clic.args2 user_arg password_arg)
        (Clic.prefixes ["feed"] @@ endpoint_param @@ Clic.stop)
        (fun auth endpoint cctxt ->
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
          let state = Server_archiver.{cohttp_ctx; auth; endpoint} in
          main_server state cctxt);
    ]

let () = Client_main_run.run (module Client_config) ~select_commands
