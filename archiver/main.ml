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

let select_commands ctxt Client_config.{protocol; chain; block; _} =
  let* proto_commands =
    match protocol with
    | Some protocol ->
        return
          (Tezos_client_commands.Client_commands.commands_for_version
             protocol
             None)
    | None -> (
        let*! protocol =
          Shell_services.Blocks.protocols ctxt ~chain ~block ()
        in
        match protocol with
        | Ok {next_protocol; _} ->
            return
              (Tezos_client_commands.Client_commands.commands_for_version
                 next_protocol
                 None)
        | Error err ->
            let () = Format.eprintf "%a@." Error_monad.pp_print_trace err in
            return [])
  in
  return
    ([
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
         (Clic.prefixes ["insert"; "rights"; "in"]
         @@ endpoint_param @@ Clic.stop)
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
           let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
           Server_archiver.rights
             ctx
             (source, pass)
             endpoint
             chain
             starting
             cctxt);
       Clic.command
         ~group
         ~desc:"inject past blocks in a teztale_server"
         (Clic.args3 user_arg password_arg starting_block_arg)
         (Clic.prefixes ["insert"; "blocks"; "in"]
         @@ endpoint_param @@ Clic.stop)
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
           let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
           Server_archiver.blocks
             ctx
             (source, pass)
             endpoint
             chain
             starting
             cctxt);
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
           let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
           let*! () =
             Lwt.join
               [
                 Server_archiver.endorsements_loop ctx auth endpoint cctxt;
                 Server_archiver.blocks_loop ctx auth endpoint cctxt;
               ]
           in
           return_unit);
     ]
    @ proto_commands)

let () = Client_main_run.run (module Client_config) ~select_commands
