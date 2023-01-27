(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type config = {rpc_addr : string; rpc_port : int; debug : bool}

let default_config = {rpc_addr = "127.0.0.1"; rpc_port = 8545; debug = true}

let install_finalizer server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let+ () = Tezos_rpc_http_server.RPC_server.shutdown server in
  Format.printf "Server exited with code %d\n%!" exit_status

let callback_log server conn req body =
  let open Cohttp in
  let open Lwt_syntax in
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  let headers = req |> Request.headers |> Header.to_string in
  let* body_str = body |> Cohttp_lwt.Body.to_string in
  Format.printf
    "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s\n%!"
    uri
    meth
    headers
    body_str ;
  Tezos_rpc_http_server.RPC_server.resto_callback
    server
    conn
    req
    (Cohttp_lwt.Body.of_string body_str)

let start {rpc_addr; rpc_port; debug} =
  let open Lwt_syntax in
  let open Tezos_rpc_http_server in
  let rpc_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string rpc_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  let directory = Services.directory in
  let server =
    RPC_server.init_server
      ~acl
      ~media_types:Media_type.all_media_types
      directory
  in
  Lwt.catch
    (fun () ->
      let* () =
        RPC_server.launch
          ~host
          server
          ~callback:
            (if debug then callback_log server
            else RPC_server.resto_callback server)
          node
      in
      return server)
    (fun _ -> return server)

let main_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command ~desc:"Start the RPC server" no_options stop (fun () () ->
      Format.printf "Starting server\n%!" ;
      let*! server = start default_config in
      let (_ : Lwt_exit.clean_up_callback_id) = install_finalizer server in
      let wait, _resolve = Lwt.wait () in
      let* () = wait in
      return_unit)

(* List of program commands *)
let commands = [main_command]

let global_options = Tezos_clic.no_options

let dispatch initial_ctx args =
  let open Lwt_result_syntax in
  let* ctx, remaining_args =
    Tezos_clic.parse_global_options global_options initial_ctx args
  in
  Tezos_clic.dispatch commands ctx remaining_args

let () =
  ignore
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short) ;
  let args = Array.to_list Sys.argv |> List.tl |> Option.value ~default:[] in
  let result = Lwt_main.run (dispatch () args) in
  match result with
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      let version = Tezos_version.Bin_version.version_string in
      Format.printf "%s\n" version ;
      exit 0
  | Error e ->
      Format.eprintf
        "%a\n%!"
        Tezos_clic.(
          fun ppf errs ->
            pp_cli_errors
              ppf
              ~executable_name:"evm_proxy"
              ~global_options:no_options
              ~default:pp
              errs)
        e ;
      exit 1
