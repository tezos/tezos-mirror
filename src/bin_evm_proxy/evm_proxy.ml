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

open Evm_proxy_lib

type rollup_node_endpoint = Mockup | Endpoint of Uri.t

type config = {
  rpc_addr : string;
  rpc_port : int;
  debug : bool;
  rollup_node_endpoint : rollup_node_endpoint;
}

let default_config =
  {
    rpc_addr = "127.0.0.1";
    rpc_port = 8545;
    debug = true;
    rollup_node_endpoint = Mockup;
  }

let make_config ?rpc_addr ?rpc_port ?debug ~rollup_node_endpoint () =
  {
    rpc_addr = Option.value ~default:default_config.rpc_addr rpc_addr;
    rpc_port = Option.value ~default:default_config.rpc_port rpc_port;
    debug = Option.value ~default:default_config.debug debug;
    rollup_node_endpoint =
      Option.value
        ~default:default_config.rollup_node_endpoint
        rollup_node_endpoint;
  }

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
  let* body_str = body |> Cohttp_lwt.Body.to_string in
  Format.printf "Uri: %s\nMethod: %s\nBody: %s\n%!" uri meth body_str ;
  Tezos_rpc_http_server.RPC_server.resto_callback
    server
    conn
    req
    (Cohttp_lwt.Body.of_string body_str)

module Event = struct
  let section = ["evm_proxy_server"]

  let event_starting =
    Internal_event.Simple.declare_0
      ~section
      ~name:"start_evm_proxy_server"
      ~msg:"starting the EVM proxy server"
      ~level:Notice
      ()

  let event_is_ready =
    Internal_event.Simple.declare_2
      ~section
      ~name:"evm_proxy_server_is_ready"
      ~msg:"the EVM proxy server is listening to {addr}:{port}"
      ~level:Notice
      ("addr", Data_encoding.string)
      ("port", Data_encoding.uint16)
end

let start {rpc_addr; rpc_port; debug; rollup_node_endpoint} =
  let open Lwt_result_syntax in
  let open Tezos_rpc_http_server in
  let p2p_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string p2p_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  let* rollup_node_config =
    match rollup_node_endpoint with
    | Endpoint endpoint ->
        let module Rollup_node_rpc = Rollup_node.Make (struct
          let base = endpoint
        end) in
        let* smart_rollup_address = Rollup_node_rpc.smart_rollup_address in
        return ((module Rollup_node_rpc : Rollup_node.S), smart_rollup_address)
    | Mockup ->
        let* smart_rollup_address = Mockup.smart_rollup_address in
        return ((module Mockup : Rollup_node.S), smart_rollup_address)
  in
  let directory = Services.directory rollup_node_config in
  let server =
    RPC_server.init_server
      ~acl
      ~media_types:Media_type.all_media_types
      directory
  in
  Lwt.catch
    (fun () ->
      let*! () =
        RPC_server.launch
          ~host
          server
          ~callback:
            (if debug then callback_log server
            else RPC_server.resto_callback server)
          node
      in
      let*! () =
        Internal_event.Simple.emit Event.event_is_ready (rpc_addr, rpc_port)
      in
      return server)
    (fun _ -> return server)

module Params = struct
  let string = Tezos_clic.parameter (fun _ s -> Lwt.return_ok s)

  let int = Tezos_clic.parameter (fun _ s -> Lwt.return_ok (int_of_string s))

  let rollup_node_endpoint =
    Tezos_clic.parameter (fun _ s ->
        let endpoint =
          match s with
          | "mockup" -> Mockup
          | uri -> Endpoint (Uri.of_string uri)
        in
        Lwt.return_ok endpoint)
end

let rpc_addr_arg =
  Tezos_clic.arg
    ~long:"rpc-addr"
    ~placeholder:"ADDR"
    ~doc:"The EVM proxy server rpc address."
    Params.string

let rpc_port_arg =
  Tezos_clic.arg
    ~long:"rpc-port"
    ~placeholder:"PORT"
    ~doc:"The EVM proxy server rpc port."
    Params.int

let rollup_node_endpoint_arg =
  Tezos_clic.arg
    ~long:"rollup-node-endpoint"
    ~placeholder:"ADDR:PORT"
    ~doc:
      "The smart rollup node endpoint address the proxy server will \
       communicate with, or [mockup] if you want to use mock values."
    Params.rollup_node_endpoint

let main_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Start the RPC server"
    (args3 rpc_addr_arg rpc_port_arg rollup_node_endpoint_arg)
    (prefixes ["run"] @@ stop)
    (fun (rpc_addr, rpc_port, rollup_node_endpoint) () ->
      let*! () = Tezos_base_unix.Internal_event_unix.init () in
      let*! () = Internal_event.Simple.emit Event.event_starting () in
      let config = make_config ?rpc_addr ?rpc_port ~rollup_node_endpoint () in
      let* server = start config in
      let (_ : Lwt_exit.clean_up_callback_id) = install_finalizer server in
      let wait, _resolve = Lwt.wait () in
      let* () = wait in
      return_unit)

(* List of program commands *)
let commands = [main_command]

let global_options = Tezos_clic.no_options

let executable_name = Filename.basename Sys.executable_name

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let dispatch initial_ctx args =
  let open Lwt_result_syntax in
  let commands =
    Tezos_clic.add_manual
      ~executable_name
      ~global_options
      (if Unix.isatty Unix.stdout then Tezos_clic.Ansi else Tezos_clic.Plain)
      Format.std_formatter
      commands
  in
  let* ctx, remaining_args =
    Tezos_clic.parse_global_options global_options initial_ctx args
  in
  Tezos_clic.dispatch commands ctx remaining_args

let handle_error = function
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      let version = Tezos_version_value.Bin_version.version_string in
      Format.printf "%s\n" version ;
      exit 0
  | Error [Tezos_clic.Help command] ->
      Tezos_clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options
        (match command with None -> [] | Some c -> [c]) ;
      Stdlib.exit 0
  | Error errs ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options
        ~default:Error_monad.pp
        errs ;
      Stdlib.exit 1

let () =
  let _ =
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short)
  in
  Lwt_main.run (dispatch () (argv ())) |> handle_error
