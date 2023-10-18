(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

type mode = Prod | Dev

type config = {
  rpc_addr : string;
  rpc_port : int;
  debug : bool;
  rollup_node_endpoint : Uri.t;
  mode : mode;
  cors_origins : string list;
  cors_headers : string list;
  verbose : bool;
}

let default_config =
  {
    rpc_addr = "127.0.0.1";
    rpc_port = 8545;
    debug = true;
    rollup_node_endpoint = Uri.empty;
    mode = Prod;
    cors_origins = [];
    cors_headers = [];
    verbose = false;
  }

let make_config ?mode ?rpc_addr ?rpc_port ?debug ?cors_origins ?cors_headers
    ~rollup_node_endpoint ~verbose () =
  {
    rpc_addr = Option.value ~default:default_config.rpc_addr rpc_addr;
    rpc_port = Option.value ~default:default_config.rpc_port rpc_port;
    debug = Option.value ~default:default_config.debug debug;
    rollup_node_endpoint;
    mode = Option.value ~default:default_config.mode mode;
    cors_origins =
      Option.value ~default:default_config.cors_origins cors_origins;
    cors_headers =
      Option.value ~default:default_config.cors_headers cors_headers;
    verbose;
  }

let install_finalizer_prod server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let+ () = Tezos_rpc_http_server.RPC_server.shutdown server in
  Format.printf "Server exited with code %d\n%!" exit_status

let install_finalizer_dev server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  Format.printf "Server exited with code %d\n%!" exit_status ;
  let+ () = Evm_proxy_lib_dev.Tx_pool.shutdown () in
  Format.printf "Shutting down Tx-Pool\n%!"

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

let rollup_node_config_prod ~rollup_node_endpoint =
  let open Lwt_result_syntax in
  let open Evm_proxy_lib_prod in
  let module Rollup_node_rpc = Rollup_node.Make (struct
    let base = rollup_node_endpoint
  end) in
  let* smart_rollup_address = Rollup_node_rpc.smart_rollup_address in
  return ((module Rollup_node_rpc : Rollup_node.S), smart_rollup_address)

let rollup_node_config_dev ~rollup_node_endpoint =
  let open Lwt_result_syntax in
  let open Evm_proxy_lib_dev in
  let module Rollup_node_rpc = Rollup_node.Make (struct
    let base = rollup_node_endpoint
  end) in
  let* smart_rollup_address = Rollup_node_rpc.smart_rollup_address in
  return ((module Rollup_node_rpc : Rollup_node.S), smart_rollup_address)

let prod_directory ~verbose rollup_node_config =
  let open Lwt_result_syntax in
  let open Evm_proxy_lib_prod in
  return @@ Services.directory ~verbose rollup_node_config

let dev_directory ~verbose rollup_node_config =
  let open Lwt_result_syntax in
  let open Evm_proxy_lib_dev in
  return @@ Services.directory ~verbose rollup_node_config

let start {rpc_addr; rpc_port; debug; cors_origins; cors_headers; _} ~directory
    =
  let open Lwt_result_syntax in
  let open Tezos_rpc_http_server in
  let p2p_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string p2p_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  let cors =
    Resto_cohttp.Cors.
      {allowed_headers = cors_headers; allowed_origins = cors_origins}
  in
  let server =
    RPC_server.init_server
      ~acl
      ~cors
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

  let mode =
    Tezos_clic.parameter (fun _ s ->
        let mode =
          match s with
          | "prod" -> Prod
          | "dev" -> Dev
          | _ -> Stdlib.failwith "The mode must be prod or dev."
        in
        Lwt.return_ok mode)

  let rollup_node_endpoint =
    Tezos_clic.parameter (fun _ uri -> Lwt.return_ok (Uri.of_string uri))

  let string_list =
    Tezos_clic.parameter (fun _ s ->
        let list = String.split ',' s in
        Lwt.return_ok list)
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

let cors_allowed_headers_arg =
  Tezos_clic.arg
    ~long:"cors-headers"
    ~placeholder:"ALLOWED_HEADERS"
    ~doc:"List of accepted cors headers."
    Params.string_list

let cors_allowed_origins_arg =
  Tezos_clic.arg
    ~long:"cors-origin"
    ~placeholder:"ALLOWED_ORIGINS"
    ~doc:"List of accepted cors origins."
    Params.string_list

let mode_arg =
  Tezos_clic.arg
    ~long:"mode"
    ~placeholder:"MODE"
    ~doc:"The EVM proxy server mode, it's either prod or dev."
    Params.mode

let verbose_arg =
  Tezos_clic.switch
    ~short:'v'
    ~long:"verbose"
    ~doc:"If verbose is set, the proxy will display the responses to RPCs."
    ()

let rollup_node_endpoint_param =
  Tezos_clic.param
    ~name:"rollup-node-endpoint"
    ~desc:
      "The smart rollup node endpoint address (as ADDR:PORT) the proxy server \
       will communicate with, or [mockup] if you want to use mock values."
    Params.rollup_node_endpoint

let rollup_address_arg =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  parameter (fun _ hash ->
      let hash_opt =
        Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_opt hash
      in
      match hash_opt with
      | Some hash -> return hash
      | None ->
          failwith
            "Parameter '%s' is an invalid smart rollup address encoded in a \
             base58 string."
            hash)
  |> default_arg
       ~long:"rollup-address"
       ~doc:
         "The smart rollup address in Base58 encoding used to produce the \
          chunked messages"
       ~default:Tezos_crypto.Hashed.Smart_rollup_address.(to_b58check zero)
       ~placeholder:"sr1..."

let data_parameter =
  Tezos_clic.param
    ~name:"data"
    ~desc:"Data to prepare and chunk with the EVM rollup format"
    Params.string

let main_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Start the RPC server"
    (args6
       mode_arg
       rpc_addr_arg
       rpc_port_arg
       cors_allowed_origins_arg
       cors_allowed_headers_arg
       verbose_arg)
    (prefixes ["run"; "with"; "endpoint"] @@ rollup_node_endpoint_param @@ stop)
    (fun (mode, rpc_addr, rpc_port, cors_origins, cors_headers, verbose)
         rollup_node_endpoint
         () ->
      let*! () = Tezos_base_unix.Internal_event_unix.init () in
      let*! () = Internal_event.Simple.emit Event.event_starting () in
      let config =
        make_config
          ?mode
          ?rpc_addr
          ?rpc_port
          ?cors_origins
          ?cors_headers
          ~rollup_node_endpoint
          ~verbose
          ()
      in
      let* () =
        match config.mode with
        | Prod ->
            let* rollup_config =
              rollup_node_config_prod ~rollup_node_endpoint
            in
            let* () = Evm_proxy_lib_prod.Tx_pool.start rollup_config in
            let* directory =
              prod_directory ~verbose:config.verbose rollup_config
            in
            let* server = start config ~directory in
            let (_ : Lwt_exit.clean_up_callback_id) =
              install_finalizer_prod server
            in
            return_unit
        | Dev ->
            let* rollup_config = rollup_node_config_dev ~rollup_node_endpoint in
            let* () = Evm_proxy_lib_dev.Tx_pool.start rollup_config in
            let* directory =
              dev_directory ~verbose:config.verbose rollup_config
            in
            let* server = start config ~directory in
            let (_ : Lwt_exit.clean_up_callback_id) =
              install_finalizer_dev server
            in
            return_unit
      in
      let wait, _resolve = Lwt.wait () in
      let* () = wait in
      return_unit)

let make_prod_messages ~smart_rollup_address s =
  let open Lwt_result_syntax in
  let open Evm_proxy_lib_prod in
  let*? _, messages =
    Rollup_node.make_encoded_messages
      ~smart_rollup_address
      (Evm_proxy_lib_prod_encoding.Ethereum_types.hex_of_string s)
  in
  return messages

let make_dev_messages ~smart_rollup_address s =
  let open Lwt_result_syntax in
  let open Evm_proxy_lib_dev in
  let*? _, messages =
    Rollup_node.make_encoded_messages
      ~smart_rollup_address
      (Evm_proxy_lib_dev_encoding.Ethereum_types.hex_of_string s)
  in
  return messages

let chunker_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:
      "Chunk hexadecimal data according to the message representation of the \
       EVM rollup"
    (args2 mode_arg rollup_address_arg)
    (prefixes ["chunk"; "data"] @@ data_parameter @@ stop)
    (fun (mode, rollup_address) data () ->
      let print_chunks smart_rollup_address s =
        let* messages =
          match Option.value ~default:Prod mode with
          | Prod -> make_prod_messages ~smart_rollup_address s
          | Dev -> make_dev_messages ~smart_rollup_address s
        in
        Format.printf "Chunked transactions :\n%!" ;
        List.iter (Format.printf "%s\n%!") messages ;
        return_unit
      in
      let rollup_address =
        Tezos_crypto.Hashed.Smart_rollup_address.to_string rollup_address
      in
      print_chunks rollup_address data)

(* List of program commands *)
let commands = [main_command; chunker_command]

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
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Lwt_main.run (dispatch () (argv ())) |> handle_error
