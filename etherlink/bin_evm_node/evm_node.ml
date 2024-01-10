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
open Configuration

module Event = struct
  let section = ["evm_node"]

  let event_starting =
    Internal_event.Simple.declare_1
      ~section
      ~name:"evm_node_start"
      ~msg:"starting the EVM node ({mode})"
      ~level:Notice
      ("mode", Data_encoding.string)

  let event_is_ready =
    Internal_event.Simple.declare_2
      ~section
      ~name:"evm_node_is_ready"
      ~msg:"the EVM node is listening to {addr}:{port}"
      ~level:Notice
      ("addr", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let event_retrying_connect =
    Internal_event.Simple.declare_2
      ~section
      ~name:"evm_node_retrying_connect"
      ~msg:"Cannot connect to {endpoint}, retrying in {delay} seconds.."
      ~level:Notice
      ("endpoint", Data_encoding.string)
      ("delay", Data_encoding.float)

  let event_shutdown_node =
    Internal_event.Simple.declare_1
      ~section
      ~name:"evm_node_shutting_down"
      ~msg:"Stopping the EVM node"
      ~level:Notice
      ("exit_status", Data_encoding.int8)

  let event_shutdown_rpc_server ~private_ =
    let server = if private_ then "private" else "public" in
    Internal_event.Simple.declare_0
      ~section
      ~name:("evm_node_shutting_down_" ^ server ^ "_rpc_server")
      ~msg:("Stopping the" ^ server ^ " RPC server")
      ~level:Notice
      ()

  let event_shutdown_tx_pool =
    Internal_event.Simple.declare_0
      ~section
      ~name:"evm_node_shutting_down_tx_pool"
      ~msg:"Stopping the tx-pool"
      ~level:Notice
      ()
end

let emit = Internal_event.Simple.emit

(** [retry_connection f] retries the connection using [f]. If an error
    happens in [f] and it is a lost connection, the connection is retried  *)
let retry_connection (f : Uri.t -> string tzresult Lwt.t) endpoint :
    string tzresult Lwt.t =
  let open Lwt_result_syntax in
  let rec retry ~delay =
    let*! result = f endpoint in
    match result with
    | Ok smart_rollup_address -> return smart_rollup_address
    | Error err
      when Evm_node_lib_dev.Rollup_node_services.is_connection_error err ->
        let*! () =
          emit Event.event_retrying_connect (Uri.to_string endpoint, delay)
        in
        let*! () = Lwt_unix.sleep delay in
        let next_delay = delay *. 2. in
        let delay = Float.min next_delay 30. in
        retry ~delay
    | res -> Lwt.return res
  in
  retry ~delay:1.

(** [fetch_smart_rollup_address ~keep_alive f] tries to fetch the
    smart rollup address using [f]. If [keep_alive] is true, tries to
    fetch until it works. *)
let fetch_smart_rollup_address ~keep_alive f (endpoint : Uri.t) =
  if keep_alive then retry_connection f endpoint else f endpoint

let install_finalizer_prod server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = emit Event.event_shutdown_node exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  emit (Event.event_shutdown_rpc_server ~private_:false) ()

let install_finalizer_dev server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = emit Event.event_shutdown_node exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = emit (Event.event_shutdown_rpc_server ~private_:false) () in
  let* () = Evm_node_lib_dev.Tx_pool.shutdown () in
  emit Event.event_shutdown_tx_pool ()

let install_finalizer_seq server private_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Internal_event.Simple.emit Event.event_shutdown_node exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = emit (Event.event_shutdown_rpc_server ~private_:false) () in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown private_server in
  let* () = emit (Event.event_shutdown_rpc_server ~private_:true) () in
  let* () = Evm_node_lib_dev.Tx_pool.shutdown () in
  emit Event.event_shutdown_tx_pool ()

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

let rollup_node_config_prod ~rollup_node_endpoint ~keep_alive =
  let open Lwt_result_syntax in
  let open Evm_node_lib_prod in
  let module Rollup_node_rpc = Rollup_node.Make (struct
    let base = rollup_node_endpoint
  end) in
  let* smart_rollup_address =
    fetch_smart_rollup_address
      ~keep_alive
      (fun _endpoint -> Rollup_node_rpc.smart_rollup_address)
      rollup_node_endpoint
  in
  return ((module Rollup_node_rpc : Rollup_node.S), smart_rollup_address)

let rollup_node_config_dev ~rollup_node_endpoint ~keep_alive =
  let open Lwt_result_syntax in
  let open Evm_node_lib_dev in
  let module Rollup_node_rpc = Rollup_node.Make (struct
    let base = rollup_node_endpoint
  end) in
  let* smart_rollup_address =
    fetch_smart_rollup_address
      ~keep_alive
      Rollup_node_services.smart_rollup_address
      rollup_node_endpoint
  in
  return
    ((module Rollup_node_rpc : Services_backend_sig.S), smart_rollup_address)

let prod_directory config rollup_node_config =
  let open Lwt_result_syntax in
  let open Evm_node_lib_prod in
  return @@ Services.directory config rollup_node_config

let dev_directory config rollup_node_config =
  let open Lwt_result_syntax in
  let open Evm_node_lib_dev in
  return @@ Services.directory config rollup_node_config

let dev_private_directory config rollup_node_config =
  let open Evm_node_lib_dev in
  Services.private_directory config rollup_node_config

let start {rpc_addr; rpc_port; cors_origins; cors_headers; verbose; _}
    ~directory =
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
            (if verbose then callback_log server
            else RPC_server.resto_callback server)
          node
      in
      let*! () =
        Internal_event.Simple.emit Event.event_is_ready (rpc_addr, rpc_port)
      in
      return server)
    (fun _ -> return server)

let seq_start
    {
      rpc_addr;
      rpc_port;
      cors_origins;
      cors_headers;
      verbose;
      mode = {private_rpc_port; _};
      _;
    } ~directory ~private_directory =
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
  let private_node = `TCP (`Port private_rpc_port) in
  let private_server =
    RPC_server.init_server
      ~acl
      ~cors
      ~media_types:Media_type.all_media_types
      private_directory
  in
  Lwt.catch
    (fun () ->
      let*! () =
        RPC_server.launch
          ~host
          server
          ~callback:
            (if verbose then callback_log server
            else RPC_server.resto_callback server)
          node
      in
      let*! () =
        RPC_server.launch
          ~host:Ipaddr.V4.(to_string localhost)
          private_server
          ~callback:
            (if verbose then callback_log private_server
            else RPC_server.resto_callback private_server)
          private_node
      in
      let*! () =
        Internal_event.Simple.emit Event.event_is_ready (rpc_addr, rpc_port)
      in
      return (server, private_server))
    (fun _ -> return (server, private_server))

module Params = struct
  let string = Tezos_clic.parameter (fun _ s -> Lwt.return_ok s)

  let int = Tezos_clic.parameter (fun _ s -> Lwt.return_ok (int_of_string s))

  let rollup_node_endpoint =
    Tezos_clic.parameter (fun _ uri -> Lwt.return_ok (Uri.of_string uri))

  let string_list =
    Tezos_clic.parameter (fun _ s ->
        let list = String.split ',' s in
        Lwt.return_ok list)

  let float =
    Tezos_clic.parameter (fun _ s -> Lwt.return_ok (Float.of_string s))
end

let rpc_addr_arg =
  Tezos_clic.arg
    ~long:"rpc-addr"
    ~placeholder:"ADDR"
    ~doc:"The EVM node server rpc address."
    Params.string

let rpc_port_arg =
  Tezos_clic.arg
    ~long:"rpc-port"
    ~placeholder:"PORT"
    ~doc:"The EVM node server rpc port."
    Params.int

let private_rpc_port_arg =
  Tezos_clic.arg
    ~long:"private-rpc-port"
    ~placeholder:"PORT"
    ~doc:"The EVM node private server rpc port."
    Params.int

let cors_allowed_headers_arg =
  Tezos_clic.arg
    ~long:"cors-headers"
    ~placeholder:"ALLOWED_HEADERS"
    ~doc:"List of accepted cors headers."
    Params.string_list

let cors_allowed_origins_arg =
  Tezos_clic.arg
    ~long:"cors-origins"
    ~placeholder:"ALLOWED_ORIGINS"
    ~doc:"List of accepted cors origins."
    Params.string_list

let devmode_arg =
  Tezos_clic.switch ~long:"devmode" ~doc:"The EVM node in development mode." ()

let verbose_arg =
  Tezos_clic.switch
    ~short:'v'
    ~long:"verbose"
    ~doc:"If verbose is set, the node will display the responses to RPCs."
    ()

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Tezos_clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:
      (Format.sprintf
         "The path to the EVM node data directory. Default value is %s"
         default)
    ~default
    Params.string

let rollup_node_endpoint_param =
  Tezos_clic.param
    ~name:"rollup-node-endpoint"
    ~desc:
      "The smart rollup node endpoint address (as ADDR:PORT) the node will \
       communicate with."
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

let kernel_arg =
  Tezos_clic.arg
    ~long:"kernel"
    ~placeholder:"evm_installer.wasm"
    ~doc:"Path to the EVM kernel"
    Params.string

let preimages_arg =
  Tezos_clic.arg
    ~long:"preimage-dir"
    ~doc:"Path to the preimages directory"
    ~placeholder:"_evm_installer_preimages"
    Params.string

let time_between_blocks_arg =
  Tezos_clic.arg
    ~long:"time-between-blocks"
    ~doc:"Interval at which the sequencer creates an empty block by default."
    ~placeholder:"10."
    Params.float

let keep_alive_arg =
  Tezos_clic.switch
    ~doc:
      "Keep the EVM node process alive even if the connection is lost with the \
       rollup node."
    ~short:'K'
    ~long:"keep-alive"
    ()

let proxy_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Start the EVM node in proxy mode"
    (args8
       data_dir_arg
       devmode_arg
       rpc_addr_arg
       rpc_port_arg
       cors_allowed_origins_arg
       cors_allowed_headers_arg
       verbose_arg
       keep_alive_arg)
    (prefixes ["run"; "proxy"; "with"; "endpoint"]
    @@ rollup_node_endpoint_param @@ stop)
    (fun ( data_dir,
           devmode,
           rpc_addr,
           rpc_port,
           cors_origins,
           cors_headers,
           verbose,
           keep_alive )
         rollup_node_endpoint
         () ->
      let*! () = Tezos_base_unix.Internal_event_unix.init () in
      let*! () = Internal_event.Simple.emit Event.event_starting "proxy" in
      let* config =
        Cli.create_or_read_proxy_config
          ~data_dir
          ~devmode
          ?rpc_addr
          ?rpc_port
          ?cors_origins
          ?cors_headers
          ~rollup_node_endpoint
          ~verbose
          ()
      in
      let* () = Configuration.save_proxy ~force:true ~data_dir config in
      let* () =
        if not config.devmode then
          let* rollup_config =
            rollup_node_config_prod ~rollup_node_endpoint ~keep_alive
          in
          let* () = Evm_node_lib_prod.Tx_pool.start rollup_config in
          let* directory = prod_directory config rollup_config in
          let* server = start config ~directory in
          let (_ : Lwt_exit.clean_up_callback_id) =
            install_finalizer_prod server
          in
          return_unit
        else
          let* ((backend_rpc, smart_rollup_address) as rollup_config) =
            rollup_node_config_dev ~rollup_node_endpoint ~keep_alive
          in
          let* () =
            Evm_node_lib_dev.Tx_pool.start
              {
                rollup_node = backend_rpc;
                smart_rollup_address;
                mode = Proxy {rollup_node_endpoint};
              }
          in
          let* directory = dev_directory config rollup_config in
          let* server = start config ~directory in
          let (_ : Lwt_exit.clean_up_callback_id) =
            install_finalizer_dev server
          in
          return_unit
      in
      let wait, _resolve = Lwt.wait () in
      let* () = wait in
      return_unit)

let main_sequencer : sequencer Configuration.t -> unit tzresult Lwt.t =
 fun config ->
  let open Lwt_result_syntax in
  let open Evm_node_lib_dev in
  let time_between_blocks = config.mode.time_between_blocks in
  let rec loop last_produced_block =
    let now = Helpers.now () in
    (* We force if the last produced block is older than [time_between_blocks]. *)
    let force =
      let diff = Time.Protocol.(diff now last_produced_block) in
      diff >= Int64.of_float time_between_blocks
    in
    let* nb_transactions = Tx_pool.produce_block ~force ~timestamp:now in
    let*! () = Lwt_unix.sleep 0.5 in
    if nb_transactions > 0 || force then loop now else loop last_produced_block
  in
  let now = Helpers.now () in
  let* _nb_transactions = Tx_pool.produce_block ~force:true ~timestamp:now in
  loop now

let sequencer_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Start the EVM node in sequencer mode"
    (args10
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       private_rpc_port_arg
       cors_allowed_origins_arg
       cors_allowed_headers_arg
       verbose_arg
       kernel_arg
       preimages_arg
       time_between_blocks_arg)
    (prefixes ["run"; "sequencer"; "with"; "endpoint"]
    @@ rollup_node_endpoint_param @@ stop)
    (fun ( data_dir,
           rpc_addr,
           rpc_port,
           private_rpc_port,
           cors_origins,
           cors_headers,
           verbose,
           kernel,
           preimages,
           time_between_blocks )
         rollup_node_endpoint
         () ->
      let*! () = Tezos_base_unix.Internal_event_unix.init () in
      let*! () = Internal_event.Simple.emit Event.event_starting "sequencer" in
      let* config =
        Cli.create_or_read_sequencer_config
          ~data_dir
          ~devmode:true
          ?rpc_addr
          ?rpc_port
          ?private_rpc_port
          ?cors_origins
          ?cors_headers
          ~rollup_node_endpoint
          ~verbose
          ?kernel
          ?preimages
          ?time_between_blocks
          ()
      in
      let* () = Configuration.save_sequencer ~force:true ~data_dir config in
      let open Evm_node_lib_dev in
      let* smart_rollup_address =
        Rollup_node_services.smart_rollup_address rollup_node_endpoint
      in
      let* ctxt, loaded =
        Sequencer_context.init
          ~data_dir
          ~kernel:config.mode.kernel
          ~preimages:config.mode.preimages
          ~smart_rollup_address
      in
      let* ctxt =
        if loaded then return ctxt
        else
          Sequencer_state.init ~rollup_node_endpoint ~smart_rollup_address ctxt
      in
      let module Sequencer = Sequencer.Make (struct
        let ctxt = ctxt

        let rollup_node_endpoint = rollup_node_endpoint
      end) in
      (* Ignore the smart rollup address for now. *)
      let* () =
        Tx_pool.start
          {
            rollup_node = (module Sequencer);
            smart_rollup_address;
            mode = Sequencer;
          }
      in
      let* directory =
        dev_directory config ((module Sequencer), smart_rollup_address)
      in
      let private_directory =
        dev_private_directory config ((module Sequencer), smart_rollup_address)
      in
      let* server, private_server =
        seq_start config ~directory ~private_directory
      in
      let (_ : Lwt_exit.clean_up_callback_id) =
        install_finalizer_seq server private_server
      in
      let* () = main_sequencer config in
      return_unit)

let make_prod_messages ~smart_rollup_address s =
  let open Lwt_result_syntax in
  let open Evm_node_lib_prod in
  let*? _, messages =
    Rollup_node.make_encoded_messages
      ~smart_rollup_address
      (Evm_node_lib_prod_encoding.Ethereum_types.hex_of_string s)
  in
  return messages

let make_dev_messages ~smart_rollup_address s =
  let open Lwt_result_syntax in
  let open Evm_node_lib_dev in
  let s = Ethereum_types.hex_of_string s in
  let*? _, messages =
    Transaction_format.make_encoded_messages
      ~smart_rollup_address
      (Evm_node_lib_dev_encoding.Ethereum_types.hex_to_bytes s)
  in
  return (List.map (fun m -> m |> Hex.of_string |> Hex.show) messages)

let chunker_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:
      "Chunk hexadecimal data according to the message representation of the \
       EVM rollup"
    (args2 devmode_arg rollup_address_arg)
    (prefixes ["chunk"; "data"] @@ data_parameter @@ stop)
    (fun (devmode, rollup_address) data () ->
      let print_chunks smart_rollup_address s =
        let* messages =
          if devmode then make_dev_messages ~smart_rollup_address s
          else make_prod_messages ~smart_rollup_address s
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
let commands = [proxy_command; sequencer_command; chunker_command]

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
      let devmode = Tezos_version_value.Bin_version.version_string in
      Format.printf "%s\n" devmode ;
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
