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
      ~name:"start"
      ~msg:"starting the EVM node ({mode})"
      ~level:Notice
      ("mode", Data_encoding.string)

  let event_is_ready =
    Internal_event.Simple.declare_2
      ~section
      ~name:"is_ready"
      ~msg:"the EVM node is listening to {addr}:{port}"
      ~level:Notice
      ("addr", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let event_retrying_connect =
    Internal_event.Simple.declare_2
      ~section
      ~name:"retrying_connect"
      ~msg:"Cannot connect to {endpoint}, retrying in {delay} seconds.."
      ~level:Notice
      ("endpoint", Data_encoding.string)
      ("delay", Data_encoding.float)

  let event_shutdown_node =
    Internal_event.Simple.declare_1
      ~section
      ~name:"shutting_down"
      ~msg:"Stopping the EVM node"
      ~level:Notice
      ("exit_status", Data_encoding.int8)

  let event_shutdown_tx_pool =
    Internal_event.Simple.declare_0
      ~section
      ~name:"shutting_down_tx_pool"
      ~msg:"Stopping the tx-pool"
      ~level:Notice
      ()

  let event_shutdown_rpc_server ~private_ =
    let server = if private_ then "private" else "public" in
    Internal_event.Simple.declare_0
      ~section
      ~name:("shutting_down_" ^ server ^ "_rpc_server")
      ~msg:("Stopping the" ^ server ^ " RPC server")
      ~level:Notice
      ()

  let event_callback_log =
    Internal_event.Simple.declare_3
      ~section
      ~name:"callback_log"
      ~msg:"Uri: {uri}\nMethod: {method}\nBody: {body}\n"
      ~level:Debug
      ("uri", Data_encoding.string)
      ("method", Data_encoding.string)
      ("body", Data_encoding.string)
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
  let* () = emit (Event.event_shutdown_rpc_server ~private_:false) () in
  let* () = Evm_node_lib_prod.Tx_pool.shutdown () in
  emit Event.event_shutdown_tx_pool ()

let install_finalizer_dev server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = emit Event.event_shutdown_node exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = emit (Event.event_shutdown_rpc_server ~private_:false) () in
  let* () = Evm_node_lib_dev.Tx_pool.shutdown () in
  Evm_node_lib_dev.Tx_pool_events.shutdown ()

let install_finalizer_seq server private_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Internal_event.Simple.emit Event.event_shutdown_node exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = emit (Event.event_shutdown_rpc_server ~private_:false) () in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown private_server in
  let* () = emit (Event.event_shutdown_rpc_server ~private_:true) () in
  let* () = Evm_node_lib_dev.Tx_pool.shutdown () in
  let* () = Evm_node_lib_dev.Tx_pool_events.shutdown () in
  let* () = Evm_node_lib_dev.Blueprints_publisher.shutdown () in
  let* () = Evm_node_lib_dev.Blueprint_events.publisher_shutdown () in
  let* () = Evm_node_lib_dev.Delayed_inbox.shutdown () in
  Evm_node_lib_dev.Delayed_inbox_events.shutdown ()

let install_finalizer_observer server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Internal_event.Simple.emit Event.event_shutdown_node exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = emit (Event.event_shutdown_rpc_server ~private_:false) () in
  return_unit

let callback_log server conn req body =
  let open Cohttp in
  let open Lwt_syntax in
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  let* body_str = body |> Cohttp_lwt.Body.to_string in
  let* () = emit Event.event_callback_log (uri, meth, body_str) in
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
      Rollup_node_services.smart_rollup_address
      rollup_node_endpoint
  in
  return
    ((module Rollup_node_rpc : Services_backend_sig.S), smart_rollup_address)

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

let start {rpc_addr; rpc_port; cors_origins; cors_headers; _} ~directory =
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
        RPC_server.launch ~host server ~callback:(callback_log server) node
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
        RPC_server.launch ~host server ~callback:(callback_log server) node
      in
      let*! () =
        RPC_server.launch
          ~host:Ipaddr.V4.(to_string localhost)
          private_server
          ~callback:(callback_log private_server)
          private_node
      in
      let*! () =
        Internal_event.Simple.emit Event.event_is_ready (rpc_addr, rpc_port)
      in
      return (server, private_server))
    (fun _ -> return (server, private_server))

let observer_start
    {rpc_addr; rpc_port; cors_origins; cors_headers; mode = (_ : observer); _}
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
  let*! () =
    RPC_server.launch ~host server ~callback:(callback_log server) node
  in
  let*! () =
    Internal_event.Simple.emit Event.event_is_ready (rpc_addr, rpc_port)
  in
  return server

module Params = struct
  let string = Tezos_clic.parameter (fun _ s -> Lwt.return_ok s)

  let int = Tezos_clic.parameter (fun _ s -> Lwt.return_ok (int_of_string s))

  let endpoint =
    Tezos_clic.parameter (fun _ uri -> Lwt.return_ok (Uri.of_string uri))

  let rollup_node_endpoint = endpoint

  let evm_node_endpoint = endpoint

  let secret_key : (Signature.secret_key, unit) Tezos_clic.parameter =
    Tezos_clic.parameter (fun _ sk ->
        Lwt.return_ok (Signature.Secret_key.of_b58check_exn sk))

  let string_list =
    Tezos_clic.parameter (fun _ s ->
        let list = String.split ',' s in
        Lwt.return_ok list)

  let time_between_blocks =
    Tezos_clic.parameter (fun _ s ->
        let time_between_blocks =
          if s = "none" then Nothing
          else Time_between_blocks (Float.of_string s)
        in
        Lwt.return_ok time_between_blocks)

  let timestamp =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun _ timestamp ->
        let timestamp = String.trim timestamp in
        match Time.Protocol.of_notation timestamp with
        | Some t -> return t
        | None -> (
            match
              Int64.of_string_opt timestamp
              |> Option.map Time.Protocol.of_seconds
            with
            | Some t -> return t
            | None ->
                failwith
                  "Timestamp must be either in RFC3399 format  (e.g., \
                   [\"1970-01-01T00:00:00Z\"]) or in number of seconds since \
                   the {!Time.Protocol.epoch}."))
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

let maximum_blueprints_lag_arg =
  Tezos_clic.default_arg
    ~long:"maximum-blueprints-lag"
    ~placeholder:"LAG"
    ~default:"500"
    ~doc:
      "The maximum advance (in blueprints) the Sequencer accepts to have \
       before trying to send its backlog again."
    Params.int

let maximum_blueprints_catchup_arg =
  Tezos_clic.default_arg
    ~long:"maximum-blueprints-catch-up"
    ~placeholder:"CATCH_UP"
    ~default:"1_000"
    ~doc:"The maximum number of blueprints the Sequencer resends at once."
    Params.int

let catchup_cooldown_arg =
  Tezos_clic.default_arg
    ~long:"catch-up-cooldown"
    ~placeholder:"COOLDOWN"
    ~default:"10"
    ~doc:
      "The maximum number of Layer 1 blocks the Sequencer waits after \
       resending its blueprints before trying to catch-up again."
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
    ~doc:"Sets logging level to debug. Beware, it is highly verbose."
    ()

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Tezos_clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:"The path to the EVM node data directory"
    ~default
    Params.string

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

let kernel_arg =
  Tezos_clic.arg
    ~long:"initial-kernel"
    ~placeholder:"evm_installer.wasm"
    ~doc:
      "Path to the EVM kernel used to launch the PVM, it will be loaded from \
       storage afterward"
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
    Params.time_between_blocks

let keep_alive_arg =
  Tezos_clic.switch
    ~doc:
      "Keep the EVM node process alive even if the connection is lost with the \
       rollup node."
    ~short:'K'
    ~long:"keep-alive"
    ()

let blueprint_mode_arg =
  Tezos_clic.switch
    ~long:"as-blueprint"
    ~doc:"Chunk the data into a blueprint usable in sequencer mode"
    ()

let timestamp_arg =
  Params.timestamp
  |> Tezos_clic.default_arg
       ~long:"timestamp"
       ~doc:""
       ~placeholder:"1970-01-01T00:00:00Z"
       ~default:"0"

let genesis_timestamp_arg =
  Params.timestamp
  |> Tezos_clic.arg
       ~long:"genesis-timestamp"
       ~doc:
         "Timestamp used for the genesis block, uses machine's clock if not \
          provided"
       ~placeholder:"1970-01-01T00:00:00Z"

let blueprint_number_arg =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  parameter (fun _ number ->
      try String.trim number |> Z.of_string |> return
      with _ -> failwith "Blueprint number must be an integer")
  |> default_arg
       ~long:"number"
       ~doc:"Level of the blueprint"
       ~placeholder:"0"
       ~default:"0"

let parent_hash_arg =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  parameter (fun _ hash -> return hash)
  |> default_arg
       ~long:"parent-hash"
       ~doc:"Blueprint's parent hash"
       ~placeholder:
         "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
       ~default:
         "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

let secret_key_arg =
  let open Tezos_clic in
  (* This is `Bootstrap.bootstrap1.secret_key` in
     `tezt/lib_tezos/account.ml`. *)
  let default_sk = "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh" in
  Params.secret_key
  |> default_arg
       ~long:"secret-key"
       ~doc:"Unencrypted secret key to sign the blueprints."
       ~placeholder:"edsk..."
       ~default:default_sk

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
    @@ param
         ~name:"rollup-node-endpoint"
         ~desc:
           "The smart rollup node endpoint address (as ADDR:PORT) the node \
            will communicate with."
         Params.rollup_node_endpoint
    @@ stop)
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
      let*! () =
        let open Tezos_base_unix.Internal_event_unix in
        let config =
          if verbose then Some (make_with_defaults ~verbosity:Debug ())
          else None
        in
        init ?config ()
      in
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
          ()
      in
      let* () = Configuration.save_proxy ~force:true ~data_dir config in
      let* () =
        if not config.devmode then
          let* ((backend_rpc, smart_rollup_address) as rollup_config) =
            rollup_node_config_prod ~rollup_node_endpoint ~keep_alive
          in
          let* () =
            Evm_node_lib_prod.Tx_pool.start
              {
                rollup_node = backend_rpc;
                smart_rollup_address;
                mode = Proxy {rollup_node_endpoint};
              }
          in
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
      match time_between_blocks with
      | Nothing -> false
      | Time_between_blocks time_between_blocks ->
          let diff = Time.Protocol.(diff now last_produced_block) in
          diff >= Int64.of_float time_between_blocks
    in
    let* nb_transactions = Tx_pool.produce_block ~force ~timestamp:now in
    let*! () = Lwt_unix.sleep 0.5 in
    if nb_transactions > 0 || force then loop now else loop last_produced_block
  in
  let now = Helpers.now () in
  loop now

let sequencer_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Start the EVM node in sequencer mode"
    (args14
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       private_rpc_port_arg
       cors_allowed_origins_arg
       cors_allowed_headers_arg
       verbose_arg
       kernel_arg
       preimages_arg
       time_between_blocks_arg
       genesis_timestamp_arg
       maximum_blueprints_lag_arg
       maximum_blueprints_catchup_arg
       catchup_cooldown_arg)
    (prefixes ["run"; "sequencer"; "with"; "endpoint"]
    @@ param
         ~name:"rollup-node-endpoint"
         ~desc:
           "The smart rollup node endpoint address (as ADDR:PORT) the node \
            will communicate with."
         Params.rollup_node_endpoint
    @@ prefixes ["signing"; "with"]
    @@ param
         ~name:"secret-key"
         ~desc:
           "The tezos secret key used to sign the blueprints published to L1"
         Params.secret_key
    @@ stop)
    (fun ( data_dir,
           rpc_addr,
           rpc_port,
           private_rpc_port,
           cors_origins,
           cors_headers,
           verbose,
           kernel,
           preimages,
           time_between_blocks,
           genesis_timestamp,
           max_blueprints_lag,
           max_blueprints_catchup,
           catchup_cooldown )
         rollup_node_endpoint
         sequencer
         () ->
      let*! () =
        let open Tezos_base_unix.Internal_event_unix in
        let verbosity = if verbose then Some Internal_event.Debug else None in
        let config =
          make_with_defaults
            ?verbosity
            ~enable_default_daily_logs_at:
              Filename.Infix.(data_dir // "daily_logs")
              (* Show only above Info rpc_server events, they are not
                 relevant as we do not have a REST-API server. If not
                 set, the daily logs are polluted with these
                 uninformative logs. *)
            ~daily_logs_section_prefixes:
              [
                ("rpc_server", Notice);
                ("rpc_server", Warning);
                ("rpc_server", Error);
                ("rpc_server", Fatal);
              ]
            ()
        in
        init ~config ()
      in
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
          ?preimages
          ?time_between_blocks
          ~sequencer
          ()
      in
      let* () = Configuration.save_sequencer ~force:true ~data_dir config in
      let open Evm_node_lib_dev in
      let* smart_rollup_address =
        Rollup_node_services.smart_rollup_address rollup_node_endpoint
      in
      let* () =
        Blueprints_publisher.start
          ~rollup_node_endpoint
          ~max_blueprints_lag
          ~max_blueprints_catchup
          ~catchup_cooldown
          (Blueprint_store.make ~data_dir)
      in
      let* ctxt =
        Evm_context.init
          ?genesis_timestamp
          ~produce_genesis_with:sequencer
          ~data_dir
          ?kernel_path:kernel
          ~preimages:config.mode.preimages
          ~smart_rollup_address
          ()
      in
      let module Sequencer = Sequencer.Make (struct
        let ctxt = ctxt

        let secret_key = sequencer
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
      let* () =
        Delayed_inbox.start {rollup_node_endpoint; delayed_inbox_interval = 1}
      in
      let* directory =
        dev_directory config ((module Sequencer), smart_rollup_address)
      in
      let directory = directory |> Evm_services.register ctxt in
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

let observer_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Start the EVM node in observer mode"
    (args8
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       cors_allowed_origins_arg
       cors_allowed_headers_arg
       verbose_arg
       kernel_arg
       preimages_arg)
    (prefixes ["run"; "observer"; "with"; "endpoint"]
    @@ param
         ~name:"evm-node-endpoint"
         ~desc:
           "The EVM node endpoint address (as ADDR:PORT) the node will \
            communicate with."
         Params.evm_node_endpoint
    @@ stop)
  @@ fun ( data_dir,
           rpc_addr,
           rpc_port,
           cors_origins,
           cors_headers,
           verbose,
           kernel,
           preimages )
             evm_node_endpoint
             () ->
  let open Evm_node_lib_dev in
  let*! () =
    let open Tezos_base_unix.Internal_event_unix in
    let config =
      if verbose then Some (make_with_defaults ~verbosity:Debug ()) else None
    in
    init ?config ()
  in
  let*! () = Internal_event.Simple.emit Event.event_starting "observer" in
  let* config =
    Cli.create_or_read_observer_config
      ~data_dir
      ~devmode:true
      ?rpc_addr
      ?rpc_port
      ?cors_origins
      ?cors_headers
      ~evm_node_endpoint
      ?preimages
      ()
  in
  let* () = Configuration.save_observer ~force:true ~data_dir config in

  let* smart_rollup_address =
    Evm_services.get_smart_rollup_address ~evm_node_endpoint
  in

  let* ctxt =
    Evm_context.init
      ~data_dir
      ?kernel_path:kernel
      ~preimages:config.mode.preimages
      ~smart_rollup_address:
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
      ()
  in

  let observer_backend =
    (module Observer.Make (struct
      let ctxt = ctxt
    end) : Services_backend_sig.S)
  in

  let* directory =
    dev_directory config (observer_backend, smart_rollup_address)
  in
  let directory = directory |> Evm_services.register ctxt in

  let* server = observer_start config ~directory in

  let (_ : Lwt_exit.clean_up_callback_id) = install_finalizer_observer server in

  Observer.main ctxt ~evm_node_endpoint

let make_prod_messages ~smart_rollup_address s =
  let open Lwt_result_syntax in
  let open Evm_node_lib_prod in
  let s = Ethereum_types.hex_of_string s in
  let*? _, messages =
    Transaction_format.make_encoded_messages
      ~smart_rollup_address
      (Evm_node_lib_dev_encoding.Ethereum_types.hex_to_bytes s)
  in
  return (List.map (fun m -> m |> Hex.of_string |> Hex.show) messages)

let make_dev_messages ~kind ~smart_rollup_address s =
  let open Lwt_result_syntax in
  let open Evm_node_lib_dev in
  let s = Ethereum_types.hex_of_string s in
  let*? messages =
    match kind with
    | `Blueprint (secret_key, timestamp, number, parent_hash) ->
        let Sequencer_blueprint.{to_publish; _} =
          Sequencer_blueprint.create
            ~secret_key
            ~timestamp
            ~smart_rollup_address
            ~number:(Ethereum_types.quantity_of_z number)
            ~parent_hash:(Ethereum_types.block_hash_of_string parent_hash)
            ~transactions:
              [Evm_node_lib_dev_encoding.Ethereum_types.hex_to_bytes s]
            ~delayed_transactions:[]
        in
        to_publish |> List.map (fun (`External s) -> s) |> Result.ok
    | `Transaction ->
        Transaction_format.make_encoded_messages
          ~smart_rollup_address
          (Evm_node_lib_dev_encoding.Ethereum_types.hex_to_bytes s)
        |> Result.map snd
  in
  return (List.map (fun m -> m |> Hex.of_string |> Hex.show) messages)

let chunker_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:
      "Chunk hexadecimal data according to the message representation of the \
       EVM rollup"
    (args7
       devmode_arg
       rollup_address_arg
       blueprint_mode_arg
       timestamp_arg
       blueprint_number_arg
       parent_hash_arg
       secret_key_arg)
    (prefixes ["chunk"; "data"]
    @@ param
         ~name:"data"
         ~desc:"Data to prepare and chunk with the EVM rollup format"
         Params.string
    @@ stop)
    (fun ( devmode,
           rollup_address,
           as_blueprint,
           blueprint_timestamp,
           blueprint_number,
           blueprint_parent_hash,
           secret_key )
         data
         () ->
      let kind =
        if as_blueprint then
          `Blueprint
            ( secret_key,
              blueprint_timestamp,
              blueprint_number,
              blueprint_parent_hash )
        else `Transaction
      in
      let print_chunks smart_rollup_address s =
        let* messages =
          if devmode then make_dev_messages ~kind ~smart_rollup_address s
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

let make_upgrade_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Create bytes payload for the upgrade entrypoint"
    no_options
    (prefixes ["make"; "upgrade"; "payload"; "with"; "root"; "hash"]
    @@ param
         ~name:"preimage_hash"
         ~desc:"Root hash of the kernel to upgrade to"
         Params.string
    @@ prefixes ["at"; "activation"; "timestamp"]
    @@ param
         ~name:"activation_timestamp"
         ~desc:
           "After activation timestamp, the kernel will upgrade to this value"
         Params.timestamp
    @@ stop)
    (fun () root_hash activation_timestamp () ->
      let open Rlp in
      let activation_timestamp =
        Evm_node_lib_dev.Helpers.timestamp_to_bytes activation_timestamp
      in
      let root_hash_bytes = Hex.to_bytes_exn (`Hex root_hash) in
      let kernel_upgrade =
        List [Value root_hash_bytes; Value activation_timestamp]
      in
      let payload = encode kernel_upgrade in
      Printf.printf "%s" Hex.(of_bytes payload |> show) ;
      return_unit)

let init_from_rollup_node_command =
  let open Tezos_clic in
  let rollup_node_data_dir_param =
    Tezos_clic.param
      ~name:"rollup-node-data-dir"
      ~desc:(Format.sprintf "The path to the rollup node data directory.")
      Params.string
  in
  command
    ~desc:
      "initialises the EVM node data-dir using the data-dir of a rollup node."
    (args1 data_dir_arg)
    (prefixes ["init"; "from"; "rollup"; "node"]
    @@ rollup_node_data_dir_param @@ stop)
    (fun data_dir rollup_node_data_dir () ->
      Evm_node_lib_dev.Evm_context.init_from_rollup_node
        ~data_dir
        ~rollup_node_data_dir)

(* List of program commands *)
let commands =
  [
    proxy_command;
    sequencer_command;
    observer_command;
    chunker_command;
    make_upgrade_command;
    init_from_rollup_node_command;
  ]

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
  let _ =
    Tezos_clic.(
      setup_formatter
        Format.err_formatter
        (if Unix.isatty Unix.stderr then Ansi else Plain)
        Short)
  in
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Lwt_main.run (dispatch () (argv ())) |> handle_error
