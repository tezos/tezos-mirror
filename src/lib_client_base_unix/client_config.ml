(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* Tezos Command line interface - Configuration and Arguments Parsing *)

type cli_args = {
  chain : Chain_services.chain;
  block : Shell_services.block;
  confirmations : int option;
  sources : Tezos_proxy.Light.sources_config option;
  password_filename : string option;
  protocol : Protocol_hash.t option;
  print_timings : bool;
  log_requests : bool;
  better_errors : bool;
  client_mode : client_mode;
}

and client_mode = [`Mode_client | `Mode_light | `Mode_mockup | `Mode_proxy]

let all_modes = [`Mode_client; `Mode_light; `Mode_mockup; `Mode_proxy]

let client_mode_to_string = function
  | `Mode_client -> "client"
  | `Mode_light -> "light"
  | `Mode_mockup -> "mockup"
  | `Mode_proxy -> "proxy"

type error += Invalid_endpoint_arg of string

type error += Invalid_media_type_arg of string

type error += Suppressed_arg of {args : string list; by : string}

type error += Invalid_chain_argument of string

type error += Invalid_block_argument of string

type error += Invalid_protocol_argument of string

type error += Invalid_port_arg of string

type error += Invalid_remote_signer_argument of string

type error += Invalid_wait_arg of string

type error += Invalid_mode_arg of string

let () =
  register_error_kind
    `Branch
    ~id:"badMediaTypeArgument"
    ~title:"Bad Media Type Argument"
    ~description:"Media type argument could not be parsed"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "media_type parameter must be 'json', 'binary' or 'any'. Got: %s"
        s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_media_type_arg s -> Some s | _ -> None)
    (fun s -> Invalid_media_type_arg s) ;
  register_error_kind
    `Branch
    ~id:"badEndpointArgument"
    ~title:"Bad Endpoint Argument"
    ~description:"Endpoint argument could not be parsed"
    ~pp:(fun ppf s -> Format.pp_print_string ppf s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_endpoint_arg s -> Some s | _ -> None)
    (fun s -> Invalid_endpoint_arg s) ;
  register_error_kind
    `Branch
    ~id:"suppressedArgument"
    ~title:"Suppressed Argument"
    ~description:"Certain arguments are conflicting with some other"
    ~pp:(fun ppf (args, by) ->
      Format.fprintf
        ppf
        (if List.compare_length_with args 1 = 0 then
         "Option %s is in conflict with %s"
        else "Options %s are in conflict with %s")
        (String.concat " and " args)
        by)
    Data_encoding.(obj2 (req "suppressed" (list string)) (req "by" string))
    (function Suppressed_arg e -> Some (e.args, e.by) | _ -> None)
    (fun (args, by) -> Suppressed_arg {args; by}) ;
  register_error_kind
    `Branch
    ~id:"badChainArgument"
    ~title:"Bad Chain Argument"
    ~description:"Chain argument could not be parsed"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "Value %s is not a value chain reference." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_chain_argument s -> Some s | _ -> None)
    (fun s -> Invalid_chain_argument s) ;
  register_error_kind
    `Branch
    ~id:"badBlockArgument"
    ~title:"Bad Block Argument"
    ~description:"Block argument could not be parsed"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "Value %s is not a value block reference." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_block_argument s -> Some s | _ -> None)
    (fun s -> Invalid_block_argument s) ;
  register_error_kind
    `Branch
    ~id:"badProtocolArgument"
    ~title:"Bad Protocol Argument"
    ~description:"Protocol argument could not be parsed"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "Value %s does not correspond to any known protocol." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_protocol_argument s -> Some s | _ -> None)
    (fun s -> Invalid_protocol_argument s) ;
  register_error_kind
    `Branch
    ~id:"invalidPortArgument"
    ~title:"Bad Port Argument"
    ~description:"Port argument could not be parsed"
    ~pp:(fun ppf s -> Format.fprintf ppf "Value %s is not a valid TCP port." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_port_arg s -> Some s | _ -> None)
    (fun s -> Invalid_port_arg s) ;
  register_error_kind
    `Branch
    ~id:"invalid_remote_signer_argument"
    ~title:"Unexpected URI of remote signer"
    ~description:"The remote signer argument could not be parsed"
    ~pp:(fun ppf s -> Format.fprintf ppf "Value '%s' is not a valid URI." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_remote_signer_argument s -> Some s | _ -> None)
    (fun s -> Invalid_remote_signer_argument s) ;
  register_error_kind
    `Branch
    ~id:"invalidWaitArgument"
    ~title:"Bad Wait Argument"
    ~description:"Wait argument could not be parsed"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Value %s is not a valid number of confirmation, nor 'none'."
        s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_wait_arg s -> Some s | _ -> None)
    (fun s -> Invalid_wait_arg s) ;
  register_error_kind
    `Branch
    ~id:"invalidModeArgument"
    ~title:"Invalid Mode Argument"
    ~description:"Mode argument could not be parsed"
    ~pp:(fun ppf s ->
      let enclose s = "\"" ^ s ^ "\"" in
      let pp_mode s = enclose @@ client_mode_to_string s in
      let mode_strings = List.map pp_mode all_modes in
      Format.fprintf
        ppf
        "Value \"%s\" is invalid. It should be one of: %s"
        s
        (String.concat " or " mode_strings))
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_mode_arg s -> Some s | _ -> None)
    (fun s -> Invalid_mode_arg s)

let home = try Sys.getenv "HOME" with Not_found -> "/root"

let base_dir_env_name = "TEZOS_CLIENT_DIR"

let default_base_dir =
  try Sys.getenv base_dir_env_name
  with Not_found -> Filename.concat home ".tezos-client"

let default_chain = `Main

let default_block = `Head 0

let default_endpoint = Uri.of_string "http://localhost:8732"

let default_media_type = Media_type.Command_line.Any

let default_daily_logs_path = None

open Filename.Infix

module Cfg_file = struct
  (* the fields [node_addr], [node_port], and [tls] are deprecated by
   * and should not coexist with [endpoint].
   * see [parse_config_args] for the exact handling *)
  type t = {
    base_dir : string;
    node_addr : string option;
    node_port : int option;
    tls : bool option;
    media_type : Media_type.Command_line.t option;
    endpoint : Uri.t option;
    web_port : int;
    remote_signer : Uri.t option;
    confirmations : int option;
    password_filename : string option;
    internal_events : Tezos_base.Internal_event_config.t option;
  }

  let default =
    {
      base_dir = default_base_dir;
      media_type = None;
      endpoint = None;
      node_addr = None;
      node_port = None;
      tls = None;
      web_port = 8080;
      remote_signer = None;
      confirmations = Some 0;
      password_filename = None;
      internal_events = None;
    }

  open Data_encoding

  let encoding =
    conv
      (fun {
             base_dir;
             node_addr;
             node_port;
             tls;
             media_type;
             endpoint;
             web_port;
             remote_signer;
             confirmations;
             password_filename;
             internal_events;
           } ->
        ( ( base_dir,
            node_addr,
            node_port,
            tls,
            media_type,
            endpoint,
            Some web_port,
            remote_signer,
            confirmations,
            password_filename ),
          internal_events ))
      (fun ( ( base_dir,
               node_addr,
               node_port,
               tls,
               media_type,
               endpoint,
               web_port,
               remote_signer,
               confirmations,
               password_filename ),
             internal_events ) ->
        let web_port = Option.value ~default:default.web_port web_port in
        {
          base_dir;
          node_addr;
          node_port;
          tls;
          media_type;
          endpoint;
          web_port;
          remote_signer;
          confirmations;
          password_filename;
          internal_events;
        })
      (merge_objs
         (obj10
            (req "base_dir" string)
            (opt "node_addr" string)
            (opt "node_port" uint16)
            (opt "tls" bool)
            (opt "media_type" Media_type.Command_line.encoding)
            (opt "endpoint" Tezos_rpc.Encoding.uri_encoding)
            (opt "web_port" uint16)
            (opt "remote_signer" Tezos_rpc.Encoding.uri_encoding)
            (opt "confirmations" int8)
            (opt "password_filename" string))
         (obj1
            (opt "internal_events" Tezos_base.Internal_event_config.encoding)))

  let from_json json = Data_encoding.Json.destruct encoding json

  let read fp =
    let open Lwt_result_syntax in
    let* json = Lwt_utils_unix.Json.read_file fp in
    return (from_json json)

  let write out cfg =
    Lwt_utils_unix.Json.write_file
      out
      (Data_encoding.Json.construct encoding cfg)
end

let default_cli_args =
  {
    chain = default_chain;
    block = default_block;
    confirmations = Some 0;
    sources = None;
    password_filename = None;
    protocol = None;
    print_timings = false;
    log_requests = false;
    better_errors = false;
    client_mode = `Mode_client;
  }

let string_parameter () : (string, #Client_context.full) Tezos_clic.parameter =
  Tezos_clic.parameter (fun _ x -> Lwt.return_ok x)

let media_type_parameter () :
    (Media_type.Command_line.t, #Client_context.full) Tezos_clic.parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ x ->
      match Media_type.Command_line.parse_cli_parameter x with
      | Some v -> return v
      | None -> tzfail (Invalid_media_type_arg x))

let endpoint_parameter () =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ x ->
      let parsed = Uri.of_string x in
      let* _ =
        match Uri.scheme parsed with
        | Some "http" | Some "https" -> return_unit
        | _ ->
            tzfail
              (Invalid_endpoint_arg
                 ("only http and https endpoints are supported: " ^ x))
      in
      match (Uri.query parsed, Uri.fragment parsed) with
      | [], None -> return parsed
      | _ ->
          tzfail
            (Invalid_endpoint_arg
               ("endpoint uri should not have query string or fragment: " ^ x)))

let sources_parameter () =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ path ->
      let*! r = Lwt_utils_unix.Json.read_file path in
      match r with
      | Error errs ->
          failwith
            "Can't parse the file specified by --sources as JSON: %s@,%a"
            path
            pp_print_trace
            errs
      | Ok json -> (
          try
            match Tezos_proxy.Light.destruct_sources_config json with
            | Ok sources_cfg -> return sources_cfg
            | Error msg -> failwith "%s" msg
          with exn ->
            failwith
              "Can't parse the file specified by --sources: %s@,%a"
              path
              (fun ppf exn -> Json_encoding.print_error ppf exn)
              exn))

let chain_parameter () =
  Tezos_clic.parameter (fun _ chain ->
      let open Lwt_result_syntax in
      match Chain_services.parse_chain chain with
      | Error _ -> tzfail (Invalid_chain_argument chain)
      | Ok chain -> return chain)

let block_parameter () =
  Tezos_clic.parameter (fun _ block ->
      let open Lwt_result_syntax in
      match Block_services.parse_block block with
      | Error _ -> tzfail (Invalid_block_argument block)
      | Ok block -> return block)

let wait_parameter () =
  Tezos_clic.parameter (fun _ wait ->
      let open Lwt_result_syntax in
      match wait with
      | "no" | "none" -> return_none
      | _ -> (
          match int_of_string_opt wait with
          | Some w when 0 <= w -> return_some w
          | None | Some _ -> tzfail (Invalid_wait_arg wait)))

let protocol_parameter () =
  Tezos_clic.parameter (fun _ arg ->
      let open Lwt_result_syntax in
      match
        Seq.filter
          (fun (hash, _commands) ->
            String.has_prefix ~prefix:arg (Protocol_hash.to_b58check hash))
          (Client_commands.get_versions ())
        @@ ()
      with
      | Cons ((hash, _commands), _) -> return_some hash
      | Nil -> tzfail (Invalid_protocol_argument arg))

(* Command-line only args (not in config file) *)
let base_dir_arg () =
  Tezos_clic.arg
    ~long:"base-dir"
    ~short:'d'
    ~placeholder:"path"
    ~doc:
      (Format.asprintf
         "@[<v>@[<2>client data directory (absent: %s env)@,\
          The directory where the Tezos client will store all its data.@,\
          If absent, its value is the value of the %s@,\
          environment variable. If %s is itself not specified,@,\
          defaults to %s@]@]@."
         base_dir_env_name
         base_dir_env_name
         base_dir_env_name
         default_base_dir)
    (string_parameter ())

let no_base_dir_warnings_switch () =
  Tezos_clic.switch
    ~long:"no-base-dir-warnings"
    ~short:'n'
    ~doc:"silence warnings about client data directory"
    ()

let config_file_arg () =
  Tezos_clic.arg
    ~long:"config-file"
    ~short:'c'
    ~placeholder:"path"
    ~doc:"configuration file"
    (string_parameter ())

let timings_switch () =
  Tezos_clic.switch ~long:"timings" ~short:'t' ~doc:"show RPC request times" ()

let chain_arg () =
  Tezos_clic.default_arg
    ~long:"chain"
    ~placeholder:"hash|tag"
    ~doc:
      "chain on which to apply contextual commands (commands dependent on the \
       context associated with the specified chain). Possible tags are 'main' \
       and 'test'."
    ~default:(Chain_services.to_string default_cli_args.chain)
    (chain_parameter ())

let block_arg () =
  Tezos_clic.default_arg
    ~long:"block"
    ~short:'b'
    ~placeholder:"hash|level|tag"
    ~doc:
      "block on which to apply contextual commands (commands dependent on the \
       context associated with the specified block). Possible tags include \
       'head' and 'genesis' +/- an optional offset (e.g. \"octez-client -b \
       head-1 get timestamp\"). Note that block queried must exist in node's \
       storage."
    ~default:(Block_services.to_string default_cli_args.block)
    (block_parameter ())

let wait_arg () =
  Tezos_clic.arg
    ~long:"wait"
    ~short:'w'
    ~placeholder:"none|<int>"
    ~doc:
      "how many confirmation blocks are needed before an operation is \
       considered included"
    (wait_parameter ())

let protocol_arg () =
  Tezos_clic.arg
    ~long:"protocol"
    ~short:'p'
    ~placeholder:"hash"
    ~doc:"use commands of a specific protocol"
    (protocol_parameter ())

let log_requests_switch () =
  Tezos_clic.switch
    ~long:"log-requests"
    ~short:'l'
    ~doc:"log all requests to the node"
    ()

let better_errors () =
  Tezos_clic.switch
    ~long:"better-errors"
    ~doc:
      "Error reporting is more detailed. Can be used if a call to an RPC fails \
       or if you don't know the input accepted by the RPC. It may happen that \
       the RPC calls take more time however."
    ()

(* Command-line args which can be set in config file as well *)
let addr_confdesc = "-A/--addr ('node_addr' in config file)"

let addr_arg () =
  Tezos_clic.arg
    ~long:"addr"
    ~short:'A'
    ~placeholder:"IP addr|host"
    ~doc:"[DEPRECATED: use --endpoint instead] IP address of the node"
    (string_parameter ())

let port_confdesc = "-P/--port ('node_port' in config file)"

let port_arg () =
  Tezos_clic.arg
    ~long:"port"
    ~short:'P'
    ~placeholder:"number"
    ~doc:"[DEPRECATED: use --endpoint instead] RPC port of the node"
    (Tezos_clic.parameter (fun _ x ->
         let open Lwt_result_syntax in
         match int_of_string_opt x with
         | Some i -> return i
         | None -> tzfail (Invalid_port_arg x)))

let tls_confdesc = "-S/--tls ('tls' in config file)"

let tls_switch () =
  Tezos_clic.switch
    ~long:"tls"
    ~short:'S'
    ~doc:"[DEPRECATED: use --endpoint instead] use TLS to connect to node."
    ()

let media_type_confdesc = "-m/--media-type"

let media_type_arg () =
  Tezos_clic.arg
    ~long:"media-type"
    ~short:'m'
    ~placeholder:"json, binary, any or default"
    ~doc:
      "Sets the \"media-type\" value for the \"accept\" header for RPC \
       requests to the node. The media accept header indicates to the node \
       which format of data serialisation is supported. Use the value \"json\" \
       for serialisation to the JSON format.\n\
      \          Use the value \"binary\" for faster but less human-readable \
       binary serialisation format."
    (media_type_parameter ())

let endpoint_confdesc = "-E/--endpoint ('endpoint' in config file)"

let endpoint_arg () =
  Tezos_clic.arg
    ~long:"endpoint"
    ~short:'E'
    ~placeholder:"uri"
    ~doc:
      "HTTP(S) endpoint of the node RPC interface; e.g. 'http://localhost:8732'"
    (endpoint_parameter ())

let sources_arg () =
  Tezos_clic.arg
    ~long:"sources"
    ~short:'s'
    ~placeholder:"path"
    ~doc:
      ("path to JSON file containing sources for --mode light. Example file \
        content: " ^ Tezos_proxy.Light.example_sources)
    (sources_parameter ())

let remote_signer_arg () =
  Tezos_clic.arg
    ~long:"remote-signer"
    ~short:'R'
    ~placeholder:"uri"
    ~doc:"URI of the remote signer"
    (Tezos_clic.parameter (fun _ x ->
         Tezos_signer_backends_unix.Remote.parse_base_uri x))

let password_filename_arg () =
  Tezos_clic.arg
    ~long:"password-filename"
    ~short:'f'
    ~placeholder:"filename"
    ~doc:"path to the password filename"
    (string_parameter ())

let client_mode_arg () =
  let mode_strings = List.map client_mode_to_string all_modes in
  let parse_client_mode (str : string) : client_mode tzresult =
    let open Result_syntax in
    let* modes_and_strings =
      List.combine
        ~when_different_lengths:(TzTrace.make @@ Exn (Failure __LOC__))
        mode_strings
        all_modes
    in
    match List.assoc_opt ~equal:String.equal str modes_and_strings with
    | None -> tzfail (Invalid_mode_arg str)
    | Some mode -> return mode
  in
  Tezos_clic.default_arg
    ~short:'M'
    ~long:"mode"
    ~placeholder:(String.concat "|" mode_strings)
    ~doc:"how to interact with the node"
    ~default:(client_mode_to_string `Mode_client)
    (Tezos_clic.parameter
       ~autocomplete:(fun _ -> Lwt.return_ok mode_strings)
       (fun _ param -> Lwt.return (parse_client_mode param)))

let read_config_file config_file =
  let open Lwt_result_syntax in
  let*! r = Lwt_utils_unix.Json.read_file config_file in
  match r with
  | Error errs ->
      failwith
        "Can't parse the configuration file as a JSON: %s@,%a"
        config_file
        pp_print_trace
        errs
  | Ok cfg_json -> (
      try return @@ Cfg_file.from_json cfg_json
      with exn ->
        failwith
          "Can't parse the configuration file: %s@,%a"
          config_file
          (fun ppf exn -> Json_encoding.print_error ppf exn)
          exn)

let fail_on_non_mockup_dir (cctxt : #Client_context.full) =
  let open Lwt_result_syntax in
  let base_dir = cctxt#get_base_dir in
  let open Tezos_mockup.Persistence in
  let* b = classify_base_dir base_dir in
  match b with
  | Base_dir_does_not_exist | Base_dir_is_file | Base_dir_is_nonempty
  | Base_dir_is_empty ->
      failwith
        "base directory at %s should be a mockup directory for this operation \
         to be allowed (it may contain sensitive data otherwise). What you \
         likely want is calling `octez-client --mode mockup --base-dir \
         /some/dir create mockup` where `/some/dir` is **fresh** and **empty** \
         and redo this operation, specifying `--base-dir /some/dir` this time."
        base_dir
  | Base_dir_is_mockup -> return_unit

let default_config_file_name = "config"

let mockup_bootstrap_accounts = "bootstrap-accounts"

let mockup_protocol_constants = "protocol-constants"

(* The implementation of ["config"; "show"] when --mode is "client" *)
let config_show_client (cctxt : #Client_context.full) (config_file : string) cfg
    =
  let open Lwt_syntax in
  let* () =
    if not @@ Sys.file_exists config_file then
      cctxt#warning
        "@[<v 2>Warning: no config file at %s,@,\
         displaying the default configuration.@]"
        config_file
    else Lwt.return_unit
  in
  let* () =
    cctxt#message
      "%a@,"
      Data_encoding.Json.pp
      (Data_encoding.Json.construct Cfg_file.encoding cfg)
  in
  return_ok_unit

(* The implementation of ["config"; "show"] when --mode is "mockup" *)
let config_show_mockup (cctxt : #Client_context.full)
    (protocol_hash_opt : Protocol_hash.t option) (base_dir : string) =
  let open Lwt_result_syntax in
  let* () = fail_on_non_mockup_dir cctxt in
  let* mockup, _ =
    Tezos_mockup.Persistence.get_mockup_context_from_disk
      ~base_dir
      ~protocol_hash:protocol_hash_opt
      cctxt
  in
  let (module Mockup) = mockup in
  let json_pp encoding ppf value =
    Data_encoding.Json.pp ppf (Data_encoding.Json.construct encoding value)
  in
  let* bootstrap_accounts_string = Mockup.default_bootstrap_accounts cctxt in
  let*! () =
    cctxt#message
      "@[<v>Default value of --%s:@,%s@]"
      mockup_bootstrap_accounts
      bootstrap_accounts_string
  in
  let* protocol_constants = Mockup.default_protocol_constants cctxt in
  let*! () =
    cctxt#message
      "@[<v>Default value of --%s:@,%a@]"
      mockup_protocol_constants
      (json_pp Mockup.protocol_constants_encoding)
      protocol_constants
  in
  return_unit

(* The implementation of ["config"; "init"] when --mode is "client" *)
let config_init_client config_file cfg =
  if not (Sys.file_exists config_file) then Cfg_file.(write config_file cfg)
    (* Should be default or command would have failed *)
  else failwith "Config file already exists at location: %s" config_file

(* The implementation of ["config"; "init"] when --mode is "mockup" *)
let config_init_mockup cctxt protocol_hash_opt bootstrap_accounts_file
    protocol_constants_file base_dir =
  let open Lwt_result_syntax in
  let* () = fail_on_non_mockup_dir cctxt in
  let* () =
    fail_when
      (Sys.file_exists bootstrap_accounts_file)
      (error_of_fmt
         "Config file to write value of --%s exists already: %s"
         mockup_bootstrap_accounts
         bootstrap_accounts_file)
  in
  let* () =
    fail_when
      (Sys.file_exists protocol_constants_file)
      (error_of_fmt
         "Config file to write value of --%s exists already: %s"
         mockup_protocol_constants
         protocol_constants_file)
  in
  let* mockup, _ =
    Tezos_mockup.Persistence.get_mockup_context_from_disk
      ~base_dir
      ~protocol_hash:protocol_hash_opt
      cctxt
  in
  let (module Mockup) = mockup in
  let* string_to_write = Mockup.default_bootstrap_accounts cctxt in
  let*! _ =
    Lwt_utils_unix.create_file bootstrap_accounts_file string_to_write
  in
  let*! () =
    cctxt#message
      "Written default --%s file: %s"
      mockup_bootstrap_accounts
      bootstrap_accounts_file
  in
  let* protocol_constants = Mockup.default_protocol_constants cctxt in
  let string_to_write =
    Data_encoding.Json.construct
      Mockup.protocol_constants_encoding
      protocol_constants
  in
  let* () =
    Lwt_utils_unix.Json.write_file protocol_constants_file string_to_write
  in
  let*! () =
    cctxt#message
      "Written default --%s file: %s"
      mockup_protocol_constants
      protocol_constants_file
  in
  return_unit

let commands config_file cfg (client_mode : client_mode)
    (protocol_hash_opt : Protocol_hash.t option) (base_dir : string) =
  let open Tezos_clic in
  let group =
    {
      name = "config";
      title = "Commands for editing and viewing the client's config file";
    }
  in
  [
    command
      ~group
      ~desc:
        "Show the current config (config file content + command line \
         arguments) or the mockup config files if `--mode mockup` is \
         specified."
      no_options
      (fixed ["config"; "show"])
      (fun () (cctxt : #Client_context.full) ->
        match client_mode with
        | `Mode_client | `Mode_light | `Mode_proxy ->
            config_show_client cctxt config_file cfg
        | `Mode_mockup -> config_show_mockup cctxt protocol_hash_opt base_dir);
    command
      ~group
      ~desc:"Reset the config file to the factory defaults."
      no_options
      (fixed ["config"; "reset"])
      (fun () _cctxt -> Cfg_file.(write config_file default));
    command
      ~group
      ~desc:
        "Update the config based on the current cli values.\n\
         Loads the current configuration (default or as specified with \
         `-config-file`), applies alterations from other command line \
         arguments (such as the node's address, etc.), and overwrites the \
         updated configuration file."
      no_options
      (fixed ["config"; "update"])
      (fun () _cctxt -> Cfg_file.(write config_file cfg));
    command
      ~group
      ~desc:
        "Create config file(s) based on the current CLI values.\n\
         If the `-file` option is not passed, this will initialize the default \
         config file, based on default parameters, altered by other command \
         line options (such as the node's address, etc.).\n\
         Otherwise, it will create a new config file, based on the default \
         parameters (or the the ones specified with `-config-file`), altered \
         by other command line options.\n\n\
         If `-mode mockup` is specified, this will initialize the mockup's \
         default files instead of the config file. Use `-bootstrap-accounts` \
         and `-protocol-constants` to specify custom paths.\n\n\
         The command will always fail if file(s) to create exist already"
      (args3
         (default_arg
            ~long:"output"
            ~short:'o'
            ~placeholder:"path"
            ~doc:"path at which to create the file"
            ~default:(cfg.base_dir // default_config_file_name)
            (parameter (fun _ctx str -> Lwt.return_ok str)))
         (default_arg
            ~long:mockup_bootstrap_accounts
            ~placeholder:"path"
            ~doc:"path at which to create the file"
            ~default:((cfg.base_dir // mockup_bootstrap_accounts) ^ ".json")
            (parameter (fun _ctx str -> Lwt.return_ok str)))
         (default_arg
            ~long:mockup_protocol_constants
            ~placeholder:"path"
            ~doc:"path at which to create the file"
            ~default:((cfg.base_dir // mockup_protocol_constants) ^ ".json")
            (parameter (fun _ctx str -> Lwt.return_ok str))))
      (fixed ["config"; "init"])
      (fun (config_file, bootstrap_accounts_file, protocol_constants_file) cctxt ->
        match client_mode with
        | `Mode_client | `Mode_light | `Mode_proxy ->
            config_init_client config_file cfg
        | `Mode_mockup ->
            config_init_mockup
              cctxt
              protocol_hash_opt
              bootstrap_accounts_file
              protocol_constants_file
              base_dir);
  ]

let global_options () =
  Tezos_clic.args19
    (base_dir_arg ())
    (no_base_dir_warnings_switch ())
    (config_file_arg ())
    (timings_switch ())
    (chain_arg ())
    (block_arg ())
    (wait_arg ())
    (protocol_arg ())
    (log_requests_switch ())
    (better_errors ())
    (addr_arg ())
    (port_arg ())
    (tls_switch ())
    (media_type_arg ())
    (endpoint_arg ())
    (sources_arg ())
    (remote_signer_arg ())
    (password_filename_arg ())
    (client_mode_arg ())

type parsed_config_args = {
  parsed_config_file : Cfg_file.t option;
  parsed_args : cli_args option;
  config_commands : Client_context.full Tezos_clic.command list;
  base_dir : string option;
  require_auth : bool;
}

let default_parsed_config_args =
  {
    parsed_config_file = None;
    parsed_args = None;
    config_commands = [];
    base_dir = None;
    require_auth = false;
  }

(* Check that the base directory is actually in the right configuration for
 * the mode used by the client.
 *
 * Depending on the criticality of the compatibility issue, this function fails
 * (when all/most commands will fail) or emits a warning (some commands may
 * fail).
 *)
let check_base_dir_for_mode (ctx : #Client_context.full) client_mode
    no_base_dir_warnings base_dir =
  let open Lwt_result_syntax in
  let open Tezos_mockup.Persistence in
  let* base_dir_class = classify_base_dir base_dir in
  match client_mode with
  | `Mode_client | `Mode_light | `Mode_proxy -> (
      match base_dir_class with
      | Base_dir_is_mockup ->
          failwith
            "Base directory %s is in mockup mode while operation is in %s mode"
            base_dir
          @@ client_mode_to_string client_mode
      (* You might be creating a mockup directory here *)
      | Base_dir_is_empty -> return_unit
      | Base_dir_is_file | Base_dir_does_not_exist ->
          (* This case is checked in by the caller so that it should not happen *)
          failwith
            "Error for base-dir %s should not have happened (this is due to %a)"
            base_dir
            pp_base_dir_class
            base_dir_class
      | _ -> return_unit)
  | `Mode_mockup -> (
      let warn_might_not_work explain =
        let*! () =
          if not no_base_dir_warnings then
            ctx#warning
              "@[<hv>Base directory %s %a@ Some commands (e.g., transfer) \
               might not work correctly.@]"
              base_dir
              explain
              ()
          else Lwt.return_unit
        in
        return_unit
      in
      let show_cmd ppf () =
        Format.fprintf
          ppf
          "./octez-client --mode mockup --base-dir %s create mockup"
          base_dir
      in
      match base_dir_class with
      | Base_dir_is_empty ->
          warn_might_not_work (fun ppf () ->
              Format.fprintf
                ppf
                "is empty.@ Move directory %s away and create it anew with:@ \
                 %a@ or use another directory name."
                base_dir
                show_cmd
                ())
      | Base_dir_does_not_exist ->
          warn_might_not_work (fun ppf () ->
              Format.fprintf
                ppf
                "does not exist.@ Create it with:@ %a"
                show_cmd
                ())
      | Base_dir_is_nonempty ->
          warn_might_not_work (fun ppf () ->
              Format.fprintf
                ppf
                "is non empty.@ Move directory %s away and create it anew \
                 with:@ %a@ or use another directory name."
                base_dir
                show_cmd
                ())
      | Base_dir_is_file ->
          warn_might_not_work (fun ppf () ->
              Format.fprintf
                ppf
                "is a file.@ This is expected to be a directory.@ It can be \
                 created with:@ %a"
                show_cmd
                ())
      | Base_dir_is_mockup -> return_unit)

let build_endpoint addr port tls =
  let updatecomp updatef ov uri =
    match ov with Some x -> updatef uri (Some x) | None -> uri
  in
  let scheme = Option.map (function true -> "https" | false -> "http") tls in
  let url = default_endpoint in
  url
  |> updatecomp Uri.with_host addr
  |> updatecomp Uri.with_port port
  |> updatecomp Uri.with_scheme scheme

let light_mode_checks mode endpoint sources =
  let open Lwt_result_syntax in
  match (mode, sources) with
  | `Mode_client, None | `Mode_mockup, None | `Mode_proxy, None ->
      (* No --mode light, no --sources; good *)
      return_unit
  | `Mode_client, Some _ | `Mode_mockup, Some _ | `Mode_proxy, Some _ ->
      (* --sources without the light mode: wrong *)
      failwith
        "--sources is specified whereas mode is %s. --sources should only be \
         used with --mode light."
      @@ client_mode_to_string mode
  | `Mode_light, None ->
      (* --mode light without --sources: wrong *)
      failwith
        "--mode light requires passing --sources. Example --sources file: %s"
        Tezos_proxy.Light.example_sources
  | `Mode_light, Some sources ->
      let sources_uris = Tezos_proxy.Light.sources_config_to_uris sources in
      if List.mem ~equal:Uri.equal endpoint sources_uris then return_unit
      else
        let uri_to_json_string uri =
          Uri.to_string uri |> Printf.sprintf "\"%s\""
        in
        failwith
          "Value of --endpoint is %a. Therefore, this URI MUST be in field \
           'uris' of --sources (whose value is: [%s]). If you did not specify \
           --endpoint, it is being defaulted; you may hereby specify \
           --endpoint %a to fix this error."
          Uri.pp
          endpoint
          (String.concat ", " @@ List.map uri_to_json_string sources_uris)
          (* By the check done in Light.mk_sources_config, [sources_uris]
             cannot be empty, but we don't rely on this here, by using
             pp_print_option. *)
          (Format.pp_print_option Uri.pp)
          (List.hd sources_uris)

let parse_config_args (ctx : #Client_context.full) argv =
  let open Lwt_result_syntax in
  let* ( ( base_dir,
           no_base_dir_warnings,
           config_file,
           timings,
           chain,
           block,
           confirmations,
           protocol,
           log_requests,
           better_errors,
           node_addr,
           node_port,
           tls,
           media_type,
           endpoint,
           sources,
           remote_signer,
           password_filename,
           client_mode ),
         remaining ) =
    Tezos_clic.parse_global_options (global_options ()) ctx argv
  in
  let* base_dir =
    match base_dir with
    | None ->
        let base_dir = default_base_dir in
        let* () =
          unless
            (* Mockup mode will create the base directory on need *)
            (client_mode = `Mode_mockup || Sys.file_exists base_dir)
            (fun () ->
              let*! () = Lwt_utils_unix.create_dir base_dir in
              return_unit)
        in
        return base_dir
    | Some dir -> (
        match client_mode with
        | `Mode_client | `Mode_light | `Mode_proxy ->
            if not (Sys.file_exists dir) then
              failwith
                "Specified --base-dir does not exist. Please create the \
                 directory and try again."
            else if Sys.is_directory dir then return dir
            else failwith "Specified --base-dir must be a directory"
        | `Mode_mockup ->
            (* In mockup mode base dir may be created automatically. *)
            return dir)
  in
  let* () =
    check_base_dir_for_mode ctx client_mode no_base_dir_warnings base_dir
  in
  let* () =
    when_
      (Option.is_some sources && client_mode <> `Mode_light)
      (fun () ->
        failwith
          "--sources is specific to --mode light, please do not specify it \
           with --mode %s."
        @@ client_mode_to_string client_mode)
  in
  let* config_file =
    match config_file with
    | None -> return @@ (base_dir // default_config_file_name)
    | Some config_file ->
        if Sys.file_exists config_file then return config_file
        else
          failwith
            "Config file specified in option does not exist. Use `client \
             config init` to create one."
  in
  let config_dir = Filename.dirname config_file in
  let protocol = match protocol with None -> None | Some p -> p in
  let* cfg =
    if not (Sys.file_exists config_file) then
      return {Cfg_file.default with base_dir}
    else read_config_file config_file
  in
  (* endpoint logic:
   *   1) when --endpoint provided as argument,
   *      use it but check no presence of --addr, --port, or --tls
   *   2) otherwise, merge --addr, --port, and --tls with config file; then
   *        2a) --endpoint exists in config file,
   *            use it but check no presence of merged --addr, --port, or --tls
   *        2b) synthesize --endpoint from --addr, --port, and --tls *)
  let check_absence addr port tls =
    let checkabs argdesc = function
      | None -> fun x -> x
      | _ -> fun x -> x @ [argdesc]
    in
    let superr =
      []
      |> checkabs addr_confdesc addr
      |> checkabs port_confdesc port
      |> checkabs tls_confdesc tls
    in
    if superr <> [] then
      tzfail (Suppressed_arg {args = superr; by = endpoint_confdesc})
    else return_unit
  in
  let tls = if tls then Some true else None in
  let* endpoint =
    match endpoint with
    | Some endpt ->
        let* _ = check_absence node_addr node_port tls in
        return endpt
    | None -> (
        let node_addr = Option.either node_addr cfg.node_addr in
        let node_port = Option.either node_port cfg.node_port in
        let tls = Option.either tls cfg.tls in
        match cfg.endpoint with
        | Some endpt ->
            let* _ = check_absence node_addr node_port tls in
            return endpt
        | None -> return (build_endpoint node_addr node_port tls))
  in
  (* give a kind warning when any of -A -P -S exists *)
  (let got = function Some _ -> true | None -> false in
   let gotany =
     got node_addr || got node_port || got tls || got cfg.node_addr
     || got cfg.node_port || got cfg.tls
   in
   if gotany then (
     Format.(
       eprintf
         "@{<warning>Warning:@}  the --addr --port --tls options are now \
          deprecated; use --endpoint instead\n" ;
       pp_print_flush err_formatter ()))) ;
  let* () = light_mode_checks client_mode endpoint sources in
  let* remote_signer_env =
    Tezos_signer_backends_unix.Remote.read_base_uri_from_env ()
  in
  let remote_signer =
    Option.either remote_signer
    @@ Option.either remote_signer_env cfg.remote_signer
  in
  let confirmations = Option.value ~default:cfg.confirmations confirmations in
  (* --password-filename has precedence over --config-file's
     "password-filename" json field *)
  let password_filename =
    Option.either password_filename cfg.password_filename
  in
  let media_type = Option.either media_type cfg.media_type in
  let cfg =
    {
      cfg with
      node_addr = None;
      node_port = None;
      tls = None;
      media_type;
      endpoint = Some endpoint;
      remote_signer;
      confirmations;
      password_filename;
    }
  in
  if Sys.file_exists base_dir && not (Sys.is_directory base_dir) then (
    Format.eprintf "%s is not a directory.@." base_dir ;
    exit 1) ;
  if Sys.file_exists config_dir && not (Sys.is_directory config_dir) then (
    Format.eprintf "%s is not a directory.@." config_dir ;
    exit 1) ;
  let* () =
    unless (client_mode = `Mode_mockup) (fun () ->
        let*! () = Lwt_utils_unix.create_dir config_dir in
        return_unit)
  in
  let parsed_args =
    {
      chain;
      block;
      confirmations;
      sources;
      print_timings = timings;
      log_requests;
      better_errors;
      password_filename;
      protocol;
      client_mode;
    }
  in
  return
    ( {
        default_parsed_config_args with
        parsed_config_file = Some cfg;
        parsed_args = Some parsed_args;
        config_commands =
          commands config_file cfg client_mode parsed_args.protocol base_dir;
      },
      remaining )

type t =
  string option
  * bool
  * string option
  * bool
  * Shell_services.chain
  * Shell_services.block
  * int option option
  * Protocol_hash.t option option
  * bool
  * bool
  * string option
  * int option
  * bool
  * Media_type.Command_line.t option
  * Uri.t option
  * Tezos_proxy.Light.sources_config option
  * Uri.t option
  * string option
  * client_mode

module type Remote_params = sig
  val authenticate :
    Tezos_crypto.Signature.public_key_hash list ->
    Bytes.t ->
    Tezos_crypto.Signature.t tzresult Lwt.t

  val logger : Tezos_rpc_http_client_unix.RPC_client_unix.logger
end

let other_registrations : (_ -> (module Remote_params) -> _) option =
  Some
    (fun parsed_config_file (module Remote_params) ->
      parsed_config_file.Cfg_file.remote_signer
      |> Option.iter (fun signer ->
             Client_keys.register_signer
               (module Tezos_signer_backends_unix.Remote.Make
                         (Tezos_rpc_http_client_unix.RPC_client_unix)
                         (struct
                           let default = signer

                           include Remote_params
                         end))))

let clic_commands ~base_dir:_ ~config_commands ~builtin_commands ~other_commands
    ~require_auth:_ =
  config_commands @ builtin_commands @ other_commands

let logger = None
