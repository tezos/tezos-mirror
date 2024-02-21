(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Tezos Command line interface - Main Program *)

open Client_context_unix

let builtin_commands =
  let open Lwt_syntax in
  let open Tezos_clic in
  [
    command
      ~desc:"List the protocol versions that this client understands."
      no_options
      (fixed ["list"; "understood"; "protocols"])
      (fun () (cctxt : #Client_context.full) ->
        let* () =
          Seq.S.iter
            (fun (ver, _) -> cctxt#message "%a" Protocol_hash.pp_short ver)
            (Client_commands.get_versions ())
        in
        return_ok_unit);
  ]

module type M = sig
  type t

  val global_options :
    unit -> (t, Client_context_unix.unix_full) Tezos_clic.options

  val parse_config_args :
    #Tezos_client_base.Client_context.full ->
    string list ->
    (Client_config.parsed_config_args * string list) tzresult Lwt.t

  val default_chain : Chain_services.chain

  val default_block : [> `Head of int]

  val default_base_dir : string

  val default_daily_logs_path : string option

  val default_media_type : Media_type.Command_line.t

  val other_registrations :
    (Client_config.Cfg_file.t -> (module Client_config.Remote_params) -> unit)
    option

  val clic_commands :
    base_dir:string ->
    config_commands:
      Tezos_client_base.Client_context.full Tezos_clic.command list ->
    builtin_commands:
      Tezos_client_base.Client_context.full Tezos_clic.command list ->
    other_commands:Tezos_client_base.Client_context.full Tezos_clic.command list ->
    require_auth:bool ->
    Tezos_client_base.Client_context.full Tezos_clic.command list

  val logger : RPC_client_unix.logger option
end

let register_default_signer ?other_registrations ?logger
    (cctxt : Client_context.io_wallet) =
  let module Remote_params = struct
    let authenticate pkhs payload =
      let open Lwt_result_syntax in
      let* keys = Client_keys.list_keys cctxt in
      match
        List.filter_map
          (function
            | _, known_pkh, _, Some known_sk_uri
              when List.exists
                     (fun pkh ->
                       Tezos_crypto.Signature.Public_key_hash.equal
                         pkh
                         known_pkh)
                     pkhs ->
                Some known_sk_uri
            | _ -> None)
          keys
      with
      | sk_uri :: _ -> Client_keys.sign cctxt sk_uri payload
      | [] ->
          failwith
            "remote signer expects authentication signature, but no authorized \
             key was found in the wallet"

    let logger =
      Option.value ~default:RPC_client_unix.default_config.logger logger
  end in
  let module Http =
    Tezos_signer_backends.Http.Make (RPC_client_unix) (Remote_params)
  in
  let module Https =
    Tezos_signer_backends.Https.Make (RPC_client_unix) (Remote_params)
  in
  let module Socket = Tezos_signer_backends_unix.Socket.Make (Remote_params) in
  Client_keys.register_signer
    (module Tezos_signer_backends.Encrypted.Make (struct
      let cctxt = cctxt
    end)) ;
  Client_keys.register_aggregate_signer
    (module Tezos_signer_backends.Encrypted.Make_aggregate (struct
      let cctxt = cctxt
    end)) ;
  Client_keys.register_signer (module Tezos_signer_backends.Unencrypted) ;
  Client_keys.register_aggregate_signer
    (module Tezos_signer_backends.Unencrypted.Aggregate) ;
  Client_keys.register_signer
    (module Tezos_signer_backends_unix.Ledger.Signer_implementation) ;
  Client_keys.register_signer (module Socket.Unix) ;
  Client_keys.register_signer (module Socket.Tcp) ;
  Client_keys.register_signer (module Http) ;
  Client_keys.register_signer (module Https) ;
  match other_registrations with
  | Some other_registrations ->
      other_registrations (module Remote_params : Client_config.Remote_params)
  | None -> ()

let register_signer (module C : M) (cctxt : Client_context.io_wallet)
    (rpc_config : RPC_client_unix.config) parsed_config_file =
  let logger =
    (* overriding the logger we might already have with the one from
       module C *)
    match C.logger with Some logger -> logger | None -> rpc_config.logger
  in
  let other_registrations =
    match parsed_config_file with
    | None -> None
    | Some parsed_config_file -> (
        match C.other_registrations with
        | Some r -> Some (r parsed_config_file)
        | None -> None)
  in
  register_default_signer ?other_registrations ~logger cctxt

(** Warn the user if there are duplicate URIs in the sources (may or may
    not be a misconfiguration). *)
let warn_if_duplicates_light_sources (printer : unix_logger) uris =
  let module UriMap = Map.Make (Uri) in
  let uri_duplicates =
    List.fold_left
      (fun map uri ->
        UriMap.update
          uri
          (fun nb -> Option.fold ~none:1 ~some:succ nb |> Option.some)
          map)
      UriMap.empty
      uris
    |> UriMap.filter (fun _uri nb -> nb > 1)
  in
  if not (UriMap.is_empty uri_duplicates) then
    printer#warning
      "The following URIs are duplicated in the light mode --sources argument \
       (this will increase their weight in the consensus check). If this is \
       not intentional, please fix your configuration: [%a]"
      Format.(
        pp_print_list
          ~pp_sep:(fun ppf () -> pp_print_string ppf "; ")
          (fun ppf (uri, nb) -> fprintf ppf "%a (%i occurrences)" Uri.pp uri nb))
      (UriMap.bindings uri_duplicates)
  else Lwt.return_unit

let setup_default_proxy_client_config parsed_args base_dir rpc_config mode =
  let open Lwt_result_syntax in
  (* Make sure that base_dir is not a mockup. *)
  let* () =
    let* b = Tezos_mockup.Persistence.classify_base_dir base_dir in
    match b with
    | Base_dir_is_mockup ->
        failwith
          "%s is setup as a mockup, yet mockup mode is not active"
          base_dir
    | _ -> return_unit
  in
  let chain, block, confirmations, password_filename, protocol, sources =
    match parsed_args with
    | None ->
        ( Client_config.default_chain,
          Client_config.default_block,
          None,
          None,
          None,
          None )
    | Some p ->
        ( p.Client_config.chain,
          p.Client_config.block,
          p.Client_config.confirmations,
          p.Client_config.password_filename,
          p.Client_config.protocol,
          p.Client_config.sources )
  in
  let verbose_rpc_error_diagnostics =
    match parsed_args with
    | None -> false
    | Some parsed_args -> parsed_args.better_errors
  in
  match mode with
  | `Mode_client ->
      return
      @@ new unix_full
           ~chain
           ~block
           ~confirmations
           ~password_filename
           ~base_dir
           ~rpc_config
           ~verbose_rpc_error_diagnostics
  | (`Mode_light | `Mode_proxy) as mode ->
      let printer = new unix_logger ~base_dir in
      let get_mode () =
        match (mode, sources) with
        | `Mode_proxy, _ -> return Tezos_proxy.Proxy_services.Proxy_client
        | `Mode_light, None ->
            failwith
              "--sources MUST be specified when --mode light is specified"
        | `Mode_light, Some sources_config ->
            let*! () =
              warn_if_duplicates_light_sources printer sources_config.uris
            in
            let rpc_builder endpoint =
              (new Tezos_rpc_http_client_unix.RPC_client_unix.http_ctxt
                 {rpc_config with endpoint}
                 (Media_type.Command_line.of_command_line rpc_config.media_type)
                :> Tezos_rpc.Context.simple)
            in
            let sources =
              Tezos_proxy.Light.sources_config_to_sources
                rpc_builder
                sources_config
            in
            return (Tezos_proxy.Proxy_services.Light_client sources)
      in
      let* mode = get_mode () in
      return
      @@ new unix_proxy
           ?protocol
           ~chain
           ~block
           ~confirmations
           ~password_filename
           ~base_dir
           ~rpc_config
           ~mode
           ()

let setup_mockup_rpc_client_config
    (cctxt : Tezos_client_base.Client_context.printer)
    (args : Client_config.cli_args) base_dir =
  let open Lwt_result_syntax in
  let in_memory_mockup (args : Client_config.cli_args) =
    match args.protocol with
    | None -> Tezos_mockup.Persistence.default_mockup_context cctxt
    | Some protocol_hash ->
        Tezos_mockup.Persistence.init_mockup_context_by_protocol_hash
          ~cctxt
          ~protocol_hash
          ~constants_overrides_json:None
          ~bootstrap_accounts_json:None
  in
  let* b = Tezos_mockup.Persistence.classify_base_dir base_dir in
  let* (mockup_env, {chain = chain_id; rpc_context; protocol_data}), mem_only =
    match b with
    | Tezos_mockup.Persistence.Base_dir_is_empty
    | Tezos_mockup.Persistence.Base_dir_is_file
    | Tezos_mockup.Persistence.Base_dir_is_nonempty
    | Tezos_mockup.Persistence.Base_dir_does_not_exist ->
        let mem_only = true in
        let* res = in_memory_mockup args in
        return (res, mem_only)
    | Tezos_mockup.Persistence.Base_dir_is_mockup ->
        let mem_only = false in
        let* res =
          Tezos_mockup.Persistence.get_mockup_context_from_disk
            ~base_dir
            ~protocol_hash:args.protocol
            cctxt
        in
        return (res, mem_only)
  in
  return
    (new unix_mockup
       ~base_dir
       ~mem_only
       ~mockup_env
       ~chain_id
       ~rpc_context
       ~protocol_data)

let setup_client_config (cctxt : Tezos_client_base.Client_context.printer)
    (parsed_args : Client_config.cli_args option) base_dir rpc_config =
  let setup_non_mockup_rpc_client_config =
    setup_default_proxy_client_config parsed_args base_dir rpc_config
  in
  match parsed_args with
  | None -> setup_non_mockup_rpc_client_config `Mode_client
  | Some args -> (
      match args.Client_config.client_mode with
      | (`Mode_client | `Mode_light | `Mode_proxy) as m ->
          setup_non_mockup_rpc_client_config m
      | `Mode_mockup -> setup_mockup_rpc_client_config cctxt args base_dir)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4025
   Remove backwards compatible Tezos symlinks. *)
let warn_if_argv0_name_not_octez () =
  let executable_name =
    (* example: tezos-tx-rollup-client-015-PtKathma *)
    Filename.basename Sys.argv.(0)
  in
  let old_head = "tezos-" in
  let new_head = "octez-" in
  match TzString.has_prefix executable_name ~prefix:old_head with
  | false -> ()
  | true ->
      let expected_name =
        let len_prefix = String.length old_head in
        let headless_name =
          (* example: tx-rollup-client-015-PtKathma *)
          String.sub
            executable_name
            len_prefix
            (String.length executable_name - len_prefix)
        in
        let name_without_version name =
          match headless_name |> String.starts_with ~prefix:name with
          | false -> None
          | true ->
              let len_name = String.length name in
              let version_proto =
                (* example: -015-PtKathma *)
                String.sub
                  headless_name
                  len_name
                  (String.length headless_name - len_name)
              in
              let num_hyphens =
                (* example: 2 *)
                version_proto
                |> String.fold_left
                     (fun acc c -> match c with '-' -> acc + 1 | _ -> acc)
                     0
              in
              let proto =
                (* example: -PtKathma *)
                if num_hyphens = 2 then
                  String.sub version_proto 4 (String.length version_proto - 4)
                else version_proto
              in
              Some (name ^ proto)
        in
        (* example: tx-rollup-client-PtKathma *)
        let versionless_name =
          ["accuser"; "baker"; "tx-rollup-client"; "tx-rollup-node"]
          |> List.map name_without_version
          |> List.find Option.is_some |> Option.join
        in
        (* example: octez-tx-rollup-client-PtKathma *)
        new_head
        ^
        match versionless_name with
        | None -> headless_name
        | Some versionless_name -> versionless_name
      in
      Format.eprintf
        "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
         The executable with name @{<kwd>%s@} has been renamed to @{<kwd>%s@}. \
         The name @{<kwd>%s@} is now@,\
         deprecated, and it will be removed in a future release. Please update@,\
         your scripts to use the new name.@]@\n\
         @."
        executable_name
        expected_name
        executable_name

(* Main (lwt) entry *)
let main (module C : M) ~select_commands =
  let open Lwt_result_syntax in
  let global_options = C.global_options () in
  let executable_name = Filename.basename Sys.executable_name in
  let original_args, autocomplete =
    (* for shell aliases *)
    let rec move_autocomplete_token_upfront acc = function
      | "bash_autocomplete" :: prev_arg :: cur_arg :: script :: args ->
          let args = List.rev acc @ args in
          (args, Some (prev_arg, cur_arg, script))
      | x :: rest -> move_autocomplete_token_upfront (x :: acc) rest
      | [] -> (List.rev acc, None)
    in
    match Array.to_list Sys.argv with
    | _ :: args -> move_autocomplete_token_upfront [] args
    | [] -> ([], None)
  in
  Random.self_init () ;
  ignore
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short) ;
  ignore
    Tezos_clic.(
      setup_formatter
        Format.err_formatter
        (if Unix.isatty Unix.stderr then Ansi else Plain)
        Short) ;
  warn_if_argv0_name_not_octez () ;
  let*! retcode =
    Lwt.catch
      (fun () ->
        let full =
          new unix_full
            ~chain:C.default_chain
            ~block:C.default_block
            ~confirmations:None
            ~password_filename:None
            ~base_dir:C.default_base_dir
            ~rpc_config:RPC_client_unix.default_config
            ~verbose_rpc_error_diagnostics:false
        in
        let*! r =
          let* parsed, remaining = C.parse_config_args full original_args in
          let parsed_config_file = parsed.Client_config.parsed_config_file in
          let parsed_args = parsed.Client_config.parsed_args in
          let config_commands = parsed.Client_config.config_commands in
          let base_dir : string =
            match parsed.Client_config.base_dir with
            | Some p -> p
            | None -> (
                match parsed_config_file with
                | None -> C.default_base_dir
                | Some p -> p.Client_config.Cfg_file.base_dir)
          in
          let daily_logs_path =
            C.default_daily_logs_path
            |> Option.map
                 Filename.Infix.(fun logdir -> base_dir // "logs" // logdir)
          in
          let require_auth = parsed.Client_config.require_auth in
          let*! () =
            let open Tezos_base_unix.Internal_event_unix in
            let config =
              make_with_defaults
                ?enable_default_daily_logs_at:daily_logs_path
                ()
            in
            match parsed_config_file with
            | None -> init ~config ()
            | Some cf -> (
                match cf.Client_config.Cfg_file.internal_events with
                | None -> init ~config ()
                | Some config -> init ~config ())
          in
          let rpc_config =
            let rpc_config : RPC_client_unix.config =
              match parsed_config_file with
              | None -> RPC_client_unix.default_config
              | Some cfg ->
                  {
                    RPC_client_unix.default_config with
                    media_type =
                      Option.value cfg.media_type ~default:C.default_media_type;
                    endpoint =
                      Option.value
                        cfg.endpoint
                        ~default:Client_config.default_endpoint;
                  }
            in
            match parsed_args with
            | Some parsed_args ->
                if parsed_args.Client_config.print_timings then
                  let gettimeofday = Unix.gettimeofday in
                  {
                    rpc_config with
                    logger =
                      RPC_client_unix.timings_logger
                        ~gettimeofday
                        Format.err_formatter;
                  }
                else if parsed_args.Client_config.log_requests then
                  {
                    rpc_config with
                    logger = RPC_client_unix.full_logger Format.err_formatter;
                  }
                else rpc_config
            | None -> rpc_config
          in
          let* client_config =
            setup_client_config
              (full :> Client_context.printer)
              parsed_args
              base_dir
              rpc_config
          in
          register_signer
            (module C)
            (client_config :> Client_context.io_wallet)
            rpc_config
            parsed_config_file ;
          let* other_commands =
            match parsed_args with
            | Some parsed_args ->
                select_commands
                  (client_config :> RPC_client_unix.http_ctxt)
                  parsed_args
            | None -> return_nil
          in
          let commands =
            Tezos_clic.add_manual
              ~executable_name
              ~global_options
              (if Unix.isatty Unix.stdout then Tezos_clic.Ansi
              else Tezos_clic.Plain)
              Format.std_formatter
              (C.clic_commands
                 ~base_dir
                 ~config_commands
                 ~builtin_commands
                 ~other_commands
                 ~require_auth)
          in
          match autocomplete with
          | Some (prev_arg, cur_arg, script) ->
              let* completions =
                Tezos_clic.autocompletion
                  ~script
                  ~cur_arg
                  ~prev_arg
                  ~args:original_args
                  ~global_options
                  commands
                  client_config
              in
              List.iter print_endline completions ;
              return_unit
          | None -> Tezos_clic.dispatch commands client_config remaining
        in
        match r with
        | Ok () -> Lwt.return 0
        | Error [Tezos_clic.Version] ->
            let version = Tezos_version_value.Bin_version.version_string in
            Format.printf "%s\n" version ;
            Lwt.return 0
        | Error [Tezos_clic.Help command] ->
            Tezos_clic.usage
              Format.std_formatter
              ~executable_name
              ~global_options
              (match command with None -> [] | Some c -> [c]) ;
            Lwt.return 0
        | Error errs ->
            Tezos_clic.pp_cli_errors
              Format.err_formatter
              ~executable_name
              ~global_options
              ~default:Error_monad.pp
              errs ;
            Lwt.return 1)
      (function
        | Client_commands.Version_not_found ->
            Format.eprintf
              "@{<error>@{<title>Fatal error@}@} unknown protocol version.@." ;
            Lwt.return 1
        | Failure message ->
            Format.eprintf
              "@{<error>@{<title>Fatal error@}@}@.  @[<h 0>%a@]@."
              Format.pp_print_text
              message ;
            Lwt.return 1
        | exn ->
            Format.printf
              "@{<error>@{<title>Fatal error@}@}@.  @[<h 0>%a@]@."
              Format.pp_print_text
              (Printexc.to_string exn) ;
            Lwt.return 1)
  in
  Format.pp_print_flush Format.err_formatter () ;
  Format.pp_print_flush Format.std_formatter () ;
  let*! () = Tezos_base_unix.Internal_event_unix.close () in
  Lwt.return retcode

(* Where all the user friendliness starts *)
let run (module M : M)
    ~(select_commands :
       RPC_client_unix.http_ctxt ->
       Client_config.cli_args ->
       Client_context.full Tezos_clic.command list tzresult Lwt.t) =
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Stdlib.exit @@ Lwt_main.run @@ Lwt_exit.wrap_and_forward
  @@ main (module M) ~select_commands
