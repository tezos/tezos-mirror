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

let default_tcp_host =
  match Sys.getenv_opt "TEZOS_SIGNER_TCP_HOST" with
  | None -> "localhost"
  | Some host -> host

let default_tcp_port =
  match Sys.getenv_opt "TEZOS_SIGNER_TCP_PORT" with
  | None -> "7732"
  | Some port -> port

let default_https_host =
  match Sys.getenv_opt "TEZOS_SIGNER_HTTPS_HOST" with
  | None -> "localhost"
  | Some host -> host

let default_https_port =
  match Sys.getenv_opt "TEZOS_SIGNER_HTTPS_PORT" with
  | None -> "443"
  | Some port -> port

let default_http_host =
  match Sys.getenv_opt "TEZOS_SIGNER_HTTP_HOST" with
  | None -> "localhost"
  | Some host -> host

let default_http_port =
  match Sys.getenv_opt "TEZOS_SIGNER_HTTP_PORT" with
  | None -> "6732"
  | Some port -> port

let group =
  {
    Tezos_clic.name = "signer";
    title = "Commands specific to the signing daemon";
  }

let magic_bytes_arg =
  Tezos_clic.arg
    ~doc:"values allowed for the magic bytes, defaults to any"
    ~short:'M'
    ~long:"magic-bytes"
    ~placeholder:"0xHH,0xHH,..."
    (Tezos_clic.parameter (fun _ s ->
         Lwt.return
           (List.map_e
              (fun s ->
                match int_of_string_opt s with
                | Some b when 0 <= b && b <= 255 -> Ok b
                | Some _ (* out of range *) | None (* not a number *) ->
                    error_with
                      "Bad format for magic bytes, a series of numbers is \
                       expected, separated by commas.")
              (String.split_no_empty ',' s))))

let high_watermark_switch =
  Tezos_clic.switch
    ~doc:
      "high watermark restriction\n\
       Stores the highest level signed for blocks and attestations for each \
       address, and forbids to sign a level and round that are inferior or \
       equal afterwards, except for the exact same input data."
    ~short:'W'
    ~long:"check-high-watermark"
    ()

let pidfile_arg =
  Tezos_clic.arg
    ~doc:"write process id in file"
    ~short:'P'
    ~long:"pidfile"
    ~placeholder:"filename"
    (Tezos_clic.parameter (fun _ s -> Lwt.return_ok s))

let may_setup_pidfile pidfile_opt f =
  match pidfile_opt with
  | None -> f ()
  | Some pidfile ->
      Lwt_lock_file.try_with_lock
        ~when_locked:(fun () ->
          failwith "Failed to create the pidfile: %s" pidfile)
        ~filename:pidfile
        f

let commands base_dir require_auth : Client_context.full Tezos_clic.command list
    =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  Tezos_signer_backends_unix.Ledger.commands ()
  @ Client_keys_commands.commands None
  @ [
      command
        ~group
        ~desc:"Launch a signer daemon over a TCP socket."
        (args6
           pidfile_arg
           magic_bytes_arg
           high_watermark_switch
           (default_arg
              ~doc:"listening address or host name"
              ~short:'a'
              ~long:"address"
              ~placeholder:"host|address"
              ~default:default_tcp_host
              (parameter (fun _ s -> return s)))
           (default_arg
              ~doc:"listening TCP port or service name"
              ~short:'p'
              ~long:"port"
              ~placeholder:"port number"
              ~default:default_tcp_port
              (parameter (fun _ s -> return s)))
           (default_arg
              ~doc:
                "timeout before the signer closes client connection (in \
                 seconds)"
              ~short:'t'
              ~long:"timeout"
              ~placeholder:"timeout"
              ~default:
                (Printf.sprintf
                   "%.0f"
                   (Ptime.Span.to_float_s !Lwt_utils_unix.default_net_timeout))
              (parameter (fun _ s ->
                   return (Time.System.Span.of_seconds_exn (Float.of_string s))))))
        (prefixes ["launch"; "socket"; "signer"] @@ stop)
        (fun (pidfile, magic_bytes, check_high_watermark, host, port, timeout)
             cctxt ->
          may_setup_pidfile pidfile @@ fun () ->
          let* () = Tezos_signer_backends.Encrypted.decrypt_all cctxt in
          let* _ =
            Socket_daemon.run
              cctxt
              (Tcp (host, port, [AI_SOCKTYPE SOCK_STREAM]))
              ?magic_bytes
              ~check_high_watermark
              ~require_auth
              ~timeout
          in
          return_unit);
      command
        ~group
        ~desc:"Launch a signer daemon over a local Unix socket."
        (args4
           pidfile_arg
           magic_bytes_arg
           high_watermark_switch
           (default_arg
              ~doc:"path to the local socket file"
              ~short:'s'
              ~long:"socket"
              ~placeholder:"path"
              ~default:(Filename.concat base_dir "socket")
              (parameter (fun _ s -> return s))))
        (prefixes ["launch"; "local"; "signer"] @@ stop)
        (fun (pidfile, magic_bytes, check_high_watermark, path) cctxt ->
          may_setup_pidfile pidfile @@ fun () ->
          let* () = Tezos_signer_backends.Encrypted.decrypt_all cctxt in
          let* _ =
            Socket_daemon.run
              cctxt
              (Unix path)
              ?magic_bytes
              ~check_high_watermark
              ~require_auth
          in
          return_unit);
      command
        ~group
        ~desc:"Launch a signer daemon over HTTP."
        (args5
           pidfile_arg
           magic_bytes_arg
           high_watermark_switch
           (default_arg
              ~doc:"listening address or host name"
              ~short:'a'
              ~long:"address"
              ~placeholder:"host|address"
              ~default:default_http_host
              (parameter (fun _ s -> return s)))
           (default_arg
              ~doc:"listening HTTP port"
              ~short:'p'
              ~long:"port"
              ~placeholder:"port number"
              ~default:default_http_port
              (parameter (fun _ x ->
                   try return (int_of_string x)
                   with Failure _ -> failwith "Invalid port %s" x))))
        (prefixes ["launch"; "http"; "signer"] @@ stop)
        (fun (pidfile, magic_bytes, check_high_watermark, host, port) cctxt ->
          may_setup_pidfile pidfile @@ fun () ->
          let* () = Tezos_signer_backends.Encrypted.decrypt_all cctxt in
          Http_daemon.run_http
            cctxt
            ~host
            ~port
            ?magic_bytes
            ~check_high_watermark
            ~require_auth);
      command
        ~group
        ~desc:"Launch a signer daemon over HTTPS."
        (args5
           pidfile_arg
           magic_bytes_arg
           high_watermark_switch
           (default_arg
              ~doc:"listening address or host name"
              ~short:'a'
              ~long:"address"
              ~placeholder:"host|address"
              ~default:default_https_host
              (parameter (fun _ s -> return s)))
           (default_arg
              ~doc:"listening HTTPS port"
              ~short:'p'
              ~long:"port"
              ~placeholder:"port number"
              ~default:default_https_port
              (parameter (fun _ x ->
                   try return (int_of_string x)
                   with Failure _ -> failwith "Invalid port %s" x))))
        (prefixes ["launch"; "https"; "signer"]
        @@ param
             ~name:"cert"
             ~desc:"path to the TLS certificate"
             (parameter (fun _ s ->
                  if not (Sys.file_exists s) then
                    failwith "No such TLS certificate file %s" s
                  else return s))
        @@ param
             ~name:"key"
             ~desc:"path to the TLS key"
             (parameter (fun _ s ->
                  if not (Sys.file_exists s) then
                    failwith "No such TLS key file %s" s
                  else return s))
        @@ stop)
        (fun (pidfile, magic_bytes, check_high_watermark, host, port)
             cert
             key
             cctxt ->
          may_setup_pidfile pidfile @@ fun () ->
          let* () = Tezos_signer_backends.Encrypted.decrypt_all cctxt in
          Http_daemon.run_https
            cctxt
            ~host
            ~port
            ~cert
            ~key
            ?magic_bytes
            ~check_high_watermark
            ~require_auth);
      command
        ~group
        ~desc:"Authorize a given public key to perform signing requests."
        (args1
           (arg
              ~doc:"an optional name for the key (defaults to the hash)"
              ~short:'N'
              ~long:"name"
              ~placeholder:"name"
              (parameter (fun _ s -> return s))))
        (prefixes ["add"; "authorized"; "key"]
        @@ param
             ~name:"pk"
             ~desc:"full public key (Base58 encoded)"
             (parameter (fun _ s ->
                  Lwt.return (Tezos_crypto.Signature.Public_key.of_b58check s)))
        @@ stop)
        (fun name key cctxt ->
          let pkh = Tezos_crypto.Signature.Public_key.hash key in
          let name =
            match name with
            | Some name -> name
            | None -> Tezos_crypto.Signature.Public_key_hash.to_b58check pkh
          in
          Handler.Authorized_key.add ~force:false cctxt name key);
    ]

let home = try Sys.getenv "HOME" with Not_found -> "/root"

let default_base_dir = Filename.concat home ".tezos-signer"

let string_parameter () : (string, _) Tezos_clic.parameter =
  Tezos_clic.parameter (fun _ x -> Lwt.return_ok x)

let base_dir_arg () =
  Tezos_clic.arg
    ~long:"base-dir"
    ~short:'d'
    ~placeholder:"path"
    ~doc:
      ("signer data directory\n\
        The directory where the Tezos client will store all its data.\n\
        By default: '" ^ default_base_dir ^ "'.")
    (string_parameter ())

let require_auth_arg () =
  Tezos_clic.switch
    ~long:"require-authentication"
    ~short:'A'
    ~doc:"Require a signature from the caller to sign."
    ()

let password_filename_arg () =
  Tezos_clic.arg
    ~long:"password-filename"
    ~short:'f'
    ~placeholder:"filename"
    ~doc:"path to the password filename"
    (string_parameter ())

let global_options () =
  Tezos_clic.args3
    (base_dir_arg ())
    (require_auth_arg ())
    (password_filename_arg ())

module Signer_config = struct
  type t = string option * bool * string option

  let global_options = global_options

  let parse_config_args ctx argv =
    let open Lwt_result_syntax in
    let* (base_dir, require_auth, password_filename), remaining =
      Tezos_clic.parse_global_options (global_options ()) ctx argv
    in
    return
      ( {
          Client_config.default_parsed_config_args with
          parsed_args =
            Some
              {
                Client_config.default_cli_args with
                confirmations = None;
                password_filename;
              };
          base_dir;
          require_auth;
        },
        remaining )

  let default_chain = Client_config.default_chain

  let default_block = Client_config.default_block

  let default_base_dir = default_base_dir

  let default_media_type = Media_type.Command_line.Binary

  let other_registrations = None

  let default_daily_logs_path = None

  let clic_commands ~base_dir ~config_commands:_ ~builtin_commands:_
      ~other_commands ~require_auth =
    commands base_dir require_auth @ other_commands

  let logger = Some (RPC_client_unix.full_logger Format.err_formatter)
end

let () =
  Client_main_run.run
    (module Signer_config)
    ~select_commands:(fun _ _ -> Lwt_result_syntax.return_nil)
