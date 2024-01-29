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

class unix_wallet ~base_dir ~password_filename : Client_context.wallet =
  object (self)
    val lock_mutex = Lwt_mutex.create ()

    method load_passwords =
      match password_filename with
      | None -> None
      | Some filename ->
          if Sys.file_exists filename then Some (Lwt_io.lines_of_file filename)
          else None

    method read_file path =
      let open Lwt_syntax in
      Lwt.catch
        (fun () ->
          let* content = Lwt_io.(with_file ~mode:Input path read) in
          return_ok content)
        (fun exn -> failwith "cannot read file (%s)" (Printexc.to_string exn))

    method private filename alias_name =
      Filename.concat
        base_dir
        (String.map (function ' ' -> '_' | c -> c) alias_name ^ "s")

    method get_base_dir = base_dir

    method with_lock : type a. (unit -> a Lwt.t) -> a Lwt.t =
      fun f ->
        Lwt_mutex.with_lock lock_mutex (fun () ->
            let open Lwt_syntax in
            let unlock fd =
              let fd = Lwt_unix.unix_file_descr fd in
              Unix.lockf fd Unix.F_ULOCK 0 ;
              Unix.close fd
            in
            let lock () =
              let* fd =
                Lwt_unix.openfile
                  (Filename.concat base_dir "wallet_lock")
                  Lwt_unix.[O_CREAT; O_WRONLY]
                  0o644
              in
              let* () = Lwt_unix.lockf fd Unix.F_LOCK 0 in
              let sighandler =
                Lwt_unix.on_signal Sys.sigint (fun _s -> unlock fd)
              in
              Lwt.return (fd, sighandler)
            in
            let* fd, sh = lock () in
            (* catch might be useless if f always uses the error monad *)
            let* res =
              Lwt.finalize f (fun () ->
                  unlock fd ;
                  Lwt.return_unit)
            in
            Lwt_unix.disable_signal_handler sh ;
            Lwt.return res)

    method load : type a.
        string -> default:a -> a Data_encoding.encoding -> a tzresult Lwt.t =
      fun alias_name ~default encoding ->
        let open Lwt_result_syntax in
        let filename = self#filename alias_name in
        if not (Sys.file_exists filename) then return default
        else
          let* json =
            trace_eval
              (fun () ->
                error_of_fmt "could not read the %s alias file" alias_name)
              (Lwt_utils_unix.Json.read_file filename)
          in
          match Data_encoding.Json.destruct encoding json with
          | exception e ->
              failwith
                "did not understand the %s alias file %s : %s"
                alias_name
                filename
                (Printexc.to_string e)
          | data -> return data

    method write : type a.
        string -> a -> a Data_encoding.encoding -> unit tzresult Lwt.t =
      fun alias_name list encoding ->
        let open Lwt_result_syntax in
        trace_eval (fun () ->
            error_of_fmt "could not write the %s alias file." alias_name)
        @@ Error_monad.catch_es (fun () ->
               let*! () = Lwt_utils_unix.create_dir base_dir in
               let filename = self#filename alias_name in
               let json = Data_encoding.Json.construct encoding list in
               let content = Data_encoding.Json.to_string ~minify:false json in
               let*! res =
                 Lwt_utils_unix.with_atomic_open_out ~overwrite:true filename
                 @@ fun chan -> Lwt_utils_unix.write_string chan content
               in
               match res with
               | Ok x -> return x
               | Error {unix_code; caller; arg; _} ->
                   tzfail (Exn (Unix.Unix_error (unix_code, caller, arg))))

    method last_modification_time : string -> float option tzresult Lwt.t =
      let open Lwt_result_syntax in
      fun alias_name ->
        let filename = self#filename alias_name in
        let*! exists = Lwt_unix.file_exists filename in
        if exists then
          let* stat = Error_monad.catch_s (fun () -> Lwt_unix.stat filename) in
          return_some stat.st_mtime
        else return_none
  end

class unix_prompter : Client_context.prompter =
  object
    method prompt : type a. (a, string tzresult) Client_context.lwt_format -> a
        =
      Format.kasprintf (fun msg ->
          print_string msg ;
          let line = read_line () in
          Lwt.return_ok line)

    method prompt_password : type a.
        (a, Bytes.t tzresult) Client_context.lwt_format -> a =
      Format.kasprintf (fun msg ->
          print_string msg ;
          let line =
            if Unix.isatty Unix.stdin then Lwt_utils_unix.getpass ()
            else read_line ()
          in
          Lwt.return_ok (Bytes.of_string line))

    method multiple_password_retries = true
  end

class unix_logger ~base_dir : Client_context.printer =
  let startup = Format.asprintf "%a" Time.System.pp_hum (Time.System.now ()) in
  let log channel msg =
    let open Lwt_syntax in
    match channel with
    | "stdout" ->
        print_endline msg ;
        Lwt.return_unit
    | "stderr" ->
        prerr_endline msg ;
        Lwt.return_unit
    | log ->
        let open Filename.Infix in
        let* () = Lwt_utils_unix.create_dir (base_dir // "logs" // log) in
        Lwt_io.with_file
          ~flags:Unix.[O_APPEND; O_CREAT; O_WRONLY]
          ~mode:Lwt_io.Output
          (base_dir // "logs" // log // startup)
          (fun chan -> Lwt_io.write chan msg)
  in
  object
    inherit Client_context.simple_printer log
  end

class unix_io_wallet ~base_dir ~password_filename : Client_context.io_wallet =
  object
    inherit unix_wallet ~base_dir ~password_filename

    inherit unix_logger ~base_dir

    inherit unix_prompter
  end

class unix_ui : Client_context.ui =
  object
    method sleep f = Lwt_unix.sleep f

    method exit : 'a. int -> 'a = fun i -> Lwt_exit.exit_and_raise i

    method now = Tezos_base.Time.System.now
  end

class unix_full ~base_dir ~chain ~block ~confirmations ~password_filename
  ~rpc_config ~verbose_rpc_error_diagnostics : Client_context.full =
  object
    inherit unix_logger ~base_dir

    inherit unix_prompter

    inherit unix_wallet ~base_dir ~password_filename

    inherit
      Tezos_rpc_http_client_unix.RPC_client_unix.http_ctxt
        rpc_config
        (Media_type.Command_line.of_command_line rpc_config.media_type)

    inherit unix_ui

    method chain = chain

    method block = block

    method confirmations = confirmations

    method verbose_rpc_error_diagnostics = verbose_rpc_error_diagnostics
  end

class unix_mockup ~base_dir ~mem_only ~mockup_env ~chain_id ~rpc_context
  ~protocol_data : Client_context.full =
  object
    inherit unix_logger ~base_dir

    inherit unix_prompter

    inherit unix_wallet ~base_dir ~password_filename:None

    inherit
      Tezos_mockup.RPC_client.mockup_ctxt
        base_dir
        mem_only
        mockup_env
        chain_id
        rpc_context
        protocol_data

    inherit unix_ui

    method chain = `Hash chain_id

    method block = `Head 0

    method confirmations = None

    method verbose_rpc_error_diagnostics = false
  end

class unix_proxy ~base_dir ?protocol ~chain ~block ~confirmations
  ~password_filename ~rpc_config ~mode () : Client_context.full =
  object
    inherit unix_logger ~base_dir

    inherit unix_prompter

    inherit unix_wallet ~base_dir ~password_filename

    inherit
      Tezos_proxy_rpc.RPC_client.http_local_ctxt
        (new unix_logger ~base_dir)
        (new Tezos_rpc_http_client_unix.RPC_client_unix.http_ctxt
           rpc_config
           (Media_type.Command_line.of_command_line rpc_config.media_type))
        mode
        protocol

    inherit unix_ui

    method chain = chain

    method block = block

    method confirmations = confirmations

    method verbose_rpc_error_diagnostics = false
  end
