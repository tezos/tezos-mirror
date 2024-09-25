(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

module Make (P : External_process_parameters.S) = struct
  type error +=
    | Inconsistent_handshake of string
    | Cannot_process_while_shutting_down

  let () =
    Error_monad.register_error_kind
      `Temporary
      ~id:(P.name ^ "_process.inconsistent_handshake")
      ~title:"Inconsistent handshake"
      ~description:(Format.sprintf "Inconsistent handshake with %s." P.name)
      ~pp:(fun ppf msg ->
        Format.fprintf ppf "Inconsistent handshake with %s: %s." P.name msg)
      Data_encoding.(obj1 (req "msg" string))
      (function Inconsistent_handshake msg -> Some msg | _ -> None)
      (fun msg -> Inconsistent_handshake msg)

  let () =
    Error_monad.register_error_kind
      `Temporary
      ~id:(P.name ^ "_process.cannot_process_while_shutting_down")
      ~title:"Cannot process while shutting down"
      ~description:
        (Format.sprintf "Cannot process while the %s is shutting down." P.name)
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "Cannot process while the %s is shutting down."
          P.name)
      Data_encoding.empty
      (function Cannot_process_while_shutting_down -> Some () | _ -> None)
      (fun () -> Cannot_process_while_shutting_down)

  module Events = struct
    open Internal_event.Simple

    let section = ["external_" ^ P.name; "process"]

    let declare_0 ~level ~name ~msg () =
      declare_0 ~section ~level ~name:(String.concat "_" [P.name; name]) ~msg ()

    let declare_1 ~level ~name ~msg ~pp1 =
      declare_1
        ~section
        ~level
        ~name:(String.concat "_" [P.name; name])
        ~msg
        ~pp1

    let declare_2 ~level ~name ~msg ~pp1 ~pp2 =
      declare_2
        ~section
        ~level
        ~name:(String.concat "_" [P.name; name])
        ~msg
        ~pp1
        ~pp2

    let init =
      declare_0
        ~level:Notice
        ~name:"process_initialized"
        ~msg:(Format.sprintf "external %s initialized" P.name)
        ()

    let init_error =
      declare_0
        ~level:Error
        ~name:"init_error"
        ~msg:
          (Format.sprintf
             "failed to initialize the external %s: shutting down"
             P.name)
        ()

    let close =
      declare_0
        ~level:Notice
        ~name:"proc_close"
        ~msg:(Format.sprintf "shutting down external %s" P.name)
        ()

    let process_exited_abnormally =
      let open Unix in
      let process_status_encoding =
        let open Data_encoding in
        union
          [
            case
              (Tag 0)
              ~title:"wexited"
              int31
              (function WEXITED i -> Some i | _ -> None)
              (fun i -> WEXITED i);
            case
              (Tag 1)
              ~title:"wsignaled"
              int31
              (function WSIGNALED i -> Some i | _ -> None)
              (fun i -> WSIGNALED i);
            case
              (Tag 2)
              ~title:"wstopped"
              int31
              (function WSTOPPED i -> Some i | _ -> None)
              (fun i -> WSTOPPED i);
          ]
      in
      declare_1
        ~level:Error
        ~name:"proc_status"
        ~msg:"{status_msg}"
        ~pp1:(fun fmt status ->
          match status with
          | WEXITED i ->
              Format.fprintf
                fmt
                "process terminated abnormally with exit code %i"
                i
          | WSIGNALED i ->
              Format.fprintf
                fmt
                "process was killed by signal %s"
                (Lwt_exit.signal_name i)
          | WSTOPPED i ->
              Format.fprintf
                fmt
                "process was stopped by signal %s"
                (Lwt_exit.signal_name i))
        ("status_msg", process_status_encoding)

    let process_exited_normally =
      declare_0
        ~level:Notice
        ~name:"proc_exited_normally"
        ~msg:"process terminated normally"
        ()

    let process_started =
      declare_1
        ~level:Notice
        ~name:"proc_started"
        ~msg:(Format.sprintf "%s process started with pid {pid}" P.name)
        ~pp1:Format.pp_print_int
        ("pid", Data_encoding.int31)

    let cannot_close =
      declare_0
        ~level:Info
        ~name:"cannot_close"
        ~msg:
          (Format.sprintf
             "cannot close the %s process: connection failed"
             P.name)
        ()

    let unresponsive_process =
      declare_0
        ~level:Notice
        ~name:"unresponsive"
        ~msg:
          (Format.sprintf
             "force quitting the %s process as it seems to be unresponsive"
             P.name)
        ()

    let cannot_start_process =
      declare_0
        ~level:Info
        ~name:"cannot_start_process"
        ~msg:
          (Format.sprintf
             "cannot start %s process: the node is shutting down"
             P.name)
        ()

    let request_for =
      declare_1
        ~level:Debug
        ~name:"proc_request"
        ~msg:"request for {request}"
        ~pp1:(fun fmt (P.Erequest r) -> P.request_pp fmt r)
        ("request", P.request_encoding)

    let request_result =
      declare_2
        ~level:Debug
        ~name:"proc_request_result"
        ~msg:"completion of {request_result} in {timespan}"
        ~pp1:(fun fmt (P.Erequest r) -> P.request_pp fmt r)
        ("request_result", P.request_encoding)
        ~pp2:Time.System.Span.pp_hum
        ("timespan", Time.System.Span.encoding)

    let emit = Internal_event.Simple.emit
  end

  type external_process = {
    process : Lwt_process.process_none;
    process_socket : Lwt_unix.file_descr;
    input : Lwt_io.output_channel;
    output : Lwt_io.input_channel;
    canceler : Lwt_canceler.t;
    clean_up_callback_id : Lwt_exit.clean_up_callback_id;
  }

  type process_status = Uninitialized | Running of external_process | Exiting

  type t = {
    parameters : P.parameters;
    process_path : string;
    mutable process : process_status;
    lock : Lwt_mutex.t;
  }

  (* The shutdown_timeout is used when closing the external
     process. It aims to allow it to shutdown gracefully. This delay
     is long enough to allow the process to successfully terminate
     its current task and is short enough to avoid bothering the
     user. *)
  let shutdown_timeout = 5.

  (* Returns a temporary path for the socket to be
     spawned. $XDG_RUNTIME_DIR is returned if the environment variable
     is defined. Otherwise, the default temporary directory is used. *)
  let get_temporary_socket_dir () =
    match Sys.getenv_opt "XDG_RUNTIME_DIR" with
    | Some xdg_runtime_dir when xdg_runtime_dir <> "" -> xdg_runtime_dir
    | Some _ | None -> Filename.get_temp_dir_name ()

  (* Ad-hoc request to make the handshake with the external process.
     This is expected to be used each time the external process
     process is (re)started.
     The scenario of the handshake is the following:
     - simultaneously, the node and the external process send some magic bytes
       to each others,
     - simultaneously, the node and the external process wait for each others
       magic bytes and check their validity,
     - handshake is finished. *)
  let process_handshake process_input process_output =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_unix_socket.send process_input Data_encoding.Variable.bytes P.magic
    in
    let*! magic =
      Lwt_unix_socket.recv process_output Data_encoding.Variable.bytes
    in
    fail_when
      (not (Bytes.equal magic P.magic))
      (Inconsistent_handshake "bad magic")

  (* Ad-hoc request to pass startup arguments to the external
     process. This is expected to be run after the
     [process_handshake].
     This is expected to be used each time the external
     process is (re)started.
     The scenario of the init is the following:
     - execute the handshake,
     - the node sends some parameters and waits for an ack,
     - the external process initializes it's state thanks to the
       given parameters,
     - the external process returns a ack to confirm a successful
       initialization,
     - the node receives the ack and continues,
     - initialization is finished.
  *)
  let process_init p process_input process_output =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_unix_socket.send process_input P.parameters_encoding p.parameters
    in
    let* () =
      Lwt_unix_socket.recv
        process_output
        (Error_monad.result_encoding Data_encoding.empty)
    in
    return_unit

  (* Proceeds to a full initialization of the external
     process by opening the communication channels, spawning the
     external process and calling the handshake and initialization
     functions.
     TODO: Add critical section for external process launch, see
     https://gitlab.com/tezos/tezos/-/issues/5175
  *)
  let start_process p =
    let open Lwt_result_syntax in
    let canceler = Lwt_canceler.create () in
    (* We assume that there is only one external process per socket *)
    let socket_dir = get_temporary_socket_dir () in
    let proc_name, args = P.command_line_args p.parameters ~socket_dir in
    let arg0 =
      match proc_name with
      | Some p -> p
      | None -> Filename.basename p.process_path
    in
    let args = arg0 :: args in
    let env = Unix.environment () in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/4837

       We unset the [env_var_name] environment variable environment
       variable so that events emitted by the external
       process are not mixed up with the events that could be printed
       by the external process. This is a temporary fix and a better
       solution would be welcome! *)
    let env =
      Array.to_seq env
      |> Seq.filter (fun binding ->
             match String.split_on_char '=' binding with
             | env_var_name :: _
               when env_var_name = Internal_event_unix.env_var_name ->
                 false
             | _ -> true)
      |> Array.of_seq
    in
    let process =
      Lwt_process.open_process_none ~env (p.process_path, Array.of_list args)
    in
    let socket_path = P.socket_path ~socket_dir ~pid:process#pid in
    (* Make sure that the mimicked anonymous file descriptor is
        removed if the spawn of the process is interrupted. Thus, we
        avoid generating potential garbage in the [socket_dir].
        No interruption can occur since the resource was created
        because there are no yield points. *)
    let clean_process_fd socket_path =
      Lwt.catch
        (fun () -> Lwt_unix.unlink socket_path)
        (function
          | Unix.Unix_error (ENOENT, _, _) ->
              (* The file does not exist *)
              Lwt.return_unit
          | Unix.Unix_error (EACCES, _, _) ->
              (* We ignore failing on EACCES as no file was created *)
              Lwt.return_unit
          | exn -> Lwt.reraise exn)
    in
    let process_fd_cleaner =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          clean_process_fd socket_path)
    in
    let* process_socket =
      protect
        ~on_error:(function
          | [Exn Lwt_unix.Timeout] as err ->
              let*! () = Events.(emit init_error ()) in
              process#terminate ;
              let*! _ = Lwt_exit.exit_and_wait 1 in
              Lwt.return_error err
          | err -> Lwt.return_error err)
        (fun () ->
          Lwt.finalize
            (fun () ->
              let* process_socket =
                Lwt_unix_socket.create_socket_listen
                  ~canceler
                  ~max_requests:1
                  ~socket_path
              in
              let*! v, _ =
                Lwt.pick [Lwt_unix.timeout 30.; Lwt_unix.accept process_socket]
              in
              let*! () = Lwt_unix.close process_socket in
              return v)
            (fun () ->
              (* As the external external process is now started, we can
                  unlink the named socket. Indeed, the file descriptor will
                  remain valid as long as at least one process keeps it
                  open. This method mimics an anonymous file descriptor
                  without relying on Linux specific features. It also
                  trigger the clean up procedure if some sockets related
                  errors are thrown. *)
              clean_process_fd socket_path))
    in
    Lwt_exit.unregister_clean_up_callback process_fd_cleaner ;
    (* Register clean up callback to ensure that the process
       will be terminated even if the node is brutally stopped. *)
    let clean_up_callback_id =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          Lwt.return (Stdlib.at_exit (fun () -> process#terminate)))
    in
    let process_input = Lwt_io.of_fd ~mode:Output process_socket in
    let process_output = Lwt_io.of_fd ~mode:Input process_socket in
    let*! () = Events.(emit process_started process#pid) in
    p.process <-
      Running
        {
          process;
          process_socket;
          input = process_input;
          output = process_output;
          canceler;
          clean_up_callback_id;
        } ;
    let* () = process_handshake process_input process_output in
    let* () = process_init p process_input process_output in
    return (process, process_input, process_output)

  (* Inspects the process's state and return it. If the process is
     in an inconsistent state, it will be restarted automatically --
     by running [start_process]. *)
  let process_state p =
    let open Lwt_result_syntax in
    match p.process with
    | Running
        {
          process;
          process_socket;
          input = process_input;
          output = process_output;
          canceler;
          clean_up_callback_id;
        } -> (
        match process#state with
        | Running -> return (process, process_input, process_output)
        | Exited status ->
            (* When the process is in an inconsistent state, we restart
               it automatically. *)
            let*! () = Error_monad.cancel_with_exceptions canceler in
            Lwt_exit.unregister_clean_up_callback clean_up_callback_id ;
            let*! () =
              Lwt.catch
                (fun () -> Lwt_unix.close process_socket)
                (fun _ -> Lwt.return_unit)
            in
            p.process <- Uninitialized ;
            let*! () = Events.(emit process_exited_abnormally status) in
            start_process p)
    | Uninitialized -> start_process p
    | Exiting ->
        let*! () = Events.(emit cannot_start_process ()) in
        tzfail Cannot_process_while_shutting_down

  (* Sends the given request to the external process. If the request
     failed to be fulfilled, the status of the external process is
     set to Uninitialized and the associated error is propagated. *)
  let send_request p request =
    let open Lwt_result_syntax in
    let prequest = P.Erequest request in
    let* process, process_input, process_output = process_state p in
    Lwt.catch
      (fun () ->
        (* Make sure that the promise is not cancelled between a send
           and recv *)
        let* res =
          Lwt.protected
            (Lwt_mutex.with_lock p.lock (fun () ->
                 let now = Time.System.now () in
                 let*! () = Events.(emit request_for prequest) in
                 let*! () =
                   Lwt_unix_socket.send
                     process_input
                     P.request_encoding
                     prequest
                 in
                 let*! res =
                   Lwt_unix_socket.recv_result
                     process_output
                     Data_encoding.(
                       tup2
                         (P.result_encoding request)
                         (option
                            (tup2
                               (option Profiler.report_encoding)
                               (option Profiler.report_encoding))))
                 in
                 let timespan =
                   let then_ = Time.System.now () in
                   Ptime.diff then_ now
                 in
                 let*! () = Events.(emit request_result (prequest, timespan)) in
                 Lwt.return res))
        in
        match process#state with
        | Running -> return res
        | Exited status ->
            p.process <- Uninitialized ;
            let*! () = Events.(emit process_exited_abnormally status) in
            return res)
      (fun exn ->
        let*! () =
          match process#state with
          | Running -> Lwt.return_unit
          | Exited status ->
              let*! () = Events.(emit process_exited_abnormally status) in
              p.process <- Uninitialized ;
              Lwt.return_unit
        in
        fail_with_exn exn)

  (* The initialization phase aims to configure the external process
     and start it's associated process. This will result in the call
     of [process_handshake] and [process_init].
     Note that it is important to have [init] as a blocking promise as
     the external process must initialize the context (in RW) before
     the node tries to open it (in RO).*)
  let init parameters ~process_path =
    let open Lwt_result_syntax in
    let process =
      {
        parameters;
        process_path;
        process = Uninitialized;
        lock = Lwt_mutex.create ();
      }
    in
    let* (_ :
           Lwt_process.process_none
           * Lwt_io.output Lwt_io.channel
           * Lwt_io.input Lwt_io.channel) =
      start_process process
    in
    let*! () = Events.(emit init ()) in
    return process

  let reconfigure_event_logging process config =
    let open Lwt_result_syntax in
    let* (), _report =
      send_request process (P.reconfigure_event_logging_request config)
    in
    return_unit

  let close p =
    let open Lwt_syntax in
    let* () = Events.(emit close ()) in
    match p.process with
    | Running {process; input = process_input; canceler; _} ->
        let request = P.terminate_request in
        let* () = Events.(emit request_for request) in
        let* () =
          Lwt.catch
            (fun () ->
              p.process <- Exiting ;
              (* Try to trigger the clean shutdown of the external process. *)
              Lwt_unix_socket.send process_input P.request_encoding request)
            (function
              | Unix.Unix_error (ECONNREFUSED, _, _)
              | Unix.Unix_error (EPIPE, _, _)
              | Unix.Unix_error (ENOTCONN, _, _) ->
                  (* It may fail if the external process is not
                     responding (connection already closed) and is
                     killed afterwards. No need to propagate the error. *)
                  let* () = Events.(emit cannot_close ()) in
                  Lwt.return_unit
              | e -> Lwt.reraise e)
        in
        let* () =
          Lwt.catch
            (fun () ->
              Lwt_unix.with_timeout shutdown_timeout (fun () ->
                  let* s = process#status in
                  match s with
                  | Unix.WEXITED 0 -> Events.(emit process_exited_normally ())
                  | status ->
                      let* () =
                        Events.(emit process_exited_abnormally status)
                      in
                      process#terminate ;
                      Lwt.return_unit))
            (function
              | Lwt_unix.Timeout -> Events.(emit unresponsive_process) ()
              | err -> Lwt.reraise err)
        in
        let* () = Error_monad.cancel_with_exceptions canceler in
        (* Set the process status as uninitialized so that the process can be
           restarted and avoid raising [Cannot_process_while_shutting_down]. *)
        p.process <- Uninitialized ;
        Lwt.return_unit
    | Uninitialized | Exiting -> Lwt.return_unit
end
