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
  module Hypervisor_params = Hypervisor_process_main.Make_process_parameters (P)

  type error +=
    | Inconsistent_hypervisor_handshake of string
    | Inconsistent_hypervisee_handshake of string
    | Cannot_process_while_shutting_down

  let () =
    Error_monad.register_error_kind
      `Temporary
      ~id:(P.name ^ "_hypervisor_process.inconsistent_handshake")
      ~title:"Inconsistent handshake"
      ~description:
        (Format.sprintf
           "Inconsistent handshake with %s_hypervisor_process."
           P.name)
      ~pp:(fun ppf msg ->
        Format.fprintf
          ppf
          "Inconsistent handshake with %s_hypervisor_process: %s."
          P.name
          msg)
      Data_encoding.(obj1 (req "msg" string))
      (function Inconsistent_hypervisor_handshake msg -> Some msg | _ -> None)
      (fun msg -> Inconsistent_hypervisor_handshake msg)

  let () =
    Error_monad.register_error_kind
      `Temporary
      ~id:(P.name ^ "_hypervisee_process.inconsistent_handshake")
      ~title:"Inconsistent handshake"
      ~description:
        (Format.sprintf
           "Inconsistent handshake with %s_hypervisee_process."
           P.name)
      ~pp:(fun ppf msg ->
        Format.fprintf
          ppf
          "Inconsistent handshake with %s_hypervisee_process: %s."
          P.name
          msg)
      Data_encoding.(obj1 (req "msg" string))
      (function Inconsistent_hypervisee_handshake msg -> Some msg | _ -> None)
      (fun msg -> Inconsistent_hypervisee_handshake msg)

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

    let hypervisee_initialized =
      declare_1
        ~level:Info
        ~name:"hypervisee_initialized"
        ~msg:"Hypervisee initialized with pid {pid}"
        ~pp1:Format.pp_print_int
        ("pid", Data_encoding.int31)

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

  type hypervisee = {
    pid : int;
    input : Lwt_io.output_channel;
    output : Lwt_io.input_channel;
  }

  type t = {
    parameters : P.parameters;
    process_path : string;
    external_process : external_process;
    mutable hypervisee : hypervisee;
    lock : Lwt_mutex.t;
    mutable restarting : unit tzresult Lwt_condition.t option;
  }

  (* The shutdown_timeout is used when closing the external
     process. It aims to allow it to shutdown gracefully. This delay
     is long enough to allow the process to successfully terminate
     its current task and is short enough to avoid bothering the
     user. *)
  let shutdown_timeout = 5.

  (** [handshake input output magic] handshakes, the scenario is the following:
      - simultaneously, the node and the external process send some magic bytes
        to each others,
      - simultaneously, the node and the external process wait for each others
        magic bytes and check their validity,
      - handshake is finished.
  *)
  let handshake input output magic err =
    let open Lwt_result_syntax in
    let*! () = Lwt_unix_socket.send input Data_encoding.Variable.bytes magic in
    let*! recv_magic =
      Lwt_unix_socket.recv output Data_encoding.Variable.bytes
    in
    fail_when (not (Bytes.equal recv_magic magic)) err

  (* Ad-hoc request to make the handshake with the external process.
     This is expected to be used each time the hypervisee process is
     (re)started. *)
  let process_hypervisee_handshake process_input process_output =
    handshake
      process_input
      process_output
      P.magic
      (Inconsistent_hypervisee_handshake "bad magic")

  (* Similar to {!process_hypervisee_handshake} but it is not expected
     to be used more than once. *)
  let process_hypervisor_handshake process_input process_output =
    handshake
      process_input
      process_output
      Hypervisor_process_main.magic
      (Inconsistent_hypervisor_handshake "bad magic")

  let init_hypervisor process_input process_output internal_events =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_unix_socket.send
        process_input
        Internal_event_config.encoding
        internal_events
    in
    let* () =
      Lwt_unix_socket.recv
        process_output
        (Error_monad.result_encoding Data_encoding.empty)
    in
    return_unit

  let process_hypervisee_init input output parameters =
    let open Lwt_result_syntax in
    let*! () = Lwt_unix_socket.send input P.parameters_encoding parameters in
    let* () =
      Lwt_unix_socket.recv
        output
        (Error_monad.result_encoding Data_encoding.empty)
    in
    return_unit

  let process_socket ~canceler ~socket_path process =
    let open Lwt_result_syntax in
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
    let* r =
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
    return r

  (* Proceeds to a full initialization of the external
     process by opening the communication channels, spawning the
     external process and calling the handshake and initialization
     functions.
     TODO: Add critical section for external process launch, see
     https://gitlab.com/tezos/tezos/-/issues/5175
  *)
  let start_process ~process_path parameters =
    let open Lwt_result_syntax in
    let canceler = Lwt_canceler.create () in
    (* We assume that there is only one external process per socket *)
    let socket_dir = Hypervisor_process_main.get_temporary_socket_dir () in
    let args = [P.hypervisor_name; "--socket-dir"; socket_dir] in
    let env = Unix.environment () in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/4837

       We unset the [env_var_name] environment variable environment
       variable so that events emitted by the external
       process are not mixed up with the events that could be printed
       by the external process. This is a temporary fix and a better
       solution would be welcome! *)
    let env =
      if P.share_sink then env
      else
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
      Lwt_process.open_process_none ~env (process_path, Array.of_list args)
    in
    let socket_path = P.socket_path ~socket_dir ~pid:process#pid in
    let* process_socket = process_socket ~canceler ~socket_path process in
    (* Register clean up callback to ensure that the process
       will be terminated even if the node is brutally stopped. *)
    let clean_up_callback_id =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          Lwt.return (Stdlib.at_exit (fun () -> process#terminate)))
    in
    let process_input = Lwt_io.of_fd ~mode:Output process_socket in
    let process_output = Lwt_io.of_fd ~mode:Input process_socket in
    let external_process =
      {
        process;
        process_socket;
        input = process_input;
        output = process_output;
        canceler;
        clean_up_callback_id;
      }
    in
    let* () = process_hypervisor_handshake process_input process_output in
    let* () =
      init_hypervisor
        process_input
        process_output
        (P.internal_events parameters)
    in
    return external_process

  let start_hypervisee (external_process : external_process) parameters :
      hypervisee tzresult Lwt.t =
    let open Lwt_result_syntax in
    Lwt.catch
      (fun () ->
        (* Ask the hypervisor to start an hypervisee. *)
        let request = Hypervisor_params.Params.(Start_hypervisee) in
        let*! () =
          Lwt_unix_socket.send
            external_process.input
            Hypervisor_params.Params.request_encoding
            (Erequest request)
        in
        (* Hypervisor answers the socket path the hypervisee will connect. *)
        let* (hypervisee_socket_path, hypervisee_pid), _profiler =
          Lwt_unix_socket.recv_result
            external_process.output
            Data_encoding.(
              tup2
                (Hypervisor_params.Params.result_encoding request)
                (option
                   (tup2
                      (option Profiler.report_encoding)
                      (option Profiler.report_encoding))))
        in
        let* socket_process =
          process_socket
            ~canceler:external_process.canceler
            ~socket_path:hypervisee_socket_path
            external_process.process
        in
        let hypervisee_output = Lwt_io.of_fd ~mode:Input socket_process in
        let hypervisee_input = Lwt_io.of_fd ~mode:Output socket_process in
        (* Handshake with the hypervisee. *)
        let* () =
          process_hypervisee_handshake hypervisee_input hypervisee_output
        in
        (* Initialize the hypervisee. *)
        let* () =
          process_hypervisee_init hypervisee_input hypervisee_output parameters
        in
        let*! () = Events.(emit hypervisee_initialized hypervisee_pid) in
        return
          {
            pid = hypervisee_pid;
            input = hypervisee_input;
            output = hypervisee_output;
          })
      (fun exn ->
        let*! () =
          match external_process.process#state with
          | Running -> Lwt.return_unit
          | Exited (WEXITED 127) ->
              (* Exit Code 127 during shutdown as processes are killed
                 asynchronously. Main process, External watchdogs and hypervisor
                 processes might be unable to call specific subprocesses
                 commands *)
              Lwt_exit.exit_and_raise 0
          | Exited status ->
              let*! () = Events.(emit process_exited_abnormally status) in
              Lwt_exit.exit_and_raise 1
        in
        fail_with_exn exn)

  let restart_hypervisee ~stop_hypervisee p =
    let open Lwt_result_syntax in
    match p.restarting with
    | Some restart ->
        (* The hypervisor is already in the process of restarting the
           hypervisee. Simply wait for it to be done. *)
        Lwt_condition.wait restart
    | None ->
        let restart_cond = Lwt_condition.create () in
        p.restarting <- Some restart_cond ;
        Lwt.catch
          (fun () ->
            (* Terminate the hypervisee. *)
            let*! () =
              if stop_hypervisee then
                Lwt_unix_socket.send
                  p.hypervisee.input
                  P.request_encoding
                  P.terminate_request
              else Lwt.return_unit
            in
            (* Ask the hypervisor to make sure the hypervisee is terminated. *)
            let hypervisor_request = Hypervisor_params.Params.Stop_hypervisee in
            let*! () =
              Lwt_unix_socket.send
                p.external_process.input
                Hypervisor_params.Params.request_encoding
                (Erequest hypervisor_request)
            in
            let* (), _profiler =
              Lwt_unix_socket.recv_result
                p.external_process.output
                Data_encoding.(
                  tup2
                    (Hypervisor_params.Params.result_encoding
                       hypervisor_request)
                    (option
                       (tup2
                          (option Profiler.report_encoding)
                          (option Profiler.report_encoding))))
            in
            let*! () = Lwt_io.close p.hypervisee.output in
            (* Restart the hypervisee. *)
            let* hypervisee =
              start_hypervisee p.external_process p.parameters
            in
            p.hypervisee <- hypervisee ;
            Lwt_condition.broadcast restart_cond (Ok ()) ;
            p.restarting <- None ;
            return_unit)
          (fun exn ->
            let*! () =
              match p.external_process.process#state with
              | Running -> Lwt.return_unit
              | Exited (WEXITED 127) -> Lwt.return_unit
              | Exited status -> Events.(emit process_exited_abnormally status)
            in
            let res = error_with_exn exn in
            Lwt_condition.broadcast restart_cond res ;
            p.restarting <- None ;
            Lwt.return res)

  (* Sends the given request to the external process. If the request
     failed to be fulfilled, the status of the external process is
     set to Uninitialized and the associated error is propagated. *)
  let send_request p request =
    let open Lwt_result_syntax in
    let rec send_request_aux ~retried =
      let prequest = P.Erequest request in
      Lwt.catch
        (fun () ->
          let* () =
            (* Wait for any pending restart *)
            match p.restarting with
            | Some restart -> Lwt_condition.wait restart
            | None -> return_unit
          in
          (* Make sure that the promise is not cancelled between a send
             and recv *)
          Lwt.protected @@ Lwt_mutex.with_lock p.lock
          @@ fun () ->
          let now = Time.System.now () in
          let*! () = Events.(emit request_for prequest) in
          let*! () =
            Lwt_unix_socket.send p.hypervisee.input P.request_encoding prequest
          in
          let*! res =
            Lwt_unix_socket.recv_result
              p.hypervisee.output
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
          Lwt.return res)
        (fun exn ->
          match exn with
          | Lwt.Canceled ->
              (* The request was canceled (e.g. the bootstrap pipeline
                 was canceled due to a peer disconnection). The external
                 process is still healthy, do not restart it. *)
              fail_with_exn exn
          | _ ->
              if retried then fail_with_exn exn
              else
                (* The hypervisee appears to be down, we restart it. *)
                let* () = restart_hypervisee ~stop_hypervisee:false p in
                send_request_aux ~retried:true)
    in
    send_request_aux ~retried:false

  let restart_hypervisee = restart_hypervisee ~stop_hypervisee:true

  let pid p = p.hypervisee.pid

  (* The initialization phase aims to configure the external process
     and start its associated process. This will result in the call
     of [process_handshake] and [process_init]. *)
  let init parameters ~process_path =
    let open Lwt_result_syntax in
    let lock = Lwt_mutex.create () in
    let* external_process = start_process ~process_path parameters in
    let* hypervisee = start_hypervisee external_process parameters in
    let*! () = Events.(emit init ()) in
    return
      {
        parameters;
        process_path;
        external_process;
        hypervisee;
        lock;
        restarting = None;
      }

  let reconfigure_event_logging process config =
    let open Lwt_result_syntax in
    let* (), _report =
      send_request process (P.reconfigure_event_logging_request config)
    in
    return_unit

  let close p =
    let open Lwt_syntax in
    let* () = Events.(emit close ()) in
    let {process; input = process_input; canceler; _} = p.external_process in
    let request = P.terminate_request in
    let* () = Events.(emit request_for request) in
    (* Terminates the hypervisee. *)
    let* () =
      Lwt.catch
        (fun () ->
          (* Try to trigger the clean shutdown of the external process. *)
          Lwt_unix_socket.send p.hypervisee.input P.request_encoding request)
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
    (* Terminates the hypervisor. *)
    let* () =
      Lwt.catch
        (fun () ->
          (* Try to trigger the clean shutdown of the external process. *)
          Lwt_unix_socket.send
            process_input
            Hypervisor_params.Params.request_encoding
            Hypervisor_params.Params.terminate_request)
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
    let* () = Lwt_io.close p.hypervisee.output
    and* () = Lwt_io.close p.external_process.output in
    let* () =
      Lwt.catch
        (fun () ->
          Lwt_unix.with_timeout shutdown_timeout (fun () ->
              let* s = process#status in
              match s with
              | Unix.WEXITED 0 | Unix.WEXITED 127 ->
                  Events.(emit process_exited_normally ())
              | status ->
                  let* () = Events.(emit process_exited_abnormally status) in
                  process#terminate ;
                  Lwt.return_unit))
        (function
          | Lwt_unix.Timeout -> Events.(emit unresponsive_process) ()
          | err -> Lwt.reraise err)
    in
    let* () = Error_monad.cancel_with_exceptions canceler in
    Lwt.return_unit
end
