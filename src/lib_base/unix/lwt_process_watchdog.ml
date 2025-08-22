(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

module type NAME = sig
  val base : string list

  val component : string list
end

module type EVENTS = sig
  open Internal_event.Simple

  val emit : 'a t -> 'a -> unit Lwt.t

  val shutting_down_process : unit t

  val process_started : int t

  val process_exited_abnormally : (int * Unix.process_status) t

  val cannot_start_process : string t

  val waiting_for_process_restart : float t
end

module MakeEvent (N : NAME) : EVENTS = struct
  let emit = Internal_event.Simple.emit

  let section = N.base

  let component_base_name = String.concat "_" N.component

  let component_name = String.concat " " N.component

  let shutting_down_process =
    let open Internal_event.Simple in
    declare_0
      ~section
      ~name:(Format.sprintf "shutting_down_%s" component_base_name)
      ~msg:(Format.sprintf "shutting down the %s" component_name)
      ~level:Notice
      ()

  let process_started =
    let open Internal_event.Simple in
    declare_1
      ~section
      ~name:(Format.sprintf "%s_started" component_base_name)
      ~msg:(Format.sprintf "%s was started on pid {pid}" component_name)
      ~level:Notice
      ("pid", Data_encoding.int31)

  let process_exited_abnormally =
    let open Unix in
    let exit_status_encoding =
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
    let open Internal_event.Simple in
    declare_2
      ~section
      ~level:Error
      ~name:(Format.sprintf "%s_exited_status" component_base_name)
      ~msg:(Format.sprintf "%s (pid {pid}) {status_msg}" component_name)
      ("pid", Data_encoding.int31)
      ~pp2:(fun fmt status ->
        match status with
        | WEXITED i ->
            Format.fprintf fmt "terminated abnormally with exit code %i" i
        | WSIGNALED i ->
            Format.fprintf
              fmt
              "was killed by signal %s"
              (Lwt_exit.signal_name i)
        | WSTOPPED i ->
            Format.fprintf
              fmt
              "was stopped by signal %s"
              (Lwt_exit.signal_name i))
      ("status_msg", exit_status_encoding)

  let cannot_start_process =
    let open Internal_event.Simple in
    declare_1
      ~section
      ~name:(Format.sprintf "cannot_start_%s" component_base_name)
      ~level:Error
      ~msg:(Format.sprintf "cannot start %s: {trace}" component_name)
      ("trace", Data_encoding.string)

  let waiting_for_process_restart =
    let open Internal_event.Simple in
    declare_1
      ~section
      ~name:(Format.sprintf "waiting_for_%s_restart" component_base_name)
      ~level:Error
      ~msg:(Format.sprintf "restarting %s in {sleep} seconds" component_name)
      ("sleep", Data_encoding.float)
end

(* State of the worker. *)
type 'a t = {
  mutable server : Lwt_process.process_none option;
  (* Promise that aims to be resolved as soon as the server is
     shutting down. *)
  stop : (int * Unix.process_status) Lwt.t;
  (* Resolver that will wakeup the above stop promise. *)
  stopper : (int * Unix.process_status) Lwt.u;
  (* The parameters that will be passed to the process after the
     handshake.*)
  parameters : 'a;
  (* The parameters encoding associated to the above field. *)
  parameters_encoding : 'a Data_encoding.t;
}

let create ~parameters ~parameters_encoding =
  let stop, stopper = Lwt.wait () in
  {server = None; stop; stopper; parameters; parameters_encoding}

(** [get_init_socket_path ~socket_dir ?socket_prefix ~pid ()]
    generates the socket path in which the socket will be created. The
    socket will be named from the [?socket_prefix] (a random filename
    is generated if none) and the [pid] and will be located in
    [socket_dir]. *)
let get_init_socket_path ~socket_dir ?socket_prefix ~pid () =
  let socket_prefix =
    match socket_prefix with
    | Some v -> v
    | None -> Filename.(temp_file ~temp_dir:"" "" "")
  in
  let filename = Format.sprintf "%s-%d" socket_prefix pid in
  Filename.concat socket_dir filename

module Daemon (Event : EVENTS) = struct
  let shutdown t =
    let open Lwt_syntax in
    match t.server with
    | None -> return_unit
    | Some process ->
        let* () = Event.(emit shutting_down_process) () in
        process#terminate ;
        return_unit

  let stop t =
    Lwt.wakeup t.stopper (0, Lwt_unix.WSTOPPED 0) ;
    shutdown t

  let run_process t ~binary_path ~arguments () =
    let open Lwt_result_syntax in
    let process =
      Lwt_process.open_process_none
        ~stdout:`Keep
        ~stderr:`Keep
        (binary_path, arguments)
    in
    let*! () = Event.(emit process_started) process#pid in
    t.server <- Some process ;
    return t

  let run_process_with_sockets t ~process_name ?socket_prefix ?executable_name
      ~handshake () =
    let open Lwt_result_syntax in
    let socket_dir = Socket.get_temporary_socket_dir () in
    let socket_dir_arg = ["--socket-dir"; socket_dir] in
    let args = process_name :: socket_dir_arg in
    let executable_name =
      Option.value executable_name ~default:Sys.executable_name
    in
    let process =
      Lwt_process.open_process_none
        ~stdout:(`FD_copy Unix.stdout)
        ~stderr:(`FD_copy Unix.stderr)
        (executable_name, Array.of_list args)
    in
    let pid = process#pid in
    let init_socket_path =
      get_init_socket_path ~socket_dir ?socket_prefix ~pid ()
    in
    let* init_socket_fd =
      let* fds = Socket.bind (Unix init_socket_path) in
      match fds with
      | [fd] ->
          let*! init_socket_fd, _ = Lwt_unix.accept ~cloexec:true fd in
          let*! () = Lwt_unix.close fd in
          return init_socket_fd
      | _ ->
          (* This assertions holds as long as
             Tezos_base_unix.Socket.bind returns a single list element
             when binding Unix sockets. *)
          assert false
    in
    let* () = Socket.handshake init_socket_fd handshake in
    let* () = Socket.send init_socket_fd t.parameters_encoding t.parameters in
    let* () = Socket.recv init_socket_fd Data_encoding.unit in
    let*! () = Lwt_unix.close init_socket_fd in
    let*! () = Event.(emit process_started) pid in
    t.server <- Some process ;
    return t

  (* [run_with_backoff ~backoff f] evaluates [f]. If [f] fails, the
     error is caught, printed as an error event, and [f] is re-evaluated
     after a [backoff] delay. The delay increases at each failing
     try. *)
  let rec run_with_backoff ~backoff ~f =
    let open Lwt_result_syntax in
    let timestamp, sleep = backoff in
    let now = Time.System.now () in
    let diff = Ptime.diff now timestamp in
    if Ptime.Span.to_float_s diff > sleep then
      protect
        (fun () -> f ())
        ~on_error:(function
          | errs ->
          let*! () =
            Event.(
              emit
                cannot_start_process
                (Format.asprintf "%a" pp_print_trace errs))
          in
          run_with_backoff ~backoff:(Time.System.now (), sleep *. 1.2) ~f)
    else
      let*! () = Event.(emit waiting_for_process_restart sleep) in
      let*! () = Lwt_unix.sleep sleep in
      run_with_backoff ~backoff:(timestamp, sleep) ~f

  (* [watch_dog ~start_new_server] make sure that the RPC process is restarted as soon as it
     dies. *)
  let watch_dog ~start_new_server =
    let open Lwt_result_syntax in
    let initial_backoff = (Time.System.epoch, 0.5) in
    let rec loop t =
      match t.server with
      | None ->
          let* new_server =
            run_with_backoff ~backoff:initial_backoff ~f:start_new_server
          in
          loop new_server
      | Some process -> (
          let wait_pid_t =
            let*! _, status = Lwt_unix.waitpid [] process#pid in
            (* Sleep is necessary here to avoid waitpid to be faster than
               the Lwt_exit stack. It avoids the clean_up_starts to be
               pending while the node is properly shutting down. *)
            let*! () = Lwt_unix.sleep 1. in
            Lwt.return (`Wait_pid status)
          in
          let stop_t =
            let*! _ = t.stop in
            Lwt.return `Stopped
          in
          let*! res = Lwt.choose [wait_pid_t; stop_t] in
          match res with
          | `Stopped -> return_unit
          | `Wait_pid _ when not (Lwt.is_sleeping Lwt_exit.clean_up_starts) ->
              return_unit
          | `Wait_pid status ->
              t.server <- None ;
              let*! () =
                Event.(emit process_exited_abnormally) (process#pid, status)
              in
              let* new_server =
                run_with_backoff ~backoff:initial_backoff ~f:start_new_server
              in
              loop new_server)
    in
    loop
end
