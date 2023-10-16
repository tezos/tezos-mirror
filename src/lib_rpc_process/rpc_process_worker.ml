(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "main"]

  let shutting_down_rpc_process =
    declare_0
      ~section
      ~name:"shutting_down_rpc_process"
      ~msg:"shutting down the RPC process"
      ~level:Notice
      ()

  let rpc_process_started =
    declare_1
      ~section
      ~name:"rpc_process_started"
      ~msg:"RPC process was started on pid {pid}"
      ~level:Notice
      ("pid", Data_encoding.int31)

  let rpc_process_exited_abnormally =
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
    declare_2
      ~section
      ~level:Error
      ~name:"rpc_process_exited_status"
      ~msg:"rpc process (pid {pid}) {status_msg}"
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

  let cannot_start_rpc_process =
    declare_1
      ~section
      ~name:"cannot_start_rpc_process"
      ~level:Error
      ~msg:"cannot start rpc process: {trace}"
      ("trace", Data_encoding.string)

  let waiting_for_rpc_process_restart =
    declare_1
      ~section
      ~name:"waiting_for_rpc_process_restart"
      ~level:Error
      ~msg:"restarting RPC process in {sleep} seconds"
      ("sleep", Data_encoding.float)
end

(* State of the worker. *)
type t = {
  mutable server : Lwt_process.process_none option;
  stop : (int * Unix.process_status) Lwt.t;
  stopper : (int * Unix.process_status) Lwt.u;
  external_process_parameters : Parameters.t;
}

let create ~comm_socket_path (config : Config_file.t) node_version events_config
    =
  let stop, stopper = Lwt.wait () in
  {
    server = None;
    stop;
    stopper;
    external_process_parameters =
      {
        internal_events = events_config;
        config;
        rpc_comm_socket_path = comm_socket_path;
        node_version;
      };
  }

let shutdown t =
  let open Lwt_syntax in
  match t.server with
  | None -> return_unit
  | Some process ->
      let* () = Event.(emit shutting_down_rpc_process) () in
      process#terminate ;
      return_unit

let stop t =
  Lwt.wakeup t.stopper (0, Lwt_unix.WSTOPPED 0) ;
  shutdown t

let run_server t () =
  let open Lwt_result_syntax in
  let socket_dir = Tezos_base_unix.Socket.get_temporary_socket_dir () in
  let socket_dir_arg = ["--socket-dir"; socket_dir] in
  let args = "octez-rpc-process" :: socket_dir_arg in
  let process =
    Lwt_process.open_process_none
      ~stdout:(`FD_copy Unix.stdout)
      ~stderr:(`FD_copy Unix.stderr)
      (Sys.executable_name, Array.of_list args)
  in
  let pid = process#pid in
  let init_socket_path = Main.get_init_socket_path socket_dir pid in
  let* init_socket_fd =
    let* fds = Tezos_base_unix.Socket.bind (Unix init_socket_path) in
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
  let* () = Tezos_base_unix.Socket.handshake init_socket_fd Main.socket_magic in
  let* () =
    Socket.send
      init_socket_fd
      Parameters.parameters_encoding
      t.external_process_parameters
  in
  let* () = Socket.recv init_socket_fd Data_encoding.unit in
  let*! () = Lwt_unix.close init_socket_fd in
  let*! () = Event.(emit rpc_process_started) pid in
  t.server <- Some process ;
  return t

(* Evaluates [f]. If [f] fails, the error is caught, printed as an
   error event, and [f] is re-evaluated after a [backoff] delay. The
   delay increases at each failing try. *)
let rec may_start backoff f =
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
                  cannot_start_rpc_process
                  (Format.asprintf "%a" pp_print_trace errs))
            in
            may_start (Time.System.now (), sleep *. 1.2) f)
  else
    let*! () = Event.(emit waiting_for_rpc_process_restart sleep) in
    let*! () = Lwt_unix.sleep sleep in
    may_start (timestamp, sleep) f

(* Watch_dog make sure that the RPC process is restarted as soon as it
   dies. *)
let watch_dog run_server =
  let open Lwt_result_syntax in
  let rec loop t =
    match t.server with
    | None ->
        let* new_server = may_start (Time.System.epoch, 0.5) run_server in
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
              Event.(emit rpc_process_exited_abnormally) (process#pid, status)
            in
            let* new_server = may_start (Time.System.epoch, 0.5) run_server in
            loop new_server)
  in
  loop

let start server =
  let open Lwt_result_syntax in
  let* new_server = run_server server () in
  let _ = watch_dog (run_server server) new_server in
  return_unit
