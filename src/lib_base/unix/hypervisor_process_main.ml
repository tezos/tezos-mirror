(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Returns a temporary path for the socket to be
   spawned. $XDG_RUNTIME_DIR is returned if the environment variable
   is defined. Otherwise, the default temporary directory is used. *)
let get_temporary_socket_dir () =
  match Sys.getenv_opt "XDG_RUNTIME_DIR" with
  | Some xdg_runtime_dir when xdg_runtime_dir <> "" -> xdg_runtime_dir
  | Some _ | None -> Filename.get_temp_dir_name ()

let magic = Bytes.of_string "TEZOS_FORK_HYPERVISOR_0"

module Make_process_parameters (Params : External_process_parameters.S) = struct
  module Params = struct
    type parameters = Internal_event_config.t

    type _ request =
      | Start_hypervisee : (string * int) request
      | Stop_hypervisee : unit request
      | Stop : unit request
      | Reconfigure_event_logging : Internal_event_config.t -> unit request

    let name = Params.name ^ "_hypervisor"

    let request_pp : type a. Format.formatter -> a request -> unit =
     fun fmt -> function
      | Start_hypervisee -> Format.fprintf fmt "start %s hypervisee" Params.name
      | Stop_hypervisee -> Format.fprintf fmt "stop %s hypervisee" Params.name
      | Stop -> Format.fprintf fmt "stop %s" name
      | Reconfigure_event_logging _ ->
          Format.fprintf fmt "reconfigure event logging"

    let internal_events p = p

    let magic = magic

    let parameters_encoding = Internal_event_config.encoding

    type packed_request = Erequest : _ request -> packed_request

    let request_encoding =
      let open Data_encoding in
      union
        [
          case
            ~title:"start_hypervisee"
            (Tag 0)
            unit
            (function Erequest Start_hypervisee -> Some () | _ -> None)
            (fun () -> Erequest Start_hypervisee);
          case
            ~title:"stop_hypervisee"
            (Tag 1)
            unit
            (function Erequest Stop_hypervisee -> Some () | _ -> None)
            (fun () -> Erequest Stop_hypervisee);
          case
            ~title:"stop"
            (Tag 2)
            unit
            (function Erequest Stop -> Some () | _ -> None)
            (fun () -> Erequest Stop);
          case
            (Tag 3)
            ~title:"reconfigure_event_logging"
            Internal_event_config.encoding
            (function
              | Erequest (Reconfigure_event_logging c) -> Some c | _ -> None)
            (fun c -> Erequest (Reconfigure_event_logging c));
        ]

    let result_encoding : type a. a request -> a Data_encoding.t = function
      | Start_hypervisee -> Data_encoding.tup2 string int31
      | Stop_hypervisee -> Data_encoding.unit
      | Stop -> Data_encoding.unit
      | Reconfigure_event_logging _ -> Data_encoding.unit

    let reconfigure_event_logging_request c = Reconfigure_event_logging c

    let terminate_request = Erequest Stop

    let socket_path_prefix = Params.socket_path_prefix

    let socket_path = Params.socket_path

    let command_line_args = Params.command_line_args

    let hypervisor_name = Params.hypervisor_name

    let share_sink = Params.share_sink
  end

  module Processing = struct
    type state = {hypervisee : Lwt_process.process_none option}

    let initial_state _parameters = Lwt_result_syntax.return {hypervisee = None}

    (* This function is a subset of [External_process.start_process]. The key
       difference is that it doesn't communicate with the child process. *)
    let start_hypervisee () =
      let open Lwt_result_syntax in
      let socket_dir = get_temporary_socket_dir () in
      let arg0, args = Params.command_line_args ~socket_dir in
      let args = arg0 :: args in
      let env = Unix.environment () in
      let env =
        if Params.share_sink then env
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
        Lwt_process.open_process_none
          ~env
          (Sys.executable_name, Array.of_list args)
      in
      (* Register clean up callback to ensure that the process
         will be terminated even if the node is brutally stopped. *)
      let _clean_up_callback_id =
        Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
            Lwt.return (Stdlib.at_exit (fun () -> process#terminate)))
      in
      let socket_path = Params.socket_path ~socket_dir ~pid:process#pid in
      return ((socket_path, process#pid), {hypervisee = Some process})

    let stop_hypervisee state =
      let open Lwt_syntax in
      match state.hypervisee with
      | None -> return ((), state)
      | Some process ->
          let* () =
            Lwt.catch
              (fun () ->
                Lwt_unix.with_timeout 5. (fun () ->
                    let* s = process#status in
                    match s with
                    | Unix.WEXITED 0 -> return_unit
                    | _status ->
                        process#terminate ;
                        return_unit))
              (function
                | Lwt_unix.Timeout -> return_unit | err -> Lwt.reraise err)
          in
          return ((), {hypervisee = None})

    let handle_request : type a.
        Params.parameters ->
        state ->
        a Params.request ->
        [ `Continue of
          (a * (Profiler.report option * Profiler.report option) option)
          tzresult
          * state
        | `Stop ]
        Lwt.t =
     fun _parameters state req ->
      let open Lwt_result_syntax in
      let continue res =
        match res with
        | Error _errs -> Lwt.return `Stop
        | Ok (res, state) -> Lwt.return (`Continue (Ok (res, None), state))
      in
      match req with
      | Start_hypervisee ->
          let*! res = start_hypervisee () in
          continue res
      | Stop_hypervisee ->
          let*! res = stop_hypervisee state in
          continue (Ok res)
      | Stop -> Lwt.return `Stop
      | Reconfigure_event_logging config ->
          let*! res =
            let* r = Internal_event_config.reapply config in
            return (r, state)
          in
          continue res
  end
end

module Make (Params : External_process_parameters.S) = struct
  module PP = Make_process_parameters (Params)
  include External_process_main.Make (PP.Params) (PP.Processing)
end
