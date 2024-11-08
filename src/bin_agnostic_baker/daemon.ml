(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Agnostic_baker_errors

module Baker = struct
  type t = {
    protocol_hash : Protocol_hash.t;
    binary_path : string;
    process : Lwt_process.process_none;
    ccid : Lwt_exit.clean_up_callback_id;
  }

  let baker_path ?(user_path = "./") proto_hash =
    let short_name = Parameters.protocol_short_hash proto_hash in
    Format.sprintf "%soctez-baker-%s" user_path short_name

  let shutdown protocol_hash process =
    let open Lwt_syntax in
    let* () = Agnostic_baker_events.(emit stopping_baker) protocol_hash in
    process#terminate ;
    Lwt.return_unit

  let spawn_baker protocol_hash ~binaries_directory ~baker_args =
    let open Lwt_result_syntax in
    let args_as_string =
      Format.asprintf
        "%a"
        (Format.pp_print_list
           ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        baker_args
    in
    let*! () =
      Agnostic_baker_events.(emit starting_baker) (protocol_hash, args_as_string)
    in
    let binary_path = baker_path ?user_path:binaries_directory protocol_hash in
    let baker_args = binary_path :: baker_args in
    let baker_args = Array.of_list baker_args in
    let process =
      Lwt_process.open_process_none
        ~stdout:`Keep
        ~stderr:`Keep
        (binary_path, baker_args)
    in
    let*! () = Agnostic_baker_events.(emit baker_running) protocol_hash in
    let ccid =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          let*! () = shutdown protocol_hash process in
          Lwt.return_unit)
    in
    return {protocol_hash; binary_path; process; ccid}
end

type state = {
  binaries_directory : string option;
  node_endpoint : string;
  baker_args : string list;
  mutable current_baker : Baker.t option;
}

type t = state

let monitor_heads ~node_addr =
  let open Lwt_result_syntax in
  let uri = Format.sprintf "%s/monitor/heads/main" node_addr in
  let* _, body = Rpc_services.request_uri ~node_addr ~uri in
  let cohttp_stream = Cohttp_lwt.Body.to_stream body in
  let buffer = Buffer.create 2048 in
  let stream, push = Lwt_stream.create () in
  let on_chunk v = push (Some v) and on_close () = push None in
  let rec loop () =
    let*! v = Lwt_stream.get cohttp_stream in
    match v with
    | None ->
        on_close () ;
        Lwt.return_unit
    | Some chunk ->
        Buffer.add_string buffer chunk ;
        let data = Buffer.contents buffer in
        Buffer.reset buffer ;
        on_chunk data ;
        loop ()
  in
  ignore (loop () : unit Lwt.t) ;
  return stream

let hot_swap_baker ~state ~next_protocol_hash =
  let open Lwt_result_syntax in
  let next_proto_status = Parameters.protocol_status next_protocol_hash in
  let*! () =
    Agnostic_baker_events.(emit protocol_encountered)
      (next_proto_status, next_protocol_hash)
  in
  let*! () =
    match state.current_baker with
    | None -> Lwt.return_unit (* Could be assert false*)
    | Some b ->
        let*! () = Baker.shutdown b.protocol_hash b.process in
        let () = Lwt_exit.unregister_clean_up_callback b.ccid in
        state.current_baker <- None ;
        Lwt.return_unit
  in
  let* new_baker =
    Baker.spawn_baker
      next_protocol_hash
      ~binaries_directory:state.binaries_directory
      ~baker_args:state.baker_args
  in
  state.current_baker <- Some new_baker ;
  return_unit

let monitor_voting_periods ~state head_stream =
  let open Lwt_result_syntax in
  let node_addr = state.node_endpoint in
  let rec loop () =
    let*! v = Lwt_stream.get head_stream in
    match v with
    | Some _tick ->
        let* period_kind, remaining =
          Rpc_services.get_current_period ~node_addr
        in
        let*! () =
          Agnostic_baker_events.(emit period_status) (period_kind, remaining)
        in
        let* next_protocol_hash =
          Rpc_services.get_next_protocol_hash ~node_addr
        in
        let current_protocol_hash =
          match state.current_baker with
          | None -> assert false
          | Some v -> v.protocol_hash
        in
        let* () =
          if not (Protocol_hash.equal current_protocol_hash next_protocol_hash)
          then hot_swap_baker ~state ~next_protocol_hash
          else return_unit
        in
        loop ()
    | None -> return_unit
  in
  let* () = loop () in
  return_unit

(* Aims to start the baker associated to the current protocol. If
   the protocol is considered as frozen (not active anymore), and
   there is thus no actual baker binary anymore, the initial phase
   consist in waiting until an active protocol is observed on
   monitored heads. *)
let may_start_initial_baker state =
  let open Lwt_result_syntax in
  let rec may_start ?last_known_proto ~head_stream () =
    let* protocol_hash =
      Rpc_services.get_next_protocol_hash ~node_addr:state.node_endpoint
    in
    let proto_status = Parameters.protocol_status protocol_hash in
    let*! () =
      match last_known_proto with
      | None -> Lwt.return_unit
      | Some h ->
          if not (Protocol_hash.equal h protocol_hash) then
            Agnostic_baker_events.(emit protocol_encountered)
              (proto_status, protocol_hash)
          else Lwt.return_unit
    in
    match proto_status with
    | Active ->
        let* current_baker =
          Baker.spawn_baker
            protocol_hash
            ~binaries_directory:state.binaries_directory
            ~baker_args:state.baker_args
        in
        state.current_baker <- Some current_baker ;
        return_unit
    | Frozen -> (
        let* head_stream =
          match head_stream with
          | Some v -> return v
          | None ->
              let*! () =
                Agnostic_baker_events.(emit protocol_encountered)
                  (proto_status, protocol_hash)
              in
              let*! () =
                Agnostic_baker_events.(emit waiting_for_active_protocol) ()
              in
              monitor_heads ~node_addr:state.node_endpoint
        in
        let*! v = Lwt_stream.get head_stream in
        match v with
        | Some _tick ->
            may_start
              ~last_known_proto:protocol_hash
              ~head_stream:(Some head_stream)
              ()
        | None -> tzfail Lost_node_connection)
  in
  may_start ~head_stream:None ()

let create ~binaries_directory ~node_endpoint ~baker_args =
  {binaries_directory; node_endpoint; baker_args; current_baker = None}

let run state =
  let open Lwt_result_syntax in
  let node_addr = state.node_endpoint in
  let*! () = Agnostic_baker_events.(emit starting_daemon) () in
  let _ccid =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        let*! () = Agnostic_baker_events.(emit stopping_daemon) () in
        Lwt.return_unit)
  in
  let* () = may_start_initial_baker state in
  let* _protocol_proposal = Rpc_services.get_current_proposal ~node_addr in
  let* head_stream = monitor_heads ~node_addr in
  (* Monitoring voting periods through heads monitoring to avoid
     missing UAUs. *)
  let* () = monitor_voting_periods ~state head_stream in
  return_unit
