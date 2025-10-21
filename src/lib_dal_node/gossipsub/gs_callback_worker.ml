(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2018-2025 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

module Worker = Gs_interface.Worker_instance
open Gs_interface.Worker_instance
module Events = Gs_transport_connection_events

let gossipsub_worker_base_name = "dal_gossipsub"

type (_, _) loop = Loop : (unit, tztrace) loop

module Request : Worker_intf.REQUEST with type ('a, 'b) t = ('a, 'b) loop =
struct
  type ('a, 'b) t = ('a, 'b) loop

  type view = View : ('a, 'b) loop -> view

  let encoding =
    let open Data_encoding in
    conv (fun (View Loop) -> ()) (fun () -> View Loop) unit

  let pp ppf (View Loop) = Format.fprintf ppf "loop"

  let view msg = View msg
end

module Generic_name = struct
  type t = string

  let encoding = Data_encoding.string

  let pp = Format.pp_print_string

  let equal = String.equal
end

(** This worker pops and processes the items put by the gossipsub worker in the
    p2p output stream. The out messages are sent to the corresponding peers and
    the directives to the P2P layer to connect or disconnect peers are
    handled. *)
module P2p_callback_worker = struct
  module P2P_name : Tezos_base.Worker_intf.NAME with type t = string = struct
    include Generic_name

    let base = [gossipsub_worker_base_name; "P2p_worker"]
  end

  type 'a p2p_state = {
    gs_worker : Worker.t;
    p2p_layer :
      ( p2p_message,
        'a,
        Transport_layer_interface.Types.P2P.Metadata.Connection.t )
      P2p.t;
    log_sending_message : p2p_message -> bool;
    output_stream : p2p_output Stream.t;
  }

  module P2P_types = struct
    type state = S : 'a p2p_state -> state

    type parameters =
      | P : {
          gs_worker : Worker.t;
          p2p_layer :
            ( p2p_message,
              'a,
              Transport_layer_interface.Types.P2P.Metadata.Connection.t )
            P2p.t;
        }
          -> parameters
  end

  module P2p_worker =
    Tezos_workers.Worker.MakeSingle (P2P_name) (Request) (P2P_types)

  type p2p_worker = P2p_worker.callback P2p_worker.t

  type self = p2p_worker

  type launch_error = error trace

  let try_connect ?expected_peer_id gs_worker p2p_layer ~trusted point =
    let open Lwt_syntax in
    (* We don't wait for the promise to resolve here, because if the
     advertised peer is not reachable or is not responding, we might block
     until connection timeout is reached (we observed a timeout of 10
     seconds in some case). Blocking here means that processing of other
     messages from p2p_output_stream (including shards propagation) will
     be delayed. *)
    Lwt.dont_wait
      (fun () ->
        (* We don't check [expected_peer_id] anymore because people frequently
         wipe / regenerate their peer identities while keeping the same IP
         addresses. The [expected_peer_id] check, if enabled, will make
         Octez-p2p reject any other connection with a different identity. *)
        ignore expected_peer_id ;
        let* (result : _ P2p.connection tzresult) =
          P2p.connect ~trusted p2p_layer point
        in
        Result.iter_error
          (fun _ -> Worker.set_unreachable_point gs_worker point)
          result ;
        return_unit)
      (fun exn ->
        Format.eprintf
          "Warning: got an exception while trying to connect to %a: %s@."
          Point.pp
          point
          (Printexc.to_string exn))
    |> return

  (* P2P worker main loop *)
  let handle_p2p_output self p2p_output =
    let open Lwt_syntax in
    let (S state) = P2p_worker.state self in
    let* r =
      match p2p_output with
      | Worker.Out_message {to_peer; p2p_message} -> (
          let conn =
            P2p.find_connection_by_peer_id state.p2p_layer to_peer.peer_id
          in
          match conn with
          | None ->
              (* This could happen when the peer is disconnected or the
                 connection is accepted but not running (authenticated) yet. *)
              Events.(emit no_connection_for_peer to_peer.peer_id)
          | Some conn -> (
              let* (res : unit tzresult) =
                let* () =
                  if state.log_sending_message p2p_message then
                    Events.(
                      emit send_p2p_message (to_peer.peer_id, p2p_message))
                  else return_unit
                in
                P2p.send state.p2p_layer conn p2p_message
              in
              match res with
              | Ok () -> return_unit
              | Error err ->
                  Events.(emit send_p2p_message_failed (to_peer.peer_id, err))))
      | Disconnect {peer} ->
          P2p.find_connection_by_peer_id state.p2p_layer peer.peer_id
          |> Option.iter_s
               (P2p.disconnect
                  ~reason:"disconnected by Gossipsub"
                  state.p2p_layer)
      | Connect {peer; origin} ->
          let trusted = origin = Trusted in
          let Types.Peer.{maybe_reachable_point; peer_id} = peer in
          try_connect
            ~trusted
            ~expected_peer_id:peer_id
            state.gs_worker
            state.p2p_layer
            maybe_reachable_point
      | Connect_point {point} ->
          try_connect state.gs_worker state.p2p_layer point ~trusted:false
      | Forget _ -> return_unit
      | Kick {peer} ->
          P2p.pool state.p2p_layer
          |> Option.iter_s (fun pool -> P2p_pool.Peers.ban pool peer.peer_id)
    in
    Lwt_result_syntax.return r

  let on_launch (_self : self) _ (P2P_types.P {gs_worker; p2p_layer}) :
      (P2P_types.state, launch_error) result Lwt.t =
    (* Only log sending of GS control messages  *)
    let log_sending_message = function
      | Message_with_header _ -> false
      | _ -> true
    in
    let output_stream = Worker.p2p_output_stream gs_worker in
    let state = {gs_worker; p2p_layer; log_sending_message; output_stream} in
    Lwt.return_ok (P2P_types.S state)

  let on_request : type res err.
      self -> (res, err) Request.t -> (res, err) result Lwt.t =
   fun self Loop ->
    let open Lwt_result_syntax in
    let (S state) = P2p_worker.state self in
    let*! p2p_output = Worker.Stream.pop state.output_stream in
    handle_p2p_output self p2p_output

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit

  let on_error : type res err.
      self ->
      _ ->
      (res, err) Request.t ->
      err ->
      [`Continue | `Shutdown] tzresult Lwt.t =
   fun _self _ Loop error -> Lwt.return_error error

  let on_completion _ _ _ _ = Lwt.return_unit
end

type t = P2p_callback_worker.p2p_worker

let create gs_worker p2p_layer =
  let table =
    P2p_callback_worker.P2p_worker.create_table
      (Callback
         (fun () ->
           Lwt.return
             (P2p_callback_worker.P2p_worker.Any_request
                (Loop, {P2p_callback_worker.P2p_worker.scope = None}))))
  in
  P2p_callback_worker.P2p_worker.launch
    table
    "P2P_gs_worker"
    (P2p_callback_worker.P2P_types.P {gs_worker; p2p_layer})
    (module P2p_callback_worker)

let shutdown = P2p_callback_worker.P2p_worker.shutdown
