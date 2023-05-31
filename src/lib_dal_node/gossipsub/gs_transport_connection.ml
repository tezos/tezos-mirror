(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

module Worker = Gs_interface.Worker_instance
open Gs_interface.Worker_instance

module Events = struct
  include Internal_event.Simple

  let section = ["gossipsub"; "transport"; "event"]

  let prefix =
    let prefix = String.concat "_" section in
    fun s -> prefix ^ "-" ^ s

  let no_connection_for_peer =
    declare_1
      ~section
      ~name:(prefix "no_connection_for_peer")
      ~msg:"No running connection found for peer {peer}"
      ~level:Notice
      ~pp1:P2p_peer.Id.pp
      ("peer", P2p_peer.Id.encoding)

  let message_notified_to_app =
    declare_1
      ~section
      ~name:(prefix "message_notified_to_app")
      ~msg:"Successfully notified message id {message_id} to the application"
      ~level:Info
      ~pp1:Worker.GS.Message_id.pp
      ("message_id", Gs_interface.message_id_encoding)

  let app_message_callback_failed =
    declare_2
      ~section
      ~name:(prefix "app_message_callback_failed")
      ~msg:"Callback failed for message id {message_id}. Failure is {failure}"
      ~level:Warning
      ~pp1:Worker.GS.Message_id.pp
      ("message_id", Gs_interface.message_id_encoding)
      ("failure", Data_encoding.string)
end

(** This handler forwards information about connections established by the P2P layer
    to the Gossipsub worker. *)
let new_connections_handler gs_worker p2p_layer peer conn =
  let info = P2p.connection_info p2p_layer conn in
  let outbound = not info.incoming in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5584

     Add the ability to have direct peers. *)
  let direct = false in
  Worker.(New_connection {peer; direct; outbound} |> p2p_input gs_worker)

(** This handler forwards information about P2P disconnections to the Gossipsub
    worker. *)
let disconnections_handler gs_worker peer =
  Worker.(Disconnection {peer} |> p2p_input gs_worker)

(* This function translates a Worker p2p_message to the type of messages sent
   via the P2P layer. The two types don't coincide because of Prune. *)
let wrap_p2p_message =
  let module W = Worker in
  let open Transport_layer_interface in
  function
  | W.Graft {topic} -> Graft {topic}
  | W.Prune _ ->
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5646

         Handle Prune messages in GS/P2P interconnection. *)
      assert false
  | W.IHave {topic; message_ids} -> IHave {topic; message_ids}
  | W.IWant {message_ids} -> IWant {message_ids}
  | W.Subscribe {topic} -> Subscribe {topic}
  | W.Unsubscribe {topic} -> Unsubscribe {topic}
  | W.Message_with_header {message; topic; message_id} ->
      Message_with_header {message; topic; message_id}

(* This function translates a message received via the P2P layer to a Worker
   p2p_message. The two types don't coincide because of Prune. *)
let unwrap_p2p_message =
  let open Worker in
  let module I = Transport_layer_interface in
  function
  | I.Graft {topic} -> Graft {topic}
  | I.Prune _ ->
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5646

         Handle Prune messages in GS/P2P interconnection. *)
      assert false
  | I.IHave {topic; message_ids} -> IHave {topic; message_ids}
  | I.IWant {message_ids} -> IWant {message_ids}
  | I.Subscribe {topic} -> Subscribe {topic}
  | I.Unsubscribe {topic} -> Unsubscribe {topic}
  | I.Message_with_header {message; topic; message_id} ->
      Message_with_header {message; topic; message_id}

(** This handler pops and processes the items put by the worker in the p2p
    output stream. The out messages are sent to the corresponding peers and the
    directives to the P2P layer to connect or disconnect peers are handled. *)
let gs_worker_p2p_output_handler gs_worker p2p_layer =
  let open Lwt_syntax in
  let rec loop output_stream =
    let* p2p_output = Worker.Stream.pop output_stream in
    let* () =
      match p2p_output with
      | Worker.Out_message {to_peer; p2p_message} -> (
          let conn = P2p.find_connection_by_peer_id p2p_layer to_peer in
          match conn with
          | None ->
              (* This could happen when the peer is disconnected or the
                 connection is accepted but not running (authenticated) yet. *)
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/5649

                 Are there weird cases in which there is no connection
                 associated to the peer, but the peer is still registered as
                 connected on the GS side? *)
              Events.(emit no_connection_for_peer to_peer)
          | Some conn ->
              Error_monad.dont_wait
                (fun () ->
                  wrap_p2p_message p2p_message |> P2p.send p2p_layer conn)
                (Format.eprintf
                   "Uncaught error in %s: %a\n%!"
                   __FUNCTION__
                   Error_monad.pp_print_trace)
                (fun exc ->
                  Format.eprintf
                    "Uncaught exception in %s: %s\n%!"
                    __FUNCTION__
                    (Printexc.to_string exc)) ;
              return_unit)
      | Disconnect {peer = _}
      | Connect {px = _; origin = _}
      | Forget {px = _; origin = _}
      | Kick {peer = _} ->
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5647

             Handle Disconnect, Connect and Kick directives from GS *)
          assert false
    in
    loop output_stream
  in
  Worker.p2p_output_stream gs_worker |> loop

(** This handler forwards p2p messages received via Octez p2p to the Gossipsub
    worker. *)
let transport_layer_inputs_handler gs_worker p2p_layer =
  let open Lwt_syntax in
  let rec loop () =
    let* conn, msg = P2p.recv_any p2p_layer in
    let {P2p_connection.Info.peer_id; _} = P2p.connection_info p2p_layer conn in
    Worker.(
      In_message {from_peer = peer_id; p2p_message = unwrap_p2p_message msg}
      |> p2p_input gs_worker) ;
    loop ()
  in
  loop ()

(** This loop pops messages from application output stream and calls the given
    [app_messages_callback] on them. *)
let app_messages_handler gs_worker ~app_messages_callback =
  let open Lwt_syntax in
  let rec loop app_output_stream =
    let* Worker.{message; message_id; topic = _} =
      Worker.Stream.pop app_output_stream
    in
    let* res = app_messages_callback message message_id in
    let* () =
      match res with
      | Ok () -> Events.(emit message_notified_to_app message_id)
      | Error err ->
          Events.(
            emit
              app_message_callback_failed
              (message_id, Format.asprintf "%a" pp_print_trace err))
    in
    loop app_output_stream
  in
  Worker.app_output_stream gs_worker |> loop

let activate gs_worker p2p_layer ~app_messages_callback =
  (* Register a handler to notify new P2P connections to GS. *)
  let () =
    new_connections_handler gs_worker p2p_layer
    |> P2p.on_new_connection p2p_layer
  in
  (* Register a handler to notify P2P disconnections to GS. *)
  let () = disconnections_handler gs_worker |> P2p.on_disconnection p2p_layer in
  Lwt.join
    [
      gs_worker_p2p_output_handler gs_worker p2p_layer;
      transport_layer_inputs_handler gs_worker p2p_layer;
      app_messages_handler gs_worker ~app_messages_callback;
    ]
