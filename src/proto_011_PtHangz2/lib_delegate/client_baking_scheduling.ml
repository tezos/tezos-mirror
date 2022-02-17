(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol_client_context

type error += Node_connection_lost

module Events = Delegate_events.Baking_scheduling

let () =
  register_error_kind
    `Temporary
    ~id:"client_baking_scheduling.node_connection_lost"
    ~title:"Node connection lost"
    ~description:"The connection with the node was lost."
    ~pp:(fun fmt () -> Format.fprintf fmt "Lost connection with the node")
    Data_encoding.empty
    (function Node_connection_lost -> Some () | _ -> None)
    (fun () -> Node_connection_lost)

let sleep_until time =
  (* Sleeping is a system op, baking is a protocol op, this is where we convert *)
  let time = Time.System.of_protocol_exn time in
  let delay = Ptime.diff time (Tezos_base.Time.System.now ()) in
  if Ptime.Span.compare delay Ptime.Span.zero < 0 then None
  else Some (Lwt_unix.sleep (Ptime.Span.to_float_s delay))

let rec wait_for_first_event ~name stream =
  Lwt_stream.get stream >>= function
  | None | Some (Error _) ->
      Events.(emit cannot_fetch_event) name >>= fun () ->
      (* NOTE: this is not a tight loop because of Lwt_stream.get *)
      wait_for_first_event ~name stream
  | Some (Ok bi) -> Lwt.return bi

let log_errors_and_continue ~name p =
  p >>= function
  | Ok () -> Lwt.return_unit
  | Error errs -> Events.(emit daemon_error) (name, errs)

let main ~(name : string) ~(cctxt : #Protocol_client_context.full)
    ~(stream : 'event tzresult Lwt_stream.t)
    ~(state_maker : 'event -> 'state tzresult Lwt.t)
    ~(pre_loop :
       #Protocol_client_context.full -> 'state -> 'event -> unit tzresult Lwt.t)
    ~(compute_timeout : 'state -> 'timesup Lwt.t)
    ~(timeout_k :
       #Protocol_client_context.full ->
       'state ->
       'timesup ->
       unit tzresult Lwt.t)
    ~(event_k :
       #Protocol_client_context.full -> 'state -> 'event -> unit tzresult Lwt.t)
    ~finalizer =
  Events.(emit daemon_setup) name >>= fun () ->
  wait_for_first_event ~name stream >>= fun first_event ->
  (* statefulness *)
  let last_get_event = ref None in
  let get_event () =
    match !last_get_event with
    | None ->
        let t = Lwt_stream.get stream in
        last_get_event := Some t ;
        t
    | Some t -> t
  in
  state_maker first_event >>=? fun state ->
  (* main loop *)
  let rec worker_loop () =
    (* event construction *)
    let timeout = compute_timeout state in
    Lwt.choose
      [
        (Lwt_exit.clean_up_starts >|= fun _ -> `Termination);
        (timeout >|= fun timesup -> `Timeout timesup);
        (get_event () >|= fun e -> `Event e);
      ]
    >>= function
    (* event matching *)
    | `Termination -> return_unit
    | `Event (None | Some (Error _)) ->
        (* exit when the node is unavailable *)
        last_get_event := None ;
        Events.(emit daemon_connection_lost) name >>= fun () ->
        fail Node_connection_lost
    | `Event (Some (Ok event)) ->
        (* new event: cancel everything and execute callback *)
        last_get_event := None ;
        (* TODO: pretty-print events (requires passing a pp as argument) *)
        log_errors_and_continue ~name @@ event_k cctxt state event >>= fun () ->
        worker_loop ()
    | `Timeout timesup ->
        (* main event: it's time *)
        Events.(emit daemon_wakeup) name >>= fun () ->
        (* core functionality *)
        log_errors_and_continue ~name @@ timeout_k cctxt state timesup
        >>= fun () -> worker_loop ()
  in
  (* ignition *)
  Events.(emit daemon_start) name >>= fun () ->
  Lwt.finalize
    (fun () ->
      log_errors_and_continue ~name @@ pre_loop cctxt state first_event
      >>= fun () -> worker_loop ())
    (fun () -> finalizer state)
