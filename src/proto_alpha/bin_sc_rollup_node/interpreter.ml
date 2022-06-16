(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Protocol
open Alpha_context
module Inbox = Store.Inbox

module type S = sig
  module PVM : Pvm.S

  (** [process_head node_ctxt store head] interprets the messages associated
      with a [head] from a chain [event]. This requires the inbox to be updated
      beforehand. *)
  val process_head :
    Node_context.t -> Store.t -> Layer1.head -> unit tzresult Lwt.t

  (** [state_of_tick node_ctxt store tick level] returns [Some (state, hash)]
      for a given [tick] if this [tick] happened before
      [level]. Otherwise, returns [None].*)
  val state_of_tick :
    Node_context.t ->
    Store.t ->
    Sc_rollup.Tick.t ->
    Raw_level.t ->
    (PVM.state * PVM.hash) option tzresult Lwt.t
end

module Make (PVM : Pvm.S) : S with module PVM = PVM = struct
  module PVM = PVM

  let consume_fuel = Option.map pred

  let continue_with_fuel fuel state f =
    let open Lwt_syntax in
    match fuel with
    | Some 0 -> return (state, fuel)
    | _ -> f (consume_fuel fuel) state

  (** [eval_until_input ?fuel state] advances a PVM [state] until it
     wants more inputs or there are no more [fuel] (if [Some fuel] is
     specified). *)
  let eval_until_input ?fuel state =
    let open Lwt_syntax in
    let rec go fuel state =
      let* input_request = PVM.is_input_state state in
      continue_with_fuel fuel state @@ fun fuel state ->
      match input_request with
      | No_input_required ->
          let* next_state = PVM.eval state in
          go fuel next_state
      | _ -> return (state, fuel)
    in
    go fuel state

  (** [feed_input state input] feeds [input] to the PVM in order to
     advance [state] to the next step that requires an input. *)
  let feed_input ?fuel state input =
    let open Lwt_syntax in
    let* state, fuel = eval_until_input ?fuel state in
    continue_with_fuel fuel state @@ fun fuel state ->
    let* state = PVM.set_input input state in
    let* state = PVM.eval state in
    let* state, fuel = eval_until_input ?fuel state in
    return (state, fuel)

  let eval_level ?fuel store hash predecessor_state =
    let open Lwt_result_syntax in
    (* Obtain inbox and its messages for this block. *)
    let*! inbox_opt = Store.Inboxes.find store hash in

    (* Iterate the PVM state with all the messages for this level. *)
    match inbox_opt with
    | Some inbox ->
        let inbox_level = Inbox.inbox_level inbox in
        let*! messages = Store.Messages.get store hash in
        let* state, fuel =
          List.fold_left_i_es
            (fun message_counter (state, fuel) message ->
              let*? payload =
                Environment.wrap_tzresult
                  (Sc_rollup.Inbox.Message.serialize message)
              in
              let input =
                Sc_rollup.
                  {
                    inbox_level;
                    message_counter = Z.of_int message_counter;
                    payload;
                  }
              in
              let*! state = feed_input ?fuel state input in
              return state)
            (predecessor_state, fuel)
            messages
        in
        return (state, fuel)
    | None -> return (predecessor_state, fuel)

  let genesis_state node_ctxt store =
    let open Node_context in
    let open Lwt_result_syntax in
    let* boot_sector =
      Plugin.RPC.Sc_rollup.boot_sector
        node_ctxt.cctxt
        (node_ctxt.cctxt#chain, node_ctxt.cctxt#block)
        node_ctxt.rollup_address
    in
    let*! initial_state = PVM.initial_state store in
    let*! genesis_state = PVM.install_boot_sector initial_state boot_sector in
    return genesis_state

  let state_of_hash node_ctxt store hash =
    let open Lwt_result_syntax in
    let*! state = Store.PVMState.find store hash in
    match state with
    | None -> genesis_state node_ctxt store
    | Some state -> return state

  (** [transition_pvm node_ctxt store predecessor_hash hash] runs a PVM at the previous state from block
      [predecessor_hash] by consuming as many messages as possible from block [hash]. *)
  let transition_pvm node_ctxt store predecessor_hash hash =
    let open Lwt_result_syntax in
    (* Retrieve the previous PVM state from store. *)
    let*! predecessor_state = Store.PVMState.find store predecessor_hash in
    let* predecessor_state =
      match predecessor_state with
      | None -> genesis_state node_ctxt store
      | Some predecessor_state -> return predecessor_state
    in

    let*! level = Layer1.level_of_hash store hash in
    let level = Raw_level.of_int32_exn level in
    let* state, _ = eval_level store hash predecessor_state in
    let*! messages = Store.Messages.get store hash in

    (* Write final state to store. *)
    let*! () = Store.PVMState.set store hash state in

    (* Compute extra information about the state. *)
    let*! initial_tick = PVM.get_tick predecessor_state in
    let*! last_tick = PVM.get_tick state in
    (* TODO: #2717
       The number of ticks should not be an arbitrarily-sized integer or
       the difference between two ticks should be made an arbitrarily-sized
       integer too.
    *)
    let num_ticks = Sc_rollup.Tick.distance initial_tick last_tick in
    (* TODO: #2717
       The length of messages here can potentially overflow the [int] returned from [List.length].
    *)
    let num_messages = Z.of_int (List.length messages) in
    let*! () =
      Store.StateInfo.add store hash {num_messages; num_ticks; initial_tick}
    in
    let*! () =
      let open Store.StateHistoryRepr in
      let event =
        {tick = last_tick; predecessor_hash; block_hash = hash; level}
      in
      Store.StateHistory.insert store event
    in
    (* Produce events. *)
    let*! () = Interpreter_event.transitioned_pvm level state num_messages in

    return_unit

  (** [process_head node_ctxt store head] runs the PVM for the given head. *)
  let process_head node_ctxt store (Layer1.Head {hash; _} as head) =
    let open Lwt_result_syntax in
    let*! predecessor_hash = Layer1.predecessor store head in
    transition_pvm node_ctxt store predecessor_hash hash

  (** [run_until_tick tick] *)
  let run_until_tick node_ctxt store hash tick_distance =
    let open Lwt_result_syntax in
    let* state = state_of_hash node_ctxt store hash in
    let* state, fuel = eval_level ~fuel:tick_distance store hash state in
    assert (fuel = Some 0) ;
    return state

  (** [state_of_tick node_ctxt store tick level] returns [Some (state, hash)]
     for a given [tick] if this [tick] happened before
     [level]. Otherwise, returns [None].*)
  let state_of_tick node_ctxt store tick level =
    let open Lwt_result_syntax in
    let* closest_event =
      Store.StateHistory.event_of_largest_tick_before store tick
    in
    match closest_event with
    | None -> return None
    | Some event ->
        if Raw_level.(event.level > level) then return None
        else
          let tick_distance =
            Sc_rollup.Tick.distance tick event.tick |> Z.to_int
          in
          let hash = event.block_hash in
          let* state = run_until_tick node_ctxt store hash tick_distance in
          let*! hash = PVM.state_hash state in
          return (Some (state, hash))
end
