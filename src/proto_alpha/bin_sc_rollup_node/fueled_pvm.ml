(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
module Inbox = Sc_rollup.Inbox
open Protocol
open Alpha_context

module type S = sig
  type state

  type fuel

  type eval_result = {state : state; remaining_fuel : fuel; num_ticks : Z.t}

  (** [eval_block_inbox ~fuel node_ctxt block_hash state] evaluates the
      [messages] for the inbox of block [block_hash] in the given [state] of the
      PVM and returns the evaluation results containing the new state, the
      number of messages, the inbox level and the remaining fuel. *)
  val eval_block_inbox :
    fuel:fuel ->
    _ Node_context.t ->
    Tezos_crypto.Block_hash.t ->
    state ->
    (state * Z.t * Raw_level.t * fuel) Node_context.delayed_write tzresult Lwt.t

  (** [eval_messages ~fuel node_ctxt state inbox_level messages] evaluates the
      [messages] for inbox level [inbox_level] in the given [state] of the PVM
      and returns the evaluation results containing the new state, the remaining
      fuel, and the number of ticks for the evaluation
      of this message. *)
  val eval_messages :
    fuel:fuel ->
    _ Node_context.t ->
    state ->
    Raw_level.t ->
    Sc_rollup.Inbox_message.t list ->
    eval_result Node_context.delayed_write tzresult Lwt.t
end

module Make (PVM : Pvm.S) = struct
  module Make_fueled (F : Fuel.S) :
    S with type state = PVM.state and type fuel = F.t = struct
    type state = PVM.state

    type fuel = F.t

    type eval_result = {state : state; remaining_fuel : fuel; num_ticks : Z.t}

    let continue_with_fuel consumption initial_fuel state f =
      let open Delayed_write_monad.Lwt_result_syntax in
      match F.consume consumption initial_fuel with
      | None -> return (state, 0L)
      | Some fuel_left -> f fuel_left state

    exception Error_wrapper of tztrace

    (** [eval_until_input node_ctxt level message_index ~fuel start_tick
        failing_ticks state] advances a PVM [state] until it wants more inputs or
        there are no more [fuel] (if [Some fuel] is specified). The evaluation is
        running under the processing of some [message_index] at a given [level]
        and this is the [start_tick] of this message processing. If some
        [failing_ticks] are planned by the loser mode, they will be made. *)
    let eval_until_input node_ctxt level message_index ~fuel start_tick
        failing_ticks state =
      let open Lwt_result_syntax in
      let open Delayed_write_monad.Lwt_result_syntax in
      let metadata = Node_context.metadata node_ctxt in
      let dal_attestation_lag =
        node_ctxt.protocol_constants.parametric.dal.attestation_lag
      in
      let module Builtins = struct
        let reveal_preimage hash =
          let hash =
            (* The 32-byte payload represents the encoded [Reveal_hash.t]. We must
               decode it properly, instead of converting it byte-for-byte. *)
            Tezos_webassembly_interpreter.Reveal.reveal_hash_to_string hash
            |> Data_encoding.Binary.of_string_exn Sc_rollup.Reveal_hash.encoding
          in
          let*! data = Reveals.get ~data_dir ~pvm_name:PVM.name ~hash in
          match data with
          | Error error ->
              (* The [Error_wrapper] must be caught upstream and converted into a
                 tzresult. *)
              Lwt.fail (Error_wrapper error)
          | Ok data -> Lwt.return data

        let reveal_metadata () =
          Lwt.return
            (Data_encoding.Binary.to_string_exn
               Sc_rollup.Metadata.encoding
               metadata)
      end in
      let builtins = (module Builtins : Tezos_scoru_wasm.Builtins.S) in
      let eval_tick fuel tick failing_ticks state =
        let max_steps = F.max_ticks fuel in
        let normal_eval state =
          Lwt.catch
            (fun () ->
              let*! state, executed_ticks =
                PVM.eval_many ~builtins ~max_steps state
              in
              return (state, executed_ticks, failing_ticks))
            (function
              | Error_wrapper error -> Lwt.return (Error error)
              | exn -> raise exn)
        in
        let failure_insertion_eval state failing_ticks' =
          let*! () =
            Interpreter_event.intended_failure
              ~level
              ~message_index
              ~message_tick:tick
              ~internal:true
          in
          let*! state = PVM.Internal_for_tests.insert_failure state in
          return (state, 1L, failing_ticks')
        in
        match failing_ticks with
        | xtick :: failing_ticks' when xtick = tick ->
            failure_insertion_eval state failing_ticks'
        | _ -> normal_eval state
      in
      let rec go (fuel : fuel) current_tick failing_ticks state =
        let*! input_request = PVM.is_input_state state in
        if F.is_empty fuel then return (state, fuel, current_tick, failing_ticks)
        else
          match input_request with
          | No_input_required ->
              let>* next_state, executed_ticks, failing_ticks =
                eval_tick fuel current_tick failing_ticks state
              in
              go
                fuel
                (Int64.add current_tick executed_ticks)
                failing_ticks
                next_state
          | Needs_reveal (Reveal_raw_data hash) -> (
              let* data =
                Reveals.get
                  ~data_dir:node_ctxt.data_dir
                  ~pvm_name:PVM.name
                  ~hash
              in
              let*! next_state = PVM.set_input (Reveal (Raw_data data)) state in
              match F.consume F.one_tick_consumption fuel with
              | None -> return (state, fuel, current_tick, failing_ticks)
              | Some fuel ->
                  go fuel (Int64.succ current_tick) failing_ticks next_state)
          | Needs_reveal Reveal_metadata -> (
              let*! next_state =
                PVM.set_input (Reveal (Metadata metadata)) state
              in
              match F.consume F.one_tick_consumption fuel with
              | None -> return (state, fuel, current_tick, failing_ticks)
              | Some fuel ->
                  go fuel (Int64.succ current_tick) failing_ticks next_state)
          | Needs_reveal (Request_dal_page page_id) -> (
              let>* content_opt =
                Dal_pages_request.page_content
                  ~dal_attestation_lag
                  node_ctxt
                  page_id
              in
              let*! next_state =
                PVM.set_input (Reveal (Dal_page content_opt)) state
              in
              match F.consume F.one_tick_consumption fuel with
              | None -> return (state, fuel, current_tick, failing_ticks)
              | Some fuel ->
                  go fuel (Int64.succ current_tick) failing_ticks next_state)
          | Initial | First_after _ ->
              return (state, fuel, current_tick, failing_ticks)
      in
      go fuel start_tick failing_ticks state

    (** [mutate input] corrupts the payload of [input] for testing purposes. *)
    let mutate input =
      let payload = Sc_rollup.Inbox_message.unsafe_of_string "0xC4C4" in
      {input with Sc_rollup.payload}

    (** [feed_input node_ctxt level message_index ~fuel ~failing_ticks state
        input] feeds [input] (that has a given [message_index] in inbox
        of [level]) to the PVM in order to advance [state] to the next
        step that requires an input. This function is controlled by
        some [fuel] and may introduce intended failures at some given
        [failing_ticks]. *)
    let feed_input node_ctxt level message_index ~fuel ~failing_ticks state
        input =
      let open Lwt_result_syntax in
      let open Delayed_write_monad.Lwt_result_syntax in
      let>* state, fuel, tick, failing_ticks =
        eval_until_input
          node_ctxt
          level
          message_index
          ~fuel
          0L
          failing_ticks
          state
      in
      let consumption = F.of_ticks tick in
      continue_with_fuel consumption fuel state @@ fun fuel state ->
      let>* input, failing_ticks =
        match failing_ticks with
        | xtick :: failing_ticks' ->
            if xtick = tick then
              let*! () =
                Interpreter_event.intended_failure
                  ~level
                  ~message_index
                  ~message_tick:tick
                  ~internal:false
              in
              return (mutate input, failing_ticks')
            else return (input, failing_ticks)
        | _ -> return (input, failing_ticks)
      in
      let*! state = PVM.set_input (Inbox_message input) state in
      let>* state, _fuel, tick, _failing_ticks =
        eval_until_input
          node_ctxt
          level
          message_index
          ~fuel
          tick
          failing_ticks
          state
      in
      return (state, tick)

    let eval_messages ~fuel node_ctxt state inbox_level messages =
      let open Lwt_result_syntax in
      let open Delayed_write_monad.Lwt_result_syntax in
      let level = Raw_level.to_int32 inbox_level |> Int32.to_int in
      (* Iterate the PVM state with all the messages. *)
      list_fold_left_i_es
        (fun message_counter (state, fuel) message ->
          let*? payload =
            Sc_rollup.Inbox_message.(
              message |> serialize |> Environment.wrap_tzresult)
          in
          let input =
            Sc_rollup.
              {inbox_level; message_counter = Z.of_int message_counter; payload}
          in
          let failing_ticks =
            Loser_mode.is_failure
              node_ctxt.Node_context.loser_mode
              ~level
              ~message_index:message_counter
          in
          let>* state, executed_ticks =
            feed_input
              node_ctxt
              level
              message_counter
              ~fuel
              ~failing_ticks
              state
              input
          in
          return (state, F.of_ticks executed_ticks))
        (state, fuel)
        messages

    let eval_block_inbox ~fuel (Node_context.{store; _} as node_ctxt) hash
        (state : state) :
        (state * Z.t * Raw_level.t * fuel) Node_context.delayed_write tzresult
        Lwt.t =
      let open Lwt_result_syntax in
      let open Delayed_write_monad.Lwt_result_syntax in
      (* Obtain inbox and its messages for this block. *)
      let*! inbox = Store.Inboxes.find store hash in
      match inbox with
      | None ->
          (* A level with no messages for use. Skip it. *)
          let* level = State.level_of_hash store hash in
          return (state, Z.zero, Raw_level.of_int32_exn level, fuel)
      | Some inbox ->
          let inbox_level = Inbox.inbox_level inbox in
          let*! messages = Store.Messages.get store hash in
          (* TODO: #2717
             The length of messages here can potentially overflow the [int] returned from [List.length].
          *)
          let num_messages = List.length messages |> Z.of_int in
          (* Evaluate all the messages for this level. *)
          let>* state, fuel =
            eval_messages ~fuel node_ctxt state inbox_level messages
          in
          return (state, num_messages, inbox_level, fuel)

    let eval_messages ~fuel node_ctxt state inbox_level messages =
      let open Lwt_result_syntax in
      let open Delayed_write_monad.Lwt_result_syntax in
      let*! initial_tick = PVM.get_tick state in
      let>* state, remaining_fuel =
        eval_messages ~fuel node_ctxt state inbox_level messages
      in
      let*! final_tick = PVM.get_tick state in
      let num_ticks = Sc_rollup.Tick.distance initial_tick final_tick in
      return {state; remaining_fuel; num_ticks}
  end

  module Free = Make_fueled (Fuel.Free)
  module Accounted = Make_fueled (Fuel.Accounted)
end
