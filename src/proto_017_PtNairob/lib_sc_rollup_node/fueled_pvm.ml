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
open Context_wrapper.Irmin
module Inbox = Sc_rollup.Inbox
open Pvm_plugin_sig

module Make_fueled (F : Fuel.S) : FUELED_PVM with type fuel = F.t = struct
  type fuel = F.t

  type pvm_state = Irmin_context.tree

  let get_reveal ~data_dir ~pvm_kind reveal_map hash =
    let found_in_map =
      match reveal_map with
      | None -> None
      | Some map ->
          Utils.Reveal_hash_map.find_opt
            (Reveals.proto_hash_to_dac_hash hash)
            map
    in
    match found_in_map with
    | Some data -> return data
    | None -> Reveals.get ~data_dir ~pvm_kind ~hash

  type eval_completion =
    | Aborted of {state : pvm_state; fuel : fuel; current_tick : int64}
    | Completed of {
        state : pvm_state;
        fuel : fuel;
        current_tick : int64;
        failing_ticks : int64 list;
      }

  exception Error_wrapper of tztrace

  let metadata (node_ctxt : _ Node_context.t) =
    let address =
      Sc_rollup_proto_types.Address.of_octez node_ctxt.config.sc_rollup_address
    in
    let origination_level =
      Raw_level.of_int32_exn node_ctxt.genesis_info.level
    in
    Sc_rollup.Metadata.{address; origination_level}

  (** [eval_until_input node_ctxt reveal_map level message_index ~fuel
        start_tick failing_ticks state] advances a PVM [state] until it wants
        more inputs or there are no more [fuel] (if [Some fuel] is
        specified). The evaluation is running under the processing of some
        [message_index] at a given [level] and this is the [start_tick] of this
        message processing. If some [failing_ticks] are planned by the loser
        mode, they will be made. *)
  let eval_until_input (node_ctxt : _ Node_context.t) reveal_map level
      message_index ~fuel start_tick failing_ticks state =
    let open Lwt_result_syntax in
    let open Delayed_write_monad.Lwt_result_syntax in
    let* constants =
      Protocol_plugins.get_constants_of_level node_ctxt (Int32.of_int level)
    in
    let module PVM = (val Pvm.of_kind node_ctxt.kind) in
    let metadata = metadata node_ctxt in
    let dal_attestation_lag = constants.dal.attestation_lag in
    let decode_reveal (Tezos_scoru_wasm.Wasm_pvm_state.Reveal_raw payload) =
      match
        Data_encoding.Binary.of_string_opt
          Sc_rollup_PVM_sig.reveal_encoding
          payload
      with
      | Some reveal -> reveal
      | None ->
          (* If the kernel has tried to submit an incorrect reveal request,
             we don’t stuck the rollup. Instead, we fallback to the
             requesting the [well_known_reveal_hash] preimage *)
          Reveal_raw_data Sc_rollup.Wasm_2_0_0PVM.well_known_reveal_hash
    in
    let reveal_builtins request =
      match decode_reveal request with
      | Reveal_raw_data hash -> (
          let*! data =
            get_reveal
              ~data_dir:node_ctxt.data_dir
              ~pvm_kind:node_ctxt.kind
              reveal_map
              hash
          in
          match data with
          | Error error ->
              (* The [Error_wrapper] must be caught upstream and converted into
                 a tzresult. *)
              Lwt.fail (Error_wrapper error)
          | Ok data -> Lwt.return data)
      | Reveal_metadata ->
          Lwt.return
            (Data_encoding.Binary.to_string_exn
               Sc_rollup.Metadata.encoding
               metadata)
      | Request_dal_page _ ->
          (* DAL in the Fast Execution WASM PVM is not supported for Mumbai. *)
          assert false
    in
    let eval_tick fuel failing_ticks state =
      let max_steps = F.max_ticks fuel in
      let normal_eval ?(max_steps = max_steps) state =
        Lwt.catch
          (fun () ->
            let*! state, executed_ticks =
              PVM.eval_many
                ~reveal_builtins
                ~write_debug:(Printer node_ctxt.kernel_debug_logger)
                ~max_steps
                state
            in
            return (state, executed_ticks, failing_ticks))
          (function
            | Error_wrapper error -> Lwt.return (Error error)
            | exn -> Lwt.reraise exn)
      in
      let failure_insertion_eval state tick failing_ticks' =
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
      | xtick :: failing_ticks' ->
          let jump = Int64.(max 0L (pred xtick)) in
          if Compare.Int64.(jump = 0L) then
            (* Insert the failure in the first tick. *)
            failure_insertion_eval state xtick failing_ticks'
          else
            (* Jump just before the tick where we'll insert a failure.
               Nevertheless, we don't execute more than [max_steps]. *)
            let max_steps = Int64.max 0L max_steps |> Int64.min max_steps in
            let open Delayed_write_monad.Lwt_result_syntax in
            let>* state, executed_ticks, _failing_ticks =
              normal_eval ~max_steps state
            in
            (* Insert the failure. *)
            let>* state, executed_ticks', failing_ticks' =
              failure_insertion_eval state xtick failing_ticks'
            in
            let executed_ticks = Int64.add executed_ticks executed_ticks' in
            return (state, executed_ticks, failing_ticks')
      | _ -> normal_eval state
    in
    let abort state fuel current_tick =
      return (Aborted {state; fuel; current_tick})
    in
    let complete state fuel current_tick failing_ticks =
      return (Completed {state; fuel; current_tick; failing_ticks})
    in
    let rec go (fuel : fuel) current_tick failing_ticks state =
      let*! input_request = PVM.is_input_state state in
      match input_request with
      | No_input_required when F.is_empty fuel -> abort state fuel current_tick
      | No_input_required -> (
          let>* next_state, executed_ticks, failing_ticks =
            eval_tick fuel failing_ticks state
          in
          let fuel_executed = F.of_ticks executed_ticks in
          match F.consume fuel_executed fuel with
          | None -> abort state fuel current_tick
          | Some fuel ->
              go
                fuel
                (Int64.add current_tick executed_ticks)
                failing_ticks
                next_state)
      | Needs_reveal (Reveal_raw_data hash) -> (
          let* data =
            get_reveal
              ~data_dir:node_ctxt.data_dir
              ~pvm_kind:node_ctxt.kind
              reveal_map
              hash
          in
          let*! next_state = PVM.set_input (Reveal (Raw_data data)) state in
          match F.consume F.one_tick_consumption fuel with
          | None -> abort state fuel current_tick
          | Some fuel ->
              go fuel (Int64.succ current_tick) failing_ticks next_state)
      | Needs_reveal Reveal_metadata -> (
          let*! next_state = PVM.set_input (Reveal (Metadata metadata)) state in
          match F.consume F.one_tick_consumption fuel with
          | None -> abort state fuel current_tick
          | Some fuel ->
              go fuel (Int64.succ current_tick) failing_ticks next_state)
      | Needs_reveal (Request_dal_page page_id) -> (
          let* content_opt =
            Dal_pages_request.page_content
              ~dal_attestation_lag
              node_ctxt
              page_id
          in
          let*! next_state =
            PVM.set_input (Reveal (Dal_page content_opt)) state
          in
          match F.consume F.one_tick_consumption fuel with
          | None -> abort state fuel current_tick
          | Some fuel ->
              go fuel (Int64.succ current_tick) failing_ticks next_state)
      | Initial | First_after _ ->
          complete state fuel current_tick failing_ticks
    in
    go fuel start_tick failing_ticks state

  (** [mutate input] corrupts the payload of [input] for testing purposes. *)
  let mutate input =
    let payload =
      Sc_rollup.Inbox_message.unsafe_of_string
        "\001to the cheater we promise pain and misery"
    in
    {input with Sc_rollup.payload}

  type feed_input_completion =
    | Feed_input_aborted of {state : pvm_state; fuel : fuel; fed_input : bool}
    | Feed_input_completed of {state : pvm_state; fuel : fuel}

  (** [feed_input node_ctxt reveal_map level message_index ~fuel
        ~failing_ticks state input] feeds [input] (that has a given
        [message_index] in inbox of [level]) to the PVM in order to advance
        [state] to the next step that requires an input. This function is
        controlled by some [fuel] and may introduce intended failures at some
        given [failing_ticks]. *)
  let feed_input (node_ctxt : _ Node_context.t) reveal_map level message_index
      ~fuel ~failing_ticks state input =
    let open Lwt_result_syntax in
    let open Delayed_write_monad.Lwt_result_syntax in
    let module PVM = (val Pvm.of_kind node_ctxt.kind) in
    let>* res =
      eval_until_input
        node_ctxt
        reveal_map
        level
        message_index
        ~fuel
        0L
        failing_ticks
        state
    in
    match res with
    | Aborted {state; fuel; _} ->
        return (Feed_input_aborted {state; fuel; fed_input = false})
    | Completed {state; fuel; current_tick = tick; failing_ticks} -> (
        let open Delayed_write_monad.Lwt_result_syntax in
        match F.consume F.one_tick_consumption fuel with
        | None -> return (Feed_input_aborted {state; fuel; fed_input = false})
        | Some fuel -> (
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
              | [] -> return (input, failing_ticks)
            in
            let*! state = PVM.set_input (Inbox_message input) state in
            let>* res =
              eval_until_input
                node_ctxt
                reveal_map
                level
                message_index
                ~fuel
                tick
                failing_ticks
                state
            in
            match res with
            | Aborted {state; fuel; _} ->
                return (Feed_input_aborted {state; fuel; fed_input = true})
            | Completed {state; fuel; _} ->
                return (Feed_input_completed {state; fuel})))

  let eval_messages ~reveal_map ~fuel node_ctxt ~message_counter_offset state
      inbox_level messages =
    let open Delayed_write_monad.Lwt_result_syntax in
    let level = Int32.to_int inbox_level in
    (* Iterate the PVM state with all the messages. *)
    let rec feed_messages (state, fuel) message_index = function
      | [] ->
          (* Fed all messages *)
          return (state, fuel, message_index - message_counter_offset, [])
      | messages when F.is_empty fuel ->
          (* Consumed all fuel *)
          return (state, fuel, message_index - message_counter_offset, messages)
      | message :: messages -> (
          let payload = Sc_rollup.Inbox_message.unsafe_of_string message in
          let message_counter = Z.of_int message_index in
          let input =
            Sc_rollup.
              {
                inbox_level = Raw_level.of_int32_exn inbox_level;
                message_counter;
                payload;
              }
          in
          let failing_ticks =
            Loser_mode.is_failure
              node_ctxt.Node_context.config.loser_mode
              ~level
              ~message_index
          in
          let>* res =
            feed_input
              node_ctxt
              reveal_map
              level
              message_index
              ~fuel
              ~failing_ticks
              state
              input
          in
          match res with
          | Feed_input_completed {state; fuel} ->
              feed_messages (state, fuel) (message_index + 1) messages
          | Feed_input_aborted {state; fuel; fed_input = false} ->
              return
                ( state,
                  fuel,
                  message_index - message_counter_offset,
                  message :: messages )
          | Feed_input_aborted {state; fuel; fed_input = true} ->
              return
                ( state,
                  fuel,
                  message_index + 1 - message_counter_offset,
                  messages ))
    in
    (feed_messages [@tailcall]) (state, fuel) message_counter_offset messages

  let eval_block_inbox ~fuel (node_ctxt : _ Node_context.t) (inbox, messages)
      (state : Context.pvmstate) :
      fuel eval_result Node_context.delayed_write tzresult Lwt.t =
    let open Lwt_result_syntax in
    let open Delayed_write_monad.Lwt_result_syntax in
    let module PVM = (val Pvm.of_kind node_ctxt.kind) in
    (* Obtain inbox and its messages for this block. *)
    let inbox_level = Octez_smart_rollup.Inbox.inbox_level inbox in
    let*! initial_tick = PVM.get_tick (of_node_pvmstate state) in
    (* Evaluate all the messages for this level. *)
    let>* state, remaining_fuel, num_messages, remaining_messages =
      eval_messages
        ~reveal_map:None
        ~fuel
        node_ctxt
        ~message_counter_offset:0
        (of_node_pvmstate state)
        inbox_level
        messages
    in
    let*! final_tick = PVM.get_tick state in
    let*! state_hash = PVM.state_hash state in
    let num_ticks = Sc_rollup.Tick.distance initial_tick final_tick in
    let eval_state =
      {
        state = to_node_pvmstate state;
        state_hash = Sc_rollup_proto_types.State_hash.to_octez state_hash;
        tick = Sc_rollup.Tick.to_z final_tick;
        inbox_level;
        message_counter_offset = num_messages;
        remaining_fuel;
        remaining_messages;
      }
    in
    return {state = eval_state; num_ticks; num_messages}

  let eval_messages ?reveal_map (node_ctxt : _ Node_context.t)
      {
        state;
        tick = initial_tick;
        inbox_level;
        message_counter_offset;
        remaining_fuel = fuel;
        remaining_messages = messages;
        _;
      } =
    let open Lwt_result_syntax in
    let open Delayed_write_monad.Lwt_result_syntax in
    let module PVM = (val Pvm.of_kind node_ctxt.kind) in
    let>* state, remaining_fuel, num_messages, remaining_messages =
      match messages with
      | [] ->
          let level = Int32.to_int inbox_level in
          let message_index = message_counter_offset - 1 in
          let failing_ticks =
            Loser_mode.is_failure
              node_ctxt.Node_context.config.loser_mode
              ~level
              ~message_index
          in
          let>* res =
            eval_until_input
              node_ctxt
              reveal_map
              level
              message_index
              ~fuel
              0L
              failing_ticks
              (of_node_pvmstate state)
          in
          let state, remaining_fuel =
            match res with
            | Aborted {state; fuel; _} | Completed {state; fuel; _} ->
                (state, fuel)
          in
          return (state, remaining_fuel, 0, [])
      | _ ->
          eval_messages
            ~reveal_map
            ~fuel
            node_ctxt
            ~message_counter_offset
            (of_node_pvmstate state)
            inbox_level
            messages
    in
    let*! final_tick = PVM.get_tick state in
    let final_tick = Sc_rollup.Tick.to_z final_tick in
    let*! state_hash = PVM.state_hash state in
    let num_ticks = Z.sub final_tick initial_tick in
    let eval_state =
      {
        state = to_node_pvmstate state;
        state_hash = Sc_rollup_proto_types.State_hash.to_octez state_hash;
        tick = final_tick;
        inbox_level;
        message_counter_offset = message_counter_offset + num_messages;
        remaining_fuel;
        remaining_messages;
      }
    in
    return {state = eval_state; num_ticks; num_messages}
end

module Free = Make_fueled (Fuel.Free)
module Accounted = Make_fueled (Fuel.Accounted)
