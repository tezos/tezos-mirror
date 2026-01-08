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
open Pvm_plugin_sig

module Make_fueled (F : Fuel.S) : FUELED_PVM with type fuel = F.t = struct
  type fuel = F.t

  type 'a mut_pvm_impl = (module Pvm_sig.MUTABLE_STATE_S with type t = 'a)

  let get_reveal ~pre_images_endpoint ~data_dir ~pvm_kind reveal_map hash =
    let open Lwt_result_syntax in
    let found_in_map =
      match reveal_map with
      | None -> None
      | Some map ->
          Utils.Reveal_hash_map.find_opt (Reveals.proto_hash_to_bytes hash) map
    in
    match found_in_map with
    | Some data -> return data
    | None -> Reveals.get ~pre_images_endpoint ~data_dir ~pvm_kind hash

  type eval_completion =
    | Aborted of {fuel : fuel; current_tick : int64}
    | Completed of {
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
  let eval_until_input (type mut_state)
      ((module PVM_mut_state) : mut_state mut_pvm_impl)
      (node_ctxt : _ Node_context.t) reveal_map level message_index ~fuel
      start_tick failing_ticks (state : mut_state) =
    let open Lwt_result_syntax in
    let* constants =
      Protocol_plugins.get_constants_of_level node_ctxt (Int32.of_int level)
    in
    let is_reveal_enabled =
      match constants.sc_rollup.reveal_activation_level with
      | None -> fun ~current_block_level:_ _ -> true
      | Some reveal_activation_level ->
          reveal_activation_level
          |> Sc_rollup_proto_types.Constants.reveal_activation_level_of_octez
          |> Protocol.Alpha_context.Sc_rollup.is_reveal_enabled_predicate
    in
    let metadata = metadata node_ctxt in
    let dal_attested_slots_validity_lag =
      match constants.sc_rollup.reveal_activation_level with
      | Some reveal_activation_level when constants.dal.feature_enable ->
          Int32.to_int reveal_activation_level.dal_attested_slots_validity_lag
      | _ -> max_int
    in
    let* dal_activation_level =
      if constants.dal.feature_enable then
        match constants.sc_rollup.reveal_activation_level with
        | None -> return_none
        | Some reveal_activation_level ->
            let*? level =
              Raw_level.of_int32 reveal_activation_level.dal_parameters
              |> Environment.wrap_tzresult
            in
            return_some level
      else return_none
    in
    let dal_parameters =
      Sc_rollup.Dal_parameters.
        {
          number_of_slots = Int64.of_int constants.dal.number_of_slots;
          attestation_lag = Int64.of_int constants.dal.attestation_lag;
          slot_size = Int64.of_int constants.dal.cryptobox_parameters.slot_size;
          page_size = Int64.of_int constants.dal.cryptobox_parameters.page_size;
        }
    in
    let reveal_builtins request =
      match Sc_rollup.Wasm_2_0_0PVM.decode_reveal request with
      | Reveal_raw_data hash
        when Sc_rollup_reveal_hash.(equal hash well_known_reveal_hash) ->
          Lwt.return Sc_rollup_reveal_hash.well_known_reveal_preimage
      | Reveal_raw_data hash -> (
          let*! data =
            get_reveal
              ~pre_images_endpoint:node_ctxt.config.pre_images_endpoint
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
      | (Request_dal_page _ | Request_adal_page _) as xdal_request -> (
          let ( dal_page,
                attestation_threshold_percent,
                restricted_commitments_publishers ) =
            match xdal_request with
            | Request_dal_page dal_page -> (dal_page, None, None)
            | Request_adal_page
                {
                  page_id;
                  attestation_threshold_percent;
                  restricted_commitments_publishers;
                } ->
                ( page_id,
                  Some attestation_threshold_percent,
                  restricted_commitments_publishers )
            | _ ->
                (* This case is not reachable because we know that [xdal_request]
                   is either [Request_dal_page] or [Request_adal_page] *)
                assert false
          in
          let*! content =
            Dal_pages_request.page_content
              constants.dal
              ~dal_activation_level
              ~dal_attested_slots_validity_lag
              ~inbox_level:(Int32.of_int level)
              node_ctxt
              ~attestation_threshold_percent
              ~restricted_commitments_publishers
              dal_page
          in
          match content with
          | Error error ->
              (* The [Error_wrapper] must be caught upstream and converted into
                 a tzresult. *)
              (* This happens when, for example, the kernel requests a page from a future level. *)
              Lwt.fail (Error_wrapper error)
          | Ok None ->
              (* The page was not confirmed by L1.
                 We return empty string in this case, as done in the slow executon. *)
              Lwt.return ""
          | Ok (Some b) -> Lwt.return (Bytes.to_string b))
      | Reveal_dal_parameters ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/6562
             Consider supporting revealing of historical DAL parameters. *)
          let dal_parameters =
            match
              Loser_mode.is_invalid_dal_parameters node_ctxt.config.loser_mode
            with
            | None -> dal_parameters
            | Some {number_of_slots; attestation_lag; slot_size; page_size} ->
                Sc_rollup.Dal_parameters.
                  {number_of_slots; attestation_lag; slot_size; page_size}
          in
          Lwt.return
            (Data_encoding.Binary.to_string_exn
               Sc_rollup.Dal_parameters.encoding
               dal_parameters)
    in

    let eval_tick_consume_fuel fuel failing_ticks state =
      let normal_eval state fuel =
        let max_steps = F.max_ticks fuel in
        Lwt.catch
          (fun () ->
            (* This call to the PVM implementation of [eval_many] should never return running more
               ticks than the given [max_steps] ticks. If it were to happen, this is a security issue,
               and should be reported as such. *)
            let*! executed_ticks =
              PVM_mut_state.eval_many
                ~check_invalid_kernel:
                  (not node_ctxt.config.unsafe_disable_wasm_kernel_checks)
                ~reveal_builtins
                ~write_debug:(Printer node_ctxt.kernel_debug_logger)
                ~max_steps
                ~is_reveal_enabled
                state
            in
            let* remaining_fuel =
              match F.consume (F.of_ticks executed_ticks) fuel with
              | None ->
                  tzfail
                  @@ Rollup_node_errors.PVM_eval_too_many_ticks
                       {max_given = max_steps; executed = executed_ticks}
              | Some remaining_fuel -> Lwt_result.return remaining_fuel
            in
            return (executed_ticks, failing_ticks, remaining_fuel))
          (function
            | Error_wrapper error -> Lwt.return (Error error)
            | exn -> Lwt.reraise exn)
      in
      let normal_eval_wrapper state fuel one_step_eval =
        let* xticks, fticks, remaining_fuel = normal_eval state fuel in
        return (xticks, fticks, remaining_fuel, one_step_eval)
      in
      let failure_insertion_eval state tick =
        let*! () =
          Interpreter_event.intended_failure
            ~level
            ~message_index
            ~message_tick:tick
            ~internal:true
        in
        PVM_mut_state.Internal_for_tests.insert_failure state
      in
      match failing_ticks with
      | xtick :: failing_ticks' ->
          let one_step_eval state = failure_insertion_eval state xtick in
          let jump = Int64.(max 0L (pred xtick)) in
          if Compare.Int64.(jump = 0L) then
            (* Insert the failure in the first tick. *)
            return (xtick, failing_ticks', fuel, Some one_step_eval)
          else
            (* Jump just before the tick where we'll insert a failure.
               Nevertheless, we don't execute more than [fuel] steps. *)
            normal_eval_wrapper state fuel (Some one_step_eval)
      | _ -> normal_eval_wrapper state fuel None
    in
    let abort fuel current_tick = return (Aborted {fuel; current_tick}) in
    let rec go (fuel : fuel) current_tick failing_ticks state =
      let*! input_request =
        PVM_mut_state.is_input_state ~is_reveal_enabled state
      in
      match input_request with
      | No_input_required when F.is_empty fuel -> abort fuel current_tick
      | No_input_required -> (
          let* executed_ticks, failing_ticks, fuel, one_step_advancement =
            eval_tick_consume_fuel fuel failing_ticks state
          in
          let tick_after_eval_consume = Int64.add current_tick executed_ticks in
          match one_step_advancement with
          | None -> go fuel tick_after_eval_consume failing_ticks state
          | Some one_step_eval -> (
              let*! () = one_step_eval state in
              match F.consume F.one_tick_consumption fuel with
              | None -> abort fuel tick_after_eval_consume
              | Some fuel ->
                  go
                    fuel
                    (Int64.succ tick_after_eval_consume)
                    failing_ticks
                    state))
      | Needs_reveal (Reveal_raw_data hash) -> (
          match F.consume F.one_tick_consumption fuel with
          | None -> abort fuel current_tick
          | Some fuel ->
              let* data =
                get_reveal
                  ~pre_images_endpoint:node_ctxt.config.pre_images_endpoint
                  ~data_dir:node_ctxt.data_dir
                  ~pvm_kind:node_ctxt.kind
                  reveal_map
                  hash
              in
              let*! () =
                PVM_mut_state.set_input (Reveal (Raw_data data)) state
              in
              go fuel (Int64.succ current_tick) failing_ticks state)
      | Needs_reveal Reveal_metadata -> (
          match F.consume F.one_tick_consumption fuel with
          | None -> abort fuel current_tick
          | Some fuel ->
              let*! () =
                PVM_mut_state.set_input (Reveal (Metadata metadata)) state
              in
              go fuel (Int64.succ current_tick) failing_ticks state)
      | Needs_reveal
          ((Request_dal_page _ | Request_adal_page _) as xdal_request) -> (
          let ( page_id,
                attestation_threshold_percent,
                restricted_commitments_publishers ) =
            match xdal_request with
            | Request_dal_page page_id -> (page_id, None, None)
            | Request_adal_page
                {
                  page_id;
                  attestation_threshold_percent;
                  restricted_commitments_publishers;
                } ->
                ( page_id,
                  Some attestation_threshold_percent,
                  restricted_commitments_publishers )
            | _ ->
                (* This case is not reachable because we know that [xdal_request]
                   is either [Request_dal_page] or [Request_adal_page] *)
                assert false
          in
          match F.consume F.one_tick_consumption fuel with
          | None -> abort fuel current_tick
          | Some fuel ->
              let* content_opt =
                Dal_pages_request.page_content
                  constants.dal
                  ~inbox_level:(Int32.of_int level)
                  ~dal_activation_level
                  ~dal_attested_slots_validity_lag
                  node_ctxt
                  ~attestation_threshold_percent
                  ~restricted_commitments_publishers
                  page_id
              in
              let*! () =
                PVM_mut_state.set_input (Reveal (Dal_page content_opt)) state
              in
              go fuel (Int64.succ current_tick) failing_ticks state)
      | Needs_reveal Reveal_dal_parameters -> (
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/6562
             Consider supporting revealing of historical DAL parameters. *)
          match F.consume F.one_tick_consumption fuel with
          | None -> abort fuel current_tick
          | Some fuel ->
              let*! () =
                PVM_mut_state.set_input
                  (Reveal (Dal_parameters dal_parameters))
                  state
              in
              go fuel (Int64.succ current_tick) failing_ticks state)
      | Initial | First_after _ ->
          return @@ Completed {fuel; current_tick; failing_ticks}
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
    | Feed_input_aborted of {fuel : fuel; fed_input : bool}
    | Feed_input_completed of fuel

  (** [feed_input node_ctxt reveal_map level message_index ~fuel
        ~failing_ticks state input] feeds [input] (that has a given
        [message_index] in inbox of [level]) to the PVM in order to advance
        [state] to the next step that requires an input. This function is
        controlled by some [fuel] and may introduce intended failures at some
        given [failing_ticks]. *)
  let feed_input (type mut_state)
      ((module PVM_mut_state) : mut_state mut_pvm_impl)
      (node_ctxt : _ Node_context.t) reveal_map level message_index ~fuel
      ~failing_ticks (state : mut_state) input =
    let open Lwt_result_syntax in
    let* res =
      eval_until_input
        (module PVM_mut_state)
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
    | Aborted {fuel; _} -> return (Feed_input_aborted {fuel; fed_input = false})
    | Completed {fuel; current_tick = tick; failing_ticks} -> (
        match F.consume F.one_tick_consumption fuel with
        | None -> return (Feed_input_aborted {fuel; fed_input = false})
        | Some fuel -> (
            let* input, failing_ticks =
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
            let*! () = PVM_mut_state.set_input (Inbox_message input) state in
            let* res =
              eval_until_input
                (module PVM_mut_state)
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
            | Aborted {fuel; _} ->
                return (Feed_input_aborted {fuel; fed_input = true})
            | Completed {fuel; _} -> return (Feed_input_completed fuel)))

  let eval_messages ~reveal_map ~fuel (type mut_state)
      ((module PVM_mut_state) : mut_state mut_pvm_impl) node_ctxt
      ~message_counter_offset (state : mut_state) inbox_level messages =
    let open Lwt_result_syntax in
    let level = Int32.to_int inbox_level in
    (* Iterate the PVM state with all the messages. *)
    let rec feed_messages (state, fuel) message_index = function
      | [] ->
          (* Fed all messages *)
          return (fuel, message_index - message_counter_offset, [])
      | messages when F.is_empty fuel ->
          (* Consumed all fuel *)
          return (fuel, message_index - message_counter_offset, messages)
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
          let* res =
            feed_input
              (module PVM_mut_state)
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
          | Feed_input_completed fuel ->
              feed_messages (state, fuel) (message_index + 1) messages
          | Feed_input_aborted {fuel; fed_input = false} ->
              return
                ( fuel,
                  message_index - message_counter_offset,
                  message :: messages )
          | Feed_input_aborted {fuel; fed_input = true} ->
              return (fuel, message_index + 1 - message_counter_offset, messages)
          )
    in
    (feed_messages [@tailcall]) (state, fuel) message_counter_offset messages

  let eval_block_inbox ~fuel (node_ctxt : _ Node_context.t) (inbox, messages)
      (state : Context.pvmstate) : fuel eval_result tzresult Lwt.t =
    let open Lwt_result_syntax in
    let module PVM = (val Pvm.of_kind node_ctxt.kind) in
    let module PVM_mut_state = PVM.Mutable_state in
    let mut_state =
      PVM.Ctxt_wrapper.from_imm @@ PVM.Ctxt_wrapper.of_node_pvmstate state
    in
    (* Obtain inbox and its messages for this block. *)
    let inbox_level = Octez_smart_rollup.Inbox.inbox_level inbox in
    let*! initial_tick = PVM_mut_state.get_tick mut_state in
    (* Evaluate all the messages for this level. *)
    let* remaining_fuel, num_messages, remaining_messages =
      eval_messages
        ~reveal_map:None
        ~fuel
        (module PVM_mut_state)
        node_ctxt
        ~message_counter_offset:0
        mut_state
        inbox_level
        messages
    in
    let*! final_tick = PVM_mut_state.get_tick mut_state in
    let*! state_hash = PVM_mut_state.state_hash mut_state in
    let num_ticks = Sc_rollup.Tick.distance initial_tick final_tick in
    let state =
      PVM.Ctxt_wrapper.to_node_pvmstate @@ PVM.Ctxt_wrapper.to_imm mut_state
    in
    let eval_state =
      {
        state;
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
    let module PVM = (val Pvm.of_kind node_ctxt.kind) in
    let module PVM_mut_state = PVM.Mutable_state in
    let state =
      PVM.Ctxt_wrapper.from_imm @@ PVM.Ctxt_wrapper.of_node_pvmstate state
    in
    let* remaining_fuel, num_messages, remaining_messages =
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
          let* res =
            eval_until_input
              (module PVM_mut_state)
              node_ctxt
              reveal_map
              level
              message_index
              ~fuel
              0L
              failing_ticks
              state
          in
          let remaining_fuel =
            match res with Aborted {fuel; _} | Completed {fuel; _} -> fuel
          in
          return (remaining_fuel, 0, [])
      | _ ->
          eval_messages
            ~reveal_map
            ~fuel
            (module PVM_mut_state)
            node_ctxt
            ~message_counter_offset
            state
            inbox_level
            messages
    in
    let*! final_tick = PVM_mut_state.get_tick state in
    let final_tick = Sc_rollup.Tick.to_z final_tick in
    let*! state_hash = PVM_mut_state.state_hash state in
    let num_ticks = Z.sub final_tick initial_tick in
    let state =
      PVM.Ctxt_wrapper.to_node_pvmstate @@ PVM.Ctxt_wrapper.to_imm state
    in
    let eval_state =
      {
        state;
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
