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
module Fueled_pvm = Fueled_pvm.Free

type level_position = Start | Middle | End

type info_per_level = {
  predecessor_timestamp : Timestamp.time;
  predecessor : Block_hash.t;
}

type t = {
  ctxt : Context.ro;
  inbox_level : Raw_level.t;
  state : Context.tree;
  reveal_map : string Sc_rollup_reveal_hash.Map.t option;
  nb_messages_inbox : int;
  level_position : level_position;
  info_per_level : info_per_level;
}

let simulate_info_per_level (node_ctxt : [`Read] Node_context.t) predecessor =
  let open Lwt_result_syntax in
  let* block_info = Layer1.fetch_tezos_block node_ctxt.l1_ctxt predecessor in
  let predecessor_timestamp = block_info.header.shell.timestamp in
  return {predecessor_timestamp; predecessor}

let start_simulation node_ctxt ~reveal_map (Layer1.{hash; level} as head) =
  let open Lwt_result_syntax in
  let*? level = Environment.wrap_tzresult @@ Raw_level.of_int32 level in
  let*? () =
    error_unless
      Raw_level.(level >= node_ctxt.Node_context.genesis_info.level)
      (Exn (Failure "Cannot simulate before origination level"))
  in
  let first_inbox_level = Raw_level.succ node_ctxt.genesis_info.level in
  let* ctxt =
    if Raw_level.(level < first_inbox_level) then
      (* This is before we have interpreted the boot sector, so we start
         with an empty context in genesis *)
      return (Context.empty node_ctxt.context)
    else Node_context.checkout_context node_ctxt hash
  in
  let* ctxt, state = Interpreter.state_of_head node_ctxt ctxt head in
  let+ info_per_level = simulate_info_per_level node_ctxt hash in
  let inbox_level = Raw_level.succ level in
  {
    ctxt;
    inbox_level;
    state;
    reveal_map;
    nb_messages_inbox = 0;
    level_position = Start;
    info_per_level;
  }

let simulate_messages_no_checks (node_ctxt : Node_context.ro)
    ({
       ctxt;
       state;
       inbox_level;
       reveal_map;
       nb_messages_inbox;
       level_position = _;
       info_per_level = _;
     } as sim) messages =
  let open Lwt_result_syntax in
  let module PVM = (val node_ctxt.pvm) in
  let*! state_hash = PVM.state_hash state in
  let*! tick = PVM.get_tick state in
  let eval_state =
    Fueled_pvm.
      {
        state;
        state_hash;
        tick;
        inbox_level;
        message_counter_offset = nb_messages_inbox;
        remaining_fuel = Fuel.Free.of_ticks 0L;
        remaining_messages = messages;
      }
  in
  (* Build new state *)
  let* eval_result =
    Fueled_pvm.eval_messages ?reveal_map node_ctxt eval_state
  in
  let Fueled_pvm.{state = {state; _}; num_ticks; num_messages; _} =
    Delayed_write_monad.ignore eval_result
  in
  let*! ctxt = PVM.State.set ctxt state in
  let nb_messages_inbox = nb_messages_inbox + num_messages in
  return ({sim with ctxt; state; nb_messages_inbox}, num_ticks)

let simulate_messages (node_ctxt : Node_context.ro) sim messages =
  let open Lwt_result_syntax in
  (* Build new inbox *)
  let*? () =
    error_when
      (sim.level_position = End)
      (Exn (Failure "Level for simulation is ended"))
  in
  let*? messages =
    let open Result_syntax in
    if sim.level_position = Start then
      let {predecessor_timestamp; predecessor} = sim.info_per_level in
      let open Sc_rollup.Inbox_message in
      let* internals =
        List.map_e
          serialize
          [
            Internal Start_of_level;
            Internal (Info_per_level {predecessor_timestamp; predecessor});
          ]
        |> Environment.wrap_tzresult
      in
      return (internals @ messages)
    else return messages
  in
  let+ sim, num_ticks = simulate_messages_no_checks node_ctxt sim messages in
  ({sim with level_position = Middle}, num_ticks)

let end_simulation node_ctxt sim =
  let open Lwt_result_syntax in
  let*? () =
    error_when
      (sim.level_position = End)
      (Exn (Failure "Level for simulation is ended"))
  in
  let*? eol =
    Sc_rollup.Inbox_message.serialize
      (Sc_rollup.Inbox_message.Internal End_of_level)
    |> Environment.wrap_tzresult
  in
  let+ sim, num_ticks = simulate_messages_no_checks node_ctxt sim [eol] in
  ({sim with level_position = End}, num_ticks)
