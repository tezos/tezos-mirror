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

let get_boot_sector (module Plugin : Protocol_plugin_sig.PARTIAL) block_hash
    (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  match node_ctxt.config.boot_sector_file with
  | None -> Plugin.Layer1_helpers.get_boot_sector block_hash node_ctxt
  | Some boot_sector_file ->
      let*! boot_sector = Lwt_utils_unix.read_file boot_sector_file in
      let*? boot_sector =
        Option.value_e
          ~error:
            [
              Rollup_node_errors.Unparsable_boot_sector {path = boot_sector_file};
            ]
          (Plugin.Pvm.parse_boot_sector node_ctxt.kind boot_sector)
      in
      return boot_sector

let genesis_state (module Plugin : Protocol_plugin_sig.PARTIAL) block_hash
    node_ctxt ctxt =
  let open Lwt_result_syntax in
  let* boot_sector = get_boot_sector (module Plugin) block_hash node_ctxt in
  let*! initial_state = Plugin.Pvm.initial_state node_ctxt.kind in
  let*! genesis_state =
    Plugin.Pvm.install_boot_sector node_ctxt.kind initial_state boot_sector
  in
  let*! ctxt = Context.PVMState.set ctxt genesis_state in
  return (ctxt, genesis_state)

let state_of_head plugin node_ctxt ctxt Layer1.{hash; level} =
  let open Lwt_result_syntax in
  let*! state = Context.PVMState.find ctxt in
  match state with
  | None ->
      let genesis_level = node_ctxt.Node_context.genesis_info.level in
      if level = genesis_level then genesis_state plugin hash node_ctxt ctxt
      else tzfail (Rollup_node_errors.Missing_PVM_state (hash, level))
  | Some state -> return (ctxt, state)

(** [transition_pvm plugin node_ctxt ctxt predecessor head] runs a PVM at the
    previous state from block [predecessor] by consuming as many messages as
    possible from block [head]. *)
let transition_pvm (module Plugin : Protocol_plugin_sig.PARTIAL) node_ctxt ctxt
    predecessor Layer1.{hash = _; _} inbox_messages =
  let open Lwt_result_syntax in
  (* Retrieve the previous PVM state from store. *)
  let* ctxt, predecessor_state =
    state_of_head (module Plugin) node_ctxt ctxt predecessor
  in
  let* eval_result =
    Plugin.Pvm.Fueled.Free.eval_block_inbox
      ~fuel:(Fuel.Free.of_ticks 0L)
      node_ctxt
      inbox_messages
      predecessor_state
  in
  let* {
         state = {state; state_hash; inbox_level; tick; _};
         num_messages;
         num_ticks;
       } =
    Delayed_write_monad.apply node_ctxt eval_result
  in
  let*! ctxt = Context.PVMState.set ctxt state in
  let*! initial_tick = Plugin.Pvm.get_tick node_ctxt.kind predecessor_state in
  (* Produce events. *)
  let*! () =
    Interpreter_event.transitioned_pvm inbox_level state_hash tick num_messages
  in
  return (ctxt, num_messages, Z.to_int64 num_ticks, initial_tick)

(** [process_head plugin node_ctxt ctxt ~predecessor head inbox_and_messages] runs the PVM for the given
    head. *)
let process_head plugin (node_ctxt : _ Node_context.t) ctxt
    ~(predecessor : Layer1.head) (head : Layer1.head) inbox_and_messages =
  let open Lwt_result_syntax in
  let first_inbox_level = node_ctxt.genesis_info.level |> Int32.succ in
  if head.level >= first_inbox_level then
    transition_pvm plugin node_ctxt ctxt predecessor head inbox_and_messages
  else if head.Layer1.level = node_ctxt.genesis_info.level then
    let* ctxt, state = genesis_state plugin head.hash node_ctxt ctxt in
    let*! ctxt = Context.PVMState.set ctxt state in
    return (ctxt, 0, 0L, Z.zero)
  else return (ctxt, 0, 0L, Z.zero)

(** Returns the starting evaluation before the evaluation of the block. It
    contains the PVM state at the end of the execution of the previous block and
    the messages the block ([remaining_messages]). *)
let start_state_of_block plugin node_ctxt (block : Sc_rollup_block.t) =
  let open Lwt_result_syntax in
  let pred_level = Int32.pred block.header.level in
  let* ctxt =
    Node_context.checkout_context node_ctxt block.header.predecessor
  in
  let* _ctxt, state =
    state_of_head
      plugin
      node_ctxt
      ctxt
      Layer1.{hash = block.header.predecessor; level = pred_level}
  in
  let* inbox = Node_context.get_inbox node_ctxt block.header.inbox_hash in
  let inbox_level = Octez_smart_rollup.Inbox.inbox_level inbox in
  let module Plugin = (val plugin) in
  let*! tick = Plugin.Pvm.get_tick node_ctxt.kind state in
  let*! state_hash = Plugin.Pvm.state_hash node_ctxt.kind state in
  let* messages = Messages.get node_ctxt block.header.inbox_witness in
  return
    Pvm_plugin_sig.
      {
        state;
        state_hash;
        inbox_level;
        tick;
        message_counter_offset = 0;
        remaining_fuel = Fuel.Accounted.of_ticks 0L;
        remaining_messages = messages;
      }

(** [run_for_ticks plugin node_ctxt start_state tick_distance] starts the
    evaluation of messages in the [start_state] for at most [tick_distance]. *)
let run_to_tick (module Plugin : Protocol_plugin_sig.PARTIAL) node_ctxt
    start_state tick =
  let open Delayed_write_monad.Lwt_result_syntax in
  let tick_distance =
    Z.sub tick start_state.Pvm_plugin_sig.tick |> Z.to_int64
  in
  let>+ eval_result =
    Plugin.Pvm.Fueled.Accounted.eval_messages
      node_ctxt
      {start_state with remaining_fuel = Fuel.Accounted.of_ticks tick_distance}
  in
  eval_result.state

let state_of_tick_aux plugin node_ctxt ~start_state (event : Sc_rollup_block.t)
    tick =
  let open Lwt_result_syntax in
  let* start_state =
    match start_state with
    | Some start_state
      when start_state.Pvm_plugin_sig.inbox_level = event.header.level ->
        return start_state
    | _ ->
        (* Recompute start state on level change or if we don't have a
           starting state on hand. *)
        start_state_of_block plugin node_ctxt event
  in
  (* TODO: #3384
     We should test that we always have enough blocks to find the tick
     because [state_of_tick] is a critical function. *)
  let* result_state = run_to_tick plugin node_ctxt start_state tick in
  let result_state = Delayed_write_monad.ignore result_state in
  return result_state

(* The cache allows cache intermediate states of the PVM in e.g. dissections. *)
module Tick_state_cache =
  Aches_lwt.Lache.Make
    (Aches.Rache.Transfer
       (Aches.Rache.LRU)
       (struct
         type t = Z.t * Block_hash.t

         let equal (t1, b1) (t2, b2) = Z.equal t1 t2 && Block_hash.(b1 = b2)

         let hash (tick, block) = (Z.hash tick * 13) + Block_hash.hash block
       end))

let tick_state_cache = Tick_state_cache.create 64 (* size of 2 dissections *)

(* Memoized version of [state_of_tick_aux]. *)
let memo_state_of_tick_aux plugin node_ctxt ~start_state
    (event : Sc_rollup_block.t) tick =
  Tick_state_cache.bind_or_put
    tick_state_cache
    (tick, event.header.block_hash)
    (fun (tick, _hash) ->
      state_of_tick_aux plugin node_ctxt ~start_state event tick)
    Lwt.return

(** [state_of_tick plugin node_ctxt ?start_state ~tick level] returns [Some
    end_state] for [tick] if [tick] happened before
    [level]. Otherwise, returns [None].*)
let state_of_tick plugin node_ctxt ?start_state ~tick level =
  let open Lwt_result_syntax in
  let* event = Node_context.block_with_tick node_ctxt ~max_level:level tick in
  match event with
  | None -> return_none
  | Some event ->
      assert (event.header.level <= level) ;
      let* result_state =
        if Node_context.is_loser node_ctxt then
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/5253
             The failures/loser mode does not work properly when restarting
             from intermediate states. *)
          state_of_tick_aux plugin node_ctxt ~start_state:None event tick
        else memo_state_of_tick_aux plugin node_ctxt ~start_state event tick
      in
      return_some result_state
