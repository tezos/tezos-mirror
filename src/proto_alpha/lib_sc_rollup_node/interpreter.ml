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

module type S = sig
  module PVM : Pvm.S

  module Accounted_pvm :
    Fueled_pvm.S with module PVM = PVM and type fuel = Fuel.Accounted.t

  module Free_pvm :
    Fueled_pvm.S with module PVM = PVM and type fuel = Fuel.Free.t

  val process_head :
    Node_context.rw ->
    'a Context.t ->
    Layer1.head ->
    Sc_rollup.Inbox.t * Sc_rollup.Inbox_message.t list ->
    ('a Context.t * int * int64 * Sc_rollup.Tick.t) tzresult Lwt.t

  val state_of_tick :
    _ Node_context.t ->
    Sc_rollup.Tick.t ->
    Raw_level.t ->
    (PVM.state * PVM.hash) option tzresult Lwt.t

  val state_of_head :
    'a Node_context.t ->
    'a Context.t ->
    Layer1.head ->
    ('a Context.t * PVM.state) tzresult Lwt.t
end

module Make (PVM : Pvm.S) : S with module PVM = PVM = struct
  module PVM = PVM
  module Fueled_pvm = Fueled_pvm.Make (PVM)
  module Accounted_pvm = Fueled_pvm.Accounted
  module Free_pvm = Fueled_pvm.Free

  (** [get_boot_sector block_hash node_ctxt] fetches the operations in the
      [block_hash] and looks for the bootsector used to originate the rollup
      we're following.
      It must be called with [block_hash.level] = [node_ctxt.genesis_info.level].
  *)
  let get_boot_sector block_hash (node_ctxt : _ Node_context.t) =
    let open Lwt_result_syntax in
    let exception Found_boot_sector of string in
    let* block = Layer1.fetch_tezos_block node_ctxt.cctxt block_hash in
    let missing_boot_sector () =
      failwith
        "Boot sector not found in Tezos block %a"
        Block_hash.pp
        block_hash
    in
    Lwt.catch
      (fun () ->
        let apply (type kind) accu ~source:_
            (operation : kind manager_operation)
            (result : kind Apply_results.successful_manager_operation_result) =
          match (operation, result) with
          | ( Sc_rollup_originate {kind; boot_sector; _},
              Sc_rollup_originate_result {address; _} )
            when node_ctxt.rollup_address = address && node_ctxt.kind = kind ->
              raise (Found_boot_sector boot_sector)
          | _ -> accu
        in
        let apply_internal (type kind) accu ~source:_
            (_operation : kind Apply_internal_results.internal_operation)
            (_result :
              kind Apply_internal_results.successful_internal_operation_result)
            =
          accu
        in
        let*? () =
          Layer1_services.(
            process_applied_manager_operations
              (Ok ())
              block.operations
              {apply; apply_internal})
        in
        missing_boot_sector ())
      (function
        | Found_boot_sector boot_sector -> return boot_sector
        | _ -> missing_boot_sector ())

  let genesis_state block_hash node_ctxt ctxt =
    let open Lwt_result_syntax in
    let* boot_sector = get_boot_sector block_hash node_ctxt in
    let*! initial_state = PVM.initial_state ~empty:(PVM.State.empty ()) in
    let*! genesis_state = PVM.install_boot_sector initial_state boot_sector in
    let*! ctxt = PVM.State.set ctxt genesis_state in
    return (ctxt, genesis_state)

  let state_of_head node_ctxt ctxt Layer1.{hash; level} =
    let open Lwt_result_syntax in
    let genesis_level =
      Raw_level.to_int32 node_ctxt.Node_context.genesis_info.level
    in
    if level = genesis_level then genesis_state hash node_ctxt ctxt
    else
      let*! state = PVM.State.find ctxt in
      match state with
      | None -> tzfail (Sc_rollup_node_errors.Missing_PVM_state (hash, level))
      | Some state -> return (ctxt, state)

  (** [transition_pvm node_ctxt predecessor head] runs a PVM at the
      previous state from block [predecessor] by consuming as many messages
      as possible from block [head]. *)
  let transition_pvm node_ctxt ctxt predecessor Layer1.{hash = _; _}
      inbox_messages =
    let open Lwt_result_syntax in
    (* Retrieve the previous PVM state from store. *)
    let* ctxt, predecessor_state = state_of_head node_ctxt ctxt predecessor in
    let* eval_result =
      Free_pvm.eval_block_inbox
        ~fuel:(Fuel.Free.of_ticks 0L)
        node_ctxt
        inbox_messages
        predecessor_state
    in
    let* state, num_messages, inbox_level, _fuel =
      Delayed_write_monad.apply node_ctxt eval_result
    in
    let*! ctxt = PVM.State.set ctxt state in
    let*! initial_tick = PVM.get_tick predecessor_state in
    let*! last_tick = PVM.get_tick state in
    let num_ticks =
      Sc_rollup.Tick.distance initial_tick last_tick |> Z.to_int64
    in
    (* Produce events. *)
    let*! state_hash = PVM.state_hash state in
    let*! () =
      Interpreter_event.transitioned_pvm
        inbox_level
        state_hash
        last_tick
        num_messages
    in
    return (ctxt, num_messages, num_ticks, initial_tick)

  (** [process_head node_ctxt head] runs the PVM for the given head. *)
  let process_head (node_ctxt : _ Node_context.t) ctxt head inbox_messages =
    let open Lwt_result_syntax in
    let first_inbox_level =
      Raw_level.to_int32 node_ctxt.genesis_info.level |> Int32.succ
    in
    if head.Layer1.level >= first_inbox_level then
      let* predecessor = Node_context.get_predecessor node_ctxt head in
      transition_pvm node_ctxt ctxt predecessor head inbox_messages
    else if head.Layer1.level = Raw_level.to_int32 node_ctxt.genesis_info.level
    then
      let* ctxt, state = genesis_state head.hash node_ctxt ctxt in
      (* Write final state to store. *)
      let*! ctxt = PVM.State.set ctxt state in

      (* let*! context_hash = Context.commit ctxt in *)
      (* let*! () = Store.Contexts.add node_ctxt.store head.hash context_hash in *)
      return (ctxt, 0, 0L, Sc_rollup.Tick.initial)
    else return (ctxt, 0, 0L, Sc_rollup.Tick.initial)

  (** [run_for_ticks node_ctxt block tick_distance] starts the
      evaluation of the inbox at [block] for at most [tick_distance]. *)
  let run_for_ticks node_ctxt (block : Sc_rollup_block.t) tick_distance =
    let open Lwt_result_syntax in
    let open Delayed_write_monad.Lwt_result_syntax in
    let pred_level = Raw_level.to_int32 block.header.level |> Int32.pred in
    let* ctxt =
      Node_context.checkout_context node_ctxt block.header.predecessor
    in
    let* _ctxt, state =
      state_of_head
        node_ctxt
        ctxt
        Layer1.{hash = block.header.predecessor; level = pred_level}
    in
    let* inbox = Node_context.get_inbox node_ctxt block.header.inbox_hash in
    let* {is_migration_block; predecessor; predecessor_timestamp; messages} =
      Node_context.get_messages node_ctxt block.header.inbox_witness
    in
    let messages =
      let open Sc_rollup.Inbox_message in
      Internal Start_of_level
      ::
      (if is_migration_block then
       [Internal Sc_rollup.Inbox_message.protocol_migration_internal_message]
      else [])
      @ Internal (Info_per_level {predecessor; predecessor_timestamp})
        :: messages
      @ [Internal End_of_level]
    in
    let>* state, _counter, _level, _fuel =
      Accounted_pvm.eval_block_inbox
        ~fuel:(Fuel.Accounted.of_ticks tick_distance)
        node_ctxt
        (inbox, messages)
        state
    in
    return state

  (** [state_of_tick node_ctxt tick level] returns [Some (state, hash)] for a
      given [tick] if this [tick] happened before [level].  Otherwise, returns
      [None].*)
  let state_of_tick node_ctxt tick level =
    let open Lwt_result_syntax in
    let* event = Node_context.block_with_tick node_ctxt ~max_level:level tick in
    match event with
    | None -> return_none
    | Some event ->
        assert (Raw_level.(event.header.level <= level)) ;
        let tick_distance =
          Sc_rollup.Tick.distance tick event.initial_tick |> Z.to_int64
        in
        (* TODO: #3384
           We should test that we always have enough blocks to find the tick
           because [state_of_tick] is a critical function. *)
        let* state = run_for_ticks node_ctxt event tick_distance in
        let state = Delayed_write_monad.ignore state in
        let*! hash = PVM.state_hash state in
        return (Some (state, hash))
end
