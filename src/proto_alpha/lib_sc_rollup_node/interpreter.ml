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
    predecessor:Layer1.header ->
    Layer1.header ->
    Sc_rollup.Inbox.t * Sc_rollup.Inbox_message.t list ->
    ('a Context.t * int * int64 * Sc_rollup.Tick.t) tzresult Lwt.t

  val state_of_tick :
    _ Node_context.t ->
    ?start_state:Accounted_pvm.eval_state ->
    Sc_rollup.Tick.t ->
    Raw_level.t ->
    Accounted_pvm.eval_state option tzresult Lwt.t

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

  let get_boot_sector block_hash (node_ctxt : _ Node_context.t) =
    let open Lwt_result_syntax in
    match node_ctxt.boot_sector_file with
    | None -> get_boot_sector block_hash node_ctxt
    | Some boot_sector_file ->
        let*! boot_sector = Lwt_utils_unix.read_file boot_sector_file in
        let*? boot_sector =
          Option.value_e
            ~error:
              [
                Sc_rollup_node_errors.Unparsable_boot_sector
                  {path = boot_sector_file};
              ]
            (PVM.parse_boot_sector boot_sector)
        in
        return boot_sector

  let genesis_state block_hash node_ctxt ctxt =
    let open Lwt_result_syntax in
    let* boot_sector = get_boot_sector block_hash node_ctxt in
    let*! initial_state = PVM.initial_state ~empty:(PVM.State.empty ()) in
    let*! genesis_state = PVM.install_boot_sector initial_state boot_sector in
    let*! ctxt = PVM.State.set ctxt genesis_state in
    (* Assert that the initial genesis state is equal to the rollup's genesis
       state in the protocol. *)
    let* () =
      let genesis_commitment = node_ctxt.genesis_info.commitment_hash in
      let*! compressed_state = PVM.state_hash genesis_state in
      let our_genesis_commitment =
        Sc_rollup.Commitment.(
          hash_uncarbonated
            {
              compressed_state;
              inbox_level = node_ctxt.genesis_info.level;
              predecessor = Sc_rollup.Commitment.Hash.zero;
              number_of_ticks = Sc_rollup.Number_of_ticks.zero;
            })
      in
      fail_unless
        (Sc_rollup.Commitment.Hash.equal
           genesis_commitment
           our_genesis_commitment)
        (Sc_rollup_node_errors.Invalid_genesis_state
           {expected = genesis_commitment; actual = our_genesis_commitment})
    in
    return (ctxt, genesis_state)

  let state_of_head node_ctxt ctxt Layer1.{hash; level} =
    let open Lwt_result_syntax in
    let*! state = PVM.State.find ctxt in
    match state with
    | None ->
        let genesis_level =
          Raw_level.to_int32 node_ctxt.Node_context.genesis_info.level
        in
        if level = genesis_level then genesis_state hash node_ctxt ctxt
        else tzfail (Sc_rollup_node_errors.Missing_PVM_state (hash, level))
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
    let* {
           state = {state; state_hash; inbox_level; tick; _};
           num_messages;
           num_ticks;
         } =
      Delayed_write_monad.apply node_ctxt eval_result
    in
    let*! ctxt = PVM.State.set ctxt state in
    let*! initial_tick = PVM.get_tick predecessor_state in
    (* Produce events. *)
    let*! () =
      Interpreter_event.transitioned_pvm
        inbox_level
        state_hash
        tick
        num_messages
    in
    return (ctxt, num_messages, Z.to_int64 num_ticks, initial_tick)

  (** [process_head node_ctxt ctxt ~predecessor head] runs the PVM for the given
      head. *)
  let process_head (node_ctxt : _ Node_context.t) ctxt
      ~(predecessor : Layer1.header) (head : Layer1.header) inbox_messages =
    let open Lwt_result_syntax in
    let first_inbox_level =
      Raw_level.to_int32 node_ctxt.genesis_info.level |> Int32.succ
    in
    if head.Layer1.level >= first_inbox_level then
      transition_pvm
        node_ctxt
        ctxt
        (Layer1.head_of_header predecessor)
        (Layer1.head_of_header head)
        inbox_messages
    else if head.Layer1.level = Raw_level.to_int32 node_ctxt.genesis_info.level
    then
      let* ctxt, state = genesis_state head.hash node_ctxt ctxt in
      let*! ctxt = PVM.State.set ctxt state in
      return (ctxt, 0, 0L, Sc_rollup.Tick.initial)
    else return (ctxt, 0, 0L, Sc_rollup.Tick.initial)

  (** Returns the starting evaluation before the evaluation of the block. It
      contains the PVM state at the end of the execution of the previous block
      and the messages the block ([remaining_messages]). *)
  let start_state_of_block node_ctxt (block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
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
    let inbox_level = Sc_rollup.Inbox.inbox_level inbox in
    let*! tick = PVM.get_tick state in
    let*! state_hash = PVM.state_hash state in
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
    return
      Accounted_pvm.
        {
          state;
          state_hash;
          inbox_level;
          tick;
          message_counter_offset = 0;
          remaining_fuel = Fuel.Accounted.of_ticks 0L;
          remaining_messages = messages;
        }

  (** [run_for_ticks node_ctxt start_state tick_distance] starts the evaluation
      of messages in the [start_state] for at most [tick_distance]. *)
  let run_to_tick node_ctxt start_state tick =
    let open Delayed_write_monad.Lwt_result_syntax in
    let tick_distance =
      Sc_rollup.Tick.distance tick start_state.Accounted_pvm.tick |> Z.to_int64
    in
    let>+ eval_result =
      Accounted_pvm.eval_messages
        node_ctxt
        {
          start_state with
          remaining_fuel = Fuel.Accounted.of_ticks tick_distance;
        }
    in
    eval_result.state

  let state_of_tick_aux node_ctxt ~start_state (event : Sc_rollup_block.t) tick
      =
    let open Lwt_result_syntax in
    let* start_state =
      match start_state with
      | Some start_state
        when Raw_level.(
               start_state.Accounted_pvm.inbox_level = event.header.level) ->
          return start_state
      | _ ->
          (* Recompute start state on level change or if we don't have a
             starting state on hand. *)
          start_state_of_block node_ctxt event
    in
    (* TODO: #3384
       We should test that we always have enough blocks to find the tick
       because [state_of_tick] is a critical function. *)
    let* result_state = run_to_tick node_ctxt start_state tick in
    let result_state = Delayed_write_monad.ignore result_state in
    return result_state

  (* The cache allows cache intermediate states of the PVM in e.g. dissections. *)
  module Tick_state_cache =
    Aches_lwt.Lache.Make
      (Aches.Rache.Transfer
         (Aches.Rache.LRU)
         (struct
           type t = Sc_rollup.Tick.t * Block_hash.t

           let equal (t1, b1) (t2, b2) =
             Sc_rollup.Tick.(t1 = t2) && Block_hash.(b1 = b2)

           let hash (tick, block) =
             ((Sc_rollup.Tick.to_z tick |> Z.hash) * 13) + Block_hash.hash block
         end))

  let tick_state_cache = Tick_state_cache.create 64 (* size of 2 dissections *)

  (* Memoized version of [state_of_tick_aux]. *)
  let memo_state_of_tick_aux node_ctxt ~start_state (event : Sc_rollup_block.t)
      tick =
    Tick_state_cache.bind_or_put
      tick_state_cache
      (tick, event.header.block_hash)
      (fun (tick, _hash) -> state_of_tick_aux node_ctxt ~start_state event tick)
      Lwt.return

  (** [state_of_tick node_ctxt ?start_state tick level] returns [Some end_state]
      for a given [tick] if this [tick] happened before [level]. Otherwise,
      returns [None].*)
  let state_of_tick node_ctxt ?start_state tick level =
    let open Lwt_result_syntax in
    let* event = Node_context.block_with_tick node_ctxt ~max_level:level tick in
    match event with
    | None -> return_none
    | Some event ->
        assert (Raw_level.(event.header.level <= level)) ;
        let* result_state =
          if Node_context.is_loser node_ctxt then
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/5253
               The failures/loser mode does not work properly when restarting
               from intermediate states. *)
            state_of_tick_aux node_ctxt ~start_state:None event tick
          else memo_state_of_tick_aux node_ctxt ~start_state event tick
        in
        return_some result_state
end
