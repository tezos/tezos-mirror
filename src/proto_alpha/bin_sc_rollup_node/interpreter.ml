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
  (** [process_head node_ctxt store head] interprets the messages associated
      with a [head] from a chain [event]. This requires the inbox to be updated
      beforehand. *)
  val process_head :
    Node_context.t -> Store.t -> Layer1.head -> unit tzresult Lwt.t
end

module Make (PVM : Pvm.S) : S = struct
  module PVM = PVM

  (** [eval_until_input state] advances a PVM [state] until it wants more inputs. *)
  let eval_until_input state =
    let open Lwt_syntax in
    let rec go state =
      let* input_request = PVM.is_input_state state in
      match input_request with
      | Some _ -> return state
      | None ->
          let* next_state = PVM.eval state in
          go next_state
    in
    go state

  (** [feed_input state input] feeds [input] to the PVM in order to advance [state] to the next step
     that requires an input. *)
  let feed_input state input =
    let open Lwt_syntax in
    let* state = eval_until_input state in
    let* state = PVM.set_input input state in
    let* state = PVM.eval state in
    let* state = eval_until_input state in
    return state

  (** [transition_pvm node_ctxt store predecessor_hash hash] runs a PVM at the previous state from block
      [predecessor_hash] by consuming as many messages as possible from block [hash]. *)
  let transition_pvm node_ctxt store predecessor_hash hash =
    let open Node_context in
    let open Lwt_result_syntax in
    (* Retrieve the previous PVM state from store. *)
    let*! predecessor_state = Store.PVMState.find store predecessor_hash in
    let* predecessor_state =
      match predecessor_state with
      | None ->
          (* The predecessor is before the origination.
             Here we use an RPC to get the boot sector instead of doing this
             before and packing it into the [node_ctxt] because the bootsector
             might be very large and we don't want to keep that in memory for
             ever.
          *)
          let* boot_sector =
            Plugin.RPC.Sc_rollup.boot_sector
              node_ctxt.cctxt
              (node_ctxt.cctxt#chain, node_ctxt.cctxt#block)
              node_ctxt.rollup_address
          in
          let*! initial_state = PVM.initial_state store boot_sector in
          return initial_state
      | Some predecessor_state -> return predecessor_state
    in

    (* Obtain inbox and its messages for this block. *)
    let*! inbox = Store.Inboxes.get store hash in
    let inbox_level = Inbox.inbox_level inbox in
    let*! messages = Store.Messages.get store hash in

    (* Iterate the PVM state with all the messages for this level. *)
    let*! state =
      List.fold_left_i_s
        (fun message_counter state payload ->
          let input =
            Sc_rollup_PVM_sem.
              {inbox_level; message_counter = Z.of_int message_counter; payload}
          in
          feed_input state input)
        predecessor_state
        messages
    in

    (* Write final state to store. *)
    let*! () = Store.PVMState.set store hash state in

    (* Compute extra information about the state. *)
    let*! initial_tick = PVM.get_tick predecessor_state in
    let*! last_tick = PVM.get_tick state in
    (* TODO: #2717
       The number of ticks should not be an arbitrarily-sized integer.
    *)
    let num_ticks = Sc_rollup.Tick.distance initial_tick last_tick in
    (* TODO: #2717
       The length of messages here can potentially overflow the [int] returned from [List.length].
    *)
    let num_messages = Z.of_int (List.length messages) in
    let*! () = Store.StateInfo.add store hash {num_messages; num_ticks} in

    (* Produce events. *)
    let*! () = Interpreter_event.transitioned_pvm state num_messages in

    return_unit

  (** [process_head node_ctxt store head] runs the PVM for the given head. *)
  let process_head node_ctxt store (Layer1.Head {hash; _} as head) =
    let open Lwt_result_syntax in
    let*! predecessor_hash = Layer1.predecessor store head in
    transition_pvm node_ctxt store predecessor_hash hash
end
