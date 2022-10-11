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

module type S = sig
  module Interpreter : Interpreter.S

  module PVM = Interpreter.PVM
  module Fueled_pvm = Interpreter.Free_pvm

  type t = {
    ctxt : Context.ro;
    inbox_level : Raw_level.t;
    nb_messages : int64;
    state : PVM.state;
    reveal_map : string Sc_rollup.Reveal_hash.Map.t option;
  }

  val start_simulation :
    Node_context.ro ->
    reveal_map:string Sc_rollup.Reveal_hash.Map.t option ->
    Layer1.head ->
    t tzresult Lwt.t

  val simulate_messages :
    Node_context.ro ->
    t ->
    Sc_rollup.Inbox_message.t list ->
    (t * Z.t) tzresult Lwt.t
end

module Make (Interpreter : Interpreter.S) :
  S with module Interpreter = Interpreter = struct
  module Interpreter = Interpreter
  module PVM = Interpreter.PVM
  module Fueled_pvm = Interpreter.Free_pvm

  type t = {
    ctxt : Context.ro;
    inbox_level : Raw_level.t;
    nb_messages : int64;
    state : PVM.state;
    reveal_map : string Sc_rollup.Reveal_hash.Map.t option;
  }

  let start_simulation node_ctxt ~reveal_map (Layer1.{hash; level} as head) =
    let open Lwt_result_syntax in
    let*? level = Environment.wrap_tzresult @@ Raw_level.of_int32 level in
    let* ctxt =
      if Raw_level.(level <= node_ctxt.Node_context.genesis_info.level) then
        (* This is before we have interpreted the boot sector, so we start
           with an empty context in genesis *)
        return (Context.empty node_ctxt.context)
      else Node_context.checkout_context node_ctxt hash
    in
    let* inbox = Inbox.inbox_of_head node_ctxt head in
    let+ ctxt, state = Interpreter.state_of_head node_ctxt ctxt head in
    let nb_messages =
      Sc_rollup.Inbox.number_of_messages_during_commitment_period inbox
    in
    let inbox_level = Raw_level.succ level in
    {ctxt; nb_messages; state; inbox_level; reveal_map}

  let simulate_messages (node_ctxt : Node_context.ro)
      {ctxt; nb_messages; state; inbox_level; reveal_map} messages =
    let open Lwt_result_syntax in
    (* Build new inbox *)
    let*? () =
      error_when
        (messages = [])
        (Environment.wrap_tzerror Sc_rollup_errors.Sc_rollup_add_zero_messages)
    in
    let max_messages =
      node_ctxt.protocol_constants.parametric.sc_rollup
        .max_number_of_messages_per_commitment_period |> Int64.of_int
    in
    let nb_messages =
      Int64.add nb_messages (Int64.of_int (List.length messages))
    in
    let*? () =
      error_when
        Compare.Int64.(nb_messages > max_messages)
        (Environment.wrap_tzerror
           Sc_rollup_errors
           .Sc_rollup_max_number_of_messages_reached_for_commitment_period)
    in
    (* Build new state *)
    let* Fueled_pvm.{state; num_ticks; _} =
      Fueled_pvm.eval_messages
        ?reveal_map
        ~fuel:(Fuel.Free.of_ticks 0L)
        node_ctxt
        state
        inbox_level
        messages
    in
    let*! ctxt = PVM.State.set ctxt state in
    return ({ctxt; nb_messages; state; inbox_level; reveals_map}, num_ticks)
end
