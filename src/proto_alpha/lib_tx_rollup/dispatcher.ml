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

let withdawals_reveals_of_message_result ctxt msg =
  let open Lwt_result_syntax in
  let withdrawals =
    match msg.Inbox.result with
    | Inbox.Discarded _ -> []
    | Interpreted (_result, withdrawals) -> withdrawals
  in
  List.map_es
    (fun Tx_rollup_withdraw.{claimer; ticket_hash; amount} ->
      let* ticket_index = Context.Ticket_index.get ctxt ticket_hash in
      let*? ticket_index =
        Result.of_option
          ~error:
            [
              Error.Tx_rollup_unknown_ticket
                Protocol.(
                  Indexable.forget
                  @@ Protocol.Tx_rollup_l2_context_sig.Ticket_indexable.value
                       ticket_hash);
            ]
          ticket_index
      in
      let*! ticket = Context.get_ticket ctxt ticket_index in
      let*? Ticket.{ticketer; contents; ty} =
        Result.of_option
          ~error:
            [
              Error.Tx_rollup_unknown_ticket
                (Protocol.Indexable.forget ticket_index);
            ]
          ticket
      in
      return
        Tx_rollup_reveal.
          {
            contents = Script.lazy_expr contents;
            ty = Script.lazy_expr ty;
            ticketer;
            amount;
            claimer;
          })
    withdrawals

let dispatch_operations_of_block (state : State.t) (block : L2block.t) =
  let open Lwt_result_syntax in
  let level = block.header.level in
  let* ctxt = Context.checkout state.context_index block.header.context in
  let tx_rollup = state.rollup_info.rollup_id in
  let commitment = block.commitment in
  let+ rev_ops, _ =
    List.fold_left_es
      (fun (acc, message_index) msg ->
        let context_hash = msg.Inbox.l2_context_hash.tree_hash in
        let* tickets_info = withdawals_reveals_of_message_result ctxt msg in
        let+ acc =
          match tickets_info with
          | [] ->
              (* No withdrawals for this message *)
              return acc
          | _ ->
              let*? message_result_path =
                let open Tx_rollup_commitment.Merkle in
                let tree = List.fold_left snoc nil commitment.messages in
                Environment.wrap_tzresult @@ compute_path tree message_index
              in
              return
                (Tx_rollup_dispatch_tickets
                   {
                     tx_rollup;
                     level;
                     context_hash;
                     message_index;
                     message_result_path;
                     tickets_info;
                   }
                :: acc)
        in
        (acc, message_index + 1))
      ([], 0)
      block.inbox
  in
  List.rev rev_ops

let dispatch_withdrawals ~source state block =
  let open Lwt_result_syntax in
  let* operations = dispatch_operations_of_block state block in
  List.iter_es (Injector.add_pending_operation ~source) operations
