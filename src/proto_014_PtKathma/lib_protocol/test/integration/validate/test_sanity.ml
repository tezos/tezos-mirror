(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic-Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission  is hereby granted, free of charge, to any person obtaining a  *)
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

(** Testing
    -------
    Component:  Protocol (validate manager)
    Invocation: dune exec \
                src/proto_014_PtKathma/lib_protocol/test/integration/validate/main.exe \
                -- test "^sanity checks"
    Subject:    Sanity check for Validation of manager operation tests.
*)

open Protocol
open Alpha_context
open Manager_operation_helpers

(** The goal of this test is to ensure that [select_op] generate the
   wanted kind of manager operation

    Note: if a new manager operation kind is added in the protocol,
   [Manager_operation_helpers.manager_operation_kind] should be
   extended. You will also have to extend
   [Manager_operation_helpers.select_op] with a new `mk` for this new
   operation. Finally the list [Manager_operation_helpers.subjects]
   should also be extended to run the validate test on the new manager
   operation kind. *)
let ensure_kind infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {(operation_req_default kind) with force_reveal = Some false}
      infos
  in
  let (Operation_data {contents; _}) = op.protocol_data in
  match contents with
  | Single (Manager_operation {operation; _}) -> (
      match (operation, kind) with
      | Transaction _, K_Transaction
      | Reveal _, K_Reveal
      | Origination _, K_Origination
      | Delegation _, K_Delegation
      | Delegation _, K_Undelegation
      | Delegation _, K_Self_delegation
      | Register_global_constant _, K_Register_global_constant
      | Set_deposits_limit _, K_Set_deposits_limit
      | Increase_paid_storage _, K_Increase_paid_storage
      | Tx_rollup_origination, K_Tx_rollup_origination
      | Tx_rollup_submit_batch _, K_Tx_rollup_submit_batch
      | Tx_rollup_commit _, K_Tx_rollup_commit
      | Tx_rollup_return_bond _, K_Tx_rollup_return_bond
      | Tx_rollup_finalize_commitment _, K_Tx_rollup_finalize
      | Tx_rollup_remove_commitment _, K_Tx_rollup_remove_commitment
      | Tx_rollup_rejection _, K_Tx_rollup_reject
      | Tx_rollup_dispatch_tickets _, K_Tx_rollup_dispatch_tickets
      | Transfer_ticket _, K_Transfer_ticket
      | Sc_rollup_originate _, K_Sc_rollup_origination
      | Sc_rollup_add_messages _, K_Sc_rollup_add_messages
      | Sc_rollup_cement _, K_Sc_rollup_cement
      | Sc_rollup_publish _, K_Sc_rollup_publish
      | Sc_rollup_refute _, K_Sc_rollup_refute
      | Sc_rollup_timeout _, K_Sc_rollup_timeout
      | Sc_rollup_execute_outbox_message _, K_Sc_rollup_execute_outbox_message
      | Sc_rollup_recover_bond _, K_Sc_rollup_recover_bond ->
          return_unit
      | ( ( Transaction _ | Origination _ | Register_global_constant _
          | Delegation _ | Set_deposits_limit _ | Increase_paid_storage _
          | Reveal _ | Tx_rollup_origination | Tx_rollup_submit_batch _
          | Tx_rollup_commit _ | Tx_rollup_return_bond _
          | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _
          | Tx_rollup_dispatch_tickets _ | Transfer_ticket _
          | Tx_rollup_rejection _ | Sc_rollup_originate _ | Sc_rollup_publish _
          | Sc_rollup_cement _ | Sc_rollup_add_messages _ | Sc_rollup_refute _
          | Sc_rollup_timeout _ | Sc_rollup_execute_outbox_message _
          | Sc_rollup_recover_bond _ | Dal_publish_slot_header _
          | Sc_rollup_dal_slot_subscribe _ ),
          _ ) ->
          assert false)
  | Single _ -> assert false
  | Cons _ -> assert false

let ensure_manager_operation_coverage () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  List.iter_es (fun kind -> ensure_kind infos kind) subjects

let tests =
  List.map
    (fun (name, f) -> Tztest.tztest name `Quick f)
    [("manager operation coverage", ensure_manager_operation_coverage)]
