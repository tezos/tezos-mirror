(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Octez_smart_rollup

let make_ctxt ~rollup_node_endpoint =
  new Retrier_context.ctxt ~timeout:30. rollup_node_endpoint [Media_type.json]

type outbox_message = {
  outbox_level : int32;
  message_index : int;
  transactions : Outbox_message.transaction_summary list;
}

let get_outbox_messages ctxt ~outbox_level =
  let open Lwt_result_syntax in
  let+ messages =
    Rollup_node_services.Local.make_call
      Rollup_node_services.Local.outbox_pending
      ctxt
      ()
      (Some outbox_level)
      ()
  in
  List.fold_left
    (fun acc ((outbox_level, messages), _) ->
      List.fold_left
        (fun acc -> function
          | _, None -> acc
          | _, Some (Outbox_message.Whitelist_update _) -> acc
          | message_index, Some (Transaction_batch transactions) ->
              {outbox_level; message_index; transactions} :: acc)
        acc
        messages)
    []
    messages
  |> List.rev

let get_lcc ctxt =
  Rollup_node_services.Block.make_call
    Rollup_node_services.Block.level
    ctxt
    ((), `Cemented)
    ()
    ()

let get_rollup_address ctxt =
  Rollup_node_services.Global.make_call
    Rollup_node_services.Global.sc_rollup_address
    ctxt
    ()
    ()
    ()
