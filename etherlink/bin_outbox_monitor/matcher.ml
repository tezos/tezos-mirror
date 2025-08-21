(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  include Internal_event.Simple
  module Contract = Tezos_raw_protocol_alpha.Alpha_context.Contract

  let section = ["outbox_monitor"; "matcher"]

  let matching_error =
    declare_5
      ~section
      ~name:"matching_error"
      ~msg:
        "Could not match {withdrawals} withdrawals in L2 levels between \
         {min_l2_level} and {max_l2_level} with {outbox_transactions} outbox \
         transactions in outbox level {outbox_level}"
      ~level:Error
      ("withdrawals", Data_encoding.int31)
      ("outbox_transactions", Data_encoding.int31)
      ("outbox_level", Data_encoding.int32)
      ("min_l2_level", Db.quantity_hum_encoding)
      ("max_l2_level", Db.quantity_hum_encoding)
      ~pp4:Ethereum_types.pp_quantity
      ~pp5:Ethereum_types.pp_quantity

  let unmatched_withdrawal =
    declare_8
      ~section
      ~name:"unmatched_withdrawal"
      ~msg:
        "Could not match withdrawal {withdrawal_id} of {amount} {token} from \
         {sender} to {receiver} in transaction {transactionHash} of block \
         {blockNumber} with outbox messages of {outbox_level}"
      ~level:Error
      ("withdrawal_id", Db.quantity_hum_encoding)
      ("amount", Db.quantity_hum_encoding)
      ("token", Db.withdrawal_kind_encoding)
      ("sender", Ethereum_types.address_encoding)
      ("receiver", Contract.encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("blockNumber", Db.quantity_hum_encoding)
      ("outbox_level", Data_encoding.int32)
      ~pp1:Ethereum_types.pp_quantity
      ~pp2:Ethereum_types.pp_quantity
      ~pp3:Db.pp_withdrawal_kind
      ~pp4:(fun fmt a ->
        Format.pp_print_string fmt (Ethereum_types.Address.to_string a))
      ~pp5:Contract.pp
      ~pp6:Ethereum_types.pp_hash
      ~pp7:Ethereum_types.pp_quantity

  let emit_unmatched_withdrawal outbox_level (w : Db.withdrawal_log) =
    emit
      unmatched_withdrawal
      ( w.withdrawal.withdrawal_id,
        w.withdrawal.amount,
        w.withdrawal.kind,
        w.withdrawal.sender,
        w.withdrawal.receiver,
        w.transactionHash,
        w.blockNumber,
        outbox_level )

  let matched =
    declare_4
      ~section
      ~name:"matched"
      ~msg:
        "Matched {withdrawals} withdrawals with outbox level {outbox_level} \
         for L2 levels between {min_l2_level} and {max_l2_level}"
      ~level:Warning
      ("withdrawals", Data_encoding.int31)
      ("outbox_level", Data_encoding.int32)
      ("min_l2_level", Db.quantity_hum_encoding)
      ("max_l2_level", Db.quantity_hum_encoding)
      ~pp3:Ethereum_types.pp_quantity
      ~pp4:Ethereum_types.pp_quantity

  let no_blueprint_in_l1_level =
    declare_1
      ~section
      ~name:"no_blueprint_in_l1_level"
      ~msg:"No blueprint in L1 level {level}"
      ~level:Info
      ("level", Data_encoding.int32)

  let trying_to_match =
    declare_3
      ~section
      ~name:"trying_to_match"
      ~msg:
        "Trying to match outbox level {outbox_level} for withdrawals of L2 \
         levels between {min_l2_level} and {max_l2_level}"
      ~level:Info
      ("outbox_level", Data_encoding.int32)
      ("min_l2_level", Db.quantity_hum_encoding)
      ("max_l2_level", Db.quantity_hum_encoding)
      ~pp2:Ethereum_types.pp_quantity
      ~pp3:Ethereum_types.pp_quantity
end

(** [match_levels db ~outbox_level l2_levels_range] matches withdrawals from L2
    blocks with the corresponding outbox messages/transaction known by the
    rollup node.

    For a given L1 outbox level and a list of L2 levels that were finalized at
    that L1 level, this function:
    1. Retrieves all withdrawals that occurred in those L2 blocks
    2. Matches them with outbox messages known by the rollup node at the L1
       level
    3. Records the matching in the database

    The matching is done in order (by transaction/log indexes) to ensure correct
    pairing. If there is a mismatch between the number of withdrawals and outbox
    messages, an error is logged but processing continues because we know the
    L1/L2 levels associations.

    @param db The database connection
    @param outbox_level The L1 level containing outbox messages
    @param l2_levels_range Range of L2 levels that were finalized at this L1
      level (strict on the left)
*)
let match_levels db ~outbox_level Db.{start_l2; end_l2} =
  let open Lwt_result_syntax in
  let min_level = Ethereum_types.Qty.next start_l2 in
  let max_level = end_l2 in
  let*! () =
    Event.(emit trying_to_match) (outbox_level, min_level, max_level)
  in
  (* Fetch all withdrawals between the first and last L2 levels in the range. *)
  let* withdrawals =
    Db.Withdrawals.list_by_block_numbers db ~min_level ~max_level
  in
  (* Only proceed if there are withdrawals to match *)
  when_ (withdrawals <> []) @@ fun () ->
  (* Get all outbox message indexes for this L1 level *)
  let* outbox_indexes =
    Db.Outbox_messages.indexes_by_outbox_level db ~outbox_level
  in
  (* Check that we have the same number of withdrawals and outbox messages.
     If not, this indicates a mismatch between L1 and L2 state. *)
  if List.compare_lengths withdrawals outbox_indexes <> 0 then
    let*! () =
      Event.(emit matching_error)
        ( List.length withdrawals,
          List.length outbox_indexes,
          outbox_level,
          min_level,
          max_level )
    in
    let*! () =
      List.iter_s (Event.emit_unmatched_withdrawal outbox_level) withdrawals
    in
    return_unit
  else
    (* Log successful matching *)
    let*! () =
      Event.(emit matched)
        (List.length withdrawals, outbox_level, min_level, max_level)
    in
    (* Associate each withdrawal with its corresponding outbox message index.
       This allows us to track which L1 outbox message and transaction
       corresponds to each L2 withdrawal. *)
    let* () =
      List.iter2_es
        ~when_different_lengths:
          (TzTrace.make (error_of_exn (Failure "unreachable")))
        (fun Db.{transactionHash; transactionIndex; logIndex; _} outbox_index ->
          Db.Withdrawals.set_outbox_index
            db
            ~transactionHash
            ~transactionIndex
            ~logIndex
            outbox_index)
        withdrawals
        outbox_indexes
    in
    return_unit

let match_from_l1_level db l1_level
    (Db.{start_l2 = Qty start_l2; end_l2 = Qty end_l2} as l2_levels_range) =
  let open Lwt_result_syntax in
  let* () =
    if Z.Compare.(start_l2 >= end_l2) then
      let*! () = Event.(emit no_blueprint_in_l1_level) l1_level in
      return_unit
    else match_levels db ~outbox_level:l1_level l2_levels_range
  in
  Db.Pointers.Last_matched_L1_level.set db l1_level

let run db =
  let open Lwt_result_syntax in
  let* to_match = Db.Levels.levels_to_match db in
  List.iter_es
    (fun (l1_level, l2_levels_range) ->
      match_from_l1_level db l1_level l2_levels_range)
    to_match
