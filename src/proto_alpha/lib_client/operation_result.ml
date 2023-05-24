(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Apply_results
open Apply_operation_result
open Apply_internal_results

let tez_sym = "\xEA\x9C\xA9"

let pp_micheline_expr ppf expr =
  Format.fprintf ppf "@[<v 0>%a@]" Michelson_v1_printer.print_expr expr

let pp_micheline_from_lazy_expr ppf expr =
  let expr =
    WithExceptions.Option.to_exn
      ~none:(Failure "ill-serialized micheline expression")
      (Data_encoding.force_decode expr)
  in
  pp_micheline_expr ppf expr

let pp_internal_operation ppf (Internal_operation {operation; sender; _}) =
  (* For now, try to use the same format as in [pp_manager_operation_content]. *)
  Format.fprintf ppf "@[<v 2>Internal " ;
  (match operation with
  | Transaction {destination; amount; parameters; entrypoint} ->
      Format.fprintf
        ppf
        "Transaction:@,Amount: %s%a@,From: %a@,To: %a"
        tez_sym
        Tez.pp
        amount
        Destination.pp
        sender
        Destination.pp
        destination ;
      if not (Entrypoint.is_default entrypoint) then
        Format.fprintf ppf "@,Entrypoint: %a" Entrypoint.pp entrypoint ;
      if not (Script_repr.is_unit_parameter parameters) then
        Format.fprintf
          ppf
          "@,Parameter: %a"
          pp_micheline_from_lazy_expr
          parameters
  | Origination {delegate; credit; script = {code; storage}} -> (
      Format.fprintf
        ppf
        "Origination:@,From: %a@,Credit: %s%a"
        Destination.pp
        sender
        tez_sym
        Tez.pp
        credit ;
      let code =
        WithExceptions.Option.to_exn
          ~none:(Failure "ill-serialized code")
          (Data_encoding.force_decode code)
      in
      let {Michelson_v1_parser.source; _} =
        Michelson_v1_printer.unparse_toplevel code
      in
      Format.fprintf
        ppf
        "@,@[<hv 2>Script:@ @[<h>%a@]@,@[<hv 2>Initial storage:@ %a@]@]"
        Format.pp_print_text
        source
        pp_micheline_from_lazy_expr
        storage ;
      match delegate with
      | None -> Format.fprintf ppf "@,No delegate for this contract"
      | Some delegate ->
          Format.fprintf
            ppf
            "@,Delegate: %a"
            Signature.Public_key_hash.pp
            delegate)
  | Delegation delegate_opt -> (
      Format.fprintf ppf "Delegation:@,Contract: %a@,To: " Destination.pp sender ;
      match delegate_opt with
      | None -> Format.pp_print_string ppf "nobody"
      | Some delegate -> Signature.Public_key_hash.pp ppf delegate)
  | Event {ty; tag; payload} ->
      Format.fprintf
        ppf
        "Event:@,From: %a@,Type: %a"
        Destination.pp
        sender
        pp_micheline_expr
        ty ;
      if not (Entrypoint.is_default tag) then
        Format.fprintf ppf "@,Tag: %a" Entrypoint.pp tag ;
      Format.fprintf ppf "@,Payload: %a" pp_micheline_expr payload) ;
  Format.fprintf ppf "@]"

let pp_manager_operation_content (type kind) source ppf
    (operation : kind manager_operation) =
  (* For now, try to keep formatting in sync with [pp_internal_operation]. *)
  match operation with
  | Transaction {destination; amount; parameters; entrypoint} ->
      Format.fprintf
        ppf
        "Transaction:@,Amount: %s%a@,From: %a@,To: %a"
        tez_sym
        Tez.pp
        amount
        Contract.pp
        source
        Contract.pp
        destination ;
      if not (Entrypoint.is_default entrypoint) then
        Format.fprintf ppf "@,Entrypoint: %a" Entrypoint.pp entrypoint ;
      if not (Script_repr.is_unit_parameter parameters) then
        Format.fprintf
          ppf
          "@,Parameter: %a"
          pp_micheline_from_lazy_expr
          parameters
  | Origination {delegate; credit; script = {code; storage}} -> (
      Format.fprintf
        ppf
        "Origination:@,From: %a@,Credit: %s%a"
        Contract.pp
        source
        tez_sym
        Tez.pp
        credit ;
      let code =
        WithExceptions.Option.to_exn
          ~none:(Failure "ill-serialized code")
          (Data_encoding.force_decode code)
      in
      let {Michelson_v1_parser.source; _} =
        Michelson_v1_printer.unparse_toplevel code
      in
      Format.fprintf
        ppf
        "@,@[<hv 2>Script:@ @[<h>%a@]@,@[<hv 2>Initial storage:@ %a@]"
        Format.pp_print_text
        source
        pp_micheline_from_lazy_expr
        storage ;
      match delegate with
      | None -> Format.fprintf ppf "@,No delegate for this contract"
      | Some delegate ->
          Format.fprintf
            ppf
            "@,Delegate: %a"
            Signature.Public_key_hash.pp
            delegate)
  | Reveal key ->
      Format.fprintf
        ppf
        "Revelation of manager public key:@,Contract: %a@,Key: %a"
        Contract.pp
        source
        Signature.Public_key.pp
        key
  | Delegation delegate_opt -> (
      Format.fprintf ppf "Delegation:@,Contract: %a@,To: " Contract.pp source ;
      match delegate_opt with
      | None -> Format.pp_print_string ppf "nobody"
      | Some delegate -> Signature.Public_key_hash.pp ppf delegate)
  | Register_global_constant {value} ->
      Format.fprintf
        ppf
        "Register Global:@,Value: %a"
        pp_micheline_from_lazy_expr
        value
  | Set_deposits_limit limit_opt -> (
      Format.fprintf
        ppf
        "Set deposits limit:@,Delegate: %a@,"
        Contract.pp
        source ;
      match limit_opt with
      | None -> Format.pp_print_string ppf "Unlimited deposits"
      | Some limit -> Format.fprintf ppf "Limit: %a" Tez.pp limit)
  | Increase_paid_storage {amount_in_bytes; destination} ->
      Format.fprintf
        ppf
        "Increase paid storage:@,Increased size: %a bytes@,From: %a@,To: %a"
        Z.pp_print
        amount_in_bytes
        Contract.pp
        source
        Contract_hash.pp
        destination
  | Update_consensus_key pk ->
      Format.fprintf
        ppf
        "Update_consensus_key:@,Public key hash: %a"
        Signature.Public_key_hash.pp
        (Signature.Public_key.hash pk)
  | Transfer_ticket {contents; ty; ticketer; amount; destination; entrypoint} ->
      Format.fprintf
        ppf
        "Transfer tickets:@,\
         Ticket content: %a@,\
         Ticket content type: %a@,\
         Ticket ticketer: %a@,\
         Ticket amount: %a@,\
         Destination: %a%a@,\
         From: %a"
        pp_micheline_from_lazy_expr
        contents
        pp_micheline_from_lazy_expr
        ty
        Contract.pp
        ticketer
        Z.pp_print
        Script_int.(to_zint (amount :> n num))
        Contract.pp
        destination
        (fun ppf entrypoint ->
          if not (Entrypoint.is_default entrypoint) then
            Format.fprintf ppf "@,Entrypoint: %a" Entrypoint.pp entrypoint)
        entrypoint
        Contract.pp
        source
  | Sc_rollup_originate {kind; boot_sector; parameters_ty} ->
      Format.fprintf
        ppf
        "Smart rollup origination:@,\
         Kind: %a@,\
         Parameter type: %a@,\
         Kernel Blake2B hash: '%a'"
        Sc_rollup.Kind.pp
        kind
        pp_micheline_from_lazy_expr
        parameters_ty
        Tezos_crypto.Blake2B.pp
        (Tezos_crypto.Blake2B.hash_string [boot_sector])
  | Sc_rollup_add_messages {messages = _} ->
      Format.pp_print_string ppf "Smart rollup messages submission:"
  | Sc_rollup_cement {rollup; commitment} ->
      Format.fprintf
        ppf
        "Smart rollup commitment cementing:@,Address: %a@,Commitment: %a"
        Sc_rollup.Address.pp
        rollup
        Sc_rollup.Commitment.Hash.pp
        commitment
  | Sc_rollup_publish {rollup; commitment} ->
      Format.fprintf
        ppf
        "Smart rollup commitment publishing:@,\
         Address: %a@,\
         @[<v 2>Commitment:@,\
         %a@]"
        Sc_rollup.Address.pp
        rollup
        Sc_rollup.Commitment.pp
        commitment
  | Sc_rollup_refute {rollup; opponent; refutation} ->
      Format.fprintf
        ppf
        "Smart rollup refutation move:@,Address: %a@,Staker: %a@,Refutation: %a"
        Sc_rollup.Address.pp
        rollup
        Sc_rollup.Staker.pp
        opponent
        Sc_rollup.Game.pp_refutation
        refutation
  | Sc_rollup_timeout {rollup; stakers = {alice; bob}} ->
      Format.fprintf
        ppf
        "Smart rollup refutation timeout:@,\
         Address: %a@,\
         First staker (Alice): %a@,\
         Second staker (Bob): %a"
        Sc_rollup.Address.pp
        rollup
        Sc_rollup.Staker.pp
        alice
        Sc_rollup.Staker.pp
        bob
  | Sc_rollup_execute_outbox_message
      {rollup; cemented_commitment; output_proof = _} ->
      Format.fprintf
        ppf
        "Smart rollup output message execution:@,\
         Address: %a@,\
         Cemented commitment: %a"
        Sc_rollup.Address.pp
        rollup
        Sc_rollup.Commitment.Hash.pp
        cemented_commitment
  | Sc_rollup_recover_bond {sc_rollup; staker} ->
      Format.fprintf
        ppf
        "Smart rollup bond retrieval:@,Address: %a@,Staker: %a"
        Sc_rollup.Address.pp
        sc_rollup
        Signature.Public_key_hash.pp
        staker
  | Dal_publish_slot_header operation ->
      Format.fprintf
        ppf
        "Data availability slot header publishing:@,Slot: %a"
        Dal.Operations.Publish_slot_header.pp
        operation
  | Zk_rollup_origination _ ->
      Format.fprintf ppf "Epoxy origination:@,From: %a" Contract.pp source
  | Zk_rollup_publish _ ->
      Format.fprintf ppf "Epoxy publish:@,From: %a" Contract.pp source
  | Zk_rollup_update _ ->
      Format.fprintf ppf "Epoxy update:@,From: %a" Contract.pp source

let pp_balance_updates ppf balance_updates =
  let open Receipt in
  (* For dry runs, the baker's key is zero
     (tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU). Instead of printing this
     key hash, we want to make the result more informative. *)
  let pp_baker ppf baker =
    if Signature.Public_key_hash.equal baker Signature.Public_key_hash.zero then
      Format.fprintf ppf "the baker who will include this operation"
    else Signature.Public_key_hash.pp ppf baker
  in
  let balance_updates =
    List.map
      (fun (balance, update, origin) ->
        let balance =
          match balance with
          | Contract c -> Format.asprintf "%a" Contract.pp c
          | Block_fees -> "payload fees(the block proposer)"
          | Deposits pkh -> Format.asprintf "deposits(%a)" pp_baker pkh
          | Nonce_revelation_rewards -> "nonce revelation rewards"
          | Endorsing_rewards -> "endorsing rewards"
          | Baking_rewards -> "baking rewards"
          | Baking_bonuses -> "baking bonuses"
          | Storage_fees -> "storage fees"
          | Double_signing_punishments -> "double signing punishments"
          | Lost_endorsing_rewards (pkh, p, r) ->
              let reason =
                match (p, r) with
                | false, false -> ""
                | false, true -> ",revelation"
                | true, false -> ",participation"
                | true, true -> ",participation,revelation"
              in
              Format.asprintf "lost endorsing rewards(%a%s)" pp_baker pkh reason
          | Liquidity_baking_subsidies -> "liquidity baking subsidies"
          | Burned -> "burned"
          | Commitments bpkh ->
              Format.asprintf "commitment(%a)" Blinded_public_key_hash.pp bpkh
          | Bootstrap -> "bootstrap"
          | Invoice -> "invoices"
          | Initial_commitments -> "initial commitments"
          | Minted -> "minted"
          | Frozen_bonds (contract, bond_id) ->
              Format.asprintf
                "Frozen_bonds(%a,%a)"
                Contract.pp
                contract
                Bond_id.pp
                bond_id
          | Sc_rollup_refutation_punishments ->
              "smart rollup refutation punishments"
          | Sc_rollup_refutation_rewards -> "smart rollup refutation rewards"
        in
        let balance =
          match origin with
          | Block_application -> balance
          | Protocol_migration -> Format.asprintf "migration %s" balance
          | Subsidy -> Format.asprintf "subsidy %s" balance
          | Simulation -> Format.asprintf "simulation %s" balance
        in
        (balance, update))
      balance_updates
  in
  let column_size =
    List.fold_left
      (fun acc (balance, _) -> Compare.Int.max acc (String.length balance))
      0
      balance_updates
  in
  let pp_update ppf = function
    | Credited amount -> Format.fprintf ppf "+%s%a" tez_sym Tez.pp amount
    | Debited amount -> Format.fprintf ppf "-%s%a" tez_sym Tez.pp amount
  in
  let pp_one ppf (balance, update) =
    let to_fill = column_size + 3 - String.length balance in
    let filler = String.make to_fill '.' in
    Format.fprintf ppf "%s %s %a" balance filler pp_update update
  in
  match balance_updates with
  | [] -> ()
  | balance_updates ->
      Format.fprintf
        ppf
        "@,@[<v 2>Balance updates:@,%a@]"
        (Format.pp_print_list pp_one)
        balance_updates

let pp_ticket_receipt ppf ticket_receipt =
  let open Ticket_receipt in
  let pp_amount ppf amount =
    Format.fprintf
      ppf
      "%s%a"
      (if Z.(zero < amount) then "+" else "")
      Z.pp_print
      amount
  in
  let pp_account_update ppf {account; amount} =
    Format.fprintf ppf "%a ... %a" Destination.pp account pp_amount amount
  in
  let pp_account_updates ppf updates =
    Format.fprintf
      ppf
      "@[<v 2>Account updates:@,%a@]"
      (Format.pp_print_list pp_account_update)
      updates
  in
  let pp_item ppf {ticket_token; updates} =
    let Ticket_token.{ticketer; contents_type; contents} = ticket_token in
    Format.fprintf
      ppf
      "Ticketer: %a@,Content type: %a@,Content: %a@,%a"
      Contract.pp
      ticketer
      Michelson_v1_printer.print_expr
      contents_type
      Michelson_v1_printer.print_expr
      contents
      pp_account_updates
      updates
  in
  match ticket_receipt with
  | [] -> ()
  | ticket_updates ->
      Format.fprintf
        ppf
        "@,@[<v 2>Ticket updates:@,%a@]"
        (Format.pp_print_list pp_item)
        ticket_updates

let pp_slot_header ppf slot_header =
  Format.fprintf ppf "@,@[%a@]" Dal.Slot.Header.pp slot_header

let pp_consumed_gas ppf consumed_gas =
  Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas

let pp_paid_storage_size_diff ppf paid_storage_size_diff =
  if paid_storage_size_diff <> Z.zero then
    Format.fprintf
      ppf
      "@,Paid storage size diff: %s bytes"
      (Z.to_string paid_storage_size_diff)

let pp_storage_size ppf storage_size =
  if storage_size <> Z.zero then
    Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string storage_size)

let pp_lazy_storage_diff ppf = function
  | None -> ()
  | Some lazy_storage_diff -> (
      let big_map_diff =
        Contract.Legacy_big_map_diff.of_lazy_storage_diff lazy_storage_diff
      in
      match (big_map_diff :> Contract.Legacy_big_map_diff.item list) with
      | [] -> ()
      | _ :: _ ->
          (* TODO: print all lazy storage diff *)
          Format.fprintf
            ppf
            "@,@[<v 2>Updated big_maps:@ %a@]"
            Michelson_v1_printer.print_big_map_diff
            lazy_storage_diff)

let pp_origination_result ppf
    {
      lazy_storage_diff;
      balance_updates;
      consumed_gas;
      originated_contracts;
      storage_size;
      paid_storage_size_diff;
    } =
  (match originated_contracts with
  | [] -> ()
  | contracts ->
      Format.fprintf
        ppf
        "@,@[<v 2>Originated contracts:@,%a@]"
        (Format.pp_print_list Contract_hash.pp)
        contracts) ;
  pp_storage_size ppf storage_size ;
  pp_lazy_storage_diff ppf lazy_storage_diff ;
  pp_paid_storage_size_diff ppf paid_storage_size_diff ;
  pp_consumed_gas ppf consumed_gas ;
  pp_balance_updates ppf balance_updates

let pp_transaction_result ppf = function
  | Transaction_to_contract_result
      {
        balance_updates;
        ticket_receipt;
        consumed_gas;
        storage;
        originated_contracts;
        storage_size;
        paid_storage_size_diff;
        lazy_storage_diff;
        allocated_destination_contract = _;
      } ->
      (match originated_contracts with
      | [] -> ()
      | contracts ->
          Format.fprintf
            ppf
            "@,@[<v 2>Originated contracts:@,%a@]"
            (Format.pp_print_list Contract_hash.pp)
            contracts) ;
      (match storage with
      | None -> ()
      | Some expr ->
          Format.fprintf
            ppf
            "@,@[<hv 2>Updated storage:@ %a@]"
            Michelson_v1_printer.print_expr
            expr) ;
      pp_lazy_storage_diff ppf lazy_storage_diff ;
      pp_storage_size ppf storage_size ;
      pp_paid_storage_size_diff ppf paid_storage_size_diff ;
      pp_consumed_gas ppf consumed_gas ;
      pp_balance_updates ppf balance_updates ;
      pp_ticket_receipt ppf ticket_receipt
  | Transaction_to_sc_rollup_result {consumed_gas; ticket_receipt} ->
      pp_consumed_gas ppf consumed_gas ;
      pp_ticket_receipt ppf ticket_receipt
  | Transaction_to_zk_rollup_result
      {balance_updates; consumed_gas; ticket_hash; paid_storage_size_diff} ->
      pp_consumed_gas ppf consumed_gas ;
      pp_balance_updates ppf balance_updates ;
      Format.fprintf ppf "@,Ticket hash: %a" Ticket_hash.pp ticket_hash ;
      pp_paid_storage_size_diff ppf paid_storage_size_diff

let pp_operation_result ~operation_name pp_operation_result ppf = function
  | Skipped _ -> Format.fprintf ppf "This operation was skipped."
  | Failed (_, _errs) -> Format.fprintf ppf "This operation FAILED."
  | Applied op_res ->
      Format.fprintf
        ppf
        "This %s was successfully applied"
        (operation_name op_res) ;
      pp_operation_result ppf op_res
  | Backtracked (op_res, _errs) ->
      Format.fprintf
        ppf
        "This %s was BACKTRACKED, its expected effects were NOT applied."
        (operation_name op_res) ;
      pp_operation_result ppf op_res

let pp_manager_operation_contents_result ppf op_result =
  let pp_register_global_constant_result
      (Register_global_constant_result
        {balance_updates; consumed_gas; size_of_constant; global_address}) =
    (match balance_updates with
    | [] ->
        (* Not possible - register global constant operation always returns
           balance updates. *)
        assert false
    | balance_updates -> pp_balance_updates ppf balance_updates) ;
    pp_consumed_gas ppf consumed_gas ;
    pp_storage_size ppf size_of_constant ;
    Format.fprintf ppf "@,Global address: %a" Script_expr_hash.pp global_address
  in
  let pp_increase_paid_storage_result
      (Increase_paid_storage_result {consumed_gas; balance_updates}) =
    pp_balance_updates ppf balance_updates ;
    pp_consumed_gas ppf consumed_gas
  in
  let pp_transfer_ticket_result
      (Transfer_ticket_result
        {balance_updates; ticket_receipt; consumed_gas; paid_storage_size_diff})
      =
    pp_paid_storage_size_diff ppf paid_storage_size_diff ;
    pp_ticket_receipt ppf ticket_receipt ;
    pp_consumed_gas ppf consumed_gas ;
    pp_balance_updates ppf balance_updates
  in
  let pp_dal_publish_slot_header_result
      (Dal_publish_slot_header_result {slot_header; consumed_gas}) =
    pp_slot_header ppf slot_header ;
    pp_consumed_gas ppf consumed_gas
  in
  let pp_sc_rollup_originate_result
      (Sc_rollup_originate_result
        {address; genesis_commitment_hash; consumed_gas; size; balance_updates})
      =
    pp_consumed_gas ppf consumed_gas ;
    pp_storage_size ppf size ;
    Format.fprintf ppf "@,Address: %a" Sc_rollup.Address.pp address ;
    Format.fprintf
      ppf
      "@,Genesis commitment hash: %a"
      Sc_rollup.Commitment.Hash.pp
      genesis_commitment_hash ;
    pp_balance_updates ppf balance_updates
  in
  let pp_sc_rollup_add_messages_result
      (Sc_rollup_add_messages_result {consumed_gas}) =
    pp_consumed_gas ppf consumed_gas
  in
  let pp_sc_rollup_cement_result
      (Sc_rollup_cement_result {consumed_gas; inbox_level; commitment_hash}) =
    pp_consumed_gas ppf consumed_gas ;
    Format.fprintf
      ppf
      "@,Inbox level: %a@,Commitment hash: %a"
      Raw_level.pp
      inbox_level
      Sc_rollup.Commitment.Hash.pp
      commitment_hash
  in
  let pp_sc_rollup_publish_result
      (Sc_rollup_publish_result
        {consumed_gas; staked_hash; published_at_level; balance_updates}) =
    pp_consumed_gas ppf consumed_gas ;
    Format.fprintf
      ppf
      "@,Hash of commit: %a"
      Sc_rollup.Commitment.Hash.pp
      staked_hash ;
    Format.fprintf
      ppf
      "@,Commitment published at level: %a"
      Raw_level.pp
      published_at_level ;
    pp_balance_updates ppf balance_updates
  in
  let pp_sc_rollup_refute_result
      (Sc_rollup_refute_result {consumed_gas; game_status; balance_updates}) =
    pp_consumed_gas ppf consumed_gas ;
    Format.fprintf
      ppf
      "@,Refutation game status: %a"
      Sc_rollup.Game.pp_status
      game_status ;
    pp_balance_updates ppf balance_updates
  in
  let pp_sc_rollup_timeout_result
      (Sc_rollup_timeout_result {consumed_gas; game_status; balance_updates}) =
    pp_consumed_gas ppf consumed_gas ;
    Format.fprintf
      ppf
      "@,Refutation game status: %a"
      Sc_rollup.Game.pp_status
      game_status ;
    pp_balance_updates ppf balance_updates
  in
  let pp_sc_rollup_execute_outbox_message_result
      (Sc_rollup_execute_outbox_message_result
        {balance_updates; ticket_receipt; consumed_gas; paid_storage_size_diff})
      =
    pp_paid_storage_size_diff ppf paid_storage_size_diff ;
    pp_consumed_gas ppf consumed_gas ;
    pp_balance_updates ppf balance_updates ;
    pp_ticket_receipt ppf ticket_receipt
  in
  let pp_sc_rollup_recover_bond_result
      (Sc_rollup_recover_bond_result {balance_updates; consumed_gas}) =
    pp_balance_updates ppf balance_updates ;
    pp_consumed_gas ppf consumed_gas
  in
  let pp_zk_rollup_origination_result
      (Zk_rollup_origination_result
        {consumed_gas; originated_zk_rollup; storage_size; balance_updates}) =
    pp_consumed_gas ppf consumed_gas ;
    pp_storage_size ppf storage_size ;
    Format.fprintf ppf "@,Address: %a" Zk_rollup.Address.pp originated_zk_rollup ;
    pp_balance_updates ppf balance_updates
  in
  let pp_zk_rollup_publish_result
      (Zk_rollup_publish_result
        {balance_updates; consumed_gas; paid_storage_size_diff}) =
    pp_paid_storage_size_diff ppf paid_storage_size_diff ;
    pp_consumed_gas ppf consumed_gas ;
    pp_balance_updates ppf balance_updates
  in
  let pp_zk_rollup_update_result
      (Zk_rollup_update_result
        {balance_updates; consumed_gas; paid_storage_size_diff}) =
    pp_consumed_gas ppf consumed_gas ;
    pp_paid_storage_size_diff ppf paid_storage_size_diff ;
    pp_balance_updates ppf balance_updates
  in

  let manager_operation_name (type kind)
      (result : kind successful_manager_operation_result) =
    match result with
    | Reveal_result _ -> "revelation"
    | Transaction_result _ -> "transaction"
    | Origination_result _ -> "origination"
    | Delegation_result _ -> "delegation"
    | Register_global_constant_result _ -> "global constant registration"
    | Set_deposits_limit_result _ -> "deposits limit modification"
    | Update_consensus_key_result _ -> "consensus key update"
    | Increase_paid_storage_result _ -> "paid storage increase"
    | Transfer_ticket_result _ -> "tickets transfer"
    | Sc_rollup_originate_result _ -> "smart rollup origination"
    | Sc_rollup_add_messages_result _ -> "smart rollup messages submission"
    | Sc_rollup_cement_result _ -> "smart rollup commitment cementing"
    | Sc_rollup_publish_result _ -> "smart rollup commitment publishing"
    | Sc_rollup_refute_result _ -> "smart rollup refutation move"
    | Sc_rollup_timeout_result _ -> "smart rollup refutation timeout"
    | Sc_rollup_execute_outbox_message_result _ ->
        "smart output message execution"
    | Sc_rollup_recover_bond_result _ -> "smart rollup bond retrieval"
    | Dal_publish_slot_header_result _ ->
        "data availability slot header publishing"
    | Zk_rollup_origination_result _ -> "epoxy originate"
    | Zk_rollup_publish_result _ -> "epoxy publish"
    | Zk_rollup_update_result _ -> "epoxy update"
  in
  let pp_manager_operation_contents_result (type kind) ppf
      (result : kind successful_manager_operation_result) =
    match result with
    | Reveal_result {consumed_gas} -> pp_consumed_gas ppf consumed_gas
    | Delegation_result {consumed_gas} -> pp_consumed_gas ppf consumed_gas
    | Set_deposits_limit_result {consumed_gas} ->
        pp_consumed_gas ppf consumed_gas
    | Update_consensus_key_result {consumed_gas} ->
        pp_consumed_gas ppf consumed_gas
    | Transaction_result tx -> pp_transaction_result ppf tx
    | Origination_result op_res -> pp_origination_result ppf op_res
    | Register_global_constant_result _ as op ->
        pp_register_global_constant_result op
    | Increase_paid_storage_result _ as op -> pp_increase_paid_storage_result op
    | Transfer_ticket_result _ as op -> pp_transfer_ticket_result op
    | Sc_rollup_originate_result _ as op -> pp_sc_rollup_originate_result op
    | Sc_rollup_add_messages_result _ as op ->
        pp_sc_rollup_add_messages_result op
    | Sc_rollup_cement_result _ as op -> pp_sc_rollup_cement_result op
    | Sc_rollup_publish_result _ as op -> pp_sc_rollup_publish_result op
    | Sc_rollup_refute_result _ as op -> pp_sc_rollup_refute_result op
    | Sc_rollup_timeout_result _ as op -> pp_sc_rollup_timeout_result op
    | Sc_rollup_execute_outbox_message_result _ as op ->
        pp_sc_rollup_execute_outbox_message_result op
    | Sc_rollup_recover_bond_result _ as op ->
        pp_sc_rollup_recover_bond_result op
    | Dal_publish_slot_header_result _ as op ->
        pp_dal_publish_slot_header_result op
    | Zk_rollup_origination_result _ as op -> pp_zk_rollup_origination_result op
    | Zk_rollup_publish_result _ as op -> pp_zk_rollup_publish_result op
    | Zk_rollup_update_result _ as op -> pp_zk_rollup_update_result op
  in
  pp_operation_result
    ~operation_name:manager_operation_name
    pp_manager_operation_contents_result
    ppf
    op_result

let pp_internal_operation_and_result ppf (Internal_operation_result (op, res)) =
  let internal_operation_name (type kind) :
      kind successful_internal_operation_result -> string = function
    | ITransaction_result _ -> "transaction"
    | IOrigination_result _ -> "origination"
    | IDelegation_result _ -> "delegation"
    | IEvent_result _ -> "event"
  in
  let pp_internal_operation_result (type kind) ppf
      (result : kind successful_internal_operation_result) =
    match result with
    | ITransaction_result tx -> pp_transaction_result ppf tx
    | IOrigination_result op_res -> pp_origination_result ppf op_res
    | IDelegation_result {consumed_gas} | IEvent_result {consumed_gas} ->
        pp_consumed_gas ppf consumed_gas
  in
  Format.fprintf
    ppf
    "@[<v 2>%a@,%a@]"
    pp_internal_operation
    (Internal_operation op)
    (pp_operation_result
       ~operation_name:internal_operation_name
       pp_internal_operation_result)
    res

let pp_internal_operation_results_list ppf = function
  | [] -> ()
  | _ :: _ as internal_operation_results ->
      Format.fprintf
        ppf
        "@,@[<v 2>Internal operations:@,%a@]"
        (Format.pp_print_list pp_internal_operation_and_result)
        internal_operation_results

let pp_manager_operation_result ppf
    ( Manager_operation
        {source; fee; operation; counter; gas_limit; storage_limit},
      Manager_operation_result
        {balance_updates; operation_result; internal_operation_results} ) =
  Format.fprintf ppf "@[<v 2>Manager signed operations:" ;
  Format.fprintf ppf "@,From: %a" Signature.Public_key_hash.pp source ;
  Format.fprintf ppf "@,Fee to the baker: %s%a" tez_sym Tez.pp fee ;
  Format.fprintf ppf "@,Expected counter: %a" Manager_counter.pp counter ;
  Format.fprintf ppf "@,Gas limit: %a" Gas.Arith.pp_integral gas_limit ;
  Format.fprintf ppf "@,Storage limit: %a bytes" Z.pp_print storage_limit ;
  pp_balance_updates ppf balance_updates ;
  Format.fprintf
    ppf
    "@,@[<v 2>%a@,%a%a@]"
    (pp_manager_operation_content (Contract.Implicit source))
    operation
    pp_manager_operation_contents_result
    operation_result
    pp_internal_operation_results_list
    internal_operation_results ;
  Format.fprintf ppf "@]"

let pp_contents_and_result :
    type kind. Format.formatter -> kind contents * kind contents_result -> unit
    =
 fun ppf -> function
  | Seed_nonce_revelation {level; nonce}, Seed_nonce_revelation_result bus ->
      Format.fprintf
        ppf
        "@[<v 2>Seed nonce revelation:@,\
         Level: %a@,\
         Nonce (hash): %a@,\
         Balance updates:@,\
         %a@]"
        Raw_level.pp
        level
        Nonce_hash.pp
        (Nonce.hash nonce)
        pp_balance_updates
        bus
  | Vdf_revelation {solution}, Vdf_revelation_result bus ->
      Format.fprintf
        ppf
        "@[<v 2>Vdf revelation:@,Solution: %a@,Balance updates:@,%a@]"
        Seed.pp_solution
        solution
        pp_balance_updates
        bus
  | Double_baking_evidence {bh1; bh2}, Double_baking_evidence_result bus ->
      Format.fprintf
        ppf
        "@[<v 2>Double baking evidence:@,\
         Exhibit A: %a@,\
         Exhibit B: %a@,\
         Balance updates:@,\
         %a@]"
        Block_hash.pp
        (Block_header.hash bh1)
        Block_hash.pp
        (Block_header.hash bh2)
        pp_balance_updates
        bus
  | ( Preendorsement {level; _},
      Preendorsement_result
        {balance_updates; delegate; consensus_key; preendorsement_power} ) ->
      Format.fprintf
        ppf
        "@[<v 2>Preendorsement:@,\
         Level: %a@,\
         Balance updates:%a@,\
         Delegate: %a@,\
         Preendorsement Power: %d@]"
        Raw_level.pp
        level
        pp_balance_updates
        balance_updates
        Consensus_key.pp
        {delegate; consensus_pkh = consensus_key}
        preendorsement_power
  | ( Endorsement {level; _},
      Endorsement_result
        {balance_updates; delegate; consensus_key; endorsement_power} ) ->
      Format.fprintf
        ppf
        "@[<v 2>Endorsement:@,\
         Level: %a@,\
         Balance updates:%a@,\
         Delegate: %a@,\
         Endorsement power: %d@]"
        Raw_level.pp
        level
        pp_balance_updates
        balance_updates
        Consensus_key.pp
        {delegate; consensus_pkh = consensus_key}
        endorsement_power
  | Dal_attestation _, Dal_attestation_result {delegate} ->
      Format.fprintf
        ppf
        "@[<v 2>Slot attestation:@,Delegate: %a@]"
        Signature.Public_key_hash.pp
        delegate
  | ( Double_endorsement_evidence {op1; op2},
      Double_endorsement_evidence_result bus ) ->
      Format.fprintf
        ppf
        "@[<v 2>Double endorsement evidence:@,\
         Exhibit A: %a@,\
         Exhibit B: %a@,\
         Balance updates:@,\
        \  %a@]"
        Operation_hash.pp
        (Operation.hash op1)
        Operation_hash.pp
        (Operation.hash op2)
        pp_balance_updates
        bus
  | ( Double_preendorsement_evidence {op1; op2},
      Double_preendorsement_evidence_result bus ) ->
      Format.fprintf
        ppf
        "@[<v 2>Double preendorsement evidence:@,\
         Exhibit A: %a@,\
         Exhibit B: %a@,\
         Balance updates:@,\
        \  %a@]"
        Operation_hash.pp
        (Operation.hash op1)
        Operation_hash.pp
        (Operation.hash op2)
        pp_balance_updates
        bus
  | Activate_account {id; _}, Activate_account_result bus ->
      Format.fprintf
        ppf
        "@[<v 2>Genesis account activation:@,\
         Account: %a@,\
         Balance updates:@,\
        \  %a@]"
        Signature.Ed25519.Public_key_hash.pp
        id
        pp_balance_updates
        bus
  | Proposals {source; period; proposals}, Proposals_result ->
      Format.fprintf
        ppf
        "@[<v 2>Proposals:@,From: %a@,Period: %ld@,Protocols:@,  @[<v 0>%a@]@]"
        Signature.Public_key_hash.pp
        source
        period
        (Format.pp_print_list Protocol_hash.pp)
        proposals
  | Ballot {source; period; proposal; ballot}, Ballot_result ->
      Format.fprintf
        ppf
        "@[<v 2>Ballot:@,From: %a@,Period: %ld@,Protocol: %a@,Vote: %a@]"
        Signature.Public_key_hash.pp
        source
        period
        Protocol_hash.pp
        proposal
        Data_encoding.Json.pp
        (Data_encoding.Json.construct Vote.ballot_encoding ballot)
  | ( Drain_delegate {consensus_key; delegate; destination},
      Drain_delegate_result {balance_updates; allocated_destination_contract} )
    ->
      Format.fprintf
        ppf
        "@[<v 2>Drain delegate:@,\
         Consensus key hash: %a@,\
         Delegate: %a@,\
         Destination: %a%s%a@]"
        Signature.Public_key_hash.pp
        consensus_key
        Signature.Public_key_hash.pp
        delegate
        Signature.Public_key_hash.pp
        destination
        (if allocated_destination_contract then " (allocated)" else "")
        pp_balance_updates
        balance_updates
  | Failing_noop _arbitrary, _ ->
      (* the Failing_noop operation always fails and can't have result *)
      .
  | (Manager_operation _ as op), (Manager_operation_result _ as res) ->
      pp_manager_operation_result ppf (op, res)

let rec pp_contents_and_result_list :
    type kind. Format.formatter -> kind contents_and_result_list -> unit =
 fun ppf -> function
  | Single_and_result (op, res) -> pp_contents_and_result ppf (op, res)
  | Cons_and_result
      ((Manager_operation _ as op), (Manager_operation_result _ as res), rest)
    ->
      Format.fprintf
        ppf
        "%a@,%a"
        pp_manager_operation_result
        (op, res)
        pp_contents_and_result_list
        rest

let pp_operation_result ppf
    ((op, res) : 'kind contents_list * 'kind contents_result_list) =
  let contents_and_result_list = Apply_results.pack_contents_list op res in
  Format.fprintf
    ppf
    "@[<v 0>%a@]@."
    pp_contents_and_result_list
    contents_and_result_list
