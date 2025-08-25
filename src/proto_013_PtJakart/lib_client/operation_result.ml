(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let pp_manager_operation_content (type kind) source internal pp_result ppf
    ((operation, result) : kind manager_operation * _) =
  Format.fprintf ppf "@[<v 0>" ;
  (match operation with
  | Transaction {destination; amount; parameters; entrypoint} ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,Amount: %s%a@,From: %a@,To: %a"
        (if internal then "Internal transaction" else "Transaction")
        Client_proto_args.tez_sym
        Tez.pp
        amount
        Contract.pp
        source
        Destination.pp
        destination ;
      if not (Entrypoint.is_default entrypoint) then
        Format.fprintf ppf "@,Entrypoint: %a" Entrypoint.pp entrypoint ;
      (if not (Script_repr.is_unit_parameter parameters) then
         let expr =
           WithExceptions.Option.to_exn
             ~none:(Failure "ill-serialized argument")
             (Data_encoding.force_decode parameters)
         in
         Format.fprintf
           ppf
           "@,Parameter: @[<v 0>%a@]"
           Michelson_v1_printer.print_expr
           expr) ;
      pp_result ppf result ;
      Format.fprintf ppf "@]"
  | Origination {delegate; credit; script = {code; storage}} ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,From: %a@,Credit: %s%a"
        (if internal then "Internal origination" else "Origination")
        Contract.pp
        source
        Client_proto_args.tez_sym
        Tez.pp
        credit ;
      let code =
        WithExceptions.Option.to_exn
          ~none:(Failure "ill-serialized code")
          (Data_encoding.force_decode code)
      and storage =
        WithExceptions.Option.to_exn
          ~none:(Failure "ill-serialized storage")
          (Data_encoding.force_decode storage)
      in
      let {Michelson_v1_parser.source; _} =
        Michelson_v1_printer.unparse_toplevel code
      in
      Format.fprintf
        ppf
        "@,@[<hv 2>Script:@ @[<h>%a@]@,@[<hv 2>Initial storage:@ %a@]"
        Format.pp_print_text
        source
        Michelson_v1_printer.print_expr
        storage ;
      (match delegate with
      | None -> Format.fprintf ppf "@,No delegate for this contract"
      | Some delegate ->
          Format.fprintf
            ppf
            "@,Delegate: %a"
            Signature.V0.Public_key_hash.pp
            delegate) ;
      pp_result ppf result ;
      Format.fprintf ppf "@]"
  | Reveal key ->
      Format.fprintf
        ppf
        "@[<v 2>%s of manager public key:@,Contract: %a@,Key: %a%a@]"
        (if internal then "Internal revelation" else "Revelation")
        Contract.pp
        source
        Signature.V0.Public_key.pp
        key
        pp_result
        result
  | Delegation None ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,Contract: %a@,To: nobody%a@]"
        (if internal then "Internal Delegation" else "Delegation")
        Contract.pp
        source
        pp_result
        result
  | Delegation (Some delegate) ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,Contract: %a@,To: %a%a@]"
        (if internal then "Internal Delegation" else "Delegation")
        Contract.pp
        source
        Signature.V0.Public_key_hash.pp
        delegate
        pp_result
        result
  | Register_global_constant {value = lazy_value} ->
      let value =
        WithExceptions.Option.to_exn
          ~none:(Failure "ill-serialized value")
          (Data_encoding.force_decode lazy_value)
      in
      Format.fprintf
        ppf
        "Register Global:@,@[<v 2>  Value: %a%a@]"
        Michelson_v1_printer.print_expr
        value
        pp_result
        result
  | Set_deposits_limit None ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,Delegate: %a@,Unlimited deposits%a@]"
        (if internal then "Internal set deposits limit"
         else "Set deposits limit")
        Contract.pp
        source
        pp_result
        result
  | Set_deposits_limit (Some limit) ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,Delegate: %a@,Limit: %a%a@]"
        (if internal then "Internal set deposits limit"
         else "Set deposits limit")
        Contract.pp
        source
        Tez.pp
        limit
        pp_result
        result
  | Tx_rollup_origination ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,From: %a%a@]"
        (if internal then "Internal tx rollup origination"
         else "Tx rollup origination")
        Contract.pp
        source
        pp_result
        result
  | Tx_rollup_submit_batch {tx_rollup; content; burn_limit = _} ->
      Format.fprintf
        ppf
        "@[<v 2>%s:%a, %d bytes, From: %a%a@]"
        (if internal then "Internal tx rollup transaction"
         else "Tx rollup transaction")
        Tx_rollup.pp
        tx_rollup
        (String.length content)
        Contract.pp
        source
        pp_result
        result
  | Tx_rollup_commit {tx_rollup; commitment} ->
      Format.fprintf
        ppf
        "@[<v 2>%s:%a, %a@,From: %a%a@]"
        (if internal then "Internal tx rollup commitment"
         else "Tx rollup commitment")
        Tx_rollup.pp
        tx_rollup
        Tx_rollup_commitment.Full.pp
        commitment
        Contract.pp
        source
        pp_result
        result
  | Tx_rollup_return_bond {tx_rollup} ->
      Format.fprintf
        ppf
        "@[<v 2>%s:%a @,From: %a%a@]"
        (if internal then "Internal tx rollup return commitment bond"
         else "Tx rollup return commitment bond")
        Tx_rollup.pp
        tx_rollup
        Contract.pp
        source
        pp_result
        result
  | Tx_rollup_finalize_commitment {tx_rollup} ->
      Format.fprintf
        ppf
        "@[<v >%s:%a @,From: %a%a@]"
        (if internal then "Internal tx rollup finalize commitment"
         else "Tx rollup finalize commitment")
        Tx_rollup.pp
        tx_rollup
        Contract.pp
        source
        pp_result
        result
  | Tx_rollup_remove_commitment {tx_rollup; _} ->
      Format.fprintf
        ppf
        "@[<v 2>%s:%a @,From: %a%a@]"
        (if internal then "Internal tx rollup remove commitment"
         else "Tx rollup remove commitment")
        Tx_rollup.pp
        tx_rollup
        Contract.pp
        source
        pp_result
        result
  | Tx_rollup_rejection {tx_rollup; _} ->
      (* FIXME/TORU *)
      Format.fprintf
        ppf
        "@[<v 2>%s:%a @,From: %a%a@]"
        (if internal then "Internal tx rollup rejection"
         else "Tx rollup rejection")
        Tx_rollup.pp
        tx_rollup
        Contract.pp
        source
        pp_result
        result
  | Tx_rollup_dispatch_tickets {tx_rollup; _} ->
      Format.fprintf
        ppf
        "@[<v 2>%s:%a@,From: %a%a@]"
        (if internal then "Internal tx rollup dispatch tickets"
         else "Tx rollup dispatch tickets")
        Tx_rollup.pp
        tx_rollup
        Contract.pp
        source
        pp_result
        result
  | Transfer_ticket _ ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,From: %a%a@]"
        (if internal then "Internal transfer ticket" else "Transfer ticket")
        Contract.pp
        source
        pp_result
        result
  | Sc_rollup_originate {kind; boot_sector} ->
      let (module R : Sc_rollups.PVM.S) = Sc_rollups.of_kind kind in
      Format.fprintf
        ppf
        "@[<v 2>Originate smart contract rollup of kind %s with boot sector \
         '%a'%a@]"
        R.name
        R.pp_boot_sector
        boot_sector
        pp_result
        result
  | Sc_rollup_add_messages {rollup; messages = _} ->
      Format.fprintf
        ppf
        "@[<v 2>Add a message to the inbox of the smart contract rollup at \
         address %a%a@]"
        Sc_rollup.Address.pp
        rollup
        pp_result
        result
  | Sc_rollup_cement {rollup; commitment} ->
      Format.fprintf
        ppf
        "@[<v 2>Cement the commitment %a in the smart contract rollup at \
         address %a%a@]"
        Sc_rollup.Commitment_hash.pp
        commitment
        Sc_rollup.Address.pp
        rollup
        pp_result
        result
  | Sc_rollup_publish {rollup; commitment} ->
      Format.fprintf
        ppf
        "@[<v 2>Publish commitment %a in the smart contract rollup at address \
         %a%a@]"
        Sc_rollup.Commitment.pp
        commitment
        Sc_rollup.Address.pp
        rollup
        pp_result
        result) ;

  Format.fprintf ppf "@]"

let pp_balance_updates ppf = function
  | [] -> ()
  | balance_updates ->
      let open Receipt in
      (* For dry runs, the baker's key is zero
         (tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU). Instead of printing this
         key hash, we want to make the result more informative. *)
      let pp_baker ppf baker =
        if
          Signature.V0.Public_key_hash.equal
            baker
            Signature.V0.Public_key_hash.zero
        then Format.fprintf ppf "the baker who will include this operation"
        else Signature.V0.Public_key_hash.pp ppf baker
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
              | Double_signing_evidence_rewards ->
                  "double signing evidence rewards"
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
                  Format.asprintf
                    "lost endorsing rewards(%a%s)"
                    pp_baker
                    pkh
                    reason
              | Liquidity_baking_subsidies -> "liquidity baking subsidies"
              | Burned -> "burned"
              | Commitments bpkh ->
                  Format.asprintf
                    "commitment(%a)"
                    Blinded_public_key_hash.pp
                    bpkh
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
              | Tx_rollup_rejection_rewards -> "tx rollup rejection rewards"
              | Tx_rollup_rejection_punishments ->
                  "tx rollup rejection punishments"
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
        | Credited amount ->
            Format.fprintf ppf "+%s%a" Client_proto_args.tez_sym Tez.pp amount
        | Debited amount ->
            Format.fprintf ppf "-%s%a" Client_proto_args.tez_sym Tez.pp amount
      in
      let pp_one ppf (balance, update) =
        let to_fill = column_size + 3 - String.length balance in
        let filler = String.make to_fill '.' in
        Format.fprintf ppf "%s %s %a" balance filler pp_update update
      in
      Format.fprintf
        ppf
        "@[<v 0>%a@]"
        (Format.pp_print_list pp_one)
        balance_updates

let pp_balance_updates_opt ppf balance_updates =
  match balance_updates with
  | [] -> ()
  | balance_updates ->
      Format.fprintf
        ppf
        "@,Balance updates:@,  %a"
        pp_balance_updates
        balance_updates

let pp_manager_operation_contents_and_result ppf
    ( Manager_operation
        {source; fee; operation; counter; gas_limit; storage_limit},
      Manager_operation_result
        {balance_updates; operation_result; internal_operation_results} ) =
  let pp_lazy_storage_diff = function
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
  in
  let pp_transaction_result = function
    | Transaction_to_contract_result
        {
          balance_updates;
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
              (Format.pp_print_list Contract.pp)
              contracts) ;
        (match storage with
        | None -> ()
        | Some expr ->
            Format.fprintf
              ppf
              "@,@[<hv 2>Updated storage:@ %a@]"
              Michelson_v1_printer.print_expr
              expr) ;
        pp_lazy_storage_diff lazy_storage_diff ;
        if storage_size <> Z.zero then
          Format.fprintf
            ppf
            "@,Storage size: %s bytes"
            (Z.to_string storage_size) ;
        if paid_storage_size_diff <> Z.zero then
          Format.fprintf
            ppf
            "@,Paid storage size diff: %s bytes"
            (Z.to_string paid_storage_size_diff) ;
        Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
        pp_balance_updates_opt ppf balance_updates
    | Transaction_to_tx_rollup_result
        {balance_updates; consumed_gas; ticket_hash; paid_storage_size_diff} ->
        Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
        pp_balance_updates_opt ppf balance_updates ;
        Format.fprintf ppf "@,Ticket hash: %a" Ticket_hash.pp ticket_hash ;
        Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
        if paid_storage_size_diff <> Z.zero then
          Format.fprintf
            ppf
            "@,Paid storage size diff: %s bytes"
            (Z.to_string paid_storage_size_diff)
  in

  let pp_origination_result
      (Origination_result
         {
           lazy_storage_diff;
           balance_updates;
           consumed_gas;
           originated_contracts;
           storage_size;
           paid_storage_size_diff;
         }) =
    (match originated_contracts with
    | [] -> ()
    | contracts ->
        Format.fprintf
          ppf
          "@,@[<v 2>Originated contracts:@,%a@]"
          (Format.pp_print_list Contract.pp)
          contracts) ;
    if storage_size <> Z.zero then
      Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string storage_size) ;
    pp_lazy_storage_diff lazy_storage_diff ;
    if paid_storage_size_diff <> Z.zero then
      Format.fprintf
        ppf
        "@,Paid storage size diff: %s bytes"
        (Z.to_string paid_storage_size_diff) ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    pp_balance_updates_opt ppf balance_updates
  in
  let pp_register_global_constant_result
      (Register_global_constant_result
         {balance_updates; consumed_gas; size_of_constant; global_address}) =
    (match balance_updates with
    | [] ->
        (* Not possible - register global constant operation always returns
           balance updates. *)
        assert false
    | balance_updates -> pp_balance_updates_opt ppf balance_updates) ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string size_of_constant) ;
    Format.fprintf ppf "@,Global address: %a" Script_expr_hash.pp global_address
  in
  let pp_tx_rollup_result
      (Tx_rollup_origination_result
         {balance_updates; consumed_gas; originated_tx_rollup}) =
    Format.fprintf
      ppf
      "@,Balance updates:@,  %a"
      pp_balance_updates
      balance_updates ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    Format.fprintf
      ppf
      "@,Originated tx rollup: %a"
      Tx_rollup.pp
      originated_tx_rollup
  in
  let pp_tx_rollup_submit_batch_result
      (Tx_rollup_submit_batch_result
         {balance_updates; consumed_gas; paid_storage_size_diff}) =
    Format.fprintf
      ppf
      "@,Balance updates:@,  %a"
      pp_balance_updates
      balance_updates ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    if paid_storage_size_diff <> Z.zero then
      Format.fprintf
        ppf
        "@,Paid storage size diff: %s bytes"
        (Z.to_string paid_storage_size_diff)
  in
  let pp_tx_rollup_commit_result
      (Tx_rollup_commit_result {balance_updates; consumed_gas}) =
    Format.fprintf
      ppf
      "@,Balance updates:@,  %a"
      pp_balance_updates
      balance_updates ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas
  in
  let pp_tx_rollup_return_bond_result
      (Tx_rollup_return_bond_result {balance_updates; consumed_gas}) =
    Format.fprintf
      ppf
      "@,Balance updates:@,  %a"
      pp_balance_updates
      balance_updates ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas
  in
  let pp_tx_rollup_finalize_commitment_result
      (Tx_rollup_finalize_commitment_result
         {balance_updates; consumed_gas; level}) =
    Format.fprintf
      ppf
      "@,Balance updates:@,  %a"
      pp_balance_updates
      balance_updates ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    Format.fprintf ppf "@finalized level:@,  %a" Tx_rollup_level.pp level
  in
  let pp_tx_rollup_remove_commitment_result
      (Tx_rollup_remove_commitment_result {balance_updates; consumed_gas; level})
      =
    Format.fprintf
      ppf
      "@,Balance updates:@,  %a"
      pp_balance_updates
      balance_updates ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    Format.fprintf ppf "@finalized level:@,  %a" Tx_rollup_level.pp level
  in
  let pp_tx_rollup_rejection_result
      (Tx_rollup_rejection_result {balance_updates; consumed_gas}) =
    Format.fprintf
      ppf
      "@,Balance updates:@,  %a"
      pp_balance_updates
      balance_updates ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas
  in
  let pp_tx_rollup_dispatch_tickets_result
      (Tx_rollup_dispatch_tickets_result
         {balance_updates; consumed_gas; paid_storage_size_diff}) =
    if paid_storage_size_diff <> Z.zero then
      Format.fprintf
        ppf
        "@,Paid storage size diff: %s bytes"
        (Z.to_string paid_storage_size_diff) ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    pp_balance_updates_opt ppf balance_updates
  in
  let pp_transfer_ticket_result
      (Transfer_ticket_result
         {balance_updates; consumed_gas; paid_storage_size_diff}) =
    if paid_storage_size_diff <> Z.zero then
      Format.fprintf
        ppf
        "@,Paid storage size diff: %s bytes"
        (Z.to_string paid_storage_size_diff) ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    pp_balance_updates_opt ppf balance_updates
  in
  let pp_sc_rollup_originate_result
      (Sc_rollup_originate_result {address; consumed_gas; size; balance_updates})
      =
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string size) ;
    Format.fprintf ppf "@,Address: %a" Sc_rollup.Address.pp address ;
    pp_balance_updates_opt ppf balance_updates
  in
  let pp_sc_rollup_add_messages_result
      (Sc_rollup_add_messages_result {consumed_gas; inbox_after}) =
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    Format.fprintf
      ppf
      "@,Resulting inbox state: %a"
      Sc_rollup.Inbox.pp
      inbox_after
  in
  let pp_sc_rollup_cement_result (Sc_rollup_cement_result {consumed_gas}) =
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas
  in
  let pp_sc_rollup_publish_result
      (Sc_rollup_publish_result {consumed_gas; staked_hash}) =
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    Format.fprintf
      ppf
      "@,Hash of commit: %a"
      Sc_rollup.Commitment_hash.pp
      staked_hash
  in
  let pp_result (type kind) ppf (result : kind manager_operation_result) =
    Format.fprintf ppf "@," ;
    match result with
    | Skipped _ -> Format.fprintf ppf "This operation was skipped"
    | Failed (_, _errs) -> Format.fprintf ppf "This operation FAILED."
    | Applied (Reveal_result {consumed_gas}) ->
        Format.fprintf ppf "This revelation was successfully applied" ;
        Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas
    | Backtracked (Reveal_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>This revelation was BACKTRACKED, its expected effects were \
           NOT applied.@]"
    | Applied (Delegation_result {consumed_gas}) ->
        Format.fprintf ppf "This delegation was successfully applied" ;
        Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas
    | Backtracked (Delegation_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>This delegation was BACKTRACKED, its expected effects were \
           NOT applied.@]"
    | Applied (Set_deposits_limit_result {consumed_gas}) ->
        Format.fprintf ppf "The deposits limit was successfully set" ;
        Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas
    | Backtracked (Set_deposits_limit_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>This deposits limit modification was BACKTRACKED, its \
           expected effects were NOT applied.@]"
    | Applied (Transaction_result tx) ->
        Format.fprintf ppf "This transaction was successfully applied" ;
        pp_transaction_result tx
    | Backtracked (Transaction_result tx, _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This transaction was BACKTRACKED, its expected effects (as \
           follow) were NOT applied.@]" ;
        pp_transaction_result tx
    | Applied (Origination_result _ as op) ->
        Format.fprintf ppf "This origination was successfully applied" ;
        pp_origination_result op
    | Backtracked ((Origination_result _ as op), _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This origination was BACKTRACKED, its expected effects (as \
           follow) were NOT applied.@]" ;
        pp_origination_result op
    | Applied (Register_global_constant_result _ as op) ->
        Format.fprintf
          ppf
          "This global constant registration was successfully applied" ;
        pp_register_global_constant_result op
    | Backtracked ((Register_global_constant_result _ as op), _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This registration of a global constant was BACKTRACKED, its \
           expected effects (as follow) were NOT applied.@]" ;
        pp_register_global_constant_result op
    | Applied (Tx_rollup_origination_result _ as op) ->
        Format.fprintf
          ppf
          "This tx rollup origination operation was successfully applied" ;
        pp_tx_rollup_result op
    | Backtracked ((Tx_rollup_origination_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This rollup operation was BACKTRACKED, its expected effects \
           (as follow) were NOT applied.@]" ;
        pp_tx_rollup_result op
    | Applied (Tx_rollup_submit_batch_result _ as op) ->
        Format.fprintf
          ppf
          "This tx rollup submit operation was successfully applied" ;
        pp_tx_rollup_submit_batch_result op
    | Backtracked ((Tx_rollup_submit_batch_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This rollup submit operation was BACKTRACKED, its expected \
           effects (as follow) were NOT applied.@]" ;
        pp_tx_rollup_submit_batch_result op
    | Applied (Tx_rollup_commit_result _ as op) ->
        Format.fprintf
          ppf
          "This tx rollup commit operation was successfully applied" ;
        pp_tx_rollup_commit_result op
    | Backtracked ((Tx_rollup_commit_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This tx rollup commit operation was BACKTRACKED, its \
           expected effects (as follow) were NOT applied.@]" ;
        pp_tx_rollup_commit_result op
    | Applied (Tx_rollup_return_bond_result _ as op) ->
        Format.fprintf
          ppf
          "This tx rollup return commitment bond operation was successfully \
           applied" ;
        pp_tx_rollup_return_bond_result op
    | Backtracked ((Tx_rollup_return_bond_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This tx rollup return commitment bond operation was \
           BACKTRACKED, its expected effects (as follow) were NOT applied.@]" ;
        pp_tx_rollup_return_bond_result op
    | Applied (Tx_rollup_finalize_commitment_result _ as op) ->
        Format.fprintf
          ppf
          "This tx rollup finalize operation was successfully applied" ;
        pp_tx_rollup_finalize_commitment_result op
    | Backtracked ((Tx_rollup_finalize_commitment_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This tx rollup finalize operation was BACKTRACKED, its \
           expected effects (as follow) were NOT applied.@]" ;
        pp_tx_rollup_finalize_commitment_result op
    | Applied (Tx_rollup_remove_commitment_result _ as op) ->
        Format.fprintf
          ppf
          "This tx rollup remove operation was successfully applied" ;
        pp_tx_rollup_remove_commitment_result op
    | Backtracked ((Tx_rollup_remove_commitment_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This tx rollup remove operation was BACKTRACKED, its \
           expected effects (as follow) were NOT applied.@]" ;
        pp_tx_rollup_remove_commitment_result op
    | Applied (Tx_rollup_rejection_result _ as op) ->
        Format.fprintf
          ppf
          "This tx rollup rejection operation was successfully applied" ;
        pp_tx_rollup_rejection_result op
    | Backtracked ((Tx_rollup_rejection_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This tx rollup rejection operation was BACKTRACKED, its \
           expected effects (as follow) were NOT applied.@]" ;
        pp_tx_rollup_rejection_result op
    | Applied (Tx_rollup_dispatch_tickets_result _ as op) ->
        Format.fprintf
          ppf
          "This tx rollup reveal_withdrawals operation was successfully applied" ;
        pp_tx_rollup_dispatch_tickets_result op
    | Backtracked ((Tx_rollup_dispatch_tickets_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This tx rollup reveal_withdrawals rollup operation was \
           BACKTRACKED, its expected effects (as follow) were NOT applied.@]" ;
        pp_tx_rollup_dispatch_tickets_result op
    | Applied (Transfer_ticket_result _ as op) ->
        Format.fprintf
          ppf
          "This transfer ticket operation was successfully applied" ;
        pp_transfer_ticket_result op
    | Backtracked ((Transfer_ticket_result _ as op), _err) ->
        Format.fprintf
          ppf
          "@[<v 0>This transfer ticket operation was BACKTRACKED, its expected \
           effects (as follow) were NOT applied.@]" ;
        pp_transfer_ticket_result op
    | Applied (Sc_rollup_originate_result _ as op) ->
        Format.fprintf
          ppf
          "This smart contract rollup origination was successfully applied" ;
        pp_sc_rollup_originate_result op
    | Backtracked ((Sc_rollup_originate_result _ as op), _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This rollup origination was BACKTRACKED, its expected \
           effects (as follow) were NOT applied.@]" ;
        pp_sc_rollup_originate_result op
    | Applied (Sc_rollup_add_messages_result _ as op) ->
        Format.fprintf
          ppf
          "This operation sending a message to a smart contract rollup was \
           successfully applied" ;
        pp_sc_rollup_add_messages_result op
    | Backtracked ((Sc_rollup_add_messages_result _ as op), _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This operation sending a message to a smart contract rollup \
           was BACKTRACKED, its expected effects (as follow) were NOT \
           applied.@]" ;
        pp_sc_rollup_add_messages_result op
    | Applied (Sc_rollup_cement_result _ as op) ->
        Format.fprintf
          ppf
          "This operation cementing a commitment on a smart contract rollup \
           was successfully applied" ;
        pp_sc_rollup_cement_result op
    | Backtracked ((Sc_rollup_cement_result _ as op), _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This operation cementing a commitment on a smart contract \
           rollup was BACKTRACKED, its expected effects (as follow) were NOT \
           applied.@]" ;
        pp_sc_rollup_cement_result op
    | Applied (Sc_rollup_publish_result _ as op) ->
        Format.fprintf
          ppf
          "This operation publishing a commitment on a smart contract rollup \
           was successfully applied" ;
        pp_sc_rollup_publish_result op
    | Backtracked ((Sc_rollup_publish_result _ as op), _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This operation publishing a commitment on a smart contract \
           rollup was BACKTRACKED, its expected effects (as follow) were NOT \
           applied.@]" ;
        pp_sc_rollup_publish_result op
  in

  Format.fprintf
    ppf
    "@[<v 0>@[<v 2>Manager signed operations:@,\
     From: %a@,\
     Fee to the baker: %s%a@,\
     Expected counter: %s@,\
     Gas limit: %a@,\
     Storage limit: %s bytes"
    Signature.V0.Public_key_hash.pp
    source
    Client_proto_args.tez_sym
    Tez.pp
    fee
    (Z.to_string counter)
    Gas.Arith.pp_integral
    gas_limit
    (Z.to_string storage_limit) ;
  pp_balance_updates_opt ppf balance_updates ;
  Format.fprintf
    ppf
    "@,%a"
    (pp_manager_operation_content
       (Contract.implicit_contract source)
       false
       pp_result)
    (operation, operation_result) ;
  (match internal_operation_results with
  | [] -> ()
  | _ :: _ ->
      Format.fprintf
        ppf
        "@,@[<v 2>Internal operations:@ %a@]"
        (Format.pp_print_list
           (fun ppf (Internal_manager_operation_result (op, res)) ->
             let operation =
               manager_operation_of_internal_operation op.operation
             in
             pp_manager_operation_content
               op.source
               false
               pp_result
               ppf
               (operation, res)))
        internal_operation_results) ;
  Format.fprintf ppf "@]"

let rec pp_contents_and_result_list : type kind.
    Format.formatter -> kind contents_and_result_list -> unit =
 fun ppf -> function
  | Single_and_result
      (Seed_nonce_revelation {level; nonce}, Seed_nonce_revelation_result bus)
    ->
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
  | Single_and_result
      (Double_baking_evidence {bh1; bh2}, Double_baking_evidence_result bus) ->
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
  | Single_and_result
      ( Preendorsement {level; _},
        Preendorsement_result {balance_updates; delegate; preendorsement_power}
      ) ->
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
        Signature.V0.Public_key_hash.pp
        delegate
        preendorsement_power
  | Single_and_result
      ( Endorsement {level; _},
        Endorsement_result {balance_updates; delegate; endorsement_power} ) ->
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
        Signature.V0.Public_key_hash.pp
        delegate
        endorsement_power
  | Single_and_result
      ( Double_endorsement_evidence {op1; op2},
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
  | Single_and_result
      ( Double_preendorsement_evidence {op1; op2},
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
  | Single_and_result (Activate_account {id; _}, Activate_account_result bus) ->
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
  | Single_and_result (Proposals {source; period; proposals}, Proposals_result)
    ->
      Format.fprintf
        ppf
        "@[<v 2>Proposals:@,From: %a@,Period: %ld@,Protocols:@,  @[<v 0>%a@]@]"
        Signature.V0.Public_key_hash.pp
        source
        period
        (Format.pp_print_list Protocol_hash.pp)
        proposals
  | Single_and_result (Ballot {source; period; proposal; ballot}, Ballot_result)
    ->
      Format.fprintf
        ppf
        "@[<v 2>Ballot:@,From: %a@,Period: %ld@,Protocol: %a@,Vote: %a@]"
        Signature.V0.Public_key_hash.pp
        source
        period
        Protocol_hash.pp
        proposal
        Data_encoding.Json.pp
        (Data_encoding.Json.construct Vote.ballot_encoding ballot)
  | Single_and_result (Failing_noop _arbitrary, _) ->
      (* the Failing_noop operation always fails and can't have result *)
      .
  | Single_and_result
      ((Manager_operation _ as op), (Manager_operation_result _ as res)) ->
      Format.fprintf ppf "%a" pp_manager_operation_contents_and_result (op, res)
  | Cons_and_result
      ((Manager_operation _ as op), (Manager_operation_result _ as res), rest)
    ->
      Format.fprintf
        ppf
        "%a@\n%a"
        pp_manager_operation_contents_and_result
        (op, res)
        pp_contents_and_result_list
        rest

let pp_operation_result ppf
    ((op, res) : 'kind contents_list * 'kind contents_result_list) =
  Format.fprintf ppf "@[<v 0>" ;
  let contents_and_result_list = Apply_results.pack_contents_list op res in
  pp_contents_and_result_list ppf contents_and_result_list ;
  Format.fprintf ppf "@]@."

let pp_internal_operation_result ppf (Apply_results.Internal_contents op) =
  let operation = manager_operation_of_internal_operation op.operation in
  pp_manager_operation_content
    op.source
    true
    (fun _ppf () -> ())
    ppf
    (operation, ())

let pp_internal_operation ppf (Script_typed_ir.Internal_operation op) =
  let op = contents_of_internal_operation op in
  pp_internal_operation_result ppf (Internal_contents op)
