(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

let pp_origination ppf ~pp_result ~result ~source ~internal pp_delegate
    ~delegate ~credit ~script =
  let Script.{code; storage} = script in
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
    Option.unopt_exn
      (Failure "ill-serialized code")
      (Data_encoding.force_decode code)
  and storage =
    Option.unopt_exn
      (Failure "ill-serialized storage")
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
  ( match delegate with
  | None ->
      Format.fprintf ppf "@,No delegate for this contract"
  | Some delegate ->
      Format.fprintf ppf "@,Delegate: %a" pp_delegate delegate ) ;
  pp_result ppf result ; Format.fprintf ppf "@]"

let pp_manager_operation_content (type kind) source internal pp_result ppf
    ((operation, result) : kind manager_operation * _) =
  Format.fprintf ppf "@[<v 0>" ;
  ( match operation with
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
        Contract.pp
        destination ;
      ( match entrypoint with
      | "default" ->
          ()
      | _ ->
          Format.fprintf ppf "@,Entrypoint: %s" entrypoint ) ;
      ( if not (Script_repr.is_unit_parameter parameters) then
        let expr =
          Option.unopt_exn
            (Failure "ill-serialized argument")
            (Data_encoding.force_decode parameters)
        in
        Format.fprintf
          ppf
          "@,Parameter: @[<v 0>%a@]"
          Michelson_v1_printer.print_expr
          expr ) ;
      pp_result ppf result ; Format.fprintf ppf "@]"
  | Origination_legacy {delegate; credit; script; preorigination = _} ->
      pp_origination
        ppf
        ~pp_result
        ~result
        ~source
        ~internal
        Signature.Public_key_hash.pp
        ~delegate
        ~credit
        ~script
  | Origination {delegate; credit; script; preorigination = _} ->
      pp_origination
        ppf
        ~pp_result
        ~result
        ~source
        ~internal
        Baker_hash.pp
        ~delegate
        ~credit
        ~script
  | Baker_registration {credit; _} ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,From: %a@,Credit: %s%a"
        ( if internal then "Internal baker registration"
        else "Baker registration" )
        Contract.pp
        source
        Client_proto_args.tez_sym
        Tez.pp
        credit ;
      pp_result ppf result ;
      Format.fprintf ppf "@]"
  | Reveal key ->
      Format.fprintf
        ppf
        "@[<v 2>%s of manager public key:@,Contract: %a@,Key: %a%a@]"
        (if internal then "Internal revelation" else "Revelation")
        Contract.pp
        source
        Signature.Public_key.pp
        key
        pp_result
        result
  | Delegation_legacy None ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,Contract: %a@,To: nobody%a@]"
        (if internal then "Internal Delegation" else "Delegation")
        Contract.pp
        source
        pp_result
        result
  | Delegation_legacy (Some delegate) ->
      Format.fprintf
        ppf
        "@[<v 2>%s:@,Contract: %a@,To: %a%a@]"
        (if internal then "Internal Delegation" else "Delegation")
        Contract.pp
        source
        Signature.Public_key_hash.pp
        delegate
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
        Baker_hash.pp
        delegate
        pp_result
        result ) ;
  Format.fprintf ppf "@]"

let pp_baker_operation_content (type kind) baker pp_result ppf
    ((operation, result) : kind baker_operation * _) =
  Format.fprintf ppf "@[<v 0>" ;
  ( match operation with
  | Baker_proposals {period; proposals} ->
      Format.fprintf
        ppf
        "@[<v 2>Proposals:@,\
         Baker: %a@,\
         Period: %a@,\
         Protocols:@,\
        \  @[<v 0>%a@]%a@]"
        Baker_hash.pp
        baker
        Voting_period.pp
        period
        Format.(pp_print_list pp_print_string)
        proposals
        pp_result
        result
  | Baker_ballot {period; proposal; ballot} ->
      Format.fprintf
        ppf
        "@[<v 2>Ballot:@,Baker: %a@,Period: %a@,Protocol: %a@,Vote: %a%a@]"
        Baker_hash.pp
        baker
        Voting_period.pp
        period
        Format.pp_print_string
        proposal
        Data_encoding.Json.pp
        (Data_encoding.Json.construct Vote.ballot_encoding ballot)
        pp_result
        result
  | Set_baker_active active ->
      Format.fprintf
        ppf
        "@[<v 2>Set baker active:@,Baker: %a@,Active: %a%a@]"
        Baker_hash.pp
        baker
        Format.pp_print_bool
        active
        pp_result
        result
  | Toggle_baker_delegations accept ->
      Format.fprintf
        ppf
        "@[<v 2>Toggle baker delegations:@,Baker: %a@,Accept: %a%a@]"
        Baker_hash.pp
        baker
        Format.pp_print_bool
        accept
        pp_result
        result
  | Set_baker_consensus_key key ->
      Format.fprintf
        ppf
        "@[<v 2>Set baker consensus key:@,Baker: %a@,Key: %a%a@]"
        Baker_hash.pp
        baker
        Signature.Public_key.pp
        key
        pp_result
        result
  | Set_baker_pvss_key key ->
      Format.fprintf
        ppf
        "@[<v 2>Set baker pvss key:@,Baker: %a@,Key: %a%a@]"
        Baker_hash.pp
        baker
        Pvss_secp256k1.Public_key.pp
        key
        pp_result
        result ) ;
  Format.fprintf ppf "@]"

let pp_balance_updates ppf = function
  | [] ->
      ()
  | balance_updates ->
      let open Receipt in
      (* For dry runs, the baker's key is zero. Instead of printing this baker
         hash, we want to make the result more informative. *)
      let pp_baker ppf baker_hash =
        if Baker_hash.equal baker_hash Baker_hash.zero then
          Format.fprintf ppf "the baker who will include this operation"
        else Baker_hash.pp ppf baker_hash
      in
      let balance_updates =
        List.map
          (fun (balance, update) ->
            let balance =
              match balance with
              | Contract c ->
                  Format.asprintf "%a" Contract.pp c
              | Rewards (baker, l) ->
                  Format.asprintf "rewards(%a,%a)" pp_baker baker Cycle.pp l
              | Fees (baker, l) ->
                  Format.asprintf "fees(%a,%a)" pp_baker baker Cycle.pp l
              | Deposits (baker, l) ->
                  Format.asprintf "deposits(%a,%a)" pp_baker baker Cycle.pp l
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

let pp_manager_operation_contents_and_result ppf
    ( Manager_operation
        {source; fee; operation; counter; gas_limit; storage_limit},
      Manager_operation_result
        {balance_updates; operation_result; internal_operation_results} ) =
  let pp_lazy_storage_diff = function
    | None ->
        ()
    | Some lazy_storage_diff -> (
        let big_map_diff =
          Contract.Legacy_big_map_diff.of_lazy_storage_diff lazy_storage_diff
        in
        match (big_map_diff :> Contract.Legacy_big_map_diff.item list) with
        | [] ->
            ()
        | _ :: _ ->
            (* TODO: print all lazy storage diff *)
            Format.fprintf
              ppf
              "@,@[<v 2>Updated big_maps:@ %a@]"
              Michelson_v1_printer.print_big_map_diff
              lazy_storage_diff )
  in
  let pp_transaction_result
      (Transaction_result
        { balance_updates;
          consumed_gas;
          code = _;
          storage;
          originated_contracts;
          storage_size;
          paid_storage_size_diff;
          lazy_storage_diff;
          allocated_destination_contract = _ }) =
    ( match originated_contracts with
    | [] ->
        ()
    | contracts ->
        Format.fprintf
          ppf
          "@,@[<v 2>Originated contracts:@,%a@]"
          (Format.pp_print_list Contract.pp)
          contracts ) ;
    ( match storage with
    | None ->
        ()
    | Some expr ->
        Format.fprintf
          ppf
          "@,@[<hv 2>Updated storage:@ %a@]"
          Michelson_v1_printer.print_expr
          expr ) ;
    pp_lazy_storage_diff lazy_storage_diff ;
    if storage_size <> Z.zero then
      Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string storage_size) ;
    if paid_storage_size_diff <> Z.zero then
      Format.fprintf
        ppf
        "@,Paid storage size diff: %s bytes"
        (Z.to_string paid_storage_size_diff) ;
    Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas) ;
    match balance_updates with
    | [] ->
        ()
    | balance_updates ->
        Format.fprintf
          ppf
          "@,Balance updates:@,  %a"
          pp_balance_updates
          balance_updates
  in
  let pp_origination_result ~lazy_storage_diff ~balance_updates ~consumed_gas
      ~originated_contracts ~storage_size ~paid_storage_size_diff =
    ( match originated_contracts with
    | [] ->
        ()
    | contracts ->
        Format.fprintf
          ppf
          "@,@[<v 2>Originated contracts:@,%a@]"
          (Format.pp_print_list Contract.pp)
          contracts ) ;
    if storage_size <> Z.zero then
      Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string storage_size) ;
    pp_lazy_storage_diff lazy_storage_diff ;
    if paid_storage_size_diff <> Z.zero then
      Format.fprintf
        ppf
        "@,Paid storage size diff: %s bytes"
        (Z.to_string paid_storage_size_diff) ;
    Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas) ;
    match balance_updates with
    | [] ->
        ()
    | balance_updates ->
        Format.fprintf
          ppf
          "@,Balance updates:@,  %a"
          pp_balance_updates
          balance_updates
  in
  let pp_baker_registration_result
      (Baker_registration_result
        { balance_updates;
          consumed_gas;
          registered_baker;
          storage_size;
          paid_storage_size_diff }) =
    Format.fprintf
      ppf
      "@,@[<v 2>Registered baker:@,%a@]"
      Baker_hash.pp
      registered_baker ;
    if storage_size <> Z.zero then
      Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string storage_size) ;
    if paid_storage_size_diff <> Z.zero then
      Format.fprintf
        ppf
        "@,Paid storage size diff: %s bytes"
        (Z.to_string paid_storage_size_diff) ;
    Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas) ;
    match balance_updates with
    | [] ->
        ()
    | balance_updates ->
        Format.fprintf
          ppf
          "@,Balance updates:@,  %a"
          pp_balance_updates
          balance_updates
  in
  let pp_manager_result (type kind) ppf
      (result : kind manager_operation_result) =
    Format.fprintf ppf "@," ;
    match result with
    | Skipped _ ->
        Format.fprintf ppf "This operation was skipped"
    | Failed (_, _errs) ->
        Format.fprintf ppf "This operation FAILED."
    | Applied (Reveal_result {consumed_gas}) ->
        Format.fprintf ppf "This revelation was successfully applied" ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Backtracked (Reveal_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>This revelation was BACKTRACKED, its expected effects were \
           NOT applied.@]"
    | Applied (Delegation_legacy_result {consumed_gas}) ->
        Format.fprintf ppf "This delegation was successfully applied" ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Applied (Delegation_result {consumed_gas}) ->
        Format.fprintf ppf "This delegation was successfully applied" ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Backtracked (Delegation_legacy_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>This delegation was BACKTRACKED, its expected effects were \
           NOT applied.@]"
    | Backtracked (Delegation_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>This delegation was BACKTRACKED, its expected effects were \
           NOT applied.@]"
    | Applied (Transaction_result _ as tx) ->
        Format.fprintf ppf "This transaction was successfully applied" ;
        pp_transaction_result tx
    | Backtracked ((Transaction_result _ as tx), _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This transaction was BACKTRACKED, its expected effects (as \
           follow) were NOT applied.@]" ;
        pp_transaction_result tx
    | Applied
        (Origination_legacy_result
          { lazy_storage_diff;
            balance_updates;
            consumed_gas;
            originated_contracts;
            storage_size;
            paid_storage_size_diff }) ->
        Format.fprintf ppf "This origination was successfully applied" ;
        pp_origination_result
          ~lazy_storage_diff
          ~balance_updates
          ~consumed_gas
          ~originated_contracts
          ~storage_size
          ~paid_storage_size_diff
    | Applied
        (Origination_result
          { lazy_storage_diff;
            balance_updates;
            consumed_gas;
            originated_contracts;
            storage_size;
            paid_storage_size_diff }) ->
        Format.fprintf ppf "This origination was successfully applied" ;
        pp_origination_result
          ~lazy_storage_diff
          ~balance_updates
          ~consumed_gas
          ~originated_contracts
          ~storage_size
          ~paid_storage_size_diff
    | Backtracked
        ( Origination_legacy_result
            { lazy_storage_diff;
              balance_updates;
              consumed_gas;
              originated_contracts;
              storage_size;
              paid_storage_size_diff },
          _errs ) ->
        Format.fprintf
          ppf
          "@[<v 0>This origination was BACKTRACKED, its expected effects (as \
           follow) were NOT applied.@]" ;
        pp_origination_result
          ~lazy_storage_diff
          ~balance_updates
          ~consumed_gas
          ~originated_contracts
          ~storage_size
          ~paid_storage_size_diff
    | Backtracked
        ( Origination_result
            { lazy_storage_diff;
              balance_updates;
              consumed_gas;
              originated_contracts;
              storage_size;
              paid_storage_size_diff },
          _errs ) ->
        Format.fprintf
          ppf
          "@[<v 0>This origination was BACKTRACKED, its expected effects (as \
           follow) were NOT applied.@]" ;
        pp_origination_result
          ~lazy_storage_diff
          ~balance_updates
          ~consumed_gas
          ~originated_contracts
          ~storage_size
          ~paid_storage_size_diff
    | Applied (Baker_registration_result _ as op) ->
        Format.fprintf ppf "This baker registration was successfully applied" ;
        pp_baker_registration_result op
    | Backtracked ((Baker_registration_result _ as op), _errs) ->
        Format.fprintf
          ppf
          "@[<v 0>This baker registration was BACKTRACKED, its expected \
           effects (as follow) were NOT applied.@]" ;
        pp_baker_registration_result op
  in
  let pp_baker_result (type kind) ppf (result : kind baker_operation_result) =
    Format.fprintf ppf "@," ;
    match result with
    | Skipped _ ->
        Format.fprintf ppf "This operation was skipped"
    | Failed (_, _errs) ->
        Format.fprintf ppf "This operation FAILED."
    | Applied (Baker_proposals_result {consumed_gas}) ->
        Format.fprintf ppf "The proposals were successfully submitted" ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Backtracked (Baker_proposals_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>The proposals submission was BACKTRACKED, its expected \
           effects were NOT applied.@]"
    | Applied (Baker_ballot_result {consumed_gas}) ->
        Format.fprintf ppf "The ballot was successfully submitted" ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Backtracked (Baker_ballot_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>The ballot submission was BACKTRACKED, its expected effects \
           were NOT applied.@]"
    | Applied (Set_baker_active_result {active; consumed_gas}) ->
        Format.fprintf
          ppf
          "The baker was successfully %s"
          (if active then "activated" else "deactivated") ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Backtracked (Set_baker_active_result {active; _}, _) ->
        Format.fprintf
          ppf
          "@[<v 0>The baker %s was BACKTRACKED, its expected effects were NOT \
           applied.@]"
          (if active then "activation" else "deactivation")
    | Applied (Toggle_baker_delegations_result {accept; consumed_gas}) ->
        Format.fprintf
          ppf
          "The baker was successfully set to %s new delegations"
          (if accept then "accept" else "decline") ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Backtracked (Toggle_baker_delegations_result {accept; _}, _) ->
        Format.fprintf
          ppf
          "@[<v 0>The baker set to %s new delegations was BACKTRACKED, its \
           expected effects were NOT applied.@]"
          (if accept then "accept" else "decline")
    | Applied (Set_baker_consensus_key_result {consumed_gas}) ->
        Format.fprintf ppf "The baker consensus key was successfully set" ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Backtracked (Set_baker_consensus_key_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>The operation to set baker consensus key was BACKTRACKED, \
           its expected effects were NOT applied.@]"
    | Applied (Set_baker_pvss_key_result {consumed_gas}) ->
        Format.fprintf ppf "The baker PVSS key was successfully set" ;
        Format.fprintf ppf "@,Consumed gas: %s" (Z.to_string consumed_gas)
    | Backtracked (Set_baker_pvss_key_result _, _) ->
        Format.fprintf
          ppf
          "@[<v 0>The operation to set baker PVSS key was BACKTRACKED, its \
           expected effects were NOT applied.@]"
  in
  Format.fprintf
    ppf
    "@[<v 0>@[<v 2>Manager signed operations:@,\
     From: %a@,\
     Fee to the baker: %s%a@,\
     Expected counter: %s@,\
     Gas limit: %s@,\
     Storage limit: %s bytes"
    Signature.Public_key_hash.pp
    source
    Client_proto_args.tez_sym
    Tez.pp
    fee
    (Z.to_string counter)
    (Z.to_string gas_limit)
    (Z.to_string storage_limit) ;
  ( match balance_updates with
  | [] ->
      ()
  | balance_updates ->
      Format.fprintf
        ppf
        "@,Balance updates:@,  %a"
        pp_balance_updates
        balance_updates ) ;
  Format.fprintf
    ppf
    "@,%a"
    (pp_manager_operation_content
       (Contract.implicit_contract source)
       false
       pp_manager_result)
    (operation, operation_result) ;
  ( match internal_operation_results with
  | [] ->
      ()
  | _ :: _ ->
      Format.fprintf
        ppf
        "@,@[<v 2>Internal operations:@ %a@]"
        (Format.pp_print_list (fun ppf ->
           function
           | Internal_manager_operation_result (op, res) ->
               pp_manager_operation_content
                 op.source
                 false
                 pp_manager_result
                 ppf
                 (op.operation, res)
           | Internal_baker_operation_result (op, res) ->
               pp_baker_operation_content
                 op.baker
                 pp_baker_result
                 ppf
                 (op.operation, res)))
        internal_operation_results ) ;
  Format.fprintf ppf "@]"

let rec pp_contents_and_result_list :
    type kind. Format.formatter -> kind contents_and_result_list -> unit =
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
        \  %a@]"
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
        \  %a@]"
        Block_hash.pp
        (Block_header.hash bh1)
        Block_hash.pp
        (Block_header.hash bh2)
        pp_balance_updates
        bus
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
  | Single_and_result (Activate_account {id; _}, Activate_account_result bus)
    ->
      Format.fprintf
        ppf
        "@[<v 2>Genesis account activation:@,\
         Account: %a@,\
         Balance updates:@,\
        \  %a@]"
        Ed25519.Public_key_hash.pp
        id
        pp_balance_updates
        bus
  | Single_and_result
      (Endorsement {level}, Endorsement_result {balance_updates; baker; slots})
    ->
      Format.fprintf
        ppf
        "@[<v 2>Endorsement:@,\
         Level: %a@,\
         Balance updates:%a@,\
         Baker: %a@,\
         Slots: %a@]"
        Raw_level.pp
        level
        pp_balance_updates
        balance_updates
        Baker_hash.pp
        baker
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int)
        slots
  | Single_and_result (Proposals {source; period; proposals}, Proposals_result)
    ->
      Format.fprintf
        ppf
        "@[<v 2>Proposals:@,From: %a@,Period: %a@,Protocols:@,  @[<v 0>%a@]@]"
        Signature.Public_key_hash.pp
        source
        Voting_period.pp
        period
        (Format.pp_print_list Protocol_hash.pp)
        proposals
  | Single_and_result (Ballot {source; period; proposal; ballot}, Ballot_result)
    ->
      Format.fprintf
        ppf
        "@[<v 2>Ballot:@,From: %a@,Period: %a@,Protocol: %a@,Vote: %a@]"
        Signature.Public_key_hash.pp
        source
        Voting_period.pp
        period
        Protocol_hash.pp
        proposal
        Data_encoding.Json.pp
        (Data_encoding.Json.construct Vote.ballot_encoding ballot)
  | Single_and_result (Failing_noop arbitrary, Failing_noop_result) ->
      Format.fprintf ppf "@[<v 2>Failing_noop arbitrary: %s@]" arbitrary
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

let pp_internal_operation ppf = function
  | Internal_manager_operation {source; operation; nonce = _} ->
      pp_manager_operation_content
        source
        true
        (fun _ppf () -> ())
        ppf
        (operation, ())
  | Internal_baker_operation {baker; operation; nonce = _} ->
      pp_baker_operation_content baker (fun _ppf () -> ()) ppf (operation, ())
