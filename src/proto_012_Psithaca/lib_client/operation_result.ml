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
        Contract.pp
        destination ;
      (match entrypoint with
      | "default" -> ()
      | _ -> Format.fprintf ppf "@,Entrypoint: %s" entrypoint) ;
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
  | Origination {delegate; credit; script = {code; storage}; preorigination = _}
    ->
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
              | Legacy_rewards (pkh, l) ->
                  Format.asprintf
                    "legacy_rewards(%a,%a)"
                    pp_baker
                    pkh
                    Cycle.pp
                    l
              | Block_fees -> "payload fees(the block proposer)"
              | Legacy_deposits (pkh, l) ->
                  Format.asprintf
                    "legacy_deposits(%a,%a)"
                    pp_baker
                    pkh
                    Cycle.pp
                    l
              | Deposits pkh -> Format.asprintf "deposits(%a)" pp_baker pkh
              | Nonce_revelation_rewards -> "nonce revelation rewards"
              | Double_signing_evidence_rewards ->
                  "double signing evidence rewards"
              | Endorsing_rewards -> "endorsing rewards"
              | Baking_rewards -> "baking rewards"
              | Baking_bonuses -> "baking bonuses"
              | Legacy_fees (pkh, c) ->
                  Format.asprintf "legacy_fees(%a,%a)" pp_baker pkh Cycle.pp c
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
  let pp_transaction_result
      (Transaction_result
         {
           balance_updates;
           consumed_gas;
           storage;
           originated_contracts;
           storage_size;
           paid_storage_size_diff;
           lazy_storage_diff;
           allocated_destination_contract = _;
         }) =
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
      Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string storage_size) ;
    if paid_storage_size_diff <> Z.zero then
      Format.fprintf
        ppf
        "@,Paid storage size diff: %s bytes"
        (Z.to_string paid_storage_size_diff) ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    match balance_updates with
    | [] -> ()
    | balance_updates ->
        Format.fprintf
          ppf
          "@,Balance updates:@,  %a"
          pp_balance_updates
          balance_updates
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
    match balance_updates with
    | [] -> ()
    | balance_updates ->
        Format.fprintf
          ppf
          "@,Balance updates:@,  %a"
          pp_balance_updates
          balance_updates
  in
  let pp_register_global_constant_result
      (Register_global_constant_result
         {balance_updates; consumed_gas; size_of_constant; global_address}) =
    (match balance_updates with
    | [] ->
        (* Not possible - register global constant operation always returns
           balance updates. *)
        assert false
    | balance_updates ->
        Format.fprintf
          ppf
          "@,Balance updates:@,  %a"
          pp_balance_updates
          balance_updates) ;
    Format.fprintf ppf "@,Consumed gas: %a" Gas.Arith.pp consumed_gas ;
    Format.fprintf ppf "@,Storage size: %s bytes" (Z.to_string size_of_constant) ;
    Format.fprintf ppf "@,Global address: %a" Script_expr_hash.pp global_address
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
    | Applied (Transaction_result _ as tx) ->
        Format.fprintf ppf "This transaction was successfully applied" ;
        pp_transaction_result tx
    | Backtracked ((Transaction_result _ as tx), _errs) ->
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
  (match balance_updates with
  | [] -> ()
  | balance_updates ->
      Format.fprintf
        ppf
        "@,Balance updates:@,  %a"
        pp_balance_updates
        balance_updates) ;
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
        (Format.pp_print_list (fun ppf (Internal_operation_result (op, res)) ->
             pp_manager_operation_content
               op.source
               false
               pp_result
               ppf
               (op.operation, res)))
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

let pp_internal_operation ppf
    (Internal_operation {source; operation; nonce = _}) =
  pp_manager_operation_content
    source
    true
    (fun _ppf () -> ())
    ppf
    (operation, ())
