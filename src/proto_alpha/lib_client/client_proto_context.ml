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
open Protocol_client_context
open Tezos_micheline
open Client_proto_contracts
open Client_keys

let get_balance (rpc : #rpc_context) ~chain ~block contract =
  Alpha_services.Contract.balance rpc (chain, block) contract

let get_storage (rpc : #rpc_context) ~chain ~block ~unparsing_mode contract =
  Plugin.RPC.Contract.get_storage_normalized
    rpc
    (chain, block)
    ~unparsing_mode
    ~contract

let get_used_storage_space (rpc : #rpc_context) ~chain ~block contract =
  Plugin.RPC.Contract.get_used_storage_space rpc (chain, block) ~contract

let get_paid_storage_space (rpc : #rpc_context) ~chain ~block contract =
  Plugin.RPC.Contract.get_paid_storage_space rpc (chain, block) ~contract

let get_big_map_value (rpc : #rpc_context) ~chain ~block ~unparsing_mode id key
    =
  Plugin.RPC.Big_map.big_map_get_normalized
    rpc
    (chain, block)
    ~unparsing_mode
    id
    key

let get_contract_big_map_value (rpc : #rpc_context) ~chain ~block contract key =
  Alpha_services.Contract.contract_big_map_get_opt
    rpc
    (chain, block)
    contract
    key

let get_script (rpc : #rpc_context) ~chain ~block ~unparsing_mode
    ~normalize_types contract =
  Plugin.RPC.Contract.get_script_normalized
    rpc
    (chain, block)
    ~unparsing_mode
    ~normalize_types
    ~contract

let get_script_hash (rpc : #rpc_context) ~chain ~block contract =
  Alpha_services.Contract.script_opt rpc (chain, block) contract
  >>=? fun script_opt ->
  Lwt.return @@ Environment.wrap_tzresult
  @@ Option.map_e
       (fun {Script.code; storage = _} ->
         Script_repr.force_decode code >>? fun code ->
         let bytes =
           Data_encoding.Binary.to_bytes_exn Script.expr_encoding code
         in
         let hash = Script_expr_hash.hash_bytes [bytes] in
         ok hash)
       script_opt

let get_contract_ticket_balance (rpc : #rpc_context) ~chain ~block contract key
    =
  Plugin.RPC.Contract.get_ticket_balance rpc (chain, block) contract key

let get_contract_all_ticket_balances (rpc : #rpc_context) ~chain ~block contract
    =
  Plugin.RPC.Contract.get_all_ticket_balances rpc (chain, block) contract

let ticket_balances_encoding = Plugin.RPC.Contract.ticket_balances_encoding

let get_frozen_deposits_limit (rpc : #rpc_context) ~chain ~block delegate =
  Alpha_services.Delegate.frozen_deposits_limit rpc (chain, block) delegate

let parse_expression arg =
  Lwt.return
    (Micheline_parser.no_parsing_error
       (Michelson_v1_parser.parse_expression arg))

let parse_arg_transfer arg =
  (match arg with
  | Some arg ->
      parse_expression arg >>=? fun {expanded = arg; _} -> return_some arg
  | None -> return_none)
  >>=? fun parameters ->
  return
    (Option.fold ~some:Script.lazy_expr ~none:Script.unit_parameter parameters)

let build_transaction_operation ~amount ~parameters
    ?(entrypoint = Entrypoint.default) ?fee ?gas_limit ?storage_limit
    destination =
  let operation = Transaction {amount; parameters; destination; entrypoint} in
  Injection.prepare_manager_operation
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    operation

let transfer_with_script (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?(force = false) ?branch ?successor_level
    ~source ~src_pk ~src_sk ~destination ?(entrypoint = Entrypoint.default)
    ~parameters ~amount ?fee ?gas_limit ?storage_limit ?counter ~fee_parameter
    ?replace_by_fees () =
  let contents =
    build_transaction_operation
      ~amount
      ~parameters
      ~entrypoint
      ?fee
      ?gas_limit
      ?storage_limit
      destination
  in
  let contents = Annotated_manager_operation.Single_manager contents in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ~force
    ?branch
    ?successor_level
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    ?counter
    ?replace_by_fees
    ~src_pk
    ~src_sk
    ~fee_parameter
    contents
  >>=? fun (oph, _, op, result) ->
  Lwt.return (Injection.originated_contracts ~force result)
  >>=? fun contracts ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return ((oph, op, result), contracts)

let transfer (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?(force = false) ?branch ?successor_level
    ~source ~src_pk ~src_sk ~destination ?entrypoint ?arg ~amount ?fee
    ?gas_limit ?storage_limit ?counter ~fee_parameter ?replace_by_fees () =
  parse_arg_transfer arg >>=? fun parameters ->
  transfer_with_script
    (cctxt : #full)
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ~force
    ?branch
    ?successor_level
    ~source
    ~src_pk
    ~src_sk
    ~destination
    ?entrypoint
    ~parameters
    ~amount
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ~fee_parameter
    ?replace_by_fees
    ()

let build_reveal_operation ?fee ?gas_limit ?storage_limit pk =
  let operation = Reveal pk in
  Injection.prepare_manager_operation
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    operation

let reveal cctxt ~chain ~block ?confirmations ?dry_run ?verbose_signing ?branch
    ~source ~src_pk ~src_sk ?fee ~fee_parameter () =
  let contents =
    Annotated_manager_operation.Single_manager
      (build_reveal_operation ?fee ~storage_limit:Z.zero src_pk)
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:Limit.unknown
    ~storage_limit:Limit.unknown
    ~src_pk
    ~src_sk
    ~fee_parameter
    contents
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let build_delegate_operation ?fee ?gas_limit ?storage_limit delegate_opt =
  let operation = Delegation delegate_opt in
  Injection.prepare_manager_operation
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    operation

let delegate_contract cctxt ~chain ~block ?branch ?confirmations ?dry_run
    ?verbose_signing ?simulation ~source ~src_pk ~src_sk ?fee ~fee_parameter
    delegate_opt =
  let operation =
    Annotated_manager_operation.Single_manager
      (build_delegate_operation ?fee delegate_opt)
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?branch
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:Limit.unknown
    ~storage_limit:Limit.unknown
    ~src_pk
    ~src_sk
    ~fee_parameter
    operation
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let list_contract_labels cctxt ~chain ~block =
  Alpha_services.Contract.list cctxt (chain, block) >>=? fun contracts ->
  List.rev_map_es
    (fun h ->
      (match (h : Contract.t) with
      | Implicit m -> (
          Public_key_hash.rev_find cctxt m >>=? function
          | None -> return ""
          | Some nm -> (
              RawContractAlias.find_opt cctxt nm >>=? function
              | None -> return (" (known as " ^ nm ^ ")")
              | Some _ -> return (" (known as key:" ^ nm ^ ")")))
      | Originated _ -> (
          RawContractAlias.rev_find cctxt h >>=? function
          | None -> return ""
          | Some nm -> return (" (known as " ^ nm ^ ")")))
      >>=? fun nm ->
      let kind =
        match h with Implicit _ -> " (implicit)" | Originated _ -> ""
      in
      let h_b58 = Contract.to_b58check h in
      return (nm, h_b58, kind))
    contracts
  >|=? List.rev

let message_added_contract (cctxt : #full) name =
  cctxt#message "Contract memorized as %s." name

let set_delegate cctxt ~chain ~block ?confirmations ?dry_run ?verbose_signing
    ?simulation ?fee contract ~src_pk ~manager_sk ~fee_parameter opt_delegate =
  delegate_contract
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ~source:contract
    ~src_pk
    ~src_sk:manager_sk
    ?fee
    ~fee_parameter
    opt_delegate

let build_update_consensus_key ?fee ?gas_limit ?storage_limit consensus_pk =
  let operation = Update_consensus_key consensus_pk in
  Injection.prepare_manager_operation
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    operation

let register_as_delegate cctxt ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?fee ~manager_sk ~fee_parameter ?consensus_pk src_pk =
  let source = Signature.Public_key.hash src_pk in
  let delegate_op = build_delegate_operation ?fee (Some source) in
  match consensus_pk with
  | None -> (
      let operation = Annotated_manager_operation.Single_manager delegate_op in
      Injection.inject_manager_operation
        cctxt
        ~chain
        ~block
        ?confirmations
        ?dry_run
        ?verbose_signing
        ~source
        ~fee:(Limit.of_option fee)
        ~gas_limit:Limit.unknown
        ~storage_limit:Limit.unknown
        ~src_pk
        ~src_sk:manager_sk
        ~fee_parameter
        operation
      >>=? fun (oph, _, op, result) ->
      match Apply_results.pack_contents_list op result with
      | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
          return ((oph, op, result), None))
  | Some consensus_pk -> (
      let operation =
        Annotated_manager_operation.Cons_manager
          ( delegate_op,
            Annotated_manager_operation.Single_manager
              (build_update_consensus_key ?fee consensus_pk) )
      in
      Injection.inject_manager_operation
        cctxt
        ~chain
        ~block
        ?confirmations
        ?dry_run
        ?verbose_signing
        ~successor_level:true
        ~source
        ~fee:(Limit.of_option fee)
        ~gas_limit:Limit.unknown
        ~storage_limit:Limit.unknown
        ~src_pk
        ~src_sk:manager_sk
        ~fee_parameter
        operation
      >>=? fun (oph, _, op, result) ->
      match Apply_results.pack_contents_list op result with
      | Apply_results.Single_and_result
          (Manager_operation _, Manager_operation_result _) ->
          .
      | Apply_results.Cons_and_result
          ( (Manager_operation _ as op1),
            res1,
            Single_and_result ((Manager_operation _ as op2), res2) ) ->
          return ((oph, op1, res1), Some (op2, res2)))

let update_consensus_key cctxt ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ~consensus_pk ~manager_sk ~fee_parameter
    src_pk =
  let source = Signature.Public_key.hash src_pk in
  let operation = build_update_consensus_key ?fee consensus_pk in
  let operation = Annotated_manager_operation.Single_manager operation in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ~successor_level:true
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:Limit.unknown
    ~storage_limit:Limit.unknown
    ~src_pk
    ~src_sk:manager_sk
    ~fee_parameter
    operation
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let drain_delegate cctxt ~chain ~block ?confirmations ?dry_run ?verbose_signing
    ?simulation ~consensus_sk ~consensus_pkh ?(destination = consensus_pkh)
    ~delegate () =
  let operation =
    Single
      (Drain_delegate {consensus_key = consensus_pkh; delegate; destination})
  in
  Injection.inject_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ~src_sk:consensus_sk
    operation
  >>=? fun (oph, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Drain_delegate _ as op), result) ->
      return (oph, op, result)

let set_deposits_limit cctxt ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee contract ~src_pk ~manager_sk
    ~fee_parameter limit_opt =
  let operation = Set_deposits_limit limit_opt in
  let operation =
    Injection.prepare_manager_operation
      ~fee:(Limit.of_option fee)
      ~gas_limit:Limit.unknown
      ~storage_limit:Limit.unknown
      operation
  in
  let operation = Annotated_manager_operation.Single_manager operation in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ~source:contract
    ~fee:(Limit.of_option fee)
    ~gas_limit:Limit.unknown
    ~storage_limit:Limit.unknown
    ~src_pk
    ~src_sk:manager_sk
    ~fee_parameter
    operation
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let increase_paid_storage cctxt ~chain ~block ?force ?dry_run ?verbose_signing
    ?fee ?confirmations ?simulation ~source ~destination ~src_pk ~manager_sk
    ~fee_parameter ~amount_in_bytes () =
  let operation = Increase_paid_storage {amount_in_bytes; destination} in
  let operation =
    Injection.prepare_manager_operation
      ~fee:(Limit.of_option fee)
      ~gas_limit:Limit.unknown
      ~storage_limit:Limit.unknown
      operation
  in
  let operation = Annotated_manager_operation.Single_manager operation in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?force
    ?dry_run
    ?verbose_signing
    ?simulation
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:Limit.unknown
    ~storage_limit:Limit.unknown
    ~src_pk
    ~src_sk:manager_sk
    ~fee_parameter
    operation
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let save_contract ~force cctxt alias_name contract =
  RawContractAlias.add ~force cctxt alias_name contract >>=? fun () ->
  message_added_contract cctxt alias_name >>= fun () -> return_unit

let build_origination_operation ?fee ?gas_limit ?storage_limit ~initial_storage
    ~code ~delegate ~balance () =
  (* With the change of making implicit accounts delegatable, the following
     3 arguments are being defaulted before they can be safely removed. *)
  Lwt.return (Michelson_v1_parser.parse_expression initial_storage)
  >>= fun result ->
  Lwt.return (Micheline_parser.no_parsing_error result)
  >>=? fun {Michelson_v1_parser.expanded = storage; _} ->
  let code = Script.lazy_expr code and storage = Script.lazy_expr storage in
  let origination =
    Origination {delegate; script = {code; storage}; credit = balance}
  in
  return
    (Injection.prepare_manager_operation
       ~fee:(Limit.of_option fee)
       ~gas_limit:(Limit.of_option gas_limit)
       ~storage_limit:(Limit.of_option storage_limit)
       origination)

let originate_contract (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?branch ?fee ?gas_limit ?storage_limit ~delegate
    ~initial_storage ~balance ~source ~src_pk ~src_sk ~code ~fee_parameter () =
  build_origination_operation
    ?fee
    ?gas_limit
    ?storage_limit
    ~initial_storage
    ~code
    ~delegate
    ~balance
    ()
  >>=? fun origination ->
  let origination = Annotated_manager_operation.Single_manager origination in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    origination
  >>=? fun (oph, _, op, result) ->
  (match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result))
  >>=? fun res ->
  Lwt.return (Injection.originated_contracts ~force:false result) >>=? function
  | [contract] -> return (res, Contract.Originated contract)
  | contracts ->
      failwith
        "The origination introduced %d contracts instead of one."
        (List.length contracts)

let michelson_expression_of_string str =
  Michelson_v1_parser.parse_expression str |> Micheline_parser.no_parsing_error
  >>? fun {Michelson_v1_parser.expanded = v; _} -> ok @@ Script.lazy_expr v

let build_register_global_constant ?fee ?gas_limit ?storage_limit value =
  michelson_expression_of_string value >>? fun value ->
  let op = Register_global_constant {value} in
  ok
    (Injection.prepare_manager_operation
       ~fee:(Limit.of_option fee)
       ~gas_limit:(Limit.of_option gas_limit)
       ~storage_limit:(Limit.of_option storage_limit)
       op)

let register_global_constant (cctxt : #full) ~chain ~block ?confirmations
    ?dry_run ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit
    ?counter ~source ~src_pk ~src_sk ~fee_parameter ~constant () =
  build_register_global_constant ?fee ?storage_limit ?gas_limit constant
  >>?= fun op ->
  let op = Annotated_manager_operation.Single_manager op in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

type activation_key = {
  pkh : Signature.Ed25519.Public_key_hash.t;
  amount : Tez.t;
  activation_code : Blinded_public_key_hash.activation_code;
  mnemonic : string list;
  password : string;
  email : string;
}

let raw_activation_key_encoding =
  let open Data_encoding in
  obj6
    (req "pkh" Signature.Ed25519.Public_key_hash.encoding)
    (req "amount" Tez.encoding)
    (req "activation_code" Blinded_public_key_hash.activation_code_encoding)
    (req "mnemonic" (list string))
    (req "password" string)
    (req "email" string)

let activation_key_encoding =
  (* Hack: allow compatibility with older encoding *)
  let open Data_encoding in
  conv
    (fun {pkh; amount; activation_code; mnemonic; password; email} ->
      (pkh, amount, activation_code, mnemonic, password, email))
    (fun (pkh, amount, activation_code, mnemonic, password, email) ->
      {pkh; amount; activation_code; mnemonic; password; email})
  @@ splitted
       ~binary:raw_activation_key_encoding
       ~json:
         (union
            [
              case
                ~title:"Activation"
                Json_only
                raw_activation_key_encoding
                (fun x -> Some x)
                (fun x -> x);
              case
                ~title:"Deprecated_activation"
                Json_only
                (obj6
                   (req "pkh" Signature.Ed25519.Public_key_hash.encoding)
                   (req "amount" Tez.encoding)
                   (req
                      "secret"
                      Blinded_public_key_hash.activation_code_encoding)
                   (req "mnemonic" (list string))
                   (req "password" string)
                   (req "email" string))
                (fun _ -> None)
                (fun x -> x);
            ])

type batch_transfer_operation = {
  destination : string;
  fee : string option;
  gas_limit : Gas.Arith.integral option;
  storage_limit : Z.t option;
  amount : string;
  arg : string option;
  entrypoint : Entrypoint.t option;
}

let batch_transfer_operation_encoding =
  let open Data_encoding in
  conv
    (fun {destination; fee; gas_limit; storage_limit; amount; arg; entrypoint} ->
      (destination, fee, gas_limit, storage_limit, amount, arg, entrypoint))
    (fun (destination, fee, gas_limit, storage_limit, amount, arg, entrypoint) ->
      {destination; fee; gas_limit; storage_limit; amount; arg; entrypoint})
    (obj7
       (req "destination" string)
       (opt "fee" string)
       (opt "gas-limit" Gas.Arith.n_integral_encoding)
       (opt "storage-limit" z)
       (req "amount" string)
       (opt "arg" string)
       (opt "entrypoint" Entrypoint.simple_encoding))

let read_key key =
  match Bip39.of_words key.mnemonic with
  | None -> failwith ""
  | Some t ->
      (* TODO: unicode normalization (NFKD)... *)
      let passphrase =
        Bytes.(cat (of_string key.email) (of_string key.password))
      in
      let sk = Bip39.to_seed ~passphrase t in
      let sk = Bytes.sub sk 0 32 in
      let sk : Signature.Secret_key.t =
        Ed25519
          (Data_encoding.Binary.of_bytes_exn
             Signature.Ed25519.Secret_key.encoding
             sk)
      in
      let pk = Signature.Secret_key.to_public_key sk in
      let pkh = Signature.Public_key.hash pk in
      return (pkh, pk, sk)

let inject_activate_operation cctxt ~chain ~block ?confirmations ?dry_run alias
    pkh activation_code =
  let contents = Single (Activate_account {id = pkh; activation_code}) in
  Injection.inject_operation
    cctxt
    ?confirmations
    ?dry_run
    ~chain
    ~block
    contents
  >>=? fun (oph, op, result) ->
  (match confirmations with
  | None -> return_unit
  | Some _confirmations ->
      Alpha_services.Contract.balance
        cctxt
        (chain, block)
        (Contract.Implicit (Ed25519 pkh))
      >>=? fun balance ->
      cctxt#message
        "Account %s (%a) activated with %s%a."
        alias
        Signature.Ed25519.Public_key_hash.pp
        pkh
        Operation_result.tez_sym
        Tez.pp
        balance
      >>= fun () -> return_unit)
  >>=? fun () ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Activate_account _ as op), result) ->
      return (oph, op, result)

let activate_account (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?(encrypted = false) ?force key name =
  read_key key >>=? fun (pkh, pk, sk) ->
  fail_unless
    (Signature.Public_key_hash.equal pkh (Ed25519 key.pkh))
    (error_of_fmt
       "@[<v 2>Inconsistent activation key:@ Computed pkh: %a@ Embedded pkh: \
        %a @]"
       Signature.Public_key_hash.pp
       pkh
       Signature.Ed25519.Public_key_hash.pp
       key.pkh)
  >>=? fun () ->
  let pk = Signature.Of_V1.public_key pk in
  let sk = Signature.Of_V1.secret_key sk in
  Tezos_signer_backends.Unencrypted.make_pk pk >>?= fun pk_uri ->
  (if encrypted then
   Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt cctxt sk
  else Tezos_signer_backends.Unencrypted.make_sk sk >>?= return)
  >>=? fun sk_uri ->
  Client_keys.register_key cctxt ?force (pkh, pk_uri, sk_uri) name
  >>=? fun () ->
  inject_activate_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    name
    key.pkh
    key.activation_code

let activate_existing_account (cctxt : #full) ~chain ~block ?confirmations
    ?dry_run alias activation_code =
  Client_keys.alias_keys cctxt alias >>=? function
  | Some (Ed25519 pkh, _, _) ->
      inject_activate_operation
        cctxt
        ~chain
        ~block
        ?confirmations
        ?dry_run
        alias
        pkh
        activation_code
  | Some _ -> failwith "Only Ed25519 accounts can be activated"
  | None -> failwith "Unknown account"

type period_info = {
  current_period_kind : Voting_period.kind;
  position : Int32.t;
  remaining : Int32.t;
  current_proposal : Protocol_hash.t option;
}

type ballots_info = {
  current_quorum : Int32.t;
  participation : Int32.t;
  supermajority : Int64.t;
  ballots : Vote.ballots;
}

let get_ballots_info (cctxt : #full) ~chain ~block =
  (* Get the next level, not the current *)
  let cb = (chain, block) in
  Alpha_services.Voting.ballots cctxt cb >>=? fun ballots ->
  Alpha_services.Voting.current_quorum cctxt cb >>=? fun current_quorum ->
  Alpha_services.Voting.total_voting_power cctxt cb
  >>=? fun max_participation ->
  let all_votes = Int64.(add (add ballots.yay ballots.nay) ballots.pass) in
  let participation =
    Z.(
      to_int32
        (div
           (mul (of_int64 all_votes) (of_int 100_00))
           (of_int64 max_participation)))
  in
  let supermajority = Int64.(div (mul 8L (add ballots.yay ballots.nay)) 10L) in
  return {current_quorum; participation; supermajority; ballots}

let get_period_info ?(successor = false) (cctxt : #full) ~chain ~block =
  let cb = (chain, block) in
  (if successor then Alpha_services.Voting.successor_period
  else Alpha_services.Voting.current_period)
    cctxt
    cb
  >>=? fun voting_period ->
  Alpha_services.Voting.current_proposal cctxt cb >>=? fun current_proposal ->
  return
    {
      current_period_kind = voting_period.voting_period.kind;
      position = voting_period.position;
      remaining = voting_period.remaining;
      current_proposal;
    }

let get_proposals (cctxt : #full) ~chain ~block =
  let cb = (chain, block) in
  Alpha_services.Voting.proposals cctxt cb

let submit_proposals ?dry_run ?verbose_signing (cctxt : #full) ~chain ~block
    ?confirmations ~src_sk source proposals =
  Alpha_services.Voting.successor_period cctxt (chain, block)
  >>=? fun {voting_period = {index; _}; _} ->
  let contents = Single (Proposals {source; period = index; proposals}) in
  Injection.inject_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ~src_sk
    contents
    ?verbose_signing

let submit_ballot ?dry_run ?verbose_signing (cctxt : #full) ~chain ~block
    ?confirmations ~src_sk source proposal ballot =
  (* The user must provide the proposal explicitly to make himself sure
     for what he is voting. *)
  Alpha_services.Voting.successor_period cctxt (chain, block)
  >>=? fun {voting_period = {index; _}; _} ->
  let contents = Single (Ballot {source; period = index; proposal; ballot}) in
  Injection.inject_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ~src_sk
    contents
    ?verbose_signing

let pp_operation formatter (a : Alpha_block_services.operation) =
  match (a.receipt, a.protocol_data) with
  | Receipt (Apply_results.Operation_metadata omd), Operation_data od -> (
      match Apply_results.kind_equal_list od.contents omd.contents with
      | Some Apply_results.Eq ->
          Operation_result.pp_operation_result
            formatter
            (od.contents, omd.contents)
      | None -> Stdlib.failwith "Unexpected result.")
  | Empty, _ ->
      Stdlib.failwith
        "Pruned metadata: the operation receipt was removed accordingly to the \
         node's history mode."
  | Too_large, _ -> Stdlib.failwith "Too large metadata."
  | _ -> Stdlib.failwith "Unexpected result."

let get_operation_from_block (cctxt : #full) ~chain predecessors operation_hash
    =
  Client_confirmations.lookup_operation_in_previous_blocks
    cctxt
    ~chain
    ~predecessors
    operation_hash
  >>=? function
  | None -> return_none
  | Some (block, i, j) ->
      cctxt#message
        "Operation found in block: %a (pass: %d, offset: %d)"
        Block_hash.pp
        block
        i
        j
      >>= fun () ->
      Protocol_client_context.Alpha_block_services.Operations.operation
        cctxt
        ~chain
        ~block:(`Hash (block, 0))
        i
        j
      >>=? fun op' -> return_some op'

let display_receipt_for_operation (cctxt : #full) ~chain ?(predecessors = 10)
    operation_hash =
  get_operation_from_block cctxt ~chain predecessors operation_hash
  >>=? function
  | None -> failwith "Couldn't find operation"
  | Some op -> cctxt#message "%a" pp_operation op >>= fun () -> return_unit

let cached_contracts cctxt ~chain ~block =
  let cb = (chain, block) in
  Alpha_services.Cache.cached_contracts cctxt cb

let contract_rank cctxt ~chain ~block contract =
  let cb = (chain, block) in
  Alpha_services.Cache.contract_rank cctxt cb contract

let contract_cache_size cctxt ~chain ~block =
  let cb = (chain, block) in
  Alpha_services.Cache.contract_cache_size cctxt cb

let contract_cache_size_limit cctxt ~chain ~block =
  let cb = (chain, block) in
  Alpha_services.Cache.contract_cache_size_limit cctxt cb

let transfer_ticket (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~src_pk ~src_sk ~fee_parameter ~contents ~ty ~ticketer ~amount ~destination
    ~entrypoint () =
  parse_expression contents >>=? fun {expanded; _} ->
  let contents = Script.lazy_expr expanded in
  parse_expression ty >>=? fun {expanded; _} ->
  let ty = Script.lazy_expr expanded in
  let operation :
      Kind.transfer_ticket Annotated_manager_operation.annotated_list =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Transfer_ticket
            {contents; ty; ticketer; amount; destination; entrypoint}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~successor_level:true
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    operation
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let sc_rollup_originate (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~kind ~boot_sector ~parameters_ty ~src_pk ~src_sk ~fee_parameter () =
  Proof_helpers.origination_proof ~boot_sector kind
  >|= Environment.wrap_tzresult
  >>=? fun origination_proof ->
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Sc_rollup_originate
            {kind; boot_sector; origination_proof; parameters_ty}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let sc_rollup_add_messages (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~messages ~src_pk ~src_sk ~fee_parameter () =
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Sc_rollup_add_messages {messages}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let sc_rollup_cement (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~rollup ~commitment ~src_pk ~src_sk ~fee_parameter () =
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Sc_rollup_cement {rollup; commitment}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let sc_rollup_publish (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~rollup ~commitment ~src_pk ~src_sk ~fee_parameter () =
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Sc_rollup_publish {rollup; commitment}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let sc_rollup_execute_outbox_message (cctxt : #full) ~chain ~block
    ?confirmations ?dry_run ?verbose_signing ?simulation ?fee ?gas_limit
    ?storage_limit ?counter ~source ~rollup ~cemented_commitment ~output_proof
    ~src_pk ~src_sk ~fee_parameter () =
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Sc_rollup_execute_outbox_message
            {rollup; cemented_commitment; output_proof}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let sc_rollup_recover_bond (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~src_pk ~src_sk ~fee_parameter ~sc_rollup ~staker () =
  let contents :
      Kind.sc_rollup_recover_bond Annotated_manager_operation.annotated_list =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Sc_rollup_recover_bond {sc_rollup; staker}))
  in
  (* TODO/Fixme: https://gitlab.com/tezos/tezos/-/issues/2609
     Decide if we should enforce ~successor_level:true for simulation.
     See https://gitlab.com/tezos/tezos/-/merge_requests/5395#note_958326685 *)
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    contents
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let sc_rollup_refute (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~rollup ~refutation ~opponent ~src_pk ~src_sk ~fee_parameter () =
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Sc_rollup_refute {rollup; refutation; opponent}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let sc_rollup_timeout (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~rollup ~alice ~bob ~src_pk ~src_sk ~fee_parameter () =
  let stakers = Sc_rollup.Game.Index.make alice bob in
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Sc_rollup_timeout {rollup; stakers}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let zk_rollup_originate (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~public_parameters ~circuits_info ~init_state ~nb_ops ~src_pk ~src_sk
    ~fee_parameter () =
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Zk_rollup_origination
            {public_parameters; circuits_info; init_state; nb_ops}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let zk_rollup_publish (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~zk_rollup ~ops ~src_pk ~src_sk ~fee_parameter () =
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Zk_rollup_publish {zk_rollup; ops}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let zk_rollup_update (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?fee ?gas_limit ?storage_limit ?counter ~source
    ~zk_rollup ~update ~src_pk ~src_sk ~fee_parameter () =
  let op =
    Annotated_manager_operation.Single_manager
      (Injection.prepare_manager_operation
         ~fee:(Limit.of_option fee)
         ~gas_limit:(Limit.of_option gas_limit)
         ~storage_limit:(Limit.of_option storage_limit)
         (Zk_rollup_update {zk_rollup; update}))
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?counter
    ~source
    ~fee:(Limit.of_option fee)
    ~storage_limit:(Limit.of_option storage_limit)
    ~gas_limit:(Limit.of_option gas_limit)
    ~src_pk
    ~src_sk
    ~fee_parameter
    op
  >>=? fun (oph, _, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)
