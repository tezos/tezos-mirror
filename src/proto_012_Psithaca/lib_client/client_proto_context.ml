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
open Client_keys_v0

let get_balance (rpc : #rpc_context) ~chain ~block contract =
  Alpha_services.Contract.balance rpc (chain, block) contract

let get_storage (rpc : #rpc_context) ~chain ~block ~unparsing_mode contract =
  Plugin.RPC.Contract.get_storage_normalized
    rpc
    (chain, block)
    ~unparsing_mode
    ~contract

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

let get_script (rpc : #rpc_context) ~chain ~block ~unparsing_mode contract =
  Plugin.RPC.Contract.get_script_normalized
    rpc
    (chain, block)
    ~unparsing_mode
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

let build_transaction_operation ~amount ~parameters ?(entrypoint = "default")
    ?fee ?gas_limit ?storage_limit destination =
  let operation = Transaction {amount; parameters; destination; entrypoint} in
  Injection.prepare_manager_operation
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    operation

let transfer (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?(force = false) ?branch ~source ~src_pk
    ~src_sk ~destination ?(entrypoint = "default") ?arg ~amount ?fee ?gas_limit
    ?storage_limit ?counter ~fee_parameter () =
  parse_arg_transfer arg >>=? fun parameters ->
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
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    ?counter
    ~src_pk
    ~src_sk
    ~fee_parameter
    contents
  >>=? fun (oph, op, result) ->
  Lwt.return (Injection.originated_contracts ~force result)
  >>=? fun contracts ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return ((oph, op, result), contracts)

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
  >>=? fun (oph, op, result) ->
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
  >>=? fun (oph, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let list_contract_labels cctxt ~chain ~block =
  Alpha_services.Contract.list cctxt (chain, block) >>=? fun contracts ->
  List.rev_map_es
    (fun h ->
      (match Contract.is_implicit h with
      | Some m -> (
          Public_key_hash.rev_find cctxt m >>=? function
          | None -> return ""
          | Some nm -> (
              Raw_contract_alias.find_opt cctxt nm >>=? function
              | None -> return (" (known as " ^ nm ^ ")")
              | Some _ -> return (" (known as key:" ^ nm ^ ")")))
      | None -> (
          Raw_contract_alias.rev_find cctxt h >>=? function
          | None -> return ""
          | Some nm -> return (" (known as " ^ nm ^ ")")))
      >>=? fun nm ->
      let kind =
        match Contract.is_implicit h with Some _ -> " (implicit)" | None -> ""
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

let register_as_delegate cctxt ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?fee ~manager_sk ~fee_parameter src_pk =
  let source = Signature.V0.Public_key.hash src_pk in
  delegate_contract
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ~source
    ~src_pk
    ~src_sk:manager_sk
    ?fee
    ~fee_parameter
    (Some source)

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
  >>=? fun (oph, op, result) ->
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)

let save_contract ~force cctxt alias_name contract =
  Raw_contract_alias.add ~force cctxt alias_name contract >>=? fun () ->
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
    Origination
      {
        delegate;
        script = {code; storage};
        credit = balance;
        preorigination = None;
      }
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
  >>=? fun (oph, op, result) ->
  (match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result))
  >>=? fun res ->
  Lwt.return (Injection.originated_contracts ~force:false result) >>=? function
  | [contract] -> return (res, contract)
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
  >>=? fun (oph, op, result) ->
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
  entrypoint : string option;
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
       (opt "entrypoint" string))

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
      let sk : Signature.V0.Secret_key.t =
        Ed25519
          (Data_encoding.Binary.of_bytes_exn
             Signature.Ed25519.Secret_key.encoding
             sk)
      in
      let pk = Signature.V0.Secret_key.to_public_key sk in
      let pkh = Signature.V0.Public_key.hash pk in
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
    ~fee_parameter:Injection.dummy_fee_parameter
    contents
  >>=? fun (oph, op, result) ->
  (match confirmations with
  | None -> return_unit
  | Some _confirmations ->
      Alpha_services.Contract.balance
        cctxt
        (chain, block)
        (Contract.implicit_contract (Ed25519 pkh))
      >>=? fun balance ->
      cctxt#message
        "Account %s (%a) activated with %s%a."
        alias
        Signature.Ed25519.Public_key_hash.pp
        pkh
        Client_proto_args.tez_sym
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
    (Signature.V0.Public_key_hash.equal pkh (Ed25519 key.pkh))
    (error_of_fmt
       "@[<v 2>Inconsistent activation key:@ Computed pkh: %a@ Embedded pkh: \
        %a @]"
       Signature.V0.Public_key_hash.pp
       pkh
       Signature.Ed25519.Public_key_hash.pp
       key.pkh)
  >>=? fun () ->
  let pk = Signature.Of_V0.public_key pk in
  let sk = Signature.Of_V0.secret_key sk in
  Tezos_signer_backends.Unencrypted.make_pk pk >>?= fun pk_uri ->
  (if encrypted then
   Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt cctxt sk
  else Tezos_signer_backends.Unencrypted.make_sk sk >>?= return)
  >>=? fun sk_uri ->
  Client_keys_v0.register_key cctxt ?force (pkh, pk_uri, sk_uri) name
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
  Client_keys_v0.alias_keys cctxt alias >>=? function
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
  supermajority : Int32.t;
  ballots : Vote.ballots;
}

let get_ballots_info (cctxt : #full) ~chain ~block =
  (* Get the next level, not the current *)
  let cb = (chain, block) in
  Alpha_services.Voting.ballots cctxt cb >>=? fun ballots ->
  Alpha_services.Voting.current_quorum cctxt cb >>=? fun current_quorum ->
  Alpha_services.Voting.listings cctxt cb >>=? fun listings ->
  let max_participation =
    List.fold_left (fun acc (_, w) -> Int32.add w acc) 0l listings
  in
  let all_votes = Int32.(add (add ballots.yay ballots.nay) ballots.pass) in
  let participation = Int32.(div (mul all_votes 100_00l) max_participation) in
  let supermajority = Int32.(div (mul 8l (add ballots.yay ballots.nay)) 10l) in
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
    ~fee_parameter:Injection.dummy_fee_parameter
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
    ~fee_parameter:Injection.dummy_fee_parameter
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
