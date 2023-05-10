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

let get_storage (rpc : #rpc_context) ~chain ~block contract =
  Alpha_services.Contract.storage_opt rpc (chain, block) contract

let get_big_map_value (rpc : #rpc_context) ~chain ~block id key =
  Alpha_services.Contract.big_map_get rpc (chain, block) id key

let get_contract_big_map_value (rpc : #rpc_context) ~chain ~block contract key =
  Alpha_services.Contract.contract_big_map_get_opt
    rpc
    (chain, block)
    contract
    key

let get_script (rpc : #rpc_context) ~chain ~block contract =
  Alpha_services.Contract.script_opt rpc (chain, block) contract

let parse_expression arg =
  Lwt.return
    (Micheline_parser.no_parsing_error
       (Michelson_v1_parser.parse_expression arg))

let transfer (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?branch ~source ~src_pk ~src_sk ~destination
    ?(entrypoint = "default") ?arg ~amount ?fee ?gas_limit ?storage_limit
    ?counter ~fee_parameter () =
  (match arg with
  | Some arg ->
      parse_expression arg >>=? fun {expanded = arg; _} -> return_some arg
  | None -> return_none)
  >>=? fun parameters ->
  let parameters =
    Option.fold ~some:Script.lazy_expr ~none:Script.unit_parameter parameters
  in
  let contents = Transaction {amount; parameters; destination; entrypoint} in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ~src_pk
    ~src_sk
    ~fee_parameter
    contents
  >>=? fun ((_oph, _op, result) as res) ->
  Lwt.return (Injection.originated_contracts (Single_result result))
  >>=? fun contracts -> return (res, contracts)

let reveal cctxt ~chain ~block ?confirmations ?dry_run ?verbose_signing ?branch
    ~source ~src_pk ~src_sk ?fee ~fee_parameter () =
  let compute_fee, fee =
    match fee with None -> (true, Tez.zero) | Some fee -> (false, fee)
  in
  Alpha_services.Contract.counter cctxt (chain, block) source
  >>=? fun pcounter ->
  let counter = Z.succ pcounter in
  Alpha_services.Contract.manager_key cctxt (chain, block) source
  >>=? fun key ->
  match key with
  | Some _ -> failwith "The manager key was previously revealed."
  | None -> (
      let contents =
        Single
          (Manager_operation
             {
               source;
               fee;
               counter;
               gas_limit = Z.of_int ~-1;
               storage_limit = Z.zero;
               operation = Reveal src_pk;
             })
      in
      Injection.inject_operation
        cctxt
        ~chain
        ~block
        ?confirmations
        ?dry_run
        ?verbose_signing
        ?branch
        ~src_sk
        ~compute_fee
        ~fee_parameter
        contents
      >>=? fun (oph, op, result) ->
      match Apply_results.pack_contents_list op result with
      | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
          return (oph, op, result))

let delegate_contract cctxt ~chain ~block ?branch ?confirmations ?dry_run
    ?verbose_signing ~source ~src_pk ~src_sk ?fee ~fee_parameter delegate_opt =
  let operation = Delegation delegate_opt in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ?fee
    ~storage_limit:Z.zero
    ~src_pk
    ~src_sk
    ~fee_parameter
    operation
  >>=? fun res -> return res

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
    ?fee contract ~src_pk ~manager_sk ~fee_parameter opt_delegate =
  delegate_contract
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
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

let save_contract ~force cctxt alias_name contract =
  Raw_contract_alias.add ~force cctxt alias_name contract >>=? fun () ->
  message_added_contract cctxt alias_name >>= fun () -> return_unit

let originate_contract (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?branch ?fee ?gas_limit ?storage_limit ~delegate
    ~initial_storage ~balance ~source ~src_pk ~src_sk ~code ~fee_parameter () =
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
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ?fee
    ?gas_limit
    ?storage_limit
    ~src_pk
    ~src_sk
    ~fee_parameter
    origination
  >>=? fun ((_oph, _op, result) as res) ->
  Lwt.return (Injection.originated_contracts (Single_result result))
  >>=? function
  | [contract] -> return (res, contract)
  | contracts ->
      failwith
        "The origination introduced %d contracts instead of one."
        (List.length contracts)

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

let get_period_info (cctxt : #full) ~chain ~block =
  (* Get the next level, not the current *)
  let cb = (chain, block) in
  Alpha_services.Helpers.current_level cctxt ~offset:1l cb >>=? fun level ->
  Alpha_services.Constants.all cctxt cb >>=? fun constants ->
  Alpha_services.Voting.current_proposal cctxt cb >>=? fun current_proposal ->
  let position = level.voting_period_position in
  let remaining =
    Int32.(sub constants.parametric.blocks_per_voting_period position)
  in
  Alpha_services.Voting.current_period_kind cctxt cb
  >>=? fun current_period_kind ->
  return {current_period_kind; position; remaining; current_proposal}

let get_proposals (cctxt : #full) ~chain ~block =
  let cb = (chain, block) in
  Alpha_services.Voting.proposals cctxt cb

let submit_proposals ?dry_run ?verbose_signing (cctxt : #full) ~chain ~block
    ?confirmations ~src_sk source proposals =
  (* We need the next level, not the current *)
  Alpha_services.Helpers.current_level cctxt ~offset:1l (chain, block)
  >>=? fun (level : Level.t) ->
  let period = level.voting_period in
  let contents = Single (Proposals {source; period; proposals}) in
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
  Alpha_services.Helpers.current_level cctxt ~offset:1l (chain, block)
  >>=? fun (level : Level.t) ->
  let period = level.voting_period in
  let contents = Single (Ballot {source; period; proposal; ballot}) in
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
