(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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
open Client_proto_args
open Tezos_micheline

let group =
  {
    Tezos_clic.name = "multisig";
    title = "Commands for managing a multisig smart contract";
  }

let threshold_param () =
  Tezos_clic.param
    ~name:"threshold"
    ~desc:"Number of required signatures"
    Client_proto_args.int_parameter

let public_key_param () =
  Client_keys_v0.Public_key.source_param
    ~name:"key"
    ~desc:"Each signer of the multisig contract"

let secret_key_param () =
  Client_keys_v0.Secret_key.source_param
    ~name:"key"
    ~desc:
      "Secret key corresponding to one of the public keys stored on the \
       multisig contract"

let signature_param () =
  Tezos_clic.param
    ~name:"signature"
    ~desc:"Each signer of the multisig contract"
    Client_proto_args.signature_parameter

let lambda_param () =
  Tezos_clic.param
    ~name:"lambda"
    ~desc:"the lambda to execute, of type lambda unit (list operation)"
    string_parameter

let bytes_only_switch =
  Tezos_clic.switch
    ~long:"bytes-only"
    ~doc:"return only the byte sequence to be signed"
    ()

let bytes_param ~name ~desc =
  Tezos_clic.param ~name ~desc Client_proto_args.bytes_parameter

let transfer_options =
  Tezos_clic.args10
    Client_proto_args.fee_arg
    Client_proto_context_commands.dry_run_switch
    Client_proto_context_commands.verbose_signing_switch
    Client_proto_args.gas_limit_arg
    Client_proto_args.storage_limit_arg
    Client_proto_args.counter_arg
    Client_proto_args.arg_arg
    Client_proto_args.no_print_source_flag
    Client_proto_args.fee_parameter_args
    Client_proto_args.entrypoint_arg

let non_transfer_options =
  Tezos_clic.args8
    Client_proto_args.fee_arg
    Client_proto_context_commands.dry_run_switch
    Client_proto_context_commands.verbose_signing_switch
    Client_proto_args.gas_limit_arg
    Client_proto_args.storage_limit_arg
    Client_proto_args.counter_arg
    Client_proto_args.no_print_source_flag
    Client_proto_args.fee_parameter_args

let prepare_command_display prepared_command bytes_only =
  if bytes_only then
    Format.printf
      "0x%a@."
      Hex.pp
      (Hex.of_bytes prepared_command.Client_proto_multisig.bytes)
  else
    Format.printf
      "%a@.%a@.%a@.%a@."
      (fun ppf x ->
        Format.fprintf ppf "Bytes to sign: '0x%a'" Hex.pp (Hex.of_bytes x))
      prepared_command.Client_proto_multisig.bytes
      (fun ppf x ->
        Format.fprintf
          ppf
          "Blake 2B Hash: '%s'"
          (Tezos_crypto.Base58.raw_encode
             Tezos_crypto.Blake2B.(hash_bytes [x] |> to_string)))
      prepared_command.Client_proto_multisig.bytes
      (fun ppf z ->
        Format.fprintf
          ppf
          "Threshold (number of signatures required): %s"
          (Z.to_string z))
      prepared_command.Client_proto_multisig.threshold
      (fun ppf ->
        Format.fprintf
          ppf
          "@[<2>Public keys of the signers:@ %a@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
             Tezos_crypto.Signature.V0.Public_key.pp))
      prepared_command.Client_proto_multisig.keys

let get_parameter_type (cctxt : #Protocol_client_context.full)
    ~(destination : Contract.t) ~entrypoint =
  match destination with
  | Implicit _ ->
      let open Micheline in
      return @@ strip_locations @@ Prim (0, Script.T_unit, [], [])
  | Originated contract -> (
      Michelson_v1_entrypoints.contract_entrypoint_type
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~contract
        ~entrypoint
        ~normalize_types:true
      >>=? function
      | None ->
          cctxt#error
            "Contract %a has no entrypoint named %a"
            Contract_hash.pp
            contract
            Entrypoint.pp
            entrypoint
      | Some parameter_type -> return parameter_type)

let commands_ro () : #Protocol_client_context.full Tezos_clic.command list =
  Tezos_clic.
    [
      command
        ~group
        ~desc:"Show the hashes of the supported multisig contracts."
        no_options
        (fixed ["show"; "supported"; "multisig"; "hashes"])
        (fun () _cctxt ->
          Format.printf "Hashes of supported multisig contracts:@." ;
          List.iter
            (fun h -> Format.printf "%a@." Script_expr_hash.pp h)
            Client_proto_multisig.known_multisig_hashes ;
          return_unit);
      command
        ~group
        ~desc:"Show the script of the recommended multisig contract."
        no_options
        (fixed ["show"; "multisig"; "script"])
        (fun () _cctxt ->
          let {Michelson_v1_parser.source; _} =
            Michelson_v1_printer.unparse_toplevel
              Client_proto_multisig.multisig_script
          in
          Format.printf "%s@." source ;
          return_unit);
    ]

let commands_rw () : #Protocol_client_context.full Tezos_clic.command list =
  Tezos_clic.
    [
      command
        ~group
        ~desc:"Originate a new multisig contract."
        (args9
           Client_proto_args.fee_arg
           Client_proto_context_commands.dry_run_switch
           Client_proto_args.gas_limit_arg
           Client_proto_args.storage_limit_arg
           Client_proto_args.delegate_arg
           (Client_keys_v0.force_switch ())
           Client_proto_args.no_print_source_flag
           Client_proto_args.fee_parameter_args
           Client_proto_context_commands.verbose_signing_switch)
        (prefixes ["deploy"; "multisig"]
        @@ Client_proto_contracts.Raw_contract_alias.fresh_alias_param
             ~name:"new_multisig"
             ~desc:"name of the new multisig contract"
        @@ prefix "transferring"
        @@ Client_proto_args.tez_param
             ~name:"qty"
             ~desc:"amount taken from source"
        @@ prefix "from"
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"src"
             ~desc:"name of the source contract"
        @@ prefixes ["with"; "threshold"]
        @@ threshold_param ()
        @@ prefixes ["on"; "public"; "keys"]
        @@ seq_of_param (public_key_param ()))
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               delegate,
               force,
               no_print_source,
               fee_parameter,
               verbose_signing )
             alias_name
             balance
             source
             threshold
             keys
             (cctxt : #Protocol_client_context.full)
           ->
          Client_proto_contracts.Raw_contract_alias.of_fresh
            cctxt
            force
            alias_name
          >>=? fun alias_name ->
          match source with
          | Originated _ ->
              failwith
                "only implicit accounts can be the source of an origination"
          | Implicit source -> (
              Client_keys_v0.get_key cctxt source
              >>=? fun (_, src_pk, src_sk) ->
              List.map_es
                (fun (pk_uri, _) -> Client_keys_v0.public_key pk_uri)
                keys
              >>=? fun keys ->
              Client_proto_multisig.originate_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ?fee
                ?gas_limit
                ?storage_limit
                ~verbose_signing
                ~delegate
                ~threshold:(Z.of_int threshold)
                ~keys
                ~balance
                ~source
                ~src_pk
                ~src_sk
                ~fee_parameter
                ()
              >>= fun errors ->
              Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"multisig origination simulation failed"
                cctxt
                errors
              >>= function
              | None -> return_unit
              | Some (_res, contract) ->
                  if dry_run then return_unit
                  else
                    Client_proto_context.save_contract
                      ~force
                      cctxt
                      alias_name
                      contract
                    >>=? fun () -> return_unit));
      command
        ~group
        ~desc:"Sign a transaction for a multisig contract."
        (args2 arg_arg entrypoint_arg)
        (prefixes ["sign"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefix "transferring"
        @@ Client_proto_args.tez_param
             ~name:"qty"
             ~desc:"amount taken from source"
        @@ prefix "to"
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"dst"
             ~desc:"name/literal of the destination contract"
        @@ prefixes ["using"; "secret"; "key"]
        @@ secret_key_param () @@ stop)
        (fun (parameter, entrypoint)
             multisig_contract
             amount
             destination
             sk
             (cctxt : #Protocol_client_context.full)
           ->
          let entrypoint =
            Option.value ~default:Entrypoint.default entrypoint
          in
          let parameter = Option.value ~default:"Unit" parameter in
          Lwt.return @@ Micheline_parser.no_parsing_error
          @@ Michelson_v1_parser.parse_expression parameter
          >>=? fun {expanded = parameter; _} ->
          get_parameter_type cctxt ~destination ~entrypoint
          >>=? fun parameter_type ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:
              (Client_proto_multisig.Transfer
                 {amount; destination; entrypoint; parameter_type; parameter})
            ()
          >>=? fun prepared_command ->
          Client_keys_v0.sign cctxt sk prepared_command.bytes
          >>=? fun signature ->
          return @@ Format.printf "%a@." Tezos_crypto.Signature.V0.pp signature);
      command
        ~group
        ~desc:"Sign a lambda for a generic multisig contract."
        no_options
        (prefixes ["sign"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["running"; "lambda"]
        @@ lambda_param ()
        @@ prefixes ["using"; "secret"; "key"]
        @@ secret_key_param () @@ stop)
        (fun ()
             multisig_contract
             lambda
             sk
             (cctxt : #Protocol_client_context.full)
           ->
          Lwt.return @@ Micheline_parser.no_parsing_error
          @@ Michelson_v1_parser.parse_expression lambda
          >>=? fun {expanded = lambda; _} ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:(Lambda lambda)
            ()
          >>=? fun prepared_command ->
          Client_keys_v0.sign cctxt sk prepared_command.bytes
          >>=? fun signature ->
          return @@ Format.printf "%a@." Tezos_crypto.Signature.V0.pp signature);
      command
        ~group
        ~desc:"Sign a delegate change for a multisig contract."
        no_options
        (prefixes ["sign"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["setting"; "delegate"; "to"]
        @@ Client_keys_v0.Public_key_hash.source_param
             ~name:"dlgt"
             ~desc:"new delegate of the new multisig contract"
        @@ prefixes ["using"; "secret"; "key"]
        @@ secret_key_param () @@ stop)
        (fun ()
             multisig_contract
             delegate
             sk
             (cctxt : #Protocol_client_context.full)
           ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:(Client_proto_multisig.Change_delegate (Some delegate))
            ()
          >>=? fun prepared_command ->
          Client_keys_v0.sign cctxt sk prepared_command.bytes
          >>=? fun signature ->
          return @@ Format.printf "%a@." Tezos_crypto.Signature.V0.pp signature);
      command
        ~group
        ~desc:"Sign a delegate withdraw for a multisig contract."
        no_options
        (prefixes ["sign"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["withdrawing"; "delegate"]
        @@ prefixes ["using"; "secret"; "key"]
        @@ secret_key_param () @@ stop)
        (fun () multisig_contract sk (cctxt : #Protocol_client_context.full) ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:(Client_proto_multisig.Change_delegate None)
            ()
          >>=? fun prepared_command ->
          Client_keys_v0.sign cctxt sk prepared_command.bytes
          >>=? fun signature ->
          return @@ Format.printf "%a@." Tezos_crypto.Signature.V0.pp signature);
      command
        ~group
        ~desc:
          "Sign a change of public keys and threshold for a multisig contract."
        no_options
        (prefixes ["sign"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["using"; "secret"; "key"]
        @@ secret_key_param ()
        @@ prefixes ["setting"; "threshold"; "to"]
        @@ threshold_param ()
        @@ prefixes ["and"; "public"; "keys"; "to"]
        @@ seq_of_param (public_key_param ()))
        (fun ()
             multisig_contract
             sk
             new_threshold
             new_keys
             (cctxt : #Protocol_client_context.full)
           ->
          List.map_es
            (fun (pk_uri, _) -> Client_keys_v0.public_key pk_uri)
            new_keys
          >>=? fun keys ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:
              (Client_proto_multisig.Change_keys (Z.of_int new_threshold, keys))
            ()
          >>=? fun prepared_command ->
          Client_keys_v0.sign cctxt sk prepared_command.bytes
          >>=? fun signature ->
          return @@ Format.printf "%a@." Tezos_crypto.Signature.V0.pp signature);
      command
        ~group
        ~desc:"Transfer tokens using a multisig contract."
        transfer_options
        (prefixes ["from"; "multisig"; "contract"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name/literal of the multisig contract"
        @@ prefix "transfer"
        @@ Client_proto_args.tez_param
             ~name:"qty"
             ~desc:"amount taken from the multisig contract"
        @@ prefix "to"
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"dst"
             ~desc:"name/literal of the destination contract"
        @@ prefixes ["on"; "behalf"; "of"]
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"src"
             ~desc:"source calling the multisig contract"
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()))
        (fun ( fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               parameter,
               no_print_source,
               fee_parameter,
               entrypoint )
             multisig_contract
             amount
             destination
             source
             signatures
             (cctxt : #Protocol_client_context.full)
           ->
          let entrypoint =
            Option.value ~default:Entrypoint.default entrypoint
          in
          let parameter = Option.value ~default:"Unit" parameter in
          Lwt.return @@ Micheline_parser.no_parsing_error
          @@ Michelson_v1_parser.parse_expression parameter
          >>=? fun {expanded = parameter; _} ->
          get_parameter_type cctxt ~destination ~entrypoint
          >>=? fun parameter_type ->
          match source with
          | Originated _ ->
              failwith
                "only implicit accounts can be the source of a contract call"
          | Implicit source -> (
              Client_keys_v0.get_key cctxt source
              >>=? fun (_, src_pk, src_sk) ->
              Client_proto_multisig.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~verbose_signing
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~multisig_contract
                ~action:
                  (Client_proto_multisig.Transfer
                     {
                       amount;
                       destination;
                       entrypoint;
                       parameter_type;
                       parameter;
                     })
                ~signatures
                ~amount:Tez.zero
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit
              | Some (_res, _contracts) -> return_unit));
      command
        ~group
        ~desc:"Run a lambda on a generic multisig contract."
        non_transfer_options
        (prefixes ["from"; "multisig"; "contract"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name/literal of the multisig contract"
        @@ prefixes ["run"; "lambda"]
        @@ lambda_param ()
        @@ prefixes ["on"; "behalf"; "of"]
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"src"
             ~desc:"source calling the multisig contract"
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()))
        (fun ( fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               fee_parameter )
             multisig_contract
             lambda
             source
             signatures
             (cctxt : #Protocol_client_context.full)
           ->
          match source with
          | Originated _ ->
              failwith
                "only implicit accounts can be the source of a contract call"
          | Implicit source -> (
              Client_keys_v0.get_key cctxt source
              >>=? fun (_, src_pk, src_sk) ->
              Lwt.return @@ Micheline_parser.no_parsing_error
              @@ Michelson_v1_parser.parse_expression lambda
              >>=? fun {expanded = lambda; _} ->
              Client_proto_multisig.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~verbose_signing
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~multisig_contract
                ~action:(Client_proto_multisig.Lambda lambda)
                ~signatures
                ~amount:Tez.zero
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit
              | Some (_res, _contracts) -> return_unit));
      command
        ~group
        ~desc:"Change the delegate of a multisig contract."
        non_transfer_options
        (prefixes ["set"; "delegate"; "of"; "multisig"; "contract"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefix "to"
        @@ Client_keys_v0.Public_key_hash.source_param
             ~name:"dlgt"
             ~desc:"new delegate of the new multisig contract"
        @@ prefixes ["on"; "behalf"; "of"]
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"src"
             ~desc:"source calling the multisig contract"
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()))
        (fun ( fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               fee_parameter )
             multisig_contract
             delegate
             source
             signatures
             (cctxt : #Protocol_client_context.full)
           ->
          match source with
          | Originated _ ->
              failwith
                "only implicit accounts can be the source of a contract call"
          | Implicit source -> (
              Client_keys_v0.get_key cctxt source
              >>=? fun (_, src_pk, src_sk) ->
              Client_proto_multisig.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~verbose_signing
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~multisig_contract
                ~action:(Client_proto_multisig.Change_delegate (Some delegate))
                ~signatures
                ~amount:Tez.zero
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit
              | Some (_res, _contracts) -> return_unit));
      command
        ~group
        ~desc:"Withdraw the delegate of a multisig contract."
        non_transfer_options
        (prefixes ["withdraw"; "delegate"; "of"; "multisig"; "contract"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["on"; "behalf"; "of"]
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"src"
             ~desc:"source calling the multisig contract"
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()))
        (fun ( fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               fee_parameter )
             multisig_contract
             source
             signatures
             (cctxt : #Protocol_client_context.full)
           ->
          match source with
          | Originated _ ->
              failwith
                "only implicit accounts can be the source of a contract call"
          | Implicit source -> (
              Client_keys_v0.get_key cctxt source
              >>=? fun (_, src_pk, src_sk) ->
              Client_proto_multisig.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~verbose_signing
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~multisig_contract
                ~action:(Client_proto_multisig.Change_delegate None)
                ~signatures
                ~amount:Tez.zero
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit
              | Some (_res, _contracts) -> return_unit));
      command
        ~group
        ~desc:"Change public keys and threshold for a multisig contract."
        non_transfer_options
        (prefixes ["set"; "threshold"; "of"; "multisig"; "contract"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["to"] @@ threshold_param ()
        @@ prefixes ["and"; "public"; "keys"; "to"]
        @@ non_terminal_seq (public_key_param ()) ~suffix:["on"; "behalf"; "of"]
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"src"
             ~desc:"source calling the multisig contract"
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()))
        (fun ( fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               fee_parameter )
             multisig_contract
             new_threshold
             new_keys
             source
             signatures
             (cctxt : #Protocol_client_context.full)
           ->
          match source with
          | Originated _ ->
              failwith
                "only implicit accounts can be the source of a contract call"
          | Implicit source -> (
              Client_keys_v0.get_key cctxt source
              >>=? fun (_, src_pk, src_sk) ->
              List.map_es
                (fun (pk_uri, _) -> Client_keys_v0.public_key pk_uri)
                new_keys
              >>=? fun keys ->
              Client_proto_multisig.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~verbose_signing
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~multisig_contract
                ~action:
                  (Client_proto_multisig.Change_keys
                     (Z.of_int new_threshold, keys))
                ~signatures
                ~amount:Tez.zero
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit
              | Some (_res, _contracts) -> return_unit));
      (* This command is no longer necessary as Tezos_clic now supports non terminal
         lists of parameters, however, it is kept for compatibility. *)
      command
        ~group
        ~desc:
          "Run a transaction described by a sequence of bytes on a multisig \
           contract."
        non_transfer_options
        (prefixes ["run"; "transaction"]
        @@ bytes_param
             ~name:"bytes"
             ~desc:
               "the sequence of bytes to deserialize as a multisig action, can \
                be obtained by one of the \"prepare multisig transaction\" \
                commands"
        @@ prefixes ["on"; "multisig"; "contract"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["on"; "behalf"; "of"]
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"src"
             ~desc:"source calling the multisig contract"
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()))
        (fun ( fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               fee_parameter )
             bytes
             multisig_contract
             source
             signatures
             (cctxt : #Protocol_client_context.full)
           ->
          match source with
          | Originated _ ->
              failwith
                "only implicit accounts can be the source of a contract call"
          | Implicit source -> (
              Client_keys_v0.get_key cctxt source
              >>=? fun (_, src_pk, src_sk) ->
              Client_proto_multisig.call_multisig_on_bytes
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~verbose_signing
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~multisig_contract
                ~bytes
                ~signatures
                ~amount:Tez.zero
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit
              | Some (_res, _contracts) -> return_unit));
      command
        ~group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign for a \
           multisigned transfer."
        (args3 bytes_only_switch arg_arg entrypoint_arg)
        (prefixes ["prepare"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefix "transferring"
        @@ Client_proto_args.tez_param
             ~name:"qty"
             ~desc:"amount taken from source"
        @@ prefix "to"
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"dst"
             ~desc:"name/literal of the destination contract"
        @@ stop)
        (fun (bytes_only, parameter, entrypoint)
             multisig_contract
             amount
             destination
             (cctxt : #Protocol_client_context.full)
           ->
          let entrypoint =
            Option.value ~default:Entrypoint.default entrypoint
          in
          let parameter = Option.value ~default:"Unit" parameter in
          Lwt.return @@ Micheline_parser.no_parsing_error
          @@ Michelson_v1_parser.parse_expression parameter
          >>=? fun {expanded = parameter; _} ->
          get_parameter_type cctxt ~destination ~entrypoint
          >>=? fun parameter_type ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:
              (Client_proto_multisig.Transfer
                 {amount; destination; entrypoint; parameter_type; parameter})
            ()
          >>=? fun prepared_command ->
          return @@ prepare_command_display prepared_command bytes_only);
      command
        ~group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign for a \
           multisigned lambda execution in a generic multisig contract."
        (args1 bytes_only_switch)
        (prefixes ["prepare"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["running"; "lambda"]
        @@ lambda_param () @@ stop)
        (fun bytes_only
             multisig_contract
             lambda
             (cctxt : #Protocol_client_context.full)
           ->
          Lwt.return @@ Micheline_parser.no_parsing_error
          @@ Michelson_v1_parser.parse_expression lambda
          >>=? fun {expanded = lambda; _} ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:(Client_proto_multisig.Lambda lambda)
            ()
          >>=? fun prepared_command ->
          return @@ prepare_command_display prepared_command bytes_only);
      command
        ~group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign for a \
           multisigned delegate change."
        (args1 bytes_only_switch)
        (prefixes ["prepare"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["setting"; "delegate"; "to"]
        @@ Client_keys_v0.Public_key_hash.source_param
             ~name:"dlgt"
             ~desc:"new delegate of the new multisig contract"
        @@ stop)
        (fun bytes_only
             multisig_contract
             new_delegate
             (cctxt : #Protocol_client_context.full)
           ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:(Client_proto_multisig.Change_delegate (Some new_delegate))
            ()
          >>=? fun prepared_command ->
          return @@ prepare_command_display prepared_command bytes_only);
      command
        ~group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign for a \
           multisigned delegate withdraw."
        (args1 bytes_only_switch)
        (prefixes ["prepare"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["withdrawing"; "delegate"]
        @@ stop)
        (fun bytes_only
             multisig_contract
             (cctxt : #Protocol_client_context.full)
           ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:(Client_proto_multisig.Change_delegate None)
            ()
          >>=? fun prepared_command ->
          return @@ prepare_command_display prepared_command bytes_only);
      command
        ~group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign for a \
           multisigned change of keys and threshold."
        (args1 bytes_only_switch)
        (prefixes ["prepare"; "multisig"; "transaction"; "on"]
        @@ Client_proto_contracts.Originated_contract_alias.destination_param
             ~name:"multisig"
             ~desc:"name or address of the originated multisig contract"
        @@ prefixes ["setting"; "threshold"; "to"]
        @@ threshold_param ()
        @@ prefixes ["and"; "public"; "keys"; "to"]
        @@ seq_of_param (public_key_param ()))
        (fun bytes_only
             multisig_contract
             new_threshold
             new_keys
             (cctxt : #Protocol_client_context.full)
           ->
          List.map_es
            (fun (pk_uri, _) -> Client_keys_v0.public_key pk_uri)
            new_keys
          >>=? fun keys ->
          Client_proto_multisig.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~multisig_contract
            ~action:
              (Client_proto_multisig.Change_keys (Z.of_int new_threshold, keys))
            ()
          >>=? fun prepared_command ->
          return @@ prepare_command_display prepared_command bytes_only);
    ]

let commands () = commands_ro () @ commands_rw ()
