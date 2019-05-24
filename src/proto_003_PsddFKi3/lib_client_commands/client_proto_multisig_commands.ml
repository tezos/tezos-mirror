(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Error_monad

let group =
  { Clic.name = "multisig" ;
    title = "Commands for managing a multisig smart contract" }

let threshold_param () =
  Clic.param
    ~name:"threshold"
    ~desc:"Number of required signatures"
    Client_proto_args.int_parameter

let public_key_param () =
  Client_keys.Public_key.source_param
    ~name:"key"
    ~desc:"Each signer of the multisig contract"

let secret_key_param () =
  Client_keys.Secret_key.source_param
    ~name:"key"
    ~desc:"Secret key corresponding to one of the public keys stored on the multisig contract"

let signature_param () =
  Clic.param
    ~name:"signature"
    ~desc:"Each signer of the multisig contract"
    Client_proto_args.signature_parameter

let bytes_only_switch =
  Clic.switch
    ~long: "bytes-only"
    ~doc: "return only the byte sequence to be signed"
    ()

let bytes_param ~name ~desc =
  Clic.param ~name ~desc
    Client_proto_args.bytes_parameter

let transfer_options =
  Clic.args12
    Client_proto_args.fee_arg
    Client_proto_context_commands.dry_run_switch
    Client_proto_args.gas_limit_arg
    Client_proto_args.storage_limit_arg
    Client_proto_args.counter_arg
    Client_proto_args.no_print_source_flag
    Client_proto_args.minimal_fees_arg
    Client_proto_args.minimal_nanotez_per_byte_arg
    Client_proto_args.minimal_nanotez_per_gas_unit_arg
    Client_proto_args.force_low_fee_arg
    Client_proto_args.fee_cap_arg
    Client_proto_args.burn_cap_arg

let commands () =
  Clic.[
    command ~group ~desc: "Originate a new multisig contract."
      (args15
         Client_proto_args.fee_arg
         Client_proto_context_commands.dry_run_switch
         Client_proto_args.gas_limit_arg
         Client_proto_args.storage_limit_arg
         Client_proto_args.delegate_arg
         (Client_keys.force_switch ())
         Client_proto_args.delegatable_switch
         Client_proto_args.spendable_switch
         Client_proto_args.no_print_source_flag
         Client_proto_args.minimal_fees_arg
         Client_proto_args.minimal_nanotez_per_byte_arg
         Client_proto_args.minimal_nanotez_per_gas_unit_arg
         Client_proto_args.force_low_fee_arg
         Client_proto_args.fee_cap_arg
         Client_proto_args.burn_cap_arg)
      (prefixes ["deploy"; "multisig"]
       @@ Client_proto_contracts.RawContractAlias.fresh_alias_param
         ~name: "new_multisig" ~desc: "name of the new multisig contract"
       @@ prefix "for"
       @@ Client_keys.Public_key_hash.source_param
         ~name: "mgr" ~desc: "manager of the new multisig contract"
       @@ prefix "transferring"
       @@ Client_proto_args.tez_param
         ~name: "qty" ~desc: "amount taken from source"
       @@ prefix "from"
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name:"src" ~desc: "name of the source contract"
       @@ prefixes ["with"; "threshold"]
       @@ threshold_param ()
       @@ prefixes ["on"; "public"; "keys"]
       @@ (seq_of_param (public_key_param ())))
      begin fun (fee, dry_run, gas_limit, storage_limit, delegate, force,
                 delegatable, spendable, no_print_source,
                 minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap,
                 burn_cap)
        alias_name manager balance (_, source) threshold keys
        (cctxt : Proto_alpha.full) ->
        Client_proto_contracts.RawContractAlias.of_fresh
          cctxt force alias_name >>=? fun alias_name ->
        Client_proto_context.source_to_keys cctxt ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        map_s (fun (pk_uri, _) -> Client_keys.public_key pk_uri) keys >>=?
        fun keys ->
        Client_proto_multisig.originate_multisig cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ?fee ?gas_limit ?storage_limit ~delegate ~delegatable ~spendable
          ~threshold:(Z.of_int threshold) ~keys
          ~manager ~balance ~source ~src_pk ~src_sk
          ~fee_parameter
          () >>= fun errors ->
        Client_proto_context_commands.report_michelson_errors ~no_print_source
          ~msg:"multisig origination simulation failed" cctxt errors >>= function
        | None -> return_unit
        | Some (_res, contract) ->
            if dry_run then
              return_unit
            else
              Client_proto_context.save_contract ~force cctxt alias_name
                contract >>=? fun () -> return_unit
      end;

    command ~group ~desc: "Display the threshold, public keys, and byte sequence to sign for a multisigned transfer."
      (args1 bytes_only_switch)
      (prefixes ["prepare"; "multisig"; "transaction"; "on"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefix "transferring"
       @@ Client_proto_args.tez_param
         ~name: "qty" ~desc: "amount taken from source"
       @@ prefix "to"
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name: "dst" ~desc: "name/literal of the destination contract"
       @@ stop)
      begin fun (bytes_only) (_, multisig_contract) amount (_, destination)
        (cctxt : Proto_alpha.full) ->
        Client_proto_multisig.prepare_multisig_transaction cctxt
          ~chain:cctxt#chain ~block:cctxt#block ~multisig_contract
          ~action:(Client_proto_multisig.Transfer (amount, destination))
          () >>=? fun prepared_command ->
        return @@
        if bytes_only then
          Format.printf "0x%a@."
            MBytes.pp_hex
            prepared_command.Client_proto_multisig.bytes
        else
          Format.printf "%a@.%a@.%a@."
            (fun ppf -> Format.fprintf ppf "Bytes to sign: '0x%a'" MBytes.pp_hex)
            prepared_command.Client_proto_multisig.bytes
            (fun ppf z ->
               Format.fprintf ppf
                 "Threshold (number of signatures required): %s"
                 (Z.to_string z))
            prepared_command.Client_proto_multisig.threshold
            (fun ppf ->
               Format.fprintf ppf
                 "@[<2>Public keys of the signers:@ %a@]"
                 (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
                    Signature.Public_key.pp))
            prepared_command.Client_proto_multisig.keys
      end;

    command ~group ~desc: "Display the threshold, public keys, and byte sequence to sign for a multisigned delegate change."
      (args1 bytes_only_switch)
      (prefixes ["prepare"; "multisig"; "transaction"; "on"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefixes ["setting"; "delegate"; "to"]
       @@ Client_keys.Public_key_hash.source_param
         ~name: "dlgt" ~desc: "new delegate of the new multisig contract"
       @@ stop)
      begin fun (bytes_only) (_, multisig_contract)
        new_delegate (cctxt : Proto_alpha.full) ->
        Client_proto_multisig.prepare_multisig_transaction cctxt
          ~chain:cctxt#chain ~block:cctxt#block ~multisig_contract
          ~action:(Client_proto_multisig.Change_delegate (Some new_delegate))
          () >>=? fun prepared_command ->
        return @@
        if bytes_only then
          Format.printf "0x%a@."
            MBytes.pp_hex
            prepared_command.Client_proto_multisig.bytes
        else
          Format.printf "%a@.%a@.%a@."
            (fun ppf -> Format.fprintf ppf "Bytes to sign: '0x%a'" MBytes.pp_hex)
            prepared_command.Client_proto_multisig.bytes
            (fun ppf z ->
               Format.fprintf ppf
                 "Threshold (number of signatures required): %s"
                 (Z.to_string z))
            prepared_command.Client_proto_multisig.threshold
            (fun ppf ->
               Format.fprintf ppf
                 "@[<2>Public keys of the signers:@ %a@]"
                 (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
                    Signature.Public_key.pp))
            prepared_command.Client_proto_multisig.keys
      end;

    command ~group ~desc: "Display the threshold, public keys, and byte sequence to sign for a multisigned delegate withdraw."
      (args1 bytes_only_switch)
      (prefixes ["prepare"; "multisig"; "transaction"; "on"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefixes ["withdrawing"; "delegate"]
       @@ stop)
      begin fun (bytes_only) (_, multisig_contract) (cctxt : Proto_alpha.full) ->
        Client_proto_multisig.prepare_multisig_transaction cctxt
          ~chain:cctxt#chain ~block:cctxt#block ~multisig_contract
          ~action:(Client_proto_multisig.Change_delegate None)
          () >>=? fun prepared_command ->
        return @@
        if bytes_only then
          Format.printf "0x%a@."
            MBytes.pp_hex
            prepared_command.Client_proto_multisig.bytes
        else
          Format.printf "%a@.%a@.%a@."
            (fun ppf -> Format.fprintf ppf "Bytes to sign: '0x%a'" MBytes.pp_hex)
            prepared_command.Client_proto_multisig.bytes
            (fun ppf z ->
               Format.fprintf ppf
                 "Threshold (number of signatures required): %s"
                 (Z.to_string z))
            prepared_command.Client_proto_multisig.threshold
            (fun ppf ->
               Format.fprintf ppf
                 "@[<2>Public keys of the signers:@ %a@]"
                 (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
                    Signature.Public_key.pp))
            prepared_command.Client_proto_multisig.keys
      end;

    command ~group ~desc: "Display the threshold, public keys, and byte sequence to sign for a multisigned change of keys and threshold."
      (args1 bytes_only_switch)
      (prefixes ["prepare"; "multisig"; "transaction"; "on"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefixes ["setting"; "threshold"; "to"]
       @@ threshold_param ()
       @@ prefixes ["and"; "public"; "keys"; "to"]
       @@ (seq_of_param (public_key_param ())))
      begin fun (bytes_only) (_, multisig_contract)
        new_threshold new_keys (cctxt : Proto_alpha.full) ->
        map_s (fun (pk_uri, _) -> Client_keys.public_key pk_uri) new_keys >>=?
        fun keys ->
        Client_proto_multisig.prepare_multisig_transaction cctxt
          ~chain:cctxt#chain ~block:cctxt#block ~multisig_contract
          ~action:(Client_proto_multisig.Change_keys (Z.of_int new_threshold, keys))
          () >>=? fun prepared_command ->
        return @@
        if bytes_only then
          Format.printf "0x%a@."
            MBytes.pp_hex
            prepared_command.Client_proto_multisig.bytes
        else
          Format.printf "%a@.%a@.%a@."
            (fun ppf -> Format.fprintf ppf "Bytes to sign: '0x%a'" MBytes.pp_hex)
            prepared_command.Client_proto_multisig.bytes
            (fun ppf z ->
               Format.fprintf ppf
                 "Threshold (number of signatures required): %s"
                 (Z.to_string z))
            prepared_command.Client_proto_multisig.threshold
            (fun ppf ->
               Format.fprintf ppf
                 "@[<2>Public keys of the signers:@ %a@]"
                 (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
                    Signature.Public_key.pp))
            prepared_command.Client_proto_multisig.keys
      end;

    command ~group ~desc: "Sign a transaction for a multisig contract."
      no_options
      (prefixes ["sign"; "multisig"; "transaction"; "on"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefix "transferring"
       @@ Client_proto_args.tez_param
         ~name: "qty" ~desc: "amount taken from source"
       @@ prefix "to"
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name: "dst" ~desc: "name/literal of the destination contract"
       @@ prefixes ["using"; "secret"; "key"]
       @@ secret_key_param ()
       @@ stop)
      begin fun () (_, multisig_contract) amount (_, destination) (sk)
        (cctxt : Proto_alpha.full) ->
        Client_proto_multisig.prepare_multisig_transaction cctxt
          ~chain:cctxt#chain ~block:cctxt#block ~multisig_contract
          ~action:(Client_proto_multisig.Transfer (amount, destination))
          () >>=? fun prepared_command ->
        Client_keys.sign cctxt sk prepared_command.bytes >>=? fun signature ->
        return @@ Format.printf "%a@." Signature.pp signature
      end;

    command ~group ~desc: "Sign a delegate change for a multisig contract."
      no_options
      (prefixes ["sign"; "multisig"; "transaction"; "on"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefixes ["setting"; "delegate"; "to"]
       @@ Client_keys.Public_key_hash.source_param
         ~name: "dlgt" ~desc: "new delegate of the new multisig contract"
       @@ prefixes ["using"; "secret"; "key"]
       @@ secret_key_param ()
       @@ stop)
      begin fun () (_, multisig_contract) delegate (sk)
        (cctxt : Proto_alpha.full) ->
        Client_proto_multisig.prepare_multisig_transaction cctxt
          ~chain:cctxt#chain ~block:cctxt#block ~multisig_contract
          ~action:(Client_proto_multisig.Change_delegate (Some delegate))
          () >>=? fun prepared_command ->
        Client_keys.sign cctxt sk prepared_command.bytes >>=? fun signature ->
        return @@ Format.printf "%a@." Signature.pp signature
      end;

    command ~group ~desc: "Sign a delegate withdraw for a multisig contract."
      no_options
      (prefixes ["sign"; "multisig"; "transaction"; "on"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefixes ["withdrawing"; "delegate"]
       @@ prefixes ["using"; "secret"; "key"]
       @@ secret_key_param ()
       @@ stop)
      begin fun () (_, multisig_contract) (sk)
        (cctxt : Proto_alpha.full) ->
        Client_proto_multisig.prepare_multisig_transaction cctxt
          ~chain:cctxt#chain ~block:cctxt#block ~multisig_contract
          ~action:(Client_proto_multisig.Change_delegate None)
          () >>=? fun prepared_command ->
        Client_keys.sign cctxt sk prepared_command.bytes >>=? fun signature ->
        return @@ Format.printf "%a@." Signature.pp signature
      end;

    command ~group ~desc: "Sign a change of public keys and threshold for a multisig contract."
      no_options
      (prefixes ["sign"; "multisig"; "transaction"; "on"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefixes ["using"; "secret"; "key"]
       @@ secret_key_param ()
       @@ prefixes ["setting"; "threshold"; "to"]
       @@ threshold_param ()
       @@ prefixes ["and"; "public"; "keys"; "to"]
       @@ (seq_of_param (public_key_param ())))
      begin fun () (_, multisig_contract) sk new_threshold new_keys
        (cctxt : Proto_alpha.full) ->
        map_s (fun (pk_uri, _) -> Client_keys.public_key pk_uri) new_keys >>=?
        fun keys ->
        Client_proto_multisig.prepare_multisig_transaction cctxt
          ~chain:cctxt#chain ~block:cctxt#block ~multisig_contract
          ~action:(Client_proto_multisig.Change_keys (Z.of_int new_threshold, keys))
          () >>=? fun prepared_command ->
        Client_keys.sign cctxt sk prepared_command.bytes >>=? fun signature ->
        return @@ Format.printf "%a@." Signature.pp signature
      end;

    command ~group ~desc: "Transfer tokens using a multisig contract."
      transfer_options
      (prefixes ["from"; "multisig"; "contract"]
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name: "multisig" ~desc: "name/literal of the multisig contract"
       @@ prefix "transfer"
       @@ Client_proto_args.tez_param
         ~name: "qty" ~desc: "amount taken from the multisig contract"
       @@ prefix "to"
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name: "dst" ~desc: "name/literal of the destination contract"
       @@ prefixes ["on"; "behalf"; "of"]
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name: "src" ~desc: "source calling the multisig contract"
       @@ prefixes ["with"; "signatures"]
       @@ (seq_of_param (signature_param ())))
      begin fun (fee, dry_run, gas_limit, storage_limit, counter, no_print_source,
                 minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)
        (_, multisig_contract) amount (_, destination) (_, source) signatures cctxt ->
        Client_proto_context.source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        Client_proto_multisig.call_multisig cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee_parameter
          ~source ?fee ~src_pk ~src_sk ~multisig_contract
          ~action:(Client_proto_multisig.Transfer (amount, destination))
          ~signatures
          ~amount:Proto_alpha.Alpha_context.Tez.zero
          ?gas_limit ?storage_limit ?counter () >>=
        Client_proto_context_commands.report_michelson_errors ~no_print_source
          ~msg:"transfer simulation failed" cctxt >>= function
        | None -> return_unit
        | Some (_res, _contracts) ->
            return_unit
      end;

    command ~group ~desc: "Change the delegate of a multisig contract."
      transfer_options
      (prefixes ["set"; "delegate"; "of"; "multisig"; "contract"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefix "to"
       @@ Client_keys.Public_key_hash.source_param
         ~name: "dlgt" ~desc: "new delegate of the new multisig contract"
       @@ prefixes ["on"; "behalf"; "of"]
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name: "src" ~desc: "source calling the multisig contract"
       @@ prefixes ["with"; "signatures"]
       @@ (seq_of_param (signature_param ())))
      begin fun (fee, dry_run, gas_limit, storage_limit, counter, no_print_source,
                 minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)
        (_, multisig_contract) delegate (_, source) signatures cctxt ->
        Client_proto_context.source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        Client_proto_multisig.call_multisig cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee_parameter
          ~source ?fee ~src_pk ~src_sk ~multisig_contract
          ~action:(Client_proto_multisig.Change_delegate (Some delegate))
          ~signatures
          ~amount:Proto_alpha.Alpha_context.Tez.zero
          ?gas_limit ?storage_limit ?counter () >>=
        Client_proto_context_commands.report_michelson_errors ~no_print_source
          ~msg:"transfer simulation failed" cctxt >>= function
        | None -> return_unit
        | Some (_res, _contracts) ->
            return_unit
      end;

    command ~group ~desc: "Withdrow the delegate of a multisig contract."
      transfer_options
      (prefixes ["withdraw"; "delegate"; "of"; "multisig"; "contract"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefixes ["on"; "behalf"; "of"]
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name: "src" ~desc: "source calling the multisig contract"
       @@ prefixes ["with"; "signatures"]
       @@ (seq_of_param (signature_param ())))
      begin fun (fee, dry_run, gas_limit, storage_limit, counter, no_print_source,
                 minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)
        (_, multisig_contract) (_, source) signatures cctxt ->
        Client_proto_context.source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        Client_proto_multisig.call_multisig cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee_parameter
          ~source ?fee ~src_pk ~src_sk ~multisig_contract
          ~action:(Client_proto_multisig.Change_delegate None)
          ~signatures
          ~amount:Proto_alpha.Alpha_context.Tez.zero
          ?gas_limit ?storage_limit ?counter () >>=
        Client_proto_context_commands.report_michelson_errors ~no_print_source
          ~msg:"transfer simulation failed" cctxt >>= function
        | None -> return_unit
        | Some (_res, _contracts) ->
            return_unit
      end;

    (* Unfortunately, Clic does not support non terminal lists of
       parameters so we cannot pass both a list of public keys and a
       list of signatures on the command line. This would permit a
       command for running the Change_keys action.

       However, we can run any action by deserialising the sequence of
       bytes built using the "prepare multisig transaction" commands *)

    command ~group ~desc: "Run a transaction described by a sequence of bytes on a multisig contract."
      transfer_options
      (prefixes ["run"; "transaction"]
       @@ bytes_param
         ~name:"bytes"
         ~desc:"the sequence of bytes to deserialize as a multisig \
                action, can be obtained by one of the \"prepare \
                multisig transaction\" commands"
       @@ prefixes ["on"; "multisig"; "contract"]
       @@ Client_proto_contracts.RawContractAlias.alias_param
         ~name: "multisig" ~desc: "name of the originated multisig contract"
       @@ prefixes ["on"; "behalf"; "of"]
       @@ Client_proto_contracts.ContractAlias.destination_param
         ~name: "src" ~desc: "source calling the multisig contract"
       @@ prefixes ["with"; "signatures"]
       @@ (seq_of_param (signature_param ())))
      begin fun (fee, dry_run, gas_limit, storage_limit, counter, no_print_source,
                 minimal_fees, minimal_nanotez_per_byte,
                 minimal_nanotez_per_gas_unit, force_low_fee, fee_cap, burn_cap)
        bytes (_, multisig_contract) (_, source) signatures cctxt ->
        Client_proto_context.source_to_keys cctxt
          ~chain:cctxt#chain ~block:cctxt#block
          source >>=? fun (src_pk, src_sk) ->
        let fee_parameter = {
          Injection.minimal_fees ;
          minimal_nanotez_per_byte ;
          minimal_nanotez_per_gas_unit ;
          force_low_fee ;
          fee_cap ;
          burn_cap ;
        } in
        Client_proto_multisig.call_multisig_on_bytes cctxt
          ~chain:cctxt#chain ~block:cctxt#block ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee_parameter
          ~source ?fee ~src_pk ~src_sk ~multisig_contract
          ~bytes
          ~signatures
          ~amount:Proto_alpha.Alpha_context.Tez.zero
          ?gas_limit ?storage_limit ?counter () >>=
        Client_proto_context_commands.report_michelson_errors ~no_print_source
          ~msg:"transfer simulation failed" cctxt >>= function
        | None -> return_unit
        | Some (_res, _contracts) ->
            return_unit
      end;

  ]
