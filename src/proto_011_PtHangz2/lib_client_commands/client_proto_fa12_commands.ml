(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let group =
  {
    Tezos_clic.name = "tokens";
    title = "Commands for managing FA1.2-compatible smart contracts";
  }

let alias_param = Client_proto_contracts.Contract_alias.destination_param

let token_contract_param () =
  alias_param
    ~name:"contract"
    ~desc:"name or address of the FA1.2-compatible contract"

let from_param () =
  alias_param ~name:"from" ~desc:"name or address of the sender"

let to_param () = alias_param ~name:"to" ~desc:"name or address of the receiver"

let amount_param () =
  Tezos_clic.param
    ~name:"amount"
    ~desc:"number of tokens"
    (Tezos_clic.parameter (fun _ s ->
         try
           let v = Z.of_string s in
           assert (Compare.Z.(v >= Z.zero)) ;
           return v
         with _ -> failwith "invalid amount (must be a non-negative number)"))

let tez_amount_arg =
  tez_arg ~default:"0" ~parameter:"tez-amount" ~doc:"amount in \xEA\x9C\xA9"

let as_arg =
  Client_proto_contracts.Contract_alias.destination_arg
    ~name:"as"
    ~doc:"name or address of the caller of the contract"
    ()

let payer_arg =
  Client_proto_contracts.Contract_alias.destination_arg
    ~name:"payer"
    ~doc:"name of the payer (i.e. SOURCE) contract for the transaction"
    ()

let callback_entrypoint_arg =
  Tezos_clic.arg
    ~doc:"Entrypoint the view should use to callback to"
    ~long:"callback-entrypoint"
    ~placeholder:"name"
    string_parameter

let contract_call_options =
  Tezos_clic.args14
    tez_amount_arg
    fee_arg
    Client_proto_context_commands.dry_run_switch
    Client_proto_context_commands.verbose_signing_switch
    gas_limit_arg
    storage_limit_arg
    counter_arg
    no_print_source_flag
    minimal_fees_arg
    minimal_nanotez_per_byte_arg
    minimal_nanotez_per_gas_unit_arg
    force_low_fee_arg
    fee_cap_arg
    burn_cap_arg

let contract_view_options =
  Tezos_clic.args15
    callback_entrypoint_arg
    tez_amount_arg
    fee_arg
    Client_proto_context_commands.dry_run_switch
    Client_proto_context_commands.verbose_signing_switch
    gas_limit_arg
    storage_limit_arg
    counter_arg
    no_print_source_flag
    minimal_fees_arg
    minimal_nanotez_per_byte_arg
    minimal_nanotez_per_gas_unit_arg
    force_low_fee_arg
    fee_cap_arg
    burn_cap_arg

let view_options =
  Tezos_clic.args3
    run_gas_limit_arg
    payer_arg
    (unparsing_mode_arg ~default:"Readable")

let dummy_callback =
  Contract.implicit_contract Tezos_crypto.Signature.V0.Public_key_hash.zero

let get_contract_caller_keys cctxt caller =
  match Contract.is_implicit caller with
  | None ->
      failwith "only implicit accounts can be the source of a contract call"
  | Some source ->
      Client_keys_v0.get_key cctxt source >>=? fun (_, caller_pk, caller_sk) ->
      return (source, caller_pk, caller_sk)

let commands () : #Protocol_client_context.full Tezos_clic.command list =
  let open Client_proto_args in
  Tezos_clic.
    [
      command
        ~group
        ~desc:"Check that a contract is FA1.2-compatible."
        no_options
        (prefixes ["check"; "contract"]
        @@ token_contract_param ()
        @@ prefixes ["implements"; "fa1.2"]
        @@ stop)
        (fun () (_, contract) (cctxt : #Protocol_client_context.full) ->
          Client_proto_fa12.contract_has_fa12_interface
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ()
          >>=? fun _ ->
          Format.printf
            "Contract %a has an FA1.2 interface.\n%!"
            Contract.pp
            contract ;
          return_unit);
      command
        ~group
        ~desc:"Transfer tokens between two given accounts"
        (Tezos_clic.args15
           as_arg
           tez_amount_arg
           fee_arg
           Client_proto_context_commands.dry_run_switch
           Client_proto_context_commands.verbose_signing_switch
           gas_limit_arg
           storage_limit_arg
           counter_arg
           no_print_source_flag
           minimal_fees_arg
           minimal_nanotez_per_byte_arg
           minimal_nanotez_per_gas_unit_arg
           force_low_fee_arg
           fee_cap_arg
           burn_cap_arg)
        (prefixes ["from"; "fa1.2"; "contract"]
        @@ token_contract_param () @@ prefix "transfer" @@ amount_param ()
        @@ prefix "from" @@ from_param () @@ prefix "to" @@ to_param () @@ stop
        )
        (fun ( as_address,
               tez_amount,
               fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             (_, contract)
             amount
             src
             (_, dst)
             (cctxt : #Protocol_client_context.full) ->
          let _, caller = Option.value ~default:src as_address in
          get_contract_caller_keys cctxt caller
          >>=? fun (source, caller_pk, caller_sk) ->
          let action = Client_proto_fa12.Transfer (snd src, dst, amount) in
          let fee_parameter =
            {
              Injection.minimal_fees;
              minimal_nanotez_per_byte;
              minimal_nanotez_per_gas_unit;
              force_low_fee;
              fee_cap;
              burn_cap;
            }
          in
          Client_proto_fa12.call_contract
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ~action
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~verbose_signing
            ?fee
            ~source
            ~src_pk:caller_pk
            ~src_sk:caller_sk
            ~tez_amount
            ?gas_limit
            ?storage_limit
            ?counter
            ~fee_parameter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= fun _ -> return_unit);
      command
        ~group
        ~desc:"Allow account to transfer an amount of token"
        contract_call_options
        (prefixes ["from"; "fa1.2"; "contract"]
        @@ token_contract_param () @@ prefix "as"
        @@ alias_param ~name:"as" ~desc:"name or address of the sender"
        @@ prefix "approve" @@ amount_param () @@ prefix "from"
        @@ alias_param
             ~name:"from"
             ~desc:"name or address to approve withdrawal"
        @@ stop)
        (fun ( tez_amount,
               fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             (_, contract)
             (_, source)
             amount
             (_, dst)
             (cctxt : #Protocol_client_context.full) ->
          get_contract_caller_keys cctxt source
          >>=? fun (source, src_pk, src_sk) ->
          let action = Client_proto_fa12.Approve (dst, amount) in
          let fee_parameter =
            {
              Injection.minimal_fees;
              minimal_nanotez_per_byte;
              minimal_nanotez_per_gas_unit;
              force_low_fee;
              fee_cap;
              burn_cap;
            }
          in
          Client_proto_fa12.call_contract
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ~action
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~verbose_signing
            ?fee
            ~source
            ~src_pk
            ~src_sk
            ~tez_amount
            ?gas_limit
            ?storage_limit
            ?counter
            ~fee_parameter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= fun _ -> return_unit);
      command
        ~group
        ~desc:"Ask for an address's balance offchain"
        view_options
        (prefixes ["from"; "fa1.2"; "contract"]
        @@ token_contract_param ()
        @@ prefixes ["get"; "balance"; "for"]
        @@ alias_param
             ~name:"from"
             ~desc:
               "name or address of the account to lookup (also the source \
                contract)"
        @@ stop)
        (fun (gas, payer, unparsing_mode)
             (_, contract)
             (_, addr)
             (cctxt : #Protocol_client_context.full) ->
          let action =
            Client_proto_fa12.Get_balance (addr, (dummy_callback, None))
          in
          let payer = Option.map snd payer in
          Client_proto_fa12.run_view_action
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ~action
            ~source:addr
            ?gas
            ?payer
            ~unparsing_mode
            ()
          >>= fun res -> Client_proto_programs.print_view_result cctxt res);
      command
        ~group
        ~desc:"Ask for an address's allowance offchain"
        view_options
        (prefixes ["from"; "fa1.2"; "contract"]
        @@ token_contract_param ()
        @@ prefixes ["get"; "allowance"; "on"]
        @@ alias_param
             ~name:"owner"
             ~desc:"name or address of the account giving the allowance"
        @@ prefix "as"
        @@ alias_param
             ~name:"operator"
             ~desc:"name or address of the account receiving the allowance"
        @@ stop)
        (fun (gas, payer, unparsing_mode)
             (_, contract)
             (_, source)
             (_, destination)
             (cctxt : #Protocol_client_context.full) ->
          let action =
            Client_proto_fa12.Get_allowance
              (source, destination, (dummy_callback, None))
          in
          let payer = Option.map snd payer in
          Client_proto_fa12.run_view_action
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ~action
            ~source
            ?gas
            ?payer
            ~unparsing_mode
            ()
          >>= fun res -> Client_proto_programs.print_view_result cctxt res);
      command
        ~group
        ~desc:"Ask for the contract's total token supply offchain"
        view_options
        (prefixes ["from"; "fa1.2"; "contract"]
        @@ token_contract_param ()
        @@ prefixes ["get"; "total"; "supply"]
        @@ stop)
        (fun (gas, payer, unparsing_mode)
             (_, contract)
             (cctxt : #Protocol_client_context.full) ->
          let action =
            Client_proto_fa12.Get_total_supply (dummy_callback, None)
          in
          let payer = Option.map snd payer in
          Client_proto_fa12.run_view_action
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ~action
            ?gas
            ?payer
            ~unparsing_mode
            ()
          >>= fun res -> Client_proto_programs.print_view_result cctxt res);
      command
        ~group
        ~desc:"Ask for an address's balance using a callback contract"
        contract_view_options
        (prefixes ["from"; "fa1.2"; "contract"]
        @@ token_contract_param ()
        @@ prefixes ["get"; "balance"; "for"]
        @@ alias_param
             ~name:"from"
             ~desc:
               "name or address of the account to lookup (also the source \
                contract)"
        @@ prefixes ["callback"; "on"]
        @@ alias_param
             ~name:"callback"
             ~desc:"name or address of the callback contract"
        @@ stop)
        (fun ( callback_entrypoint,
               tez_amount,
               fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             (_, contract)
             (_, addr)
             (_, callback)
             (cctxt : #Protocol_client_context.full) ->
          get_contract_caller_keys cctxt addr
          >>=? fun (source, src_pk, src_sk) ->
          let action =
            Client_proto_fa12.Get_balance (addr, (callback, callback_entrypoint))
          in
          let fee_parameter =
            {
              Injection.minimal_fees;
              minimal_nanotez_per_byte;
              minimal_nanotez_per_gas_unit;
              force_low_fee;
              fee_cap;
              burn_cap;
            }
          in
          Client_proto_fa12.call_contract
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ~action
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~verbose_signing
            ?fee
            ~source
            ~src_pk
            ~src_sk
            ~tez_amount
            ?gas_limit
            ?storage_limit
            ?counter
            ~fee_parameter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= fun _ -> return_unit);
      command
        ~group
        ~desc:"Ask for an address's allowance using a callback contract"
        contract_view_options
        (prefixes ["from"; "fa1.2"; "contract"]
        @@ token_contract_param ()
        @@ prefixes ["get"; "allowance"; "on"]
        @@ alias_param
             ~name:"from"
             ~desc:"name or address of the account giving the allowance"
        @@ prefix "as"
        @@ alias_param
             ~name:"to"
             ~desc:"name or address of the account receiving the allowance"
        @@ prefixes ["callback"; "on"]
        @@ alias_param
             ~name:"callback"
             ~desc:"name or address of the callback contract"
        @@ stop)
        (fun ( callback_entrypoint,
               tez_amount,
               fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             (_, contract)
             (_, src)
             (_, dst)
             (_, callback)
             (cctxt : #Protocol_client_context.full) ->
          get_contract_caller_keys cctxt src
          >>=? fun (source, src_pk, src_sk) ->
          let action =
            Client_proto_fa12.Get_allowance
              (src, dst, (callback, callback_entrypoint))
          in
          let fee_parameter =
            {
              Injection.minimal_fees;
              minimal_nanotez_per_byte;
              minimal_nanotez_per_gas_unit;
              force_low_fee;
              fee_cap;
              burn_cap;
            }
          in
          Client_proto_fa12.call_contract
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ~action
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~verbose_signing
            ?fee
            ~source
            ~src_pk
            ~src_sk
            ~tez_amount
            ?gas_limit
            ?storage_limit
            ?counter
            ~fee_parameter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= fun _ -> return_unit);
      command
        ~group
        ~desc:
          "Ask for a contract's total token supply using a callback contract"
        contract_view_options
        (prefixes ["from"; "fa1.2"; "contract"]
        @@ token_contract_param ()
        @@ prefixes ["get"; "total"; "supply"; "as"]
        @@ alias_param
             ~name:"from"
             ~desc:"name or address of the source account"
        @@ prefixes ["callback"; "on"]
        @@ alias_param
             ~name:"callback"
             ~desc:"name or address of the callback contract"
        @@ stop)
        (fun ( callback_entrypoint,
               tez_amount,
               fee,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             (_, contract)
             (_, addr)
             (_, callback)
             (cctxt : #Protocol_client_context.full) ->
          get_contract_caller_keys cctxt addr
          >>=? fun (source, src_pk, src_sk) ->
          let action =
            Client_proto_fa12.Get_total_supply (callback, callback_entrypoint)
          in
          let fee_parameter =
            {
              Injection.minimal_fees;
              minimal_nanotez_per_byte;
              minimal_nanotez_per_gas_unit;
              force_low_fee;
              fee_cap;
              burn_cap;
            }
          in
          Client_proto_fa12.call_contract
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~contract
            ~action
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~verbose_signing
            ?fee
            ~source
            ~src_pk
            ~src_sk
            ~tez_amount
            ?gas_limit
            ?storage_limit
            ?counter
            ~fee_parameter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= fun _ -> return_unit);
      command
        ~group
        ~desc:
          "Execute multiple token transfers from a single source account. If \
           one of the token transfers fails, none of them are executed."
        (args14
           default_fee_arg
           as_arg
           Client_proto_context_commands.dry_run_switch
           Client_proto_context_commands.verbose_signing_switch
           default_gas_limit_arg
           default_storage_limit_arg
           counter_arg
           no_print_source_flag
           minimal_fees_arg
           minimal_nanotez_per_byte_arg
           minimal_nanotez_per_gas_unit_arg
           force_low_fee_arg
           fee_cap_arg
           burn_cap_arg)
        (prefixes ["multiple"; "fa1.2"; "transfers"; "from"]
        @@ alias_param
             ~name:"src"
             ~desc:"name or address of the source of the transfers"
        @@ prefix "using"
        @@ param
             ~name:"transfers.json"
             ~desc:
               (Format.sprintf
                  "List of token transfers to inject from the source contract \
                   in JSON format (as a file or string). The JSON must be an \
                   array of objects of the form: '[ {\"token_contract\": \
                   address or alias, \"destination\": address or alias, \
                   \"amount\": non-negative integer (, <field>: <val> ...) } \
                   (, ...) ]', where an optional <field> can either be \
                   \"tez-amount\", \"fee\", \"gas-limit\" or \
                   \"storage-limit\". The complete schema can be inspected via \
                   `tezos-codec describe %s.fa1.2.token_transfer json schema`."
                  Protocol.name)
             Client_proto_context_commands.json_file_or_text_parameter
        @@ stop)
        (fun ( fee,
               as_address,
               dry_run,
               verbose_signing,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             src
             operations_json
             cctxt ->
          let _, caller = Option.value ~default:src as_address in
          let fee_parameter =
            {
              Injection.minimal_fees;
              minimal_nanotez_per_byte;
              minimal_nanotez_per_gas_unit;
              force_low_fee;
              fee_cap;
              burn_cap;
            }
          in
          match
            Data_encoding.Json.destruct
              (Data_encoding.list Client_proto_fa12.token_transfer_encoding)
              operations_json
          with
          | [] -> failwith "Empty operation list"
          | operations ->
              get_contract_caller_keys cctxt caller
              >>=? fun (source, src_pk, src_sk) ->
              Client_proto_fa12.inject_token_transfer_batch
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~verbose_signing
                ~sender:(snd src)
                ~source
                ~src_pk
                ~src_sk
                ~token_transfers:operations
                ~fee_parameter
                ?counter
                ?default_fee:fee
                ?default_gas_limit:gas_limit
                ?default_storage_limit:storage_limit
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"multiple transfers simulation failed"
                    cctxt
              >>= fun _ -> return_unit
          | exception (Data_encoding.Json.Cannot_destruct (path, exn2) as exn)
            -> (
              match (path, operations_json) with
              | [`Index n], `A lj -> (
                  match List.nth_opt lj n with
                  | Some j ->
                      failwith
                        "Invalid transfer at index %i: %a %a"
                        n
                        (fun ppf -> Data_encoding.Json.print_error ppf)
                        exn2
                        Data_encoding.Json.pp
                        j
                  | _ ->
                      failwith
                        "Invalid transfer at index %i: %a"
                        n
                        (fun ppf -> Data_encoding.Json.print_error ppf)
                        exn2)
              | _ ->
                  failwith
                    "Invalid transfer file: %a %a"
                    (fun ppf -> Data_encoding.Json.print_error ppf)
                    exn
                    Data_encoding.Json.pp
                    operations_json));
    ]
