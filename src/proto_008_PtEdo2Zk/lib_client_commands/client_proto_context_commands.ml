(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Alpha_context
open Tezos_micheline
open Client_proto_context
open Client_proto_contracts
open Client_proto_programs
open Client_keys_v0
open Client_proto_args

let encrypted_switch =
  Tezos_clic.switch ~long:"encrypted" ~doc:"encrypt the key on-disk" ()

let dry_run_switch =
  Tezos_clic.switch
    ~long:"dry-run"
    ~short:'D'
    ~doc:"don't inject the operation, just display it"
    ()

let verbose_signing_switch =
  Tezos_clic.switch
    ~long:"verbose-signing"
    ~doc:"display extra information before signing the operation"
    ()

let simulate_switch =
  Tezos_clic.switch
    ~long:"simulation"
    ~doc:
      "Simulate the execution of the command, without needing any signatures."
    ()

let report_michelson_errors ?(no_print_source = false) ~msg
    (cctxt : #Client_context.printer) = function
  | Error errs ->
      cctxt#warning
        "%a"
        (Michelson_v1_error_reporter.report_errors
           ~details:(not no_print_source)
           ~show_source:(not no_print_source)
           ?parsed:None)
        errs
      >>= fun () ->
      cctxt#error "%s" msg >>= fun () -> Lwt.return_none
  | Ok data -> Lwt.return_some data

let json_file_or_text_parameter =
  Tezos_clic.parameter (fun _ p ->
      match String.split ~limit:1 ':' p with
      | ["text"; text] -> return (Ezjsonm.from_string text)
      | ["file"; path] -> Lwt_utils_unix.Json.read_file path
      | _ -> (
          if Sys.file_exists p then Lwt_utils_unix.Json.read_file p
          else
            try return (Ezjsonm.from_string p)
            with Ezjsonm.Parse_error _ ->
              failwith "Neither an existing file nor valid JSON: '%s'" p))

let data_parameter =
  Tezos_clic.parameter (fun _ data ->
      Lwt.return
        (Micheline_parser.no_parsing_error
        @@ Michelson_v1_parser.parse_expression data))

let non_negative_param =
  Tezos_clic.parameter (fun _ s ->
      match int_of_string_opt s with
      | Some i when i >= 0 -> return i
      | _ -> failwith "Parameter should be a non-negative integer literal")

let block_hash_param =
  Tezos_clic.parameter (fun _ s ->
      try return (Block_hash.of_b58check_exn s)
      with _ -> failwith "Parameter '%s' is an invalid block hash" s)

let group =
  {
    Tezos_clic.name = "context";
    title = "Block contextual commands (see option -block)";
  }

let alphanet = {Tezos_clic.name = "alphanet"; title = "Alphanet only commands"}

let binary_description =
  {Tezos_clic.name = "description"; title = "Binary Description"}

let transfer_command amount source destination cctxt
    ( fee,
      dry_run,
      verbose_signing,
      simulation,
      gas_limit,
      storage_limit,
      counter,
      arg,
      no_print_source,
      minimal_fees,
      minimal_nanotez_per_byte,
      minimal_nanotez_per_gas_unit,
      force_low_fee,
      fee_cap,
      burn_cap,
      entrypoint ) =
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
  (match Contract.is_implicit source with
  | None ->
      let contract = source in
      Managed_contract.get_contract_manager cctxt source >>=? fun source ->
      Client_keys_v0.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
      Managed_contract.transfer
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ?confirmations:cctxt#confirmations
        ~dry_run
        ~verbose_signing
        ~simulation
        ~fee_parameter
        ?fee
        ~contract
        ~source
        ~src_pk
        ~src_sk
        ~destination
        ?entrypoint
        ?arg
        ~amount
        ?gas_limit
        ?storage_limit
        ?counter
        ()
  | Some source ->
      Client_keys_v0.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
      transfer
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ?confirmations:cctxt#confirmations
        ~dry_run
        ~simulation
        ~verbose_signing
        ~fee_parameter
        ~source
        ?fee
        ~src_pk
        ~src_sk
        ~destination
        ?entrypoint
        ?arg
        ~amount
        ?gas_limit
        ?storage_limit
        ?counter
        ())
  >>= report_michelson_errors
        ~no_print_source
        ~msg:"transfer simulation failed"
        cctxt
  >>= function
  | None -> return_unit
  | Some (_res, _contracts) -> return_unit

let tez_of_string_exn index field s =
  match Tez.of_string s with
  | Some t -> return t
  | None ->
      failwith
        "Invalid \xEA\x9C\xA9 notation at entry %i, field \"%s\": %s"
        index
        field
        s

let tez_of_opt_string_exn index field s =
  match s with
  | None -> return None
  | Some s -> tez_of_string_exn index field s >>=? fun s -> return (Some s)

let prepare_batch_operation cctxt ?arg ?fee ?gas_limit ?storage_limit
    ?entrypoint source index batch =
  Client_proto_contracts.Contract_alias.find_destination cctxt batch.destination
  >>=? fun (_, destination) ->
  tez_of_string_exn index "amount" batch.amount >>=? fun amount ->
  tez_of_opt_string_exn index "fee" batch.fee >>=? fun batch_fee ->
  let fee = Option.either batch_fee fee in
  let arg = Option.either batch.arg arg in
  let gas_limit = Option.either batch.gas_limit gas_limit in
  let storage_limit = Option.either batch.storage_limit storage_limit in
  let entrypoint = Option.either batch.entrypoint entrypoint in
  parse_arg_transfer arg >>=? fun parameters ->
  (match Contract.is_implicit source with
  | None ->
      Managed_contract.build_transaction_operation
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~contract:source
        ~destination
        ?entrypoint
        ?arg
        ~amount
        ?fee
        ?gas_limit
        ?storage_limit
        ()
  | Some _ ->
      return
        (build_transaction_operation
           ~amount
           ~parameters
           ?entrypoint
           ?fee
           ?gas_limit
           ?storage_limit
           destination))
  >>=? fun operation -> return (Injection.Annotated_manager_operation operation)

let commands network () =
  let open Tezos_clic in
  [
    command
      ~group
      ~desc:"Access the timestamp of the block."
      (args1
         (switch ~doc:"output time in seconds" ~short:'s' ~long:"seconds" ()))
      (fixed ["get"; "timestamp"])
      (fun seconds (cctxt : Protocol_client_context.full) ->
        Shell_services.Blocks.Header.shell_header
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ()
        >>=? fun {timestamp = v; _} ->
        (if seconds then cctxt#message "%Ld" (Time.Protocol.to_seconds v)
        else cctxt#message "%s" (Time.Protocol.to_notation v))
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Lists all non empty contracts of the block."
      no_options
      (fixed ["list"; "contracts"])
      (fun () (cctxt : Protocol_client_context.full) ->
        list_contract_labels cctxt ~chain:cctxt#chain ~block:cctxt#block
        >>=? fun contracts ->
        List.iter_s
          (fun (alias, hash, kind) -> cctxt#message "%s%s%s" hash kind alias)
          contracts
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the balance of a contract."
      no_options
      (prefixes ["get"; "balance"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        get_balance cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? fun amount ->
        cctxt#answer "%a %s" Tez.pp amount Client_proto_args.tez_sym
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the storage of a contract."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["get"; "contract"; "storage"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun unparsing_mode (_, contract) (cctxt : Protocol_client_context.full) ->
        Plugin.RPC.get_storage_normalized
          cctxt
          (cctxt#chain, cctxt#block)
          ~contract
          ~unparsing_mode
        >>=? function
        | None -> cctxt#error "This is not a smart contract."
        | Some storage ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped storage
            >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Get the value associated to a key in the big map storage of a \
         contract (deprecated)."
      no_options
      (prefixes ["get"; "big"; "map"; "value"; "for"]
      @@ Tezos_clic.param ~name:"key" ~desc:"the key to look for" data_parameter
      @@ prefixes ["of"; "type"]
      @@ Tezos_clic.param ~name:"type" ~desc:"type of the key" data_parameter
      @@ prefix "in"
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () key key_type (_, contract) (cctxt : Protocol_client_context.full) ->
        get_contract_big_map_value
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          contract
          (key.expanded, key_type.expanded)
        >>=? function
        | None -> cctxt#error "No value associated to this key."
        | Some value ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped value
            >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get a value in a big map."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["get"; "element"]
      @@ Tezos_clic.param
           ~name:"key"
           ~desc:"the key to look for"
           (Tezos_clic.parameter (fun _ s ->
                return (Script_expr_hash.of_b58check_exn s)))
      @@ prefixes ["of"; "big"; "map"]
      @@ Tezos_clic.param
           ~name:"big_map"
           ~desc:"identifier of the big_map"
           int_parameter
      @@ stop)
      (fun unparsing_mode key id (cctxt : Protocol_client_context.full) ->
        get_big_map_value
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          (Big_map.Id.parse_z (Z.of_int id))
          key
          ~unparsing_mode
        >>=? fun value ->
        cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped value
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the code of a contract."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["get"; "contract"; "code"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun unparsing_mode (_, contract) (cctxt : Protocol_client_context.full) ->
        Plugin.RPC.get_script_normalized
          cctxt
          (cctxt#chain, cctxt#block)
          ~contract
          ~unparsing_mode
        >>=? function
        | None -> cctxt#error "This is not a smart contract."
        | Some {code; storage = _} -> (
            match Script_repr.force_decode code with
            | Error errs ->
                cctxt#error
                  "%a"
                  (Format.pp_print_list
                     ~pp_sep:Format.pp_print_newline
                     Environment.Error_monad.pp)
                  errs
            | Ok (code, _) ->
                let {Michelson_v1_parser.source; _} =
                  Michelson_v1_printer.unparse_toplevel code
                in
                cctxt#answer "%s" source >>= return));
    command
      ~group
      ~desc:"Get the `BLAKE2B` script hash of a contract."
      no_options
      (prefixes ["get"; "contract"; "script"; "hash"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        get_script cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? function
        | None -> cctxt#error "This is not a smart contract."
        | Some {code; storage = _} -> (
            match Script_repr.force_decode code with
            | Error errs ->
                cctxt#error
                  "%a"
                  (Format.pp_print_list
                     ~pp_sep:Format.pp_print_newline
                     Environment.Error_monad.pp)
                  errs
            | Ok (code, _) ->
                let bytes =
                  Data_encoding.Binary.to_bytes_exn Script.expr_encoding code
                in
                let hash = Script_expr_hash.hash_bytes [bytes] in
                cctxt#answer "%a" Script_expr_hash.pp hash >|= ok));
    command
      ~group
      ~desc:"Get the type of an entrypoint of a contract."
      no_options
      (prefixes ["get"; "contract"; "entrypoint"; "type"; "of"]
      @@ Tezos_clic.string ~name:"entrypoint" ~desc:"the entrypoint to describe"
      @@ prefixes ["for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () entrypoint (_, contract) (cctxt : Protocol_client_context.full) ->
        Michelson_v1_entrypoints.contract_entrypoint_type
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~contract
          ~entrypoint
        >>= Michelson_v1_entrypoints.print_entrypoint_type
              cctxt
              ~emacs:false
              ~contract
              ~entrypoint);
    command
      ~group
      ~desc:"Get the entrypoint list of a contract."
      no_options
      (prefixes ["get"; "contract"; "entrypoints"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        Michelson_v1_entrypoints.list_contract_entrypoints
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~contract
        >>= Michelson_v1_entrypoints.print_entrypoints_list
              cctxt
              ~emacs:false
              ~contract);
    command
      ~group
      ~desc:"Get the list of unreachable paths in a contract's parameter type."
      no_options
      (prefixes ["get"; "contract"; "unreachable"; "paths"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        Michelson_v1_entrypoints.list_contract_unreachables
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~contract
        >>= Michelson_v1_entrypoints.print_unreachables
              cctxt
              ~emacs:false
              ~contract);
    command
      ~group
      ~desc:"Get the delegate of a contract."
      no_options
      (prefixes ["get"; "delegate"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        Client_proto_contracts.get_delegate
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          contract
        >>=? function
        | None -> cctxt#message "none" >>= fun () -> return_unit
        | Some delegate ->
            Public_key_hash.rev_find cctxt delegate >>=? fun mn ->
            Public_key_hash.to_source delegate >>=? fun m ->
            cctxt#message
              "%s (%s)"
              m
              (match mn with None -> "unknown" | Some n -> "known as " ^ n)
            >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Set the delegate of a contract."
      (args10
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["set"; "delegate"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ prefix "to"
      @@ Public_key_hash.source_param
           ~name:"dlgt"
           ~desc:"new delegate of the contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, contract)
           delegate
           (cctxt : Protocol_client_context.full) ->
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
        match Contract.is_implicit contract with
        | None ->
            Managed_contract.get_contract_manager cctxt contract
            >>=? fun source ->
            Client_keys_v0.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            Managed_contract.set_delegate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~simulation
              ~fee_parameter
              ?fee
              ~source
              ~src_pk
              ~src_sk
              contract
              (Some delegate)
            >>= fun errors ->
            report_michelson_errors
              ~no_print_source:true
              ~msg:"Setting delegate through entrypoints failed."
              cctxt
              errors
            >>= fun _ -> return_unit
        | Some mgr ->
            Client_keys_v0.get_key cctxt mgr >>=? fun (_, src_pk, manager_sk) ->
            set_delegate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~simulation
              ~fee_parameter
              ?fee
              mgr
              (Some delegate)
              ~src_pk
              ~manager_sk
            >>=? fun _ -> return_unit);
    command
      ~group
      ~desc:"Withdraw the delegate from a contract."
      (args9
         fee_arg
         dry_run_switch
         verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["withdraw"; "delegate"; "from"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, contract)
           (cctxt : Protocol_client_context.full) ->
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
        match Contract.is_implicit contract with
        | None ->
            Managed_contract.get_contract_manager cctxt contract
            >>=? fun source ->
            Client_keys_v0.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            Managed_contract.set_delegate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~fee_parameter
              ?fee
              ~source
              ~src_pk
              ~src_sk
              contract
              None
            >>= fun errors ->
            report_michelson_errors
              ~no_print_source:true
              ~msg:"Withdrawing delegate through entrypoints failed."
              cctxt
              errors
            >>= fun _ -> return_unit
        | Some mgr ->
            Client_keys_v0.get_key cctxt mgr >>=? fun (_, src_pk, manager_sk) ->
            set_delegate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~fee_parameter
              mgr
              None
              ?fee
              ~src_pk
              ~manager_sk
            >>= fun _ -> return_unit);
    command
      ~group
      ~desc:"Launch a smart contract on the blockchain."
      (args15
         fee_arg
         dry_run_switch
         verbose_signing_switch
         gas_limit_arg
         storage_limit_arg
         delegate_arg
         (Client_keys_v0.force_switch ())
         init_arg
         no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["originate"; "contract"]
      @@ Raw_contract_alias.fresh_alias_param
           ~name:"new"
           ~desc:"name of the new contract"
      @@ prefix "transferring"
      @@ tez_param ~name:"qty" ~desc:"amount taken from source"
      @@ prefix "from"
      @@ Contract_alias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ prefix "running"
      @@ Program.source_param
           ~name:"prg"
           ~desc:
             "script of the account\n\
              Combine with -init if the storage type is not unit."
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             gas_limit,
             storage_limit,
             delegate,
             force,
             initial_storage,
             no_print_source,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           alias_name
           balance
           (_, source)
           program
           (cctxt : Protocol_client_context.full) ->
        Raw_contract_alias.of_fresh cctxt force alias_name
        >>=? fun alias_name ->
        Lwt.return (Micheline_parser.no_parsing_error program)
        >>=? fun {expanded = code; _} ->
        match Contract.is_implicit source with
        | None ->
            failwith
              "only implicit accounts can be the source of an origination"
        | Some source -> (
            Client_keys_v0.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
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
            originate_contract
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ?fee
              ?gas_limit
              ?storage_limit
              ~delegate
              ~initial_storage
              ~balance
              ~source
              ~src_pk
              ~src_sk
              ~code
              ~fee_parameter
              ()
            >>= fun errors ->
            report_michelson_errors
              ~no_print_source
              ~msg:"origination simulation failed"
              cctxt
              errors
            >>= function
            | None -> return_unit
            | Some (_res, contract) ->
                if dry_run then return_unit
                else
                  save_contract ~force cctxt alias_name contract >>=? fun () ->
                  return_unit));
    command
      ~group
      ~desc:
        "Execute multiple transfers from a single source account.\n\
         If one of the transfers fails, none of them get executed."
      (args16
         default_fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         default_gas_limit_arg
         default_storage_limit_arg
         counter_arg
         default_arg_arg
         no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg
         default_entrypoint_arg)
      (prefixes ["multiple"; "transfers"; "from"]
      @@ Contract_alias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ prefix "using"
      @@ param
           ~name:"transfers.json"
           ~desc:
             "List of operations originating from the source contract in JSON \
              format (from a file or directly inlined). The input JSON must be \
              an array of objects of the form: '[ {\"destination\": dst, \
              \"amount\": qty (, <field>: <val> ...) } (, ...) ]', where an \
              optional <field> can either be \"fee\", \"gas-limit\", \
              \"storage-limit\", \"arg\", or \"entrypoint\"."
           json_file_or_text_parameter
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             gas_limit,
             storage_limit,
             counter,
             arg,
             no_print_source,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap,
             entrypoint )
           (_, source)
           operations_json
           cctxt ->
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
        let prepare i =
          prepare_batch_operation
            cctxt
            ?arg
            ?fee
            ?gas_limit
            ?storage_limit
            ?entrypoint
            source
            i
        in
        match
          Data_encoding.Json.destruct
            (Data_encoding.list
               Client_proto_context.batch_transfer_operation_encoding)
            operations_json
        with
        | [] -> failwith "Empty operation list"
        | operations ->
            (match Contract.is_implicit source with
            | None ->
                Managed_contract.get_contract_manager cctxt source
                >>=? fun source ->
                Client_keys_v0.get_key cctxt source
                >>=? fun (_, src_pk, src_sk) -> return (source, src_pk, src_sk)
            | Some source ->
                Client_keys_v0.get_key cctxt source
                >>=? fun (_, src_pk, src_sk) -> return (source, src_pk, src_sk))
            >>=? fun (source, src_pk, src_sk) ->
            List.mapi_ep prepare operations >>=? fun contents ->
            let (Manager_list contents) = Injection.manager_of_list contents in
            Injection.inject_manager_operation
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~simulation
              ~source
              ?fee
              ?gas_limit
              ?storage_limit
              ?counter
              ~src_pk
              ~src_sk
              ~fee_parameter
              contents
            >>= report_michelson_errors
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
    command
      ~group
      ~desc:"Transfer tokens / call a smart contract."
      (args16
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         gas_limit_arg
         storage_limit_arg
         counter_arg
         arg_arg
         no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg
         entrypoint_arg)
      (prefixes ["transfer"]
      @@ tez_param ~name:"qty" ~desc:"amount taken from source"
      @@ prefix "from"
      @@ Contract_alias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ prefix "to"
      @@ Contract_alias.destination_param
           ~name:"dst"
           ~desc:"name/literal of the destination contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             gas_limit,
             storage_limit,
             counter,
             arg,
             no_print_source,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap,
             entrypoint )
           amount
           (_, source)
           (_, destination)
           cctxt ->
        transfer_command
          amount
          source
          destination
          cctxt
          ( fee,
            dry_run,
            verbose_signing,
            simulation,
            gas_limit,
            storage_limit,
            counter,
            arg,
            no_print_source,
            minimal_fees,
            minimal_nanotez_per_byte,
            minimal_nanotez_per_gas_unit,
            force_low_fee,
            fee_cap,
            burn_cap,
            entrypoint ));
    command
      ~group
      ~desc:"Call a smart contract (same as 'transfer 0')."
      (args16
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         gas_limit_arg
         storage_limit_arg
         counter_arg
         arg_arg
         no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg
         entrypoint_arg)
      (prefixes ["call"]
      @@ Contract_alias.destination_param
           ~name:"dst"
           ~desc:"name/literal of the destination contract"
      @@ prefix "from"
      @@ Contract_alias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             gas_limit,
             storage_limit,
             counter,
             arg,
             no_print_source,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap,
             entrypoint )
           (_, destination)
           (_, source)
           cctxt ->
        let amount = Tez.zero in
        transfer_command
          amount
          source
          destination
          cctxt
          ( fee,
            dry_run,
            verbose_signing,
            simulation,
            gas_limit,
            storage_limit,
            counter,
            arg,
            no_print_source,
            minimal_fees,
            minimal_nanotez_per_byte,
            minimal_nanotez_per_gas_unit,
            force_low_fee,
            fee_cap,
            burn_cap,
            entrypoint ));
    command
      ~group
      ~desc:"Reveal the public key of the contract manager."
      (args9
         fee_arg
         dry_run_switch
         verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["reveal"; "key"; "for"]
      @@ Contract_alias.alias_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, source)
           cctxt ->
        match Contract.is_implicit source with
        | None -> failwith "only implicit accounts can be revealed"
        | Some source ->
            Client_keys_v0.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
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
            reveal
              cctxt
              ~dry_run
              ~verbose_signing
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~source
              ?fee
              ~src_pk
              ~src_sk
              ~fee_parameter
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Register the public key hash as a delegate."
      (args9
         fee_arg
         dry_run_switch
         verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["register"; "key"]
      @@ Public_key_hash.source_param ~name:"mgr" ~desc:"the delegate key"
      @@ prefixes ["as"; "delegate"]
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           src_pkh
           cctxt ->
        Client_keys_v0.get_key cctxt src_pkh >>=? fun (_, src_pk, src_sk) ->
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
        register_as_delegate
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee_parameter
          ~verbose_signing
          ?fee
          ~manager_sk:src_sk
          src_pk
        >>= function
        | Ok _ -> return_unit
        | Error [Environment.Ecoproto_error Delegate_storage.Active_delegate] ->
            cctxt#message "Delegate already activated." >>= fun () ->
            return_unit
        | Error el -> Lwt.return_error el);
  ]
  @ (match network with
    | Some `Mainnet -> []
    | Some `Testnet | None ->
        [
          command
            ~group
            ~desc:"Register and activate an Alphanet/Zeronet faucet account."
            (args2 (Secret_key.force_switch ()) encrypted_switch)
            (prefixes ["activate"; "account"]
            @@ Secret_key.fresh_alias_param @@ prefixes ["with"]
            @@ param
                 ~name:"activation_key"
                 ~desc:
                   "Activate an Alphanet/Zeronet faucet account from the JSON \
                    (file or directly inlined)."
                 json_file_or_text_parameter
            @@ stop)
            (fun (force, encrypted) name activation_json cctxt ->
              Secret_key.of_fresh cctxt force name >>=? fun name ->
              match
                Data_encoding.Json.destruct
                  Client_proto_context.activation_key_encoding
                  activation_json
              with
              | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
                  Format.kasprintf
                    (fun s -> failwith "%s" s)
                    "Invalid activation file: %a %a"
                    (fun ppf -> Data_encoding.Json.print_error ppf)
                    exn
                    Data_encoding.Json.pp
                    activation_json
              | key ->
                  activate_account
                    cctxt
                    ~chain:cctxt#chain
                    ~block:cctxt#block
                    ?confirmations:cctxt#confirmations
                    ~encrypted
                    ~force
                    key
                    name
                  >>=? fun _res -> return_unit);
        ])
  @ (match network with
    | Some `Testnet | None -> []
    | Some `Mainnet ->
        [
          command
            ~group
            ~desc:"Activate a fundraiser account."
            (args1 dry_run_switch)
            (prefixes ["activate"; "fundraiser"; "account"]
            @@ Public_key_hash.alias_param @@ prefixes ["with"]
            @@ param
                 ~name:"code"
                 (Tezos_clic.parameter (fun _ctx code ->
                      protect (fun () ->
                          return
                            (Blinded_public_key_hash.activation_code_of_hex
                               code))))
                 ~desc:"Activation code obtained from the Tezos foundation."
            @@ stop)
            (fun dry_run (name, _pkh) code cctxt ->
              activate_existing_account
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                name
                code
              >>=? fun _res -> return_unit);
        ])
  @ [
      command
        ~desc:"Wait until an operation is included in a block"
        (args3
           (default_arg
              ~long:"confirmations"
              ~placeholder:"num_blocks"
              ~doc:
                "wait until 'N' additional blocks after the operation appears \
                 in the considered chain"
              ~default:"0"
              non_negative_param)
           (default_arg
              ~long:"check-previous"
              ~placeholder:"num_blocks"
              ~doc:"number of previous blocks to check"
              ~default:"10"
              non_negative_param)
           (arg
              ~long:"branch"
              ~placeholder:"block_hash"
              ~doc:
                "hash of the oldest block where we should look for the \
                 operation"
              block_hash_param))
        (prefixes ["wait"; "for"]
        @@ param
             ~name:"operation"
             ~desc:"Operation to be included"
             (parameter (fun _ x ->
                  match Operation_hash.of_b58check_opt x with
                  | None ->
                      Error_monad.failwith "Invalid operation hash: '%s'" x
                  | Some hash -> return hash))
        @@ prefixes ["to"; "be"; "included"]
        @@ stop)
        (fun (confirmations, predecessors, branch)
             operation_hash
             (ctxt : Protocol_client_context.full) ->
          Client_confirmations.wait_for_operation_inclusion
            ctxt
            ~chain:ctxt#chain
            ~confirmations
            ~predecessors
            ?branch
            operation_hash
          >>=? fun _ -> return_unit);
      command
        ~desc:"Get receipt for past operation"
        (args1
           (default_arg
              ~long:"check-previous"
              ~placeholder:"num_blocks"
              ~doc:"number of previous blocks to check"
              ~default:"10"
              non_negative_param))
        (prefixes ["get"; "receipt"; "for"]
        @@ param
             ~name:"operation"
             ~desc:"Operation to be looked up"
             (parameter (fun _ x ->
                  match Operation_hash.of_b58check_opt x with
                  | None ->
                      Error_monad.failwith "Invalid operation hash: '%s'" x
                  | Some hash -> return hash))
        @@ stop)
        (fun predecessors operation_hash (ctxt : Protocol_client_context.full) ->
          display_receipt_for_operation
            ctxt
            ~chain:ctxt#chain
            ~predecessors
            operation_hash
          >>=? fun _ -> return_unit);
      command
        ~group:binary_description
        ~desc:"Describe unsigned block header"
        no_options
        (fixed ["describe"; "unsigned"; "block"; "header"])
        (fun () (cctxt : Protocol_client_context.full) ->
          cctxt#message
            "%a"
            Data_encoding.Binary_schema.pp
            (Data_encoding.Binary.describe
               Alpha_context.Block_header.unsigned_encoding)
          >>= fun () -> return_unit);
      command
        ~group:binary_description
        ~desc:"Describe unsigned operation"
        no_options
        (fixed ["describe"; "unsigned"; "operation"])
        (fun () (cctxt : Protocol_client_context.full) ->
          cctxt#message
            "%a"
            Data_encoding.Binary_schema.pp
            (Data_encoding.Binary.describe
               Alpha_context.Operation.unsigned_encoding)
          >>= fun () -> return_unit);
      command
        ~group
        ~desc:"Submit protocol proposals"
        (args3
           dry_run_switch
           verbose_signing_switch
           (switch
              ~doc:
                "Do not fail when the checks that try to prevent the user from \
                 shooting themselves in the foot do."
              ~long:"force"
              ()))
        (prefixes ["submit"; "proposals"; "for"]
        @@ Contract_alias.destination_param
             ~name:"delegate"
             ~desc:"the delegate who makes the proposal"
        @@ seq_of_param
             (param
                ~name:"proposal"
                ~desc:"the protocol hash proposal to be submitted"
                (parameter (fun _ x ->
                     match Protocol_hash.of_b58check_opt x with
                     | None ->
                         Error_monad.failwith "Invalid proposal hash: '%s'" x
                     | Some hash -> return hash))))
        (fun (dry_run, verbose_signing, force)
             (_name, source)
             proposals
             (cctxt : Protocol_client_context.full) ->
          match Contract.is_implicit source with
          | None -> failwith "only implicit accounts can submit proposals"
          | Some src_pkh -> (
              Client_keys_v0.get_key cctxt src_pkh
              >>=? fun (src_name, _src_pk, src_sk) ->
              get_period_info
              (* Find period info of the successor, because the operation will
                 be injected on the next block at the earliest *)
                ~successor:true
                ~chain:cctxt#chain
                ~block:cctxt#block
                cctxt
              >>=? fun info ->
              (match info.current_period_kind with
              | Proposal -> return_unit
              | _ -> cctxt#error "Not in a proposal period")
              >>=? fun () ->
              Shell_services.Protocol.list cctxt >>=? fun known_protos ->
              get_proposals ~chain:cctxt#chain ~block:cctxt#block cctxt
              >>=? fun known_proposals ->
              Alpha_services.Voting.listings cctxt (cctxt#chain, cctxt#block)
              >>=? fun listings ->
              (* for a proposal to be valid it must either a protocol that was already
                 proposed by somebody else or a protocol known by the node, because
                 the user is the first proposer and just injected it with
                 tezos-admin-client *)
              let check_proposals proposals : bool tzresult Lwt.t =
                let n = List.length proposals in
                let errors = ref [] in
                let error ppf =
                  Format.kasprintf (fun s -> errors := s :: !errors) ppf
                in
                if n = 0 then error "Empty proposal list." ;
                if n > Constants.fixed.max_proposals_per_delegate then
                  error
                    "Too many proposals: %d > %d."
                    n
                    Constants.fixed.max_proposals_per_delegate ;
                (match
                   Base.List.find_all_dups
                     ~compare:Protocol_hash.compare
                     proposals
                 with
                | [] -> ()
                | dups ->
                    error
                      "There %s: %a."
                      (if Compare.List_length_with.(dups = 1) then
                       "is a duplicate proposal"
                      else "are duplicate proposals")
                      Format.(
                        pp_print_list
                          ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
                          Protocol_hash.pp)
                      dups) ;
                List.iter
                  (fun (p : Protocol_hash.t) ->
                    if
                      List.mem ~equal:Protocol_hash.equal p known_protos
                      || Environment.Protocol_hash.Map.mem p known_proposals
                    then ()
                    else
                      error
                        "Protocol %a is not a known proposal."
                        Protocol_hash.pp
                        p)
                  proposals ;
                if
                  not
                    (List.exists
                       (fun (pkh, _) ->
                         Tezos_crypto.Signature.V0.Public_key_hash.equal
                           pkh
                           src_pkh)
                       listings)
                then
                  error
                    "Public-key-hash `%a` from account `%s` does not appear to \
                     have voting rights."
                    Tezos_crypto.Signature.V0.Public_key_hash.pp
                    src_pkh
                    src_name ;
                if !errors <> [] then
                  cctxt#message
                    "There %s with the submission:%t"
                    (if Compare.List_length_with.(!errors = 1) then
                     "is an issue"
                    else "are issues")
                    Format.(
                      fun ppf ->
                        pp_print_cut ppf () ;
                        pp_open_vbox ppf 0 ;
                        List.iter
                          (fun msg ->
                            pp_open_hovbox ppf 2 ;
                            pp_print_string ppf "* " ;
                            pp_print_text ppf msg ;
                            pp_close_box ppf () ;
                            pp_print_cut ppf ())
                          !errors ;
                        pp_close_box ppf ())
                  >>= fun () -> return_false
                else return_true
              in
              check_proposals proposals >>=? fun all_valid ->
              (if all_valid then cctxt#message "All proposals are valid."
              else if force then
                cctxt#message
                  "Some proposals are not valid, but `--force` was used."
              else cctxt#error "Submission failed because of invalid proposals.")
              >>= fun () ->
              submit_proposals
                ~dry_run
                ~verbose_signing
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ~src_sk
                src_pkh
                proposals
              >>= function
              | Ok _res -> return_unit
              | Error errs ->
                  (match errs with
                  | [
                   Unregistered_error
                     (`O [("kind", `String "generic"); ("error", `String msg)]);
                  ] ->
                      cctxt#message
                        "Error:@[<hov>@.%a@]"
                        Format.pp_print_text
                        (String.split_on_char ' ' msg
                        |> List.filter (function
                               | "" | "\n" -> false
                               | _ -> true)
                        |> String.concat " "
                        |> String.map (function '\n' | '\t' -> ' ' | c -> c))
                  | el -> cctxt#message "Error:@ %a" pp_print_trace el)
                  >>= fun () -> failwith "Failed to submit proposals"));
      command
        ~group
        ~desc:"Submit a ballot"
        (args2 verbose_signing_switch dry_run_switch)
        (prefixes ["submit"; "ballot"; "for"]
        @@ Contract_alias.destination_param
             ~name:"delegate"
             ~desc:"the delegate who votes"
        @@ param
             ~name:"proposal"
             ~desc:"the protocol hash proposal to vote for"
             (parameter (fun _ x ->
                  match Protocol_hash.of_b58check_opt x with
                  | None -> failwith "Invalid proposal hash: '%s'" x
                  | Some hash -> return hash))
        @@ param
             ~name:"ballot"
             ~desc:"the ballot value (yea/yay, nay, or pass)"
             (parameter
                ~autocomplete:(fun _ -> return ["yea"; "nay"; "pass"])
                (fun _ s ->
                  (* We should have [Vote.of_string]. *)
                  match String.lowercase_ascii s with
                  | "yay" | "yea" -> return Vote.Yay
                  | "nay" -> return Vote.Nay
                  | "pass" -> return Vote.Pass
                  | s -> failwith "Invalid ballot: '%s'" s))
        @@ stop)
        (fun (verbose_signing, dry_run)
             (_name, source)
             proposal
             ballot
             (cctxt : Protocol_client_context.full) ->
          match Contract.is_implicit source with
          | None -> failwith "only implicit accounts can submit ballot"
          | Some src_pkh ->
              Client_keys_v0.get_key cctxt src_pkh
              >>=? fun (_src_name, _src_pk, src_sk) ->
              get_period_info
              (* Find period info of the successor, because the operation will
                 be injected on the next block at the earliest *)
                ~successor:true
                ~chain:cctxt#chain
                ~block:cctxt#block
                cctxt
              >>=? fun info ->
              (match info.current_period_kind with
              | Testing_vote | Promotion_vote -> return_unit
              | _ ->
                  cctxt#error "Not in a Testing_vote or Promotion_vote period")
              >>=? fun () ->
              submit_ballot
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ~src_sk
                src_pkh
                ~verbose_signing
                ~dry_run
                proposal
                ballot
              >>=? fun _res -> return_unit);
      command
        ~group
        ~desc:"Summarize the current voting period"
        no_options
        (fixed ["show"; "voting"; "period"])
        (fun () (cctxt : Protocol_client_context.full) ->
          get_period_info ~chain:cctxt#chain ~block:cctxt#block cctxt
          >>=? fun info ->
          cctxt#message
            "Current period: %a\nBlocks remaining until end of period: %ld"
            Data_encoding.Json.pp
            (Data_encoding.Json.construct
               Alpha_context.Voting_period.kind_encoding
               info.current_period_kind)
            info.remaining
          >>= fun () ->
          Shell_services.Protocol.list cctxt >>=? fun known_protos ->
          get_proposals ~chain:cctxt#chain ~block:cctxt#block cctxt
          >>=? fun props ->
          let ranks =
            Environment.Protocol_hash.Map.bindings props
            |> List.sort (fun (_, v1) (_, v2) -> Int32.(compare v2 v1))
          in
          let print_proposal = function
            | None -> assert false (* not called during proposal phase *)
            | Some proposal ->
                cctxt#message "Current proposal: %a" Protocol_hash.pp proposal
          in
          match info.current_period_kind with
          | Proposal ->
              cctxt#answer
                "Current proposals:%t"
                Format.(
                  fun ppf ->
                    pp_print_cut ppf () ;
                    pp_open_vbox ppf 0 ;
                    List.iter
                      (fun (p, w) ->
                        fprintf
                          ppf
                          "* %a %ld (%sknown by the node)@."
                          Protocol_hash.pp
                          p
                          w
                          (if List.mem ~equal:Protocol_hash.equal p known_protos
                          then ""
                          else "not "))
                      ranks ;
                    pp_close_box ppf ())
              >>= fun () -> return_unit
          | Testing_vote | Promotion_vote ->
              print_proposal info.current_proposal >>= fun () ->
              get_ballots_info ~chain:cctxt#chain ~block:cctxt#block cctxt
              >>=? fun ballots_info ->
              cctxt#answer
                "Ballots: %a@,\
                 Current participation %.2f%%, necessary quorum %.2f%%@,\
                 Current in favor %ld, needed supermajority %ld"
                Data_encoding.Json.pp
                (Data_encoding.Json.construct
                   Vote.ballots_encoding
                   ballots_info.ballots)
                (Int32.to_float ballots_info.participation /. 100.)
                (Int32.to_float ballots_info.current_quorum /. 100.)
                ballots_info.ballots.yay
                ballots_info.supermajority
              >>= fun () -> return_unit
          | Testing | Adoption ->
              print_proposal info.current_proposal >>= fun () -> return_unit);
    ]
