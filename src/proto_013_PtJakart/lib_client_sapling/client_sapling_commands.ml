(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Client_keys_v0
open Tezos_sapling.Core.Client

let json_switch = Tezos_clic.switch ~long:"json" ~doc:"Use JSON format" ()

let save_json_to_file json file =
  let output_channel = open_out_bin file in
  let ppf = Format.formatter_of_out_channel output_channel in
  Data_encoding.Json.pp ppf json ;
  Format.pp_print_flush ppf () ;
  close_out output_channel

let group =
  {
    Tezos_clic.name = "sapling";
    title = "Commands for working with Sapling transactions";
  }

let keys_of_implicit_account cctxt source =
  match Protocol.Alpha_context.Contract.is_implicit source with
  | None -> assert false
  | Some src ->
      Client_keys_v0.get_key cctxt src >>=? fun (_, pk, sk) ->
      return (src, pk, sk)

let viewing_key_of_string s =
  let exception Unknown_sapling_address in
  let encoding = Viewing_key.address_b58check_encoding in
  WithExceptions.Option.to_exn
    ~none:Unknown_sapling_address
    (Tezos_crypto.Base58.simple_decode encoding s)

(** All signatures are done with an anti-replay string.
    In Tezos' protocol this string is set to be chain_id + KT1. **)
let anti_replay cctxt contract =
  Tezos_shell_services.Chain_services.chain_id cctxt ~chain:cctxt#chain ()
  >>=? fun chain_id ->
  let address = Protocol.Alpha_context.Contract.to_b58check contract in
  let chain_id = Chain_id.to_b58check chain_id in
  return (address ^ chain_id)

(** The shielded tez contract expects the recipient pkh encoded in Micheline
    in the bound_data of an unshield operation. *)
let bound_data_of_public_key_hash cctxt dst =
  let open Tezos_micheline in
  let open Protocol.Michelson_v1_primitives in
  let pkh_bytes =
    Data_encoding.Binary.to_bytes_exn
      Tezos_crypto.Signature.V0.Public_key_hash.encoding
      dst
  in
  let micheline_bytes = Micheline.(Bytes (0, pkh_bytes) |> strip_locations) in
  let micheline_pkh_type =
    Micheline.(Prim (0, T_key_hash, [], []) |> strip_locations)
  in
  Plugin.RPC.Scripts.pack_data
    cctxt
    (cctxt#chain, cctxt#block)
    ~data:micheline_bytes
    ~ty:micheline_pkh_type
  >>=? fun (bound_data, _) -> return (Bytes.to_string bound_data)

let do_unshield cctxt contract src_name stez dst =
  anti_replay cctxt contract >>=? fun anti_replay ->
  Wallet.new_address cctxt src_name None >>=? fun (src, _, backdst) ->
  Context.Client_state.sync_and_scan cctxt contract >>=? fun contract_state ->
  bound_data_of_public_key_hash cctxt dst >>=? fun bound_data ->
  Lwt.return
  @@ Context.unshield ~src ~bound_data ~backdst stez contract_state anti_replay

let do_shield cctxt ?message contract utez dst =
  anti_replay cctxt contract >>=? fun anti_replay ->
  Context.Client_state.sync_and_scan cctxt contract >>=? fun contract_state ->
  let dst = viewing_key_of_string dst in
  Context.shield cctxt ~dst ?message utez contract_state anti_replay

let do_sapling_transfer cctxt ?message contract src_name amount dst =
  anti_replay cctxt contract >>=? fun anti_replay ->
  Wallet.new_address cctxt src_name None >>=? fun (src, _, backdst) ->
  Context.Client_state.sync_and_scan cctxt contract >>=? fun contract_state ->
  let dst = viewing_key_of_string dst in
  Context.transfer
    cctxt
    ~src
    ~dst
    ~backdst
    ?message
    amount
    contract_state
    anti_replay

let message_arg =
  let open Tezos_clic in
  arg
    ~long:"message"
    ~placeholder:""
    ~doc:"Message for Sapling transaction"
    (parameter (fun _ x -> return @@ Bytes.of_string x))

let memo_size_arg =
  let open Tezos_clic in
  arg
    ~long:"memo-size"
    ~placeholder:"memo-size"
    ~doc:"Expected length for message of Sapling transaction"
    (parameter (fun _ s ->
         match
           let i = int_of_string s in
           assert (i >= 0 && i <= 65535) ;
           i
         with
         | i -> return i
         | exception _ ->
             failwith "invalid memo-size (must be between 0 and 65535)"))

let shield_cmd =
  let open Client_proto_args in
  let open Client_proto_context_commands in
  let open Protocol.Alpha_context in
  let open Client_proto_contracts in
  Tezos_clic.command
    ~group
    ~desc:"Shield tokens from an implicit account to a Sapling address."
    (Tezos_clic.args14
       fee_arg
       dry_run_switch
       verbose_signing_switch
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
       message_arg)
    (Tezos_clic.prefixes ["sapling"; "shield"]
    @@ tez_param
         ~name:"qty"
         ~desc:"Amount taken from transparent wallet of source."
    @@ Tezos_clic.prefix "from"
    @@ Contract_alias.destination_param
         ~name:"src-tz"
         ~desc:"Transparent source account."
    @@ Tezos_clic.prefix "to"
    @@ Tezos_clic.string ~name:"dst-sap" ~desc:"Sapling address of destination."
    @@ Tezos_clic.prefix "using"
    @@ Contract_alias.destination_param
         ~name:"sapling contract"
         ~desc:"Smart contract to submit this transaction to."
    @@ Tezos_clic.stop)
    (fun ( fee,
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
           burn_cap,
           message )
         amount
         (_, source)
         sapling_dst
         (_contract_name, contract_dst)
         cctxt ->
      keys_of_implicit_account cctxt source >>=? fun (pkh, src_pk, src_sk) ->
      let open Context in
      cctxt#warning
        "Shielding %a from %a to %s@ entails a loss of privacy@."
        Tez.pp
        amount
        Contract.pp
        source
        sapling_dst
      >>= fun () ->
      do_shield cctxt ?message contract_dst amount sapling_dst
      >>=? fun sapling_input ->
      let arg = sapling_transaction_as_arg sapling_input in
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
      Client_proto_context.transfer
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~fee_parameter
        ~amount
        ~src_pk
        ~src_sk
        ~destination:(Contract contract_dst)
        ~source:pkh
        ~arg
        ?confirmations:cctxt#confirmations
        ?fee
        ~dry_run
        ~verbose_signing
        ?gas_limit
        ?storage_limit
        ?counter
        ()
      >>= fun errors ->
      report_michelson_errors
        ~no_print_source
        ~msg:"transfer simulation failed"
        cctxt
        errors
      >>= function
      | None -> return_unit
      | Some (_res, _contracts) -> return_unit)

let unshield_cmd =
  let open Client_proto_args in
  let open Client_proto_context_commands in
  let open Protocol.Alpha_context in
  let open Client_proto_contracts in
  Tezos_clic.command
    ~group
    ~desc:"Unshield tokens from a Sapling address to an implicit account."
    (Tezos_clic.args13
       fee_arg
       dry_run_switch
       verbose_signing_switch
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
    (Tezos_clic.prefixes ["sapling"; "unshield"]
    @@ tez_param
         ~name:"qty"
         ~desc:"Amount taken from shielded wallet of source."
    @@ Tezos_clic.prefix "from"
    @@ Sapling_key.alias_param
         ~name:"src-sap"
         ~desc:"Sapling account of source."
    @@ Tezos_clic.prefix "to"
    @@ Contract_alias.destination_param
         ~name:"dst-tz"
         ~desc:"Transparent destination account."
    @@ Tezos_clic.prefix "using"
    @@ Contract_alias.destination_param
         ~name:"sapling contract"
         ~desc:"Smart contract to submit this transaction to."
    @@ Tezos_clic.stop)
    (fun ( fee,
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
         amount
         (name, _sapling_uri)
         (_, tz_dst)
         (_contract_name, contract_dst)
         cctxt ->
      let open Context in
      let stez = Shielded_tez.of_tez amount in
      cctxt#warning
        "Unshielding %a from %s to %a@ entails a loss of privacy@."
        Shielded_tez.pp
        stez
        name
        Contract.pp
        tz_dst
      >>= fun () ->
      keys_of_implicit_account cctxt tz_dst >>=? fun (source, src_pk, src_sk) ->
      do_unshield cctxt contract_dst name stez source >>=? fun sapling_input ->
      let arg = sapling_transaction_as_arg sapling_input in
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
      Client_proto_context.transfer
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~fee_parameter
        ~amount:Tez.zero
        ~src_pk
        ~src_sk
        ~destination:(Contract contract_dst)
        ~source
        ~arg
        ?confirmations:cctxt#confirmations
        ?fee
        ~dry_run
        ~verbose_signing
        ?gas_limit
        ?storage_limit
        ?counter
        ()
      >>= fun errors ->
      report_michelson_errors
        ~no_print_source
        ~msg:"transfer simulation failed"
        cctxt
        errors
      >>= function
      | None -> return_unit
      | Some (_res, _contracts) -> return_unit)

(* Default name for Sapling transaction file *)
let sapling_transaction_file = "sapling_transaction"

let file_arg default_filename =
  let open Tezos_clic in
  arg
    ~long:"file"
    ~placeholder:default_filename
    ~doc:"file name"
    (parameter (fun _ x -> return x))

(** Shielded transaction are first forged and printed in a file.
    Then they are submitted with the next command. **)
let forge_shielded_cmd =
  let open Client_proto_args in
  let open Client_proto_context_commands in
  let open Client_proto_contracts in
  Tezos_clic.command
    ~group
    ~desc:"Forge a sapling transaction and save it to a file."
    (Tezos_clic.args16
       fee_arg
       dry_run_switch
       verbose_signing_switch
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
       message_arg
       (file_arg sapling_transaction_file)
       json_switch)
    (Tezos_clic.prefixes ["sapling"; "forge"; "transaction"]
    @@ tez_param
         ~name:"qty"
         ~desc:"Amount taken from shielded wallet of source."
    @@ Tezos_clic.prefix "from"
    @@ Sapling_key.alias_param
         ~name:"src-sap"
         ~desc:"Sapling account of source."
    @@ Tezos_clic.prefix "to"
    @@ Tezos_clic.string ~name:"dst-sap" ~desc:"Sapling address of destination."
    @@ Tezos_clic.prefix "using"
    @@ Contract_alias.destination_param
         ~name:"sapling contract"
         ~desc:"Smart contract to submit this transaction to."
    @@ Tezos_clic.stop)
    (fun ( _fee,
           _dry_run,
           _verbose_signing,
           _gas_limit,
           _storage_limit,
           _counter,
           _no_print_source,
           _minimal_fees,
           _minimal_nanotez_per_byte,
           _minimal_nanotez_per_gas_unit,
           _force_low_fee,
           _fee_cap,
           _burn_cap,
           message,
           file,
           use_json_format )
         amount
         (name, _sapling_uri)
         destination
         (_contract_name, contract_dst)
         cctxt ->
      let open Context in
      let stez = Shielded_tez.of_tez amount in
      do_sapling_transfer cctxt ?message contract_dst name stez destination
      >>=? fun transaction ->
      let file = Option.value ~default:sapling_transaction_file file in
      cctxt#message "Writing transaction to %s@." file >>= fun () ->
      (if use_json_format then
       save_json_to_file
         (Data_encoding.Json.construct UTXO.transaction_encoding transaction)
         file
      else
        let bytes =
          Hex.of_bytes
            (Data_encoding.Binary.to_bytes_exn
               UTXO.transaction_encoding
               transaction)
        in
        let file = open_out_bin file in
        Printf.fprintf file "0x%s" (Hex.show bytes) ;
        close_out file) ;
      return_unit)

let submit_shielded_cmd =
  let open Client_proto_context_commands in
  let open Client_proto_args in
  let open Client_proto_contracts in
  Tezos_clic.command
    ~group
    ~desc:"Submit a forged sapling transaction."
    (Tezos_clic.args14
       fee_arg
       dry_run_switch
       verbose_signing_switch
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
       json_switch)
    (Tezos_clic.prefixes ["sapling"; "submit"]
    (* TODO: Add a dedicated abstracted Tezos_clic element to parse filenames,
       potentially using Sys.file_exists *)
    @@ Tezos_clic.string
         ~name:"file"
         ~desc:"Filename of the forged transaction."
    @@ Tezos_clic.prefix "from"
    @@ Contract_alias.destination_param
         ~name:"alias-tz"
         ~desc:"Transparent account paying the fees."
    @@ Tezos_clic.prefix "using"
    @@ Contract_alias.destination_param
         ~name:"sapling contract"
         ~desc:"Smart contract to submit this transaction to."
    @@ Tezos_clic.stop)
    (fun ( fee,
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
           burn_cap,
           use_json_format )
         filename
         (_, source)
         (contract_name, destination)
         (cctxt : Protocol_client_context.full) ->
      cctxt#message
        "Reading forge transaction from file %s -- sending it to %s@."
        filename
        contract_name
      >>= fun () ->
      let open Context in
      (if use_json_format then
       Lwt_utils_unix.Json.read_file filename >>=? fun json ->
       return @@ Data_encoding.Json.destruct UTXO.transaction_encoding json
      else
        Lwt_utils_unix.read_file filename >>= fun hex ->
        let hex =
          (* remove 0x *)
          String.sub hex 2 (String.length hex - 2)
        in
        return
        @@ Data_encoding.Binary.of_bytes_exn
             UTXO.transaction_encoding
             Hex.(to_bytes_exn (`Hex hex)))
      >>=? fun transaction ->
      return (sapling_transaction_as_arg transaction) >>=? fun contract_input ->
      let chain = cctxt#chain and block = cctxt#block in
      keys_of_implicit_account cctxt source >>=? fun (source, src_pk, src_sk) ->
      let open Protocol.Alpha_context in
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
      Client_proto_context.transfer
        cctxt
        ~chain
        ~block
        ~fee_parameter
        ~amount:Tez.zero
        ~src_pk
        ~src_sk
        ~destination:(Contract destination)
        ~source
        ~arg:contract_input
        ?confirmations:cctxt#confirmations
        ?fee
        ~dry_run
        ~verbose_signing
        ?gas_limit
        ?storage_limit
        ?counter
        ()
      >>= fun errors ->
      report_michelson_errors
        ~no_print_source
        ~msg:"transfer simulation failed"
        cctxt
        errors
      >>= function
      | None -> return_unit
      | Some (_res, _contracts) -> return_unit)

let for_contract_arg =
  Client_proto_contracts.Contract_alias.destination_arg
    ~name:"for-contract"
    ~doc:"name of the contract to associate new key with"
    ()

let unencrypted_switch () =
  Tezos_clic.switch
    ~long:"unencrypted"
    ~doc:"Do not encrypt the key on-disk (for testing and debugging)."
    ()

let generate_key_cmd =
  Tezos_clic.command
    ~group
    ~desc:"Generate a new sapling key."
    (Tezos_clic.args2 (Sapling_key.force_switch ()) (unencrypted_switch ()))
    (Tezos_clic.prefixes ["sapling"; "gen"; "key"]
    @@ Sapling_key.fresh_alias_param @@ Tezos_clic.stop)
    (fun (force, unencrypted) name (cctxt : Protocol_client_context.full) ->
      Sapling_key.of_fresh cctxt force name >>=? fun name ->
      let mnemonic = Wallet.Mnemonic.new_random in
      cctxt#message
        "It is important to save this mnemonic in a secure place:@\n\
         @\n\
         %a@\n\
         @\n\
         The mnemonic can be used to recover your spending key.@."
        Wallet.Mnemonic.words_pp
        (Bip39.to_words mnemonic)
      >>= fun () ->
      Wallet.register cctxt ~force ~unencrypted mnemonic name >>=? fun _vk ->
      return_unit)

let use_key_for_contract_cmd =
  Tezos_clic.command
    ~group
    ~desc:"Use a sapling key for a contract."
    (Tezos_clic.args1 memo_size_arg)
    (Tezos_clic.prefixes ["sapling"; "use"; "key"]
    @@ Sapling_key.alias_param
         ~name:"sapling-key"
         ~desc:"Sapling key to use for the contract."
    @@ Tezos_clic.prefixes ["for"; "contract"]
    @@ Client_proto_contracts.Contract_alias.destination_param
         ~name:"contract"
         ~desc:"Contract the key will be used on."
    @@ Tezos_clic.stop)
    (fun default_memo_size
         (name, _sapling_uri)
         (_contract_name, contract)
         (cctxt : Protocol_client_context.full) ->
      Wallet.find_vk cctxt name >>=? fun vk ->
      Context.Client_state.register
        cctxt
        ~default_memo_size
        ~force:false
        contract
        vk)

let import_key_cmd =
  Tezos_clic.command
    ~group
    ~desc:"Restore a sapling key from mnemonic."
    (Tezos_clic.args3
       (Sapling_key.force_switch ())
       (unencrypted_switch ())
       (Tezos_clic.arg
          ~long:"mnemonic"
          ~placeholder:"mnemonic"
          ~doc:"Mnemonic as an option, only used for testing and debugging."
          Client_proto_args.string_parameter))
    (Tezos_clic.prefixes ["sapling"; "import"; "key"]
    @@ Sapling_key.fresh_alias_param @@ Tezos_clic.stop)
    (fun (force, unencrypted, mnemonic_opt)
         fresh_name
         (cctxt : Protocol_client_context.full) ->
      (match mnemonic_opt with
      | None ->
          let rec loop_words (acc : string list) i =
            if i > 23 then return (List.rev acc)
            else
              cctxt#prompt_password "Enter word %d: " i >>=? fun word_raw ->
              let word = Bytes.to_string word_raw in
              match Bip39.index_of_word word with
              | None -> loop_words acc i
              | Some _ -> loop_words (word :: acc) (succ i)
          in
          loop_words [] 0
      | Some mnemonic -> return (String.split_on_char ' ' mnemonic))
      >>=? fun words ->
      match Bip39.of_words words with
      | None -> failwith "Not a valid mnemonic"
      | Some mnemonic ->
          Sapling_key.of_fresh cctxt force fresh_name >>=? fun name ->
          Wallet.register cctxt ~force ~unencrypted mnemonic name >>=? fun _ ->
          return_unit)

let commands () =
  let child_index_param =
    Tezos_clic.param
      ~name:"child-index"
      ~desc:"Index of the child to derive."
      Client_proto_args.int_parameter
  in
  let index_arg =
    Tezos_clic.arg
      ~doc:"index of the address to generate"
      ~long:"address-index"
      ~placeholder:"idx"
      Client_proto_args.int_parameter
  in
  [
    generate_key_cmd;
    use_key_for_contract_cmd;
    import_key_cmd;
    Tezos_clic.command
      ~group
      ~desc:"Derive a key from an existing one using zip32."
      (Tezos_clic.args4
         (Sapling_key.force_switch ())
         for_contract_arg
         (unencrypted_switch ())
         memo_size_arg)
      (Tezos_clic.prefixes ["sapling"; "derive"; "key"]
      @@ Sapling_key.fresh_alias_param @@ Tezos_clic.prefix "from"
      @@ Sapling_key.alias_param
      @@ Tezos_clic.prefixes ["at"; "index"]
      @@ child_index_param @@ Tezos_clic.stop)
      (fun (force, contract_opt, unencrypted, default_memo_size)
           fresh_name
           (existing_name, _existing_uri)
           child_index
           (cctxt : Protocol_client_context.full) ->
        Sapling_key.of_fresh cctxt force fresh_name >>=? fun new_name ->
        Wallet.derive
          cctxt
          ~force
          ~unencrypted
          existing_name
          new_name
          child_index
        >>=? fun (path, vk) ->
        cctxt#message
          "Derived new key %s from %s with path %s@."
          new_name
          existing_name
          path
        >>= fun () ->
        (* TODO must pass contract address for now *)
        let _, contract = WithExceptions.Option.get ~loc:__LOC__ contract_opt in
        Context.Client_state.register
          cctxt
          ~default_memo_size
          ~force
          contract
          vk);
    Tezos_clic.command
      ~group
      ~desc:"Generate an address for a key referenced by alias."
      (Tezos_clic.args1 index_arg)
      (Tezos_clic.prefixes ["sapling"; "gen"; "address"]
      @@ Sapling_key.alias_param @@ Tezos_clic.stop)
      (fun index_opt (name, _sapling_uri) (cctxt : Protocol_client_context.full) ->
        Wallet.new_address cctxt name index_opt
        >>=? fun (_, corrected_index, address) ->
        let address_b58 =
          Tezos_crypto.Base58.simple_encode
            Viewing_key.address_b58check_encoding
            address
        in
        cctxt#message
          "Generated address:@.%s@.at index %Ld"
          address_b58
          (Viewing_key.index_to_int64 corrected_index)
        >>= fun () -> return_unit);
    Tezos_clic.command
      ~group
      ~desc:"Save a sapling viewing key in a JSON file."
      Tezos_clic.no_options
      (Tezos_clic.prefixes ["sapling"; "export"; "key"]
      @@ Sapling_key.alias_param @@ Tezos_clic.prefix "in"
      @@ Tezos_clic.param
           ~name:"file"
           ~desc:"Filename."
           Client_proto_args.string_parameter
      @@ Tezos_clic.stop)
      (fun () (name, _sapling_uri) file (cctxt : Protocol_client_context.full) ->
        Wallet.export_vk cctxt name >>=? fun vk_json ->
        return (save_json_to_file vk_json file));
    Tezos_clic.command
      ~group
      ~desc:"Get balance associated with given sapling key and contract"
      (Tezos_clic.args1
         (Tezos_clic.switch
            ~doc:"Print the collection of non-spent inputs."
            ~short:'v'
            ~long:"verbose"
            ()))
      (Tezos_clic.prefixes ["sapling"; "get"; "balance"; "for"]
      @@ Sapling_key.alias_param
           ~name:"sapling-key"
           ~desc:"Sapling key we get balance for."
      @@ Tezos_clic.prefixes ["in"; "contract"]
      @@ Client_proto_contracts.Contract_alias.destination_param
           ~name:"contract"
           ~desc:"Contract we get balance from."
      @@ Tezos_clic.stop)
      (fun verbose
           (name, _sapling_uri)
           (_contract_name, contract)
           (cctxt : Protocol_client_context.full) ->
        Wallet.find_vk cctxt name >>= function
        | Error _ -> cctxt#error "Account %s not found" name
        | Ok vk -> (
            Context.Client_state.sync_and_scan cctxt contract
            >>=? fun contract_state ->
            Context.Contract_state.find_account vk contract_state |> function
            | None -> cctxt#error "Account %s not found" name
            | Some account ->
                (if verbose then
                 cctxt#answer
                   "@[<v 2>Received Sapling transactions for %s@,@[<v>%a@]@]"
                   name
                   Context.Account.pp_unspent
                   account
                else Lwt.return_unit)
                >>= fun () ->
                cctxt#answer
                  "Total Sapling funds %a%s"
                  Context.Shielded_tez.pp
                  (Context.Account.balance account)
                  Client_proto_args.tez_sym
                >>= fun () -> return_unit));
    Tezos_clic.command
      ~group
      ~desc:"List sapling keys."
      Tezos_clic.no_options
      (Tezos_clic.fixed ["sapling"; "list"; "keys"])
      (fun () (cctxt : Protocol_client_context.full) ->
        Sapling_key.load cctxt >>=? fun l ->
        List.iter_s
          (fun (s, _) -> cctxt#message "%s" s)
          (List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) l)
        >>= fun () -> return_unit);
    shield_cmd;
    unshield_cmd;
    forge_shielded_cmd;
    submit_shielded_cmd;
  ]
