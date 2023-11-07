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

open Client_keys
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
  let open Lwt_result_syntax in
  let* _, pk, sk = Client_keys.get_key cctxt source in
  return (pk, sk)

let viewing_key_of_string s =
  let exception Unknown_sapling_address in
  let encoding = Viewing_key.address_b58check_encoding in
  WithExceptions.Option.to_exn
    ~none:Unknown_sapling_address
    (Tezos_crypto.Base58.simple_decode encoding s)

(** All signatures are done with an anti-replay string.
    In Tezos' protocol this string is set to be chain_id + KT1. **)
let anti_replay cctxt contract =
  let open Lwt_result_syntax in
  let* chain_id =
    Tezos_shell_services.Chain_services.chain_id cctxt ~chain:cctxt#chain ()
  in
  let address = Protocol.Contract_hash.to_b58check contract in
  let chain_id = Chain_id.to_b58check chain_id in
  return (address ^ chain_id)

(** The shielded tez contract expects the recipient pkh encoded in Micheline
    in the bound_data of an unshield operation. *)
let bound_data_of_public_key_hash cctxt dst =
  let open Lwt_result_syntax in
  let open Tezos_micheline in
  let open Protocol.Michelson_v1_primitives in
  let pkh_bytes =
    Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding dst
  in
  let micheline_bytes = Micheline.(Bytes (0, pkh_bytes) |> strip_locations) in
  let micheline_pkh_type =
    Micheline.(Prim (0, T_key_hash, [], []) |> strip_locations)
  in
  let* bound_data, _ =
    Plugin.RPC.Scripts.pack_data
      cctxt
      (cctxt#chain, cctxt#block)
      ~gas:None
      ~data:micheline_bytes
      ~ty:micheline_pkh_type
  in
  return (Bytes.to_string bound_data)

let do_unshield cctxt contract src_name stez dst =
  let open Lwt_result_syntax in
  let* anti_replay = anti_replay cctxt contract in
  let* src, _, backdst = Wallet.new_address cctxt src_name None in
  let* contract_state = Context.Client_state.sync_and_scan cctxt contract in
  let* bound_data = bound_data_of_public_key_hash cctxt dst in
  Lwt.return
  @@ Context.unshield ~src ~bound_data ~backdst stez contract_state anti_replay

let do_shield cctxt ?message contract utez dst =
  let open Lwt_result_syntax in
  let* anti_replay = anti_replay cctxt contract in
  let* contract_state = Context.Client_state.sync_and_scan cctxt contract in
  let dst = viewing_key_of_string dst in
  Context.shield cctxt ~dst ?message utez contract_state anti_replay

let do_sapling_transfer cctxt ?message contract src_name amount dst =
  let open Lwt_result_syntax in
  let* anti_replay = anti_replay cctxt contract in
  let* src, _, backdst = Wallet.new_address cctxt src_name None in
  let* contract_state = Context.Client_state.sync_and_scan cctxt contract in
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
  let open Lwt_result_syntax in
  let open Client_proto_args in
  let open Client_proto_context_commands in
  let open Client_proto_contracts in
  Tezos_clic.command
    ~group
    ~desc:"Shield tokens from an implicit account to a Sapling address."
    (Tezos_clic.args9
       fee_arg
       dry_run_switch
       verbose_signing_switch
       gas_limit_arg
       storage_limit_arg
       counter_arg
       no_print_source_flag
       fee_parameter_args
       message_arg)
    (Tezos_clic.prefixes ["sapling"; "shield"]
    @@ tez_param
         ~name:"qty"
         ~desc:"Amount taken from transparent wallet of source."
    @@ Tezos_clic.prefix "from"
    @@ Client_keys.Public_key_hash.source_param
         ~name:"src-tz"
         ~desc:"Transparent source account."
    @@ Tezos_clic.prefix "to"
    @@ Tezos_clic.string ~name:"dst-sap" ~desc:"Sapling address of destination."
    @@ Tezos_clic.prefix "using"
    @@ Originated_contract_alias.destination_param
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
           fee_parameter,
           message )
         amount
         source
         sapling_dst
         contract_dst
         cctxt ->
      let* src_pk, src_sk = keys_of_implicit_account cctxt source in
      let open Context in
      let*! () =
        cctxt#warning
          "Shielding %a from %a to %s@ entails a loss of privacy@."
          Tez.pp
          amount
          Signature.Public_key_hash.pp
          source
          sapling_dst
      in
      let* sapling_input =
        do_shield cctxt ?message contract_dst amount sapling_dst
      in
      let arg = sapling_transaction_as_arg sapling_input in
      let*! errors =
        Client_proto_context.transfer
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~fee_parameter
          ~amount
          ~src_pk
          ~src_sk
          ~destination:(Originated contract_dst)
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
      in
      let*! (_ : _ option) =
        report_michelson_errors
          ~no_print_source
          ~msg:"transfer simulation failed"
          cctxt
          errors
      in
      return_unit)

let unshield_cmd =
  let open Lwt_result_syntax in
  let open Client_proto_args in
  let open Client_proto_context_commands in
  let open Client_proto_contracts in
  Tezos_clic.command
    ~group
    ~desc:"Unshield tokens from a Sapling address to an implicit account."
    (Tezos_clic.args8
       fee_arg
       dry_run_switch
       verbose_signing_switch
       gas_limit_arg
       storage_limit_arg
       counter_arg
       no_print_source_flag
       fee_parameter_args)
    (Tezos_clic.prefixes ["sapling"; "unshield"]
    @@ tez_param
         ~name:"qty"
         ~desc:"Amount taken from shielded wallet of source."
    @@ Tezos_clic.prefix "from"
    @@ Sapling_key.alias_param
         ~name:"src-sap"
         ~desc:"Sapling account of source."
    @@ Tezos_clic.prefix "to"
    @@ Client_keys.Public_key_hash.source_param
         ~name:"dst-tz"
         ~desc:"Transparent destination account."
    @@ Tezos_clic.prefix "using"
    @@ Originated_contract_alias.destination_param
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
           fee_parameter )
         amount
         (name, _sapling_uri)
         source
         contract_dst
         cctxt ->
      let open Context in
      let stez = Shielded_tez.of_tez amount in
      let*! () =
        cctxt#warning
          "Unshielding %a from %s to %a@ entails a loss of privacy@."
          Shielded_tez.pp
          stez
          name
          Signature.Public_key_hash.pp
          source
      in
      let* src_pk, src_sk = keys_of_implicit_account cctxt source in
      let* sapling_input = do_unshield cctxt contract_dst name stez source in
      let arg = sapling_transaction_as_arg sapling_input in
      let*! errors =
        Client_proto_context.transfer
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~fee_parameter
          ~amount:Tez.zero
          ~src_sk
          ~src_pk
          ~destination:(Originated contract_dst)
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
      in
      let*! (_ : _ option) =
        report_michelson_errors
          ~no_print_source
          ~msg:"transfer simulation failed"
          cctxt
          errors
      in
      return_unit)

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
  let open Lwt_result_syntax in
  let open Client_proto_args in
  let open Client_proto_context_commands in
  let open Client_proto_contracts in
  Tezos_clic.command
    ~group
    ~desc:"Forge a sapling transaction and save it to a file."
    (Tezos_clic.args11
       fee_arg
       dry_run_switch
       verbose_signing_switch
       gas_limit_arg
       storage_limit_arg
       counter_arg
       no_print_source_flag
       fee_parameter_args
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
    @@ Originated_contract_alias.destination_param
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
           _fee_parameter,
           message,
           file,
           use_json_format )
         amount
         (name, _sapling_uri)
         destination
         contract_dst
         cctxt ->
      let open Context in
      let stez = Shielded_tez.of_tez amount in
      let* transaction =
        do_sapling_transfer cctxt ?message contract_dst name stez destination
      in
      let file = Option.value ~default:sapling_transaction_file file in
      let*! () = cctxt#message "Writing transaction to %s@." file in
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
  let open Lwt_result_syntax in
  let open Client_proto_context_commands in
  let open Client_proto_args in
  let open Client_proto_contracts in
  Tezos_clic.command
    ~group
    ~desc:"Submit a forged sapling transaction."
    (Tezos_clic.args9
       fee_arg
       dry_run_switch
       verbose_signing_switch
       gas_limit_arg
       storage_limit_arg
       counter_arg
       no_print_source_flag
       fee_parameter_args
       json_switch)
    (Tezos_clic.prefixes ["sapling"; "submit"]
    (* TODO: Add a dedicated abstracted Tezos_clic element to parse filenames,
       potentially using Sys.file_exists *)
    @@ Tezos_clic.string
         ~name:"file"
         ~desc:"Filename of the forged transaction."
    @@ Tezos_clic.prefix "from"
    @@ Client_keys.Public_key_hash.source_param
         ~name:"alias-tz"
         ~desc:"Transparent account paying the fees."
    @@ Tezos_clic.prefix "using"
    @@ Originated_contract_alias.destination_param
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
           fee_parameter,
           use_json_format )
         filename
         source
         destination
         (cctxt : Protocol_client_context.full) ->
      let*! () =
        cctxt#message
          "Reading forge transaction from file %s -- sending it to %a@."
          filename
          Protocol.Contract_hash.pp
          destination
      in
      let open Context in
      let* transaction =
        if use_json_format then
          let* json = Lwt_utils_unix.Json.read_file filename in
          return @@ Data_encoding.Json.destruct UTXO.transaction_encoding json
        else
          let*! hex = Lwt_utils_unix.read_file filename in
          let hex =
            (* remove 0x *)
            String.sub hex 2 (String.length hex - 2)
          in
          return
          @@ Data_encoding.Binary.of_bytes_exn
               UTXO.transaction_encoding
               Hex.(to_bytes_exn (`Hex hex))
      in
      let contract_input = sapling_transaction_as_arg transaction in
      let chain = cctxt#chain and block = cctxt#block in
      let* src_pk, src_sk = keys_of_implicit_account cctxt source in
      let*! errors =
        Client_proto_context.transfer
          cctxt
          ~chain
          ~block
          ~fee_parameter
          ~amount:Tez.zero
          ~src_pk
          ~src_sk
          ~destination:(Originated destination)
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
      in
      let*! (_ : _ option) =
        report_michelson_errors
          ~no_print_source
          ~msg:"transfer simulation failed"
          cctxt
          errors
      in
      return_unit)

let for_contract_arg =
  Client_proto_contracts.Originated_contract_alias.destination_arg
    ~name:"for-contract"
    ~doc:"name of the contract to associate new key with"
    ()

let unencrypted_switch () =
  Tezos_clic.switch
    ~long:"unencrypted"
    ~doc:"Do not encrypt the key on-disk (for testing and debugging)."
    ()

let generate_key_cmd =
  let open Lwt_result_syntax in
  Tezos_clic.command
    ~group
    ~desc:"Generate a new sapling key."
    (Tezos_clic.args2 (Sapling_key.force_switch ()) (unencrypted_switch ()))
    (Tezos_clic.prefixes ["sapling"; "gen"; "key"]
    @@ Sapling_key.fresh_alias_param @@ Tezos_clic.stop)
    (fun (force, unencrypted) name (cctxt : Protocol_client_context.full) ->
      let* name = Sapling_key.of_fresh cctxt force name in
      let mnemonic = Mnemonic.new_random in
      let*! () =
        cctxt#message
          "It is important to save this mnemonic in a secure place:@\n\
           @\n\
           %a@\n\
           @\n\
           The mnemonic can be used to recover your spending key.@."
          Mnemonic.words_pp
          (Bip39.to_words mnemonic)
      in
      let* _vk = Wallet.register cctxt ~force ~unencrypted mnemonic name in
      return_unit)

let use_key_for_contract_cmd =
  let open Lwt_result_syntax in
  Tezos_clic.command
    ~group
    ~desc:"Use a sapling key for a contract."
    (Tezos_clic.args1 memo_size_arg)
    (Tezos_clic.prefixes ["sapling"; "use"; "key"]
    @@ Sapling_key.alias_param
         ~name:"sapling-key"
         ~desc:"Sapling key to use for the contract."
    @@ Tezos_clic.prefixes ["for"; "contract"]
    @@ Client_proto_contracts.Originated_contract_alias.destination_param
         ~name:"contract"
         ~desc:"Contract the key will be used on."
    @@ Tezos_clic.stop)
    (fun default_memo_size
         (name, _sapling_uri)
         contract
         (cctxt : Protocol_client_context.full) ->
      let* vk = Wallet.find_vk cctxt name in
      Context.Client_state.register
        cctxt
        ~default_memo_size
        ~force:false
        contract
        vk)

let import_key_cmd =
  let open Lwt_result_syntax in
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
      let* words =
        match mnemonic_opt with
        | None ->
            let rec loop_words (acc : string list) i =
              if i > 23 then return (List.rev acc)
              else
                let* word_raw = cctxt#prompt_password "Enter word %d: " i in
                let word = Bytes.to_string word_raw in
                match Bip39.index_of_word word with
                | None -> loop_words acc i
                | Some _ -> loop_words (word :: acc) (succ i)
            in
            loop_words [] 0
        | Some mnemonic -> return (String.split_on_char ' ' mnemonic)
      in
      match Bip39.of_words words with
      | None -> failwith "Not a valid mnemonic"
      | Some mnemonic ->
          let* name = Sapling_key.of_fresh cctxt force fresh_name in
          let* _ = Wallet.register cctxt ~force ~unencrypted mnemonic name in
          return_unit)

let commands () =
  let open Lwt_result_syntax in
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
        let* new_name = Sapling_key.of_fresh cctxt force fresh_name in
        let* path, vk =
          Wallet.derive
            cctxt
            ~force
            ~unencrypted
            existing_name
            new_name
            child_index
        in
        let*! () =
          cctxt#message
            "Derived new key %s from %s with path %s@."
            new_name
            existing_name
            path
        in
        (* TODO must pass contract address for now *)
        let contract = WithExceptions.Option.get ~loc:__LOC__ contract_opt in
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
        let* _, corrected_index, address =
          Wallet.new_address cctxt name index_opt
        in
        let address_b58 =
          Tezos_crypto.Base58.simple_encode
            Viewing_key.address_b58check_encoding
            address
        in
        let*! () =
          cctxt#message
            "Generated address:@.%s@.at index %Ld"
            address_b58
            (Viewing_key.index_to_int64 corrected_index)
        in
        return_unit);
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
        let* vk_json = Wallet.export_vk cctxt name in
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
      @@ Client_proto_contracts.Originated_contract_alias.destination_param
           ~name:"contract"
           ~desc:"Contract we get balance from."
      @@ Tezos_clic.stop)
      (fun verbose
           (name, _sapling_uri)
           contract
           (cctxt : Protocol_client_context.full) ->
        let*! vk_opt = Wallet.find_vk cctxt name in
        match vk_opt with
        | Error _ -> cctxt#error "Account %s not found" name
        | Ok vk -> (
            let* contract_state =
              Context.Client_state.sync_and_scan cctxt contract
            in
            Context.Contract_state.find_account vk contract_state |> function
            | None -> cctxt#error "Account %s not found" name
            | Some account ->
                let*! () =
                  if verbose then
                    cctxt#answer
                      "@[<v 2>Received Sapling transactions for %s@,@[<v>%a@]@]"
                      name
                      Context.Account.pp_unspent
                      account
                  else Lwt.return_unit
                in
                let*! () =
                  cctxt#answer
                    "Total Sapling funds %a%s"
                    Context.Shielded_tez.pp
                    (Context.Account.balance account)
                    Operation_result.tez_sym
                in
                return_unit));
    Tezos_clic.command
      ~group
      ~desc:"List sapling keys."
      Tezos_clic.no_options
      (Tezos_clic.fixed ["sapling"; "list"; "keys"])
      (fun () (cctxt : Protocol_client_context.full) ->
        let* l = Sapling_key.load cctxt in
        let*! () =
          List.iter_s
            (fun (s, _) -> cctxt#message "%s" s)
            (List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) l)
        in
        return_unit);
    shield_cmd;
    unshield_cmd;
    forge_shielded_cmd;
    submit_shielded_cmd;
  ]
