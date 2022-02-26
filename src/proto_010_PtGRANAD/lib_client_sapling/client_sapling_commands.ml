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

open Clic
open Client_keys
open Tezos_sapling.Core.Client

let save_json_to_file json file =
  let output_channel = open_out_bin file in
  let ppf = Format.formatter_of_out_channel output_channel in
  Data_encoding.Json.pp ppf json ;
  Format.pp_print_flush ppf () ;
  close_out output_channel

let group =
  {
    Clic.name = "sapling";
    title = "Commands for working with Sapling transactions";
  }

let memo_size_arg =
  let open Clic in
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

let for_contract_arg =
  Client_proto_contracts.ContractAlias.destination_arg
    ~name:"for-contract"
    ~doc:"name of the contract to associate new key with"
    ()

let unencrypted_switch () =
  Clic.switch
    ~long:"unencrypted"
    ~doc:"Do not encrypt the key on-disk (for testing and debugging)."
    ()

let generate_key_cmd =
  command
    ~group
    ~desc:"Generate a new sapling key."
    (args2 (Sapling_key.force_switch ()) (unencrypted_switch ()))
    (prefixes ["sapling"; "gen"; "key"] @@ Sapling_key.fresh_alias_param @@ stop)
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
  command
    ~group
    ~desc:"Use a sapling key for a contract."
    (args1 memo_size_arg)
    (prefixes ["sapling"; "use"; "key"]
    @@ Sapling_key.alias_param
         ~name:"sapling-key"
         ~desc:"Sapling key to use for the contract."
    @@ prefixes ["for"; "contract"]
    @@ Client_proto_contracts.ContractAlias.destination_param
         ~name:"contract"
         ~desc:"Contract the key will be used on."
    @@ stop)
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
  command
    ~group
    ~desc:"Restore a sapling key from mnemonic."
    (args3
       (Sapling_key.force_switch ())
       (unencrypted_switch ())
       (Clic.arg
          ~long:"mnemonic"
          ~placeholder:"mnemonic"
          ~doc:"Mnemonic as an option, only used for testing and debugging."
          Client_proto_args.string_parameter))
    (prefixes ["sapling"; "import"; "key"]
    @@ Sapling_key.fresh_alias_param @@ stop)
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
    Clic.param
      ~name:"child-index"
      ~desc:"Index of the child to derive."
      Client_proto_args.int_parameter
  in
  let index_arg =
    Clic.arg
      ~doc:"index of the address to generate"
      ~long:"address-index"
      ~placeholder:"idx"
      Client_proto_args.int_parameter
  in
  [
    generate_key_cmd;
    use_key_for_contract_cmd;
    import_key_cmd;
    command
      ~group
      ~desc:"Derive a key from an existing one using zip32."
      (args4
         (Sapling_key.force_switch ())
         for_contract_arg
         (unencrypted_switch ())
         memo_size_arg)
      (prefixes ["sapling"; "derive"; "key"]
      @@ Sapling_key.fresh_alias_param @@ prefix "from"
      @@ Sapling_key.alias_param
      @@ prefixes ["at"; "index"]
      @@ child_index_param @@ stop)
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
        let (_, contract) =
          WithExceptions.Option.get ~loc:__LOC__ contract_opt
        in
        Context.Client_state.register
          cctxt
          ~default_memo_size
          ~force
          contract
          vk);
    command
      ~group
      ~desc:"Generate an address for a key referenced by alias."
      (args1 index_arg)
      (prefixes ["sapling"; "gen"; "address"] @@ Sapling_key.alias_param @@ stop)
      (fun index_opt (name, _sapling_uri) (cctxt : Protocol_client_context.full) ->
        Wallet.new_address cctxt name index_opt
        >>=? fun (_, corrected_index, address) ->
        let address_b58 =
          Base58.simple_encode Viewing_key.address_b58check_encoding address
        in
        cctxt#message
          "Generated address:@.%s@.at index %Ld"
          address_b58
          (Viewing_key.index_to_int64 corrected_index)
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Save a sapling viewing key in a JSON file."
      no_options
      (prefixes ["sapling"; "export"; "key"]
      @@ Sapling_key.alias_param @@ prefix "in"
      @@ Clic.param
           ~name:"file"
           ~desc:"Filename."
           Client_proto_args.string_parameter
      @@ stop)
      (fun () (name, _sapling_uri) file (cctxt : Protocol_client_context.full) ->
        Wallet.export_vk cctxt name >>=? fun vk_json ->
        return (save_json_to_file vk_json file));
    command
      ~group
      ~desc:"Get balance associated with given sapling key and contract"
      (args1
         (Clic.switch
            ~doc:"Print the collection of non-spent inputs."
            ~short:'v'
            ~long:"verbose"
            ()))
      (prefixes ["sapling"; "get"; "balance"; "for"]
      @@ Sapling_key.alias_param
           ~name:"sapling-key"
           ~desc:"Sapling key we get balance for."
      @@ prefixes ["in"; "contract"]
      @@ Client_proto_contracts.ContractAlias.destination_param
           ~name:"contract"
           ~desc:"Contract we get balance from."
      @@ stop)
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
    command
      ~group
      ~desc:"List sapling keys."
      no_options
      (fixed ["sapling"; "list"; "keys"])
      (fun () (cctxt : Protocol_client_context.full) ->
        Sapling_key.load cctxt >>=? fun l ->
        List.iter_s
          (fun (s, _) -> cctxt#message "%s" s)
          (List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) l)
        >>= fun () -> return_unit);
  ]
