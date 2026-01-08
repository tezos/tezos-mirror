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

open Client_keys

let group =
  {
    Tezos_clic.name = "keys";
    title = "Commands for managing the wallet of cryptographic keys";
  }

let algo_param () =
  let open Lwt_result_syntax in
  Tezos_clic.parameter
    ~autocomplete:(fun _ -> return ["ed25519"; "secp256k1"; "p256"; "bls"])
    (fun _ name ->
      match name with
      | "ed25519" -> return Signature.Ed25519
      | "secp256k1" -> return Signature.Secp256k1
      | "p256" -> return Signature.P256
      | "bls" -> return Signature.Bls
      | name ->
          failwith
            "Unknown signature algorithm (%s). Available: 'ed25519', \
             'secp256k1','p256' or 'bls'"
            name)

let sig_algo_arg =
  Tezos_clic.default_arg
    ~doc:"use custom signature algorithm"
    ~long:"sig"
    ~short:'s'
    ~placeholder:"ed25519|secp256k1|p256|bls"
    ~default:"ed25519"
    (algo_param ())

let algo_to_prefix algo =
  let open Signature in
  match algo with
  | Ed25519 -> "tz1"
  | Secp256k1 -> "tz2"
  | P256 -> "tz3"
  | Bls -> "tz4"

let gen_keys_containing ?(encrypted = false) ?(prefix = false)
    ?(ignore_case = false) ?(force = false) ~algo ~containing ~name
    (cctxt : #Client_context.io_wallet) =
  let open Lwt_result_syntax in
  let unrepresentable =
    List.filter
      (fun s ->
        not
        @@ Tezos_crypto.Base58.Alphabet.all_in_alphabet
             ~ignore_case
             Tezos_crypto.Base58.Alphabet.bitcoin
             s)
      containing
  in
  let good_initial_char = "KLMNPQRSTUVWXYZabcdefghi" in
  let bad_initial_char =
    if ignore_case then "123456789Jj" else "123456789ABCDEFGHJjkmnopqrstuvwxyz"
  in
  let containing =
    if ignore_case then List.map String.lowercase_ascii containing
    else containing
  in
  match unrepresentable with
  | _ :: _ ->
      cctxt#error
        "@[<v 0>The following words can't be written in the key alphabet: %a.@,\
         Valid characters: %a@,\
         Extra restriction for the first character: %s@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           (fun ppf s -> Format.fprintf ppf "'%s'" s))
        unrepresentable
        Tezos_crypto.Base58.Alphabet.pp
        Tezos_crypto.Base58.Alphabet.bitcoin
        good_initial_char
  | [] -> (
      let unrepresentable =
        List.filter
          (fun s -> prefix && String.contains bad_initial_char s.[0])
          containing
      in
      match unrepresentable with
      | _ :: _ ->
          cctxt#error
            "@[<v 0>The following words don't respect the first character \
             restriction: %a.@,\
             Valid characters: %a@,\
             Extra restriction for the first character: %s@]"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
               (fun ppf s -> Format.fprintf ppf "'%s'" s))
            unrepresentable
            Tezos_crypto.Base58.Alphabet.pp
            Tezos_crypto.Base58.Alphabet.bitcoin
            good_initial_char
      | [] ->
          let* name_exists = Public_key_hash.mem cctxt name in
          if name_exists && not force then
            let*! () =
              cctxt#warning
                "Key for name '%s' already exists. Use --force to update."
                name
            in
            return_unit
          else
            let*! () =
              cctxt#warning
                "This process uses a brute force search and may take a long \
                 time to find a key."
            in
            let adjust_case =
              if ignore_case then String.lowercase_ascii else Fun.id
            in
            let matches =
              if prefix then
                let tz_prefix =
                  List.map (( ^ ) (algo_to_prefix algo)) containing
                in
                fun key ->
                  List.exists
                    (fun containing ->
                      String.sub (adjust_case key) 0 (String.length containing)
                      = containing)
                    tz_prefix
              else
                let re = Re.Str.regexp (String.concat "\\|" containing) in
                fun key ->
                  try
                    ignore (Re.Str.search_forward re (adjust_case key) 0) ;
                    true
                  with Not_found -> false
            in
            let rec loop attempts =
              let public_key_hash, public_key, secret_key =
                Signature.generate_key ~algo ()
              in
              let hash =
                Signature.Public_key_hash.to_b58check
                @@ Signature.Public_key.hash public_key
              in
              if matches hash then
                let*? pk_uri =
                  Tezos_signer_backends.Unencrypted.make_pk public_key
                in
                let* sk_uri =
                  if encrypted then
                    Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt
                      cctxt
                      secret_key
                  else
                    Lwt.return
                      (Tezos_signer_backends.Unencrypted.make_sk secret_key)
                in
                let* () =
                  register_key
                    cctxt
                    ~force
                    (public_key_hash, pk_uri, sk_uri)
                    ~public_key
                    name
                in
                return hash
              else
                let*! () =
                  if attempts mod 25_000 = 0 then
                    cctxt#message
                      "Tried %d keys without finding a match"
                      attempts
                  else Lwt.return_unit
                in
                let*! () = Lwt.pause () in
                loop (attempts + 1)
            in
            let* key_hash = loop 1 in
            let*! () =
              cctxt#message "Generated '%s' under the name '%s'." key_hash name
            in
            return_unit)

let rec input_fundraiser_params (cctxt : #Client_context.io_wallet) =
  let open Lwt_result_syntax in
  let rec get_boolean_answer (cctxt : #Client_context.io_wallet) ~default ~msg =
    let prompt = if default then "(Y/n/q)" else "(y/N/q)" in
    let* gen = cctxt#prompt "%s %s: " msg prompt in
    match (default, String.lowercase_ascii gen) with
    | default, "" -> return default
    | _, "y" -> return_true
    | _, "n" -> return_false
    | _, "q" -> failwith "Exit by user request."
    | _ -> get_boolean_answer cctxt ~msg ~default
  in
  let* email = cctxt#prompt "Enter the e-mail used for the paper wallet: " in
  let rec loop_words acc i =
    if i > 14 then return (List.rev acc)
    else
      let* word = cctxt#prompt_password "Enter word %d: " i in
      match Bip39.index_of_word (Bytes.to_string word) with
      | None -> loop_words acc i
      | Some wordidx -> loop_words (wordidx :: acc) (succ i)
  in
  let* words = loop_words [] 0 in
  match Bip39.of_indices words with
  | None -> assert false
  | Some t -> (
      let* password =
        cctxt#prompt_password "Enter the password used for the paper wallet: "
      in
      (* TODO: unicode normalization (NFKD)... *)
      let passphrase = Bytes.(cat (of_string email) password) in
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
      let msg =
        Format.asprintf
          "Your public Tezos address is %a is that correct?"
          Signature.Public_key_hash.pp
          pkh
      in
      let* b = get_boolean_answer cctxt ~msg ~default:true in
      match b with true -> return sk | false -> input_fundraiser_params cctxt)

let fail_if_already_registered cctxt force pk_uri name =
  let open Lwt_result_syntax in
  let* o = Public_key.find_opt cctxt name in
  match o with
  | None -> return_unit
  | Some (pk_uri_found, _) ->
      fail_unless
        (pk_uri = pk_uri_found || force)
        (error_of_fmt
           "public and secret keys '%s' don't correspond, please don't use \
            --force"
           name)

let keys_count_param =
  let open Tezos_clic in
  param
    ~name:"keys_count"
    ~desc:"How many keys to generate"
    (parameter (fun _ s ->
         let open Lwt_result_syntax in
         match int_of_string_opt s with
         | None -> failwith "number of keys must be an integer"
         | Some x ->
             if x < 0 then failwith "number of keys must be positive"
             else return x))

(** The kind of info that the [generate_test_keys] command outputs. *)
type source = {
  pkh : Signature.public_key_hash;
  pk : Signature.public_key;
  sk : Signature.secret_key;
}

let source_encoding =
  let open Data_encoding in
  conv
    (fun {pkh; pk; sk} -> (pkh, pk, sk))
    (fun (pkh, pk, sk) -> {pkh; pk; sk})
    (obj3
       (req "pkh" Signature.Public_key_hash.encoding)
       (req "pk" Signature.Public_key.encoding)
       (req "sk" Signature.Secret_key.encoding))

let source_list_encoding = Data_encoding.list source_encoding

(* Simple helpers used to manage wallet files a raw way. *)
module Wallet_helpers = struct
  let write_file path str =
    let open Lwt_syntax in
    let* fd = Lwt_unix.openfile path Unix.[O_CREAT; O_TRUNC; O_RDWR] 0o644 in
    let* _written_bytes =
      Lwt.catch
        (fun () -> Lwt_unix.write_string fd str 0 (String.length str))
        (fun exn ->
          let* () = Lwt_unix.close fd in
          Lwt.fail exn)
    in
    Lwt_unix.close fd

  module Aliases = struct
    let encoding = list (obj1 (req "alias" Data_encoding.string))

    let name = "aliases"
  end
end

(** Generate an array of accounts for testing purposes, store them
    into a wallet and output them to stdout in the JSON
    format.

    It is essential that this command lives here and not in the
    protocol-specific code because it should be available before a
    protocol is activated. *)
let generate_test_keys =
  let open Tezos_clic in
  let alias_prefix_param =
    arg
      ~long:"alias-prefix"
      ~placeholder:"PREFIX"
      ~doc:
        "use a custom alias prefix (default: bootstrap). Keys will be \
         generated with alias \"PREFIX<ID>\" where ID is unique for all key"
      (parameter (fun _ s -> Lwt_result_syntax.return s))
  in
  command
    ~group
    ~desc:"Generate an array of accounts for testing purposes."
    (args2 alias_prefix_param sig_algo_arg)
    (prefixes ["stresstest"; "gen"; "keys"] @@ keys_count_param @@ stop)
    (fun (alias_prefix, algo) n (cctxt : Client_context.full) ->
      let open Lwt_result_syntax in
      (* By default, the alias prefix matches the bootstrap<idx>
         pattern used in sandboxed mode.*)
      let alias_prefix =
        match alias_prefix with
        | None -> fun i -> Format.sprintf "bootstrap%d" (i + 6)
        | Some alias_prefix -> Format.sprintf "%s%d" alias_prefix
      in
      let* source_list =
        List.init_es ~when_negative_length:[] n (fun i ->
            let alias = alias_prefix i in
            let pkh, pk, sk = Signature.generate_key ~algo () in
            let*? pk_uri = Tezos_signer_backends.Unencrypted.make_pk pk in
            let*? sk_uri = Tezos_signer_backends.Unencrypted.make_sk sk in
            return ({pkh; pk; sk}, pk_uri, sk_uri, alias))
      in
      (* All keys are registered into a single wallet. *)
      let* () =
        register_keys
          cctxt
          (List.rev_map
             (fun (x, pk_uri, sk_uri, alias) ->
               (alias, x.pkh, x.pk, pk_uri, sk_uri))
             source_list)
      in
      (* Extract and write wallet aliases *)
      let aliases = List.rev_map (fun (_, _, _, alias) -> alias) source_list in
      let wallet_path = cctxt#get_base_dir in
      let*! () =
        Wallet_helpers.(
          write_file
            (Filename.concat wallet_path Aliases.name)
            Data_encoding.Json.(to_string (construct Aliases.encoding aliases)))
      in
      let json =
        Data_encoding.Json.construct
          source_list_encoding
          (List.map (fun (x, _, _, _) -> x) source_list)
      in
      let*! () = cctxt#message "%a@." Data_encoding.Json.pp json in
      return_unit)

let commands network : Client_context.full Tezos_clic.command list =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  let encrypted_switch () =
    if
      List.exists
        (fun (scheme, _) -> scheme = Tezos_signer_backends.Unencrypted.scheme)
        (Client_keys.registered_signers ())
    then Tezos_clic.switch ~long:"encrypted" ~doc:"Encrypt the key on-disk" ()
    else Tezos_clic.constant true
  in
  let unencrypted_switch () =
    if
      List.exists
        (fun (scheme, _) -> scheme = Tezos_signer_backends.Unencrypted.scheme)
        (Client_keys.registered_signers ())
    then
      Tezos_clic.switch
        ~long:"unencrypted"
        ~doc:"Store the secret key unencrypted on-disk"
        ()
    else Tezos_clic.constant false
  in
  let show_private_switch =
    switch ~long:"show-secret" ~short:'S' ~doc:"show the private key" ()
  in
  [
    generate_test_keys;
    command
      ~group
      ~desc:
        "List supported signing schemes.\n\
         Signing schemes are identifiers for signer modules: the built-in \
         signing routines, a hardware wallet, an external agent, etc.\n\
         Each signer has its own format for describing secret keys, such a raw \
         secret key for the default `unencrypted` scheme, the path on a \
         hardware security module, an alias for an external agent, etc.\n\
         This command gives the list of signer modules that this version of \
         the tezos client supports."
      no_options
      (fixed ["list"; "signing"; "schemes"])
      (fun () (cctxt : Client_context.full) ->
        let signers =
          List.sort
            (fun (ka, _) (kb, _) -> String.compare ka kb)
            (registered_signers ())
        in
        let*! () =
          List.iter_s
            (fun (n, signer) ->
              match signer with
              | (module S : SIGNER) ->
                  cctxt#message
                    "@[<v 2>Scheme `%s`: %s@,@[<hov 0>%a@]@]"
                    n
                    S.title
                    Format.pp_print_text
                    S.description)
            signers
        in
        return_unit);
    (match network with
    | Some `Mainnet ->
        command
          ~group
          ~desc:"Generate a pair of keys."
          (args3
             (Secret_key.force_switch ())
             sig_algo_arg
             (unencrypted_switch ()))
          (prefixes ["gen"; "keys"] @@ Secret_key.fresh_alias_param @@ stop)
          (fun (force, algo, unencrypted) name (cctxt : Client_context.full) ->
            let* name =
              Secret_key.of_fresh ~show_existing_key:false cctxt force name
            in
            let pkh, pk, sk = Signature.generate_key ~algo () in
            let*? pk_uri = Tezos_signer_backends.Unencrypted.make_pk pk in
            let* sk_uri =
              if unencrypted then
                Lwt.return (Tezos_signer_backends.Unencrypted.make_sk sk)
              else
                Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt
                  cctxt
                  sk
            in
            register_key cctxt ~force (pkh, pk_uri, sk_uri) name)
    | Some `Testnet | None ->
        command
          ~group
          ~desc:"Generate a pair of keys."
          (args3
             (Secret_key.force_switch ())
             sig_algo_arg
             (encrypted_switch ()))
          (prefixes ["gen"; "keys"] @@ Secret_key.fresh_alias_param @@ stop)
          (fun (force, algo, encrypted) name (cctxt : Client_context.full) ->
            let* name =
              Secret_key.of_fresh ~show_existing_key:false cctxt force name
            in
            let pkh, pk, sk = Signature.generate_key ~algo () in
            let*? pk_uri = Tezos_signer_backends.Unencrypted.make_pk pk in
            let* sk_uri =
              if encrypted then
                Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt
                  cctxt
                  sk
              else Lwt.return (Tezos_signer_backends.Unencrypted.make_sk sk)
            in
            register_key cctxt ~force (pkh, pk_uri, sk_uri) name));
    (match network with
    | Some `Mainnet ->
        command
          ~group
          ~desc:"Generate keys including the given string."
          (args4
             (switch
                ~long:"prefix"
                ~short:'P'
                ~doc:"the key must begin with tz1[word]"
                ())
             (switch
                ~long:"ignore-case"
                ~short:'I'
                ~doc:"make the pattern case-insensitive"
                ())
             (force_switch ())
             sig_algo_arg)
          (prefixes ["gen"; "vanity"; "keys"]
          @@ Public_key_hash.fresh_alias_param @@ prefix "matching"
          @@ seq_of_param
          @@ string
               ~name:"words"
               ~desc:"string key must contain one of these words")
          (fun (prefix, ignore_case, force, algo)
               name
               containing
               (cctxt : Client_context.full)
             ->
            let* name = Public_key_hash.of_fresh cctxt force name in
            gen_keys_containing
              ~algo
              ~encrypted:true
              ~force
              ~prefix
              ~ignore_case
              ~containing
              ~name
              cctxt)
    | Some `Testnet | None ->
        command
          ~group
          ~desc:"Generate keys including the given string."
          (args5
             (switch
                ~long:"prefix"
                ~short:'P'
                ~doc:"the key must begin with tz1[word]"
                ())
             (switch
                ~long:"ignore-case"
                ~short:'I'
                ~doc:"make the pattern case-insensitive"
                ())
             (force_switch ())
             (encrypted_switch ())
             sig_algo_arg)
          (prefixes ["gen"; "vanity"; "keys"]
          @@ Public_key_hash.fresh_alias_param @@ prefix "matching"
          @@ seq_of_param
          @@ string
               ~name:"words"
               ~desc:"string key must contain one of these words")
          (fun (prefix, ignore_case, force, encrypted, algo)
               name
               containing
               (cctxt : Client_context.full)
             ->
            let* name = Public_key_hash.of_fresh cctxt force name in
            gen_keys_containing
              ~algo
              ~encrypted
              ~force
              ~prefix
              ~ignore_case
              ~containing
              ~name
              cctxt));
    command
      ~group
      ~desc:"Encrypt an unencrypted secret key."
      no_options
      (prefixes ["encrypt"; "secret"; "key"] @@ stop)
      (fun () (cctxt : Client_context.full) ->
        let* sk_uri = cctxt#prompt_password "Enter unencrypted secret key: " in
        let sk_uri = Uri.of_string (Bytes.to_string sk_uri) in
        let* () =
          match Uri.scheme sk_uri with
          | None | Some "unencrypted" -> return_unit
          | _ ->
              failwith
                "This command can only be used with the \"unencrypted\" scheme"
        in
        let* sk =
          Lwt.return (Signature.Secret_key.of_b58check (Uri.path sk_uri))
        in
        let* sk_uri =
          Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt cctxt sk
        in
        let*! () =
          cctxt#message "Encrypted secret key %a" Uri.pp_hum (sk_uri :> Uri.t)
        in
        return_unit);
    command
      ~group
      ~desc:"Add a secret key to the wallet."
      (args1 (Secret_key.force_switch ()))
      (prefix "import"
      @@ prefixes ["secret"; "key"]
      @@ Secret_key.fresh_alias_param @@ Client_keys.sk_uri_param @@ stop)
      (fun force name sk_uri (cctxt : Client_context.full) ->
        let* name =
          Secret_key.of_fresh ~show_existing_key:false cctxt force name
        in
        let* pk_uri = Client_keys.neuterize sk_uri in
        let* () = fail_if_already_registered cctxt force pk_uri name in
        let* pkh, public_key =
          Client_keys.import_secret_key
            ~io:(cctxt :> Client_context.io_wallet)
            pk_uri
        in
        let*! () =
          cctxt#message
            "Tezos address added: %a"
            Signature.Public_key_hash.pp
            pkh
        in
        register_key cctxt ~force (pkh, pk_uri, sk_uri) ?public_key name);
  ]
  @ (if network <> Some `Mainnet then []
     else
       [
         command
           ~group
           ~desc:"Add a fundraiser secret key to the wallet."
           (args1 (Secret_key.force_switch ()))
           (prefix "import"
           @@ prefixes ["fundraiser"; "secret"; "key"]
           @@ Secret_key.fresh_alias_param @@ stop)
           (fun force name (cctxt : Client_context.full) ->
             let* name =
               Secret_key.of_fresh ~show_existing_key:false cctxt force name
             in
             let* sk = input_fundraiser_params cctxt in
             let* sk_uri =
               Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt cctxt sk
             in
             let* pk_uri = Client_keys.neuterize sk_uri in
             let* () = fail_if_already_registered cctxt force pk_uri name in
             let* pkh, _public_key = Client_keys.public_key_hash pk_uri in
             register_key cctxt ~force (pkh, pk_uri, sk_uri) name);
       ])
  @ [
      command
        ~group
        ~desc:"Add a public key to the wallet."
        (args1 (Public_key.force_switch ()))
        (prefix "import"
        @@ prefixes ["public"; "key"]
        @@ Public_key.fresh_alias_param @@ Client_keys.pk_uri_param @@ stop)
        (fun force name pk_uri (cctxt : Client_context.full) ->
          let* name = Public_key.of_fresh cctxt force name in
          let* pkh, public_key = Client_keys.public_key_hash pk_uri in
          let* () = Public_key_hash.add ~force cctxt name pkh in
          let*! () =
            cctxt#message
              "Tezos address added: %a"
              Signature.Public_key_hash.pp
              pkh
          in
          Public_key.add ~force cctxt name (pk_uri, public_key));
      command
        ~group
        ~desc:"Add an address to the wallet."
        (args1 (Public_key.force_switch ()))
        (prefixes ["add"; "address"]
        @@ Public_key_hash.fresh_alias_param @@ Public_key_hash.source_param
        @@ stop)
        (fun force name hash cctxt ->
          let* name = Public_key_hash.of_fresh cctxt force name in
          Public_key_hash.add ~force cctxt name hash);
      command
        ~group
        ~desc:"List all addresses and associated keys."
        no_options
        (fixed ["list"; "known"; "addresses"])
        (fun () (cctxt : #Client_context.full) ->
          let* l = list_keys cctxt in
          List.iter_es
            (fun (name, pkh, pk, sk) ->
              let* v = Public_key_hash.to_source pkh in
              let*! () =
                match (pk, sk) with
                | None, None -> cctxt#message "%s: %s" name v
                | _, Some uri ->
                    let scheme =
                      Option.value ~default:"unencrypted"
                      @@ Uri.scheme (uri : sk_uri :> Uri.t)
                    in
                    cctxt#message "%s: %s (%s sk known)" name v scheme
                | Some _, _ -> cctxt#message "%s: %s (pk known)" name v
              in
              return_unit)
            l);
      command
        ~group
        ~desc:"List the keys known by the remote wallet."
        no_options
        (prefix "list"
        @@ prefixes ["known"; "remote"; "keys"]
        @@ Client_keys.uri_param @@ stop)
        (fun () uri (cctxt : Client_context.full) ->
          let* pkhs = Client_keys.list_known_keys uri in
          let*! () =
            cctxt#message
              "@[<v 2>Tezos remote known keys:@,%a@]"
              (Format.pp_print_list
                 ~pp_sep:Format.pp_print_cut
                 Signature.Public_key_hash.pp)
              pkhs
          in
          return_unit);
      command
        ~group
        ~desc:"Show the keys associated with an implicit account."
        (args1 show_private_switch)
        (prefixes ["show"; "address"] @@ Public_key_hash.alias_param @@ stop)
        (fun show_private (name, _) (cctxt : #Client_context.full) ->
          let* key_info = alias_keys cctxt name in
          match key_info with
          | None ->
              let*! () = cctxt#error "No keys found for address" in
              return_unit
          | Some (pkh, pk, skloc) -> (
              let*! () =
                cctxt#message "Hash: %a" Signature.Public_key_hash.pp pkh
              in
              match pk with
              | None -> return_unit
              | Some pk ->
                  let*! () =
                    cctxt#message "Public Key: %a" Signature.Public_key.pp pk
                  in
                  if show_private then
                    match skloc with
                    | None -> return_unit
                    | Some skloc ->
                        let* skloc = Secret_key.to_source skloc in
                        let*! () = cctxt#message "Secret Key: %s" skloc in
                        return_unit
                  else return_unit));
      command
        ~group
        ~desc:"Forget one address."
        (args1
           (Tezos_clic.switch
              ~long:"force"
              ~short:'f'
              ~doc:"delete associated keys when present"
              ()))
        (prefixes ["forget"; "address"] @@ Public_key_hash.alias_param @@ stop)
        (fun force (name, _pkh) (cctxt : Client_context.full) ->
          let* has_secret_key = Secret_key.mem cctxt name in
          let* has_public_key = Public_key.mem cctxt name in
          let* () =
            fail_when
              ((not force) && (has_secret_key || has_public_key))
              (error_of_fmt
                 "secret or public key present for %s, use --force to delete"
                 name)
          in
          let* () = Secret_key.del cctxt name in
          let* () = Public_key.del cctxt name in
          Public_key_hash.del cctxt name);
      command
        ~group
        ~desc:"Forget the entire wallet of keys."
        (args1
           (Tezos_clic.switch
              ~long:"force"
              ~short:'f'
              ~doc:"you got to use the force for that"
              ()))
        (fixed ["forget"; "all"; "keys"])
        (fun force (cctxt : Client_context.full) ->
          let* () =
            fail_unless
              force
              (error_of_fmt "this can only be used with option --force")
          in
          let* () = Public_key.set cctxt [] in
          let* () = Secret_key.set cctxt [] in
          Public_key_hash.set cctxt []);
      command
        ~group
        ~desc:"Compute deterministic nonce."
        no_options
        (prefixes ["generate"; "nonce"; "for"]
        @@ Public_key_hash.alias_param @@ prefixes ["from"]
        @@ string
             ~name:"data"
             ~desc:"string from which to deterministically generate the nonce"
        @@ stop)
        (fun () (name, _pkh) data (cctxt : Client_context.full) ->
          let data = Bytes.of_string data in
          let* sk_present = Secret_key.mem cctxt name in
          let* () =
            fail_unless
              sk_present
              (error_of_fmt "secret key not present for %s" name)
          in
          let* sk_uri = Secret_key.find cctxt name in
          let* nonce = Client_keys.deterministic_nonce sk_uri data in
          let*! () = cctxt#message "%a" Hex.pp (Hex.of_bytes nonce) in
          return_unit);
      command
        ~group
        ~desc:"Compute deterministic nonce hash."
        no_options
        (prefixes ["generate"; "nonce"; "hash"; "for"]
        @@ Public_key_hash.alias_param @@ prefixes ["from"]
        @@ string
             ~name:"data"
             ~desc:
               "string from which to deterministically generate the nonce hash"
        @@ stop)
        (fun () (name, _pkh) data (cctxt : Client_context.full) ->
          let data = Bytes.of_string data in
          let* sk_present = Secret_key.mem cctxt name in
          let* () =
            fail_unless
              sk_present
              (error_of_fmt "secret key not present for %s" name)
          in
          let* sk_uri = Secret_key.find cctxt name in
          let* nonce_hash = Client_keys.deterministic_nonce_hash sk_uri data in
          let*! () = cctxt#message "%a" Hex.pp (Hex.of_bytes nonce_hash) in
          return_unit);
      command
        ~group
        ~desc:
          "Import a pair of keys to the wallet from a mnemonic phrase. This \
           command uses the BIP39 algorithm, and therefore imports \
           public/secret keys that may be different from a Ledger application, \
           depending on the BIP32 derivation path used in the Ledger. This \
           command also uses the Ed25519 algorithm, which means it generates \
           tz1 public key hashes."
        (args2
           (Secret_key.force_switch ())
           (switch ~doc:"encrypt the secret key" ~long:"encrypt" ()))
        (prefix "import"
        @@ prefixes ["keys"; "from"; "mnemonic"]
        @@ Secret_key.fresh_alias_param @@ stop)
        (fun (force, encrypt) name (cctxt : Client_context.full) ->
          let* name =
            Secret_key.of_fresh ~show_existing_key:false cctxt force name
          in
          let* mnemonic = cctxt#prompt "Enter your mnemonic: " in
          let mnemonic = String.trim mnemonic |> String.split_on_char ' ' in
          match Bip39.of_words mnemonic with
          | None ->
              failwith
                "\"%s\" is not a valid BIP39 mnemonic. Please ensure that your \
                 mnemonic is of correct length, and that each word is \
                 separated by a single space. For reference, a correct \
                 mnemonic is comprised of 12, 15, 18, 21, or 24 words where \
                 the last is a checksum. Do not try to write your own \
                 mnemonic."
                (String.concat " " mnemonic)
          | Some t ->
              let* passphrase =
                cctxt#prompt_password "Enter your passphrase: "
              in
              let sk = Bip39.to_seed ~passphrase t in
              let sk = Bytes.sub sk 0 32 in
              let sk : Signature.Secret_key.t =
                Ed25519
                  (Data_encoding.Binary.of_bytes_exn
                     Signature.Ed25519.Secret_key.encoding
                     sk)
              in
              let*? unencrypted_sk_uri =
                Tezos_signer_backends.Unencrypted.make_sk sk
              in
              let* sk_uri =
                match encrypt with
                | true ->
                    Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt
                      cctxt
                      sk
                | false -> return unencrypted_sk_uri
              in
              let* pk_uri = neuterize unencrypted_sk_uri in
              let* () = fail_if_already_registered cctxt force pk_uri name in
              let* pkh, public_key =
                import_secret_key ~io:(cctxt :> Client_context.io_wallet) pk_uri
              in
              let* () =
                register_key cctxt ~force (pkh, pk_uri, sk_uri) ?public_key name
              in
              let*! () =
                cctxt#message
                  "Tezos address added: %a"
                  Signature.Public_key_hash.pp
                  pkh
              in
              return_unit);
    ]
