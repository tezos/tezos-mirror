(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type Base58.data += Encrypted_ed25519 of Bytes.t

type Base58.data += Encrypted_secp256k1 of Bytes.t

type Base58.data += Encrypted_p256 of Bytes.t

type Base58.data += Encrypted_secp256k1_element of Bytes.t

type Base58.data += Encrypted_bls12_381 of Bytes.t

type encrypted_sk = Encrypted_aggregate_sk | Encrypted_sk of Signature.algo

type decrypted_sk =
  | Decrypted_aggregate_sk of Aggregate_signature.Secret_key.t
  | Decrypted_sk of Signature.Secret_key.t

open Client_keys

let scheme = "encrypted"

let aggregate_scheme = "aggregate_encrypted"

module Raw = struct
  (* https://tools.ietf.org/html/rfc2898#section-4.1 *)
  let salt_len = 8

  (* Fixed zero nonce *)
  let nonce = Crypto_box.zero_nonce

  (* Secret keys for Ed25519, secp256k1, P256 have the same size. *)
  let encrypted_size = Crypto_box.tag_length + Hacl.Ed25519.sk_size

  let pbkdf ~salt ~password =
    Pbkdf.SHA512.pbkdf2 ~count:32768 ~dk_len:32l ~salt ~password

  let encrypt ~password sk =
    let salt = Hacl.Rand.gen salt_len in
    let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~salt ~password) in
    let msg =
      match (sk : decrypted_sk) with
      | Decrypted_sk (Ed25519 sk) ->
          Data_encoding.Binary.to_bytes_exn Ed25519.Secret_key.encoding sk
      | Decrypted_sk (Secp256k1 sk) ->
          Data_encoding.Binary.to_bytes_exn Secp256k1.Secret_key.encoding sk
      | Decrypted_sk (P256 sk) ->
          Data_encoding.Binary.to_bytes_exn P256.Secret_key.encoding sk
      | Decrypted_aggregate_sk (Bls12_381 sk) ->
          Data_encoding.Binary.to_bytes_exn Bls.Secret_key.encoding sk
    in
    Bytes.cat salt (Crypto_box.Secretbox.secretbox key msg nonce)

  let encrypt_pvss ~password sk =
    let salt = Hacl.Rand.gen salt_len in
    let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~salt ~password) in
    let msg =
      Data_encoding.Binary.to_bytes_exn Pvss_secp256k1.Secret_key.encoding sk
    in
    Bytes.cat salt (Crypto_box.Secretbox.secretbox key msg nonce)

  let decrypt algo ~password ~encrypted_sk =
    let open Lwt_result_syntax in
    let salt = Bytes.sub encrypted_sk 0 salt_len in
    let encrypted_sk = Bytes.sub encrypted_sk salt_len encrypted_size in
    let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~salt ~password) in
    match
      (Crypto_box.Secretbox.secretbox_open key encrypted_sk nonce, algo)
    with
    | None, _ -> return_none
    | Some bytes, Encrypted_sk Signature.Ed25519 -> (
        match
          Data_encoding.Binary.of_bytes_opt Ed25519.Secret_key.encoding bytes
        with
        | Some sk ->
            return_some (Decrypted_sk (Ed25519 sk : Signature.Secret_key.t))
        | None ->
            failwith
              "Corrupted wallet, deciphered key is not a valid Ed25519 secret \
               key")
    | Some bytes, Encrypted_sk Signature.Secp256k1 -> (
        match
          Data_encoding.Binary.of_bytes_opt Secp256k1.Secret_key.encoding bytes
        with
        | Some sk ->
            return_some (Decrypted_sk (Secp256k1 sk : Signature.Secret_key.t))
        | None ->
            failwith
              "Corrupted wallet, deciphered key is not a valid Secp256k1 \
               secret key")
    | Some bytes, Encrypted_sk Signature.P256 -> (
        match
          Data_encoding.Binary.of_bytes_opt P256.Secret_key.encoding bytes
        with
        | Some sk ->
            return_some (Decrypted_sk (P256 sk : Signature.Secret_key.t))
        | None ->
            failwith
              "Corrupted wallet, deciphered key is not a valid P256 secret key")
    | Some bytes, Encrypted_aggregate_sk -> (
        match
          Data_encoding.Binary.of_bytes_opt Bls.Secret_key.encoding bytes
        with
        | Some sk ->
            return_some
              (Decrypted_aggregate_sk
                 (Bls12_381 sk : Aggregate_signature.Secret_key.t))
        | None ->
            failwith
              "Corrupted wallet, deciphered key is not a valid BLS12_381 \
               secret key")
end

module Encodings = struct
  let ed25519 =
    let length = Hacl.Ed25519.sk_size + Crypto_box.tag_length + Raw.salt_len in
    Base58.register_encoding
      ~prefix:Base58.Prefix.ed25519_encrypted_seed
      ~length
      ~to_raw:(fun sk -> Bytes.to_string sk)
      ~of_raw:(fun buf ->
        if String.length buf <> length then None else Some (Bytes.of_string buf))
      ~wrap:(fun sk -> Encrypted_ed25519 sk)

  let secp256k1 =
    let open Libsecp256k1.External in
    let length = Key.secret_bytes + Crypto_box.tag_length + Raw.salt_len in
    Base58.register_encoding
      ~prefix:Base58.Prefix.secp256k1_encrypted_secret_key
      ~length
      ~to_raw:(fun sk -> Bytes.to_string sk)
      ~of_raw:(fun buf ->
        if String.length buf <> length then None else Some (Bytes.of_string buf))
      ~wrap:(fun sk -> Encrypted_secp256k1 sk)

  let p256 =
    let length = Hacl.P256.sk_size + Crypto_box.tag_length + Raw.salt_len in
    Base58.register_encoding
      ~prefix:Base58.Prefix.p256_encrypted_secret_key
      ~length
      ~to_raw:(fun sk -> Bytes.to_string sk)
      ~of_raw:(fun buf ->
        if String.length buf <> length then None else Some (Bytes.of_string buf))
      ~wrap:(fun sk -> Encrypted_p256 sk)

  let bls12_381 =
    let length =
      Bls12_381.Signature.sk_size_in_bytes + Crypto_box.tag_length
      + Raw.salt_len
    in
    Base58.register_encoding
      ~prefix:Base58.Prefix.bls12_381_encrypted_secret_key
      ~length
      ~to_raw:(fun sk -> Bytes.to_string sk)
      ~of_raw:(fun buf ->
        if String.length buf <> length then None else Some (Bytes.of_string buf))
      ~wrap:(fun sk -> Encrypted_bls12_381 sk)

  let secp256k1_scalar =
    let length = 36 + Crypto_box.tag_length + Raw.salt_len in
    Base58.register_encoding
      ~prefix:Base58.Prefix.secp256k1_encrypted_scalar
      ~length
      ~to_raw:(fun sk -> Bytes.to_string sk)
      ~of_raw:(fun buf ->
        if String.length buf <> length then None else Some (Bytes.of_string buf))
      ~wrap:(fun sk -> Encrypted_secp256k1_element sk)

  let () =
    Base58.check_encoded_prefix ed25519 "edesk" 88 ;
    Base58.check_encoded_prefix secp256k1 "spesk" 88 ;
    Base58.check_encoded_prefix p256 "p2esk" 88 ;
    Base58.check_encoded_prefix bls12_381 "BLesk" 88 ;
    Base58.check_encoded_prefix secp256k1_scalar "seesk" 93
end

(* we cache the password in this list to avoid
   asking the user all the time *)
let passwords = ref []

(* Loop asking the user to give their password. Fails if a wrong password is
   given more than `retries_left` *)
let interactive_decrypt_loop (cctxt : #Client_context.io) ?name ~retries_left
    ~encrypted_sk algo =
  let open Lwt_result_syntax in
  let rec interactive_decrypt_loop (cctxt : #Client_context.io) name
      ~current_retries ~retries ~encrypted_sk algo =
    match current_retries with
    | n when n >= retries ->
        failwith "%d incorrect password attempts" current_retries
    | _ -> (
        let* password =
          cctxt#prompt_password "Enter password for encrypted key%s: " name
        in
        let* o = Raw.decrypt algo ~password ~encrypted_sk in
        match o with
        | Some sk ->
            passwords := password :: !passwords ;
            return sk
        | None ->
            let*! () =
              if retries_left == 1 then Lwt.return_unit
              else cctxt#message "Sorry, try again."
            in
            interactive_decrypt_loop
              cctxt
              name
              ~current_retries:(current_retries + 1)
              ~retries
              ~encrypted_sk
              algo)
  in
  let name =
    Option.fold name ~some:(fun s -> Format.sprintf " \"%s\"" s) ~none:""
  in
  interactive_decrypt_loop
    cctxt
    name
    ~current_retries:0
    ~retries:retries_left
    ~encrypted_sk
    algo

(* add all passwords obtained by [ctxt#load_passwords] to the list of known passwords *)
let password_file_load ctxt =
  let open Lwt_syntax in
  match ctxt#load_passwords with
  | Some stream ->
      let* () =
        Lwt_stream.iter
          (fun p -> passwords := Bytes.of_string p :: !passwords)
          stream
      in
      return_ok_unit
  | None -> return_ok_unit

let rec noninteractive_decrypt_loop algo ~encrypted_sk =
  let open Lwt_result_syntax in
  function
  | [] -> return_none
  | password :: passwords -> (
      let* o = Raw.decrypt algo ~password ~encrypted_sk in
      match o with
      | None -> noninteractive_decrypt_loop algo ~encrypted_sk passwords
      | Some sk -> return_some sk)

let decrypt_payload cctxt ?name encrypted_sk =
  let open Lwt_result_syntax in
  let* algo, encrypted_sk =
    match Base58.decode encrypted_sk with
    | Some (Encrypted_ed25519 encrypted_sk) ->
        return (Encrypted_sk Signature.Ed25519, encrypted_sk)
    | Some (Encrypted_secp256k1 encrypted_sk) ->
        return (Encrypted_sk Signature.Secp256k1, encrypted_sk)
    | Some (Encrypted_p256 encrypted_sk) ->
        return (Encrypted_sk Signature.P256, encrypted_sk)
    | Some (Encrypted_bls12_381 encrypted_sk) ->
        return (Encrypted_aggregate_sk, encrypted_sk)
    | _ -> failwith "Not a Base58Check-encoded encrypted key"
  in
  let* o = noninteractive_decrypt_loop algo ~encrypted_sk !passwords in
  match o with
  | Some sk -> return sk
  | None ->
      let retries_left = if cctxt#multiple_password_retries then 3 else 1 in
      interactive_decrypt_loop cctxt ?name ~retries_left ~encrypted_sk algo

let internal_decrypt_simple (cctxt : #Client_context.prompter) ?name sk_uri =
  let open Lwt_result_syntax in
  let payload = Uri.path (sk_uri : sk_uri :> Uri.t) in
  let* decrypted_sk = decrypt_payload cctxt ?name payload in
  match decrypted_sk with
  | Decrypted_sk sk -> return sk
  | Decrypted_aggregate_sk _sk ->
      failwith
        "Found an aggregate secret key where a non-aggregate one was expected."

let internal_decrypt_aggregate (cctxt : #Client_context.prompter) ?name
    aggregate_sk_uri =
  let open Lwt_result_syntax in
  let payload = Uri.path (aggregate_sk_uri : aggregate_sk_uri :> Uri.t) in
  let* decrypted_sk = decrypt_payload cctxt ?name payload in
  match decrypted_sk with
  | Decrypted_aggregate_sk sk -> return sk
  | Decrypted_sk _sk ->
      failwith
        "Found a non-aggregate secret key where an aggregate one was expected."

let decrypt (cctxt : #Client_context.prompter) ?name sk_uri =
  let open Lwt_result_syntax in
  let* () = password_file_load cctxt in
  internal_decrypt_simple (cctxt : #Client_context.prompter) ?name sk_uri

let decrypt_aggregate (cctxt : #Client_context.prompter) ?name aggregate_sk_uri
    =
  let open Lwt_result_syntax in
  let* () = password_file_load cctxt in
  internal_decrypt_aggregate
    (cctxt : #Client_context.prompter)
    ?name
    aggregate_sk_uri

let decrypt_all (cctxt : #Client_context.io_wallet) =
  let open Lwt_result_syntax in
  let* sks = Secret_key.load cctxt in
  let* () = password_file_load cctxt in
  List.iter_es
    (fun (name, sk_uri) ->
      if Uri.scheme (sk_uri : sk_uri :> Uri.t) <> Some scheme then return_unit
      else
        let* _ = internal_decrypt_simple cctxt ~name sk_uri in
        return_unit)
    sks

let decrypt_list (cctxt : #Client_context.io_wallet) keys =
  let open Lwt_result_syntax in
  let* sks = Secret_key.load cctxt in
  let* () = password_file_load cctxt in
  List.iter_es
    (fun (name, sk_uri) ->
      if
        Uri.scheme (sk_uri : sk_uri :> Uri.t) = Some scheme
        && (keys = [] || List.mem ~equal:String.equal name keys)
      then
        let* _ = internal_decrypt_simple cctxt ~name sk_uri in
        return_unit
      else return_unit)
    sks

let rec read_password (cctxt : #Client_context.io) =
  let open Lwt_result_syntax in
  let* password =
    cctxt#prompt_password "Enter password to encrypt your key: "
  in
  let* confirm = cctxt#prompt_password "Confirm password: " in
  if not (Bytes.equal password confirm) then
    let*! () = cctxt#message "Passwords do not match." in
    read_password cctxt
  else return password

let common_encrypt sk password =
  let payload = Raw.encrypt ~password sk in
  let encoding =
    match sk with
    | Decrypted_sk (Ed25519 _) -> Encodings.ed25519
    | Decrypted_sk (Secp256k1 _) -> Encodings.secp256k1
    | Decrypted_sk (P256 _) -> Encodings.p256
    | Decrypted_aggregate_sk (Bls12_381 _) -> Encodings.bls12_381
  in
  Base58.simple_encode encoding payload

let internal_encrypt_simple sk password =
  let open Lwt_result_syntax in
  let path = common_encrypt sk password in
  let*? v = Client_keys.make_sk_uri (Uri.make ~scheme ~path ()) in
  return v

let internal_encrypt_aggregate sk password =
  let open Lwt_result_syntax in
  let path = common_encrypt sk password in
  let*? v =
    Client_keys.make_aggregate_sk_uri
      (Uri.make ~scheme:aggregate_scheme ~path ())
  in
  return v

let encrypt sk password = internal_encrypt_simple (Decrypted_sk sk) password

let encrypt_aggregate sk password =
  internal_encrypt_aggregate (Decrypted_aggregate_sk sk) password

let prompt_twice_and_encrypt cctxt sk =
  let open Lwt_result_syntax in
  let* password = read_password cctxt in
  encrypt sk password

let prompt_twice_and_encrypt_aggregate cctxt sk =
  let open Lwt_result_syntax in
  let* password = read_password cctxt in
  encrypt_aggregate sk password

module Sapling_raw = struct
  let salt_len = 8

  (* 193 *)
  let encrypted_size = Crypto_box.tag_length + salt_len + 169

  let nonce = Crypto_box.zero_nonce

  let pbkdf ~salt ~password =
    Pbkdf.SHA512.pbkdf2 ~count:32768 ~dk_len:32l ~salt ~password

  let encrypt ~password msg =
    let msg = Tezos_sapling.Core.Wallet.Spending_key.to_bytes msg in
    let salt = Hacl.Rand.gen salt_len in
    let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~salt ~password) in
    Bytes.(to_string (cat salt (Crypto_box.Secretbox.secretbox key msg nonce)))

  let decrypt ~password payload =
    let ebytes = Bytes.of_string payload in
    let salt = Bytes.sub ebytes 0 salt_len in
    let encrypted_sk = Bytes.sub ebytes salt_len (encrypted_size - salt_len) in
    let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~salt ~password) in
    Option.bind
      (Crypto_box.Secretbox.secretbox_open key encrypted_sk nonce)
      Tezos_sapling.Core.Wallet.Spending_key.of_bytes

  type Base58.data += Data of Tezos_sapling.Core.Wallet.Spending_key.t

  let encrypted_b58_encoding password =
    Base58.register_encoding
      ~prefix:Base58.Prefix.sapling_spending_key
      ~length:encrypted_size
      ~to_raw:(encrypt ~password)
      ~of_raw:(decrypt ~password)
      ~wrap:(fun x -> Data x)
end

let encrypt_sapling_key cctxt sk =
  let open Lwt_result_syntax in
  let* password = read_password cctxt in
  let path =
    Base58.simple_encode (Sapling_raw.encrypted_b58_encoding password) sk
  in
  let*? v = Client_keys.make_sapling_uri (Uri.make ~scheme ~path ()) in
  return v

let decrypt_sapling_key (cctxt : #Client_context.io) (sk_uri : sapling_uri) =
  let open Lwt_result_syntax in
  let uri = (sk_uri :> Uri.t) in
  let payload = Uri.path uri in
  if Uri.scheme uri = Some scheme then
    let* password =
      cctxt#prompt_password "Enter password to decrypt your key: "
    in
    match
      Base58.simple_decode (Sapling_raw.encrypted_b58_encoding password) payload
    with
    | None ->
        failwith
          "Password incorrect or corrupted wallet, could not decipher \
           encrypted Sapling spending key."
    | Some sapling_key -> return sapling_key
  else
    match
      Base58.simple_decode
        Tezos_sapling.Core.Wallet.Spending_key.b58check_encoding
        payload
    with
    | None ->
        failwith
          "Corrupted wallet, could not read unencrypted Sapling spending key."
    | Some sapling_key -> return sapling_key

module Make (C : sig
  val cctxt : Client_context.io_wallet
end) =
struct
  let scheme = "encrypted"

  let title = "Built-in signer using encrypted keys."

  let description =
    "Valid secret key URIs are of the form\n\
    \ - encrypted:<encrypted_key>\n\
     where <encrypted_key> is the encrypted (password protected using Nacl's \
     cryptobox and pbkdf) secret key, formatted in unprefixed Base58.\n\
     Valid public key URIs are of the form\n\
    \ - encrypted:<public_key>\n\
     where <public_key> is the public key in Base58."

  include Client_keys.Signature_type

  let public_key = Unencrypted.public_key

  let public_key_hash = Unencrypted.public_key_hash

  let import_secret_key = Unencrypted.import_secret_key

  let neuterize sk_uri =
    let open Lwt_result_syntax in
    let* sk = decrypt C.cctxt sk_uri in
    let*? v = Unencrypted.make_pk (Signature.Secret_key.to_public_key sk) in
    return v

  let sign ?watermark sk_uri buf =
    let open Lwt_result_syntax in
    let* sk = decrypt C.cctxt sk_uri in
    return (Signature.sign ?watermark sk buf)

  let deterministic_nonce sk_uri buf =
    let open Lwt_result_syntax in
    let* sk = decrypt C.cctxt sk_uri in
    return (Signature.deterministic_nonce sk buf)

  let deterministic_nonce_hash sk_uri buf =
    let open Lwt_result_syntax in
    let* sk = decrypt C.cctxt sk_uri in
    return (Signature.deterministic_nonce_hash sk buf)

  let supports_deterministic_nonces _ = Lwt_result_syntax.return_true
end

module Make_aggregate (C : sig
  val cctxt : Client_context.io_wallet
end) =
struct
  let scheme = "aggregate_encrypted"

  let title = "Built-in signer using encrypted aggregate keys."

  let description =
    "Valid aggregate secret key URIs are of the form\n\
    \ - aggregate_encrypted:<encrypted_aggregate_key>\n\
     where <encrypted_key> is the encrypted (password protected using Nacl's \
     cryptobox and pbkdf) secret key, formatted in unprefixed Base58.\n\
     Valid aggregate public key URIs are of the form\n\
    \ - aggregate_encrypted:<public_aggregate_key>\n\
     where <public_aggregate_key> is the public key in Base58."

  include Client_keys.Aggregate_type

  let public_key = Unencrypted.Aggregate.public_key

  let public_key_hash = Unencrypted.Aggregate.public_key_hash

  let import_secret_key = Unencrypted.Aggregate.import_secret_key

  let neuterize sk_uri =
    let open Lwt_result_syntax in
    let* sk = decrypt_aggregate C.cctxt sk_uri in
    let*? v =
      Unencrypted.Aggregate.make_pk
        (Aggregate_signature.Secret_key.to_public_key sk)
    in
    return v

  let sign sk_uri buf =
    let open Lwt_result_syntax in
    let* sk = decrypt_aggregate C.cctxt sk_uri in
    return (Aggregate_signature.sign sk buf)
end

let encrypt_pvss_key cctxt sk =
  let open Lwt_result_syntax in
  let* password = read_password cctxt in
  let payload = Raw.encrypt_pvss ~password sk in
  let encoding = Encodings.secp256k1_scalar in
  let path = Base58.simple_encode encoding payload in
  let*? v = Client_keys.make_pvss_sk_uri (Uri.make ~scheme ~path ()) in
  return v
