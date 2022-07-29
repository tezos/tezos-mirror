(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_client_base

type bootstrap_secret = {name : string; sk_uri : Client_keys.sk_uri}

let default_bootstrap_accounts =
  let open Lwt_result_syntax in
  let unencrypted_keys =
    [
      "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh";
      "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo";
      "edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ";
      "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3";
      "edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm";
    ]
  in
  let basename = "bootstrap" in
  List.mapi_es
    (fun i ukey ->
      let*? sk_uri =
        Client_keys.make_sk_uri @@ Uri.of_string ("unencrypted:" ^ ukey)
      in
      let name = basename ^ string_of_int (i + 1) in
      return {name; sk_uri})
    unencrypted_keys

let add_bootstrap_secret cctxt {name; sk_uri} =
  let open Lwt_result_syntax in
  let force = false in
  let* pk_uri = Client_keys.neuterize sk_uri in
  let* () =
    let* o = Client_keys.Public_key.find_opt cctxt name in
    match o with
    | None -> return_unit
    | Some (pk_uri_found, _) ->
        fail_unless
          (pk_uri = pk_uri_found || force)
          (error_of_fmt
             "public and secret keys '%s' don't correspond, please don't use \
              --force"
             name)
  in
  let* pkh, public_key =
    Client_keys.import_secret_key ~io:(cctxt :> Client_context.io_wallet) pk_uri
  in
  let*! () =
    cctxt#message "Tezos address added: %a" Signature.Public_key_hash.pp pkh
  in
  Client_keys.register_key cctxt ~force (pkh, pk_uri, sk_uri) ?public_key name

let bootstrap_secret_encoding =
  let open Data_encoding in
  conv
    (fun p -> (p.name, p.sk_uri, ""))
    (fun (name, sk_uri, _) -> {name; sk_uri})
    (obj3
       (req "name" string)
       (req "sk_uri" Client_keys.Secret_key.encoding)
       (req "amount" string))

let bootstrap_secrets_encoding = Data_encoding.list bootstrap_secret_encoding

let populate (cctxt : #Tezos_client_base.Client_context.io_wallet)
    bootstrap_accounts_file =
  let open Lwt_result_syntax in
  let* accounts =
    match bootstrap_accounts_file with
    | None -> default_bootstrap_accounts
    | Some accounts_file -> (
        let* json =
          Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file accounts_file
        in
        match Data_encoding.Json.destruct bootstrap_secrets_encoding json with
        | accounts -> return accounts
        | exception e ->
            failwith
              "cannot read definitions of bootstrap accounts in %s because: %s"
              accounts_file
              (Printexc.to_string e))
  in
  List.iter_es (add_bootstrap_secret cctxt) accounts
