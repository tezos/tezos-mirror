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

let bootstrap_accounts =
  [ {
      name = "bootstrap1";
      sk_uri =
        Client_keys.make_sk_uri
        @@ Uri.of_string
             "unencrypted:edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh";
    };
    {
      name = "bootstrap2";
      sk_uri =
        Client_keys.make_sk_uri
        @@ Uri.of_string
             "unencrypted:edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo";
    };
    {
      name = "bootstrap3";
      sk_uri =
        Client_keys.make_sk_uri
        @@ Uri.of_string
             "unencrypted:edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ";
    };
    {
      name = "bootstrap4";
      sk_uri =
        Client_keys.make_sk_uri
        @@ Uri.of_string
             "unencrypted:edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3";
    };
    {
      name = "bootstrap5";
      sk_uri =
        Client_keys.make_sk_uri
        @@ Uri.of_string
             "unencrypted:edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm";
    } ]

let add_bootstrap_secret cctxt {name; sk_uri} =
  let force = false in
  Client_keys.neuterize sk_uri
  >>=? fun pk_uri ->
  Client_keys.Public_key.find_opt cctxt name
  >>=? (function
         | None ->
             return_unit
         | Some (pk_uri_found, _) ->
             fail_unless
               (pk_uri = pk_uri_found || force)
               (failure
                  "public and secret keys '%s' don't correspond, please don't \
                   use --force"
                  name))
  >>=? fun () ->
  Client_keys.import_secret_key ~io:(cctxt :> Client_context.io_wallet) pk_uri
  >>=? fun (pkh, public_key) ->
  cctxt#message "Tezos address added: %a" Signature.Public_key_hash.pp pkh
  >>= fun () ->
  Client_keys.register_key cctxt ~force (pkh, pk_uri, sk_uri) ?public_key name

let populate (cctxt : Tezos_client_base.Client_context.full) =
  Tezos_base.TzPervasives.iter_s
    (add_bootstrap_secret cctxt)
    bootstrap_accounts
