(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

let tezos_client = "./tezos-client"

let tezos_admin_client = "./tezos-admin-client"

let tezos_node = "./tezos-node"

let tezos_proxy_server = "./tezos-proxy-server"

let tezos_codec = "./tezos-codec"

let tezos_snoop = "./tezos-snoop"

let sc_rollup_node = "./tezos-sc-rollup-node-alpha"

let sc_rollup_client = "./tezos-sc-rollup-client-alpha"

let activator : Account.key =
  {
    alias = "activator";
    public_key_hash = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
    public_key = "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2";
    secret_key =
      Unencrypted "edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6";
  }

let bootstrap1 : Account.key =
  {
    alias = "bootstrap1";
    public_key_hash = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
    public_key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
    secret_key =
      Unencrypted "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh";
  }

let bootstrap2 : Account.key =
  {
    alias = "bootstrap2";
    public_key_hash = "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN";
    public_key = "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9";
    secret_key =
      Unencrypted "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo";
  }

let bootstrap3 : Account.key =
  {
    alias = "bootstrap3";
    public_key_hash = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU";
    public_key = "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV";
    secret_key =
      Unencrypted "edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ";
  }

let bootstrap4 : Account.key =
  {
    alias = "bootstrap4";
    public_key_hash = "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv";
    public_key = "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU";
    secret_key =
      Unencrypted "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3";
  }

let bootstrap5 : Account.key =
  {
    alias = "bootstrap5";
    public_key_hash = "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv";
    public_key = "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n";
    secret_key =
      Unencrypted "edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm";
  }

let bootstrap_keys =
  [bootstrap1; bootstrap2; bootstrap3; bootstrap4; bootstrap5]

(** Initial number of bootstrap accounts *)
let default_bootstrap_count = List.length bootstrap_keys

let all_secret_keys = activator :: bootstrap_keys

(** The default burn for an implicit account. *)
let implicit_account_burn =
  (* as per the "origination_size" constant *)
  Tez.of_mutez_int 257_000

(** The default time to live of an operation (in block) *)
let max_op_ttl = 120

(** A valid base58 encoded layer-2 address to be used to test
    transaction rollups. *)
let tx_rollup_l2_address = "tru2SS5NXzR5nWQkMAQGeqjmRqFcb4oQH9D2u"
