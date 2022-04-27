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

let tx_rollup_node = "./tezos-tx-rollup-node-alpha"

let tx_rollup_client = "./tezos-tx-rollup-client-alpha"

(** Key pair used to activate a protocol from genesis with [--network sandbox].
    The public key is hard-coded in the node. *)
let activator : Account.key =
  {
    alias = "activator";
    public_key_hash = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
    public_key = "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2";
    secret_key =
      Unencrypted "edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6";
  }

(* The names below are simply placeholders to ease the writing of
   tests. The array [Account.Bootstrap.keys] may contain more bootstrap
   keys. *)
let bootstrap1 = Account.Bootstrap.keys.(0)

let bootstrap2 = Account.Bootstrap.keys.(1)

let bootstrap3 = Account.Bootstrap.keys.(2)

let bootstrap4 = Account.Bootstrap.keys.(3)

let bootstrap5 = Account.Bootstrap.keys.(4)

let all_secret_keys = activator :: (Account.Bootstrap.keys |> Array.to_list)

(** The default burn for an implicit account. *)
let implicit_account_burn =
  (* as per the "origination_size" constant *)
  Tez.of_mutez_int 257_000

(** The default time to live of an operation (in block) *)
let max_op_ttl = 120

(** A valid base58 encoded layer-2 address to be used to test
    transaction rollups. *)
let tx_rollup_l2_address = "tz4MSfZsn6kMDczShy8PMeB628TNukn9hi2K"

let tx_rollup_empty_l2_context =
  "CoVu7Pqp1Gh3z33mink5T5Q2kAQKtnn3GHxVhyehdKZpQMBxFBGF"

let tx_rollup_empty_withdraw_list_hash =
  "txw1sFoLju3ySMAdY6v1dcHUMqJ4Zxc1kcynC8xkYgCmH6bpNSDhV"

let tx_rollup_initial_message_result =
  "txmr344vtdPzvWsfnoSd3mJ3MCFA5ehKLQs1pK9WGcX4FEACg1rVgC"

(** A valid rejection proof for the initial layer2 state. *)
let tx_rollup_proof_initial_state =
  {|{ "version": 3,
  "before": { "node": "CoVu7Pqp1Gh3z33mink5T5Q2kAQKtnn3GHxVhyehdKZpQMBxFBGF" },
  "after": { "node": "CoVu7Pqp1Gh3z33mink5T5Q2kAQKtnn3GHxVhyehdKZpQMBxFBGF" } ,
  "state": [] }|}

(** A valid base58 encoded layer-2 account to be used to test transaction and
    smart contract rollups. *)
let tz4_account : Account.aggregate_key =
  {
    aggregate_alias = "bls_test_account";
    aggregate_public_key_hash = "tz4EECtMxAuJ9UDLaiMZH7G1GCFYUWsj8HZn";
    aggregate_public_key =
      "BLpk1yUiLJ7RezbyViD5ZvWTfQndM3TRRYmvYWkUfH2EJqsLFnzzvpJss6pbuz3U1DDMpk8v16nV";
    aggregate_secret_key =
      Unencrypted "BLsk1hKAHyGqY9qRbgoSVnjiSmDWpKGjFF3WNQ7BaiaMUA6RMA6Pfq";
  }
