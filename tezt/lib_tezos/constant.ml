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

let tezos_client = "./octez-client"

let tezos_admin_client = "./octez-admin-client"

let tezos_node = "./octez-node"

let tezos_proxy_server = "./octez-proxy-server"

let tezos_codec = "./octez-codec"

let tezos_snoop = "./octez-snoop"

let tezos_protocol_compiler = "./octez-protocol-compiler"

let dal_node = "./octez-dal-node"

let dac_node = "./octez-dac-node"

let smart_rollup_node = "./octez-smart-rollup-node"

(* TODO: tezos/tezos#4803
   Can we do better than to depend on script-inputs?
*)
(* We use the [released-executables] script input as source of released
   executable binaries to test. *)
let released_executables = "./script-inputs/released-executables"

(* We use the [experimental-executables] script input as source of
   experimental executable binaries to test. *)
let experimental_executables = "./script-inputs/experimental-executables"

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

(** Constant gas cost required for every manager operation. Should
    match [Michelson_v1_gas.Cost_of.manager_operation]. *)
let manager_operation_gas_cost ~protocol =
  match protocol with
  | Protocol.Nairobi | Oxford | Alpha -> 100
  | Mumbai -> 1000

(** A valid base58 encoded compressed state hash. *)
let sc_rollup_compressed_state =
  "srs11Z9V76SGd97kGmDQXV8tEF67C48GMy77RuaHdF1kWLk6UTmMfj"

(** A valid base58 encoded layer-2 account to be used to test transaction and
    smart contract rollups. *)
let aggregate_tz4_account : Account.aggregate_key =
  {
    aggregate_alias = "bls_test_account";
    aggregate_public_key_hash = "tz4EECtMxAuJ9UDLaiMZH7G1GCFYUWsj8HZn";
    aggregate_public_key =
      "BLpk1yUiLJ7RezbyViD5ZvWTfQndM3TRRYmvYWkUfH2EJqsLFnzzvpJss6pbuz3U1DDMpk8v16nV";
    aggregate_secret_key =
      Unencrypted "BLsk1hKAHyGqY9qRbgoSVnjiSmDWpKGjFF3WNQ7BaiaMUA6RMA6Pfq";
  }

(** The same as {!aggregate_tz4_account} but for use on layer 1. *)
let tz4_account : Account.key =
  {
    alias = aggregate_tz4_account.aggregate_alias;
    public_key_hash = aggregate_tz4_account.aggregate_public_key_hash;
    public_key = aggregate_tz4_account.aggregate_public_key;
    secret_key = aggregate_tz4_account.aggregate_secret_key;
  }

(** The `echo` kernel that is listed in the “Smart Optimistic Rollups”
    section of the reference manual. *)
let wasm_echo_kernel_boot_sector =
  "0061736d0100000001280760037f7f7f017f60027f7f017f60057f7f7f7f7f017f60017f0060017f017f60027f7f0060000002610311736d6172745f726f6c6c75705f636f72650a726561645f696e707574000011736d6172745f726f6c6c75705f636f72650c77726974655f6f7574707574000111736d6172745f726f6c6c75705f636f72650b73746f72655f77726974650002030504030405060503010001071402036d656d02000a6b65726e656c5f72756e00060aa401042a01027f41fa002f0100210120002f010021022001200247044041e4004112410041e400410010021a0b0b0800200041c4006b0b5001057f41fe002d0000210341fc002f0100210220002d0000210420002f0100210520011004210620042003460440200041016a200141016b10011a0520052002460440200041076a200610011a0b0b0b1d01017f41dc0141840241901c100021004184022000100541840210030b0b38050041e4000b122f6b65726e656c2f656e762f7265626f6f740041f8000b0200010041fa000b0200020041fc000b0200000041fe000b0101"
