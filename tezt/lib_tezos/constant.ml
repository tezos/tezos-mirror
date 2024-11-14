(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
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

(* Some constants such as [octez_client] are automatically added to [~uses],
   so they have to be defined in the wrapper library.
   [Constant] merely provides an alias. *)
let octez_client = Uses.octez_client

let octez_admin_client = Uses.octez_admin_client

let octez_node = Uses.octez_node

let octez_codec = Uses.make ~tag:"codec" ~path:"./octez-codec" ()

let octez_snoop = Uses.make ~tag:"snoop" ~path:"./octez-snoop" ()

let octez_protocol_compiler =
  Uses.make ~tag:"protocol_compiler" ~path:"./octez-protocol-compiler" ()

let octez_dal_node = Uses.make ~tag:"dal_node" ~path:"./octez-dal-node" ()

let octez_dac_node = Uses.make ~tag:"dac_node" ~path:"./octez-dac-node" ()

let octez_dac_client = Uses.make ~tag:"dac_client" ~path:"./octez-dac-client" ()

let octez_smart_rollup_node =
  Uses.make ~tag:"smart_rollup_node" ~path:"./octez-smart-rollup-node" ()

let octez_evm_node = Uses.make ~tag:"evm_node" ~path:"./octez-evm-node" ()

let octez_dsn_node = Uses.make ~tag:"dsn_node" ~path:"./octez-dsn-node" ()

let octez_signer = Uses.make ~tag:"signer" ~path:"./octez-signer" ()

let octez_injector_server =
  Uses.make
    ~tag:"injector_server"
    ~path:
      "./_build/default/contrib/octez_injector_server/octez_injector_server.exe"
    ()

let smart_rollup_installer =
  Uses.make ~tag:"smart_rollup_installer" ~path:"smart-rollup-installer" ()

(* The following is unused because even though the WASM debugger is released,
   there are no tests for it yet, except [tezt/tests/binaries.ml].
   However, this test requires the executables it tests to be declared with
   [Uses.make] so that they are registered in the lookup table of [Uses]. *)
let _octez_smart_rollup_wasm_debugger =
  Uses.make ~tag:"wasm_debugger" ~path:"./octez-smart-rollup-wasm-debugger" ()

let teztale_archiver =
  Uses.make ~tag:"teztale_archiver" ~path:"./octez-teztale-archiver" ()

let teztale_server =
  Uses.make ~tag:"teztale_server" ~path:"./octez-teztale-server" ()

module WASM = struct
  let dal_echo_kernel =
    Uses.make ~tag:"dal_echo_kernel" ~path:"dal_echo_kernel.wasm" ()

  let debug_kernel =
    Uses.make
      ~tag:"debug_kernel"
      ~path:"etherlink/kernel_evm/kernel/tests/resources/debug_kernel.wasm"
      ()

  (* Note: this should probably depend on the protocol,
     and thus be in the [Protocol] module? *)
  let echo =
    Uses.make
      ~tag:"echo"
      ~path:
        "src/proto_alpha/lib_protocol/test/integration/wasm_kernel/echo.wasm"
      ()

  let evm_kernel =
    Uses.make
      ~how_to_build:"make -f etherlink.mk build"
      ~tag:"evm_kernel"
      ~path:"evm_kernel.wasm"
      ()

  let failed_migration =
    Uses.make
      ~tag:"failed_migration"
      ~path:"etherlink/kernel_evm/kernel/tests/resources/failed_migration.wasm"
      ()

  let mainnet_evm_kernel =
    Uses.make
      ~tag:"mainnet_evm_kernel"
      ~path:
        "etherlink/kernel_evm/kernel/tests/resources/mainnet_evm_kernel.wasm"
      ()

  let mainnet_evm_commit = "4f4457e2527cb227a90bb1c56d3a83f39c0f78fd"

  let ghostnet_evm_kernel =
    Uses.make
      ~tag:"ghostnet_evm_kernel"
      ~path:
        "etherlink/kernel_evm/kernel/tests/resources/ghostnet_evm_kernel.wasm"
      ()

  let ghostnet_evm_commit = "af7909023768bc4aad3120bec7bea4a64a576047"

  let tx_kernel = Uses.make ~tag:"tx_kernel" ~path:"tx_kernel.wasm" ()

  let tx_kernel_dal =
    Uses.make ~tag:"tx_kernel_dal" ~path:"tx_kernel_dal.wasm" ()
end

(* TODO: tezos/tezos#4803
   Can we do better than to depend on script-inputs?
*)
(* We use the [released-executables] script input as source of released
   executable binaries to test. *)
let released_executables = "./script-inputs/released-executables"

(* We use the [experimental-executables] script input as source of
   experimental executable binaries to test. *)
let experimental_executables = "./script-inputs/experimental-executables"

(** Default hostname to use for endpoints when no specific one is required. *)
let default_host =
  (* The value of [default_host] is set to ["127.0.0.1"] because the
     alternatives have the following drawbacks :
     - Using ["localhost"] leads to an extra consumption of RAM
       (https://gitlab.com/tezos/tezos/-/issues/6789).
     - There are or were some problems with IPv6 on GCP.
  *)
  "127.0.0.1"

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
let manager_operation_gas_cost ~protocol:_ = 100

(** A valid base58 encoded compressed state hash. *)
let sc_rollup_compressed_state =
  "srs11Z9V76SGd97kGmDQXV8tEF67C48GMy77RuaHdF1kWLk6UTmMfj"

(** A valid base58 encoded layer-2 account to be used to test transaction and
    smart contract rollups. *)
let tz4_account : Account.key =
  {
    alias = "bls_test_account";
    public_key_hash = "tz4EECtMxAuJ9UDLaiMZH7G1GCFYUWsj8HZn";
    public_key =
      "BLpk1yUiLJ7RezbyViD5ZvWTfQndM3TRRYmvYWkUfH2EJqsLFnzzvpJss6pbuz3U1DDMpk8v16nV";
    secret_key =
      Unencrypted "BLsk1hKAHyGqY9qRbgoSVnjiSmDWpKGjFF3WNQ7BaiaMUA6RMA6Pfq";
  }

(** The `echo` kernel that is listed in the “Smart Optimistic Rollups”
    section of the reference manual. *)
let wasm_echo_kernel_boot_sector =
  "0061736d0100000001280760037f7f7f017f60027f7f017f60057f7f7f7f7f017f60017f0060017f017f60027f7f0060000002610311736d6172745f726f6c6c75705f636f72650a726561645f696e707574000011736d6172745f726f6c6c75705f636f72650c77726974655f6f7574707574000111736d6172745f726f6c6c75705f636f72650b73746f72655f77726974650002030504030405060503010001071402036d656d02000a6b65726e656c5f72756e00060aa401042a01027f41fa002f0100210120002f010021022001200247044041e4004112410041e400410010021a0b0b0800200041c4006b0b5001057f41fe002d0000210341fc002f0100210220002d0000210420002f0100210520011004210620042003460440200041016a200141016b10011a0520052002460440200041076a200610011a0b0b0b1d01017f41dc0141840241901c100021004184022000100541840210030b0b38050041e4000b122f6b65726e656c2f656e762f7265626f6f740041f8000b0200010041fa000b0200020041fc000b0200000041fe000b0101"
