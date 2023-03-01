(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                        *)
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

(* Testing
   -------
   Component:    Smart Optimistic Rollups: EVM Kernel
   Requirement:  make -f kernels.mk build-kernels
   Invocation:   dune exec tezt/tests/main.exe -- --file evm_rollup.ml
*)

open Sc_rollup_helpers

let pvm_kind = "wasm_2_0_0"

type full_evm_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  sc_rollup_client : Sc_rollup_client.t;
  sc_rollup_address : string;
  dac_node : Dac_node.t;
  originator_key : string;
  rollup_operator_key : string;
  evm_proxy_server : Evm_proxy_server.t;
}

let hex_encode (input : string) : string =
  match Hex.of_string input with `Hex s -> s

let evm_proxy_server_version proxy_server =
  let endpoint = Evm_proxy_server.endpoint proxy_server in
  let get_version_url = endpoint ^ "/version" in
  RPC.Curl.get get_version_url

module Account = struct
  type t = {public_key : string; private_key : string}

  (** Prefunded account public key in the kernel, has a balance of 9999. *)
  let prefunded_account_pk = "0x6471A723296395CF1Dcc568941AFFd7A390f94CE"

  let accounts =
    [|
      {
        public_key = "0x6471A723296395CF1Dcc568941AFFd7A390f94CE";
        private_key = "0x9bfc9fbe6296c8fef8eb8d6ce2ed5f772a011898";
      };
      {
        public_key = "0x0b52D4D3bE5D18a7aB5E4476a2F5382bBf2B38d8";
        private_key = "0x672c4a81a943f2bf450869a135bd27fd43d90e9a";
      };
    |]
end

let setup_evm_kernel ?(originator_key = Constant.bootstrap1.public_key_hash)
    ?(rollup_operator_key = Constant.bootstrap1.public_key_hash) protocol =
  let* node, client = setup_l1 protocol in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:rollup_operator_key
  in
  let with_dac_node node client f =
    Dac.with_legacy_dac_node
      ~sc_rollup_node
      node
      client
      f
      ~pvm_name:pvm_kind
      ~threshold:0
      ~dac_members:0
  in
  with_dac_node node client @@ fun dac_node _dac_members ->
  (* Start a rollup node *)
  (* Prepare DAL/DAC: put reveal data in rollup node directory. *)
  let* installer_kernel =
    prepare_installer_kernel ~base_installee:"./" ~dac_node "evm_kernel"
  in
  let boot_sector = hex_encode installer_kernel in
  let* sc_rollup_address =
    originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector
      ~parameters_ty:"pair string (ticket string)"
      ~src:originator_key
      client
  in
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node sc_rollup_address
  in
  let* () = Sc_rollup_node.run sc_rollup_node [] in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
  (* EVM Kernel installation level. *)
  let* () = Client.bake_for_and_wait client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:30.
      sc_rollup_node
      (Node.get_level node)
  in
  let* evm_proxy_server = Evm_proxy_server.init sc_rollup_node in
  return
    {
      node;
      client;
      sc_rollup_node;
      sc_rollup_client;
      sc_rollup_address;
      dac_node;
      originator_key;
      rollup_operator_key;
      evm_proxy_server;
    }

let test_evm_proxy_server_connection =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"]
    ~title:"EVM proxy server connection"
  @@ fun protocol ->
  let* tezos_node, tezos_client = setup_l1 protocol in
  let* sc_rollup =
    originate_sc_rollup
      ~kind:"wasm_2_0_0"
      ~parameters_ty:"string"
      ~src:Constant.bootstrap1.alias
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      Observer
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:Constant.bootstrap1.alias
  in
  let evm_proxy = Evm_proxy_server.create sc_rollup_node in
  (* Tries to start the EVM proxy server without a listening rollup node. *)
  let process = Evm_proxy_server.spawn_run evm_proxy in
  let* () = Process.check ~expect_failure:true process in
  (* Starts the rollup node. *)
  let* _filename = Sc_rollup_node.config_init sc_rollup_node sc_rollup in
  let* _ = Sc_rollup_node.run sc_rollup_node [] in
  (* Starts the EVM proxy server and asks its version. *)
  let* () = Evm_proxy_server.run evm_proxy in
  let*? process = evm_proxy_server_version evm_proxy in
  let* () = Process.check process in
  unit

let test_originate_evm_kernel =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"]
    ~title:"Originate EVM kernel with installer"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; sc_rollup_client; _} =
    setup_evm_kernel protocol
  in
  (* First run of the installed EVM kernel, it will initialize the directory
     "eth_accounts". *)
  let* () = Client.bake_for_and_wait client in
  let first_evm_run_level = Node.get_level node in
  let* level =
    Sc_rollup_node.wait_for_level
      ~timeout:30.
      sc_rollup_node
      first_evm_run_level
  in
  Check.(level = first_evm_run_level)
    Check.int
    ~error_msg:"Current level has moved past first EVM run (%L = %R)" ;
  let eth_accounts_key = "eth_accounts" in
  let*! storage_root_keys =
    Sc_rollup_client.inspect_durable_state_value
      ~hooks
      sc_rollup_client
      ~pvm_kind
      ~operation:Sc_rollup_client.Subkeys
      ~key:""
  in
  Check.(
    list_mem
      string
      eth_accounts_key
      storage_root_keys
      ~error_msg:"Expected %L to be initialized by the EVM kernel.") ;
  unit

let test_rpc_getBalance =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_balance"]
    ~title:"RPC method eth_getBalance"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; evm_proxy_server; _} =
    setup_evm_kernel protocol
  in
  let* () = Client.bake_for_and_wait client in
  let first_evm_run_level = Node.get_level node in
  let* _level =
    Sc_rollup_node.wait_for_level
      ~timeout:30.
      sc_rollup_node
      first_evm_run_level
  in
  let evm_proxy_server_endoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* balance =
    Eth_cli.balance
      ~account:Account.prefunded_account_pk
      ~endpoint:evm_proxy_server_endoint
  in
  Check.((balance = 9999) int)
    ~error_msg:
      (sf
         "Expected balance of %s should be %%R, but got %%L"
         Account.prefunded_account_pk) ;
  unit

let register_evm_proxy_server ~protocols =
  test_originate_evm_kernel protocols ;
  test_evm_proxy_server_connection protocols ;
  test_rpc_getBalance protocols

let register ~protocols = register_evm_proxy_server ~protocols
