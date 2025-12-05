(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------

   Requirement:  make -f etherlink.mk build
                 make octez-node octez-client octez-smart-rollup-node octez-evm-node
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file tezosx.ml
 *)

let runtime_tags = List.map Tezosx_runtime.tag

(* /!\ Read this before adding a new test

   By default, to write a test, you should use [register_sandbox_test]. Only
   for tests actually requiring to consider L1/L2 communication (typically,
   related to the bridge, or when we need to distinguish between the latest and
   finalized block) is it useful to use [_register_fullstack_test]. *)

module Setup = struct
  let register_sandbox_test ?uses_client ~title ~tags ~with_runtimes
      ?tez_bootstrap_accounts =
    Test_helpers.register_sandbox
      ~__FILE__
      ?uses_client
      ?tez_bootstrap_accounts
      ~kernel:Latest
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~with_runtimes

  let _register_fullstack_test ~title ~tags ~with_runtimes =
    Setup.register_test
      ~__FILE__
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~kernel:Latest
      ~with_runtimes
      ~enable_dal:false
      ~enable_multichain:false
end

let test_runtime_feature_flag ~runtime () =
  Setup.register_sandbox_test
    ~title:"Set Tezos runtime feature flag"
    ~tags:["feature_flag"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  let* rpc_result =
    Rpc.state_value sandbox (Tezosx_runtime.feature_flag runtime)
  in
  match rpc_result with
  | Ok (Some _) -> unit
  | Ok None ->
      Test.fail
        "Feature flag for the %s runtime was not set"
        (String.capitalize_ascii (Tezosx_runtime.to_string runtime))
  | Error err ->
      Test.fail
        "Could not read feature flag %s: %s"
        (Tezosx_runtime.feature_flag runtime)
        err.message

let tezos_client node =
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.{(Evm_node.rpc_endpoint_record node) with path = "/tezlink"})
  in
  Client.init ~endpoint ()

let check_account sandbox (account : Account.key) =
  let* client = tezos_client sandbox in
  let* _balance =
    Client.get_balance_for ~account:account.public_key_hash client
  in
  unit

let test_bootstrap_kernel_config () =
  let tez_bootstrap_accounts = Evm_node.tez_default_bootstrap_accounts in
  Setup.register_sandbox_test
    ~tez_bootstrap_accounts
    ~title:"Set tezos bootstrap accounts"
    ~uses_client:true
    ~tags:["bootstrap"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  Lwt_list.iter_p (check_account sandbox) tez_bootstrap_accounts

let () =
  test_bootstrap_kernel_config () ;
  test_runtime_feature_flag ~runtime:Tezos ()
