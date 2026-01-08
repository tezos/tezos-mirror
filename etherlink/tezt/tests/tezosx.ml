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
  let register_sandbox_test ~title ~tags ~with_runtimes =
    Test_helpers.register_sandbox
      ~__FILE__
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

let check_account sandbox (account : Account.key) =
  let open Rpc.Syntax in
  let open Evm_node_lib_dev_encoding in
  let get_res message = function
    | Ok v -> v
    | Error _ -> Test.fail ~__LOC__ "%s" message
  in
  let get_decoded type_ decode bytes =
    get_res
      (Format.asprintf
         "Should have decoded a %s but got %a"
         type_
         Rlp.pp
         (Value bytes))
      (decode bytes)
  in
  (* get RLP value in durable storage *)
  let path =
    sf "/evm/world_state/eth_accounts/tezos/%s/info" account.public_key_hash
  in
  let*@! value = Rpc.state_value sandbox path in
  let state =
    match Rlp.decode (`Hex value |> Hex.to_bytes) with
    | Ok v -> v
    | _ ->
        Test.fail
          ~__LOC__
          "Failed to decode value at path %s in durable storage"
          path
  in
  (* Check RLP value *)
  match state with
  | List [Value balance; Value nonce; Value public_key] ->
      let balance = get_decoded "z" Rlp.decode_z balance in
      let nonce = get_decoded "int" Rlp.decode_int nonce in
      let public_key =
        get_res "Should have decoded the public key"
        @@ Data_encoding.Binary.of_bytes
             Tezos_crypto.Signature.Public_key.encoding
             public_key
      in
      Check.((nonce = 0) int ~error_msg:"Expected nonce %R but got %L") ;
      Check.(
        (Z.to_int64 balance = 3_800_000_000_000L)
          int64
          ~error_msg:"Expected balance %R but got %L") ;
      Check.(
        (Tezos_crypto.Signature.Public_key.to_b58check public_key
        = account.public_key)
          string
          ~error_msg:"Expected public key %R but got %L") ;
      unit
  | _ ->
      Test.fail
        ~__LOC__
        "Unexpected value decoded at path %s in durable storage: value = %s, \
         original data = %s"
        path
        (Format.asprintf "%a" Rlp.pp state)
        value

let test_bootstrap_kernel_config () =
  Setup.register_sandbox_test
    ~title:"Set tezos bootstrap accounts"
    ~tags:["bootstrap"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let _ =
    List.map (check_account sandbox) Evm_node.tez_default_bootstrap_accounts
  in
  unit

let () =
  test_bootstrap_kernel_config () ;
  test_runtime_feature_flag ~runtime:Tezos ()
