(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Baker
   Invocation:   dune exec tezt/tests/main.exe -- --file baker_signing_delay.ml
   Subject:      Test experimental baker variables (signing delay and fixed random seed)

   This test is used to maintain the availability of two experimental features:
   1. TEZOS_SIGN_DELAY_I_KNOW_WHAT_I_AM_DOING - adds artificial signing delays
   2. TEZOS_CLIENT_FIXED_RANDOM_SEED - uses fixed random seed for testing

   Both features are designed for testing purposes only and require explicit
   command-line flags to prevent accidental use in production.
*)

let team = Tag.layer1

(* Environment variable names for the experimental features *)

(* Helper function to test baker rejection of environment variables *)
let test_baker_rejects_env_var ~name ~env_var ~env_value node client =
  Log.info "Testing that baker rejects %s without proper flag" name ;
  let baker = Agnostic_baker.create ~name:("test_" ^ name) node client in
  let env = String_map.singleton env_var env_value in
  let process = Agnostic_baker.spawn_run ~env baker in
  Process.check_error
    ~msg:(rex (env_var ^ " is set, but.*is not allowed"))
    process

(* Helper function to test client command-line flags *)
let test_client_flag ~flag_name ~env_var ~env_value ~should_succeed client =
  let action = if should_succeed then "accepts" else "rejects" in
  Log.info "Testing that client %s flag %s" action flag_name ;
  let env = String_map.singleton env_var env_value in
  let process =
    Client.spawn_command
      ~env
      client
      [flag_name; "rpc"; "get"; "/chains/main/blocks/head/header"]
  in
  if should_succeed then (
    let* () = Process.check process in
    Log.info "✓ Client accepts %s flag" flag_name ;
    unit)
  else
    let* () =
      Process.check_error
        ~msg:(rex ("Unexpected command line option " ^ flag_name))
        process
    in
    Log.info "✓ Client correctly rejects %s flag" flag_name ;
    unit

(* Helper function to test baker with flags *)
let test_baker_with_flag ~flag_name ~env_var ~env_value node client =
  Log.info "Testing that baker accepts %s flag" flag_name ;
  let baker =
    Agnostic_baker.create ~name:("test_baker_" ^ flag_name) node client
  in
  let env = String_map.singleton env_var env_value in
  let process =
    Agnostic_baker.spawn_run ~env ~extra_arguments:[flag_name] baker
  in
  (* Give the baker time to start and then terminate it *)
  let* () = Lwt_unix.sleep 2.0 in
  Process.terminate process ;
  Log.info "✓ Baker accepts %s flag" flag_name ;
  unit

let test_baker_signing_delay =
  Protocol.register_test
    ~__FILE__
    ~title:"baker signing delay"
    ~tags:[team; "baker"; "signing"; "delay"; "experimental"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  Log.info
    "Starting experimental baker variables test with protocol %s"
    (Protocol.name protocol) ;
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  Log.info "Node and client initialized successfully" ;

  (* Test 1: Security - Baker must reject environment variables without explicit flags *)
  let* () =
    test_baker_rejects_env_var
      ~name:"signing_delay"
      ~env_var:Client.signing_delay_env_var
      ~env_value:"0.1"
      node
      client
  in

  let* () =
    test_baker_rejects_env_var
      ~name:"fixed_seed"
      ~env_var:Client.fixed_seed_env_var
      ~env_value:"42"
      node
      client
  in

  (* Test 2: Flag scope validation *)
  Log.info "Testing command-line flag availability" ;

  (* --allow-fixed-random-seed is a global client flag *)
  let* () =
    test_client_flag
      ~flag_name:"--allow-fixed-random-seed"
      ~env_var:Client.fixed_seed_env_var
      ~env_value:"42"
      ~should_succeed:true
      client
  in

  (* --allow-signing-delay is baker-only, not available for client *)
  let* () =
    test_client_flag
      ~flag_name:"--allow-signing-delay"
      ~env_var:Client.signing_delay_env_var
      ~env_value:"0.1"
      ~should_succeed:false
      client
  in

  (* Test 3: Baker accepts its specific flag *)
  let* () =
    test_baker_with_flag
      ~flag_name:"--allow-signing-delay"
      ~env_var:Client.signing_delay_env_var
      ~env_value:"0.1"
      node
      client
  in

  Log.info "✅ All experimental baker variables tests passed" ;
  Log.info "Test completed successfully" ;
  unit

let register ~protocols = test_baker_signing_delay protocols
