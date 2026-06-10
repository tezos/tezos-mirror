(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Native contract
   Invocation:   dune exec tezt/tests/main.exe -- --file native_contracts.ml
   Subject:      Test native contracts integration
*)

let team = Tag.layer1

let register_test = Protocol.register_test ~supports:(Protocol.From_protocol 25)

let find_native_contract_hash client contract =
  Lwt.catch
    (fun () ->
      let* hash =
        Client.RPC.call client
        @@ RPC.get_chain_block_context_raw
             ~value_path:["contracts"; "native_contracts"; contract]
             ()
      in
      return (Some hash))
    (fun _ -> return None)

let test_feature_flag =
  register_test
    ~__FILE__
    ~title:"Test feature flag works"
    ~tags:[team; "feature_flag"]
    ~uses:(fun _ -> [Constant.octez_node])
  @@ fun protocol ->
  let* parameter_file_enabled =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      [(["native_contracts_enable"], `Bool true)]
  in
  let* _node, client =
    Client.init_with_protocol
      ~parameter_file:parameter_file_enabled
      `Client
      ~protocol
      ()
  in
  let* contract = find_native_contract_hash client "stez" in
  if contract = None then
    Test.fail "Contract is not in the storage, the feature has not been enabled" ;
  let* parameter_file_disabled =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      [(["native_contracts_enable"], `Bool false)]
  in
  let* _node, client =
    Client.init_with_protocol
      ~parameter_file:parameter_file_disabled
      `Client
      ~protocol
      ()
  in
  let* contract = find_native_contract_hash client "accumulator" in
  if contract <> None then
    Test.fail "Contract is in the storage while the feature is disabled" ;
  unit

let test_feature_disabled_on_mainnet =
  register_test
    ~__FILE__
    ~title:"Test feature flag is disabled on mainnet"
    ~tags:[team; "feature_flag"; "mainnet"]
    ~uses:(fun _ -> [Constant.octez_node])
  @@ fun protocol ->
  let* parameter_file_mainnet =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      []
  in
  let* _node, client =
    Client.init_with_protocol
      ~parameter_file:parameter_file_mainnet
      `Client
      ~protocol
      ()
  in
  let* contract = find_native_contract_hash client "accumulator" in
  if contract <> None then
    Test.fail "Contract is in the storage while the feature is disabled" ;
  unit

(* Initialize a client with the native-contracts feature enabled and return the
   client together with the CLST ("stez") native contract hash. *)
let init_with_native_contract protocol =
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      [(["native_contracts_enable"], `Bool true)]
  in
  let* _node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let* contract = find_native_contract_hash client "stez" in
  match contract with
  | None ->
      Test.fail "CLST (stez) native contract is not present in the storage"
  | Some hash -> return (client, JSON.as_string hash)

let test_script_exposes_interface =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test script RPC exposes native contract interface"
    ~tags:[team; "script"; "rpc"]
    ~supports:(Protocol.From_protocol 25)
    ~uses:(fun _ -> [Constant.octez_node])
  @@ fun protocol ->
  let* client, clst = init_with_native_contract protocol in
  (* The script RPC must now return a non-empty script for the native contract.
     Capture the response in the regression output. *)
  let* script =
    Client.RPC.call ~hooks:Tezos_regression.hooks client
    @@ RPC.get_chain_block_context_contract_script ~id:clst ()
  in
  if JSON.is_null script then
    Test.fail "script RPC returned an empty response for the native contract" ;
  (* The toplevel code must contain the parameter, storage and code sections,
     plus at least one view (CLST declares several). *)
  let toplevel_prims =
    JSON.(script |-> "code" |> as_list)
    |> List.filter_map (fun node ->
           let prim = JSON.(node |-> "prim") in
           if JSON.is_null prim then None else Some (JSON.as_string prim))
  in
  List.iter
    (fun prim ->
      if not (List.mem prim toplevel_prims) then
        Test.fail "synthesized script is missing the %s section" prim)
    ["parameter"; "storage"; "code"] ;
  if not (List.mem "view" toplevel_prims) then
    Test.fail "synthesized script does not expose any view" ;
  (* Substring search over the encoded script, used for annotations and the
     stub marker (both are JSON string leaves whose exact framing is stable). *)
  let script_str = JSON.encode script in
  let contains substring =
    let n = String.length script_str and m = String.length substring in
    let rec aux i =
      if i + m > n then false
      else if String.sub script_str i m = substring then true
      else aux (i + 1)
    in
    aux 0
  in
  (* The implementation body must be the recognizable placeholder stub. *)
  if not (contains "native contract: no Michelson code") then
    Test.fail "synthesized script does not contain the expected stub marker" ;
  (* Every named entrypoint reported by the entrypoints RPC (which supports
     native contracts) must appear as a field annotation in the synthesized
     parameter type. *)
  let* entrypoints =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_entrypoints ~id:clst ()
  in
  let entrypoint_names =
    JSON.(entrypoints |-> "entrypoints" |> as_object) |> List.map fst
  in
  if entrypoint_names = [] then
    Test.fail "entrypoints RPC returned no entrypoints for the native contract" ;
  List.iter
    (fun name ->
      (* The root/default entrypoint is implicit and carries no annotation. *)
      if name <> "default" && not (contains (Format.sprintf {|"%%%s"|} name))
      then
        Test.fail
          "entrypoint %s is not exposed in the synthesized parameter type"
          name)
    entrypoint_names ;
  unit

let test_script_normalized_typechecks =
  register_test
    ~__FILE__
    ~title:"Test normalized script RPC type-checks for native contract"
    ~tags:[team; "script"; "rpc"; "normalized"]
    ~uses:(fun _ -> [Constant.octez_node])
  @@ fun protocol ->
  let* client, clst = init_with_native_contract protocol in
  (* The normalized endpoint re-parses and re-unparses the script; succeeding
     proves the synthesized script type-checks. *)
  let data : RPC_core.data =
    Data
      (Ezjsonm.value_from_string
         {|{"unparsing_mode": "Readable", "normalize_types": false}|})
  in
  let* normalized =
    Client.RPC.call client
    @@ RPC.post_chain_block_context_contract_script_normalized ~id:clst ~data ()
  in
  if JSON.is_null normalized then
    Test.fail "normalized script RPC returned an empty response" ;
  unit

let register ~protocols =
  test_feature_flag protocols ;
  test_feature_disabled_on_mainnet protocols ;
  test_script_exposes_interface protocols ;
  test_script_normalized_typechecks protocols
