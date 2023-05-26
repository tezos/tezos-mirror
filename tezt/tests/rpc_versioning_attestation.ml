(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    RPCs
   Invocation:   dune exec tezt/tests/main.exe -- --file
                 rpc_versioning_attestation.ml
   Subject:      rpc versioning
*)

let register_test ~title ?(additionnal_tags = []) f =
  Protocol.register_test
    ~__FILE__
    ~title
    ~supports:(Protocol.From_protocol 18)
    ~tags:(["rpc"; "versioning"] @ additionnal_tags)
    f

let get_consensus_info delegate client =
  let* level = Client.level client in
  let* slots =
    RPC.Client.call client
    @@ RPC.get_chain_block_helper_validators ~delegate ~level ()
  in
  let slots =
    List.map
      JSON.as_int
      JSON.(List.hd JSON.(slots |> as_list) |-> "slots" |> as_list)
  in
  let* block_header = RPC.Client.call client @@ RPC.get_chain_block_header () in
  let block_payload_hash =
    JSON.(block_header |-> "payload_hash" |> as_string)
  in
  return (level, slots, block_payload_hash)

let mk_consensus ?(slot = 1) ?(level = 1) ?(round = 0)
    ?(block_payload_hash =
      "vh1g87ZG6scSYxKhspAUzprQVuLAyoa5qMBKcUfjgnQGnFb3dJcG") kind
    use_legacy_name =
  Operation.Consensus.consensus
    ~kind
    ~use_legacy_name
    ~slot
    ~level
    ~round
    ~block_payload_hash

let check_kind json kind =
  let json_kind =
    JSON.(json |-> "contents" |> as_list |> List.hd |-> "kind" |> as_string)
  in
  if not (String.equal json_kind kind) then
    Test.fail ~__LOC__ "Operation should have %s kind, got: %s" kind json_kind

let use_legacy_name_from_version = function `Old -> true | `New -> false

let check_version ~version ~check ~rpc ~name ~data client =
  let* t =
    RPC.Client.call client
    @@ rpc ~version:(match version with `Old -> "0" | `New -> "1") data
  in
  return (check t name)

let check_unknown_version ~rpc ~data client =
  let version = "2" in
  let*? p = RPC.Client.spawn client @@ rpc ~version data in
  let msg = rex "Failed to parse argument 'version'" in
  Process.check_error ~msg p

let create_consensus_op ?slot ?level ?round ?block_payload_hash ~use_legacy_name
    ~signer ~kind client =
  let consensus_name =
    Operation.Consensus.kind_to_string kind use_legacy_name
  in
  Log.info "Create an %s operation" consensus_name ;
  let consensus =
    mk_consensus ?slot ?level ?round ?block_payload_hash kind use_legacy_name
  in
  let* consensus_op = Operation.Consensus.operation ~signer consensus client in

  Log.info "Ensures that the generated JSON contains the %s kind" consensus_name ;
  let consensus_json =
    JSON.annotate ~origin:__LOC__ @@ Operation.json consensus_op
  in
  check_kind consensus_json consensus_name ;
  Lwt.return consensus_op

let mk_double_consensus_evidence kind use_legacy_name op1 op2 client =
  let* op1_sign = Operation.sign op1 client in
  let* op2_sign = Operation.sign op2 client in
  return
    (Operation.Anonymous.double_consensus_evidence
       ~kind
       ~use_legacy_name
       (op1, op1_sign)
       (op2, op2_sign))

let create_double_consensus_evidence ~use_legacy_name ~double_evidence_kind
    client =
  let consensus_kind =
    match double_evidence_kind with
    | Operation.Anonymous.Double_attestation_evidence -> Operation.Attestation
    | Operation.Anonymous.Double_preattestation_evidence ->
        Operation.Preattestation
  in
  let consensus_name =
    Operation.Anonymous.kind_to_string double_evidence_kind use_legacy_name
  in
  Log.info "Create an %s operation" consensus_name ;

  let* level, slots, block_payload_hash =
    get_consensus_info Constant.bootstrap1.public_key_hash client
  in
  let signer = Constant.bootstrap1 in
  let consensus1 =
    mk_consensus
      ~level
      ~block_payload_hash
      ~slot:(List.nth slots 1)
      consensus_kind
      use_legacy_name
  in
  let* op1 = Operation.Consensus.operation ~signer consensus1 client in
  let* (`OpHash oph1) = Operation.hash op1 client in
  let consensus2 =
    mk_consensus
      ~level
      ~block_payload_hash
      ~slot:(List.nth slots 2)
      consensus_kind
      use_legacy_name
  in
  let* op2 = Operation.Consensus.operation ~signer consensus2 client in
  let* (`OpHash oph2) = Operation.hash op2 client in
  let op1, op2 =
    if String.compare oph1 oph2 < 1 then (op1, op2) else (op2, op1)
  in
  let* double_consensus_evidence =
    mk_double_consensus_evidence
      double_evidence_kind
      use_legacy_name
      op1
      op2
      client
  in
  let* double_consensus_evidence_op =
    Operation.Anonymous.operation double_consensus_evidence client
  in

  Log.info "Ensures that the generated JSON contains the %s kind" consensus_name ;
  let consensus_json =
    JSON.annotate ~origin:__LOC__ @@ Operation.json double_consensus_evidence_op
  in
  check_kind consensus_json consensus_name ;
  Lwt.return double_consensus_evidence_op

module Forge = struct
  let check_hex_from_ops op1 op2 client =
    Log.info
      "Ensures that Bytes returned from calling the forge RPC on both \
       operations are identical" ;
    let* (`Hex op1_raw) = Operation.hex op1 client in
    let* (`Hex op2_raw) = Operation.hex op2 client in
    if not (String.equal op1_raw op2_raw) then
      Test.fail ~__LOC__ "Bytes are not equal, got: %s and: %s" op1_raw op2_raw
    else unit

  let test_consensus kind protocol =
    let* _node, client = Client.init_with_protocol ~protocol `Client () in
    let signer = Constant.bootstrap1 in

    let* legacy_consensus_op =
      create_consensus_op ~use_legacy_name:true ~signer ~kind client
    in
    let* consensus_op =
      create_consensus_op ~use_legacy_name:false ~signer ~kind client
    in
    check_hex_from_ops legacy_consensus_op consensus_op client

  let test_forge_consensus =
    register_test
      ~title:"Forge consensus operations"
      ~additionnal_tags:["forge"; "operations"; "consensus"]
    @@ fun protocol -> test_consensus Operation.Attestation protocol

  let test_forge_preconsensus =
    register_test
      ~title:"Forge pre-consensus operations"
      ~additionnal_tags:["forge"; "operations"; "consensus"; "pre"]
    @@ fun protocol -> test_consensus Operation.Preattestation protocol

  let test_double_consensus_evidence double_evidence_kind protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let* () = Client.bake_for_and_wait ~node client in

    let* legacy_double_consensus_evidence_op =
      create_double_consensus_evidence
        ~use_legacy_name:true
        ~double_evidence_kind
        client
    in
    let* double_consensus_evidence_op =
      create_double_consensus_evidence
        ~use_legacy_name:false
        ~double_evidence_kind
        client
    in
    check_hex_from_ops
      legacy_double_consensus_evidence_op
      double_consensus_evidence_op
      client

  let test_forge_double_consensus_evidence =
    register_test
      ~title:"Forge double consensus evidence operations"
      ~additionnal_tags:
        ["forge"; "operations"; "consensus"; "double"; "evidence"]
    @@ fun protocol ->
    test_double_consensus_evidence
      Operation.Anonymous.Double_attestation_evidence
      protocol

  let test_forge_double_preconsensus_evidence =
    register_test
      ~title:"Forge double pre-consensus evidence operations"
      ~additionnal_tags:
        ["forge"; "operations"; "consensus"; "pre"; "double"; "evidence"]
    @@ fun protocol ->
    test_double_consensus_evidence
      Operation.Anonymous.Double_preattestation_evidence
      protocol

  let test_invalid_double_consensus_evidence double_evidence_kind protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let* () = Client.bake_for_and_wait ~node client in

    let create_double_consensus_evidence ~use_legacy_name =
      let consensus_kind =
        match double_evidence_kind with
        | Operation.Anonymous.Double_attestation_evidence ->
            Operation.Attestation
        | Operation.Anonymous.Double_preattestation_evidence ->
            Operation.Preattestation
      in
      let consensus_name =
        Operation.Anonymous.kind_to_string double_evidence_kind use_legacy_name
      in
      Log.info "Create an %s operation" consensus_name ;

      let signer = Constant.bootstrap1 in
      let consensus1 = mk_consensus consensus_kind use_legacy_name in
      let* op1 = Operation.Consensus.operation ~signer consensus1 client in
      let consensus2 =
        mk_consensus ~slot:2 consensus_kind (not use_legacy_name)
      in
      let* op2 = Operation.Consensus.operation ~signer consensus2 client in
      let* double_consensus_evidence =
        mk_double_consensus_evidence
          double_evidence_kind
          use_legacy_name
          op1
          op2
          client
      in
      let* double_consensus_evidence_op =
        Operation.Anonymous.operation double_consensus_evidence client
      in

      Log.info
        "Ensures that the generated JSON contains the %s kind"
        consensus_name ;
      let consensus_json = Operation.json double_consensus_evidence_op in
      let annotated_json = JSON.annotate ~origin:__LOC__ @@ consensus_json in
      check_kind annotated_json consensus_name ;

      Log.info
        "Ensures that the generated JSON cannot be parsed by the forge RPC" ;
      let*? t =
        RPC.Client.spawn client
        @@ RPC.post_chain_block_helpers_forge_operations
             ~data:(Data consensus_json)
             ()
      in
      let msg = rex "Failed to parse the request body: No case matched:" in
      Process.check_error ~msg t
    in

    let* () = create_double_consensus_evidence ~use_legacy_name:true in
    create_double_consensus_evidence ~use_legacy_name:false

  let test_forge_invalid_double_consensus_evidence =
    register_test
      ~title:"Forge invalid double consensus evidence operations"
      ~additionnal_tags:
        ["forge"; "operations"; "consensus"; "double"; "evidence"; "invalid"]
    @@ fun protocol ->
    test_invalid_double_consensus_evidence
      Operation.Anonymous.Double_attestation_evidence
      protocol

  let test_forge_invalid_double_preconsensus_evidence =
    register_test
      ~title:"Forge invalid double pre-consensus evidence operations"
      ~additionnal_tags:
        [
          "forge";
          "operations";
          "consensus";
          "pre";
          "double";
          "evidence";
          "invalid";
        ]
    @@ fun protocol ->
    test_invalid_double_consensus_evidence
      Operation.Anonymous.Double_preattestation_evidence
      protocol

  let register ~protocols =
    test_forge_consensus protocols ;
    test_forge_preconsensus protocols ;
    test_forge_double_consensus_evidence protocols ;
    test_forge_double_preconsensus_evidence protocols ;
    test_forge_invalid_double_consensus_evidence protocols ;
    test_forge_invalid_double_preconsensus_evidence protocols
end

module Parse = struct
  let rpc ~version raw =
    RPC.post_chain_block_helpers_parse_operations
      ~version
      (`A [JSON.unannotate raw])

  let check_parsed_kind json kind =
    check_kind JSON.(json |> as_list |> List.hd) kind

  let create_raw_op ~protocol op client =
    Log.info "Sign and forge the operation" ;
    let* signature = Operation.sign op client in
    let* (`Hex h1) = Operation.hex ~signature op client in
    Log.info
      "Use octez-codec to retrieve the raw representation of the operation \
       from its hexadecimal representation" ;
    Codec.decode
      ~name:(Protocol.encoding_prefix protocol ^ "." ^ "operation.raw")
      h1

  let test_parse kind protocol =
    let* _node, client = Client.init_with_protocol ~protocol `Client () in
    let signer = Constant.bootstrap1 in

    let* consensus_op =
      create_consensus_op ~use_legacy_name:false ~signer ~kind client
    in
    let* raw = create_raw_op ~protocol consensus_op client in

    let check_version ~version =
      let name =
        Operation.Consensus.kind_to_string
          kind
          (use_legacy_name_from_version version)
      in
      check_version
        ~version
        ~check:check_parsed_kind
        ~rpc
        ~name
        ~data:raw
        client
    in

    let* () = check_version ~version:`Old in
    let* () = check_version ~version:`New in
    check_unknown_version ~rpc ~data:raw client

  let test_parse_consensus =
    register_test
      ~title:"Parse raw consensus operations"
      ~additionnal_tags:["parse"; "raw"; "operations"; "consensus"]
    @@ fun protocol -> test_parse Operation.Attestation protocol

  let test_parse_preconsensus =
    register_test
      ~title:"Parse raw pre-consensus operations"
      ~additionnal_tags:["parse"; "raw"; "operations"; "consensus"; "pre"]
    @@ fun protocol -> test_parse Operation.Preattestation protocol

  let test_parse_double_evidence double_evidence_kind protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let* () = Client.bake_for_and_wait ~node client in

    let create_raw_double_consensus_evidence () =
      let* double_consensus_evidence_op =
        create_double_consensus_evidence
          ~use_legacy_name:false
          ~double_evidence_kind
          client
      in
      create_raw_op ~protocol double_consensus_evidence_op client
    in
    let* raw = create_raw_double_consensus_evidence () in

    let check_version ~version =
      let name =
        Operation.Anonymous.kind_to_string
          double_evidence_kind
          (use_legacy_name_from_version version)
      in
      check_version
        ~version
        ~check:check_parsed_kind
        ~rpc
        ~name
        ~data:raw
        client
    in

    let* () = check_version ~version:`Old in
    let* () = check_version ~version:`New in
    check_unknown_version ~rpc ~data:raw client

  let test_parse_double_consensus_evidence =
    register_test
      ~title:"Parse raw double consensus evidence operations"
      ~additionnal_tags:
        ["parse"; "raw"; "operations"; "double"; "consensus"; "evidence"]
    @@ fun protocol ->
    test_parse_double_evidence
      Operation.Anonymous.Double_attestation_evidence
      protocol

  let test_parse_double_preconsensus_evidence =
    register_test
      ~title:"Parse raw double pre-consensus evidence operations"
      ~additionnal_tags:
        ["parse"; "raw"; "operations"; "double"; "consensus"; "pre"; "evidence"]
    @@ fun protocol ->
    test_parse_double_evidence
      Operation.Anonymous.Double_preattestation_evidence
      protocol

  let register ~protocols =
    test_parse_consensus protocols ;
    test_parse_preconsensus protocols ;
    test_parse_double_consensus_evidence protocols ;
    test_parse_double_preconsensus_evidence protocols
end

module Mempool = struct
  let test_pending_operations_consensus kind protocol =
    let* _node, client = Client.init_with_protocol ~protocol `Client () in
    let signer = Constant.bootstrap1 in

    let* consensus_op =
      create_consensus_op ~use_legacy_name:true ~signer ~kind client
    in
    let* (`OpHash _) =
      Operation.inject ~force:true ~request:`Inject consensus_op client
    in

    let check_mempool ~use_legacy_name =
      let* mempool_json =
        RPC.Client.call client
        @@ RPC.get_chain_mempool_pending_operations
             ~version:(if use_legacy_name then "1" else "2")
             ()
      in
      return
        (check_kind
           JSON.(mempool_json |-> "refused" |> as_list |> List.hd)
           (Operation.Consensus.kind_to_string kind use_legacy_name))
    in
    let* () = check_mempool ~use_legacy_name:true in
    check_mempool ~use_legacy_name:false

  let test_pending_consensus =
    register_test
      ~title:"Pending consensus operations"
      ~additionnal_tags:["mempool"; "pending"; "operations"; "consensus"]
    @@ fun protocol ->
    test_pending_operations_consensus Operation.Attestation protocol

  let test_pending_preconsensus =
    register_test
      ~title:"Pending pre-consensus operations"
      ~additionnal_tags:["mempool"; "pending"; "operations"; "consensus"; "pre"]
    @@ fun protocol ->
    test_pending_operations_consensus Operation.Preattestation protocol

  let test_pending_operations_double_consensus_evidence double_evidence_kind
      protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let* () = Client.bake_for_and_wait ~node client in

    let* consensus_op =
      create_double_consensus_evidence
        ~use_legacy_name:true
        ~double_evidence_kind
        client
    in
    let* (`OpHash _) =
      Operation.inject ~force:true ~request:`Inject consensus_op client
    in

    let check_mempool ~use_legacy_name =
      let* mempool_json =
        RPC.Client.call client
        @@ RPC.get_chain_mempool_pending_operations
             ~version:(if use_legacy_name then "1" else "2")
             ()
      in
      return
        (check_kind
           JSON.(mempool_json |-> "applied" |> as_list |> List.hd)
           (Operation.Anonymous.kind_to_string
              double_evidence_kind
              use_legacy_name))
    in
    let* () = check_mempool ~use_legacy_name:true in
    check_mempool ~use_legacy_name:false

  let test_pending_double_consensus_evidence =
    register_test
      ~title:"Pending double consensus evidence operations"
      ~additionnal_tags:
        ["mempool"; "pending"; "operations"; "double"; "consensus"; "evidence"]
    @@ fun protocol ->
    test_pending_operations_double_consensus_evidence
      Operation.Anonymous.Double_attestation_evidence
      protocol

  let test_pending_double_preconsensus_evidence =
    register_test
      ~title:"Pending double pre-consensus evidence operations"
      ~additionnal_tags:
        [
          "mempool";
          "pending";
          "operations";
          "double";
          "consensus";
          "pre";
          "evidence";
        ]
    @@ fun protocol ->
    test_pending_operations_double_consensus_evidence
      Operation.Anonymous.Double_preattestation_evidence
      protocol

  let monitor_mempool node ~use_legacy_name =
    let monitor_operations_url =
      RPC.(
        make_uri
          node
          (get_chain_mempool_monitor_operations
             ~refused:true
             ~version:(if use_legacy_name then "0" else "1")
             ())
        |> Uri.to_string)
    in
    RPC.Curl.get monitor_operations_url

  let check_monitor_mempool p name =
    let* s = Process.check_and_read_stdout p in
    try
      let _ = Str.search_forward (Str.regexp name) s 0 in
      unit
    with Not_found -> Test.fail ~__LOC__ "Kind %s not found in %s" name s

  let check_invalid_monitor_mempool_version node =
    let monitor_operations_url =
      RPC.(
        make_uri
          node
          (get_chain_mempool_monitor_operations ~refused:true ~version:"2" ())
        |> Uri.to_string)
    in
    let*? p = RPC.Curl.get monitor_operations_url in
    let* s = Process.check_and_read_stdout p in
    try
      let _ =
        Str.(
          search_forward
            (regexp
               "Failed to parse the query string: Failed to parse argument \
                \'version\'"))
          s
          0
      in
      unit
    with Not_found ->
      Test.fail
        ~__LOC__
        {|RPC call should have return: Failed to parse the query string: Failed to parse argument \'version\' ..|}

  let test_monitor_operations_consensus kind protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let signer = Constant.bootstrap1 in

    let*? p_legacy = monitor_mempool node ~use_legacy_name:true in
    let*? p = monitor_mempool node ~use_legacy_name:false in

    let* consensus_op =
      create_consensus_op ~use_legacy_name:true ~signer ~kind client
    in
    let* (`OpHash _) =
      Operation.inject ~force:true ~request:`Inject consensus_op client
    in
    let* () = Client.bake_for_and_wait ~node client in

    let check_monitor_mempool p ~use_legacy_name =
      let name = Operation.Consensus.kind_to_string kind use_legacy_name in
      check_monitor_mempool p name
    in
    let* () = check_monitor_mempool p_legacy ~use_legacy_name:true in
    let* () = check_monitor_mempool p ~use_legacy_name:false in
    check_invalid_monitor_mempool_version node

  let test_monitor_consensus =
    register_test
      ~title:"Monitor consensus operations"
      ~additionnal_tags:["mempool"; "monitor"; "operations"; "consensus"]
    @@ fun protocol ->
    test_monitor_operations_consensus Operation.Attestation protocol

  let test_monitor_preconsensus =
    register_test
      ~title:"Monitor pre-consensus operations"
      ~additionnal_tags:["mempool"; "monitor"; "operations"; "consensus"; "pre"]
    @@ fun protocol ->
    test_monitor_operations_consensus Operation.Preattestation protocol

  let test_monitor_operations_double_consensus_evidence double_evidence_kind
      protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let* () = Client.bake_for_and_wait ~node client in

    let*? p_legacy = monitor_mempool node ~use_legacy_name:true in
    let*? p = monitor_mempool node ~use_legacy_name:false in

    let* consensus_op =
      create_double_consensus_evidence
        ~use_legacy_name:true
        ~double_evidence_kind
        client
    in
    let* (`OpHash _) =
      Operation.inject ~force:true ~request:`Inject consensus_op client
    in
    let* () = Client.bake_for_and_wait ~node client in

    let check_monitor_mempool p ~use_legacy_name =
      let name =
        Operation.Anonymous.kind_to_string double_evidence_kind use_legacy_name
      in
      check_monitor_mempool p name
    in
    let* () = check_monitor_mempool p_legacy ~use_legacy_name:true in
    let* () = check_monitor_mempool p ~use_legacy_name:false in
    check_invalid_monitor_mempool_version node

  let test_monitor_double_consensus_evidence =
    register_test
      ~title:"Monitor double consensus evidence operations"
      ~additionnal_tags:
        ["mempool"; "monitor"; "operations"; "double"; "consensus"; "evidence"]
    @@ fun protocol ->
    test_monitor_operations_double_consensus_evidence
      Operation.Anonymous.Double_attestation_evidence
      protocol

  let test_monitor_double_preconsensus_evidence =
    register_test
      ~title:"Monitor double pre-consensus evidence operations"
      ~additionnal_tags:
        [
          "mempool";
          "monitor";
          "operations";
          "double";
          "consensus";
          "pre";
          "evidence";
        ]
    @@ fun protocol ->
    test_monitor_operations_double_consensus_evidence
      Operation.Anonymous.Double_preattestation_evidence
      protocol

  let register ~protocols =
    test_pending_consensus protocols ;
    test_pending_preconsensus protocols ;
    test_pending_double_consensus_evidence protocols ;
    test_pending_double_preconsensus_evidence protocols ;
    test_monitor_consensus protocols ;
    test_monitor_preconsensus protocols ;
    test_monitor_double_consensus_evidence protocols ;
    test_monitor_double_preconsensus_evidence protocols
end

module Run_Simulate = struct
  type rpc = Run | Simulate

  let get_rpc rpc ?version op =
    match rpc with
    | Run ->
        RPC.post_chain_block_helpers_scripts_run_operation ?version (Data op)
    | Simulate ->
        RPC.post_chain_block_helpers_scripts_simulate_operation
          ?version
          ~data:(Data op)
          ()

  let get_rpc_name = function
    | Run -> "run_operations"
    | Simulate -> "simulate_operations"

  let test_rpc_operation_unsupported rpc kind protocol =
    let* _node, client = Client.init_with_protocol ~protocol `Client () in
    let signer = Constant.bootstrap1 in

    let call_rpc_operation_and_check_error op =
      let* op_json = Operation.make_run_operation_input op client in
      let msg =
        rex
          "Command failed: The run_operation RPC does not support consensus \
           operations."
        (* both run_operation and simulate_operation returns the same error *)
      in
      let*? p = RPC.Client.spawn client @@ get_rpc rpc op_json in
      Process.check_error ~msg p
    in

    let call_and_check_error ~use_legacy_name =
      Log.info
        "Create a %s operation, call %s and check that the call fail"
        (Operation.Consensus.kind_to_string kind use_legacy_name)
        (get_rpc_name rpc) ;

      let* consensus_op =
        create_consensus_op ~use_legacy_name ~signer ~kind client
      in
      call_rpc_operation_and_check_error consensus_op
    in

    let* () = call_and_check_error ~use_legacy_name:true in
    call_and_check_error ~use_legacy_name:false

  let test_run_operation_consensus =
    register_test
      ~title:"Run operation with consensus operations"
      ~additionnal_tags:["run"; "operations"; "consensus"]
    @@ fun protocol ->
    test_rpc_operation_unsupported Run Operation.Attestation protocol

  let test_run_operation_preconsensus =
    register_test
      ~title:"Run operation with preconsensus operations"
      ~additionnal_tags:["run"; "operations"; "pre"; "consensus"]
    @@ fun protocol ->
    test_rpc_operation_unsupported Run Operation.Preattestation protocol

  let test_simulate_operation_consensus =
    register_test
      ~title:"Simulate operation with consensus operations"
      ~additionnal_tags:["simulate"; "operations"; "consensus"]
    @@ fun protocol ->
    test_rpc_operation_unsupported Simulate Operation.Attestation protocol

  let test_simulate_operation_preconsensus =
    register_test
      ~title:"Simulate operation with preconsensus operations"
      ~additionnal_tags:["simulate"; "operations"; "pre"; "consensus"]
    @@ fun protocol ->
    test_rpc_operation_unsupported Simulate Operation.Preattestation protocol

  let test_rpc_double_consensus_evidence rpc double_evidence_kind protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let* () = Client.bake_for_and_wait ~node client in

    let call_and_check_versions ~use_legacy_name_in_input =
      Log.info
        "Create a %s operation and call %s "
        (Operation.Anonymous.kind_to_string
           double_evidence_kind
           use_legacy_name_in_input)
        (get_rpc_name rpc) ;

      let* consensus_op =
        create_double_consensus_evidence
          ~use_legacy_name:use_legacy_name_in_input
          ~double_evidence_kind
          client
      in
      let* op_json = Operation.make_run_operation_input consensus_op client in

      let rpc ~version data = get_rpc rpc ~version data in
      let check_version ~version =
        let name =
          Operation.Anonymous.kind_to_string
            double_evidence_kind
            (use_legacy_name_from_version version)
        in
        check_version ~version ~check:check_kind ~name ~rpc ~data:op_json client
      in
      let* () = check_version ~version:`Old in
      let* () = check_version ~version:`New in
      check_unknown_version ~rpc ~data:op_json client
    in
    let* () = call_and_check_versions ~use_legacy_name_in_input:true in
    call_and_check_versions ~use_legacy_name_in_input:false

  let test_run_operation_double_consensus_evidence =
    register_test
      ~title:"Run operation with double consensus evidence operations"
      ~additionnal_tags:["run"; "operations"; "consensus"; "double"]
    @@ fun protocol ->
    test_rpc_double_consensus_evidence
      Run
      Operation.Anonymous.Double_attestation_evidence
      protocol

  let test_run_operation_double_preconsensus_evidence =
    register_test
      ~title:"Run operation with double preconsensus evidence operations"
      ~additionnal_tags:["run"; "operations"; "consensus"; "pre"; "double"]
    @@ fun protocol ->
    test_rpc_double_consensus_evidence
      Run
      Operation.Anonymous.Double_preattestation_evidence
      protocol

  let test_simulate_operation_double_consensus_evidence =
    register_test
      ~title:"Simulate operation with double consensus evidence operations"
      ~additionnal_tags:["simulate"; "operations"; "consensus"; "double"]
    @@ fun protocol ->
    test_rpc_double_consensus_evidence
      Simulate
      Operation.Anonymous.Double_attestation_evidence
      protocol

  let test_simulate_operation_double_preconsensus_evidence =
    register_test
      ~title:"Simulate operation with double preconsensus evidence operations"
      ~additionnal_tags:["simulate"; "operations"; "consensus"; "pre"; "double"]
    @@ fun protocol ->
    test_rpc_double_consensus_evidence
      Simulate
      Operation.Anonymous.Double_preattestation_evidence
      protocol

  let register ~protocols =
    test_run_operation_consensus protocols ;
    test_run_operation_preconsensus protocols ;
    test_simulate_operation_consensus protocols ;
    test_simulate_operation_preconsensus protocols ;
    test_run_operation_double_consensus_evidence protocols ;
    test_run_operation_double_preconsensus_evidence protocols ;
    test_simulate_operation_double_consensus_evidence protocols ;
    test_simulate_operation_double_preconsensus_evidence protocols
end

module Preapply = struct
  let test_consensus kind protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let* () = Client.bake_for_and_wait ~node client in
    let signer = Constant.bootstrap1 in
    let* level, slots, block_payload_hash =
      get_consensus_info signer.public_key_hash client
    in

    let preapply_op ~use_legacy_name =
      let* consensus_op =
        create_consensus_op
          ~level
          ~slot:(List.hd slots)
          ~block_payload_hash
          ~use_legacy_name
          ~signer
          ~kind
          client
      in
      let* signature = Operation.sign consensus_op client in
      let consensus_json =
        Operation.make_preapply_operation_input
          ~protocol
          ~signature
          consensus_op
      in
      let* _ =
        RPC.Client.call client
        @@ RPC.post_chain_block_helpers_preapply_operations
             ~data:(Data (`A [consensus_json]))
             ()
      in
      unit
    in

    let* () = preapply_op ~use_legacy_name:true in
    preapply_op ~use_legacy_name:false

  let test_preapply_consensus =
    register_test
      ~title:"Preapply operation with consensus operations"
      ~additionnal_tags:["preapply"; "operations"; "consensus"]
    @@ fun protocol -> test_consensus Operation.Attestation protocol

  let test_preapply_preconsensus =
    register_test
      ~title:"Preapply operation with preconsensus operations"
      ~additionnal_tags:["preapply"; "operations"; "pre"; "consensus"]
    @@ fun protocol -> test_consensus Operation.Preattestation protocol

  let test_double_consensus_evidence double_evidence_kind protocol =
    let* node, client = Client.init_with_protocol ~protocol `Client () in
    let* () = Client.bake_for_and_wait ~node client in

    let preapply_op ~use_legacy_name =
      let* double_consensus_evidence_op =
        create_double_consensus_evidence
          ~use_legacy_name
          ~double_evidence_kind
          client
      in
      let* signature = Operation.sign double_consensus_evidence_op client in
      let consensus_json =
        Operation.make_preapply_operation_input
          ~protocol
          ~signature
          double_consensus_evidence_op
      in
      let* _ =
        RPC.Client.call client
        @@ RPC.post_chain_block_helpers_preapply_operations
             ~data:(Data (`A [consensus_json]))
             ()
      in
      unit
    in
    let* () = preapply_op ~use_legacy_name:true in
    preapply_op ~use_legacy_name:false

  let test_preapply_double_consensus_evidence =
    register_test
      ~title:"Preapply operation with double consensus evidence operations"
      ~additionnal_tags:["preapply"; "double"; "operations"; "consensus"]
    @@ fun protocol ->
    test_double_consensus_evidence
      Operation.Anonymous.Double_attestation_evidence
      protocol

  let test_preapply_double_preconsensus_evidence =
    register_test
      ~title:"Preapply operation with duoble preconsensus evidence operations"
      ~additionnal_tags:["preapply"; "double"; "operations"; "pre"; "consensus"]
    @@ fun protocol ->
    test_double_consensus_evidence
      Operation.Anonymous.Double_preattestation_evidence
      protocol

  let register ~protocols =
    test_preapply_consensus protocols ;
    test_preapply_preconsensus protocols ;
    test_preapply_double_consensus_evidence protocols ;
    test_preapply_double_preconsensus_evidence protocols
end

let register ~protocols =
  Forge.register ~protocols ;
  Parse.register ~protocols ;
  Mempool.register ~protocols ;
  Run_Simulate.register ~protocols ;
  Preapply.register ~protocols
