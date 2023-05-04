(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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
   Component:    Data-availability layer
   Requirements: make -f kernels.mk build
   Invocation:   dune exec tezt/tests/main.exe -- --file dac.ml
   Subject: Integration tests related to the data-availability layer
*)
open Dac_helper

let hooks = Tezos_regression.hooks

let assert_lwt_failure ?__LOC__ msg lwt_under_inspection =
  let* passed =
    Lwt.catch
      (fun () -> Lwt.map (fun _a -> false) lwt_under_inspection)
      (fun _exn -> return true)
  in
  if passed then unit else Test.fail ?__LOC__ msg

let init_hex_root_hash ?payload coordinator_node =
  let payload = Option.value payload ~default:"hello test message" in
  let* root_hash, _l1_op =
    RPC.call
      coordinator_node
      (Dac_rpc.post_store_preimage ~payload ~pagination_scheme:"Merkle_tree_V0")
  in
  let hex_root_hash = `Hex root_hash in
  return hex_root_hash

let assert_verify_aggregate_signature members_keys hex_root_hash agg_sig_b58 =
  let verified =
    let root_hash = Hex.to_bytes hex_root_hash in
    let data =
      List.map
        (fun (member : Account.aggregate_key) ->
          let pk =
            Tezos_crypto.Aggregate_signature.Public_key.of_b58check_exn
              member.aggregate_public_key
          in
          (pk, None, root_hash))
        members_keys
    in
    Tezos_crypto.Aggregate_signature.aggregate_check
      data
      (Tezos_crypto.Aggregate_signature.of_b58check_exn agg_sig_b58)
  in
  Check.(
    (true = verified)
      ~__LOC__
      bool
      ~error_msg:"Failed to verify aggregate signature.")

let assert_witnesses ~__LOC__ expected witnesses =
  Check.(
    (expected = witnesses)
      ~__LOC__
      int
      ~error_msg:"Expected witnesses bitset to be %L. Found: %R")

module Certificate_V0 = struct
  let parse_certificate json =
    JSON.
      ( json |-> "witnesses" |> as_int,
        json |-> "aggregate_signature" |> as_string,
        json |-> "root_hash" |> as_string,
        json |-> "version" |> as_int )

let wait_for_layer1_block_processing dac_node level =
  Dac_node.wait_for dac_node "dac_node_layer_1_new_head.v0" (fun e ->
      if JSON.(e |-> "level" |> as_int) = level then Some () else None)

let wait_for_root_hash_pushed_to_data_streamer dac_node root_hash =
  Dac_node.wait_for
    dac_node
    "root_hash_pushed_to_the_data_streamer.v0"
    (fun json -> if JSON.(json |> as_string) = root_hash then Some () else None)

let wait_for_signature_pushed_to_coordinator dac_node signature =
  Dac_node.wait_for
    dac_node
    "new_signature_pushed_to_coordinator.v0"
    (fun json -> if JSON.(json |> as_string) = signature then Some () else None)

let wait_for_received_root_hash_processed dac_node root_hash =
  Dac_node.wait_for
    dac_node
    "dac_node_received_root_hash_processed.v0"
    (fun json -> if JSON.(json |> as_string) = root_hash then Some () else None)

let wait_for_received_root_hash dac_node root_hash =
  Dac_node.wait_for dac_node "dac_node_new_root_hash_received.v0" (fun json ->
      if JSON.(json |> as_string) = root_hash then Some () else None)

let wait_for_handle_new_subscription_to_hash_streamer dac_node =
  Dac_node.wait_for
    dac_node
    "handle_new_subscription_to_hash_streamer.v0"
    (fun _ -> Some ())

let bls_sign_hex_hash (signer : Account.aggregate_key) hex_root_hash =
  let sk =
    match signer.aggregate_secret_key with
    | Unencrypted sk -> sk
    | Encrypted encsk -> raise (Invalid_argument encsk)
  in
  let bytes_root_hash = Hex.to_bytes hex_root_hash in
  let sk = Tezos_crypto.Aggregate_signature.Secret_key.of_b58check_exn sk in
  Tezos_crypto.Aggregate_signature.sign sk bytes_root_hash

type status = Applied | Failed of {error_id : string}

let pp fmt = function
  | Applied -> Format.fprintf fmt "applied"
  | Failed {error_id} -> Format.fprintf fmt "failed: %s" error_id

let status_typ = Check.equalable pp ( = )

let send_messages ?(src = Constant.bootstrap2.alias) ?(alter_final_msg = Fun.id)
    client msgs =
  let msg =
    alter_final_msg
    @@ Ezjsonm.(to_string ~minify:true @@ list Ezjsonm.string msgs)
  in
  let* () = Client.Sc_rollup.send_message ~hooks ~src ~msg client in
  Client.bake_for_and_wait client

let bake_levels n client = repeat n (fun () -> Client.bake_for_and_wait client)

let check_valid_root_hash expected_rh actual_rh =
  Check.(
    (actual_rh = expected_rh)
      string
      ~error_msg:"Invalid root hash returned (Current: %L <> Expected: %R)")

let check_preimage expected_preimage actual_preimage =
  Check.(
    (actual_preimage = expected_preimage)
      string
      ~error_msg:
        "Preimage does not match expected value (Current: %L <> Expected: %R)")

(** [check_downloaded_page coordinator observer page_hash] checks that the
     [observer] has downloaded a page with [page_hash] from the [coordinator],
     that the contents of the page corresponds to the ones of the
     [coordinator]. It returns the list  of the hashes contained in the
     [page_hash], if the page corresponds to a hash page. Otherwise, it returns
     the empty list. *)
let check_downloaded_page coordinator observer page_hash =
  let* coordinator_hex_encoded_page =
    RPC.call coordinator (Dac_rpc.get_preimage page_hash)
  in
  let coordinator_page = Hex.to_string (`Hex coordinator_hex_encoded_page) in
  (* Check that the page has been saved by the observer. *)
  let* observer_hex_encoded_page =
    RPC.call observer (Dac_rpc.get_preimage page_hash)
  in
  let observer_page = Hex.to_string (`Hex observer_hex_encoded_page) in
  (* Check that the raw page for the root hash  stored in the coordinator
     is the same as the raw page stored in the observer. *)
  Check.(
    (coordinator_page = observer_page)
      string
      ~error_msg:
        "Returned page does not match the expected one (Current: %L <> \
         Expected: %R)") ;
  let version_tag = observer_page.[0] in
  if version_tag = '\000' then return []
  else
    let hash_size = 33 in
    let preamble_size = 5 in
    let concatenated_hashes =
      String.sub observer_page 5 (String.length observer_page - preamble_size)
    in
    let rec split_hashes concatenated_hashes hashes =
      if String.equal concatenated_hashes "" then hashes
      else
        let next_hash =
          Hex.show @@ Hex.of_string
          @@ String.sub concatenated_hashes 0 hash_size
        in
        let next_concatenated_hashes =
          String.sub
            concatenated_hashes
            hash_size
            (String.length concatenated_hashes - hash_size)
        in
        split_hashes next_concatenated_hashes (next_hash :: hashes)
    in
    return @@ split_hashes concatenated_hashes []

let check_downloaded_preimage coordinator observer root_hash =
  let rec go hashes =
    match hashes with
    | [] -> return ()
    | hash :: hashes ->
        let* next_hashes = check_downloaded_page coordinator observer hash in
        go (hashes @ next_hashes)
  in
  go [root_hash]

let check_certificate
    (actual_witnesses, actual_aggregate_signature, actual_root_hash, _version)
    ( expected_witnesses,
      expected_aggregate_signature,
      expected_root_hash,
      _version ) =
  (* Because encodings of aggregate signature might be different (due to the
     presence of two variants `Bls12_381` and `Unknown), the actual byte
     representation needs to be checked.*)
  let raw_signature s =
    Data_encoding.Binary.to_string_exn Tezos_crypto.Aggregate_signature.encoding
    @@ Tezos_crypto.Aggregate_signature.of_b58check_exn s
  in
  Check.(
    (actual_witnesses = expected_witnesses)
      int
      ~error_msg:"Unexpected bitset for witnesses (Actual %L <> %R = Expected)") ;
  Check.(
    (raw_signature expected_aggregate_signature
    = raw_signature actual_aggregate_signature)
      string
      ~error_msg:"Unexpected aggregate signature (Expected %L <> %R = Actual") ;
  check_valid_root_hash expected_root_hash actual_root_hash

let sample_payload example_filename =
  let json =
    JSON.parse_file @@ "tezt/tests/dac_example_payloads/" ^ example_filename
    ^ ".json"
  in
  let payload =
    JSON.(json |-> "payload" |> as_string |> fun s -> Hex.to_string (`Hex s))
  in
  let root_hash = JSON.(json |-> "root_hash" |> as_string) in
  (payload, root_hash)

(** This modules encapsulate tests for DAC nodes when running in legacy node.
    It includes tests where we have two dac nodes running in
    the legacy mode interacting with each other. As such one node normally tries
    to mimic the coordinator and the other tries to mimic signer or observer.
    Note that both nodes still run in the [legacy] mode, where as such there is
    no notion of profiles. Once we have a fully working profiles, tests from this
    module should be refactored. *)
module Legacy = struct
  let set_coordinator dac_node coordinator =
    let coordinator =
      `O
        [
          ("rpc-host", `String (Dac_node.rpc_host coordinator));
          ("rpc-port", `Float (float_of_int (Dac_node.rpc_port coordinator)));
        ]
    in
    let mode_updated =
      Dac_node.Config_file.read dac_node
      |> JSON.get "mode"
      |> JSON.put
           ( "dac_cctxt_config",
             JSON.annotate ~origin:"dac_node_config" coordinator )
    in
    Dac_node.Config_file.update dac_node (JSON.put ("mode", mode_updated))

  let coordinator_serializes_payload coordinator ~payload ~expected_rh =
    let* actual_rh, _l1_operation =
      RPC.call
        coordinator
        (Dac_rpc.post_store_preimage
           ~payload
           ~pagination_scheme:"Merkle_tree_V0")
    in
    return @@ check_valid_root_hash expected_rh actual_rh

  let test_dac_node_imports_committee_members =
    Protocol.register_test
      ~__FILE__
      ~title:"dac node imports dac members sk_uris"
      ~tags:["dac"; "dac_node"]
      ~supports:Protocol.(From_protocol (Protocol.number Alpha))
    @@ fun protocol ->
    let* node, client = Client.init_with_protocol `Client ~protocol () in
    let run_dac = Dac_node.run ~wait_ready:false in
    let* committee_member =
      Client.bls_gen_keys ~alias:"committee_member" client
    in
    let* committee_member_info =
      Client.bls_show_address ~alias:committee_member client
    in
    let committee_member_address =
      committee_member_info.aggregate_public_key_hash
    in
    let dac_node =
      Dac_node.create_legacy
        ~node
        ~client
        ~threshold:1
        ~committee_members:[committee_member_address]
        ()
    in
    let* _dir = Dac_node.init_config dac_node in
    let ready_promise =
      Dac_node.wait_for dac_node "dac_is_ready.v0" (fun _ -> Some ())
    in
    let* () = run_dac dac_node in
    let* () = ready_promise in
    let* () = Dac_node.terminate dac_node in
    unit

  let test_dac_node_dac_threshold_not_reached =
    Protocol.register_test
      ~__FILE__
      ~title:"dac node displays warning if dac threshold is not reached"
      ~tags:["dac"; "dac_node"]
      ~supports:Protocol.(From_protocol (Protocol.number Alpha))
    @@ fun protocol ->
    let* node, client = Client.init_with_protocol `Client ~protocol () in
    let dac_node =
      Dac_node.create_legacy ~node ~client ~threshold:1 ~committee_members:[] ()
    in
    let* _dir = Dac_node.init_config dac_node in
    let run_dac = Dac_node.run ~wait_ready:false in
    let error_promise =
      Dac_node.wait_for dac_node "dac_threshold_not_reached.v0" (fun _ ->
          Some ())
    in
    let* () = run_dac dac_node in
    let* () = error_promise in
    Dac_node.terminate dac_node

  let test_dac_node_startup =
    Protocol.register_test
      ~__FILE__
      ~title:"dac node startup"
      ~tags:["dac"; "dac_node"]
    @@ fun protocol ->
    let run_dac = Dac_node.run ~wait_ready:false in
    let nodes_args = Node.[Synchronisation_threshold 0] in
    let previous_protocol =
      match Protocol.previous_protocol protocol with
      | Some p -> p
      | None -> assert false
    in
    let* node, client =
      Client.init_with_protocol
        `Client
        ~protocol:previous_protocol
        ~event_sections_levels:[("prevalidator", `Debug)]
        ~nodes_args
        ()
    in
    let dac_node =
      Dac_node.create_legacy ~node ~client ~threshold:0 ~committee_members:[] ()
    in
    let* _dir = Dac_node.init_config dac_node in
    let* () = run_dac dac_node in
    let* () =
      Dac_node.wait_for dac_node "dac_node_layer_1_start_tracking.v0" (fun _ ->
          Some ())
    in
    assert (Dac_node.is_running_not_ready dac_node) ;
    let* () = Dac_node.terminate dac_node in
    let* () = Node.terminate node in
    Node.Config_file.update
      node
      (Node.Config_file.set_sandbox_network_with_user_activated_overrides
         [(Protocol.hash previous_protocol, Protocol.hash protocol)]) ;
    let* () = Node.run node nodes_args in
    let* () = Node.wait_for_ready node in
    let* () = run_dac dac_node in
    let* () =
      Lwt.join
        [
          Dac_node.wait_for dac_node "dac_node_plugin_resolved.v0" (fun _ ->
              Some ());
          Client.bake_for_and_wait client;
        ]
    in
    let* () = Dac_node.terminate dac_node in
    return ()

  let test_dac_node_handles_dac_store_preimage_merkle_V0 _protocol dac_node
      sc_rollup_node _sc_rollup_address _node _client pvm_name _threshold
      _committee_members =
    let payload = "test" in
    let* actual_rh, l1_operation =
      RPC.call
        dac_node
        (Dac_rpc.post_store_preimage
           ~payload
           ~pagination_scheme:"Merkle_tree_V0")
    in
    (* Expected reveal hash equals to the result of
       [Tezos_dac_alpha.Dac_pages_encoding.Merkle_tree.V0.serialize_payload "test"].
    *)
    let expected_rh =
      "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
    in
    check_valid_root_hash expected_rh actual_rh ;
    let filename =
      Filename.concat
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
        actual_rh
    in
    let cin = open_in filename in
    let recovered_payload = really_input_string cin (in_channel_length cin) in
    let () = close_in cin in
    (* Discard first five preamble bytes *)
    let recovered_preimage =
      String.sub recovered_payload 5 (String.length recovered_payload - 5)
    in
    check_preimage payload recovered_preimage ;
    let* is_signature_valid =
      RPC.call dac_node (Dac_rpc.get_verify_signature l1_operation)
    in
    Check.(
      (is_signature_valid = true)
        bool
        ~error_msg:"Signature of external message is not valid") ;
    unit

  let test_dac_node_handles_dac_store_preimage_hash_chain_V0 _protocol dac_node
      sc_rollup_node _sc_rollup_address _node _client pvm_name _threshold
      _committee_members =
    let payload = "test" in
    let* actual_rh, _l1_operation =
      RPC.call
        dac_node
        (Dac_rpc.post_store_preimage
           ~payload
           ~pagination_scheme:"Hash_chain_V0")
    in
    (* Expected reveal hash equals to the result of
       [Tezos_dac_alpha.Dac_pages_encoding.Hash_chain.V0.serialize_payload "test"].
    *)
    let expected_rh =
      "00928b20366943e2afd11ebc0eae2e53a93bf177a4fcf35bcc64d503704e65e202"
    in
    check_valid_root_hash expected_rh actual_rh ;
    let filename =
      Filename.concat
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
        actual_rh
    in
    let cin = open_in filename in
    let recovered_payload = really_input_string cin (in_channel_length cin) in
    let () = close_in cin in
    let recovered_preimage =
      String.sub recovered_payload 0 (String.length payload)
    in
    check_preimage payload recovered_preimage ;
    unit

  let test_dac_node_handles_dac_retrieve_preimage_merkle_V0 _protocol dac_node
      sc_rollup_node _sc_rollup_address _node _client pvm_name _threshold
      _committee_members =
    let payload = "test" in
    let* actual_rh, _l1_operation =
      RPC.call
        dac_node
        (Dac_rpc.post_store_preimage
           ~payload
           ~pagination_scheme:"Merkle_tree_V0")
    in
    (* Expected reveal hash equals to the result of
       [Tezos_dac_alpha.Dac_pages_encoding.Merkle_tree.V0.serialize_payload "test"].
    *)
    let expected_rh =
      "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
    in
    check_valid_root_hash expected_rh actual_rh ;
    let filename =
      Filename.concat
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
        actual_rh
    in
    let cin = open_in filename in
    let recovered_payload = really_input_string cin (in_channel_length cin) in
    let () = close_in cin in
    let recovered_preimage = Hex.of_string recovered_payload in
    let* preimage = RPC.call dac_node (Dac_rpc.get_preimage expected_rh) in
    Check.(
      (preimage = Hex.show recovered_preimage)
        string
        ~error_msg:
          "Returned page does not match the expected one (Current: %L <> \
           Expected: %R)") ;
    unit

  let test_rollup_arith_uses_reveals protocol dac_node sc_rollup_node
      sc_rollup_address _node client _pvm_name _threshold _committee_members =
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
           sc_rollup_address
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in
    let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
    let* level =
      Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node init_level
    in
    let nadd = 32 * 1024 in
    let payload =
      let rec aux b n =
        if n > 0 then (
          Buffer.add_string b "1 +" ;
          (aux [@tailcall]) b (n - 1))
        else (
          Buffer.add_string b "value" ;
          String.of_bytes (Buffer.to_bytes b))
      in
      let buf = Buffer.create ((nadd * 3) + 2) in
      Buffer.add_string buf "0 " ;
      aux buf nadd
    in
    let* actual_rh, _l1_operation =
      RPC.call
        dac_node
        (Dac_rpc.post_store_preimage
           ~payload
           ~pagination_scheme:"Hash_chain_V0")
    in
    let expected_rh =
      "0027782d2a7020be332cc42c4e66592ec50305f559a4011981f1d5af81428e7aa3"
    in
    check_valid_root_hash expected_rh actual_rh ;
    let* () =
      send_messages
        client
        ["hash:" ^ actual_rh]
        ~alter_final_msg:(fun s -> "text:" ^ s)
    in
    let* () = bake_levels 2 client in
    let* _ =
      Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node (level + 2)
    in
    let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
    let*! encoded_value =
      Sc_rollup_client.state_value ~hooks sc_rollup_client ~key:"vars/value"
    in
    let value =
      match Data_encoding.(Binary.of_bytes int31) @@ encoded_value with
      | Error error ->
          failwith
            (Format.asprintf
               "The arithmetic PVM has an unexpected state: %a"
               Data_encoding.Binary.pp_read_error
               error)
      | Ok x -> x
    in
    Check.(
      (value = nadd) int ~error_msg:"Invalid value in rollup state (%L <> %R)") ;
    unit

  let test_reveals_fails_on_wrong_hash _protocol dac_node sc_rollup_node
      sc_rollup_address _node client _pvm_name _threshold _committee_members =
    let payload = "Some data that is not related to the hash" in
    let _actual_rh =
      RPC.call
        dac_node
        (Dac_rpc.post_store_preimage
           ~payload
           ~pagination_scheme:"Hash_chain_V0")
    in
    let errorneous_hash =
      "0027782d2a7020be332cc42c4e66592ec50305f559a4011981f1d5af81428ecafe"
    in
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
           sc_rollup_address
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in
    let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
    let error_promise =
      Sc_rollup_node.wait_for
        sc_rollup_node
        "sc_rollup_daemon_error.v0"
        (fun e ->
          let id = JSON.(e |=> 0 |-> "id" |> as_string) in
          if id =~ rex "could_not_open_reveal_preimage_file" then Some (Ok ())
          else Some (Error id))
    in
    let* _level =
      Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node init_level
    in
    let* () =
      send_messages
        client
        ["hash:" ^ errorneous_hash]
        ~alter_final_msg:(fun s -> "text:" ^ s)
    in
    let* ok = error_promise in
    match ok with
    | Ok () -> unit
    | Error id -> Test.fail "Rollup node failed with unexpected error %s" id

  (* The following tests involve multiple legacy DAC nodes running at
     the same time and playing either the coordinator, committee member or
     observer role. *)

  let test_streaming_of_root_hashes_as_observer _protocol node client
      coordinator threshold committee_members =
    (* 1. Create two new dac nodes; [observer_1] and [observer_2].
       2. Initialize their default configuration.
       3. Update their configuration so that their dac node client context
          points to [coordinator]. *)
    let committee_members =
      List.map
        (fun (a : Account.aggregate_key) -> a.aggregate_public_key_hash)
        committee_members
    in
    let observer_1 =
      Dac_node.create_legacy ~threshold ~committee_members ~node ~client ()
    in
    let observer_2 =
      Dac_node.create_legacy ~threshold ~committee_members ~node ~client ()
    in
    let* _ = Dac_node.init_config observer_1 in
    let* _ = Dac_node.init_config observer_2 in
    let () = set_coordinator observer_1 coordinator in
    let () = set_coordinator observer_2 coordinator in
    let payload_1 = "test_1" in
    let expected_rh_1 =
      "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
    in
    let payload_2 = "test_2" in
    let expected_rh_2 =
      "00f2f47f480fec0e4180930790e52a54b2dbd7676b5fa2a25dd93bf22969f22e33"
    in
    let push_promise_1 =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh_1
    in
    let push_promise_2 =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh_2
    in
    let observer_1_promise_1 =
      wait_for_received_root_hash observer_1 expected_rh_1
    in
    let observer_1_promise_2 =
      wait_for_received_root_hash observer_1 expected_rh_2
    in
    let observer_2_promise_1 =
      wait_for_received_root_hash observer_2 expected_rh_1
    in
    let observer_2_promise_2 =
      wait_for_received_root_hash observer_2 expected_rh_2
    in

    (* Start running [observer_1]. From now on we expect [observer_1] to
       monitor streamed root hashes produced by [coordinator]. [coordinator]
       produces and pushes them as a side effect of serializing dac payload. *)
    let observer_1_is_subscribed =
      wait_for_handle_new_subscription_to_hash_streamer coordinator
    in
    let* () = Dac_node.run observer_1 in
    let* () = observer_1_is_subscribed in
    (* [coordinator] serializes [payload_1]. We expect it would push
       [expected_rh_1] to all attached subscribers, i.e. to [observer_1]. *)
    let* () =
      coordinator_serializes_payload
        coordinator
        ~payload:payload_1
        ~expected_rh:expected_rh_1
    in
    (* Assert [coordinator] emitted event that [expected_rh_1] was pushed
       to the data_streamer. *)
    let* () = push_promise_1 in
    (* Assert [observer_1] emitted event of received [expected_rh_1]. *)
    let* () = observer_1_promise_1 in
    (* Start running [observer_2]. We expect that from now on [observer_2]
       will also monitor streamed root hashes from [coordinator]. *)
    let observer_2_is_subscribed =
      wait_for_handle_new_subscription_to_hash_streamer coordinator
    in
    let push_signature =
      wait_for_signature_pushed_to_coordinator observer_1 ""
    in
    let* () = Dac_node.run observer_2 in
    let* () = observer_2_is_subscribed in
    (* [coordinator] serializes [payload_2]. We expect it would push
       [expected_rh_2] to all attached subscribers,
       i.e. to both [observer_1] and [observer_2] this time. *)
    let* () =
      coordinator_serializes_payload
        coordinator
        ~payload:payload_2
        ~expected_rh:expected_rh_2
    in
    (* Assert [coordinator] emitted event. *)
    let* () = push_promise_2 in
    (* Assert both [observer_1] and [observer_2] received [expected_rh_2]. *)
    let* () = observer_1_promise_2 in
    let* () = observer_2_promise_2 in
    (* Since [observer_2] was not running when [expected_rh_1] was generated
       and streamed by [coordinator], we expect it never received it.
       We assert this, by making sure that the promise of [observer_2] about
       waiting for the emitted event with payload [expected_rh_1] is still not
       resolved after the promise [observer_2_promise_2] has been resolved. *)
    assert (
      Lwt.is_sleeping observer_2_promise_1 && Lwt.is_sleeping push_signature) ;
    unit

  let test_streaming_of_root_hashes_as_member _protocol node client coordinator
      threshold dac_members =
    (* This test doesn't have any meaning if run without any committee member. *)
    assert (List.length dac_members > 0) ;
    let member_key : Account.aggregate_key = List.nth dac_members 0 in
    let dac_member_pkh = member_key.aggregate_public_key_hash in

    let member =
      Dac_node.create_legacy
        ~threshold
        ~committee_members:[]
        ~node
        ~client
        ?committee_member_address:(Some dac_member_pkh)
        ()
    in
    let* _ = Dac_node.init_config member in
    let () = set_coordinator member coordinator in
    let payload = "test_1" in
    let expected_rh =
      "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
    in
    let push_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh
    in
    let member_promise = wait_for_received_root_hash member expected_rh in

    (* Start running [member]. From now on we expect [member] to
       monitor streamed root hashes produced by [coordinator]. [coordinator]
       produces and pushes them as a side effect of serializing dac payload. *)
    let member_is_subscribed =
      wait_for_handle_new_subscription_to_hash_streamer coordinator
    in
    let expected_signature = bls_sign_hex_hash member_key (`Hex expected_rh) in
    let* () = Dac_node.run member in
    let* () = member_is_subscribed in
    (* [coordinator] serializes [payload_1]. We expect it would push
       [expected_rh_1] to all attached subscribers, i.e. to [member]. *)
    let* () =
      coordinator_serializes_payload coordinator ~payload ~expected_rh
    in
    (* Assert [coordinator] emitted event that [expected_rh_1] was pushed
       to the data_streamer. *)
    let* () = push_promise in
    (* Assert [member] emitted event of received [expected_rh_1]. *)
    let* () = member_promise in

    (* If the signature inside the emitted event is equal to the [expected_signature]
       test is OK *)
    let* () =
      wait_for_signature_pushed_to_coordinator
        member
        (Tezos_crypto.Aggregate_signature.to_b58check expected_signature)
    in
    unit

  let test_observer_downloads_pages _protocol node client coordinator threshold
      committee_members =
    (* 1. Create one new dac nodes; [observer_1],
       2. Initialize the default configuration,
       3. Specify a temporary directory within the test data for the observer
          reveal data dir,
       4. Update the configuration of the observer so that the dac node client
          context points to [coordinator]. *)
    let committee_members =
      List.map
        (fun (dc : Account.aggregate_key) -> dc.aggregate_public_key_hash)
        committee_members
    in
    let observer =
      Dac_node.create_legacy
        ~threshold
        ~committee_members
        ~name:"observer"
        ~node
        ~client
        ()
    in
    let* _ = Dac_node.init_config observer in
    let () = set_coordinator observer coordinator in
    (* Payload with more than 4091 bytes to check recursive calls of the
       committee member to the coordinator.
       The payload of this JSON file corresponds to the hex encoded version of
       the Inferno, Canto I, by Dante Alighieri. The original text is also used
       in the uit tests
       (see src/proto_alpha/lib_dac/test/test_dac_pages_encoding.ml). Because
       the unit test and the integration test use pages of different size,
       the final root hash obtained is different from the one in the unit
       tests. *)
    let payload, expected_rh = sample_payload "preimage" in
    let push_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh
    in
    let wait_for_observer_subscribed_to_data_streamer =
      wait_for_handle_new_subscription_to_hash_streamer coordinator
    in
    let fetch_root_hash_promise =
      wait_for_received_root_hash_processed observer expected_rh
    in

    (* Test starts here *)

    (* Start running [observer_1]. From now on we expect [observer_1] to monitor
       streamed root hashes produced by [coordinator]. [coordinator] produces
       and pushes them as a side effect of serializing dac payload. *)
    let* () = Dac_node.run observer in
    let* () = wait_for_observer_subscribed_to_data_streamer in
    (* [coordinator] serializes [payload_1]. We expect it would push
       [expected_rh_1] to all attached subscribers, i.e. to [observer_1]. *)
    let* () =
      coordinator_serializes_payload coordinator ~payload ~expected_rh
    in
    (* Assert [coordinator] emitted event that [expected_rh] was pushed
       to the data_streamer. *)
    let* () = push_promise in
    (* Assert [observer] emitted event of received [expected_rh]. *)
    let* () = fetch_root_hash_promise in
    check_downloaded_preimage coordinator observer expected_rh

  (* 1. Observer should fetch missing page from Coordinator when GET /missing_page/{hash}
        is called.
     2. As a side effect, Observer should save fetched page into its page store before
        returning it in the response. This can be observed by checking the result of
        retrieving preimage before and after the GET /missing_page/{hash} call.*)
  let test_observer_get_missing_page _protocol node client coordinator threshold
      _committee_members =
    let root_hash =
      "00649d431e829f4adc68edecb8d8d8071154b57086cc124b465f6f6600a4bc91c7"
    in
    let root_hash_stream_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator root_hash
    in
    let* hex_root_hash =
      init_hex_root_hash ~payload:"test payload abc 123" coordinator
    in
    assert (root_hash = Hex.show hex_root_hash) ;
    let* () = root_hash_stream_promise in
    let observer =
      Dac_node.create_legacy ~threshold ~committee_members:[] ~node ~client ()
    in
    let* _ = Dac_node.init_config observer in
    let () = set_coordinator observer coordinator in
    let* () = Dac_node.run observer in
    let* () =
      assert_lwt_failure
        ~__LOC__
        "Expected retrieve_preimage"
        (RPC.call observer (Dac_rpc.get_preimage (Hex.show hex_root_hash)))
    in
    let* missing_page =
      RPC.call observer (Dac_rpc.get_missing_page ~hex_root_hash)
    in
    let* coordinator_page =
      RPC.call coordinator (Dac_rpc.get_preimage (Hex.show hex_root_hash))
    in
    check_preimage coordinator_page missing_page ;
    let* observer_preimage =
      RPC.call observer (Dac_rpc.get_preimage (Hex.show hex_root_hash))
    in
    check_preimage coordinator_page observer_preimage ;
    unit

  module Signature_manager = struct
    let test_non_committee_signer_should_fail tz_client
        (coordinator_node, hex_root_hash, _dac_committee) =
      let* invalid_signer_key =
        Client.bls_gen_and_show_keys ~alias:"invalid_signer" tz_client
      in
      let signature = bls_sign_hex_hash invalid_signer_key hex_root_hash in
      let result =
        RPC.call
          coordinator_node
          (Dac_rpc.put_dac_member_signature
             ~hex_root_hash
             ~dac_member_pkh:invalid_signer_key.aggregate_public_key_hash
             ~signature)
      in
      assert_lwt_failure
        ~__LOC__
        "Expected failure with non-committee member signer."
        result

    (* Tests that trying to store a dac member signature for a different key
       - one that was not used for creating the signature - fails. *)
    let test_signature_verification_failure_should_fail
        (coordinator_node, hex_root_hash, dac_committee) =
      let member_i = Random.int (List.length dac_committee) in
      let memberi = List.nth dac_committee member_i in
      let memberj =
        List.find
          (fun (dc : Account.aggregate_key) -> memberi <> dc)
          dac_committee
      in
      let signature = bls_sign_hex_hash memberi hex_root_hash in
      let result =
        RPC.call
          coordinator_node
          (Dac_rpc.put_dac_member_signature
             ~hex_root_hash
             ~dac_member_pkh:memberj.aggregate_public_key_hash
             ~signature)
      in
      assert_lwt_failure
        ~__LOC__
        "Expected failure when signature verification fails but did not."
        result

    (* Tests that a valid signature over [hex_root_hash] that is submitted to
       the [coordinator_node] is stored. 2 signatures are produced and stored
       in the [coordinator_node]. The effects of this can be asserted
       by checking that the witness bitset is set to 3 *)
    let test_store_valid_signature_should_update_aggregate_signature
        (coordinator_node, hex_root_hash, dac_committee) =
      let members =
        List.map
          (fun i ->
            let key = List.nth dac_committee i in
            let signature = bls_sign_hex_hash key hex_root_hash in
            (key, signature))
          (range 0 1)
      in
      let* members_keys =
        List.fold_left
          (fun keys ((member : Account.aggregate_key), signature) ->
            let* keys in
            let* () =
              RPC.call
                coordinator_node
                (Dac_rpc.put_dac_member_signature
                   ~hex_root_hash
                   ~dac_member_pkh:member.aggregate_public_key_hash
                   ~signature)
            in
            return (member :: keys))
          (return [])
          members
      in
      let* witnesses, certificate, _root_hash, _version =
        RPC.call
          coordinator_node
          (Dac_rpc.Certificate_V0.get_certificate ~hex_root_hash)
      in
      assert_witnesses ~__LOC__ 3 witnesses ;
      assert_verify_aggregate_signature members_keys hex_root_hash certificate ;
      unit

    let test_store_same_signature_more_than_once_should_be_noop
        (coordinator_node, _hex_root_hash, dac_committee) =
      let* hex_root_hash =
        init_hex_root_hash ~payload:"noop test abc 3210" coordinator_node
      in
      let member_i = 2 in
      let member = List.nth dac_committee member_i in
      let signature = bls_sign_hex_hash member hex_root_hash in
      let dac_member_pkh = member.aggregate_public_key_hash in
      let call () =
        RPC.call
          coordinator_node
          (Dac_rpc.put_dac_member_signature
             ~hex_root_hash
             ~dac_member_pkh
             ~signature)
      in
      let* () = call () in
      let* () = call () in
      let* witnesses, certificate, _root_hash, _version =
        RPC.call
          coordinator_node
          (Dac_rpc.Certificate_V0.get_certificate ~hex_root_hash)
      in
      assert_witnesses ~__LOC__ 4 witnesses ;
      assert_verify_aggregate_signature [member] hex_root_hash certificate ;
      unit

    (* Tests that the Coordinator refuses to store a [signature] for
       a [root_hash] that it doesn't know *)
    let invalid_signature (coordinator_node, _hex_root_hash, dac_committee) =
      let false_root_hash =
        `Hex
          "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
      in
      let member = List.nth dac_committee 0 in
      let signature = bls_sign_hex_hash member false_root_hash in
      let dac_member_pkh = member.aggregate_public_key_hash in
      let result =
        RPC.call
          coordinator_node
          (Dac_rpc.put_dac_member_signature
             ~hex_root_hash:false_root_hash
             ~dac_member_pkh
             ~signature)
      in
      assert_lwt_failure
        ~__LOC__
        "Expected failure when unknown root_hash"
        result

    let test_handle_store_signature _protocol _tezos_node tz_client coordinator
        _threshold dac_committee =
      let* hex_root_hash = init_hex_root_hash coordinator in
      let dac_env = (coordinator, hex_root_hash, dac_committee) in
      let* () = test_non_committee_signer_should_fail tz_client dac_env in
      let* () = test_signature_verification_failure_should_fail dac_env in
      let* () =
        test_store_valid_signature_should_update_aggregate_signature dac_env
      in
      let* () =
        test_store_same_signature_more_than_once_should_be_noop dac_env
      in
      let* () = invalid_signature dac_env in
      unit

    (* Tests that it's possible to retrieve the witness and certificate after
       storing a dac member signature. Also asserts that the certificate contains
       the member used for signing. *)
    let test_get_certificate _protocol _tezos_node _tz_client coordinator
        _threshold dac_committee =
      let i = Random.int (List.length dac_committee) in
      let member = List.nth dac_committee i in
      let* hex_root_hash =
        init_hex_root_hash
          ~payload:"test get certificate payload 123"
          coordinator
      in
      let signature = bls_sign_hex_hash member hex_root_hash in
      let* () =
        RPC.call
          coordinator
          (Dac_rpc.put_dac_member_signature
             ~hex_root_hash
             ~dac_member_pkh:member.aggregate_public_key_hash
             ~signature)
      in
      let* witnesses, certificate, _root_hash, _version =
        RPC.call
          coordinator
          (Dac_rpc.Certificate_V0.get_certificate ~hex_root_hash)
      in
      let expected_witnesses = Z.shift_left Z.one i in
      assert_witnesses ~__LOC__ (Z.to_int expected_witnesses) witnesses ;
      assert_verify_aggregate_signature [member] hex_root_hash certificate ;
      unit
  end
end

module Full_infrastructure = struct
  let coordinator_serializes_payload coordinator ~payload ~expected_rh =
    let* actual_rh =
      RPC.call coordinator (Dac_rpc.Coordinator.post_preimage ~payload)
    in
    return @@ check_valid_root_hash expected_rh actual_rh

  let test_coordinator_post_preimage_endpoint Scenarios.{coordinator_node; _} =
    (* 1. Send the [payload] to coordinator.
       2. Assert that it returns [expected_rh].
       3. Assert event that root hash has been pushed to data streamer
          was emitted. *)
    let payload = "test_1" in
    let expected_rh =
      "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
    in
    let root_hash_pushed_to_data_streamer_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator_node expected_rh
    in
    let* actual_rh =
      RPC.call coordinator_node (Dac_rpc.Coordinator.post_preimage ~payload)
    in
    let () = check_valid_root_hash expected_rh actual_rh in
    let* () = root_hash_pushed_to_data_streamer_promise in
    Lwt.return_unit

  let test_download_and_retrieval_of_pages
      Scenarios.{coordinator_node; committee_members_nodes; observer_nodes; _} =
    (* 0. Coordinator node is already running when the this function is
          executed by the test
       1. Run committee members and observers
       2. Post a preimage to coordinator
       3. Wait until all observer and committee members download the payload
       4. Check that all pages can be retrieved by committee members
          and observers using the GET preimage endpoint. *)
    let payload, expected_rh = sample_payload "preimage" in
    let push_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator_node expected_rh
    in
    let wait_for_node_subscribed_to_data_streamer () =
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
    in
    let wait_for_root_hash_processed_promises nodes =
      List.map
        (fun dac_node ->
          wait_for_received_root_hash_processed dac_node expected_rh)
        nodes
    in
    (* Initialize configuration of all nodes. *)
    let* _ =
      Lwt_list.iter_s
        (fun committee_member_node ->
          let* _ = Dac_node.init_config committee_member_node in
          return ())
        committee_members_nodes
    in
    let* _ =
      Lwt_list.iter_s
        (fun observer_node ->
          let* _ = Dac_node.init_config observer_node in
          return ())
        observer_nodes
    in
    (* 1. Run committee member and observer nodes.
       Because the event resolution loop in the Daemon always resolves
       all promises matching an event filter, when a new event is received,
       we cannot wait for multiple subscription to the hash streamer, as
       events of this kind are indistinguishable one from the other.
       Instead, we wait for the subscription of one observer/committe_member
       node to be notified before running the next node. *)
    let* () =
      Lwt_list.iter_s
        (fun node ->
          let node_is_subscribed =
            wait_for_node_subscribed_to_data_streamer ()
          in
          let* () = Dac_node.run ~wait_ready:true node in
          node_is_subscribed)
        (committee_members_nodes @ observer_nodes)
    in
    let all_nodes_have_processed_root_hash =
      Lwt.join
      @@ wait_for_root_hash_processed_promises
           (committee_members_nodes @ observer_nodes)
    in
    (* 2. Post a preimage to the coordinator. *)
    let* () =
      coordinator_serializes_payload coordinator_node ~payload ~expected_rh
    in
    (* Assert [coordinator] emitted event that [expected_rh] was pushed
       to the data_streamer. *)
    let* () = push_promise in
    (* 3. Wait until all observer and committee member nodes downloaded the
          payload. *)
    let* () = all_nodes_have_processed_root_hash in
    (* 4. Check that all pages can be retrieved by committee members
          and observers using the GET preimage endpoint.

       Note that using check_downloaded_preimage will request pages from the
       coordinator node for each observer and committee_member node.
       This might be inefficient *)
    Lwt_list.iter_s
      (fun dac_node ->
        check_downloaded_preimage coordinator_node dac_node expected_rh)
      (committee_members_nodes @ observer_nodes)

  let test_streaming_certificates
      Scenarios.
        {
          coordinator_node;
          committee_members_nodes;
          observer_nodes;
          committee_members;
          _;
        } =
    (* 0. Coordinator node is already running when the this function is
          executed by the test
       1. Client starts curl request to listen for certificate updates
       2. Run committee members and observers
       3. Client posts a preimage to coordinator
       4. Check that a number of certificates equal to the number of committee
          members are received via the streamed endpoint
       5. Fetch certificate via GET endpoint, check that returned certificate
          is equivalent to the last certificate of the streamed endpoint
       6. Request certificate via streamed endpoints again, check that one
          item is returned with the same certificate returned by the GET
          endpoint.
    *)
    (* The test requires at least one committee member *)
    assert (List.length committee_members > 0) ;
    let payload, expected_rh = sample_payload "preimage" in
    let certificate_stream_client =
      Runnable.run
      @@ Certificate_V0.streamed_certificates_client
           coordinator_node
           expected_rh
    in
    let push_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator_node expected_rh
    in
    let wait_for_node_subscribed_to_data_streamer () =
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
    in
    (* Initialize configuration of all nodes. *)
    let* _ =
      Lwt_list.iter_s
        (fun committee_member_node ->
          let* _ = Dac_node.init_config committee_member_node in
          return ())
        committee_members_nodes
    in
    let* _ =
      Lwt_list.iter_s
        (fun observer_node ->
          let* _ = Dac_node.init_config observer_node in
          return ())
        observer_nodes
    in
    (* 1. Run committee member and observer nodes.
       Because the event resolution loop in the Daemon always resolves
       all promises matching an event filter, when a new event is received,
       we cannot wait for multiple subscription to the hash streamer, as
       events of this kind are indistinguishable one from the other.
       Instead, we wait for the subscription of one observer/committe_member
       node to be notified before running the next node. *)
    let* () =
      Lwt_list.iter_s
        (fun node ->
          let node_is_subscribed =
            wait_for_node_subscribed_to_data_streamer ()
          in
          let* () = Dac_node.run ~wait_ready:true node in
          node_is_subscribed)
        (committee_members_nodes @ observer_nodes)
    in
    (* 2. Post a preimage to the coordinator. *)
    let* () =
      coordinator_serializes_payload coordinator_node ~payload ~expected_rh
    in
    (* Assert [coordinator] emitted event that [expected_rh] was pushed
       to the data_streamer. *)
    let* () = push_promise in
    (* 4. Check that a number of certificates equal to the number of committee
          members are received via the streamed endpoint. *)
    let* certificate_updates = certificate_stream_client in
    Check.(
      (List.length committee_members = List.length certificate_updates)
        int
        ~error_msg:
          "Unexpected number of streamed certificate updates (Expected = %L <> \
           %R = Actual)") ;
    let () =
      List.iter
        (fun (_, _, root_hash, _) ->
          check_valid_root_hash expected_rh root_hash)
        certificate_updates
    in
    let last_certificate_update =
      List.nth certificate_updates (List.length certificate_updates - 1)
    in
    (* 5. Check that certificate is consistent with the one returned
       from the GET /certificate endpoint. *)
    let* get_certificate =
      RPC.call
        coordinator_node
        (Dac_rpc.Certificate_V0.get_certificate
           ~hex_root_hash:(`Hex expected_rh))
    in
    check_certificate get_certificate last_certificate_update ;
    (* 6. Request certificate via streamed endpoints again, check that one
       item is returned with the same certificate returned by the GET
       endpoint. *)
    let* second_certificates_stream =
      Runnable.run
      @@ Certificate_V0.streamed_certificates_client
           coordinator_node
           expected_rh
    in
    Check.(
      (1 = List.length second_certificates_stream)
        int
        ~error_msg:
          "Unexpected number of streamed certificate updates (Expected = %L <> \
           %R = Actual)") ;
    let certificate_update_from_second_stream =
      match second_certificates_stream with
      | [certificate] -> certificate
      | _ -> assert false
    in
    check_certificate get_certificate certificate_update_from_second_stream ;
    return ()

  let certificate_encoding =
    let untagged =
      Data_encoding.(
        obj3
          (req "root_hash" (Fixed.bytes 33))
          (req "aggregate_signature" Tezos_crypto.Aggregate_signature.encoding)
          (req "witnesses" z))
    in
    Data_encoding.(
      union
        ~tag_size:`Uint8
        [
          case
            ~title:"certificate_V0"
            (Tag 0)
            untagged
            (fun certificate -> Some certificate)
            (fun certificate -> certificate);
        ])

  (* Builds a certificate for `root_hash`. `committee_members_opt` is a list
     of optional `aggregate_key` pairs. If it the i-th value of this list is
     set to `None`, then the i-th committee member won't sign the root hash in
     the certificate. Otherwise, the i-th committee member will sign the
     certificate using the secret key at the i-th position in the list.
     Only unencrypted aggregate secret keys are supported. *)
  let build_raw_certificate committee_members_opt root_hash =
    let rev_signatures, witnesses =
      List.fold_left
        (fun (rev_signatures, witnesses) committee_member_opt ->
          let witnesses = Z.(shift_left witnesses 1) in
          match committee_member_opt with
          | None -> (rev_signatures, witnesses)
          | Some committee_member -> (
              match committee_member.Account.aggregate_secret_key with
              | Encrypted _ ->
                  (* Encrypted aggregate keys are not used in dac tests. *)
                  Stdlib.failwith
                    "Unexpected encrypted aggregate key. Only unencrypted \
                     aggregate keys are supported in DAC tests"
              | Unencrypted b58_secret_key ->
                  let secret_key =
                    Tezos_crypto.Aggregate_signature.Secret_key.of_b58check_exn
                      b58_secret_key
                  in
                  let signature =
                    Tezos_crypto.Aggregate_signature.sign secret_key root_hash
                  in
                  (signature :: rev_signatures, Z.succ witnesses)))
        ([], Z.zero)
        committee_members_opt
    in
    let signatures = List.rev rev_signatures in
    let aggregate_signature =
      Tezos_crypto.Aggregate_signature.aggregate_signature_opt signatures
    in
    match aggregate_signature with
    | None ->
        Stdlib.failwith
          Format.(
            asprintf
              "Could not compute aggregate signature for %a"
              pp_print_bytes
              root_hash)
    | Some aggregate_signature ->
        Hex.of_bytes
        @@ Data_encoding.Binary.to_bytes_exn
             certificate_encoding
             (root_hash, aggregate_signature, witnesses)

  let check_raw_certificate (`Hex actual_raw_certificate)
      (`Hex expected_raw_certificate) =
    Check.(
      (actual_raw_certificate = expected_raw_certificate)
        string
        ~error_msg:
          "Raw certificate does not match the expected one: Actual = %L <> %R \
           = Expected")

  let test_client
      Scenarios.
        {
          coordinator_node;
          committee_members_nodes;
          observer_nodes;
          committee_members;
          _;
        } =
    (* 0. Coordinator node is already running when the this function is
          executed by the test
       1. Run committee members and observers
       2. Dac client posts a preimage to coordinator, and waits for all
          committee members to sign
       3. The signature is checked against one constructed manually
       4. The signature of the root hash is requested again via the
          client `get certificate` command,
       5. The returned signature is checked to be equvalent to the
          one previously output by the client.
    *)
    (* The test requires at least one committee member *)
    assert (List.length committee_members > 0) ;
    let coordinator_client = Dac_client.create coordinator_node in
    let payload, expected_rh = sample_payload "preimage" in
    let committee_members_opt =
      List.map (fun committee_member -> Some committee_member) committee_members
    in
    let expected_certificate =
      build_raw_certificate
        committee_members_opt
        (Hex.to_bytes (`Hex expected_rh))
    in
    let wait_for_node_subscribed_to_data_streamer () =
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
    in
    (* Initialize configuration of all nodes. *)
    let* _ =
      Lwt_list.iter_s
        (fun committee_member_node ->
          let* _ = Dac_node.init_config committee_member_node in
          return ())
        committee_members_nodes
    in
    let* _ =
      Lwt_list.iter_s
        (fun observer_node ->
          let* _ = Dac_node.init_config observer_node in
          return ())
        observer_nodes
    in
    (* 1. Run committee member and observer nodes.
       Because the event resolution loop in the Daemon always resolves
       all promises matching an event filter, when a new event is received,
       we cannot wait for multiple subscription to the hash streamer, as
       events of this kind are indistinguishable one from the other.
       Instead, we wait for the subscription of one observer/committe_member
       node to be notified before running the next node. *)
    let* () =
      Lwt_list.iter_s
        (fun node ->
          let node_is_subscribed =
            wait_for_node_subscribed_to_data_streamer ()
          in
          let* () = Dac_node.run ~wait_ready:true node in
          node_is_subscribed)
        (committee_members_nodes @ observer_nodes)
    in
    (* 2. Dac client posts a preimage to coordinator, and waits for all
          committee members to sign. *)
    let* client_sends_payload_output =
      Dac_client.send_payload
        coordinator_client
        (Hex.of_string payload)
        ~threshold:(List.length committee_members)
    in
    let last_certificate_update =
      match client_sends_payload_output with
      | Root_hash rh ->
          Stdlib.failwith @@ "Expected certificate, found root hash"
          ^ Hex.show rh
      | Certificate c -> c
    in
    (* 3. The signature is checked against one constructed manually. *)
    check_raw_certificate last_certificate_update expected_certificate ;
    (* 4. The signature of the root hash is requested again via the
          client `get certificate` command.v*)
    let* get_certificate_output =
      Dac_client.get_certificate coordinator_client (`Hex expected_rh)
    in
    let get_certificate =
      match get_certificate_output with
      | None -> Stdlib.failwith @@ "Expected certificate, found none"
      | Some (Certificate c) -> c
      | _ ->
          (* This case cannot happen as get_certificate_output can never have
             value `Some (Root_hash _ )`. *)
          assert false
    in
    (* 5. The returned signature is checked to be equvalent to the
          one previously output by the client. *)
    check_raw_certificate get_certificate last_certificate_update ;
    return ()
end

(* Modified from tezt/tests/tx_sc_rollup.ml *)
module Tx_kernel_e2e = struct
  open Sc_rollup_helpers
  module Bls = Tezos_crypto.Signature.Bls

  let send_message ?(src = Constant.bootstrap2.alias) client msg =
    let* () = Client.Sc_rollup.send_message ~hooks ~src ~msg client in
    Client.bake_for_and_wait client

  (* TX Kernel external messages and their encodings *)
  module Tx_kernel = struct
    open Tezos_protocol_alpha.Protocol
    open Tezos_crypto.Signature

    type ticket = {
      ticketer : Alpha_context.Contract.t;
      content : string;
      amount : int;
    }

    let ticket_of ~ticketer ~content amount =
      {
        ticketer = Result.get_ok (Alpha_context.Contract.of_b58check ticketer);
        content;
        amount;
      }

    (* Primitive operation of tx kernel.
       Several primitive operations might be outgoing from the same account.
       Corresponds to kernel_core::inbox::external::v1::OperationContent
       type in the tx kernel. *)
    type operation =
      | Withdrawal of {
          receiver_contract : Contract_hash.t;
          ticket : ticket;
          entrypoint : string;
        }
      | Transfer of {
          (* tz4 address *)
          destination : Tezos_crypto.Signature.Bls.Public_key_hash.t;
          ticket : ticket;
        }

    (* List of elemtary operations which are outgoing from the same account.
       Corresponds to a pair of
         * kernel_core::bls::BlsKey and
         * kernel_core::inbox::external::v1::verifiable::VerifiableOperation
       in the tx kernel.
       VerifiableOperation::signer is replaced by its private key
       in order to be able to sign outer [multiaccount_tx].
       In terms of the tx kernel it is rather *sending* type not *verifiable*.
    *)
    type account_operations = {
      signer : Bls.Secret_key.t;
      counter : int64;
      operations : operation list;
    }

    let account_operations_of ~sk ~counter operations =
      {signer = sk; counter; operations}

    (* Several account operations.
       Content of this tx signed by each account,
       and stored in the aggregated signature.
       Corresponds to
          kernel_core::inbox::external::v1::verifiable::VerifiableTransaction type
       in the tx kernel
    *)
    type multiaccount_tx = {
      accounts_operations : account_operations list;
      encoded_accounts_ops : string; (* just encoded concatenated list above *)
      aggregated_signature : Bls.t;
    }

    (* Batch of multiaccount transactions.
       Corresponds to
         kernel_core::inbox::external::v1::ParsedBatch type
       in the tx kernel
    *)
    type transactions_batch = {
      transactions : multiaccount_tx list;
      encoded_transactions : string; (* just encoded concatenated list above *)
      aggregated_signature : Bls.t;
    }

    module Encodings = struct
      let list_encode xs =
        Data_encoding.(Binary.to_string_exn string @@ String.concat "" xs)

      (* String ticket encoding for tx kernel.
         Corresponds to kernel_core::encoding::string_ticket::StringTicketRepr *)
      let ticket_repr {ticketer; content; amount} : string =
        let open Tezos_protocol_alpha.Protocol.Alpha_context in
        Printf.sprintf
          "\007\007\n\000\000\000\022%s\007\007\001%s\000%s"
          Data_encoding.(Binary.to_string_exn Contract.encoding ticketer)
          Data_encoding.(Binary.to_string_exn bytes @@ Bytes.of_string content)
          Data_encoding.(Binary.to_string_exn z @@ Z.of_int amount)

      (* Encoding of kernel_core::inbox::external::v1::OperationContent from tx_kernel *)
      let operation_repr = function
        | Withdrawal {receiver_contract; ticket; entrypoint} ->
            let open Alpha_context in
            let withdrawal_prefix = "\000" in
            let contract_bytes =
              Data_encoding.(
                Binary.to_string_exn Contract_hash.encoding receiver_contract)
            in
            let entrypoint_bytes =
              Data_encoding.(
                Entrypoint.of_string_strict_exn entrypoint
                |> Binary.to_string_exn Entrypoint.simple_encoding)
            in
            withdrawal_prefix ^ contract_bytes ^ ticket_repr ticket
            ^ entrypoint_bytes
        | Transfer {destination; ticket} ->
            let transfer_prefix = "\001" in
            let tz4address =
              Data_encoding.(
                Binary.to_string_exn
                  Tezos_crypto.Signature.Bls.Public_key_hash.encoding
                  destination)
            in
            transfer_prefix ^ tz4address ^ ticket_repr ticket

      let account_operations_repr {signer; counter; operations} : string =
        let signer_bytes =
          if Int64.equal counter 0L then
            "\000" (* PK signer tag *)
            ^ Data_encoding.(
                Bls.Secret_key.to_public_key signer
                |> Binary.to_string_exn Bls.Public_key.encoding)
          else
            "\001" (* tz4address signer tag *)
            ^ Data_encoding.(
                Bls.Secret_key.to_public_key signer
                |> Bls.Public_key.hash
                |> Binary.to_string_exn Bls.Public_key_hash.encoding)
        in
        let counter = Data_encoding.(Binary.to_string_exn int64 counter) in
        let contents = list_encode @@ List.map operation_repr operations in
        signer_bytes ^ counter ^ contents

      let list_of_account_operations_repr
          (accounts_operations : account_operations list) : string =
        let account_ops_encoded =
          List.map account_operations_repr accounts_operations
        in
        list_encode account_ops_encoded

      let list_of_multiaccount_tx_encoding (transactions : multiaccount_tx list)
          =
        let txs_encodings =
          List.map (fun x -> x.encoded_accounts_ops) transactions
        in
        list_encode txs_encodings
    end

    let multiaccount_tx_of (accounts_operations : account_operations list) =
      let encoded_accounts_ops =
        Encodings.list_of_account_operations_repr accounts_operations
      in
      let accounts_sks = List.map (fun x -> x.signer) accounts_operations in
      (* List consisting of single transaction, that is fine *)
      let aggregated_signature =
        Option.get
        @@ Bls.(
             aggregate_signature_opt
             @@ List.map
                  (fun sk -> sign sk @@ Bytes.of_string encoded_accounts_ops)
                  accounts_sks)
      in
      assert (
        Bls.aggregate_check
          (List.map
             (fun sk ->
               ( Bls.Secret_key.to_public_key sk,
                 None,
                 Bytes.of_string encoded_accounts_ops ))
             accounts_sks)
          aggregated_signature) ;
      {accounts_operations; encoded_accounts_ops; aggregated_signature}

    let transactions_batch_of (transactions : multiaccount_tx list) =
      let encoded_transactions =
        Encodings.list_of_multiaccount_tx_encoding transactions
      in
      let signatures =
        List.map
          (fun (x : multiaccount_tx) -> x.aggregated_signature)
          transactions
      in
      let aggregated_signature =
        Option.get @@ Bls.aggregate_signature_opt signatures
      in
      {transactions; encoded_transactions; aggregated_signature}

    let external_message_of_batch ?(should_hex_encode = true)
        (batch : transactions_batch) =
      let v1_batch_prefix = "\000" in
      let signature =
        batch.aggregated_signature |> Tezos_crypto.Signature.Bls.to_bytes
        |> Bytes.to_string
      in
      let raw = v1_batch_prefix ^ batch.encoded_transactions ^ signature in
      if should_hex_encode then hex_encode raw else raw

    (* External message consisting of single transaction. *)
    let external_message_of_account_ops (accounts_ops : account_operations list)
        =
      external_message_of_batch @@ transactions_batch_of
      @@ [multiaccount_tx_of accounts_ops]
  end

  let assert_state_changed ?block sc_rollup_client prev_state_hash =
    let*! state_hash =
      Sc_rollup_client.state_hash ?block ~hooks sc_rollup_client
    in
    Check.(state_hash <> prev_state_hash)
      Check.string
      ~error_msg:"State hash has not changed (%L <> %R)" ;
    Lwt.return_unit

  let assert_ticks_advanced ?block sc_rollup_client prev_ticks =
    let*! ticks = Sc_rollup_client.total_ticks ?block ~hooks sc_rollup_client in
    Check.(ticks > prev_ticks)
      Check.int
      ~error_msg:"Tick counter did not advance (%L > %R)" ;
    Lwt.return_unit

  (* Send a deposit into the rollup. *)
  let test_deposit ~client ~sc_rollup_node ~sc_rollup_client ~sc_rollup_address
      ~mint_and_deposit_contract level tz4_address =
    let*! prev_state_hash =
      Sc_rollup_client.state_hash ~hooks sc_rollup_client
    in
    let* () =
      (* Internal message through forwarder *)
      let arg =
        sf
          {|Pair (Pair %S "%s") (Pair 450 "Hello, Ticket!")|}
          sc_rollup_address
          tz4_address
      in
      Client.transfer
        client
        ~hooks
        ~amount:Tez.zero
        ~giver:Constant.bootstrap1.alias
        ~receiver:mint_and_deposit_contract
        ~arg
        ~burn_cap:(Tez.of_int 1000)
    in
    let* () = Client.bake_for_and_wait client in
    let* _ =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node (level + 1)
    in
    let* () = assert_state_changed sc_rollup_client prev_state_hash in
    Lwt.return @@ (level + 1)

  let rec bake_until cond client sc_rollup_node =
    let* stop = cond client in
    if stop then unit
    else
      let* () = Client.bake_for_and_wait client in
      let* current_level = Client.level client in
      let* _ =
        Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node current_level
      in
      bake_until cond client sc_rollup_node

  let ticket mint_and_deposit_contract amount =
    Tx_kernel.ticket_of
      ~ticketer:mint_and_deposit_contract
      ~content:"Hello, Ticket!"
      amount

  type prepare_message_setup = {
    sk1 : Bls.Secret_key.t;
    sk2 : Bls.Secret_key.t;
    pkh1 : Bls.Public_key_hash.t;
    pkh2 : Bls.Public_key_hash.t;
    transfer_message : string;
    withdraw_message : string;
    mint_and_deposit_contract : string;
    receive_tickets_contract : string;
    level : int;
  }

  type send_message_and_wait_setup = {
    prev_state_hash : string;
    prev_ticks : int;
    level : int;
  }

  let prepare_contracts_and_messages
      ?(transfer_message_should_hex_encode = true) ~client ~level protocol =
    let pkh1, _pk, sk1 = Tezos_crypto.Signature.Bls.generate_key () in
    let pkh2, _pk2, sk2 = Tezos_crypto.Signature.Bls.generate_key () in

    (* Originate a contract that will mint and transfer tickets to the tx kernel. *)
    (* Originate forwarder contract to send internal messages to rollup. *)
    let* _, mint_and_deposit_contract =
      Client.originate_contract_at (* ~alias:"rollup_deposit" *)
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~init:"Unit"
        ~burn_cap:Tez.(of_int 1)
        client
        ["mini_scenarios"; "smart_rollup_mint_and_deposit_ticket"]
        protocol
    in
    let* () = Client.bake_for_and_wait client in
    Log.info
      "The mint and deposit contract %s was successfully originated"
      mint_and_deposit_contract ;
    let level = level + 1 in

    (* originate ticket receiver contract that will receive withdrawls..*)
    let* _, receive_tickets_contract =
      Client.originate_contract_at
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~init:"{}"
        ~burn_cap:Tez.(of_int 1)
        client
        ["mini_scenarios"; "smart_rollup_receive_tickets"]
        protocol
    in
    let* () = Client.bake_for_and_wait client in
    Log.info
      "The receiver contract %s was successfully originated"
      receive_tickets_contract ;
    let level = level + 1 in

    (* Construct transfer message to send to rollup. *)
    let transfer_message =
      Tx_kernel.(
        [
          multiaccount_tx_of
            [
              (* Transfer 50 tickets *)
              account_operations_of
                ~sk:sk1
                ~counter:0L
                [
                  Transfer
                    {
                      destination = pkh2;
                      ticket = ticket mint_and_deposit_contract 60;
                    };
                ];
              (* Transfer 10 tickets back *)
              account_operations_of
                ~sk:sk2
                ~counter:0L
                [
                  Transfer
                    {
                      destination = pkh1;
                      ticket = ticket mint_and_deposit_contract 10;
                    };
                ];
            ];
          multiaccount_tx_of
            [
              (* Transfer another 10 tickets back but in a separate tx *)
              account_operations_of
                ~sk:sk2
                ~counter:1L
                [
                  Transfer
                    {
                      destination = pkh1;
                      ticket = ticket mint_and_deposit_contract 10;
                    };
                ];
            ];
        ]
        |> transactions_batch_of
        |> external_message_of_batch
             ~should_hex_encode:transfer_message_should_hex_encode)
    in
    (* Construct withdrawal *)
    let withdrawal_op amount =
      Tx_kernel.Withdrawal
        {
          receiver_contract =
            Tezos_protocol_alpha.Protocol.Contract_hash.of_b58check_exn
              receive_tickets_contract;
          ticket = ticket mint_and_deposit_contract amount;
          entrypoint = "receive_tickets";
        }
    in

    (* Construct withdrawal mesage to send to rollup. *)
    (* pk withdraws part of his tickets, pk2 withdraws all of his tickets *)
    let withdraw_message =
      Tx_kernel.(
        [
          account_operations_of
            ~sk:sk1
            ~counter:1L
            [withdrawal_op 220; withdrawal_op 100];
          account_operations_of ~sk:sk2 ~counter:2L [withdrawal_op 40];
        ]
        |> external_message_of_account_ops)
    in
    return
      {
        sk1;
        sk2;
        pkh1;
        pkh2;
        transfer_message;
        withdraw_message;
        mint_and_deposit_contract;
        receive_tickets_contract;
        level;
      }

  let send_message_and_wait_for_level ~level ~sc_rollup_client ~sc_rollup_node
      ~client ~hooks hex_encoded_message =
    let*! prev_state_hash =
      Sc_rollup_client.state_hash ~hooks sc_rollup_client
    in
    let*! prev_ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
    let* () = send_message client (sf "hex:[%S]" hex_encoded_message) in
    let level = level + 1 in
    let* _ = Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level in
    let* () = assert_state_changed sc_rollup_client prev_state_hash in
    return {prev_state_hash; prev_ticks; level}

  let verify_outbox_answer ~withdrawal_level ~sc_rollup_client
      ~sc_rollup_address ~client ~receive_tickets_contract
      ~mint_and_deposit_contract =
    let*! outbox =
      Sc_rollup_client.outbox ~outbox_level:withdrawal_level sc_rollup_client
    in
    Log.info "Outbox is %s" @@ JSON.encode outbox ;
    let* answer =
      let message_index = 0 in
      let outbox_level = withdrawal_level in
      let destination = receive_tickets_contract in
      let open Tezos_protocol_alpha.Protocol.Alpha_context in
      let ticketer =
        mint_and_deposit_contract |> Contract.of_b58check |> Result.get_ok
        |> Data_encoding.(Binary.to_string_exn Contract.encoding)
        |> hex_encode
      in
      let parameters d =
        Printf.sprintf
          {|Pair 0x%s (Pair "%s" %s)|}
          ticketer
          "Hello, Ticket!"
          (Int.to_string d)
      in
      let outbox_transaction param =
        Sc_rollup_client.
          {
            destination;
            entrypoint = Some "receive_tickets";
            parameters = parameters param;
            parameters_ty = None;
          }
      in
      Sc_rollup_client.outbox_proof_batch
        sc_rollup_client
        ~message_index
        ~outbox_level
        (List.map outbox_transaction [220; 100; 40])
    in
    match answer with
    | Some {commitment_hash; proof} ->
        let*! () =
          Client.Sc_rollup.execute_outbox_message
            ~hooks
            ~burn_cap:(Tez.of_int 10)
            ~rollup:sc_rollup_address
            ~src:Constant.bootstrap1.alias
            ~commitment_hash
            ~proof
            client
        in
        Client.bake_for_and_wait client
    | _ -> failwith "Unexpected error during proof generation"

  (* [prepare_dac_external_message ~coordinator_node ~committee_members_nodes payload] registers a
     [payload] with [coordinator_node] then waits for all [committee_member_nodes] to sign the resultant
     root hash. The associated DAC certificate is retrieved, the l1 external message is constructed,
     hex encoded and returned.
  *)
  let prepare_dac_external_message ~coordinator_node ~committee_members_nodes
      ?(should_run_committee_member_nodes = true) payload =
    (* monitor event emission on dac member*)
    let wait_for_signature_pushed_event dac_member =
      Dac_node.wait_for
        dac_member
        "new_signature_pushed_to_coordinator.v0"
        (fun _ -> Some ())
    in
    let wait_for_member_signature_pushed_to_coordinator =
      List.map wait_for_signature_pushed_event committee_members_nodes
    in
    let* () =
      if should_run_committee_member_nodes then
        Lwt_list.iter_s
          (fun dac_member ->
            let* _dir = Dac_node.init_config dac_member in
            let ev =
              wait_for_handle_new_subscription_to_hash_streamer coordinator_node
            in
            let* () = Dac_node.run dac_member in
            ev)
          committee_members_nodes
      else unit
    in
    let* preimage_hash =
      RPC.call coordinator_node (Dac_rpc.Coordinator.post_preimage ~payload)
    in
    let* () = Lwt.join wait_for_member_signature_pushed_to_coordinator in
    let* witnesses, signature, root_hash =
      RPC.call
        coordinator_node
        (Dac_rpc.get_certificate ~hex_root_hash:(`Hex preimage_hash))
    in
    let root_hash = `Hex root_hash |> Hex.to_string |> String.to_bytes in
    let signature =
      Data_encoding.Binary.to_bytes_exn
        Tezos_crypto.Aggregate_signature.encoding
      @@ Tezos_crypto.Aggregate_signature.of_b58check_exn signature
    in
    let witnesses =
      Data_encoding.Binary.to_bytes_exn Data_encoding.z (Z.of_int witnesses)
    in
    let dac_certificate_bin =
      Bytes.to_string
      @@ Bytes.concat Bytes.empty [root_hash; signature; witnesses]
    in
    let l1_external_message = hex_encode ("\042" ^ dac_certificate_bin) in
    return @@ (`Hex l1_external_message, `Hex preimage_hash)

  (* Test scenario where DAC Observer is in sync with the Data Availability Committee - Ideal case.
     When a DAC ceritifcate is created, the Observer should download pages from the Coordinator and
     persist them to the rollup node's data directory. On reveal_hash, the pages should already be
     present.

     The Tx-kernel outbox is verified to test the correctness of DAC infrastructure.
     This test has 1 cordinator, 1 committee member, 1 rollup node with 1 observer running.
  *)
  let test_tx_kernel_e2e_with_dac_observer_synced_with_dac commitment_period
      scenario =
    let Dac_helper.Scenarios.
          {
            protocol;
            client;
            key;
            coordinator_node;
            committee_members_nodes;
            observer_nodes;
            rollup_nodes;
            _;
          } =
      scenario
    in
    let sc_rollup_node = List.nth rollup_nodes 0 in
    let observer_node = List.nth observer_nodes 0 in
    let* _ = Dac_node.init_config observer_node in
    let* () = Dac_node.run observer_node in
    let* boot_sector =
      prepare_installer_kernel
        ~preimages_dir:
          (Filename.concat
             (Sc_rollup_node.data_dir sc_rollup_node)
             "wasm_2_0_0")
        "tx-kernel-fixed-dac"
    in
    let* sc_rollup_address =
      Client.Sc_rollup.originate
        ~burn_cap:Tez.(of_int 9999999)
        ~alias:"tx_kernel_fixed_dac_rollup"
        ~src:key
        ~kind:"wasm_2_0_0"
        ~boot_sector
        ~parameters_ty:"pair string (ticket string)"
        client
    in
    let* () = Client.bake_for_and_wait client in
    (* Run the rollup node, ensure origination succeeds. *)
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
           sc_rollup_address
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in
    let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
    let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
    let* level =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node init_level
    in
    Check.(level = init_level)
      Check.int
      ~error_msg:"Current level has moved past origination level (%L = %R)" ;
    let* {
           pkh1;
           transfer_message;
           withdraw_message;
           mint_and_deposit_contract;
           receive_tickets_contract;
           level;
           _;
         } =
      prepare_contracts_and_messages
        ~client
        ~level
        ~transfer_message_should_hex_encode:false
        protocol
    in

    (* Deposit *)
    let* level =
      test_deposit
        ~client
        ~sc_rollup_node
        ~sc_rollup_client
        ~sc_rollup_address
        ~mint_and_deposit_contract
        level
      @@ Tezos_crypto.Signature.Bls.Public_key_hash.to_b58check pkh1
    in

    let payload = transfer_message in

    (* Register root hash with coordinator.
       Retrieve certificate after committee members have signed.
       Prepare L1 Dac message hex encoded.
    *)
    (* Tx kernel dac messages expects the root hash page to be a content page containing
       a list of hashes that references the actually payload's merkle tree root hash.
       Rollup id in tx kernel is hard-coded to `0` to select the list head. *)
    let* _, hexed_preimage_hash =
      prepare_dac_external_message
        ~coordinator_node
        ~committee_members_nodes
        payload
    in
    let* `Hex l1_external_message, _ =
      prepare_dac_external_message
        ~coordinator_node
        ~committee_members_nodes
        ~should_run_committee_member_nodes:false
        (Hex.to_string hexed_preimage_hash)
    in

    (* Send DAC certificate as an External Message to L1 *)
    let* {level; _} =
      send_message_and_wait_for_level
        ~level
        ~sc_rollup_client
        ~sc_rollup_node
        ~client
        ~hooks
        l1_external_message
    in

    (* After that pkh1 has 410 tickets, pkh2 has 40 tickets *)
    (* Send withdrawal *)
    let* {prev_state_hash; prev_ticks; level = withdrawal_level} =
      send_message_and_wait_for_level
        ~level
        ~sc_rollup_client
        ~sc_rollup_node
        ~client
        ~hooks
        withdraw_message
    in
    let* _, last_lcc_level =
      Sc_rollup_helpers.last_cemented_commitment_hash_with_level
        ~sc_rollup:sc_rollup_address
        client
    in
    let next_lcc_level = last_lcc_level + commitment_period in

    (* Bake until the next commitment is cemented, this commitment
       will include the withdrawal. *)
    let* () =
      bake_until
        (fun client ->
          let* _, lcc_level =
            Sc_rollup_helpers.last_cemented_commitment_hash_with_level
              ~sc_rollup:sc_rollup_address
              client
          in
          return (lcc_level = next_lcc_level))
        client
        sc_rollup_node
    in

    let block = string_of_int next_lcc_level in
    let* () = assert_state_changed ~block sc_rollup_client prev_state_hash in
    let* () = assert_ticks_advanced ~block sc_rollup_client prev_ticks in

    (* EXECUTE withdrawal *)
    let* () =
      verify_outbox_answer
        ~client
        ~sc_rollup_client
        ~sc_rollup_address
        ~receive_tickets_contract
        ~mint_and_deposit_contract
        ~withdrawal_level
    in
    unit

  (* Test scenario where DAC Observer joins halfway and hence is cold/has missing pages. In this scenario,
     rollup node makes an RPC call to Observer via GET /missing_page/{hash}. The Observer should retrieve
     the missing page then store it to the rollup node's data directory. Since the rollup node starts cold,
     this should happen for every page in the root hash's Merkle tree.

     The Tx-kernel outbox is verified to test the correctness of DAC infrastructure.
     This test has 1 cordinator, 1 committee member, 1 rollup node with 1 observer running.
  *)
  let test_tx_kernel_e2e_with_dac_observer_missing_pages commitment_period
      scenario =
    let Dac_helper.Scenarios.
          {
            protocol;
            client;
            key;
            coordinator_node;
            committee_members_nodes;
            observer_nodes;
            rollup_nodes;
            _;
          } =
      scenario
    in
    let sc_rollup_node = List.nth rollup_nodes 0 in
    let* boot_sector =
      prepare_installer_kernel
        ~preimages_dir:
          (Filename.concat
             (Sc_rollup_node.data_dir sc_rollup_node)
             "wasm_2_0_0")
        "tx-kernel-fixed-dac"
    in
    let* sc_rollup_address =
      Client.Sc_rollup.originate
        ~burn_cap:Tez.(of_int 9999999)
        ~alias:"tx_kernel_fixed_dac_rollup"
        ~src:key
        ~kind:"wasm_2_0_0"
        ~boot_sector
        ~parameters_ty:"pair string (ticket string)"
        client
    in
    let* () = Client.bake_for_and_wait client in

    (* Run the rollup node, ensure origination succeeds. *)
    let* genesis_info =
      RPC.Client.call ~hooks client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
           sc_rollup_address
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in
    let observer_node = List.nth observer_nodes 0 in
    let* _ = Dac_node.init_config observer_node in
    let* () =
      Sc_rollup_node.run
        sc_rollup_node
        sc_rollup_address
        ["--dac-observer"; Dac_node.endpoint observer_node]
    in
    let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
    let* level =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node init_level
    in
    Check.(level = init_level)
      Check.int
      ~error_msg:"Current level has moved past origination level (%L = %R)" ;
    let* {
           pkh1;
           transfer_message;
           withdraw_message;
           mint_and_deposit_contract;
           receive_tickets_contract;
           level;
           _;
         } =
      prepare_contracts_and_messages
        ~client
        ~level
        ~transfer_message_should_hex_encode:false
        protocol
    in

    (* Deposit *)
    let* level =
      test_deposit
        ~client
        ~sc_rollup_node
        ~sc_rollup_client
        ~sc_rollup_address
        ~mint_and_deposit_contract
        level
      @@ Tezos_crypto.Signature.Bls.Public_key_hash.to_b58check pkh1
    in
    let payload = transfer_message in

    (* Register root hash with coordinator.
       Retrieve certificate after committee members have signed.
       Prepare L1 Dac message hex encoded.
    *)
    (* Tx kernel dac messages expects the root hash page to be a content page containing
       a list of hashes that references the actually payload's merkle tree root hash.
       Rollup id in tx kernel is hard-coded to `0` to select the list head. *)
    let* _, transfer_messages_preimage_hash =
      prepare_dac_external_message
        ~coordinator_node
        ~committee_members_nodes
        payload
    in
    let* `Hex l1_external_message, hexed_perimage_hash =
      prepare_dac_external_message
        ~coordinator_node
        ~committee_members_nodes
        ~should_run_committee_member_nodes:false
        (Hex.to_string transfer_messages_preimage_hash)
    in

    (* Delay Observer start up to simulate it missing coordinator's streamed
       root hash. *)
    let* filenames = Dac_node.ls_reveal_data_dir observer_node in
    Check.(
      (false
      = List.exists
          (String.equal (Hex.show transfer_messages_preimage_hash))
          filenames)
        ~__LOC__
        bool
        ~error_msg:
          "Observer node is in invalid state: Unexpected preimage hash found \
           in reveal data dir.") ;

    Check.(
      (false
      = List.exists (String.equal (Hex.show hexed_perimage_hash)) filenames)
        ~__LOC__
        bool
        ~error_msg:
          "Observer node is in invalid state: Unexpected preimage hash found \
           in reveal data dir.") ;
    let* () = Dac_node.run ~wait_ready:true observer_node in

    (* Send DAC certificate as an External Message to L1 *)
    let* {level; _} =
      send_message_and_wait_for_level
        ~level
        ~sc_rollup_client
        ~sc_rollup_node
        ~client
        ~hooks
        l1_external_message
    in

    (* After that pkh1 has 410 tickets, pkh2 has 40 tickets *)
    (* Send withdrawal *)
    let* {prev_state_hash; prev_ticks; level = withdrawal_level} =
      send_message_and_wait_for_level
        ~level
        ~sc_rollup_client
        ~sc_rollup_node
        ~client
        ~hooks
        withdraw_message
    in
    let* _, last_lcc_level =
      Sc_rollup_helpers.last_cemented_commitment_hash_with_level
        ~sc_rollup:sc_rollup_address
        client
    in
    let next_lcc_level = last_lcc_level + commitment_period in

    (* Bake until the next commitment is cemented, this commitment
       will include the withdrawal. *)
    let* () =
      bake_until
        (fun client ->
          let* _, lcc_level =
            Sc_rollup_helpers.last_cemented_commitment_hash_with_level
              ~sc_rollup:sc_rollup_address
              client
          in
          return (lcc_level = next_lcc_level))
        client
        sc_rollup_node
    in
    let block = string_of_int next_lcc_level in
    let* () = assert_state_changed ~block sc_rollup_client prev_state_hash in
    let* () = assert_ticks_advanced ~block sc_rollup_client prev_ticks in

    (* EXECUTE withdrawal *)
    let* () =
      verify_outbox_answer
        ~client
        ~sc_rollup_client
        ~sc_rollup_address
        ~receive_tickets_contract
        ~mint_and_deposit_contract
        ~withdrawal_level
    in
    unit

  let test_tx_kernel_e2e_with_dac_observer_synced_with_dac =
    let commitment_period = 10 in
    let challenge_window = 10 in
    let custom_committee_members = [Constant.aggregate_tz4_account] in
    Dac_helper.scenario_with_full_dac_infrastructure
      ~__FILE__
      ~tags:["wasm"; "kernel"; "wasm_2_0_0"; "kernel_e2e"; "dac"; "full"]
      ~pvm_name:"wasm_2_0_0"
      ~committee_size:0
      ~observers:1
      ~custom_committee_members
      ~commitment_period
      ~challenge_window
      "kernel_e2e.dac_observer_synced_with_dac"
      (test_tx_kernel_e2e_with_dac_observer_synced_with_dac commitment_period)

  let test_tx_kernel_e2e_with_dac_observer_missing_pages =
    let commitment_period = 10 in
    let challenge_window = 10 in
    let custom_committee_members = [Constant.aggregate_tz4_account] in
    Dac_helper.scenario_with_full_dac_infrastructure
      ~__FILE__
      ~tags:["wasm"; "kernel"; "wasm_2_0_0"; "kernel_e2e"; "dac"; "full"]
      ~pvm_name:"wasm_2_0_0"
      ~committee_size:0
      ~observers:1
      ~custom_committee_members
      ~commitment_period
      ~challenge_window
      "kernel_e2e.dac_observer_missing_pages"
      (test_tx_kernel_e2e_with_dac_observer_missing_pages commitment_period)
end

let register ~protocols =
  (* Tests with layer1 and dac nodes *)
  Legacy.test_dac_node_startup protocols ;
  Legacy.test_dac_node_imports_committee_members protocols ;
  Legacy.test_dac_node_dac_threshold_not_reached protocols ;
  scenario_with_layer1_legacy_and_rollup_nodes
    ~__FILE__
    ~tags:["dac"; "dac_node"]
    "dac_reveals_data_merkle_tree_v0"
    Legacy.test_dac_node_handles_dac_store_preimage_merkle_V0
    protocols
    ~threshold:1
    ~committee_size:1 ;
  scenario_with_layer1_legacy_and_rollup_nodes
    ~__FILE__
    ~tags:["dac"; "dac_node"]
    "dac_reveals_data_hash_chain_v0"
    Legacy.test_dac_node_handles_dac_store_preimage_hash_chain_V0
    protocols
    ~threshold:1
    ~committee_size:1 ;
  scenario_with_layer1_legacy_and_rollup_nodes
    ~__FILE__
    ~tags:["dac"; "dac_node"]
    ~threshold:0
    ~committee_size:0
    "dac_retrieve_preimage"
    Legacy.test_dac_node_handles_dac_retrieve_preimage_merkle_V0
    protocols ;
  scenario_with_layer1_legacy_and_rollup_nodes
    ~__FILE__
    ~tags:["dac"; "dac_node"]
    "dac_rollup_arith_uses_reveals"
    Legacy.test_rollup_arith_uses_reveals
    protocols
    ~threshold:1
    ~committee_size:1 ;
  scenario_with_layer1_legacy_and_rollup_nodes
    ~__FILE__
    ~tags:["dac"; "dac_node"]
    "dac_rollup_arith_wrong_hash"
    Legacy.test_reveals_fails_on_wrong_hash
    ~threshold:1
    ~committee_size:1
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~__FILE__
    ~threshold:0
    ~committee_size:0
    ~tags:["dac"; "dac_node"]
    "dac_streaming_of_root_hashes_in_legacy_mode"
    Legacy.test_streaming_of_root_hashes_as_observer
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~__FILE__
    ~threshold:0
    ~committee_size:1
    ~tags:["dac"; "dac_node"]
    "dac_push_signature_in_legacy_mode_as_member"
    Legacy.test_streaming_of_root_hashes_as_member
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~__FILE__
    ~threshold:0
    ~committee_size:0
    ~tags:["dac"; "dac_node"]
    "committee member downloads pages from coordinator"
    Legacy.test_observer_downloads_pages
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~__FILE__
    ~threshold:0
    ~committee_size:2
    ~tags:["dac"; "dac_node"]
    "dac_get_certificate"
    Legacy.Signature_manager.test_get_certificate
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~__FILE__
    ~threshold:0
    ~committee_size:3
    ~tags:["dac"; "dac_node"]
    "dac_store_member_signature"
    Legacy.Signature_manager.test_handle_store_signature
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:0
    ~tags:["dac"; "dac_node"]
    "dac_coordinator_post_preimage_endpoint"
    Full_infrastructure.test_coordinator_post_preimage_endpoint
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~__FILE__
    ~threshold:0
    ~committee_size:1
    ~tags:["dac"; "dac_node"]
    "dac_observer_get_missing_page"
    Legacy.test_observer_get_missing_page
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:1
    ~committee_size:1
    ~tags:["dac"; "dac_node"]
    "committee members and observers download pages from coordinator"
    Full_infrastructure.test_download_and_retrieval_of_pages
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"; "dac_node"]
    "certificates are updated in streaming endpoint"
    Full_infrastructure.test_streaming_certificates
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"; "dac_node"]
    "test client commands"
    Full_infrastructure.test_client
    protocols ;
  Tx_kernel_e2e.test_tx_kernel_e2e_with_dac_observer_synced_with_dac protocols ;
  Tx_kernel_e2e.test_tx_kernel_e2e_with_dac_observer_missing_pages protocols
