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
  let* root_hash =
    Dac_helper.Call_endpoint.V0.Coordinator.post_preimage
      coordinator_node
      ~payload
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

let parse_certificate json =
  JSON.
    ( json |-> "witnesses" |> as_int,
      json |-> "aggregate_signature" |> as_string,
      json |-> "root_hash" |> as_string,
      json |-> "version" |> as_int )

(* Helper process that listens to certificate updates through a
   RPC request. Upon termination, the list of certificate updates
   is returned *)
let streamed_certificates_client coordinator_node root_hash =
  let endpoint =
    Format.sprintf
      "http://%s:%d/%s/monitor/certificate/%s"
      (Dac_node.rpc_host coordinator_node)
      (Dac_node.rpc_port coordinator_node)
      Dac_rpc.V0.api_prefix
      root_hash
  in
  Curl.get_raw endpoint
  |> Runnable.map (fun output ->
         let as_list = String.split_on_char '\n' output in
         (* Each JSON item in the response of the curl request is
            suffixed with the '\n' character, which will cause an
            empty item to be inserted at the end of the list. *)
         let rev_as_list_no_empty_element =
           match List.rev as_list with
           | [] -> assert false
           | _ :: rev_list -> rev_list
         in
         List.rev_map
           (fun raw -> parse_certificate @@ JSON.parse ~origin:endpoint raw)
           rev_as_list_no_empty_element)

let wait_for_layer1_block_processing dac_node level =
  Dac_node.wait_for dac_node "dac_node_layer_1_new_head.v0" (fun e ->
      if JSON.(e |-> "level" |> as_int) = level then Some () else None)

let wait_for_layer1_new_head dac_node =
  Dac_node.wait_for dac_node "dac_node_layer_1_new_head.v0" (fun _ -> Some ())

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

let wait_for_l1_tracking_ended dac_node =
  Dac_node.wait_for dac_node "new_head_daemon_connection_lost.v0" (fun _ ->
      Some ())

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

let check_not_ready dac_node =
  assert_lwt_failure
    ~__LOC__
    "Expected DAC node not ready"
    (Dac_helper.Call_endpoint.get_health_ready dac_node)

let check_alive dac_node =
  let* () =
    Dac_node.wait_for dac_node "rpc_server_started.v0" (fun _ -> Some ())
  in
  let* liveness = Dac_helper.Call_endpoint.get_health_live dac_node in
  return @@ assert liveness

let check_liveness_and_readiness dac_node =
  let* liveness = Dac_helper.Call_endpoint.get_health_live dac_node in
  let* readiness = Dac_helper.Call_endpoint.get_health_ready dac_node in
  return @@ assert (liveness && readiness)

(** [check_downloaded_page coordinator observer page_hash] checks that the
     [observer] has downloaded a page with [page_hash] from the [coordinator],
     that the contents of the page corresponds to the ones of the
     [coordinator]. It returns the list  of the hashes contained in the
     [page_hash], if the page corresponds to a hash page. Otherwise, it returns
     the empty list. *)
let check_downloaded_page coordinator observer page_hash =
  let* coordinator_hex_encoded_page =
    Dac_helper.Call_endpoint.V0.get_preimage coordinator page_hash
  in
  let coordinator_page = Hex.to_string (`Hex coordinator_hex_encoded_page) in
  (* Check that the page has been saved by the observer. *)
  let* observer_hex_encoded_page =
    Dac_helper.Call_endpoint.V0.get_preimage observer page_hash
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

let decode_hex_string_to_bytes s = Hex.to_string (`Hex s)

let assert_state_changed ?block sc_rollup_node prev_state_hash =
  let* state_hash =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ?block ()
  in
  Check.(state_hash <> prev_state_hash)
    Check.string
    ~error_msg:"State hash has not changed (%L <> %R)" ;
  Lwt.return_unit

(** [Full_infrastructure] is a test suite consisting only of tests with the DAC
    nodes running. *)
module Full_infrastructure = struct
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5577
     Once we introduce DAC API ("v1"),  [Full_infrastructure] test suite should
     be refactored to use [v1] api as well. *)

  (** [coordinator_serializes_payload ?payload ?expected_rh] serializes
      provided optional [?payload] to [coordinator] node. Else it serializes
      "test" string as a payload. If [?expected_rh] is provided, it also checks
      that the actual root hash produced during serialization matches it. *)
  let coordinator_serializes_payload ?(payload = "test") ?expected_rh
      coordinator =
    let* actual_rh =
      Dac_helper.Call_endpoint.V0.Coordinator.post_preimage coordinator ~payload
    in
    let () =
      Option.iter
        (fun expected_rh -> check_valid_root_hash expected_rh actual_rh)
        expected_rh
    in
    return (`Hex actual_rh)

  let test_streaming_of_root_hashes_as_observer scenario =
    let Scenarios.{coordinator_node; observer_nodes; _} = scenario in

    (* 1. Create two new dac nodes; [observer_1] and [observer_2].
       2. Initialize their default configuration.
       3. Update their configuration so that their dac node client context
          points to [coordinator]. *)
    assert (List.length observer_nodes = 2) ;
    let observer_1 = List.nth observer_nodes 0 in
    let observer_2 = List.nth observer_nodes 1 in
    let* _ = Dac_node.init_config observer_1 in
    let* _ = Dac_node.init_config observer_2 in
    let payload_1 = "test_1" in
    let expected_rh_1 =
      "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
    in
    let payload_2 = "test_2" in
    let expected_rh_2 =
      "00f2f47f480fec0e4180930790e52a54b2dbd7676b5fa2a25dd93bf22969f22e33"
    in
    let push_promise_1 =
      wait_for_root_hash_pushed_to_data_streamer coordinator_node expected_rh_1
    in
    let push_promise_2 =
      wait_for_root_hash_pushed_to_data_streamer coordinator_node expected_rh_2
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
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
    in
    let* () = Dac_node.run observer_1 in
    let* () = observer_1_is_subscribed in
    (* [coordinator] serializes [payload_1]. We expect it would push
       [expected_rh_1] to all attached subscribers, i.e. to [observer_1]. *)
    let* _root_hash_1 =
      coordinator_serializes_payload
        coordinator_node
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
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
    in
    let push_signature =
      wait_for_signature_pushed_to_coordinator observer_1 ""
    in
    let* () = Dac_node.run observer_2 in
    let* () = observer_2_is_subscribed in
    (* [coordinator] serializes [payload_2]. We expect it would push
       [expected_rh_2] to all attached subscribers,
       i.e. to both [observer_1] and [observer_2] this time. *)
    let* _root_hash_2 =
      coordinator_serializes_payload
        coordinator_node
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

  let test_streaming_of_root_hashes_as_member scenario =
    let Scenarios.
          {coordinator_node; committee_members_nodes; committee_members; _} =
      scenario
    in
    (* This test doesn't have any meaning if run without any committee member. *)
    assert (List.length committee_members > 0) ;
    let member_key = List.nth committee_members 0 in
    let member = List.nth committee_members_nodes 0 in
    let* _ = Dac_node.init_config member in
    let payload = "test_1" in
    let expected_rh =
      "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
    in
    let push_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator_node expected_rh
    in
    let member_promise = wait_for_received_root_hash member expected_rh in

    (* Start running [member]. From now on we expect [member] to
       monitor streamed root hashes produced by [coordinator]. [coordinator]
       produces and pushes them as a side effect of serializing dac payload. *)
    let member_is_subscribed =
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
    in
    let expected_signature = bls_sign_hex_hash member_key (`Hex expected_rh) in
    let* () = Dac_node.run member in
    let* () = member_is_subscribed in
    (* [coordinator] serializes [payload_1]. We expect it would push
       [expected_rh_1] to all attached subscribers, i.e. to [member]. *)
    let* _root_hash =
      coordinator_serializes_payload coordinator_node ~payload ~expected_rh
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

  let test_observer_downloads_pages scenario =
    let Scenarios.{coordinator_node; observer_nodes; _} = scenario in

    (* 1. Create one new dac nodes; [observer_1],
       2. Initialize the default configuration,
       3. Specify a temporary directory within the test data for the observer
          reveal data dir,
       4. Update the configuration of the observer so that the dac node client
          context points to [coordinator]. *)
    assert (List.length observer_nodes = 1) ;
    let observer = List.nth observer_nodes 0 in
    let* _ = Dac_node.init_config observer in
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
      wait_for_root_hash_pushed_to_data_streamer coordinator_node expected_rh
    in
    let wait_for_observer_subscribed_to_data_streamer =
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
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
    let* _root_hash =
      coordinator_serializes_payload coordinator_node ~payload ~expected_rh
    in
    (* Assert [coordinator] emitted event that [expected_rh] was pushed
       to the data_streamer. *)
    let* () = push_promise in
    (* Assert [observer] emitted event of received [expected_rh]. *)
    let* () = fetch_root_hash_promise in
    check_downloaded_preimage coordinator_node observer expected_rh

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
      Dac_helper.Call_endpoint.V0.Coordinator.post_preimage
        coordinator_node
        ~payload
    in
    let () = check_valid_root_hash expected_rh actual_rh in
    let* () = root_hash_pushed_to_data_streamer_promise in
    Lwt.return_unit

  (** [init_config_of_nodes dac_nodes] initializes the configuration of all
      [dac_nodes]. *)
  let init_config_of_nodes dac_nodes =
    Lwt_list.iter_s
      (fun dac_node ->
        let* _ = Dac_node.init_config dac_node in
        return ())
      dac_nodes

  (** [run_and_subscribe_nodes coordinator_node dac_nodes] runs all [dac_nodes].
      Additionally, it blocks until all nodes are successfully subscribed to
      [coordinator_node] and synchronized with L1. *)
  let run_and_subscribe_nodes coordinator_node dac_nodes =
    let wait_for_node_subscribed_to_data_streamer () =
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
    in
    (* Run all dac nodes and wait for their subscription to coordinator.
       Because the event resolution loop in the Daemon always resolves
       all promises matching an event filter, when a new event is received,
       we cannot wait for multiple subscription to the hash streamer, as
       events of this kind are indistinguishable one from the other.
       Instead, we wait for the subscription of one observer/committe_member
       node to be notified before running the next node.
       In addition to that, we also wait for the dac_nodes to be
       synchronized with the L1's current head. *)
    Lwt_list.iter_s
      (fun node ->
        let node_is_subscribed = wait_for_node_subscribed_to_data_streamer () in
        let wait_for_level = wait_for_layer1_new_head node in
        let* () = Dac_node.run ~wait_ready:true node in
        let* () = check_liveness_and_readiness node in
        let* () = wait_for_level in
        node_is_subscribed)
      dac_nodes

  (** [init_run_and_subscribe_nodes coordinator_node dac_nodes] initializes
      the configuration, runs, and synchronizes all [dac_nodes], to be
      subscribed to the streaming of root hashes of [coordinator_node]. *)
  let init_run_and_subscribe_nodes coordinator_node dac_nodes =
    let* () = init_config_of_nodes dac_nodes in
    run_and_subscribe_nodes coordinator_node dac_nodes

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
    let wait_for_root_hash_processed_promises nodes =
      List.map
        (fun dac_node ->
          wait_for_received_root_hash_processed dac_node expected_rh)
        nodes
    in
    let* () =
      init_run_and_subscribe_nodes
        coordinator_node
        (committee_members_nodes @ observer_nodes)
    in
    let all_nodes_have_processed_root_hash =
      Lwt.join
      @@ wait_for_root_hash_processed_promises
           (committee_members_nodes @ observer_nodes)
    in
    (* 2. Post a preimage to the coordinator. *)
    let* _root_hash =
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
      Runnable.run @@ streamed_certificates_client coordinator_node expected_rh
    in
    let push_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator_node expected_rh
    in
    let* () =
      init_run_and_subscribe_nodes
        coordinator_node
        (committee_members_nodes @ observer_nodes)
    in
    (* 2. Post a preimage to the coordinator. *)
    let* _root_hash =
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
       from the GET v0/certificate endpoint. *)
    let* get_certificate =
      Dac_helper.Call_endpoint.V0.get_certificate
        coordinator_node
        ~hex_root_hash:(`Hex expected_rh)
    in
    check_certificate get_certificate last_certificate_update ;
    (* 6. Request certificate via streamed endpoints again, check that one
       item is returned with the same certificate returned by the GET
       endpoint. *)
    let* second_certificates_stream =
      Runnable.run @@ streamed_certificates_client coordinator_node expected_rh
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

  let test_client ~send_payload_from_file
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
       5. The returned signature is checked to be equivalent to the
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
    let* () =
      init_run_and_subscribe_nodes
        coordinator_node
        (committee_members_nodes @ observer_nodes)
    in
    (* 2. Dac client posts a preimage to coordinator, and waits for all
          committee members to sign. *)
    let* client_sends_payload_output =
      if send_payload_from_file then
        let filename = Temp.file "dac_payload" in
        let () = write_file filename ~contents:payload in
        Dac_client.send_payload_from_file
          coordinator_client
          filename
          ~threshold:(List.length committee_members)
      else
        Dac_client.send_hex_payload
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
          client `get certificate` coommand. *)
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
    (* 5. The returned signature is checked to be equivalent to the
          one previously output by the client. *)
    check_raw_certificate get_certificate last_certificate_update ;
    return ()

  (** Verifies that the serialized certificate returned in GET /v0/serialized_certificates
      is valid, by veryfing its content. *)
  let test_serialized_certificate
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
       5. The returned signature is checked to be equivalent to the
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
    let* () =
      init_run_and_subscribe_nodes
        coordinator_node
        (committee_members_nodes @ observer_nodes)
    in
    (* 2. Dac client posts a preimage to coordinator, and waits for all
          committee members to sign. *)
    let* client_sends_payload_output =
      Dac_client.send_hex_payload
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
          client `get certificate` command. *)
    let* get_certificate =
      Dac_node.RPC.call
        coordinator_node
        (Dac_rpc.V0.get_serialized_certificate
           ~hex_root_hash:(`Hex expected_rh))
    in
    let certif = `Hex get_certificate in
    (* 5. The returned signature is checked to be equivalent to the
          one previously output by the client. *)
    check_raw_certificate certif last_certificate_update ;
    return ()

  (* 1. Observer should fetch missing page from Committee Members when GET /missing_page/{hash}
        is called.
     2. As a side effect, Observer should save fetched page into its page store before
        returning it in the response. This can be observed by checking the result of
        retrieving preimage before and after the GET /missing_page/{hash} call.*)
  let test_observer_get_missing_page scenario =
    let Scenarios.{coordinator_node; committee_members_nodes; observer_nodes; _}
        =
      scenario
    in
    Check.(
      (List.length observer_nodes = 1)
        ~__LOC__
        int
        ~error_msg:"Expected number of Observers to be 1.") ;
    let observer_node = List.hd observer_nodes in
    let payload, expected_rh = sample_payload "preimage" in
    (* Initialize Committee Member nodes and wait for each node to subscribe to Coordinator's
       root hash stream. *)
    let* () =
      init_run_and_subscribe_nodes coordinator_node committee_members_nodes
    in
    let committee_members_processed_rh_promise =
      List.map
        (fun committee_member_node ->
          wait_for_received_root_hash_processed
            committee_member_node
            expected_rh)
        committee_members_nodes
    in

    (* Post payload with Observer offline so that Observer will not receive root hash. Wait
       for root hash to finish processing by each committee member before continuing. *)
    let* root_hash =
      Dac_helper.Call_endpoint.V0.Coordinator.post_preimage
        coordinator_node
        ~payload
    in
    assert (root_hash = expected_rh) ;
    let hex_root_hash = `Hex root_hash in
    let* _ = Lwt.join committee_members_processed_rh_promise in

    (* Initialize Observer node and wait for each node to subscribe to hash streamer. Assert
       that each node starts cold (without preimage of root_hash in disk) ie. get_preimage of
       Observer node should fail.
    *)
    let* _ =
      let* _ = Dac_node.init_config observer_node in
      let wait_subscribe =
        wait_for_handle_new_subscription_to_hash_streamer coordinator_node
      in
      let* _ = Dac_node.run ~wait_ready:true observer_node in
      let* _ = wait_subscribe in
      assert_lwt_failure
        ~__LOC__
        "Expected get_preimage to fail."
        (Dac_helper.Call_endpoint.V0.get_preimage
           observer_node
           (Hex.show hex_root_hash))
    in

    (* Get missing page then assert that the retrieved missing page from Observer is the
       same as the get_preimage page from Coordinator. *)
    let* missing_page =
      Dac_helper.Call_endpoint.V0.get_missing_page observer_node ~hex_root_hash
    in
    let* coordinator_page =
      Dac_helper.Call_endpoint.V0.get_preimage coordinator_node root_hash
    in
    check_preimage coordinator_page missing_page ;

    (* Now, Observer get_preimage should pass. This means get_missing_page
       saved the missing page on disk as a side effect.*)
    let* observer_preimage =
      Dac_helper.Call_endpoint.V0.get_preimage
        observer_node
        (Hex.show hex_root_hash)
    in
    check_preimage coordinator_page observer_preimage ;
    unit

  (** [test_committe_member_disconnects_scenario] checks, that
      committee member nodes automatically reconnect to the
      streaming of root hashes in case they lose connection from Coordinator.
      This may happen for example if Coordinator is rebooted. *)
  let test_committe_member_disconnects_scenario
      Scenarios.{coordinator_node; committee_members_nodes; _} =
    let wait_for_node_subscribed_to_data_streamer () =
      wait_for_handle_new_subscription_to_hash_streamer coordinator_node
    in
    (* We assert DAC network of only one committee member node. Unless this
       is the case, this test as is may exhibit race conditions. *)
    let () = assert (List.length committee_members_nodes = 1) in
    let committee_member = List.hd committee_members_nodes in
    (* Test start here. *)
    (* 1. We set up a running and functional DAC network. *)
    let* () =
      init_run_and_subscribe_nodes coordinator_node committee_members_nodes
    in
    Log.info "Terminating Coordinator node" ;
    (* 2. We restart the [coordinator_node]. We expect that all subscriptions to
          the streaming of root hashes are lost. *)
    let* () = Dac_node.terminate coordinator_node in
    Log.info "Restarting Coordinator node" ;
    (* 3. We restart [coordinator_node] and expect that clients will reconnect
          automatically. *)
    let* () = Dac_node.run coordinator_node in
    (* 4. We assert [3.] by waiting for "handle_new_subscription_to_hash_streamer"
          event. *)
    let* () = wait_for_node_subscribed_to_data_streamer () in
    let expected_rh =
      "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
    in
    let committee_member_receives_root_hash_promise =
      wait_for_received_root_hash_processed committee_member expected_rh
    in
    (* 5. We serialize random payload. *)
    let* _root_hash =
      coordinator_serializes_payload
        coordinator_node
        ~payload:"test"
        ~expected_rh
    in
    (* 6. We check that [committee_member} received a root hash that corresponds
          to the serialized payload in [5].*)
    committee_member_receives_root_hash_promise

  (** [test_tezos_node_disconnects_scenario] checks that upon L1 disconnection,
      DAC actors automatically restart L1 tracking. In addition, we also test
      that upon reconnection, DAC network works as expected when serializing a
      random payload.  *)
  let test_tezos_node_disconnects_scenario
      Scenarios.
        {node; coordinator_node; committee_members_nodes; observer_nodes; _} =
    (* We assert DAC network of only one committee member and one observer node.
       Unless this is the case, this test as is may exhibit race conditions. *)
    let () =
      assert (
        List.length committee_members_nodes = 1
        && List.length observer_nodes = 1)
    in
    let committee_member = List.hd committee_members_nodes in
    let observer = List.hd observer_nodes in
    (* Test start here. *)
    (* 1. We set up a running and functional DAC network. *)
    let* () =
      init_run_and_subscribe_nodes
        coordinator_node
        (committee_members_nodes @ observer_nodes)
    in
    (* 2. We terminate the Tezos [node], which cause a L1 disconnection of the
          DAC network. *)
    let wait_for_coordinator_stopped_tracking_l1 =
      wait_for_l1_tracking_ended coordinator_node
    in
    let wait_for_committee_member_stopped_tracking_l1 =
      wait_for_l1_tracking_ended committee_member
    in
    let wait_for_observer_stopped_tracking_l1 =
      wait_for_l1_tracking_ended observer
    in
    Log.info "Terminating Tezos node" ;
    let* () = Node.terminate node in
    let* () = wait_for_coordinator_stopped_tracking_l1 in
    let* () = wait_for_committee_member_stopped_tracking_l1 in
    let* () = wait_for_observer_stopped_tracking_l1 in
    (* 3. We restart Tezos [node] and expect the DAC network to restart tracking
         L1 heads. *)
    let wait_for_coordinator_connected_to_l1 =
      wait_for_layer1_new_head coordinator_node
    in
    let wait_for_committee_member_connected_to_l1 =
      wait_for_layer1_new_head committee_member
    in
    Log.info "Restarting Tezos node" ;
    let* () = Node.run node [] in
    (* 4. We assert [3.] by waiting for "dac_node_layer_1_new_head" event from
       both [coordinator_node] and [committee_member] node. *)
    let* () = wait_for_coordinator_connected_to_l1 in
    let* () = wait_for_committee_member_connected_to_l1 in
    let expected_rh =
      "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
    in
    let committee_member_receives_root_hash_promise =
      wait_for_received_root_hash_processed committee_member expected_rh
    in
    let observer_receives_root_hash_promise =
      wait_for_received_root_hash_processed observer expected_rh
    in
    (* 5. We serialize random payload. *)
    let* _root_hash =
      coordinator_serializes_payload
        coordinator_node
        ~payload:"test"
        ~expected_rh
    in
    (* 6. We check that [committee_member] and [observer] node both received
          a root hash that corresponds to the serialized payload in [5].*)
    let* () = committee_member_receives_root_hash_promise in
    observer_receives_root_hash_promise

  module Signature_manager = struct
    let test_non_committee_signer_should_fail tz_client
        (coordinator_node, hex_root_hash, _dac_committee) =
      let* invalid_signer_key =
        Client.bls_gen_and_show_keys ~alias:"invalid_signer" tz_client
      in
      let signature = bls_sign_hex_hash invalid_signer_key hex_root_hash in
      let result =
        Dac_helper.Call_endpoint.V0.put_dac_member_signature
          coordinator_node
          ~hex_root_hash
          ~dac_member_pkh:invalid_signer_key.aggregate_public_key_hash
          ~signature
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
        Dac_helper.Call_endpoint.V0.put_dac_member_signature
          coordinator_node
          ~hex_root_hash
          ~dac_member_pkh:memberj.aggregate_public_key_hash
          ~signature
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
              Dac_helper.Call_endpoint.V0.put_dac_member_signature
                coordinator_node
                ~hex_root_hash
                ~dac_member_pkh:member.aggregate_public_key_hash
                ~signature
            in
            return (member :: keys))
          (return [])
          members
      in
      let* witnesses, certificate, _root_hash, _version =
        Dac_helper.Call_endpoint.V0.get_certificate
          coordinator_node
          ~hex_root_hash
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
        Dac_helper.Call_endpoint.V0.put_dac_member_signature
          coordinator_node
          ~hex_root_hash
          ~dac_member_pkh
          ~signature
      in
      let* () = call () in
      let* () = call () in
      let* witnesses, certificate, _root_hash, _version =
        Dac_helper.Call_endpoint.V0.get_certificate
          coordinator_node
          ~hex_root_hash
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
        Dac_helper.Call_endpoint.V0.put_dac_member_signature
          coordinator_node
          ~hex_root_hash:false_root_hash
          ~dac_member_pkh
          ~signature
      in
      assert_lwt_failure
        ~__LOC__
        "Expected failure when unknown root_hash"
        result

    let test_handle_store_signature scenario =
      let Scenarios.{coordinator_node; committee_members; client; _} =
        scenario
      in
      let* hex_root_hash = init_hex_root_hash coordinator_node in
      let dac_env = (coordinator_node, hex_root_hash, committee_members) in
      let* () = test_non_committee_signer_should_fail client dac_env in
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
    let test_get_certificate scenario =
      let Scenarios.{coordinator_node; committee_members; _} = scenario in
      let i = Random.int (List.length committee_members) in
      let member = List.nth committee_members i in
      let* hex_root_hash =
        init_hex_root_hash
          ~payload:"test get certificate payload 123"
          coordinator_node
      in
      let signature = bls_sign_hex_hash member hex_root_hash in
      let* () =
        Dac_helper.Call_endpoint.V0.put_dac_member_signature
          coordinator_node
          ~hex_root_hash
          ~dac_member_pkh:member.aggregate_public_key_hash
          ~signature
      in
      let* witnesses, signature, _root_hash, _version =
        Dac_helper.Call_endpoint.V0.get_certificate
          coordinator_node
          ~hex_root_hash
      in
      let expected_witnesses = Z.shift_left Z.one i in
      assert_witnesses ~__LOC__ (Z.to_int expected_witnesses) witnesses ;
      assert_verify_aggregate_signature [member] hex_root_hash signature ;
      unit
  end
end

let test_observer_times_out_when_page_cannot_be_fetched _protocol node client
    _key =
  with_coordinator_node ~name:"coordinator" ~committee_members:[] node client
  @@ fun coordinator_node _ ->
  (* Root hash that will never be found. *)
  let missing_root_hash =
    "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
  in
  let sleeping_node_timeout = 20. in
  (* Fake our commmitee member connection to point to the sleeping node. *)
  Dac_node.with_sleeping_node ~timeout:sleeping_node_timeout @@ fun rpc ->
  let committee_member_rpcs = [rpc] in
  let observer_timeout = 4 in
  let wait_connected_to_coordinator =
    wait_for_handle_new_subscription_to_hash_streamer coordinator_node
  in
  let observer_node =
    Dac_node.create_observer
      ~name:"observer"
      ~coordinator_rpc_port:(Dac_node.rpc_port coordinator_node)
      ~coordinator_rpc_host:(Dac_node.rpc_host coordinator_node)
      ~timeout:observer_timeout
      ~committee_member_rpcs
      ~node
      ~client
      ()
  in
  let* _dir = Dac_node.init_config observer_node in
  let* () = Dac_node.run observer_node ~wait_ready:true in
  let* () = wait_connected_to_coordinator in
  (* Capture the time it takes for the endpoint to fail. It should fail after
     [observer_timeout] seconds but before [sleeping_node_timeout] seconds *)
  let start_time = ref 0. in
  let end_time = ref 0. in
  let* _ =
    Lwt.catch
      (fun () ->
        start_time := Unix.gettimeofday () ;
        Lwt.map
          (fun _ -> ())
          (Dac_helper.Call_endpoint.V0.get_missing_page
             observer_node
             ~hex_root_hash:(`Hex missing_root_hash)))
      (fun _ ->
        end_time := Unix.gettimeofday () ;
        Lwt.return_unit)
  in
  let request_time = Float.sub !end_time !start_time in
  let observer_timeout = Float.of_int observer_timeout in
  let result =
    request_time >= observer_timeout && request_time < sleeping_node_timeout
  in
  Check.(
    (result = true)
      bool
      ~__LOC__
      ~error_msg:
        (Printf.sprintf
           "Expected timeout after %f seconds but less than %f seconds."
           observer_timeout
           sleeping_node_timeout)) ;
  unit

(* Modified from tezt/tests/tx_sc_rollup.ml *)
module Tx_kernel_e2e = struct
  open Sc_rollup_helpers
  module Bls = Tezos_crypto.Signature.Bls

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5577
     Once we introduce DAC API ("v1"), [Tx_kernel_e2e] test suite should
     be refactored to use [v1] api as well. *)

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

  let assert_ticks_advanced ?block sc_rollup_node prev_ticks =
    let* ticks =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_total_ticks ?block ()
    in
    Check.(ticks > prev_ticks)
      Check.int
      ~error_msg:"Tick counter did not advance (%L > %R)" ;
    Lwt.return_unit

  (* Send a deposit into the rollup. *)
  let test_deposit ~client ~sc_rollup_node ~sc_rollup_address
      ~mint_and_deposit_contract level tz4_address =
    let* prev_state_hash =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_state_hash ()
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
    let* () = assert_state_changed sc_rollup_node prev_state_hash in
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

  let send_message_and_wait_for_level ~level ~sc_rollup_node ~client
      hex_encoded_message =
    let* prev_state_hash =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_state_hash ()
    in
    let* prev_ticks =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_total_ticks ()
    in
    let* () = send_message client (sf "hex:[%S]" hex_encoded_message) in
    let level = level + 1 in
    let* _ = Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level in
    let* () = assert_state_changed sc_rollup_node prev_state_hash in
    return {prev_state_hash; prev_ticks; level}

  let verify_outbox_answer ~withdrawal_level ~sc_rollup_node ~sc_rollup_client
      ~sc_rollup_address ~client =
    let* outbox =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_outbox ~outbox_level:withdrawal_level ()
    in
    Log.info "Outbox is %s" @@ JSON.encode outbox ;
    let* answer =
      let message_index = 0 in
      let outbox_level = withdrawal_level in
      Sc_rollup_client.outbox_proof
        sc_rollup_client
        ~message_index
        ~outbox_level
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
            let* () = check_liveness_and_readiness dac_member in
            ev)
          committee_members_nodes
      else unit
    in
    let* preimage_hash =
      Dac_helper.Call_endpoint.V0.Coordinator.post_preimage
        coordinator_node
        ~payload
    in
    let* () = Lwt.join wait_for_member_signature_pushed_to_coordinator in
    let* witnesses, signature, root_hash, _version =
      Dac_helper.Call_endpoint.V0.get_certificate
        coordinator_node
        ~hex_root_hash:(`Hex preimage_hash)
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
    let* () = check_liveness_and_readiness observer_node in
    let* {boot_sector; _} =
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
      Client.RPC.call ~hooks client
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
           mint_and_deposit_contract : _;
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
        ~sc_rollup_node
        ~client
        l1_external_message
    in

    (* After that pkh1 has 410 tickets, pkh2 has 40 tickets *)
    (* Send withdrawal *)
    let* {prev_state_hash; prev_ticks; level = withdrawal_level} =
      send_message_and_wait_for_level
        ~level
        ~sc_rollup_node
        ~client
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
    let* () = assert_state_changed ~block sc_rollup_node prev_state_hash in
    let* () = assert_ticks_advanced ~block sc_rollup_node prev_ticks in

    (* EXECUTE withdrawal *)
    let* () =
      verify_outbox_answer
        ~client
        ~sc_rollup_node
        ~sc_rollup_client
        ~sc_rollup_address
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
    let* {boot_sector; _} =
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
      Client.RPC.call ~hooks client
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
    let* () = check_liveness_and_readiness observer_node in

    (* Send DAC certificate as an External Message to L1 *)
    let* {level; _} =
      send_message_and_wait_for_level
        ~level
        ~sc_rollup_node
        ~client
        l1_external_message
    in

    (* After that pkh1 has 410 tickets, pkh2 has 40 tickets *)
    (* Send withdrawal *)
    let* {prev_state_hash; prev_ticks; level = withdrawal_level} =
      send_message_and_wait_for_level
        ~level
        ~sc_rollup_node
        ~client
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
    let* () = assert_state_changed ~block sc_rollup_node prev_state_hash in
    let* () = assert_ticks_advanced ~block sc_rollup_node prev_ticks in

    (* EXECUTE withdrawal *)
    let* () =
      verify_outbox_answer
        ~client
        ~sc_rollup_node
        ~sc_rollup_client
        ~sc_rollup_address
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
      ~uses:(fun _protocol -> [Constant.octez_smart_rollup_node])
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
      ~supports:Protocol.(From_protocol (number Nairobi + 1))
      ~tags:["wasm"; "kernel"; "wasm_2_0_0"; "kernel_e2e"; "dac"; "full"]
      ~uses:(fun _protocol -> [Constant.octez_smart_rollup_node])
      ~pvm_name:"wasm_2_0_0"
      ~committee_size:0
      ~observers:1
      ~custom_committee_members
      ~commitment_period
      ~challenge_window
      "kernel_e2e.dac_observer_missing_pages"
      (test_tx_kernel_e2e_with_dac_observer_missing_pages commitment_period)
end

(** [V1_API] is a test suite for [V1] API. *)
module V1_API = struct
  (** Test "v1/page" endpoint. *)
  let test_get_pages Scenarios.{coordinator_node; _} =
    (* 1. Post [payload] to [Coordinator] and receive [root_hash].
       2. Asumming that [payload] fits into one [Contents] page
          retrieving it via [GET v1/pages/[root_hash]] should result
          in initial payload. *)
    let payload = "test" in
    (* Assert that [expected] payload fits onto one [Contents] page.
       Since [init_hex_root_hash] calls "POST v0/preimage" that uses
       [Merkle_tree.V0] serialization, [max_page_size] is 4096 bytes,
       out of which 5 bytes are for preamble, meaning inital payload
       can be at max 4091 bytes in size for the above to hold. *)
    let () = assert (Bytes.length (String.to_bytes payload) < 4091) in
    let* root_hash =
      (* TODO https://gitlab.com/tezos/tezos/-/issues/5671
         Once we have "PUT v1/preimage" we should use a call to [V1] api here
         instead. *)
      Dac_helper.Call_endpoint.V0.Coordinator.post_preimage
        coordinator_node
        ~payload
    in
    let* raw =
      Dac_helper.Call_endpoint.V1.get_pages coordinator_node root_hash
    in
    let remove_preamble s =
      let preamle_size = 5 in
      String.(sub s preamle_size (length s - preamle_size))
    in
    let actual = remove_preamble (decode_hex_string_to_bytes raw) in
    return @@ check_preimage payload actual

  (** [test_allow_v1_feature_flag] tests [--allow-v1-api] feature flag. *)
  let test_allow_v1_feature_flag Scenarios.{coordinator_node; _} =
    (* Running [Coordinator] node without explicitly providing [--allow-v1-api]
       feature flag should not register [V1] API. Calling [V1] API endpoints
       should therefore result in 404 (Not found) errors. To test this:

       1. We post payload to [coordinator_node] to obtain valid [root_hash]
       2. Calling "GET v1/pages/[root_hash]" should fail with 404 response,
          because [--allow-v1-api] feature flag was not set.
    *)
    let* root_hash =
      (* TODO https://gitlab.com/tezos/tezos/-/issues/5671
         Once we have "PUT v1/preimage" we should use a call to [V1] api here
         instead. *)
      Dac_node.RPC.call
        coordinator_node
        (Dac_rpc.V0.Coordinator.post_preimage ~payload:"test")
    in
    let* response =
      Dac_node.RPC.call_raw coordinator_node @@ Dac_rpc.V1.get_pages root_hash
    in
    return @@ RPC_core.check_string_response ~code:404 response
end

(** [Api_regression] is a module that encapsulates schema regression tests of
    the DAC API. Here we test the binding contracts of the versioned API. *)
module Api_regression = struct
  let replace_variables string =
    let replacements =
      [
        ("tz[1234]\\w{33}\\b", "[PUBLIC_KEY_HASH]");
        ("(BLsig|asig)\\w{137}\\b", "[AGGREGATED_SIG]");
        ("http://127.0.0.1:\\d{4,5}/", "$SCHEME://$HOST:$PORT/");
      ]
    in
    List.fold_left
      (fun string (replace, by) ->
        replace_string ~all:true (rex replace) ~by string)
      string
      replacements

  let capture text = text |> replace_variables |> Regression.capture

  let create_uri ~path_and_query dac_node =
    let url =
      Format.sprintf
        "http://%s:%d/%s"
        (Dac_node.rpc_host dac_node)
        (Dac_node.rpc_port dac_node)
        path_and_query
    in
    Uri.of_string url

  let capture_rpc_request headers ?body verb uri =
    let () =
      capture
        (sf
           "RPC_REQUEST_URI: %s %s"
           (Cohttp.Code.string_of_method verb)
           (Uri.to_string uri))
    in
    let () =
      capture @@ sf "RPC_REQUEST_HEADER: %s" (Cohttp.Header.to_string headers)
    in
    match body with
    | Some body ->
        let* body = Cohttp_lwt.Body.to_string body in
        return @@ capture @@ sf "RPC_REQUEST_BODY: %s" body
    | None -> Lwt.return_unit

  (** [rpc_call_with_regression_test] is used for SCHEMA regression testing of the
      RPCs. A call to this function in addition to calling an RPC, captures the
      following arguments:
        1. RPC_REQUEST_URI
        2. RPC_REQUEST_HEADER
        3. RPC_REQUEST_BODY
        4. RPC_RESPONSE_BODY

      As such with the call to this function we:
        1. Test binding contract of RPC request.
        2. Test binding contract of RPC response. *)
  let rpc_call_with_regression_test ?body_json ~path_and_query verb dac_node =
    let uri = create_uri ~path_and_query dac_node in
    let headers =
      Cohttp.Header.of_list [("Content-Type", "application/json")]
    in
    let body =
      Option.map
        (fun body ->
          let json = JSON.unannotate body in
          let raw_json = JSON.encode_u json in
          let request_body = Cohttp_lwt.Body.of_string raw_json in
          request_body)
        body_json
    in
    let* () = capture_rpc_request headers ?body verb uri in
    let* _respone, body = Cohttp_lwt_unix.Client.call ~headers ?body verb uri in
    let* raw_body = Cohttp_lwt.Body.to_string body in
    return @@ capture @@ sf "RPC_RESPONSE_BODY: %s" raw_body

  let rpc_curl_with_regression_test ~path_and_query verb dac_node =
    let uri = create_uri ~path_and_query dac_node in
    let headers =
      Cohttp.Header.of_list [("Content-Type", "application/json")]
    in
    let* () = capture_rpc_request headers verb uri in
    return @@ (Curl.get_raw (Uri.to_string uri) |> Runnable.map capture)

  (** [V0] module is used for regression testing [V0] API. *)
  module V0 = struct
    let v0_api_prefix = Dac_rpc.V0.api_prefix

    let encode_bytes_to_hex_string raw =
      "\"" ^ match Hex.of_string raw with `Hex s -> s ^ "\""

    (** [test_coordinator_post_preimage] tests Cooordinator's
        "POST v0/preimage". *)
    let test_coordinator_post_preimage Scenarios.{coordinator_node; _} =
      (* [post_preimage_request] shape is binding. *)
      let post_preimage_request =
        JSON.parse
          ~origin:"Dac_api_regression.V0.coordinator_post_preimage"
          (encode_bytes_to_hex_string "test")
      in
      (* Test starts here. *)
      rpc_call_with_regression_test
        `POST
        ~path_and_query:(sf "%s/preimage" v0_api_prefix)
        ~body_json:post_preimage_request
        coordinator_node

    let test_get_preimage Scenarios.{coordinator_node; _} =
      (* First we prepare Coordinator's node by posting a payload to it. *)
      let* root_hash =
        Full_infrastructure.coordinator_serializes_payload coordinator_node
      in
      (* Test starts here. *)
      rpc_call_with_regression_test
        `GET
        ~path_and_query:(sf "%s/preimage/%s" v0_api_prefix (Hex.show root_hash))
        coordinator_node

    let create_sample_signature committee_members root_hash =
      let i = Random.int (List.length committee_members) in
      let member = List.nth committee_members i in
      (bls_sign_hex_hash member root_hash, member)

    (** [test_put_dac_member_signature] tests "PUT v0/dac_member_signature". *)
    let test_put_dac_member_signature
        Scenarios.{coordinator_node; committee_members; _} =
      (* First we prepare Coordinator's node by posting a payload to it. *)
      let* root_hash =
        Full_infrastructure.coordinator_serializes_payload coordinator_node
      in
      let signature, member =
        create_sample_signature committee_members root_hash
      in
      let request_body =
        Dac_rpc.V0.make_put_dac_member_signature_request_body
          ~dac_member_pkh:member.aggregate_public_key_hash
          ~root_hash
          signature
      in
      let body_json =
        JSON.annotate ~origin:"Dac_api_regression.V0.put_signature" request_body
      in
      (* Test starts here. *)
      rpc_call_with_regression_test
        `PUT
        ~body_json
        ~path_and_query:(sf "%s/dac_member_signature" v0_api_prefix)
        coordinator_node

    (** [test_get_certificate] tests "PUT v0/certificate". *)
    let test_get_certificate Scenarios.{coordinator_node; committee_members; _}
        =
      (* First we prepare Coordinator's node by posting a payload to it. *)
      let* root_hash =
        Full_infrastructure.coordinator_serializes_payload coordinator_node
      in
      (* Then we create a sample signature for a random member. *)
      let signature, member =
        create_sample_signature committee_members root_hash
      in
      (* We put the sample signature to Coordinator node. *)
      let* () =
        Dac_node.RPC.call
          coordinator_node
          (Dac_rpc.V0.put_dac_member_signature
             ~hex_root_hash:root_hash
             ~dac_member_pkh:member.aggregate_public_key_hash
             ~signature)
      in
      (* Test starts here. *)
      rpc_call_with_regression_test
        `GET
        ~path_and_query:
          (sf "%s/certificates/%s" v0_api_prefix (Hex.show root_hash))
        coordinator_node

    (** [test_observer_get_missing_page] regression tests "GET v0/missing_page". *)
    let test_observer_get_missing_page
        Scenarios.{coordinator_node; observer_nodes; committee_members_nodes; _}
        =
      let* () =
        Full_infrastructure.init_run_and_subscribe_nodes
          coordinator_node
          (committee_members_nodes @ observer_nodes)
      in
      let expected_rh =
        "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
      in
      let committee_members_processed_rh_promise =
        List.map
          (fun committee_member_node ->
            wait_for_received_root_hash_processed
              committee_member_node
              expected_rh)
          committee_members_nodes
      in
      let* root_hash =
        Full_infrastructure.coordinator_serializes_payload
          coordinator_node
          ~expected_rh
      in
      let* () = Lwt.join committee_members_processed_rh_promise in
      let observer_node = List.hd observer_nodes in
      (* Test starts here. *)
      rpc_call_with_regression_test
        `GET
        ~path_and_query:
          (sf "%s/missing_page/%s" v0_api_prefix (Hex.show root_hash))
        observer_node

    (** [test_monitor_root_hashes] regression tests "GET v0/monitor/root_hashes". *)
    let test_monitor_root_hashes Scenarios.{coordinator_node; _} =
      let* monitor_root_hashes_client =
        rpc_curl_with_regression_test
          `GET
          ~path_and_query:(sf "%s/monitor/root_hashes" v0_api_prefix)
          coordinator_node
      in
      let _streamed_root_hashes_client =
        Runnable.run monitor_root_hashes_client
      in
      (* TODO https://gitlab.com/tezos/tezos/-/issues/5909
         Schema regression testing of response body has been proven out difficult,
         since a call to it is blocking. Currently we have no way to send an end
         signal to the streaming component unless shutting down a node.
         Unfortunately, in such case tezt propagates the error, resulting in
         regression test to fail. *)
      let _root_hash =
        Full_infrastructure.coordinator_serializes_payload coordinator_node
      in
      Lwt.return_unit

    (** [test_monitor_certificates] regression tests "GET v0/monitor/root_hashes". *)
    let test_monitor_certificates
        Scenarios.{coordinator_node; committee_members_nodes; _} =
      let* () =
        Full_infrastructure.init_run_and_subscribe_nodes
          coordinator_node
          committee_members_nodes
      in
      let expected_rh =
        "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
      in
      let* monitor_certificate_updates_client =
        rpc_curl_with_regression_test
          `GET
          ~path_and_query:
            (sf "%s/monitor/certificate/%s" v0_api_prefix expected_rh)
          coordinator_node
      in
      let streamed_certificate_updates =
        Runnable.run monitor_certificate_updates_client
      in
      let* _root_hash =
        Full_infrastructure.coordinator_serializes_payload
          coordinator_node
          ~expected_rh
      in
      let* () = streamed_certificate_updates in
      Lwt.return_unit
  end
end

let register ~protocols =
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~committee_size:0
    ~observers:2
    ~tags:["dac"]
    "dac_streaming_of_root_hashes"
    Full_infrastructure.test_streaming_of_root_hashes_as_observer
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~committee_size:1
    ~observers:0
    ~tags:["dac"]
    "dac_push_signature_as_member"
    Full_infrastructure.test_streaming_of_root_hashes_as_member
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:1
    ~committee_size:0
    ~tags:["dac"]
    "committee member downloads pages from coordinator"
    Full_infrastructure.test_observer_downloads_pages
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"]
    "dac_get_certificate"
    Full_infrastructure.Signature_manager.test_get_certificate
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:3
    ~tags:["dac"]
    "dac_store_member_signature"
    Full_infrastructure.Signature_manager.test_handle_store_signature
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:0
    ~tags:["dac"]
    "dac_coordinator_post_preimage_endpoint"
    Full_infrastructure.test_coordinator_post_preimage_endpoint
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:1
    ~committee_size:2
    ~tags:["dac"]
    "dac_observer_get_missing_page"
    Full_infrastructure.test_observer_get_missing_page
    protocols ;
  scenario_with_layer1_node
    ~__FILE__
    ~tags:["dac"]
    ~uses:(fun _protocol -> [Constant.octez_dac_node])
    "dac_observer_times_out_when_page_cannot_be_fetched"
    test_observer_times_out_when_page_cannot_be_fetched
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:1
    ~committee_size:1
    ~tags:["dac"]
    "committee members and observers download pages from coordinator"
    Full_infrastructure.test_download_and_retrieval_of_pages
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"]
    "certificates are updated in streaming endpoint"
    Full_infrastructure.test_streaming_certificates
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"]
    ~uses:(fun _protocol -> [Constant.octez_dac_client])
    "test client commands (hex payload from CLI)"
    (Full_infrastructure.test_client ~send_payload_from_file:false)
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"]
    ~uses:(fun _protocol -> [Constant.octez_dac_client])
    "test client commands (binary payload from file)"
    (Full_infrastructure.test_client ~send_payload_from_file:true)
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"]
    ~uses:(fun _protocol -> [Constant.octez_dac_client])
    "test serialized certificate"
    Full_infrastructure.test_serialized_certificate
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:1
    ~tags:["dac"]
    "test committee member disconnects from Coordinator"
    Full_infrastructure.test_committe_member_disconnects_scenario
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:1
    ~committee_size:1
    ~tags:["dac"]
    "test DAC disconnects from L1"
    Full_infrastructure.test_tezos_node_disconnects_scenario
    protocols ;
  Tx_kernel_e2e.test_tx_kernel_e2e_with_dac_observer_synced_with_dac protocols ;
  Tx_kernel_e2e.test_tx_kernel_e2e_with_dac_observer_missing_pages protocols ;
  scenario_with_full_dac_infrastructure
    ~allow_v1_api:true
    ~__FILE__
    ~observers:0
    ~committee_size:0
    ~tags:["dac"]
    "test v1/get_pages"
    V1_API.test_get_pages
    protocols ;
  scenario_with_full_dac_infrastructure
    ~allow_v1_api:false
    ~__FILE__
    ~observers:0
    ~committee_size:0
    ~tags:["dac"]
    "test --allow_v1_api feature flag"
    V1_API.test_allow_v1_feature_flag
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:0
    ~tags:["dac"; "api_regression"]
    ~allow_regression:true
    "test Coordinator's post preimage"
    Api_regression.V0.test_coordinator_post_preimage
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:0
    ~tags:["dac"; "api_regression"]
    ~allow_regression:true
    "test GET v0/preimage"
    Api_regression.V0.test_get_preimage
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"; "api_regression"]
    ~allow_regression:true
    "test PUT v0/dac_member_signature"
    Api_regression.V0.test_put_dac_member_signature
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:2
    ~tags:["dac"; "api_regression"]
    ~allow_regression:true
    "test GET v0/certificate"
    Api_regression.V0.test_get_certificate
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:1
    ~committee_size:3
    ~tags:["dac"; "api_regression"]
    ~allow_regression:true
    "test GET v0/missing_page"
    Api_regression.V0.test_observer_get_missing_page
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:0
    ~tags:["dac"; "api_regression"]
    ~allow_regression:true
    "test GET v0/monitor/root_hashes"
    Api_regression.V0.test_monitor_root_hashes
    protocols ;
  scenario_with_full_dac_infrastructure
    ~__FILE__
    ~observers:0
    ~committee_size:1
    ~tags:["dac"; "api_regression"]
    ~allow_regression:true
    "test GET v0/monitor/certificate"
    Api_regression.V0.test_monitor_certificates
    protocols
