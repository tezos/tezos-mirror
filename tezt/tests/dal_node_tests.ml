(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL node tests: RPCs, snapshots, profiles, config, crawler, restart. *)

open Dal_helpers
module Dal = Dal_common

let test_dal_node_slot_management _protocol parameters _cryptobox _node client
    dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let slot_index = 0 in
  let slot_content = "test with invalid UTF-8 byte sequence \xFA" in
  let* _slot_commitment =
    Helpers.publish_and_store_slot
      client
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      Helpers.(make_slot ~slot_size slot_content)
  in
  let* () = bake_for client in
  let* published_level = Client.level client in
  (* Finalize the publication. *)
  let wait_for_dal_node =
    wait_for_layer1_final_block dal_node published_level
  in
  let* () = bake_for ~count:2 client in
  let* () = wait_for_dal_node in
  let* received_slot =
    Dal_RPC.(
      call dal_node
      @@ get_level_slot_content ~slot_level:published_level ~slot_index)
  in
  let received_slot_content = Helpers.content_of_slot received_slot in
  Check.(
    (slot_content = received_slot_content)
      string
      ~error_msg:"Wrong slot content: Expected: %L. Got: %R") ;
  let* _ =
    Dal_RPC.(
      call dal_node
      @@ get_level_slot_status ~slot_level:published_level ~slot_index)
  in
  let* pages =
    Dal_RPC.(call dal_node @@ get_level_slot_pages ~published_level ~slot_index)
  in
  Check.(
    slot_content = Helpers.(content_of_slot @@ slot_of_pages ~slot_size pages))
    Check.string
    ~__LOC__
    ~error_msg:"Unexecpeted slot fetched: Expected: %L. Got: %R" ;
  return ()

let () =
  Printexc.register_printer @@ function
  | Data_encoding.Binary.Read_error e ->
      Some
        (Format.asprintf
           "Failed to decode binary: %a@."
           Data_encoding.Binary.pp_read_error
           e)
  | _ -> None

let check_get_commitment dal_node ~slot_level check_result slots_info =
  Lwt_list.iter_s
    (fun (slot_index, commitment') ->
      let* response =
        Dal_RPC.(
          call_raw dal_node @@ get_level_slot_commitment ~slot_index ~slot_level)
      in
      return @@ check_result commitment' response)
    slots_info

let get_commitment_succeeds expected_commitment response =
  match response.RPC_core.code with
  | 200 ->
      let commitment =
        JSON.(parse ~origin:__LOC__ response.RPC_core.body |> as_string)
      in
      Check.(commitment = expected_commitment)
        Check.string
        ~error_msg:
          "The value of a stored commitment should match the one computed \
           locally (current = %L, expected = %R)"
  | code -> Test.fail ~__LOC__ "Unexpected HTTP response code %d" code

let get_commitment_not_found _commit r =
  RPC_core.check_string_response ~code:404 r

let check_stored_level_headers ~__LOC__ dal_node ~pub_level ~number_of_slots
    ~number_of_headers =
  let* number_of_stored_commitments =
    Lwt_list.fold_left_s
      (fun accu slot_index ->
        let* response =
          Dal_RPC.(
            call_raw dal_node
            @@ get_level_slot_commitment ~slot_level:pub_level ~slot_index)
        in
        match response.code with
        | 200 -> return (accu + 1)
        | 404 -> return accu
        | code -> Test.fail ~__LOC__ "Unexpected HTTP response code %d" code)
      0
      (List.init number_of_slots Fun.id)
  in
  Check.(number_of_stored_commitments = number_of_headers)
    ~__LOC__
    Check.int
    ~error_msg:"Unexpected slot headers length (got = %L, expected = %R)" ;
  unit

let test_dal_node_slots_headers_tracking protocol parameters _cryptobox node
    client dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = parameters.Dal.Parameters.number_of_slots in
  let check_stored_level_headers =
    check_stored_level_headers dal_node ~number_of_slots
  in
  let* level = Node.get_level node in
  let pub_level = level + 1 in
  let publish ?(fee = 12_000) source ~index content =
    let content = Helpers.make_slot ~slot_size content in
    let* commitment =
      Helpers.publish_and_store_slot ~fee client dal_node source ~index content
    in
    return (index, commitment)
  in
  let* slot0 = publish Constant.bootstrap1 ~index:0 "test0" in
  let* slot1 = publish Constant.bootstrap2 ~index:1 "test1" in
  let* () =
    (* The slot headers are not yet in a block. *)
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:0
  in

  (* slot2_a and slot3 will not be included as successful, because slot2_b has
     better fees for slot4, while slot3's fee is too low. slot4 is not injected
     into L1 or DAL nodes.

     We decide to have two failed slots instead of just one to better test some
     internal aspects of failed slots headers recording (i.e. having a collection
     of data instead of just one). *)
  let* slot2_a = publish Constant.bootstrap3 ~index:4 ~fee:12_000 "test4_a" in
  let* slot2_b = publish Constant.bootstrap4 ~index:4 ~fee:13_500 "test4_b" in
  let* slot3 = publish Constant.bootstrap5 ~index:5 ~fee:1 "test5" in
  let* slot4 =
    let slot = Helpers.make_slot ~slot_size "never associated to a slot_id" in
    let* commit, _proof = Helpers.store_slot dal_node ~slot_index:6 slot in
    return (6, commit)
  in

  Log.info "Just after injecting slots and before baking, there are no headers" ;
  (* because headers are stored based on information from finalized blocks *)
  let* () =
    check_slots_statuses
      ~expected_status:Dal_RPC.Not_found
      dal_node
      ~slot_level:level
      ~__LOC__
      [slot0; slot1; slot2_a; slot2_b; slot3; slot4]
  in
  let* () =
    (* The slot headers are still not yet in a block. *)
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:0
  in

  let wait_block_processing1 = wait_for_layer1_head dal_node pub_level in
  let* () = bake_for client in
  let* () = wait_block_processing1 in

  Log.info
    "After baking one block, there is still no header, because the block is \
     not final" ;
  let* () =
    check_slots_statuses
      dal_node
      ~expected_status:Dal_RPC.Not_found
      ~slot_level:level
      ~__LOC__
      [slot0; slot1; slot2_a; slot2_b; slot3; slot4]
  in

  let wait_block_processing2 = wait_for_layer1_final_block dal_node pub_level in
  let* () = bake_for ~count:2 client in
  let* () = wait_block_processing2 in

  Log.info
    "After baking two more blocks, the slots' status is as expected (eg for \
     published slots it's Waiting_attestation)" ;
  let ok = [slot0; slot1; slot2_b] in
  let ko = slot3 :: slot4 :: List.map (fun (i, c) -> (i + 100, c)) ok in
  let* () =
    (* There are 3 published slots: slot0, slot1, and slot2_b. But the attestation
       period is not over, so they are not yet present in the skip-list. *)
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:0
  in
  let check_slots_statuses ~expected_status ?check_attested_lag l =
    check_slots_statuses
      ~expected_status
      ?check_attested_lag
      dal_node
      ~slot_level:pub_level
      l
  in
  let check_get_commitment =
    check_get_commitment dal_node ~slot_level:pub_level
  in
  let check_published_commitment ?(expected_same = true) =
    let encapsulate = Format.sprintf "\"%s\"\n" in
    check_get_commitment (fun expected response ->
        let response_body = response.RPC_core.body in
        if response_body = encapsulate expected = expected_same then ()
        else
          Test.fail
            ~__LOC__
            "Published commitment is %s whereas we expected %s%s"
            response_body
            (if expected_same then "" else "not ")
            expected)
  in
  (* Slots waiting for attestation. *)
  let* () = check_get_commitment get_commitment_not_found ok in
  let* () =
    check_slots_statuses
      ~__LOC__
      ~expected_status:Dal_RPC.Waiting_attestation
      ok
  in
  (* slot_2_a is not selected. *)
  let* () =
    check_slots_statuses
      ~expected_status:Dal_RPC.Waiting_attestation
      ~__LOC__
      [slot2_a]
  in
  (* Slots not published or not included in blocks. *)
  let* () = check_get_commitment get_commitment_not_found ko in

  let lag = parameters.attestation_lag in
  Log.info
    "Attest slots slot0 and slot2_b, and wait for the attestations to be final" ;
  let attested = [slot0; slot2_b] in
  let unattested = [slot1] in
  let* () = bake_for ~count:(lag - 3) client in
  let wait_block_processing3 =
    let attested_level = pub_level + lag in
    wait_for_layer1_final_block dal_node attested_level
  in
  let* () =
    inject_dal_attestations_and_bake
      ~protocol
      node
      client
      (Slots (List.map fst attested))
      parameters
  in
  let* () = bake_for ~count:2 client in
  let* () = wait_block_processing3 in
  let* () =
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:3
  in

  Log.info "Check that the store is as expected" ;
  (* Slots confirmed. *)
  let* () = check_get_commitment get_commitment_succeeds attested in
  (* Slots that were waiting for attestation and now attested. *)
  let* () =
    check_slots_statuses
      ~__LOC__
      ~expected_status:(Dal_RPC.Attested lag)
      ~check_attested_lag:`At_most
      attested
  in
  (* Slots not published or not included in blocks. *)
  let* () = check_get_commitment get_commitment_not_found ko in
  (* Slots that were waiting for attestation and now unattested. *)
  let* () =
    check_slots_statuses ~expected_status:Dal_RPC.Unattested ~__LOC__ unattested
  in
  (* slot2_a is still not selected. *)
  let* () = check_published_commitment ~expected_same:false [slot2_a] in
  (* slot2_b is selected. *)
  let* () = check_published_commitment [slot2_b] in
  (* slot3 never finished in an L1 block, so the DAL node did not store a status for it. *)
  let* () =
    check_slots_statuses ~expected_status:Dal_RPC.Unpublished ~__LOC__ [slot3]
  in
  (* slot4 is never injected in any of the nodes. So, it's not
     known by the Dal node. *)
  let* () =
    check_slots_statuses ~expected_status:Dal_RPC.Unpublished ~__LOC__ [slot4]
  in
  (* The number of stored slots has not changed. *)
  let* () =
    check_stored_level_headers
      ~__LOC__
      ~pub_level:(pub_level - 1)
      ~number_of_headers:0
  in
  let* () =
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:3
  in
  check_stored_level_headers
    ~__LOC__
    ~pub_level:(pub_level + 1)
    ~number_of_headers:0

let get_shards dal_node ~slot_level ~slot_index downloaded_shard_ids =
  Lwt_list.map_s
    (fun shard_index ->
      Dal_RPC.(
        call dal_node
        @@ get_level_slot_shard_content ~slot_level ~slot_index ~shard_index))
    downloaded_shard_ids

let test_dal_node_rebuild_from_shards _protocol parameters cryptobox node client
    dal_node =
  (* Steps in this integration test:
     1. Run a dal node
     2. Generate and publish a full slot, then bake
     3. Download exactly 1/redundancy_factor shards
        from this slot (it would work with more)
     4. Ensure we can rebuild the original data using the above shards
  *)
  let crypto_params = parameters.Dal.Parameters.cryptobox in
  let slot_size = crypto_params.slot_size in
  let slot_content = generate_dummy_slot slot_size in
  let publish = Helpers.publish_and_store_slot client dal_node in
  let slot_index = 0 in
  let* _commitment =
    publish Constant.bootstrap1 ~index:slot_index
    @@ Helpers.make_slot ~slot_size slot_content
  in
  let* () = bake_for client in
  let* slot_level = Node.wait_for_level node 1 in
  let* () = bake_until_processed ~level:slot_level client [dal_node] in
  let number_of_shards =
    (crypto_params.number_of_shards / crypto_params.redundancy_factor) - 1
  in
  let downloaded_shard_ids =
    range 0 number_of_shards
    |> List.map (fun i -> i * crypto_params.redundancy_factor)
  in
  let* shards =
    get_shards dal_node ~slot_level ~slot_index downloaded_shard_ids
  in
  let shard_of_json shard =
    let shard =
      match Data_encoding.Json.from_string shard with
      | Ok s -> s
      | Error _ -> Test.fail "shard RPC sent invalid json"
    in
    let shard = Data_encoding.Json.destruct Cryptobox.shard_encoding shard in
    ({index = shard.index; share = shard.share} : Cryptobox.shard)
  in
  let shards = shards |> List.to_seq |> Seq.map shard_of_json in
  let reformed_slot =
    match Cryptobox.polynomial_from_shards cryptobox shards with
    | Ok p -> Cryptobox.polynomial_to_slot cryptobox p |> Bytes.to_string
    | Error _ -> Test.fail "Fail to build polynomial from shards"
  in
  Check.(reformed_slot = slot_content)
    Check.(string)
    ~error_msg:
      "Reconstructed slot is different from original slot (current = %L, \
       expected = %R)" ;
  return ()

let test_dal_node_rpc_list _protocol _parameters _cryptobox _node client
    dal_node =
  let endpoint = Client.Foreign_endpoint (Dal_node.as_rpc_endpoint dal_node) in
  let* (_ : string) = Client.rpc_list ~hooks ~endpoint client in
  unit

let commitment_of_slot cryptobox slot =
  let polynomial =
    Cryptobox.polynomial_from_slot
      cryptobox
      (Helpers.content_of_slot slot |> Bytes.of_string)
    |> Result.get_ok
  in
  match Cryptobox.commit cryptobox polynomial with
  | Ok cm -> cm
  | Error
      ((`Invalid_degree_strictly_less_than_expected _ | `Prover_SRS_not_loaded)
       as commit_error) ->
      Test.fail "%s" (Cryptobox.string_of_commit_error commit_error)

let test_dal_node_test_post_slot _protocol parameters cryptobox _node client
    dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let mk_slot size =
    Helpers.make_slot ~padding:false ~slot_size (generate_dummy_slot size)
  in
  let failing_post_slot_rpc ?slot_index ~body_rex slot =
    let* response = Dal_RPC.(call_raw dal_node @@ post_slot ?slot_index slot) in
    return @@ RPC_core.check_string_response ~body_rex ~code:500 response
  in
  let size = parameters.Dal.Parameters.cryptobox.slot_size in
  let slot_big = mk_slot (size + 1) in
  let slot_small = mk_slot (size - 1) in
  let slot_ok = mk_slot size in
  let* () = failing_post_slot_rpc ~body_rex:"post_slot_too_large" slot_big in
  let* commitment1, _proof = Dal_RPC.(call dal_node @@ post_slot slot_ok) in
  let* commitment2, _proof = Dal_RPC.(call dal_node @@ post_slot slot_ok) in
  (* TODO/DAL: https://gitlab.com/tezos/tezos/-/issues/4250
     The second RPC call above succeeeds, but the (untested) returned HTTP status
     should likely be 200 and 201 in the first similar RPC call.
  *)
  Check.(commitment1 = commitment2)
    Check.string
    ~error_msg:
      "Storing a slot twice should return the same commitment (current = %L, \
       expected = %R)" ;

  (* The DAL node of this test can only publish on slot index 0. *)
  let* () =
    failing_post_slot_rpc
      ~slot_index:1
      ~body_rex:"cannot_publish_on_slot_index"
      slot_small
  in
  let slot_index = 0 in
  let* commitment3, proof3 =
    Dal_RPC.(call dal_node @@ post_slot ~slot_index slot_small)
  in
  (* To retrieve the content of the slot corresponding to commitment3,
     we need to publish the commitment on the L1 and bake some blocks
     to finalize the publication. *)
  let* _ =
    Dal.Helpers.publish_commitment
      ~source:Constant.bootstrap1
      ~index:slot_index
      ~commitment:(Dal.Commitment.of_string commitment3)
      ~proof:(Dal.Commitment.proof_of_string proof3)
      client
  in
  let* () = bake_for client in
  let* slot_level = Client.level client in
  (* Finalize the publication. *)
  let* () = bake_until_processed ~level:slot_level client [dal_node] in

  (* The POST /slots RPC accepts slots shorter than the size and pads
     them.  The content_of_slot helper removes the padding. *)
  let* padded_slot =
    Dal_RPC.(call dal_node @@ get_level_slot_content ~slot_level ~slot_index)
  in
  Check.(generate_dummy_slot (size - 1) = Helpers.content_of_slot padded_slot)
    Check.string
    ~error_msg:
      "A slot shorter than the expected size should be padded by the POST \
       /slots RPC expected %L, got %R." ;
  let commitment4 =
    Cryptobox.Commitment.to_b58check @@ commitment_of_slot cryptobox slot_ok
  in
  Check.(commitment1 = commitment4)
    Check.string
    ~error_msg:
      "The commitment of a stored commitment should match the one computed \
       locally (current = %L, expected = %R)" ;
  unit

let test_dal_node_test_get_level_slot_content _protocol parameters _cryptobox
    _node client dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let slot = Helpers.make_slot ~slot_size (generate_dummy_slot slot_size) in
  let slot_index = 0 in
  let* commitment, proof = Dal_RPC.(call dal_node @@ post_slot slot) in
  let* _ =
    Dal.Helpers.publish_commitment
      ~source:Constant.bootstrap1
      ~index:slot_index
      ~commitment:(Dal.Commitment.of_string commitment)
      ~proof:(Dal.Commitment.proof_of_string proof)
      client
  in
  let* () = bake_for client in
  let* slot_level = Client.level client in
  (* Publication is not yet finalized, the RPC should fail with 404
     error code. *)
  let* () =
    let* response =
      Dal_RPC.(
        call_raw dal_node @@ get_level_slot_content ~slot_level ~slot_index)
    in
    return @@ RPC_core.check_string_response ~code:404 response
  in
  (* Finalize the publication. *)
  let* () = bake_until_processed ~level:slot_level client [dal_node] in
  let* got_slot =
    Dal_RPC.(call dal_node @@ get_level_slot_content ~slot_level ~slot_index)
  in
  Check.(Helpers.content_of_slot slot = Helpers.content_of_slot got_slot)
    Check.string
    ~error_msg:
      "The slot content retrieved from the node is not as expected (expected = \
       %L, got = %R)" ;
  unit

(** Helper to verify that expected slot data is present and matches source *)
let verify_expected_slots_present dal_node_source dal_node_target tests_ok =
  Log.info "Checking expected slot data is present..." ;
  Lwt_list.iter_s
    (fun (slot_level, slot_index) ->
      let* status_orig =
        Dal_RPC.(
          call dal_node_source @@ get_level_slot_status ~slot_level ~slot_index)
      in
      let* () =
        check_slot_status
          ~__LOC__
          ~expected_status:status_orig
          ~check_attested_lag:`Exact
          dal_node_target
          ~slot_level
          ~slot_index
      in
      let* content_orig =
        Dal_RPC.(
          call dal_node_source @@ get_level_slot_content ~slot_level ~slot_index)
        |> Lwt.map Helpers.content_of_slot
      in
      let* content_target =
        Dal_RPC.(
          call dal_node_target @@ get_level_slot_content ~slot_level ~slot_index)
        |> Lwt.map Helpers.content_of_slot
      in
      Check.(content_target = content_orig)
        ~__LOC__
        Check.string
        ~error_msg:
          (Format.sprintf
             "Slot data mismatch for (level=%d,index=%d)"
             slot_level
             slot_index) ;
      (* Also verify shard data (shard 0) is present and matches *)
      let shard_index = 0 in
      let* shard_orig =
        Dal_RPC.(
          call dal_node_source
          @@ get_level_slot_shard_content ~slot_level ~slot_index ~shard_index)
      in
      let* shard_target =
        Dal_RPC.(
          call dal_node_target
          @@ get_level_slot_shard_content ~slot_level ~slot_index ~shard_index)
      in
      Check.(shard_target = shard_orig)
        ~__LOC__
        Check.string
        ~error_msg:
          (Format.sprintf
             "Shard data mismatch for (level=%d,index=%d,shard=0)"
             slot_level
             slot_index) ;
      unit)
    tests_ok

(** Helper to verify that filtered-out slot data is absent *)
let verify_filtered_out_slots_absent dal_node_source dal_node_target tests_error
    =
  Log.info "Checking filtered-out slot data is absent..." ;
  Lwt_list.iter_s
    (fun (slot_level, slot_index) ->
      (* Verify the source node has data *)
      let* _ =
        Dal_RPC.(
          call dal_node_source @@ get_level_slot_content ~slot_level ~slot_index)
      in
      (* Verify the target node does not *)
      let* response =
        Dal_RPC.(
          call_raw dal_node_target
          @@ get_level_slot_content ~slot_level ~slot_index)
      in
      RPC_core.check_string_response ~code:404 response ;
      unit)
    tests_error

(** [check_snapshot_variation] exports and imports snapshots from [dal_node]
    into a fresh DAL node, and verifies that the expected data is present
    and the filtered-out data is absent.

    [published] is the list of [(level, index)] pairs published to the source
    node. [upper_bound_exported] is the exclusive upper bound of levels that the
    source store is expected to contain (capped by the export's
    [latest_frozen_level]). [stop] is the baking stop level, used to compute
    the maximum finalized level ([stop - 2]).

    [import_steps] is a list of import configurations. Each configuration is
    a tuple of (step_name, filters) where filters are:
    (slots_exported, slots_imported, min_level_export, max_level_export,
     min_level_import, max_level_import).

    If [import_steps] has a single element, it performs a single import
    (classic snapshot test). If it has multiple elements, it performs
    sequential imports into the same DAL node to validate metadata merge
    behavior when importing into a non-empty store.

    When not provided, filter options default to no filtering. *)
let check_snapshot_variation ~operators ~name ~published ~upper_bound_exported
    ~stop ?(compress = false)
    ?(import_steps = [("single", (None, None, None, None, None, None))]) node
    dal_node =
  (* Create a fresh DAL node for all imports *)
  let fresh_dal_node = Dal_node.create ~node () in
  let* () = Dal_node.init_config ~operator_profiles:operators fresh_dal_node in

  (* Helper to perform a single import *)
  let perform_import
      (step_name, (slots_exp, slots_imp, min_exp, max_exp, min_imp, max_imp)) =
    let entry_in_export (level, index) =
      level < upper_bound_exported
      && (match min_exp with None -> true | Some m -> level >= m)
      && (match max_exp with None -> true | Some m -> level <= m)
      && match slots_exp with None -> true | Some l -> List.mem index l
    in
    let entry_in_import (level, _index) =
      (match min_imp with None -> true | Some m -> level >= m)
      && match max_imp with None -> true | Some m -> level <= m
    in
    let entry_ok entry =
      entry_in_export entry && entry_in_import entry
      &&
      let _level, index = entry in
      match slots_imp with None -> true | Some l -> List.mem index l
    in
    let step_data = List.filter entry_ok published in

    let start_level =
      match List.rev published with (l, _) :: _ -> l | [] -> assert false
    in
    let export_min =
      match min_exp with
      | Some l -> Int32.of_int l
      | None -> Int32.of_int start_level
    in
    let export_max =
      match max_exp with Some l -> Int32.of_int l | None -> Int32.of_int stop
    in

    let snapshot_file = Temp.file ("snapshot-" ^ name ^ "-" ^ step_name) in
    let* () =
      Dal_node.snapshot_export
        ~compress
        ~min_published_level:export_min
        ~max_published_level:export_max
        ?slots:slots_exp
        ~endpoint:(Node.as_rpc_endpoint node)
        dal_node
        snapshot_file
    in

    let import_min =
      match min_imp with Some l -> Int32.of_int l | None -> export_min
    in
    let import_max =
      match max_imp with Some l -> Int32.of_int l | None -> export_max
    in

    let* () =
      Dal_node.snapshot_import
        ~no_check:true
        ~min_published_level:import_min
        ~max_published_level:import_max
        ?slots:slots_imp
        ~endpoint:(Node.as_rpc_endpoint node)
        fresh_dal_node
        snapshot_file
    in
    Log.info
      "[%s] Step %s: imported %d entries"
      name
      step_name
      (List.length step_data) ;
    return step_data
  in

  (* Perform all imports sequentially and collect expected data *)
  let* all_imported_data =
    Lwt_list.fold_left_s
      (fun acc config ->
        let* step_data = perform_import config in
        return (acc @ step_data))
      []
      import_steps
  in

  (* Start the node after all imports are done to avoid lock contention on
     the data directory during snapshot import. *)
  let* () = Dal_node.run fresh_dal_node in

  (* Verify all imported data is present *)
  let* () =
    verify_expected_slots_present dal_node fresh_dal_node all_imported_data
  in

  (* Verify data not included in any import is absent *)
  let tests_error =
    List.filter
      (fun entry ->
        let level, _ = entry in
        level < stop - 2 && not (List.mem entry all_imported_data))
      published
  in
  let* () =
    verify_filtered_out_slots_absent dal_node fresh_dal_node tests_error
  in

  let* () = Dal_node.terminate fresh_dal_node in
  (match import_steps with
  | [_] -> Log.info "[%s] Single import completed successfully" name
  | _ -> Log.info "[%s] Multi-step import completed successfully" name) ;
  unit

let test_dal_node_snapshot ~operators ?(compress = false) _protocol parameters
    cryptobox node client dal_node =
  (* Publish slots once for all snapshot test variations *)
  let* start = Lwt.map succ (Node.get_level node) in
  let expected_exported_levels = 5 in
  let stop =
    (* We add +2 because, DAL node deals with finalized blocks, which are
       described by the DAL node's block_handler as:
       > A slot header is considered finalized when it is in
       > a block with at least two other blocks on top of it, as guaranteed by
       > Tenderbake. *)
    start + expected_exported_levels
    + parameters.Dal_common.Parameters.attestation_lag
    + Tezos_dal_node_lib.Constants.validation_slack + 2
  in
  (* Set up the wait promise before baking to avoid missing the event.
     The DAL node processes finalized blocks asynchronously; we must wait for
     it to reach stop - 2 before exporting, otherwise latest_frozen_level may
     be too low and not all expected data will be in the frozen zone. *)
  let wait_dal_processed = wait_for_layer1_final_block dal_node (stop - 2) in
  let* published =
    publish_and_bake
      ~slots:operators
      ~from_level:start
      ~to_level:stop
      parameters
      cryptobox
      node
      client
      dal_node
  in
  let* () = wait_dal_processed in
  let upper_bound_exported = start + expected_exported_levels in
  let check ?slots_exported ?slots_imported ?min_level_export ?max_level_export
      ?min_level_import ?max_level_import name =
    let import_steps =
      [
        ( "single",
          ( slots_exported,
            slots_imported,
            min_level_export,
            max_level_export,
            min_level_import,
            max_level_import ) );
      ]
    in
    check_snapshot_variation
      ~operators
      ~name
      ~published
      ~upper_bound_exported
      ~stop
      ~compress
      ~import_steps
      node
      dal_node
  in
  let* () = check "no-filter" in
  let* () = check ~slots_exported:[List.hd operators] "filter-exported" in
  let* () = check ~slots_imported:[List.hd operators] "filter-imported" in
  (* Import only a sub-range of levels from the snapshot *)
  let* () =
    check
      ~min_level_import:(start + 1)
      ~max_level_import:(start + 3)
      "level-filter-import"
  in
  (* Combine slot and level filtering on import *)
  let* () =
    check
      ~slots_imported:[List.hd operators]
      ~min_level_import:(start + 1)
      ~max_level_import:(start + 3)
      "slot-and-level-filter-import"
  in
  (* Export only a sub-range of levels from the store *)
  let* () =
    check
      ~min_level_export:(start + 1)
      ~max_level_export:(start + 3)
      "level-filter-export"
  in
  (* Combine slot and level filtering on export *)
  let* () =
    check
      ~slots_exported:[List.hd operators]
      ~min_level_export:(start + 1)
      ~max_level_export:(start + 3)
      "slot-and-level-filter-export"
  in
  unit

let test_dal_node_import_l1_snapshot _protocol parameters _cryptobox node client
    dal_node =
  let* commitment, proof =
    Helpers.(
      store_slot dal_node ~slot_index:0
      @@ make_slot
           ~slot_size:parameters.Dal_common.Parameters.cryptobox.slot_size
           "content1")
  in
  let* _oph =
    publish_commitment
      ~source:Constant.bootstrap1
      ~index:0
      ~commitment
      ~proof
      client
  in
  let* level = Node.get_level node in
  let* () = bake_for client in
  let* export_level = Node.wait_for_level node (level + 1) in
  let file = Temp.file "snapshot" in
  let* () =
    Node.snapshot_export ~history_mode:Rolling_history ~export_level node file
  in
  let node2 = Node.create [] in
  let* () = Node.config_init node2 [] in
  (* We update the configuration because by default on sandbox mode,
     DAL is not activated. *)
  let config : Cryptobox.Config.t = {activated = true; bootstrap_peers = []} in
  let* () =
    Node.Config_file.update node2 Node.Config_file.set_sandbox_network
  in
  let* () =
    Node.Config_file.update
      node2
      (Node.Config_file.set_network_with_dal_config config)
  in
  let* () = Node.snapshot_import node2 file in
  unit

(** [test_dal_node_snapshot_import_merging] tests importing a snapshot
    into a DAL node that already contains some data. It verifies that:
    1. Data that was present before import remains present
    2. Newly imported data is correctly added
    3. Metadata (first_seen_level, last_processed_level) is properly merged *)
let test_dal_node_snapshot_import_merging ~operators ?(compress = false)
    _protocol parameters cryptobox node client dal_node =
  (* Publish data across multiple levels to test multi-step import *)
  let* start = Lwt.map succ (Node.get_level node) in
  let total_levels = 9 in
  let stop =
    start + total_levels + parameters.Dal_common.Parameters.attestation_lag
    + Tezos_dal_node_lib.Constants.validation_slack + 2
  in
  (* Set up the wait promise before baking to avoid missing the event.
     The DAL node processes finalized blocks asynchronously; we must wait for
     it to reach stop - 2 before exporting, otherwise latest_frozen_level may
     be too low and not all expected data will be in the frozen zone. *)
  let wait_dal_processed = wait_for_layer1_final_block dal_node (stop - 2) in
  let* published =
    publish_and_bake
      ~slots:operators
      ~from_level:start
      ~to_level:stop
      parameters
      cryptobox
      node
      client
      dal_node
  in
  let* () = wait_dal_processed in
  Log.info "Published test data (levels %d-%d)" start (stop - 1) ;

  (* Split the published data into two parts for sequential import *)
  let split_level = start + 5 in
  let upper_bound_exported = start + total_levels in

  (* Test scenario: Import first part, then second part of the same snapshot
     into a fresh DAL node. This validates metadata merge behavior. *)
  let import_steps =
    [
      (* Step 1: Import first part of data (levels start to split_level - 1) *)
      ( "part-1",
        ( None,
          None,
          Some start,
          Some (split_level - 1),
          Some start,
          Some (split_level - 1) ) );
      (* Step 2: Import second part of data (levels split_level to stop - 1) *)
      ( "part-2",
        (None, None, Some split_level, Some stop, Some split_level, Some stop)
      );
    ]
  in

  let* () =
    check_snapshot_variation
      ~operators
      ~name:"snapshot-merge-test"
      ~published
      ~upper_bound_exported
      ~stop
      ~compress
      ~import_steps
      node
      dal_node
  in
  unit

(** Test that exporting a snapshot to a destination that already exists fails
    with an appropriate error for both plain directory and tar formats. *)
let test_dal_node_snapshot_overwrite_fails ~operators _protocol parameters
    cryptobox node client dal_node =
  let* start = Lwt.map succ (Node.get_level node) in
  let stop =
    start + 3 + parameters.Dal_common.Parameters.attestation_lag
    + Tezos_dal_node_lib.Constants.validation_slack + 2
  in
  let* _published =
    publish_and_bake
      ~slots:operators
      ~from_level:start
      ~to_level:stop
      parameters
      cryptobox
      node
      client
      dal_node
  in
  let endpoint = Node.as_rpc_endpoint node in
  let min_published_level = Int32.of_int start in
  let max_published_level = Int32.of_int stop in
  (* Plain directory format: second export to existing destination should fail *)
  let plain_dst = Temp.dir "dal-snapshot-overwrite-plain" in
  let* () =
    Dal_node.snapshot_export
      ~min_published_level
      ~max_published_level
      ~endpoint
      dal_node
      plain_dst
  in
  let* () =
    Process.check_error
      ~msg:(rex "already exists")
      (Dal_node.spawn_snapshot_export
         ~min_published_level
         ~max_published_level
         ~endpoint
         dal_node
         plain_dst)
  in
  (* Tar format: second export to the same base path should fail *)
  let tar_base = Temp.file "dal-snapshot-overwrite-tar" in
  let* () =
    Dal_node.snapshot_export
      ~compress:true
      ~min_published_level
      ~max_published_level
      ~endpoint
      dal_node
      tar_base
  in
  Process.check_error
    ~msg:(rex "already exists")
    (Dal_node.spawn_snapshot_export
       ~compress:true
       ~min_published_level
       ~max_published_level
       ~endpoint
       dal_node
       tar_base)

(** Tests that importing a tar snapshot fails when the 'version' metadata entry
    is missing or contains an incompatible version number.

    The test exports a valid snapshot, then creates two corrupted variants by
    extracting the tar and repacking it with the version entry either removed
    or replaced with a wrong value.  Each variant must produce a specific error
    message on import. *)
let test_dal_node_snapshot_version_check ~operators _protocol parameters
    cryptobox node client dal_node =
  let* start = Lwt.map succ (Node.get_level node) in
  let stop =
    start + 2 + parameters.Dal_common.Parameters.attestation_lag
    + Tezos_dal_node_lib.Constants.validation_slack + 2
  in
  let wait_dal_processed = wait_for_layer1_final_block dal_node (stop - 2) in
  let* _published =
    publish_and_bake
      ~slots:operators
      ~from_level:start
      ~to_level:stop
      parameters
      cryptobox
      node
      client
      dal_node
  in
  let* () = wait_dal_processed in
  let endpoint = Node.as_rpc_endpoint node in
  let min_published_level = Int32.of_int start in
  let max_published_level = Int32.of_int stop in
  (* Export a valid tar snapshot *)
  let snapshot_base = Temp.file "dal-snapshot-version-check" in
  let* () =
    Dal_node.snapshot_export
      ~compress:true
      ~min_published_level
      ~max_published_level
      ~endpoint
      dal_node
      snapshot_base
  in
  let snapshot_file = snapshot_base ^ ".tar" in
  (* Helper: extract [snapshot_file], apply [modify] on the extracted
     directory, then repack it to [output]. *)
  let manipulate_tar ~extract_dir ~modify ~output =
    let rc =
      Sys.command (Printf.sprintf "tar xf %s -C %s" snapshot_file extract_dir)
    in
    if rc <> 0 then Test.fail "tar extract failed (exit code %d)" rc ;
    modify extract_dir ;
    let rc =
      Sys.command
        (Printf.sprintf
           "tar cf %s -C %s $(ls -A %s)"
           output
           extract_dir
           extract_dir)
    in
    if rc <> 0 then Test.fail "tar repack failed (exit code %d)" rc
  in
  (* Test 1: import fails when the version entry is absent *)
  let extract_dir_1 = Temp.dir "snapshot-no-version-extract" in
  let no_version_tar = Temp.file "snapshot-no-version" ^ ".tar" in
  manipulate_tar
    ~extract_dir:extract_dir_1
    ~modify:(fun dir ->
      ignore (Sys.command (Printf.sprintf "rm -f %s/version" dir)))
    ~output:no_version_tar ;
  let fresh_node_1 = Dal_node.create ~node () in
  let* () = Dal_node.init_config fresh_node_1 in
  let* () =
    Process.check_error
      ~msg:(rex "missing version")
      (Dal_node.spawn_snapshot_import
         ~no_check:true
         ~endpoint
         fresh_node_1
         no_version_tar)
  in
  (* Test 2: import fails when the version number does not match *)
  let extract_dir_2 = Temp.dir "snapshot-wrong-version-extract" in
  let wrong_version_tar = Temp.file "snapshot-wrong-version" ^ ".tar" in
  let wrong_version =
    Tezos_dal_node_lib.Snapshot.current_snapshot_version + 1
  in
  manipulate_tar
    ~extract_dir:extract_dir_2
    ~modify:(fun dir ->
      ignore
        (Sys.command
           (Printf.sprintf "printf '%d' > %s/version" wrong_version dir)))
    ~output:wrong_version_tar ;
  let fresh_node_2 = Dal_node.create ~node () in
  let* () = Dal_node.init_config fresh_node_2 in
  Process.check_error
    ~msg:(rex "version mismatch")
    (Dal_node.spawn_snapshot_import
       ~no_check:true
       ~endpoint
       fresh_node_2
       wrong_version_tar)

let test_dal_node_startup ~__FILE__ =
  Protocol.register_test
    ~__FILE__
    ~title:"dal node startup"
    ~tags:[Tag.tezos2; "dal"]
    ~uses:(fun _protocol -> [Constant.octez_dal_node])
    ~supports:Has_predecessor
  @@ fun protocol ->
  let run_dal = Dal_node.run ~wait_ready:false in
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
  let dal_node = Dal_node.create ~node () in
  let* () = Dal_node.init_config dal_node in
  let* () = run_dal dal_node in
  assert (Dal_node.is_running_not_ready dal_node) ;
  let* () = Dal_node.terminate dal_node in
  let* () = Node.terminate node in
  let* () =
    Node.Config_file.update
      node
      (Node.Config_file.set_sandbox_network_with_user_activated_overrides
         [(Protocol.hash previous_protocol, Protocol.hash protocol)])
  in
  let* () = Node.run node nodes_args in
  let* () = Node.wait_for_ready node in
  let* () = run_dal dal_node in
  let* () =
    Lwt.join
      [
        Dal_node.wait_for dal_node "dal_plugin_resolved.v0" (fun _ -> Some ());
        bake_for client;
      ]
  in
  let* () = Dal_node.terminate dal_node in
  return ()

let test_dal_node_invalid_config ~__FILE__ () =
  Test.register
    ~__FILE__
    ~title:"dal node invalid config"
    ~tags:[Tag.tezos2; "dal"; "config"]
    ~uses:[Constant.octez_dal_node]
    ~uses_client:false
    ~uses_admin_client:false
    ~uses_node:false
  @@ fun () ->
  let open Tezt.Base in
  let l1_node_endpoint =
    Endpoint.make ~scheme:"http" ~host:Constant.default_host ~port:8732 ()
  in
  let dal_node = Dal_node.create_from_endpoint ~l1_node_endpoint () in
  let config_file = Dal_node.Config_file.filename dal_node in
  let* correct_config_file =
    let* () = Dal_node.init_config dal_node in
    Lwt.return @@ read_file config_file
  in
  Sys.remove config_file ;
  let illformed_json_config =
    "{ \"version\": 2, \"rpc_addr\": \"127.0.0.1:1111\","
  in
  let wrong_field_config =
    "{\n  \"version\": 2,\n  \"non_existing_field\": \"127.0.0.1:1111\"}"
  in
  let test ~command ?contents ~exit_code ?msg ?check_contents () =
    let () =
      match contents with
      | Some contents -> write_file config_file ~contents
      | None -> (
          try Sys.remove config_file
          with _ -> Test.fail ~__LOC__ "Failed to delete existing config_file")
    in
    let process = command dal_node in
    let* () = Process.check_error ~exit_code ?msg process in
    match check_contents with
    | None -> Lwt.return_unit
    | Some check_contents ->
        let new_contents = read_file config_file in
        check_contents ~new_contents
  in
  let check_contents ~initial_config ~new_contents =
    Lwt.return
    @@ Check.((new_contents = initial_config) string)
         ~error_msg:
           "Invalid config file should be left untouched, got %L expected %R."
  in
  let check_updated_contents ~expected_config ~new_contents =
    Lwt.return
    @@ Check.((new_contents = expected_config) string)
         ~error_msg:"Config file is not what we expected, got %L expected %R."
  in
  let run =
   fun dal_node ->
    Process.spawn
      (Dal_node.path dal_node)
      ((if Dal_node.use_baker_to_start_dal_node then ["dal"; "run"] else ["run"])
      @ ["--data-dir"; Dal_node.data_dir dal_node; "--config-file"; config_file]
      )
  in
  let* () =
    (* running on malformed json fails *)
    test
      ~command:run
      ~contents:illformed_json_config
      ~exit_code:1
      ~msg:(rex "not a valid JSON value")
      ~check_contents:(check_contents ~initial_config:illformed_json_config)
      ()
  in
  let* () =
    (* running on configuration with non-existing fields fails *)
    test
      ~command:run
      ~contents:wrong_field_config
      ~exit_code:1
      ~msg:(rex "Unexpected object field non_existing_field")
      ~check_contents:(check_contents ~initial_config:wrong_field_config)
      ()
  in
  let* () =
    (* config init cannot overwrite config if file exists *)
    test
      ~command:Dal_node.spawn_config_init
      ~contents:illformed_json_config
      ~exit_code:1
      ~msg:(rex "overwriting is forbidden")
      ~check_contents:(check_contents ~initial_config:illformed_json_config)
      ()
  in
  let* () =
    (* config reset succeed if file  exists *)
    test
      ~command:Dal_node.spawn_config_reset
      ~contents:correct_config_file
      ~exit_code:0
      ~check_contents:
        (check_updated_contents ~expected_config:correct_config_file)
      ()
  in
  let* () =
    (* config reset also succeed if existing file is broken *)
    test
      ~command:Dal_node.spawn_config_reset
      ~contents:illformed_json_config
      ~exit_code:0
      ~msg:(rex "")
      ~check_contents:
        (check_updated_contents ~expected_config:correct_config_file)
      ()
  in
  let* () =
    (* config update fails if file doesn't exists *)
    test
      ~command:Dal_node.spawn_config_update
      ~exit_code:1
      ~msg:(rex "is missing")
      ()
  in
  unit

(* Test that the rollup kernel can fetch and store a requested DAL page. Works as follows:
   - Originate a rollup with a kernel that:
      - Downloads page 0 from slot 0 published at level [current_level - attestation_lag].
      - Writes the downloaded slot contents to "/output" in durable storage.
   - At level N, publish a slot to the L1 and DAL.
   - Bake until [attestation_lag] blocks so the L1 attests the published slot.
   - Confirm that the kernel downloaded the slot and wrote the content to "/output/slot-<index>". *)
let test_reveal_dal_page_in_fast_exec_wasm_pvm protocol parameters dal_node
    sc_rollup_node _sc_rollup_address node client pvm_name =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let attestation_lag = parameters.attestation_lag in
  let min_lag = List.hd parameters.attestation_lags in
  Check.(
    (slot_size = 32768)
      int
      ~error_msg:"The kernel used in the test assumes slot_size of %R, got %L") ;
  Check.(
    (parameters.cryptobox.page_size = 128)
      int
      ~error_msg:"The kernel used in the test assumes page_size of %R, got %L") ;
  Log.info "Originate rollup." ;
  let* {boot_sector; _} =
    Sc_rollup_helpers.prepare_installer_kernel
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      Constant.WASM.dal_echo_kernel
  in
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"dal_echo_kernel"
      ~src:Constant.bootstrap1.alias
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~parameters_ty:"unit"
      client
  in
  let* () = bake_for client in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let slot_size = parameters.cryptobox.slot_size in
  Log.info "Store slot content to DAL node and submit header." ;
  let slot_content = generate_dummy_slot slot_size in
  let* level = Node.get_level node in
  let* () =
    publish_store_and_attest_slot
      ~protocol
      client
      node
      dal_node
      Constant.bootstrap1
      ~index:0
      ~content:(Helpers.make_slot ~slot_size slot_content)
      parameters
  in
  let published_level = level + 1 in
  let* current_level = Node.get_level node in
  Log.info
    "Slot published at level %d, currently at level %d"
    published_level
    current_level ;
  Log.info "Assert that the slot was attested." ;
  let attested_levels =
    published_level + min_lag
    --> min (published_level + attestation_lag) current_level
  in
  let* all_slot_availabilities =
    Dal.collect_slot_availabilities node ~attested_levels
  in
  let to_attested_levels =
    Dal.to_attested_levels ~protocol ~dal_parameters:parameters
  in
  let* slot_0_attested =
    Dal.is_slot_attested
      ~endpoint:(Node.as_rpc_endpoint node)
      ~published_level
      ~slot_index:0
      ~to_attested_levels
      all_slot_availabilities
  in
  Check.is_true
    slot_0_attested
    ~error_msg:"Expected slot 0 from published_level to be attested" ;

  Log.info "Wait for the rollup node to catch up to the latest level." ;

  (* Before importing a slot, we wait 2 blocks for finality + 1 block for DAL
     node processing *)
  let* () = bake_for ~count:3 client in
  let* _level = Sc_rollup_node.wait_sync ~timeout:10. sc_rollup_node in

  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node published_level
  in
  Log.info "Read and assert against value written in durable storage." ;
  let key = "/output/slot-0" in
  let* value_written =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:pvm_name
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  let encoded_slot_content =
    match Hex.of_string slot_content with `Hex s -> s
  in
  Check.(
    (Some encoded_slot_content = value_written)
      (option string)
      ~error_msg:"Expected value written in /output/slot-0 to be %L, got %R") ;
  unit

(* Compact snapshot of a rollup that uses [reveal_dal_page] can be imported
   into a fresh rollup node when [--dal-node] is provided. The reconstruction
   phase replays PVM evaluation between the snapshot's first available level
   and head, which fetches the revealed DAL pages from the DAL node. *)
let test_compact_snapshot_with_dal_node protocol parameters dal_node
    sc_rollup_node _sc_rollup_address node client pvm_name =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let attestation_lag = parameters.attestation_lag in
  Check.(
    (slot_size = 32768)
      int
      ~error_msg:"The kernel used in the test assumes slot_size of %R, got %L") ;
  Check.(
    (parameters.cryptobox.page_size = 128)
      int
      ~error_msg:"The kernel used in the test assumes page_size of %R, got %L") ;
  Log.info "Originate rollup with dal_echo_kernel." ;
  let* {boot_sector; _} =
    Sc_rollup_helpers.prepare_installer_kernel
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      Constant.WASM.dal_echo_kernel
  in
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"dal_echo_kernel"
      ~src:Constant.bootstrap1.alias
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~parameters_ty:"unit"
      client
  in
  let* () = bake_for client in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  Log.info "Publish a slot." ;
  let slot_content = generate_dummy_slot slot_size in
  let* level = Node.get_level node in
  let* () =
    publish_store_and_attest_slot
      ~protocol
      client
      node
      dal_node
      Constant.bootstrap1
      ~index:0
      ~content:(Helpers.make_slot ~slot_size slot_content)
      parameters
  in
  let published_level = level + 1 in
  Log.info "Wait for the kernel to reveal the page." ;
  let* () = bake_for ~count:(attestation_lag + 3) client in
  let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:30.
      sc_rollup_node
      (published_level + attestation_lag + 1)
  in
  let key = "/output/slot-0" in
  let encoded_slot_content =
    match Hex.of_string slot_content with `Hex s -> s
  in
  let* value_written =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:pvm_name
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  Check.(
    (Some encoded_slot_content = value_written)
      (option string)
      ~error_msg:
        "Expected the kernel to have revealed the slot to /output/slot-0 (%L) \
         but got %R") ;
  Log.info "Export compact snapshot." ;
  let dir = Tezt.Temp.dir "snapshots" in
  let* snapshot_file =
    Sc_rollup_node.export_snapshot
      ~compact:true
      ~no_check:true
      sc_rollup_node
      dir
    |> Runnable.run
  in
  Log.info "Import compact snapshot in a fresh rollup node with --dal-node." ;
  let new_rollup_node =
    Sc_rollup_node.create
      Observer
      node
      ~base_dir:(Client.base_dir client)
      ~kind:pvm_name
      ~dal_node
  in
  let*! () =
    Sc_rollup_node.import_snapshot
      ~no_check:true
      ~dal_node
      new_rollup_node
      ~snapshot_file
  in
  Log.info "Run new rollup node and check that it has the revealed slot." ;
  let* () = Sc_rollup_node.run new_rollup_node sc_rollup_address [] in
  let* (_ : int) = Sc_rollup_node.wait_sync ~timeout:30. new_rollup_node in
  let* value_after_import =
    Sc_rollup_node.RPC.call new_rollup_node ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:pvm_name
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  Check.(
    (Some encoded_slot_content = value_after_import)
      (option string)
      ~error_msg:
        "Expected the imported rollup node to also have /output/slot-0 = %L \
         but got %R") ;
  unit

let test_dal_node_test_patch_profile _protocol _parameters _cryptobox _node
    _client dal_node =
  let check_bad_attester_pkh_encoding profile =
    let* response = Dal_RPC.(call_raw dal_node @@ patch_profiles [profile]) in
    return @@ RPC_core.check_string_response ~code:400 response
  in
  let patch_profile_rpc profile =
    Dal_RPC.(call dal_node (patch_profiles [profile]))
  in
  let profile1 = Dal_RPC.Attester Constant.bootstrap1.public_key_hash in
  let profile2 = Dal_RPC.Attester Constant.bootstrap2.public_key_hash in
  (* We start with empty profile list *)
  let* () = check_profiles ~__LOC__ dal_node ~expected:(Controller []) in
  (* Adding [Attester] profile with pkh that is not encoded as
     [Tezos_crypto.Signature.Public_key_hash.encoding] should fail. *)
  let* () = check_bad_attester_pkh_encoding (Attester "This is invalid PKH") in
  (* Test adding duplicate profiles stores profile only once *)
  let* () = patch_profile_rpc profile1 in
  let* () = patch_profile_rpc profile1 in
  let* () =
    check_profiles ~__LOC__ dal_node ~expected:(Controller [profile1])
  in
  (* Test adding multiple profiles *)
  let* () = patch_profile_rpc profile2 in
  let* () =
    check_profiles ~__LOC__ dal_node ~expected:(Controller [profile1; profile2])
  in
  (* Test that the patched profiles are persisted after restart using SIGTERM. *)
  let* () = Dal_node.terminate dal_node in
  Log.info "restart DAL node (1)" ;
  let* () = Dal_node.run dal_node ~wait_ready:true in

  let* () =
    check_profiles ~__LOC__ dal_node ~expected:(Controller [profile1; profile2])
  in
  (* Test whether the patched profiles persist after a restart using SIGSTOP
     (that is, even if we stop the DAL node abruptly). *)
  let profile3 = Dal_RPC.Attester Constant.bootstrap3.public_key_hash in
  let* () = patch_profile_rpc profile3 in
  let* () = Dal_node.stop dal_node in
  let* () = Dal_node.kill dal_node in
  Log.info "restart DAL node (2)" ;
  let* () = Dal_node.run dal_node ~wait_ready:true in
  check_profiles
    ~__LOC__
    dal_node
    ~expected:(Controller [profile1; profile2; profile3])

(* Check that result of the DAL node's
   GET /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices
   is consistent with the result of the L1 node GET dal/shards . *)
let test_dal_node_get_assigned_shard_indices _protocol _parameters _cryptobox
    node _client dal_node =
  let pkh = Constant.bootstrap1.public_key_hash in
  let* {level; _} =
    Node.RPC.(call node @@ get_chain_block_helper_current_level ())
  in
  let* committee_from_l1 = Dal.Committee.at_level node ~level () in
  let* shards_from_dal =
    Dal_RPC.(call dal_node @@ get_assigned_shard_indices ~level ~pkh)
  in
  match
    committee_from_l1
    |> List.find_opt (fun member ->
           String.equal member.Dal.Committee.attester pkh)
  with
  | None -> Test.fail ~__LOC__ "pkh %S not found in committee from L1." pkh
  | Some member ->
      let shards_from_l1 = member.indexes in
      Check.(
        (shards_from_dal = shards_from_l1)
          (list int)
          ~error_msg:
            "Shard indexes does not match between DAL and L1  (From DAL: %L <> \
             From L1: %R)") ;
      unit

let test_dal_node_get_attestable_slots _protocol parameters cryptobox node
    client dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let store_slot ~slot_index s =
    Helpers.(store_slot dal_node ~slot_index @@ make_slot ~slot_size s)
  in
  let number_of_slots = parameters.Dal.Parameters.number_of_slots in
  Log.info "Inject the shards of slots 1 and 3." ;
  let slot1_content = "slot 1" in
  let slot2_content = "slot 2" in
  let slot3_content = "slot 3" in
  let* _commitment, _proof = store_slot ~slot_index:0 slot1_content in
  let* _commitment, _proof = store_slot ~slot_index:2 slot3_content in
  Log.info "Publish slots 1 and 2 (inject and bake two blocks)." ;
  let* level = next_level node in
  let publish source ~index message =
    let* _op_hash =
      publish_dummy_slot ~source ~index ~message cryptobox client
    in
    unit
  in
  let* () = publish Constant.bootstrap1 ~index:0 slot1_content in
  let* () = publish Constant.bootstrap2 ~index:2 slot2_content in
  let wait_block_processing = wait_for_layer1_final_block dal_node level in
  (* bake three blocks: at [level + 1] the commitments are published, at [level +
     3] the commitments become final *)
  let* () = bake_for ~count:3 client in
  let* () = wait_block_processing in
  Log.info "Check attestability of slots." ;
  let attested_level = level + parameters.attestation_lag in
  let rec iter i =
    if i < 0 then unit
    else
      let attester = Account.Bootstrap.keys.(i) in
      (* Note: we assume that the key has at least an assigned shard index. *)
      let* res =
        Dal_RPC.(
          call dal_node @@ get_attestable_slots ~attester ~attested_level)
      in
      match res with
      | Not_in_committee ->
          Test.fail "attester %s not in committee" attester.alias
      | Attestable_slots slots ->
          Check.(
            (number_of_slots = List.length slots)
              int
              ~error_msg:"Expected %L slots (got %R)") ;
          (match slots with
          | true :: rest when List.for_all (fun b -> not b) rest ->
              (* 1st slot is attestable; the rest are not: the 2nd is not because
                 the shards are not available; the rest are not because they are not
                 published *)
              ()
          | _ -> Test.fail "Unexpected result") ;
          iter (i - 1)
  in
  let* () = iter (Array.length Account.Bootstrap.keys - 1) in
  Log.info "Check case when pkh not in the DAL committee." ;
  let* new_account = Client.gen_and_show_keys client in
  let* res =
    Dal_RPC.(
      call dal_node
      @@ get_attestable_slots ~attester:new_account ~attested_level)
  in
  match res with
  | Not_in_committee -> return ()
  | Attestable_slots _ ->
      Test.fail "attester %s is in committee!" new_account.alias

let get_validated_dal_attestations_in_mempool node for_level =
  let* mempool_json =
    Node.RPC.call node
    @@ RPC.get_chain_mempool_pending_operations
         ~version:"2"
         ~validated:true
         ~branch_delayed:false
         ~branch_refused:false
         ~refused:false
         ~outdated:false
         ~validation_passes:[0]
         ()
  in
  let validated = JSON.(mempool_json |-> "validated" |> as_list) in
  List.filter
    (fun op ->
      let contents = JSON.(op |-> "contents" |> geti 0) in
      let level = JSON.(contents |-> "level" |> as_int) in
      level = for_level
      && JSON.(contents |-> "kind" |> as_string) |> fun kind ->
         String.equal kind "attestation_with_dal")
    validated
  |> return

(* This test checks that the attester correctly emits attestations, by
   publishing a slot per level for a few levels, then checking that the slots
   are attested or not, depending on whether or not all delegates attested the
   slots. We use [attestation_threshold = 100] to this end; with a smaller
   threshold it is harder to control whether the slot will be attested or not
   (one would have to take into account the distribution of shard indexes to
   delegates).

   There are two variants of the test: with and without the baker daemon. See
   below for the version not using the daemon (only using `bake for`).

   In this version, slots are still published by baking with `bake for`, because
   when running the baker daemon, it is harder to control the time of publishing
   of a slot. See the end-to-end tests for a version without `bake for`.
*)
let test_attester_with_daemon protocol parameters cryptobox node client dal_node
    =
  Check.((parameters.Dal.Parameters.attestation_threshold = 100) int)
    ~error_msg:"attestation_threshold value (%L) should be 100" ;
  let client = Client.with_dal_node client ~dal_node in
  let all_delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun key -> key.Account.alias)
  in
  let run_baker delegates target_level =
    let* baker =
      let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
      Agnostic_baker.init
        ~event_sections_levels:[(Protocol.name protocol ^ ".baker", `Debug)]
        ~dal_node_rpc_endpoint
        ~delegates
        ~state_recorder:true
        node
        client
    in
    let* _ = Node.wait_for_level node target_level in
    Agnostic_baker.terminate baker
  in

  (* Test goal: the published slot at levels in [first_level, intermediary_level - 1]
     should be attested, the one at levels in at levels in [intermediary_level,
     max_level - 1] should not be attested. *)
  let* first_level = next_level node in
  let intermediary_level =
    (* We want at least two levels for which the published slot is attested. *)
    first_level + 2
  in
  let max_level =
    (* We want at least two levels for which the published slot is unattested;
       we add 2 for "safety"; see discussion (D) below. *)
    intermediary_level + 2 + 2
  in
  (* We want [max_level - attestation_lag < first_level], so that the
     delegates that attest at the last level baked by `bake for` (that is, at
     [max_level]) have no published slot to attest, in order not interfere
     with the attestations done by the baker daemon. *)
  Check.((parameters.attestation_lag > max_level - first_level) int)
    ~error_msg:
      "attestation_lag (%L) should be higher than [max_level - first_level] \
       (which is %R)" ;
  let wait_block_processing = wait_for_layer1_head dal_node max_level in
  let* _ =
    publish_and_bake
      ~from_level:first_level
      ~to_level:max_level
      parameters
      cryptobox
      node
      client
      dal_node
  in
  let* () = wait_block_processing in
  let last_level_of_first_baker =
    intermediary_level + parameters.attestation_lag
  in
  let last_level_of_second_baker = max_level + parameters.attestation_lag + 1 in
  (* We need this level to be processed by the DAL node in order to make the
     necessary checks. *)
  let wait_for_dal_node =
    wait_for_layer1_final_block
      dal_node
      (max_level + parameters.attestation_lag - 1)
  in

  Log.info
    "Run the first baker for all delegates till at least level %d."
    last_level_of_first_baker ;
  let* () = run_baker all_delegates last_level_of_first_baker in
  (* (D) We tried to stop the baker as soon as it reaches
     [intermediary_level + attestation_lag], but it may have baked a few
     blocks more *)
  let* actual_intermediary_level = Node.get_level node in
  Log.info "The first baker baked till level %d." actual_intermediary_level ;

  let* dal_attestations =
    get_validated_dal_attestations_in_mempool node actual_intermediary_level
  in
  let num_dal_attestations = List.length dal_attestations in
  Log.info
    "The number of validated DAL attestations in the mempool is %d."
    num_dal_attestations ;

  (* Let L := actual_intermediary_level and PL := L - lag, the corresponding
     publish level.  The second baker may build a new block (1) at level L (and
     a higher round) or (2) directly at level L+1. However, independently of
     this, when it builds the block at level L+1, it should take into account
     the DAL attestations at level L that are present in the mempool. If there
     already 5 attestations then the slot at PL should be attested. If not, it
     cannot be attested because the second baker may only include, when in case
     (2), 4 new attestations. *)
  let first_not_attested_published_level =
    if num_dal_attestations = List.length all_delegates then
      actual_intermediary_level + 2 - parameters.attestation_lag
    else actual_intermediary_level + 1 - parameters.attestation_lag
  in
  Log.info
    "We set first_not_attested_published_level to %d."
    first_not_attested_published_level ;

  Log.info
    "Run the second baker for some (not all) delegates till at least level %d."
    last_level_of_second_baker ;
  let* () = run_baker (List.tl all_delegates) last_level_of_second_baker in
  let* () = wait_for_dal_node in

  Log.info "Check the attestation status of the published slots." ;
  let rec check_attestations level =
    if level >= max_level then return ()
    else
      (* Before [first_not_attested_published_level], it should be [attested],
         and above (and including) [first_not_attested_published_level], it
         should be [unattested]. *)
      let expected_status =
        let open Dal_RPC in
        if level < first_not_attested_published_level then
          Attested parameters.attestation_lag
        else Unattested
      in
      let* () =
        check_slot_status
          ~__LOC__
          dal_node
          ~expected_status
          ~check_attested_lag:`Exact
          ~slot_level:level
          ~slot_index:(slot_idx parameters level)
      in
      check_attestations (level + 1)
  in
  check_attestations first_level

(* This is the version of [test_attester_with_daemon] that does not use the
   baker daemon, only `bake for`.

   Test goal: verify that slots are attested when all delegates participate,
   and remain unattested when a delegate is missing (threshold = 100%).

   With multi-lag attestations a slot published at level P can be attested at
   levels P+lag[1], P+lag[2], etc. For a slot to be attested, it must be attested
   at ANY of those opportunities. For a slot to remain unattested, it must fail
   to be attested at ALL opportunities.

   Scenario:
   - Phase 1 (all delegates baking): covers all attestation opportunities for
     the attested publication window.
   - Phase 2 (missing delegate): covers all attestation opportunities for the
     unattested publication window. *)
let test_attester_with_bake_for _protocol parameters cryptobox node client
    dal_node =
  Check.((parameters.Dal.Parameters.attestation_threshold = 100) int)
    ~error_msg:"attestation_threshold value (%L) should be 100" ;
  let client = Client.with_dal_node client ~dal_node in

  let max_lag = parameters.attestation_lag in
  let min_lag = List.fold_left Int.min max_int parameters.attestation_lags in

  let all_delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun key -> key.Account.alias)
  in
  let not_all_delegates = List.tl all_delegates in

  (* Number of publication levels to check in each category. *)
  let attested_levels = 2 in
  let unattested_levels = 2 in

  (* Attested window: [first_attested_level, last_attested_level]
   For these publications to be attested, we need ANY attestation opportunity
   to fall in phase 1. The test conservatively ensures ALL opportunities fall
   in phase 1 by extending phase 1 to cover up to [last_attested_level + max_lag]. *)
  let* first_attested_level = next_level node in
  let last_attested_level = first_attested_level + attested_levels - 1 in

  (* Unattested window: [first_unattested_level, last_unattested_level]
     These publications must have ALL their attestation opportunities in phase 2.
     First opportunity for [first_unattested_level] is at [first_unattested_level + min_lag].
     Phase 2 starts at (last_attested_level + max_lag + 1), so we need
    [first_unattested_level + min_lag >= last_attested_level + max_lag + 1]. *)
  let first_unattested_level = last_attested_level + max_lag - min_lag + 1 in
  let last_unattested_level = first_unattested_level + unattested_levels - 1 in

  let last_level = last_unattested_level + max_lag + 1 in

  Log.info "min_lag = %d, max_lag = %d" min_lag max_lag ;
  Log.info
    "Attested window: [%d, %d], Unattested window: [%d, %d]"
    first_attested_level
    last_attested_level
    first_unattested_level
    last_unattested_level ;

  let wait_level = last_level + 1 in
  let wait_block_processing_on_l1 = wait_for_layer1_head dal_node wait_level in

  let wait_block_processing_on_dal =
    wait_for_layer1_final_block dal_node (wait_level - 2)
  in

  (* Phase 1: Publish attested slots, then bake until phase 1 ends. *)
  let* _ =
    publish_and_bake
      ~from_level:first_attested_level
      ~to_level:(last_attested_level + max_lag)
      ~delegates:(`For all_delegates)
      parameters
      cryptobox
      node
      client
      dal_node
  in

  (* Phase 2: Publish unattested slots, then bake until all opportunities pass. *)
  let* _ =
    publish_and_bake
      ~from_level:(last_attested_level + max_lag + 1)
      ~to_level:last_level
      ~delegates:(`For not_all_delegates)
      parameters
      cryptobox
      node
      client
      dal_node
  in

  let* () = bake_for client in
  let* () = wait_block_processing_on_l1 in
  let* () = wait_block_processing_on_dal in
  let* current_level = Node.get_level node in
  Log.info "current level is %d" current_level ;
  Log.info "Check the attestation status of the published slots." ;

  let rec check_range ~from_level ~to_level ~expected_status =
    if from_level > to_level then unit
    else
      let* () =
        check_slot_status
          ~__LOC__
          dal_node
          ~expected_status
          ~check_attested_lag:`At_most
          ~slot_level:from_level
          ~slot_index:(slot_idx parameters from_level)
      in
      check_range ~from_level:(from_level + 1) ~to_level ~expected_status
  in

  let* () =
    check_range
      ~from_level:first_attested_level
      ~to_level:last_attested_level
      ~expected_status:(Attested max_lag)
  in

  check_range
    ~from_level:first_unattested_level
    ~to_level:last_unattested_level
    ~expected_status:Unattested

(** End-to-end DAL Tests.  *)

let test_operator_profile _protocol _dal_parameters _cryptobox _node _client
    dal_node =
  let index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator index])) in
  let* () =
    check_profiles
      ~__LOC__
      dal_node
      ~expected:Dal_RPC.(Controller [Operator index])
  in
  unit

let monitor_finalized_levels_events ~__LOC__ ~last_notified_level ~target_level
    dal_node =
  let next_finalized_level = ref (last_notified_level + 1) in
  Dal_node.wait_for dal_node "dal_new_L1_final_block.v0" (fun e ->
      let finalized_level = JSON.(e |-> "level" |> as_int) in
      Check.(
        (finalized_level = !next_finalized_level)
          ~__LOC__
          int
          ~error_msg:"Expected next finalized level to be %R (got %L)") ;
      incr next_finalized_level ;
      if finalized_level = target_level then Some finalized_level else None)

(** The following test checks various things related to the DAL node crawler:
- We check that the content of the file "last_processed_level" storing the last
  processed finalized level by the crawler is updated correctly;
- We restart the L1 node to check that the DAL node doesn't crash;
- We restart the DAL node and bake meanwhile. Then, we check that it's able to
  catch up;
- We check that every finalized level is processed exactly once by the crawler.
*)
let test_dal_node_crawler_reconnects_to_l1 _protocol _dal_parameters _cryptobox
    node client dal_node =
  (* The number of blocks we make in a raw before some additional checks or
     nodes behaviour change. *)
  let num_blocks = 5 in
  (* The following helper function allows baking [num_blocks]. Then, it waits
     for the DAL node's L1 crawler to process those blocks. Finally, it checks
     that disk storage of the last finalized processed level is correctly
     updated. *)
  let bake_delta_blocks_and_check_last_processed_level () =
    let* target_level =
      let* level0 = Client.level client in
      return (level0 + num_blocks)
    in
    let finalized_target = target_level - 2 in
    let wait_l1_crawler_processing =
      wait_for_layer1_head dal_node target_level
    in
    let wait_finalized_target =
      wait_for_layer1_final_block dal_node finalized_target
    in

    let* () = bake_for ~count:num_blocks client in
    let* () = wait_l1_crawler_processing in
    let* () = wait_finalized_target in
    let* written_last_finalized_level =
      Dal_node.load_last_finalized_processed_level dal_node
    in
    let written_last_finalized_level =
      Option.value ~default:0 written_last_finalized_level
    in
    Check.(
      (finalized_target = written_last_finalized_level)
        int
        ~error_msg:"Expected last processed finalized level %R (got %L)") ;
    unit
  in
  let* () = bake_for ~count:num_blocks client in

  (* For this first part of the test, we will bake [num_blocks] blocks
     twice. The two baking sessions are separated by an L1 node restart. *)
  Log.info "1.a Bake some blocks and check the content of last_processed_level" ;
  let* start_level = Client.level client in
  let finalized_levels_events =
    let start_finalized_level = start_level - 2 in
    assert (start_finalized_level > 0) ;
    monitor_finalized_levels_events
      dal_node
      ~__LOC__
      ~last_notified_level:start_finalized_level
      ~target_level:(start_finalized_level + (2 * num_blocks))
  in
  let* () = bake_delta_blocks_and_check_last_processed_level () in
  Log.info "1.b Restart L1 node, bake and check last_processed_level's content" ;
  let* () = Node.terminate ~timeout:10. node in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* () = bake_delta_blocks_and_check_last_processed_level () in
  let* last_finalized_level = finalized_levels_events in

  (* For this second part of the test, we stop the DAL node, bake [num_blocks]
     blocks with [bake_for] function, then, we restart the DAL node and bake
     additional [num_blocks] with our internal helper. *)
  Log.info "2. Stop DAL node, bake, restart it and check that it catchs-up" ;
  let* () = Dal_node.terminate ~timeout:10. dal_node in

  (* Here, we bake some blocks to make the DAL node's crawler a little bit
     late. *)
  let* () = bake_for ~count:num_blocks client in

  let* head_level = Client.level client in
  (* before the crawler is started, the bootstrap phase advances the
     [last_processed_level] (so [last_notified_level]) to the following
     value: *)
  let last_notified_level = head_level - 3 in

  (* Restart the DAL node, the finalized events watcher promise, and wait until
     the node is ready. We wait for the node to be ready after spawning an
     instance of seen_finalized_per_level_events to prevent the test from being
     flaky, in case the crawler starts processing finalized blocks and emitting
     "dal_node_layer_1_new_final_block" events before we start observing
     them. *)
  let* () = Dal_node.run ~wait_ready:false dal_node in

  let finalized_levels_events =
    monitor_finalized_levels_events
      dal_node
      ~__LOC__
      ~last_notified_level
      ~target_level:(last_finalized_level + (2 * num_blocks))
  in
  let* () = Dal_node.wait_for_ready dal_node in
  let* () = bake_delta_blocks_and_check_last_processed_level () in

  let* last_finalized_level = finalized_levels_events in
  let* expected_final_finalized_level =
    let* level = Client.level client in
    return @@ (level - 2)
  in
  Check.(
    (last_finalized_level = expected_final_finalized_level)
      int
      ~error_msg:"Expected last processed finalized level %R (got %L)") ;
  unit

(* We run a L1 node and stop the DAL node for a period and then restart it. If
   the DAL node should participate in refutations we restart it after more than
   2 cycles, but less than the history's mode length. Otherwise, we restart it
   less than the L1's history length. In both cases the DAL node should not fail
   when restarted. To make the distinction between the two cases we use the
   profile of the initial DAL node. It is expected to be either a bootstrap
   node, who stores data for [2*attestation_lag] blocks or a producer node who
   stores much more data. *)
let test_restart_dal_node protocol dal_parameters _cryptobox node client
    dal_node =
  let* history_mode = Node.RPC.call node @@ RPC.get_config_history_mode in
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let blocks_preservation_cycles =
    JSON.(history_mode |-> "blocks_preservation_cycles" |> as_int)
  in
  let additional_cycles =
    JSON.(
      history_mode |-> "history_mode" |-> "rolling" |-> "additional_cycles"
      |> as_int)
  in
  let l1_history_length =
    (blocks_preservation_cycles + additional_cycles) * blocks_per_cycle
  in
  let* profile = Dal_RPC.(call dal_node @@ get_profiles ()) in
  let offline_period =
    if profile = Dal_RPC.Bootstrap then l1_history_length + blocks_per_cycle
    else
      (* Cap to 2 cycles while stopped (seed retention is 2). *)
      (2 * blocks_per_cycle) - 1
  in
  let stop_level = 16 in
  let restart_level = stop_level + offline_period in
  let last_finalized_level =
    restart_level + dal_parameters.Dal.Parameters.attestation_lag
  in

  Log.info
    "We let the DAL node run for a few levels (till level %d)."
    stop_level ;

  let* () =
    let* level = Node.get_level node in
    bake_for ~count:(stop_level - level) client
  in

  Log.info "We then stop it." ;
  let* () = Dal_node.terminate dal_node in

  let* () =
    let* level = Node.get_level node in
    bake_for ~count:(restart_level - level) client
  in
  Log.info "We then restart it at level %d." restart_level ;
  let* () = Dal_node.run dal_node in
  let wait_for_dal_node =
    wait_for_layer1_final_block dal_node last_finalized_level
  in
  let* () =
    bake_for ~count:(dal_parameters.Dal.Parameters.attestation_lag + 2) client
  in
  let* () = wait_for_dal_node in

  if profile <> Dal_RPC.Bootstrap then
    let expected_levels =
      if Protocol.number protocol >= 025 then
        (* first level with cells is level 2; not very clear why, but it should
           not matter *)
        List.init (last_finalized_level - 1) (fun i -> string_of_int (i + 2))
      else
        List.init
          (last_finalized_level - dal_parameters.attestation_lag)
          (fun i -> string_of_int (i + 1))
    in
    check_skip_list_store
      dal_node
      ~number_of_slots:dal_parameters.number_of_slots
      ~expected_levels
  else
    check_skip_list_store
      dal_node
      ~number_of_slots:dal_parameters.number_of_slots
      ~expected_levels:[]

(** Test: dal_slots_retrievability

    This test checks that a DAL node can retrieve historical slot data using the
    --slots-backup-uri option, from various archive sources (file:// URIs), and
    appropriately validates slot integrity when --trust-slots-backup-uris is
    disabled.

    The test proceeds in three phases:

    A. Node setup:
       - Launch 3 DAL producer nodes (dal_pub1, dal_pub2, dal_pub3),
         each publishing a distinct slot (index 1, 2, 3 respectively).
       - Launch 4 DAL fetcher nodes:
         - valid_dal_fetcher_1_2: untrusted sources, has access to dal_pub1
           and dal_pub2
         - valid_dal_fetcher_3_trusted: trusted sources, access to dal_pub3
         - invalid_dal_fetcher_bad_uri: untrusted sources, invalid URI
         - invalid_dal_fetcher_bad_uri_trusted: trusted sources, invalid URI

    B. Slot publication:
       - Each dal_pubX publishes one slot at the same published_level.
       - A few blocks are baked to reach finality and ensure the slots are
         attested and commitments are available in memory, sqlite, and L1 context.
       - Publishers are then terminated to free memory (their store
         directories remain on disk for file:// backup URI reads).

    C. Retrieval and validation checks:
       C.1 - Fetch valid slots via memory cache
       C.2 - Fetch valid slots after restarting the DAL node (sqlite skip list)
       C.4 - Fetch slot from trusted archive (no validation)
       C.5 - Attempt to fetch from a missing archive (expected 404)
       C.6 - Tamper slot content to trigger commitment mismatch (expected 404)
       C.7 - Tamper slot size to trigger size mismatch (expected 404)
       C.8 - Use invalid archive URI (expected 500 with resolution errors)
       C.9 - Try to fetch slot with no commitment on L1 (expected 500)
       C.10 - Try to fetch an unpublished slot for an old level without any
              cryptobox available (expected 404)
       C.11 - Try future level (expected 404)
       C.12 - Try out-of-bounds slot index (expected 404)
       C.3 - Fetch via L1 skip list after removing sqlite DB: commitment found
             via L1 fallback but slot not in backup archives (expected 404)
       C.13 - Observer DAL node (no local sqlite skip list) uses L1 skip list
              fallback for commitment verification and successfully fetches slot 3
              from archive3 (untampered); contrast with C.3 which returns 404
              because archive1/archive2 don't hold slot 3

    The same test can be triggered by requesting shards instead of slots.
*)
let dal_slots_retrievability =
  (* Helper to run the RPC that fetches a slot from a given DAL node *)
  let get_slot_rpc dal_node ~published_level ~slot_index =
    Dal_RPC.(
      call_raw dal_node
      @@ get_level_slot_content ~slot_level:published_level ~slot_index)
  in

  (* Wait for a specific event in DAL node logs *)
  let wait_for_event_promise dal_node event_name =
    Dal_node.wait_for
      dal_node
      (Format.sprintf "dal_%s.v0" event_name)
      (fun _event -> Some ())
  in

  (* Assert the fetch succeeded with HTTP 200 *)
  let fetch_succeeded ~__LOC__ promise =
    let* RPC_core.{code; body; _} = promise in
    if code = 200 then unit
    else
      Test.fail ~__LOC__ "Unexpected response %d instead of 200.\n%s" code body
  in

  (* Assert the fetch failed with 404, optionally wait for expected event *)
  let fetch_404_expected ~__LOC__ ?expected_event promise =
    let* RPC_core.{code; body; _} = promise in
    if code = 404 then Option.value expected_event ~default:unit
    else
      Test.fail ~__LOC__ "Unexpected response %d instead of 404.\n%s" code body
  in

  (* Assert the fetch failed with 500, optionally check error message and wait for event *)
  let fetch_500_expected ~__LOC__ ?expected_error ?expected_event promise =
    let* RPC_core.{code; body; _} = promise in
    if code = 500 then
      let () =
        match expected_error with
        | Some msg ->
            Check.((body =~ rex msg) ~error_msg:"expected error =~ %R, got %L")
        | None -> ()
      in
      Option.value expected_event ~default:unit
    else
      Test.fail ~__LOC__ "Unexpected response %d instead of 500.\n%s" code body
  in

  (* Main test body *)
  fun protocol
      dal_parameters
      ~(store_kind : [`Slots | `Shards])
      _cryptobox
      l1_node
      client
      original_dal_node
    ->
    (* A. NODE SETUP *)
    let* () = Dal_node.terminate original_dal_node in
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let number_of_slots = dal_parameters.number_of_slots in
    let attestation_lag = dal_parameters.attestation_lag in

    let mk_dal_pub ~idx ~name =
      make_dal_node ~operator_profiles:[idx] ~name l1_node
    in
    let store_uri dal_node =
      let fragment =
        match store_kind with `Slots -> "#slots" | `Shards -> "#shards"
      in
      Format.sprintf "file://%s%s" (store_path dal_node store_kind) fragment
    in

    let* dal_pub1 = mk_dal_pub ~idx:1 ~name:"dal_pub1" in
    let* dal_pub2 = mk_dal_pub ~idx:2 ~name:"dal_pub2" in
    let* dal_pub3 = mk_dal_pub ~idx:3 ~name:"dal_pub3" in
    let archive1 = store_uri dal_pub1 in
    let archive2 = store_uri dal_pub2 in
    let archive3 = store_uri dal_pub3 in

    (* Launch four fetchers with different backup configurations *)
    let* valid_dal_fetcher_1_2 =
      make_dal_node
        ~operator_profiles:[0]
        ~name:"valid_dal_fetcher_1_2"
        ~slots_backup_uris:[archive1; archive2]
        ~event_level:`Notice
        l1_node
    in
    let* valid_dal_fetcher_3_trusted =
      make_dal_node
        ~operator_profiles:[0]
        ~name:"valid_dal_fetcher_3_trusted"
        ~slots_backup_uris:[archive3]
        ~trust_slots_backup_uris:true
        ~event_level:`Notice
        l1_node
    in
    let* invalid_dal_fetcher_bad_uri =
      make_dal_node
        ~operator_profiles:[5]
        ~name:"invalid_dal_fetcher_bad_uri"
        ~slots_backup_uris:["http://some-fake.endpoint"]
        ~event_level:`Notice
        l1_node
    in
    let* invalid_dal_fetcher_bad_uri_trusted =
      make_dal_node
        ~operator_profiles:[6]
        ~name:"invalid_dal_fetcher_bad_uri_trusted"
        ~slots_backup_uris:["http://some-fake.endpoint"]
        ~trust_slots_backup_uris:true
        ~event_level:`Notice
        l1_node
    in

    (* Observer node: never writes skip list cells to SQLite (observer profile
       does not support refutations).  Points its backup URI at archive3 so it
       can fetch slot 3 once the L1 fallback supplies the commitment.
       archive1 and archive2 are tampered by C.6 and C.7 respectively, so
       archive3 (untouched) is the only reliable source. *)
    let* observer_dal =
      let dal_node = Dal_node.create ~name:"observer_dal" ~node:l1_node () in
      let* () =
        Dal_node.init_config
          ~observer_profiles:[3]
          ~slots_backup_uris:[archive3]
          dal_node
      in
      let* () = Dal_node.run ~event_level:`Notice dal_node ~wait_ready:true in
      return dal_node
    in

    (* B. SLOT PUBLICATION *)
    let* start_level = Node.get_level l1_node in
    let published_level = start_level + 1 in
    let* () =
      Lwt_list.iteri_p
        (fun idx dal_publisher ->
          let pub_idx = idx + 1 in
          let source = Account.Bootstrap.keys.(pub_idx) in
          let* _commit =
            Format.sprintf "Slot<%d, %d> is cool ..." published_level pub_idx
            |> Helpers.make_slot ~slot_size
            |> Helpers.publish_and_store_slot
                 client
                 dal_publisher
                 source
                 ~index:pub_idx
          in
          unit)
        [dal_pub1; dal_pub2; dal_pub3]
    in
    let attested_level = published_level + attestation_lag in
    let wait_for_dal_nodes =
      Lwt.join
      @@ List.map
           (fun dal_node -> wait_for_layer1_final_block dal_node attested_level)
           [
             valid_dal_fetcher_1_2;
             valid_dal_fetcher_3_trusted;
             invalid_dal_fetcher_bad_uri;
             invalid_dal_fetcher_bad_uri_trusted;
             observer_dal;
           ]
    in
    let* () = bake_for ~count:attestation_lag client in

    (* Now we make sure that slots are DAL attested. *)
    let some_baker, rest_pkhs =
      let some_baker = Account.Bootstrap.keys.(0) in
      let all_pkhs =
        Account.Bootstrap.keys |> Array.to_list
        |> List.map (fun account -> account.Account.public_key_hash)
      in
      (some_baker, List.tl all_pkhs)
    in
    let* _ =
      inject_dal_attestations
        ~protocol
          (* Since the attestation threshold of the test is set to 0%,
             having only one signer is sufficient. *)
        ~signers:[some_baker]
        (Slots [1; 2; 3])
        client
        dal_parameters
        (Node.as_rpc_endpoint l1_node)
    in

    let* () = bake_for ~count:3 ~delegates:(`For rest_pkhs) client in
    let* () = wait_for_dal_nodes in

    (* Terminate publishers to free memory — their store directories
       remain on disk for file:// backup URI reads. *)
    let* () = Dal_node.terminate dal_pub1 in
    let* () = Dal_node.terminate dal_pub2 in
    let* () = Dal_node.terminate dal_pub3 in

    (* C. RETRIEVAL & VALIDATION *)
    let check_valid_dal_fetcher_1_2 ~__LOC__ =
      Lwt_list.iter_s
        (fun slot_index ->
          get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index
          |> fetch_succeeded ~__LOC__)
        [1; 2]
    in

    (* C.1 Fetch valid slots via memory cache *)
    let* () =
      Log.info "C.1: memory cache" ;
      check_valid_dal_fetcher_1_2 ~__LOC__
    in

    (* C.2 Restart and fetch via SQLite skip list *)
    let* () =
      Log.info "C.2: sqlite skip list" ;
      let* () = Dal_node.terminate valid_dal_fetcher_1_2 in
      let* () = Dal_node.run valid_dal_fetcher_1_2 in
      check_valid_dal_fetcher_1_2 ~__LOC__
    in

    (* C.4 Fetch from trusted archive (no validation) *)
    let* () =
      Log.info "C.4: trusted archive" ;
      get_slot_rpc valid_dal_fetcher_3_trusted ~published_level ~slot_index:3
      |> fetch_succeeded ~__LOC__
    in

    (* C.5 Missing archive for a slot index: expect 404 *)
    let* () =
      Log.info "C.5: missing archive for slot index 3" ;
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index:3
      |> fetch_404_expected ~__LOC__
    in

    let store_path1 = store_path dal_pub1 store_kind in
    let store_path2 = store_path dal_pub2 store_kind in
    let data_path1 =
      data_path store_path1 store_kind ~slot_size ~published_level ~slot_index:1
    in
    let data_path2 =
      data_path store_path2 store_kind ~slot_size ~published_level ~slot_index:2
    in

    (* C.6 Tamper slot1 content: expect 404 + event *)
    let* () =
      Log.info "C.6: Tamper data content" ;
      let () =
        Sys.command (Format.sprintf "cp %s %s" data_path2 data_path1)
        |> fun exit_code -> assert (exit_code = 0)
      in
      let expected_event =
        wait_for_event_promise
          valid_dal_fetcher_1_2
          "slot_from_backup_has_unexpected_commitment"
      in
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index:1
      |> fetch_404_expected ~__LOC__ ~expected_event
    in

    (* C.7 Tamper slot2 size: expect 404 + event *)
    let* () =
      if store_kind = `Shards then
        let () =
          Log.info "C.7: Tamper slot2 size disabled for shard archives"
        in
        unit
      else
        let () = Log.info "C.7: Tamper slot2 size" in
        let () = Sys.command ("echo bad > " ^ data_path2) |> ignore in
        let expected_event =
          wait_for_event_promise
            valid_dal_fetcher_1_2
            "slot_from_backup_has_unexpected_size"
        in
        get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index:2
        |> fetch_404_expected ~__LOC__ ~expected_event
    in

    (* C.8 Invalid URI (untrusted): expect 500 *)
    let* () =
      Log.info "C.8: invalid URI trusted & untrusted" ;
      let* () =
        get_slot_rpc invalid_dal_fetcher_bad_uri ~published_level ~slot_index:2
        |> fetch_500_expected
             ~__LOC__
             ~expected_error:"resolution failed: name resolution failed"
      in
      get_slot_rpc
        invalid_dal_fetcher_bad_uri_trusted
        ~published_level
        ~slot_index:2
      |> fetch_500_expected
           ~__LOC__
           ~expected_error:"resolution failed: name resolution failed"
    in

    (* C.9 No commitment on L1 on the given slot index: expect 500 *)
    let* () =
      Log.info "C.9: no commitment on L1" ;
      get_slot_rpc invalid_dal_fetcher_bad_uri ~published_level ~slot_index:4
      |> fetch_500_expected
           ~__LOC__
           ~expected_error:"No_commitment_found_for_slot_id"
    in

    (* C.10 No slot published at level 0: expect 404 *)
    let* () =
      Log.info "C.10: no slot published at level 0: no cryptobox" ;
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level:0 ~slot_index:1
      |> fetch_404_expected ~__LOC__
    in

    (* C.11 Future level: expect 404 *)
    let* () =
      Log.info "C.11: future level" ;
      get_slot_rpc
        valid_dal_fetcher_3_trusted
        ~published_level:1000000
        ~slot_index:1
      |> fetch_404_expected ~__LOC__
    in

    (* C.12 Out-of-bounds slot index: expect 404 *)
    let* () =
      Log.info "C.12: out-of-bounds index" ;
      get_slot_rpc
        valid_dal_fetcher_3_trusted
        ~published_level
        ~slot_index:number_of_slots
      |> fetch_404_expected ~__LOC__
    in

    (* C.3 Remove sqlite DB: L1 fallback finds the commitment for slot 3 but
       the backup archives (archive1, archive2) don't hold it — expect 404. *)
    let* () =
      Log.info
        "C.3: L1 skip list fallback — commitment found, slot not in archives" ;
      let skip_db =
        Format.sprintf "%s/store/skip_list_store"
        @@ Dal_node.data_dir valid_dal_fetcher_1_2
      in
      let () = Sys.command ("rm -rf " ^ skip_db) |> ignore in
      let* () = Dal_node.terminate valid_dal_fetcher_1_2 in
      let* () = Dal_node.run valid_dal_fetcher_1_2 in
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index:3
      |> fetch_404_expected ~__LOC__
    in

    (* C.13 Observer DAL node uses L1 skip list fallback for commitment
       verification: observer profile never populates the local SQLite skip
       list, so the commitment for slot 3 must be retrieved from L1.  The
       backup URI (archive3) holds the slot bytes, so the fetch succeeds.
       Contrast with C.3: same slot 3, but valid_dal_fetcher_1_2 only has
       archive1/archive2 so it gets 404; observer_dal has archive3 so it
       gets 200. *)
    let* () =
      Log.info "C.13: observer L1 skip list fallback — fetch succeeds" ;
      get_slot_rpc observer_dal ~published_level ~slot_index:3
      |> fetch_succeeded ~__LOC__
    in

    unit

(* A simpler test whith a bootstrap, a producer and an observer. The goal is to
   check regression on [get_connections] RPC response. *)
let test_rpc_get_connections _protocol dal_parameters _cryptobox node client
    dal_bootstrap =
  (* In this test we have three DAL nodes:
     - a bootstrap one to connect the other two (producer and observer),
     - a slot producer on slot 0,
     - an observer on slot 0. *)
  let index = 0 in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let peers = [Dal_node.listen_addr dal_bootstrap] in
  let peer_id dal_node = Dal_node.read_identity dal_node in

  (* Check that the dal node passed as argument to this test function
     is a running bootstrap DAL node. If not, this means that we
     forgot to register the test with ~bootstrap_profile:true *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] ~peers producer in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug producer in
  let* producer_peer_id = peer_id producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator index])
  in
  Log.info "Slot producer DAL node is running" ;

  let observer = Dal_node.create ~name:"observer" ~node () in
  let* () = Dal_node.init_config ~observer_profiles:[index] ~peers observer in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug observer in
  let* observer_peer_id = peer_id observer in
  let* () =
    check_profiles
      ~__LOC__
      observer
      ~expected:Dal_RPC.(Controller [Observer index])
  in
  Log.info "Observer DAL node is running" ;

  let all_pkhs =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun account -> account.Account.public_key_hash)
  in

  (* Wait for a GRAFT message between the observer and the producer,
     in any direction. *)
  let check_graft pkh =
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index:index
         (observer, observer_peer_id)
         (producer, producer_peer_id)
         pkh
  in
  let check_graft_promises = List.map check_graft all_pkhs in
  Log.info "Waiting for grafting of the observer - producer connection" ;

  (* We need to bake some blocks until the L1 node notifies the DAL
     nodes that some L1 block is final so that the topic pkhs are
     known. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join check_graft_promises in
  Log.info "Observer - producer connection grafted" ;

  (* Calling RPC on each node *)
  Log.info "Producer get_connections" ;
  let* producer_connections =
    Dal_RPC.call producer (Dal_RPC.get_gossipsub_connections ())
  in
  Log.info "Observer get_connections" ;
  let* observer_connections =
    Dal_RPC.call observer (Dal_RPC.get_gossipsub_connections ())
  in
  Log.info "Bootstrap get_connections" ;
  let* bootstrap_connections =
    Dal_RPC.call dal_bootstrap (Dal_RPC.get_gossipsub_connections ())
  in

  let sort list =
    List.sort
      (fun (peer1, _, _) (peer2, _, _) -> String.compare peer1 peer2)
      list
    |> List.map (fun (peer, bootstrap, topics) ->
           let topics =
             List.sort
               (fun (i1, pkh1) (i2, pkh2) ->
                 let index = Int.compare i1 i2 in
                 if index = 0 then String.compare pkh1 pkh2 else index)
               topics
           in
           (peer, bootstrap, topics))
  in
  let eq l1 l2 =
    List.equal
      (fun (peer1, bootstrap1, topics1) (peer2, bootstrap2, topics2) ->
        peer1 = peer2 && bootstrap1 = bootstrap2
        && List.equal (fun a b -> a = b) topics1 topics2)
      (sort l1)
      (sort l2)
  in

  let parse_connections connections =
    JSON.(connections |> as_list)
    |> List.map
         JSON.(
           fun json ->
             let peer = json |-> "peer" |-> "peer_id" |> as_string in
             let connection = json |-> "connection" in
             let bootstrap = connection |-> "bootstrap" |> as_bool in
             let topics =
               connection |-> "topics" |> as_list
               |> List.map (fun t ->
                      (t |-> "slot_index" |> as_int, t |-> "pkh" |> as_string))
             in
             (peer, bootstrap, topics))
  in
  let* bootstrap = Dal_node.read_identity dal_bootstrap in
  let* observer = Dal_node.read_identity observer in
  let* producer = Dal_node.read_identity producer in
  let expected_connections peers_id =
    List.map
      (fun peer_id ->
        let bootstrap = peer_id = bootstrap in
        let topics =
          if bootstrap then [] else List.map (fun pkh -> (0, pkh)) all_pkhs
        in
        (peer_id, bootstrap, topics))
      peers_id
  in

  let parsed_bootstrap = parse_connections bootstrap_connections in
  let expected_bootstrap = expected_connections [observer; producer] in
  Check.(eq parsed_bootstrap expected_bootstrap = true)
    ~__LOC__
    Check.bool
    ~error_msg:"Unexpected result for bootstrap get_connections" ;

  let parsed_observer = parse_connections observer_connections in
  let expected_observer = expected_connections [bootstrap; producer] in
  Check.(eq parsed_observer expected_observer = true)
    ~__LOC__
    Check.bool
    ~error_msg:"Unexpected result for observer get_connections" ;

  let parsed_producer = parse_connections producer_connections in
  let expected_producer = expected_connections [observer; bootstrap] in
  Check.(eq parsed_producer expected_producer = true)
    ~__LOC__
    Check.bool
    ~error_msg:"Unexpected result for producer get_connections" ;

  unit

let test_disable_shard_validation_wrong_cli _protocol _parameters _cryptobox
    _node _client dal_node =
  Dal_node.check_error
    dal_node
    ~msg:
      (rex
         ".* DAL shard validation is disabled but the option \
          '--disable-shard-validation' was not provided.*")

let test_disable_shard_validation_wrong_env _protocol _parameters _cryptobox
    _node _client dal_node =
  Dal_node.check_error
    dal_node
    ~msg:
      (rex
      @@ Format.sprintf
           ".* DAL shard validation is enabled but the environment variable %s \
            was not set.*"
           Dal_node.disable_shard_validation_environment_variable)

let test_ignore_topics_wrong_cli _protocol _parameters _cryptobox _node _client
    dal_node =
  Dal_node.check_error
    dal_node
    ~msg:
      (rex
      @@ Format.sprintf
           ".* The environment variable to ignore topics %s was set, but the \
            option '--ignore-topics' was not provided.*"
           Dal_node.ignore_topics_environment_variable)

let test_ignore_topics_wrong_env _protocol _parameters _cryptobox _node _client
    dal_node =
  Dal_node.check_error
    dal_node
    ~msg:
      (rex
      @@ Format.sprintf
           ".* The option '--ignore-topics' was provided, but the environment \
            variable to ignore topics %s was not set.*"
           Dal_node.ignore_topics_environment_variable)

let test_statuses_backfill_at_restart _protocol dal_parameters _cryptobox
    _l1_node client dal_node =
  let index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let* _ =
    Helpers.publish_and_store_slot client dal_node Constant.bootstrap1 ~index
    @@ Helpers.make_slot ~slot_size "Hello world!"
  in
  let* lvl_inject = Client.level client in
  let slot_level = lvl_inject + 1 in
  Log.info "Bake a few blocks" ;
  let* () = bake_for ~count:3 client in
  let* () = Dal_node.terminate dal_node in
  let* () = bake_for ~count:3 client in
  let* () = Dal_node.run dal_node in
  let* () =
    check_slot_status
      ~__LOC__
      dal_node
      ~expected_status:Waiting_attestation
      ~slot_level
      ~slot_index:index
  in
  let* () =
    check_slot_status
      ~__LOC__
      dal_node
      ~expected_status:Unpublished
      ~slot_level
      ~slot_index:(index + 1)
  in
  unit

(** Check that bakers do not reattest the same DAL content, with the introduction
    of multiple lags.

    Setup:
    - consensus_threshold_size = consensus_committee_size * 2/3.
    - Two baker processes run in parallel:
        baker_a: bootstrap1, bootstrap2 (~2/5 of committee)
        baker_b: bootstrap3, bootstrap4 (~2/5 of committee)
    - Together they control ~4/5 > 2/3 of the committee, so the chain
      advances normally.
    - After the publication phase, we terminate baker_b for a few seconds.
      With only baker_a's ~2/5 of attestations, the threshold cannot be
      reached, so no round 0 block can be proposed at the next level.
    - We restart baker_b as baker_b2; the chain advances at round > 0.
    - This forces the baker to reattest at a higher round for the
      current level, but it must NOT reattest at a later lag for the
      same published_level. *)
let test_no_redundant_dal_attestations protocol parameters _cryptobox node
    client dal_node =
  let client = Client.with_dal_node client ~dal_node in
  let attestation_lags = parameters.Dal.Parameters.attestation_lags in
  let min_lag = List.hd attestation_lags in
  let max_lag = parameters.attestation_lag in
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let slot_index = 0 in

  let delegates_a =
    [Account.Bootstrap.keys.(0).alias; Account.Bootstrap.keys.(1).alias]
  in
  let delegates_b =
    [Account.Bootstrap.keys.(2).alias; Account.Bootstrap.keys.(3).alias]
  in

  let run_baker delegates =
    let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
    Agnostic_baker.init
      ~event_sections_levels:[(Protocol.name protocol ^ ".baker", `Debug)]
      ~dal_node_rpc_endpoint
      ~delegates
      ~state_recorder:true
      node
      client
  in

  let* first_level = next_level node in
  let max_level = first_level + 3 in

  (* Both bakers run in parallel, covering the publish window. *)
  let* baker_a = run_baker delegates_a in
  let* baker_b = run_baker delegates_b in

  let rec publish_slots level =
    if level > max_level then unit
    else
      let slot_content = generate_dummy_slot slot_size in
      let* _ =
        Helpers.publish_and_store_slot
          client
          dal_node
          Account.Bootstrap.keys.(level mod 4)
          ~index:slot_index
          (Helpers.make_slot ~slot_size slot_content)
      in
      let* current_level = Node.wait_for_level node level in
      Log.info "Slot publication injected, node at level %d" current_level ;
      publish_slots (level + 1)
  in
  let* () = publish_slots first_level in

  Log.info "Waiting for level %d" (max_level + min_lag) ;
  let* _ = Node.wait_for_level node (max_level + min_lag) in
  Log.info "Level %d reached, stop one baker" (max_level + min_lag) ;
  let* () = Agnostic_baker.terminate baker_b in

  let sleep_time = 1. in
  Log.info
    "Sleep %.1f to have a high round block, then restart baker"
    sleep_time ;
  let* () = Lwt_unix.sleep sleep_time in
  let* baker_b2 = run_baker delegates_b in

  let last_level = max_level + max_lag in
  Log.info "Waiting for level %d" last_level ;
  let* _ = Node.wait_for_level node last_level in
  Log.info "Level %d reached" last_level ;
  let* () = Agnostic_baker.terminate baker_a in
  let* () = Agnostic_baker.terminate baker_b2 in

  (* Maps (published_level, slot_index) -> (first_consensus_level, first_lag_index). *)
  let first_seen : (int * int, int * int) Hashtbl.t = Hashtbl.create 64 in
  let rec check_level level =
    if level > last_level then unit
    else
      let* ops =
        Node.RPC.call node
        @@ RPC.get_chain_block_operations_validation_pass
             ~validation_pass:0
             ~block:(string_of_int level)
             ()
      in
      let* () =
        Lwt_list.iter_s
          (fun op ->
            let contents = JSON.(op |-> "contents" |> as_list) |> List.hd in
            let kind = JSON.(contents |-> "kind" |> as_string) in
            let delegate =
              JSON.(contents |-> "metadata" |-> "delegate" |> as_string)
            in
            if
              String.equal kind "attestation_with_dal"
              && String.equal delegate Constant.bootstrap1.public_key_hash
            then
              let dal_str =
                JSON.(contents |-> "dal_attestation" |> as_string)
              in
              let* decoded =
                Dal.Attestations.decode
                  protocol
                  (Node.as_rpc_endpoint node)
                  parameters
                  dal_str
              in
              Lwt_list.iteri_s
                (fun lag_index lag ->
                  let published_level = level - lag in
                  if
                    published_level >= first_level
                    && published_level <= max_level
                    && lag_index < Array.length decoded
                    && slot_index < Array.length decoded.(lag_index)
                    && decoded.(lag_index).(slot_index)
                  then
                    let key = (published_level, slot_index) in
                    match Hashtbl.find_opt first_seen key with
                    | None ->
                        return @@ Hashtbl.add first_seen key (level, lag_index)
                    | Some (first_level, first_lag_index)
                      when lag_index <> first_lag_index ->
                        Test.fail
                          "Slot %d published at level %d:\n\
                           - First attested at level=%d (lag=%d)\n\
                           - Re-attested at level=%d (lag=%d)"
                          slot_index
                          published_level
                          first_level
                          (List.nth attestation_lags first_lag_index)
                          level
                          lag
                    | Some _ -> unit
                  else unit)
                attestation_lags
            else unit)
          (JSON.as_list ops)
      in
      check_level (level + 1)
  in
  let* () = check_level (first_level + 1) in
  unit

let register ~__FILE__ ~protocols =
  test_dal_node_startup ~__FILE__ protocols ;
  test_dal_node_invalid_config ~__FILE__ () ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    "dal node slot management"
    test_dal_node_slot_management
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0; 1; 2; 3; 4; 5; 6]
    "dal node slot headers tracking"
    test_dal_node_slots_headers_tracking
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    "dal node shard fetching and slot reconstruction"
    test_dal_node_rebuild_from_shards
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["rpc"]
    ~regression:true
    ~prover:false
    "dal node list RPCs"
    test_dal_node_rpc_list
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    "dal node POST /slots"
    test_dal_node_test_post_slot
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    "dal node GET /levels/<level>/slots/<index>/content"
    test_dal_node_test_get_level_slot_content
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~prover:false
    "dal node PATCH+GET /profiles"
    test_dal_node_test_patch_profile
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~prover:false
    "dal node GET \
     /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices"
    test_dal_node_get_assigned_shard_indices
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    "dal node GET \
     /profiles/<public_key_hash>/attested_levels/<level>/attestable_slots"
    ~operator_profiles:[0; 1; 2]
    test_dal_node_get_attestable_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~attestation_threshold:100
    ~number_of_slots:8
    ~operator_profiles:[0; 1; 2; 3; 4; 5; 6; 7]
    "dal attester with bake for"
    test_attester_with_bake_for
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~attestation_threshold:100
    ~attestation_lag:8
    ~activation_timestamp:Now
    ~number_of_slots:8
    ~operator_profiles:[0; 1; 2; 3; 4; 5; 6; 7]
      (* when consensus_rights_delay = 1 and attestation_lag = 16,
         blocks_per_cycle must be at least 16:
         attestation_lag <= consensus_rights_delay * blocks_per_cycle *)
    ~blocks_per_cycle:16
    "dal attester with baker daemon"
    test_attester_with_daemon
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["l1_snapshot"; "import"]
    ~operator_profiles:[0]
    "dal node import l1 snapshot"
    test_dal_node_import_l1_snapshot
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["snapshot"; Tag.slow]
    ~operator_profiles:[0; 3]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    "dal node snapshot export/import"
    (test_dal_node_snapshot ~operators:[0; 3])
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~tags:["baker"; "dal"; "attestation"; "redundant"; "multi_lag"]
    ~operator_profiles:[0]
    ~activation_timestamp:Now
    ~consensus_committee_size:256
    ~consensus_threshold_size:171 (* 2/3 * 256 *)
    "No redundant DAL attestations with multi-lag"
    test_no_redundant_dal_attestations
    (List.filter (fun p -> Protocol.number p >= 025) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["snapshot"; "merge"]
    ~operator_profiles:[0; 3]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    "dal node snapshot import with merging into existing store"
    (test_dal_node_snapshot_import_merging ~operators:[0; 3])
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["snapshot"; "tar"]
    ~operator_profiles:[0; 3]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    "dal node snapshot export/import (tar)"
    (test_dal_node_snapshot ~operators:[0; 3] ~compress:true)
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["snapshot"; "merge"; "tar"]
    ~operator_profiles:[0; 3]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    "dal node snapshot import with merging into existing store (tar)"
    (test_dal_node_snapshot_import_merging ~operators:[0; 3] ~compress:true)
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["snapshot"; "overwrite"]
    ~operator_profiles:[0; 3]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    "dal node snapshot export to existing destination fails"
    (test_dal_node_snapshot_overwrite_fails ~operators:[0; 3])
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["snapshot"; "tar"; "version"; Tag.memory_hungry]
    ~operator_profiles:[0; 3]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    "dal node snapshot import rejects missing or wrong version"
    (test_dal_node_snapshot_version_check ~operators:[0; 3])
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["gossipsub"; "rpc"]
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    "GS/RPC get_connections"
    test_rpc_get_connections
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["operator"; "profile"]
    "operator profile"
    ~prover:false
    test_operator_profile
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["crawler"; "reconnection"]
    "DAL node crawler reconnects to L1 without crashing (non-producer case)"
    ~prover:false
    test_dal_node_crawler_reconnects_to_l1
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["crawler"; "reconnection"]
    "DAL node crawler reconnects to L1 without crashing (producer case)"
    ~operator_profiles:[0]
    test_dal_node_crawler_reconnects_to_l1
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["restart"]
    ~operator_profiles:[0]
    ~l1_history_mode:(Custom (Rolling (Some 5)))
    "restart DAL node (producer)"
    test_restart_dal_node
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["restart"]
    ~observer_profiles:[0]
    (* Use default L1 history (Default_without_refutation): observer profile
       requires shard_retention_period_in_levels (150 levels ≈ 19 cycles),
       so we cannot use a small fixed rolling window like the producer test. *)
    "restart DAL node (observer)"
    test_restart_dal_node
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["restart"]
    ~bootstrap_profile:true
    "restart DAL node (bootstrap)"
    test_restart_dal_node
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["http"; "backup"; "retrievability"; Tag.extra; Tag.memory_hungry]
    ~operator_profiles:[0]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    ~number_of_slots:8
    ~attestation_threshold:0
    "fetching slots from backup sources"
    (dal_slots_retrievability ~store_kind:`Slots)
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["http"; "backup"; "retrievability"; Tag.extra; Tag.memory_hungry]
    ~operator_profiles:[0]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    ~number_of_slots:8
    ~attestation_threshold:0
    "fetching shards from backup sources"
    (dal_slots_retrievability ~store_kind:`Shards)
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["restart"; "statuses"]
    "Status information is backfilled at restart"
    ~operator_profiles:[0]
    test_statuses_backfill_at_restart
    protocols ;
  scenario_with_all_nodes
    ~__FILE__
    ~operator_profiles:[0]
    "test reveal_dal_page in fast exec wasm pvm"
    ~uses:(fun _protocol ->
      [Constant.smart_rollup_installer; Constant.WASM.dal_echo_kernel])
    ~tags:[Tag.memory_hungry]
    ~pvm_name:"wasm_2_0_0"
    ~number_of_shards:256
    ~slot_size:(1 lsl 15)
    ~redundancy_factor:8
    ~page_size:128
    test_reveal_dal_page_in_fast_exec_wasm_pvm
    protocols ;
  scenario_with_all_nodes
    ~__FILE__
    ~regression:false
    ~operator_profiles:[0]
    "compact snapshot import with --dal-node"
    ~uses:(fun _protocol ->
      [Constant.smart_rollup_installer; Constant.WASM.dal_echo_kernel])
    ~tags:["snapshot"; "compact"; "import"; Tag.memory_hungry]
    ~pvm_name:"wasm_2_0_0"
    ~number_of_shards:256
    ~slot_size:(1 lsl 15)
    ~redundancy_factor:8
    ~page_size:128
    test_compact_snapshot_with_dal_node
    protocols ;
  (* Scenarios for disabling shard validation *)
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~wait_ready:false
    ~env:
      (String_map.singleton
         Dal_node.disable_shard_validation_environment_variable
         "yes")
    "DAL node disable shard validation wrong CLI"
    test_disable_shard_validation_wrong_cli
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~wait_ready:false
    ~disable_shard_validation:true
    "DAL node disable shard validation wrong env"
    test_disable_shard_validation_wrong_env
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~wait_ready:true
    ~env:
      (String_map.singleton
         Dal_node.disable_shard_validation_environment_variable
         "yes")
    ~disable_shard_validation:true
    "DAL node disable shard validation correct CLI"
    (fun _protocol _parameters _cryptobox _node _client dal_node ->
      Dal_node.terminate dal_node)
    protocols ;
  (* Scenarios for --ignore-topics *)
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~wait_ready:false
    ~env:
      (String_map.singleton Dal_node.ignore_topics_environment_variable "yes")
    "DAL node ignore topics wrong CLI"
    test_ignore_topics_wrong_cli
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~wait_ready:false
    ~ignore_pkhs:
      [
        Constant.bootstrap1.Account.public_key_hash;
        Constant.bootstrap2.Account.public_key_hash;
      ]
    "DAL node ignore topics wrong env"
    test_ignore_topics_wrong_env
    protocols
