(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Requirement:  make -f kernels.mk build

                 For dev-purpose you can also run the follwing to enable debug outputs and faster build time:

                 cd src/kernel_tx_demo/kernel
                 cargo build --release --target wasm32-unknown-unknown --features debug --features dal
                 wasm-strip ../target/wasm32-unknown-unknown/release/tx_kernel.wasm
                 cp ../target/wasm32-unknown-unknown/release/tx_kernel.wasm ../../../tx_kernel_dal.wasm

                 ./scripts/install_dal_trusted_setup.sh

   Invocation:   dune exec tezt/tests/main.exe -- --file dal.ml
   Subject: Integration tests related to the data-availability layer
*)

open Dal_helpers
module Dal = Dal_common

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

(** For each level in the range [from_level, to_level]:
    1. Publishes a dummy slot with content derived from the level and slot index
    2. Stores the slot in the DAL node
    3. Bakes a new block to include the published slot on L1
    4. Waits for the node to reach the baked level

    The [delegates] parameter is used to specify which delegates should
    participate in baking. *)
let publish_and_bake ?slots ?delegates ~from_level ~to_level parameters
    cryptobox node client dal_node =
  let num_bakers = Array.length Account.Bootstrap.keys in
  let publish source ~index message =
    let* _op_hash =
      publish_dummy_slot ~source ~index ~message cryptobox client
    in
    unit
  in
  let publish_and_store =
    let slot_idx =
      match slots with
      | Some s ->
          let num_slots = List.length s in
          fun level -> List.nth s (slot_idx parameters level mod num_slots)
      | None -> slot_idx parameters
    in
    fun level ->
      let source = Account.Bootstrap.keys.(level mod num_bakers) in
      let index = slot_idx level in
      let slot_content =
        Format.asprintf "content at level %d index %d" level index
      in
      let* predecessor = Node.wait_for_level node (pred level) in
      if predecessor >= level then
        Test.fail
          "Tried to publish a slot header at level %d but the predecessor \
           level is %d"
          level
          predecessor ;
      let* () = publish source ~index slot_content in
      (* Storing the slot is required as the producer/operator does not store
         anything. *)
      let* _commitment, _proof =
        let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
        Helpers.(
          store_slot dal_node ~slot_index:index
          @@ make_slot ~slot_size slot_content)
      in
      Log.info "Slot with %d index (normally) published at level %d" index level ;
      return (level, index)
  in
  Log.info
    "Publish (inject and bake) a slot header at each level from %d to %d."
    from_level
    to_level ;
  let rec iter acc level =
    if level > to_level then return acc
    else
      let* level, index = publish_and_store level in
      let* () =
        bake_for
          ?delegates
          ~dal_node_endpoint:(Dal_node.rpc_endpoint dal_node)
          client
      in
      let* _ = Node.wait_for_level node level in
      iter ((level, index) :: acc) (level + 1)
  in
  iter [] from_level

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

let generate_dummy_slot slot_size =
  String.init slot_size (fun i ->
      match i mod 3 with 0 -> 'a' | 1 -> 'b' | _ -> 'c')

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

let test_dal_node_snapshot_aux ~operators ~name ?slots_exported ?slots_imported
    parameters cryptobox node client dal_node =
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

  let upper_bound_exported = start + expected_exported_levels in
  let index_ok =
    let is_exported =
      match slots_exported with
      | None -> fun _ -> true
      | Some l -> fun x -> List.mem x l
    in
    let is_imported =
      match slots_imported with
      | None -> fun _ -> true
      | Some l -> fun x -> List.mem x l
    in
    fun i -> is_exported i && is_imported i
  in
  let tests_ok =
    (* Levels that must be present in both original store and snapshot *)
    List.filter
      (fun (level, index) -> index_ok index && level < upper_bound_exported)
      published
  in
  let tests_error =
    (* Levels that must be present in original store but not in snapshot *)
    (* [stop - 2] is used because [publish_and_bake ~to_level:stop] means
       that blocks up to level [stop - 2] are sure to be finalized and
       therefore present in the source DAL node's store. *)
    List.filter
      (fun (level, index) ->
        (level >= upper_bound_exported && level < stop - 2)
        || (level < upper_bound_exported && not (index_ok index)))
      published
  in
  let min_published_level = Int32.of_int start in
  let max_published_level = Int32.of_int stop in
  let file = Temp.file ("snapshot-" ^ name) in
  (* Wait for the DAL node to process finalized blocks up to [stop - 2] before
     exporting the snapshot. The snapshot export caps [max_published_level] at
     [last_processed_level - (validation_slack + attestation_lag + 1)], so if
     the DAL node hasn't caught up yet the snapshot would be missing levels
     required by [tests_ok]. *)
  let* () = wait_for_layer1_final_block dal_node (stop - 2) in
  (* Export with default levels (uses first_seen_level and last_processed_level) *)
  let* () =
    Dal_node.snapshot_export
      ~min_published_level
      ~max_published_level
      ?slots:slots_exported
      ~endpoint:(Node.as_rpc_endpoint node)
      dal_node
      file
  in
  (* Verify the snapshot file was created *)
  let* file_exists = Lwt_unix.file_exists file in
  let* () =
    if file_exists then unit
    else Test.fail "Snapshot export failed: file was not created at %s" file
  in
  (* Create a fresh DAL node using the exported snapshot data. *)
  let fresh_dal_node = Dal_node.create ~node () in
  let* () = Dal_node.init_config ~operator_profiles:operators fresh_dal_node in
  let* () =
    Dal_node.snapshot_import
      ~no_check:true (* Snapshot data validation is not implemented yet *)
      ~min_published_level
      ~max_published_level
      ?slots:slots_imported
      ~endpoint:(Node.as_rpc_endpoint node)
      fresh_dal_node
      file
  in
  let* () = Dal_node.run fresh_dal_node in
  (* Compare slot statuses between the original node and the fresh one built from snapshot. *)
  let* () =
    Log.info
      "Checking that the DAL node bootstrapped from snapshot has the expected \
       slot data..." ;
    Lwt_list.iter_s
      (fun (slot_level, slot_index) ->
        let* status_orig =
          Dal_RPC.(
            call dal_node @@ get_level_slot_status ~slot_level ~slot_index)
        in
        let* () =
          check_slot_status
            ~__LOC__
            ~expected_status:status_orig
            ~check_attested_lag:`Exact
            fresh_dal_node
            ~slot_level
            ~slot_index
        in
        let* content_orig =
          Dal_RPC.(
            call dal_node @@ get_level_slot_content ~slot_level ~slot_index)
          |> Lwt.map Helpers.content_of_slot
        in
        let* content_fresh =
          Dal_RPC.(
            call fresh_dal_node
            @@ get_level_slot_content ~slot_level ~slot_index)
          |> Lwt.map Helpers.content_of_slot
        in
        Check.(content_fresh = content_orig)
          ~__LOC__
          Check.string
          ~error_msg:
            (Format.sprintf
               "Snapshot import mismatch for slot (level=%d,index=%d)"
               slot_level
               slot_index) ;
        unit)
      tests_ok
  in
  let unexpected_success = Failure "Should not succeed" in
  let* () =
    Log.info
      "Checking that the DAL node bootstrapped from snapshot lacks the \
       expected slot data..." ;
    Lwt_list.iter_s
      (fun (slot_level, slot_index) ->
        (* Check that the source node has data *)
        let* _ =
          Dal_RPC.(
            call dal_node @@ get_level_slot_content ~slot_level ~slot_index)
        in
        (* Check that the node bootstrapped from snapshot does not have data *)
        let* () =
          Lwt.catch
            (fun () ->
              let* _ =
                Dal_RPC.(
                  call fresh_dal_node
                  @@ get_level_slot_content ~slot_level ~slot_index)
              in
              Lwt.fail unexpected_success)
            (fun e ->
              if e = unexpected_success then
                Test.fail
                  "After snapshot import: expected failure for \
                   /levels/%d/slot/%d/content"
                  slot_level
                  slot_index
              else Lwt.return_unit)
        in
        unit)
      tests_error
  in
  Dal_node.terminate fresh_dal_node

let test_dal_node_snapshot ~operators _protocol parameters cryptobox node client
    dal_node =
  let test ?slots_exported ?slots_imported name =
    test_dal_node_snapshot_aux
      ~name
      ~operators
      ?slots_exported
      ?slots_imported
      parameters
      cryptobox
      node
      client
      dal_node
  in
  let* () = test "empty" in
  let* () = test ~slots_exported:[List.hd operators] "filter-exported" in
  let* () = test ~slots_imported:[List.hd operators] "filter-imported" in
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

let test_dal_node_startup =
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

let test_dal_node_invalid_config () =
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
      [
        "run";
        "--data-dir";
        Dal_node.data_dir dal_node;
        "--config-file";
        config_file;
      ]
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

let check_topics_peers ~__LOC__ dal_node ~expected =
  let normalize_peers l = List.sort String.compare l in
  let compare_topics {Dal.RPC.topic_slot_index = s1; topic_pkh = p1}
      {Dal.RPC.topic_slot_index = s2; topic_pkh = p2} =
    let c = Int.compare s1 s2 in
    if c = 0 then String.compare p1 p2 else c
  in
  let normalize_topics_peers l =
    l
    |> List.map (fun (topic, peers) -> (topic, normalize_peers peers))
    |> List.sort (fun (t1, _p1) (t2, _p2) -> compare_topics t1 t2)
  in
  let* topic_peers = Dal_RPC.(call dal_node @@ get_topics_peers ()) in
  return
    Check.(
      (normalize_topics_peers topic_peers = normalize_topics_peers expected)
        Dal.Check.topics_peers_typ
        ~error_msg:
          (__LOC__
         ^ " : Unexpected topic - peers association (Actual: %L <> Expected: \
            %R)"))

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

type event_with_message = Publish_message | Message_with_header of peer_id

let event_with_message_to_string = function
  | Publish_message -> "publish_message"
  | Message_with_header _ -> "message_with_header"

(** This function monitors the Gossipsub worker events whose name is given by
    [event_with_message]. It's somehow similar to function
    {!check_events_with_topic}, except that what varies here is the shard index
    instead of slot index. Moreover, shards do not necessiraly start from 0 or
    end at number_of_shards - 1. *)
let check_events_with_message ~event_with_message dal_node ~number_of_shards
    ~shard_indexes ~expected_commitment ~expected_level ~expected_pkh
    ~expected_slot =
  let remaining = ref (List.length shard_indexes) in
  let seen = Array.make number_of_shards false in
  let get_shard_index_opt event =
    let topic_slot_index =
      JSON.(event |-> "topic" |-> "slot_index" |> as_int)
    in
    let topic_pkh = JSON.(event |-> "topic" |-> "pkh" |> as_string) in
    let level = JSON.(event |-> "message_id" |-> "level" |> as_int) in
    let slot_index = JSON.(event |-> "message_id" |-> "slot_index" |> as_int) in
    let shard_index =
      JSON.(event |-> "message_id" |-> "shard_index" |> as_int)
    in
    let pkh = JSON.(event |-> "message_id" |-> "pkh" |> as_string) in
    let commitment =
      JSON.(event |-> "message_id" |-> "commitment" |> as_string)
    in
    let*?? () = check_expected expected_pkh topic_pkh in
    let*?? () = check_expected expected_pkh pkh in
    let*?? () = check_expected expected_level level in
    let*?? () = check_expected expected_slot slot_index in
    let*?? () = check_expected expected_slot topic_slot_index in
    let*?? () = check_expected expected_commitment commitment in
    let*?? () =
      match event_with_message with
      | Publish_message -> Some ()
      | Message_with_header expected_peer_id ->
          check_expected expected_peer_id JSON.(event |-> "peer" |> as_string)
    in
    Some shard_index
  in
  let all_seen () =
    seen |> Array.to_seqi
    |> Seq.for_all (fun (i, b) -> if List.mem i shard_indexes then b else true)
  in
  Dal_common.Helpers.wait_for_gossipsub_worker_event
    dal_node
    ~name:(event_with_message_to_string event_with_message)
    (fun event ->
      let*?? shard_index = get_shard_index_opt event in
      Check.(
        (seen.(shard_index) = false)
          bool
          ~error_msg:
            (sf "Shard_index %d already seen. Invariant broken" shard_index)) ;
      seen.(shard_index) <- true ;
      let () = remaining := !remaining - 1 in
      if !remaining = 0 && all_seen () then Some () else None)

(** This function is quite similar to those above, except that it checks that a
    range of messages (shards) on a tracked topic have been notified by GS to
    the DAL node. This is typically needed to then be able to attest slots. *)
let check_message_notified_to_app_event dal_node ~number_of_shards
    ~shard_indexes ~expected_commitment ~expected_level ~expected_pkh
    ~expected_slot =
  let remaining = ref (List.length shard_indexes) in
  let seen = Array.make number_of_shards false in
  let get_shard_index_opt event =
    let level = JSON.(event |-> "level" |> as_int) in
    let slot_index = JSON.(event |-> "slot_index" |> as_int) in
    let shard_index = JSON.(event |-> "shard_index" |> as_int) in
    let pkh = JSON.(event |-> "pkh" |> as_string) in
    let commitment = JSON.(event |-> "commitment" |> as_string) in
    let*?? () = check_expected expected_pkh pkh in
    let*?? () = check_expected expected_level level in
    let*?? () = check_expected expected_slot slot_index in
    let*?? () = check_expected expected_commitment commitment in
    Some shard_index
  in
  let all_seen () =
    seen |> Array.to_seqi
    |> Seq.for_all (fun (i, b) -> if List.mem i shard_indexes then b else true)
  in
  Dal_node.wait_for dal_node "dal_gs_message_notified_to_app.v0" (fun event ->
      let*?? shard_index = get_shard_index_opt event in
      Check.(
        (seen.(shard_index) = false)
          bool
          ~error_msg:
            (sf "Shard_index %d already seen. Invariant broken" shard_index)) ;
      seen.(shard_index) <- true ;
      let () = remaining := !remaining - 1 in
      if !remaining = 0 && all_seen () then Some () else None)

(** This helper function makes the nodes [dal_node1] and [dal_node2] join the
    topics of the attester [pkh], by calling the RPC for tracking the corresponding profile.
    The second node calls the RPC only after receiving the Subscribe messages
    from the first node, so that when it joins the topics, it also sends Graft messages
    in addition to sending Subscribe messages. *)
let nodes_join_the_same_topics dal_node1 dal_node2 ~num_slots ~pkh1 =
  let profile1 = Dal_RPC.Attester pkh1 in
  let* peer_id1 = Dal_node.read_identity dal_node1 in
  let* peer_id2 = Dal_node.read_identity dal_node2 in
  (* node1 joins topic {pkh} -> it sends subscribe messages to node2. *)
  let event_waiter =
    check_events_with_topic
      ~event_with_topic:(Subscribe peer_id1)
      dal_node2
      ~num_slots
      pkh1
  in
  let* () = Dal_RPC.(call dal_node1 @@ patch_profiles [profile1]) in
  let* () = event_waiter in

  (* node2 joins topic {pkh} -> it sends subscribe and graft messages to
     node1. *)
  let event_waiter_subscribe =
    check_events_with_topic
      ~event_with_topic:(Subscribe peer_id2)
      dal_node1
      ~num_slots
      pkh1
  in
  let event_waiter_graft =
    check_events_with_topic
      ~event_with_topic:(Graft peer_id2)
      dal_node1
      ~num_slots
      pkh1
  in
  let* () = Dal_RPC.(call dal_node2 @@ patch_profiles [profile1]) in
  Lwt.join [event_waiter_subscribe; event_waiter_graft]

(** This helper returns the list of promises that allow to wait for the
    publication of a slot's shards into the Gossipsub layer.

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level].
*)
let waiters_publish_shards l1_committee dal_node commitment ~publish_level
    ~slot_index ~number_of_shards =
  let open Dal.Committee in
  List.map
    (fun {attester; indexes} ->
      check_events_with_message
        ~event_with_message:Publish_message
        dal_node
        ~number_of_shards
        ~shard_indexes:indexes
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attester
        ~expected_slot:slot_index)
    l1_committee

(** This helper returns the promise that allows to wait for the reception of
    messages of [slot_index] published at level [publish_level] by the attester
    [pkh].

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level]. *)
let waiter_receive_shards l1_committee dal_node commitment ~publish_level
    ~slot_index ~pkh ~from_peer ~number_of_shards =
  let open Dal.Committee in
  match List.find (fun {attester; _} -> attester = pkh) l1_committee with
  | {attester; indexes} ->
      check_events_with_message
        ~event_with_message:(Message_with_header from_peer)
        dal_node
        ~number_of_shards
        ~shard_indexes:indexes
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attester
        ~expected_slot:slot_index
  | exception Not_found ->
      Test.fail "Should not happen as %s is part of the committee" pkh

(** This helper returns the promise that allows to wait for the successful
    notification of messages of [slot_index] published at level [publish_level]
    to the app layer of the attester [pkh].

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level]. *)
let waiter_successful_shards_app_notification l1_committee dal_node commitment
    ~publish_level ~slot_index ~pkh ~number_of_shards =
  let open Dal.Committee in
  match List.find (fun {attester; _} -> attester = pkh) l1_committee with
  | {attester; indexes} ->
      check_message_notified_to_app_event
        dal_node
        ~number_of_shards
        ~shard_indexes:indexes
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attester
        ~expected_slot:slot_index
  | exception Not_found ->
      Test.fail "Should not happen as %s is part of the committee" pkh

let test_dal_node_p2p_connection_and_disconnection _protocol _parameters
    _cryptobox node _client dal_node1 =
  let dal_node2 = Dal_node.create ~node () in
  (* Connect the nodes *)
  let* () =
    Dal_common.Helpers.connect_nodes_via_p2p
      ~init_config:true
      dal_node1
      dal_node2
  in
  let* peer_id = Dal_node.read_identity dal_node2 in
  (* kill dal_node2 and check "disconnection" event in node1. *)
  let disconn_ev_in_node1 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id
  in
  let* () = Dal_node.kill dal_node2 in
  disconn_ev_in_node1

let test_dal_node_join_topic _protocol parameters _cryptobox _node _client
    dal_node1 =
  let pkh1 = Constant.bootstrap1.public_key_hash in
  let profile1 = Dal_RPC.Attester pkh1 in
  let num_slots = parameters.Dal.Parameters.number_of_slots in
  let event_waiter =
    check_events_with_topic ~event_with_topic:Join dal_node1 ~num_slots pkh1
  in
  let* () = Dal_RPC.(call dal_node1 @@ patch_profiles [profile1]) in
  event_waiter

(** This generic test function is used to test messages exchanges between two
    DAL nodes via the P2P/GS layers, once connections are established and topics
    are joined.

   The [mk_dal_node2] function is used to create a second DAL node. We may
   decide to create a regular/normal or a modified dal node.

   The [expect_app_notification] flag is used to tell whether we should wait for
   the application layer of the second DAL node to be notified with received messages.
   In case we don't expect the application layer to be notified (e.g. messages are invalid),
   set to [false].

   The [is_first_slot_attestable] flag is used to tell whether the first slot
   (that has been injected by this function) can be attested by the considered
   attester or not. In particular, it should be set to [false] if the
   application layer of the second DAL node was not notified about the messages
   sent by the first DAL node.
*)
let generic_gs_messages_exchange protocol parameters _cryptobox node client
    ?batching_time_interval dal_node1 ~mk_dal_node2 ~expect_app_notification
    ~is_first_slot_attestable =
  let* node2, dal_node2 = mk_dal_node2 protocol parameters in
  let* () =
    Dal_node.init_config
      ?batching_time_interval
      ~peers:[Dal_node.listen_addr dal_node1]
      dal_node2
  in
  let* () = Dal_common.Helpers.connect_nodes_via_p2p dal_node1 dal_node2 in
  let num_slots = parameters.Dal.Parameters.number_of_slots in
  let number_of_shards = parameters.Dal.Parameters.cryptobox.number_of_shards in
  let account1 = Constant.bootstrap1 in
  let pkh1 = account1.public_key_hash in
  (* The two nodes join the same topics *)
  let* () = nodes_join_the_same_topics dal_node1 dal_node2 ~num_slots ~pkh1 in

  (* Posting a DAL message to DAL node and to L1 *)
  let crypto_params = parameters.cryptobox in
  let slot_index = 0 in
  let* slot_commitment =
    let slot_size = crypto_params.slot_size in
    let slot_content = generate_dummy_slot slot_size in
    Helpers.publish_and_store_slot
      client
      dal_node1
      Constant.bootstrap1
      ~index:slot_index
    @@ Helpers.make_slot ~slot_size slot_content
  in

  (* Preparing event waiters for different shards that will be published by
     dal_node1 once the slot header is included in an L1 block.

     We also prepare a waiter event for:
     - the shards that will be received by dal_node2 on its topics;
     - the messages (shards) that will be notified to dal_node2 on its topic. *)
  let* publish_level = next_level node in
  let attested_level = publish_level + parameters.attestation_lag in
  let attestation_level = attested_level - 1 in
  let* committee = Dal.Committee.at_level node ~level:attestation_level () in

  let waiter_publish_list =
    waiters_publish_shards
      committee
      dal_node1
      slot_commitment
      ~publish_level
      ~slot_index
      ~number_of_shards
  in
  let waiter_receive_shards =
    let* from_peer = Dal_node.read_identity dal_node1 in
    waiter_receive_shards
      committee
      dal_node2
      slot_commitment
      ~publish_level
      ~slot_index
      ~pkh:pkh1
      ~from_peer
      ~number_of_shards
  in
  let waiter_app_notifs =
    if expect_app_notification then
      waiter_successful_shards_app_notification
        committee
        dal_node2
        slot_commitment
        ~publish_level
        ~slot_index
        ~pkh:pkh1
        ~number_of_shards
    else unit
  in

  (* We bake a block that includes a [slot_header] operation. And then another
     two blocks so that this operation is final. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join waiter_publish_list
  and* () = waiter_receive_shards
  and* () = waiter_app_notifs in

  let* () =
    match node2 with
    | None -> unit
    | Some node2 ->
        (* We need that this node reaches level 2; otherwise, the RPC call to
           [get_attestable_slots] may fail. *)
        let* _level = Node.wait_for_level node2 2 in
        unit
  in

  (* Check that dal_node2 has the shards needed by attester account1/pkh1 to
     attest the slot with index 0. *)
  let* res =
    Dal_RPC.(
      call dal_node2 @@ get_attestable_slots ~attester:account1 ~attested_level)
  in
  match res with
  | Not_in_committee -> Test.fail "attester %s not in committee" account1.alias
  | Attestable_slots slots ->
      (* only slot 0 is attestable. Others are set to false. *)
      let expected =
        is_first_slot_attestable :: List.init (num_slots - 1) (fun _ -> false)
      in
      Check.(
        (expected = slots)
          (list bool)
          ~error_msg:"Expected %L attestable slots list flags, got %R") ;
      unit

let test_dal_node_gs_valid_messages_exchange ?batching_time_interval _protocol
    parameters _cryptobox node client dal_node1 =
  generic_gs_messages_exchange
    _protocol
    parameters
    _cryptobox
    node
    client
    ?batching_time_interval
    dal_node1
    ~mk_dal_node2:(fun _protocol _parameters ->
      let dal_node2 = Dal_node.create ~node () in
      (None, dal_node2) |> return)
    ~expect_app_notification:true
    ~is_first_slot_attestable:true

(* Create a DAL node whose DAL parameters are not compatible with those in
   [parameters]. For that, the redundancy_factor field is multiplied by 2. *)
let make_invalid_dal_node node1 protocol parameters =
  (* Create another L1 node with different DAL parameters. *)
  let* node2, _client2, _xdal_parameters2 =
    let crypto_params = parameters.Dal.Parameters.cryptobox in
    let parameter_overrides =
      dal_enable_param (Some true)
      @ redundancy_factor_param (Some (crypto_params.redundancy_factor / 2))
      @ slot_size_param (Some (crypto_params.slot_size / 2))
    in
    setup_node ~protocol ~parameter_overrides ()
  in
  Node.add_peer node2 node1 ;
  let* () = Node.terminate node2 in
  let* () = Node.run node2 [] in
  (* Create a second DAL node with node2 and client2 as argument (so different
     DAL parameters compared to dal_node1. *)
  let dal_node2 = Dal_node.create ~node:node2 () in
  return (Some node2, dal_node2)

let test_dal_node_gs_invalid_messages_exchange ?batching_time_interval _protocol
    parameters _cryptobox node client dal_node1 =
  (* Messages are invalid, so the app layer is not notified. *)
  let expect_app_notification = false in
  (* The first slot published by [generic_gs_messages_exchange] is not
     attestable by the considered attester pk1 = bootstrap1, because the shards
     received by the Gossipsub layer are classified as 'Invalid'. *)
  let is_first_slot_attestable = false in
  generic_gs_messages_exchange
    _protocol
    parameters
    _cryptobox
    node
    client
    ?batching_time_interval
    dal_node1
    ~mk_dal_node2:(make_invalid_dal_node node)
    ~expect_app_notification
    ~is_first_slot_attestable

(* Checks that:
   * the baker does not crash when there's a DAL node specified, but it is not
   running
   * the baker register profiles when the DAL node restarts. *)
let test_baker_registers_profiles protocol _parameters _cryptobox l1_node client
    dal_node =
  let delegates =
    List.to_seq Constant.all_secret_keys |> Seq.take 3 |> List.of_seq
  in
  let profiles =
    List.map (fun key -> Dal_RPC.Attester key.Account.public_key_hash) delegates
  in
  let delegates = List.map (fun key -> key.Account.alias) delegates in

  Log.info
    "Terminate the DAL node and then start the baker; the baker cannot attest \
     but can advance" ;
  let* () = Dal_node.terminate dal_node in
  let baker =
    let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
    Agnostic_baker.create ~dal_node_rpc_endpoint l1_node client ~delegates
  in
  let wait_for_attestation_event =
    let attestation_event =
      if Protocol.number protocol >= 025 then
        "no_attestable_dal_slots_for_levels.v0"
      else "failed_to_get_attestations.v0"
    in
    Agnostic_baker.wait_for baker attestation_event (fun _json -> Some ())
  in
  let* () = Agnostic_baker.run baker in
  let* () = wait_for_attestation_event in
  let* _lvl = Node.wait_for_level l1_node 3 in

  Log.info "Start (again) the DAL node" ;
  let* () = Dal_node.run dal_node in
  Log.info "Wait 2 seconds; by then, profiles should have been registered" ;
  (* Note: this constant depends on how often the baker retries to register
     profiles (see [max_delay] in [Baking_scheduling.register_dal_profiles]); if
     the baker behavior changes in this respect, the constant may need
     adjusting. *)
  let* () = Lwt_unix.sleep 2.0 in
  check_profiles ~__LOC__ dal_node ~expected:(Controller profiles)

(** This helper funciton terminates dal_node2 and dal_node3 (in addition to
    those in [extra_nodes_to_restart]), and restart them after creating two
    connection events to check that dal_node2 and dal_node3 find each other. *)
let observe_nodes_connection_via_bootstrap ?(extra_nodes_to_restart = []) client
    dal_node2 dal_node3 =
  let nodes = dal_node2 :: dal_node3 :: extra_nodes_to_restart in
  let* () = List.map Dal_node.terminate nodes |> Lwt.join in
  let* dal_node2_identity = Dal_node.read_identity dal_node2 in
  let* dal_node3_identity = Dal_node.read_identity dal_node3 in
  let check_conn_event_from_2_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node2
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:dal_node3_identity
      ()
  in
  let check_conn_event_from_3_to_2 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node3
      ~other_node:dal_node2
      ~is_trusted:false
      ~other_peer_id:dal_node2_identity
      ()
  in
  let* () = List.map (Dal_node.run ~wait_ready:true) nodes |> Lwt.join in
  Log.info "Bake a block and then another two to finalize it." ;
  let* () = bake_for ~count:3 client in
  Log.info "Wait for dal_node2 and dal_node3 to find each other." ;

  let* () =
    Lwt.join [check_conn_event_from_2_to_3; check_conn_event_from_3_to_2]
  in
  unit

(** This function tests that a peer can discover another peer via a bootstrap
    node and that discovery works even when the bootstrap is (re-)started at the
    same time (or after) the two other nodes we want to connect. *)
let test_peer_discovery_via_bootstrap_node _protocol _parameters _cryptobox node
    client dal_node1 =
  (* Phase 1: dal_node1 is already running. Start dal_node2 and dal_node3 and
     use dal_node1 to establish connections between them. *)
  let* dal_node2 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~attester_profiles:[Constant.bootstrap1.public_key_hash]
      node
  in
  let* dal_node3 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~operator_profiles:[0]
      node
  in
  (* Here, we observe a first nodes connection via bootstrap nodes thanks to
     peers exchange. *)
  let* () = observe_nodes_connection_via_bootstrap client dal_node2 dal_node3 in

  (* In this variant, we also restart the bootstrap node [dal_node1]. So,
     connections to it from dal_node2 and dal_node3 are always done at startup,
     but Gossipsub worker might be needed to retry connection. *)
  observe_nodes_connection_via_bootstrap
    ~extra_nodes_to_restart:[dal_node1]
    client
    dal_node2
    dal_node3

(** Connect two nodes [dal_node2] and [dal_node3] via a trusted bootstrap peer
    [dal_node1]. Then, disconnect all the nodes (without restarting them) and
    wait for reconnection. *)
let test_peers_reconnection _protocol _parameters _cryptobox node client
    dal_node1 =
  (* Connect two nodes via bootstrap peer. *)
  Log.info "Connect two nodes via bootstrap peer." ;
  let* dal_node2 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~attester_profiles:[Constant.bootstrap1.public_key_hash]
      node
  in
  let* dal_node3 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~operator_profiles:[0]
      node
  in
  let* () =
    (* Here, we observe a first nodes connection via bootstrap nodes thanks to
       peers exchange. Below, we disconnect the nodes without restarting
       them. *)
    observe_nodes_connection_via_bootstrap client dal_node2 dal_node3
  in

  (* Get the nodes' identities. *)
  let id dal_node = Dal_node.read_identity dal_node in
  let* id_dal_node1 = id dal_node1 in
  let* id_dal_node2 = id dal_node2 in
  let* id_dal_node3 = id dal_node3 in

  (* Prepare disconnection events to observe. *)
  let disconn_ev_in_node1_2 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id:id_dal_node2
  in
  let disconn_ev_in_node1_3 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id:id_dal_node3
  in
  let disconn_ev_in_node2_3 =
    Dal_common.Helpers.check_disconnection_event dal_node2 ~peer_id:id_dal_node3
  in

  (* Prepare reconnection events checks between node1 and node2 (resp. 3). *)
  let check_conn_event_from_1_to_2 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node1
      ~other_node:dal_node2
      ~is_trusted:false
      ~other_peer_id:id_dal_node2
      ()
  in
  let check_conn_event_from_1_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node1
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:id_dal_node3
      ()
  in
  let check_conn_event_from_2_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node2
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:id_dal_node3
      ()
  in

  (* Disconnect all the nodes. *)
  let* () =
    Lwt_list.iter_p
      (fun peer_id ->
        Lwt_list.iter_p
          (fun dal_node ->
            Dal_RPC.(call dal_node @@ delete_p2p_peer_disconnect ~peer_id))
          [dal_node1; dal_node2; dal_node3])
      [id_dal_node1; id_dal_node2; id_dal_node3]
  in

  (* Observe disconnection. *)
  Log.info "Wait for disconnection" ;
  let* () =
    Lwt.join
      [disconn_ev_in_node1_2; disconn_ev_in_node1_3; disconn_ev_in_node2_3]
  in
  Log.info "Disconnection done. Wait for reconnection." ;
  let* () =
    Lwt.join
      [
        check_conn_event_from_1_to_2;
        check_conn_event_from_1_to_3;
        check_conn_event_from_2_to_3;
      ]
  in
  Log.info "Recconnection done." ;
  unit

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
       C.3 - Fetch valid slots after removing sqlite DB (L1 skip list)

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

    (* C.3 Remove sqlite DB and fetch via L1 skip list *)
    let* () =
      Log.info "C.3: L1 skip list" ;
      let skip_db =
        Format.sprintf "%s/store/skip_list_store"
        @@ Dal_node.data_dir valid_dal_fetcher_1_2
      in
      let () = Sys.command ("rm -rf " ^ skip_db) |> ignore in
      let* () = Dal_node.terminate valid_dal_fetcher_1_2 in
      let* () = Dal_node.run valid_dal_fetcher_1_2 in
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index:3
      |> fetch_500_expected
           ~__LOC__
           ~expected_error:"No_commitment_found_for_slot_id"
    in

    unit

let test_attestation_through_p2p ~batching_time_interval _protocol
    dal_parameters _cryptobox node client dal_bootstrap =
  (* In this test we have three DAL nodes:
     - a boostrap one to connect the other two (producer and attester),
     - a slot producer on slot 0,
     - an attester for all the bootstrap pkh.

     We check that when the slot producer publishes a slot, it ends up
     being attested.
  *)
  let index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let attestation_lag = dal_parameters.Dal.Parameters.attestation_lag in
  let number_of_shards =
    dal_parameters.Dal.Parameters.cryptobox.number_of_shards
  in
  let peers = [Dal_node.listen_addr dal_bootstrap] in
  let peer_id dal_node = Dal_node.read_identity dal_node in
  let* bootstrap_peer_id = peer_id dal_bootstrap in

  (* Check that the attestation threshold for this test is 100%. If
     not, this means that we forgot to register the test with
     ~attestation_threshold:100 *)
  Check.((dal_parameters.Dal.Parameters.attestation_threshold = 100) int)
    ~error_msg:"attestation_threshold value (%L) should be 100" ;
  (* Check that the dal node passed as argument to this test function
     is a running bootstrap DAL node. If not, this means that we
     forgot to register the test with ~bootstrap_profile:true *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () =
    Dal_node.init_config
      ~batching_time_interval
      ~operator_profiles:[index]
      ~peers
      producer
  in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug producer in
  let* producer_peer_id = peer_id producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator index])
  in
  Log.info "Slot producer DAL node is running" ;

  let all_pkhs =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun account -> account.Account.public_key_hash)
  in
  let attester = Dal_node.create ~name:"attester" ~node () in
  let* () =
    Dal_node.init_config
      ~batching_time_interval
      ~attester_profiles:all_pkhs
      ~peers
      attester
  in
  let* () = Dal_node.run ~event_level:`Debug ~wait_ready:true attester in
  let* attester_peer_id = peer_id attester in

  let client = Client.with_dal_node client ~dal_node:attester in

  (* Wait for a GRAFT message between the attester and the producer,
     in any direction. *)
  let check_graft pkh =
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index:index
         (attester, attester_peer_id)
         (producer, producer_peer_id)
         pkh
  in
  let check_graft_promises = List.map check_graft all_pkhs in
  Log.info "Waiting for grafting of the attester - producer connection" ;
  let* () =
    check_profiles
      ~__LOC__
      attester
      ~expected:
        Dal_RPC.(Controller (List.map (fun pkh -> Attester pkh) all_pkhs))
  in
  (* We need to bake some blocks until the L1 node notifies the
     attester DAL nodes that some L1 block is final and they have DAL
     attestation rights in it. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join check_graft_promises in
  Log.info "Attester - producer connection grafted" ;

  (* Attester should be connected to:
     - the bootstrap DAL node on all the topics,
     - the producer on all topics with slot_index=index *)
  let* () =
    let expected topic_pkh =
      Seq.ints 0 |> Seq.take number_of_slots
      |> Seq.map (fun topic_slot_index ->
             ( {Dal_RPC.topic_slot_index; topic_pkh},
               bootstrap_peer_id
               :: (if topic_slot_index = index then [producer_peer_id] else [])
             ))
      |> List.of_seq
    in
    let expected = List.concat (List.map expected all_pkhs) in
    check_topics_peers ~__LOC__ attester ~expected
  in

  (* The slot producer should be connected to:
     - the attester and the bootstrap DAL node on all topics with slot_index=index,
  *)
  let* () =
    let expected =
      all_pkhs
      |> List.map (fun topic_pkh ->
             ( {Dal_RPC.topic_slot_index = index; topic_pkh},
               [bootstrap_peer_id; attester_peer_id] ))
    in
    check_topics_peers ~__LOC__ producer ~expected
  in

  (* Produce and publish a slot *)
  let source = Constant.bootstrap1 in
  let slot_content = "content" in

  let all_shard_indices =
    Seq.ints 0 |> Seq.take number_of_shards |> List.of_seq
  in
  let wait_slot ~published_level ~slot_index =
    Lwt.join
    @@ List.map
         (fun shard_index ->
           wait_for_cached_slot
             ~shard_index
             attester
             ~published_level
             ~slot_index)
         all_shard_indices
  in
  let* publication_level, _commitment, () =
    publish_store_and_wait_slot
      node
      client
      producer
      source
      ~index
      ~wait_slot
      ~number_of_extra_blocks_to_bake:2
    @@ Helpers.make_slot ~slot_size slot_content
  in

  Log.info "Slot produced and published" ;

  let* () =
    bake_until_processed
      ~level:(publication_level + attestation_lag)
      client
      [attester]
  in

  let* () =
    check_slot_status
      ~__LOC__
      attester
      ~expected_status:(Dal_RPC.Attested attestation_lag)
      ~check_attested_lag:`At_most
      ~slot_level:publication_level
      ~slot_index:index
  in
  Log.info "Slot sucessfully attested" ;
  unit

module Profiler = Tezos_profiler.Profiler

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

let dal_crypto_benchmark () =
  Test.register
    ~__FILE__
    ~title:"Benchmark of the DAL cryptographic primitives"
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~tags:["dal"; "benchmark"; "crypto"; Tag.slow; team]
  @@ fun () ->
  let open Dal.Cryptobox in
  let driver =
    Tezos_profiler_backends.Simple_profiler.auto_write_as_txt_to_file
  in
  let file =
    match Cli.Logs.level with
    | Info | Debug -> "/dev/stdout"
    | _ -> Temp.file "profiler-output"
  in
  let instance = Profiler.instance driver (file, Debug) in
  Profiler.plug Profiler.main instance ;
  let ( let*? ) x f =
    match x with
    | Error err -> Test.fail "Unexpected error:@.%a@." Cryptobox.pp_error err
    | Ok x -> f x
  in
  (* the defaults are the Rio parameters *)
  let number_of_shards = Cli.get_int ~default:512 "nb_shards" in
  let slot_size = Cli.get_int ~default:126_944 "slot_size" in
  let redundancy_factor = Cli.get_int ~default:8 "redundancy" in
  let page_size = Cli.get_int ~default:3967 "page_size" in
  let traps_fraction =
    Cli.get_float ~default:0.0005 "traps_fraction" |> Q.of_float
  in
  let* () =
    let parameters =
      {number_of_shards; redundancy_factor; page_size; slot_size}
    in
    let message =
      Format.asprintf
        "(shards: %d, slot size: %d, redundancy_factor: %d, page size: %d)"
        number_of_shards
        slot_size
        redundancy_factor
        page_size
    in
    let* () =
      Profiler.record_f ~cpu:(Some true) Profiler.main Debug ("SRS", [])
      @@ fun () ->
      Log.info "Loading SRS..." ;
      let* result =
        init_prover_dal
          ~find_srs_files:Tezos_base.Dal_srs.find_trusted_setup_files
          ~fetch_trusted_setup:false
          ()
      in
      Log.info "SRS loaded." ;
      let*? () =
        Result.map_error
          (fun x ->
            `Fail
              (Format.asprintf
                 "%a"
                 Tezos_error_monad.Error_monad.pp_print_trace
                 x))
          result
      in
      unit
    in
    Profiler.record_f ~cpu:(Some true) Profiler.main Debug (message, [])
    @@ fun () ->
    match make parameters with
    | Error (`Fail msg) ->
        let message = Format.asprintf "Fail: %s" msg in
        Profiler.record_f ~cpu:(Some true) Profiler.main Debug (message, [])
        @@ fun () -> Lwt.return_unit
    | Ok _ ->
        let*? dal =
          Profiler.record_f ~cpu:(Some true) Profiler.main Debug ("make", [])
          @@ fun () -> make parameters
        in
        let*? precomputation =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("shard precomputation", [])
          @@ fun () -> precompute_shards_proofs dal
        in
        let slot =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("slot generation", [])
          @@ fun () -> Cryptobox.Internal_for_tests.generate_slot ~slot_size
        in
        let*? polynomial =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("polynomial from slot", [])
          @@ fun () -> polynomial_from_slot dal slot
        in
        let _slot =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("polynomial to slot", [])
          @@ fun () -> polynomial_to_slot dal polynomial
        in
        let*? commitment =
          Profiler.record_f ~cpu:(Some true) Profiler.main Debug ("commit", [])
          @@ fun () -> commit dal polynomial
        in
        let*? commitment_proof =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("prove commitment", [])
          @@ fun () -> prove_commitment dal polynomial
        in
        let shards =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("shards from polynomial", [])
          @@ fun () -> shards_from_polynomial dal polynomial
        in
        let shard_proofs =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("prove shards", [])
          @@ fun () ->
          prove_shards dal ~precomputation ~polynomial |> Array.to_seq
        in
        let _polynomial =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("reconstruct polynomial", [])
          @@ fun () -> polynomial_from_shards dal shards
        in
        let nb_pages = slot_size / page_size in
        let page_proofs =
          Seq.ints 0 |> Seq.take 1
          |> Seq.map (fun i ->
                 Profiler.record_f
                   ~cpu:(Some true)
                   Profiler.main
                   Debug
                   ("prove page", [])
                 @@ fun () ->
                 let*? page_proof = prove_page dal polynomial i in
                 page_proof)
        in
        let is_valid =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("verify commitment", [])
          @@ fun () -> verify_commitment dal commitment commitment_proof
        in
        assert is_valid ;
        let () =
          Seq.zip shards shard_proofs
          |> Seq.take 1
          |> Seq.iter (fun (shard, shard_proof) ->
                 let message =
                   Format.asprintf
                     "verify shard (size: %d)"
                     (Bytes.length
                        (Data_encoding.Binary.to_bytes_exn
                           share_encoding
                           shard.share))
                 in
                 Profiler.record_f
                   ~cpu:(Some true)
                   Profiler.main
                   Debug
                   (message, [])
                 @@ fun () ->
                 let*? () = verify_shard dal commitment shard shard_proof in
                 ())
        in
        let bench_i_shard_verif i =
          Seq.zip shards shard_proofs |> Seq.take i |> List.of_seq |> List.split
          |> fun (shard_list, shard_proof_list) ->
          let message =
            Format.asprintf
              "verify shard multi (size: %d) (number_of_shards:%d)"
              (Bytes.length
                 (Data_encoding.Binary.to_bytes_exn
                    share_encoding
                    (List.hd shard_list).share))
              (List.length shard_list)
          in
          Profiler.record_f ~cpu:(Some true) Profiler.main Debug (message, [])
          @@ fun () ->
          let*? () =
            verify_shard_multi dal commitment shard_list shard_proof_list
          in
          ()
        in
        bench_i_shard_verif 1 ;
        bench_i_shard_verif 5 ;
        bench_i_shard_verif 20 ;
        bench_i_shard_verif 100 ;
        bench_i_shard_verif 2048 ;
        let pages =
          Seq.ints 0 |> Seq.take nb_pages
          |> Seq.map (fun i -> Bytes.sub slot (i * page_size) page_size)
        in
        let () =
          Seq.zip (Seq.ints 0 |> Seq.take nb_pages) (Seq.zip pages page_proofs)
          |> Seq.take 1
          |> Seq.iter (fun (page_index, (page, page_proof)) ->
                 Profiler.record_f
                   ~cpu:(Some true)
                   Profiler.main
                   Debug
                   ("verify page", [])
                 @@ fun () ->
                 let*? () =
                   verify_page dal commitment ~page_index page page_proof
                 in
                 ())
        in
        let () =
          let message =
            sf "share_is_trap (number_of_shards:%d)" (Seq.length shards)
          in
          Profiler.record_f ~cpu:(Some true) Profiler.main Debug (message, [])
          @@ fun () ->
          shards
          |> Seq.iter (fun {share; index = _} ->
                 let res =
                   Tezos_crypto_dal.Trap.share_is_trap
                     Tezos_crypto.Signature.Public_key_hash.zero
                     share
                     ~traps_fraction
                 in
                 match res with
                 | Ok _is_trap -> ()
                 | Error err ->
                     Test.fail
                       "Unexpected error:@.%a@."
                       Data_encoding.Binary.pp_write_error
                       err)
        in
        Lwt.return_unit
  in
  Profiler.close_and_unplug Profiler.main instance ;
  Lwt.return_unit

(*
 * Test proposing to setup a node, and dal node, a baker, in multiple steps.
 * The scenario tries to follow the tutorial steps described in
 *    https://docs.tezos.com/tutorials/join-dal-baker
 * that concerns weeklynet. We use a sandbox node to simplify
 * Regressions in this test can highlight the tutorial is out of sync with
 * the octez components behavior, and the tutorial might have to be fixed.
 * an issue/pull request should be opened to
 * https://github.com/trilitech/tezos-developer-docs/ in these cases, in order
 * to change the tutorial
 *)
let scenario_tutorial_dal_baker =
  let description = "Test following dal and baker tutorial commands" in
  test
    ~regression:true
    ~__FILE__
    ~tags:[team; "tutorial"; "dal"; "baker"]
    ~uses:(fun _protocol ->
      [Constant.octez_agnostic_baker; Constant.octez_dal_node])
    (Printf.sprintf "%s" description)
    (fun protocol ->
      (* Note: Step 1 consists in setting up docker which we don't use
       * in this test
       *)
      Log.info "Step 2: Running octez node with adaptive issuance" ;
      with_layer1 ~event_sections_levels:[("prevalidator", `Debug)] ~protocol
      @@ fun _parameters _cryptobox node client _key ->
      Log.info "Step 3: setup a baker account" ;
      (* Generate the "my_baker" user *)
      let* my_baker = Client.gen_and_show_keys ~alias:"my_baker" client in

      (* Transfer 500k to "my_baker"
       * This value differs from the tutorial to obtain attestation rights *)
      let stake = Tez.of_int 500_000 in
      let* () =
        Client.transfer
          ~hooks
          ~giver:Constant.bootstrap1.alias
          ~receiver:my_baker.alias
          ~amount:stake
          ~burn_cap:Tez.one
          client
      in
      let* () = bake_for client in

      (* Check adaptive issuance is enabled, should be 0 *)
      let* adaptive_issuance_launch_cycle =
        Client.RPC.call ~hooks client
        @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
      in
      Check.(
        (Option.get adaptive_issuance_launch_cycle = 0)
          ~__LOC__
          Check.int
          ~error_msg:
            "Adaptive issuance will only be launched at cycle %L, expected: %R") ;

      let* balance = Client.get_balance_for ~account:my_baker.alias client in
      Check.(balance = stake)
        ~__LOC__
        Tez.typ
        ~error_msg:"Unexpected balance for 'mybaker'. Expected: %L. Got: %R" ;

      (* Register my_baker as a baker *)
      let* () = Client.register_key ~hooks my_baker.alias client in
      let* () = bake_for client in

      (* As in the tutorial, we stake 99.8% of the balance. *)
      let* () =
        Client.stake
          ~hooks
          Tez.(stake - of_int 100)
          ~staker:my_baker.alias
          client
      in
      let* () = bake_for client in

      let* _ =
        Client.RPC.call ~hooks client
        @@ RPC.get_chain_block_context_delegate my_baker.public_key_hash
      in

      (* Calculate how many cycles to wait for my_baker to be a baker *)
      let* proto_params =
        Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
      in
      let blocks_per_cycle =
        JSON.(proto_params |-> "blocks_per_cycle" |> as_int)
      in
      let num_cycles =
        7
        (* Value specified in tutorial *)
      in
      Log.info
        "Bake for %d cycles for %s to be a baker"
        num_cycles
        my_baker.alias ;
      let* () = bake_for ~count:(num_cycles * blocks_per_cycle) client in

      let* attestation_rights =
        Node.RPC.call node
        @@ RPC.get_chain_block_helper_attestation_rights
             ~delegate:my_baker.public_key_hash
             ()
      in

      let attestation_power =
        attestation_rights |> JSON.as_list |> List.length
      in
      Log.info
        "Attestation rights: %s\n%!"
        (if attestation_power > 0 then "OK" else "KO") ;
      Check.(attestation_power > 0)
        Check.int
        ~__LOC__
        ~error_msg:"Attestation rights should be acquired" ;

      (* Only test the request can be processed *)
      let* _ =
        Node.RPC.call node @@ RPC.get_chain_block_context_dal_shards ()
      in

      (* Launch dal node (Step 4) *)
      Log.info "Step 4: Run an Octez dal node" ;
      let* dal_node = make_dal_node node in

      let* topics = Dal_RPC.Local.call dal_node @@ Dal_RPC.get_topics () in
      Check.(
        (List.length topics = 0)
          int
          ~__LOC__
          ~error_msg:"Expecting a empty list of topics") ;

      let wait_join_event_promise =
        Dal_node.wait_for dal_node "dal_gs_join.v0" (fun _ -> Some ())
      in

      let all_delegates =
        Account.Bootstrap.keys |> Array.to_list |> List.cons my_baker
        |> List.map (fun key -> key.Account.alias)
      in
      Log.info "Step 5: Run an Octez baking daemon" ;
      let* _baker =
        let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
        Agnostic_baker.init
          ~event_sections_levels:[(Protocol.name protocol ^ ".baker", `Debug)]
          ~dal_node_rpc_endpoint
          ~delegates:all_delegates
          ~liquidity_baking_toggle_vote:(Some On)
          ~state_recorder:true
          ~force_apply_from_round:0
          node
          client
      in
      (* Wait for subscribed events, as expected in the tutorial *)
      let* () = wait_join_event_promise in

      let* topics = Dal_RPC.Local.call dal_node @@ Dal_RPC.get_topics () in
      Check.(
        (List.length topics > 0)
          int
          ~__LOC__
          ~error_msg:"Expecting a non-empty list of topcis") ;
      unit)

(** This test injects a DAL slot to (DAL and L1) network(s) via the rollup node
    using {!post_local_dal_injection} rollup RPC. It then checks that the slot
    is attested, which implies that the commitment is published to L1 and that
    the shards of the slot are declared available by the DAL node.  *)
let rollup_node_injects_dal_slots protocol parameters dal_node sc_node
    sc_rollup_address node client _pvm_name =
  let client = Client.with_dal_node client ~dal_node in
  let* () = Sc_rollup_node.run sc_node sc_rollup_address [] in
  let slot_index = 0 in
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_dal_slot_indices ~slot_indices:[slot_index]
  in
  let wait_injected =
    Node.wait_for node "operation_injected.v0" (fun _ -> Some ())
  in
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_local_dal_batcher_injection
         ~messages:["Hello DAL from a Smart Rollup"]
  in
  let* () = wait_injected in

  (* We check for each attestation lag whether the slot was attested. *)
  let* level = Client.level client in
  let published_level = level + 1 in
  let rec loop current_level lag_index lags =
    match lags with
    | [] -> Test.fail "Slot not attested"
    | lag :: lags ->
        let attested_level = published_level + lag in
        let* () = bake_for ~count:(attested_level - current_level) client in
        let* level = Client.level client in
        assert (level = attested_level) ;
        let* _level =
          Sc_rollup_node.wait_for_level ~timeout:10. sc_node level
        in
        let* metadata = Node.RPC.(call node @@ get_chain_block_metadata ()) in
        let* obtained_dal_attestation =
          match metadata.dal_attestation with
          | None ->
              (* Field is part of the encoding when the feature flag is true *)
              Test.fail
                "Field dal_attestation in block headers is mandatory when DAL \
                 is activated"
          | Some str ->
              let* decoded =
                Dal.Slot_availability.decode
                  protocol
                  (Node.as_rpc_endpoint node)
                  parameters
                  str
              in
              return decoded.(lag_index)
        in
        if obtained_dal_attestation.(slot_index) then (
          let expected_attestation =
            expected_attestation parameters [slot_index]
          in
          Check.(
            (expected_attestation = obtained_dal_attestation)
              (array bool)
              ~error_msg:"Expected attestation bitset %L, got %R") ;
          let* statuses =
            Sc_rollup_node.RPC.call sc_node
            @@ Sc_rollup_rpc.get_dal_injected_operations_statuses ()
          in
          match statuses with
          | [status_with_hash] ->
              let status = JSON.get "status" status_with_hash in
              let status_str = JSON.(status |-> "status" |> as_string) in
              if status_str <> "included" && status_str <> "committed" then
                Test.fail
                  "Unexpected injector operation status %s. Expecting \
                   'included' or 'committed'"
                  status_str ;
              unit
          | _ ->
              Test.fail
                "Expecting a status for 1 operation, got %d@."
                (List.length statuses))
        else loop level (lag_index + 1) lags
  in
  loop level 0 parameters.attestation_lags

(** This test verifies the optimal publication of DAL slots from a batch of
    messages into the DAL node and L1 via the rollup node DAL injector.

    The test ensures that:
    - DAL slots are published at the appropriate slot indices (0, 1, 2) and levels;
    - Messages are grouped efficiently into DAL slots.

    More precisely, we inject a list of messages into the rollup node's DAL
    injector worker and check that:

    - The first four chunks (with a name prefix [slot0_] below) fit exactly into
    one slot. They are published at some level L, with slot index 0.

    - The next three chunks (with a name prefix [slot1_]), which don't fully
    fill a DAL slot, are combined into a slot and published at level L, with slot
    index 1. In fact, the following message in the queue (namely [slot2_ck1]) is
    one byte too big to fit in the same slot as these messages. So, the latter
    is published into slot index 2 of the same level L (it cannot be combined
    with the remaining messages as well).

    - All available slot indices being used at level L, the remaining messages
    are considered at level L+1. The two last messages are published at slot
    indices 0 and 1, respectively. Message [slot3_ck_full] fits in a slot, so
    there is no need to combine it with other slots. As for
    [slot4_ck_almost_empty], which only occupies 1 byte, there are no remaining
    messages that could be combined with it.

    The test checks that the correct number of DAL slots are published and
    verifies the length of the messages within the slots.
*)
let rollup_batches_and_publishes_optimal_dal_slots _protocol parameters dal_node
    sc_node sc_rollup_address _node client _pvm_name =
  let client = Client.with_dal_node client ~dal_node in
  let* () = Sc_rollup_node.run sc_node sc_rollup_address [] in
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in

  (* We create some messages below with various sizes. *)
  Check.(
    (slot_size mod 4 = 0)
      int
      ~error_msg:"Expected DAL slots size to be a multiple of 4") ;
  let chunks_size = slot_size / 4 in

  let slot0_ck1 = String.make chunks_size 'A' in
  let slot0_ck2 = slot0_ck1 in
  let slot0_ck3 = slot0_ck2 in
  let slot0_ck4 = slot0_ck3 in

  let slot1_ck1 = String.make chunks_size 'B' in
  let slot1_ck2 = slot1_ck1 in
  let slot1_ck3 = slot1_ck2 in
  let slot2_ck1 = String.make (chunks_size + 1) 'C' in

  let slot3_ck_full = String.make slot_size 'D' in
  let slot4_ck_almost_empty = String.make 1 'E' in

  (* We define an order of addition of the messages into the DAL injection queue
     and group them in the way they are expected to be packed into DAL slots. *)
  let annotated_messages =
    [
      [slot0_ck1; slot0_ck2; slot0_ck3; slot0_ck4];
      [slot1_ck1; slot1_ck2; slot1_ck3];
      [slot2_ck1];
      [slot3_ck_full];
      [slot4_ck_almost_empty];
    ]
  in

  (* We derive from [annotated_messages] the raw list of messages that will be
     injected with RPC [post_local_dal_batcher_injection] below.*)
  let messages = List.concat annotated_messages in

  (* We define the expected number of total, valid and discarded messages as well
     as the number of expected DAL slots to publish. *)
  let total_num_messages = List.length messages in
  let expected_number_of_injected_dal_slots = List.length annotated_messages in

  (* We start by informing the DAL injector of the rollup node that we want to
     publish on slots indices 0, 1 and 2. *)
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_dal_slot_indices ~slot_indices:[0; 1; 2]
  in

  (* This promise will count the number of successfully injected messages. We
     expected seeing [total_num_messages] events [dal_message_received] before
     the promise resolves. *)
  let wait_dal_message_received =
    let countdown = ref total_num_messages in
    Sc_rollup_node.wait_for sc_node "dal_message_received.v0" (fun _ ->
        decr countdown ;
        if !countdown = 0 then Some () else None)
  in
  (* Given a number [expected_num_published_slots] of slots to publish, this
     function returns a promise that resolves once the corresponding number of
     expected events [inject_dal_slot_from_messages] are seen. The function
     returns the payloads of the events in a list as tuples: [(data_size,
     num_messages, published_level, slot_index)]. *)
  let wait_inject_dal_slot_from_messages ~expected_num_published_slots =
    let open JSON in
    let countdown = ref expected_num_published_slots in
    let published = ref [] in
    let* () =
      Sc_rollup_node.wait_for
        sc_node
        "inject_dal_slot_from_messages.v0"
        (fun json ->
          let data_size = get "data_size" json |> as_int in
          let num_messages = get "num_messages" json |> as_int in
          let level = get "level" json |> as_int in
          (* The slot was injected at leve [level]. We assume that it will be
             included at the next level, which will be the "published
             level". *)
          let published_level = level + 1 in
          let slot_index = get "slot_index" json |> as_int in
          published :=
            (data_size, num_messages, published_level, slot_index) :: !published ;
          decr countdown ;
          if !countdown = 0 then Some () else None)
    in
    Lwt.return (List.rev !published)
  in
  (* We finally inject the messages via post_local_dal_batcher_injection into
     the DAL injection queue. *)
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_local_dal_batcher_injection ~messages
  in

  (* We wait until all injected messages are processed. *)
  let* () = wait_dal_message_received in

  (* Before baking one block and triggering the three first slots publication,
     we create the promise that will track the [inject_dal_slot_from_messages]
     events and return injected slots information. *)
  let* published_slots_1 =
    let wait_inject_dal_slot_from_messages_first_level =
      wait_inject_dal_slot_from_messages ~expected_num_published_slots:3
    in
    let* () = bake_for client in
    let* level = Client.level client in
    let* _level = Sc_rollup_node.wait_for_level ~timeout:15. sc_node level in
    wait_inject_dal_slot_from_messages_first_level
  in

  (* Once the three first slots are published, we do the same thing for the two
     other expected slots at the next level. *)
  let* published_slots_2 =
    let wait_inject_dal_slot_from_messages_second_level =
      wait_inject_dal_slot_from_messages ~expected_num_published_slots:2
    in
    let* () = bake_for client in
    let* level = Client.level client in
    let* _level = Sc_rollup_node.wait_for_level ~timeout:10. sc_node level in
    wait_inject_dal_slot_from_messages_second_level
  in

  (* We sort the resulting published slots information by level and slot
     indices. *)
  let published_slots =
    published_slots_2 @ published_slots_1
    |> List.fast_sort
         (fun
           (_data_size, _num_messages, level, slot_index)
           (_data_size', _num_messages', level', slot_index')
         ->
           let c = level - level' in
           if c <> 0 then c else slot_index - slot_index')
  in

  (* We check that all messages are published. *)
  let count_published_messages =
    List.fold_left
      (fun acc (_data_size, num_messages, _level, _slot_index) ->
        num_messages + acc)
      0
      published_slots
  in
  Check.(
    (total_num_messages = count_published_messages)
      int
      ~error_msg:"Expected number of batched messages %L. Got %R") ;

  (* We check that the published number of DAL slots is as expected. *)
  Check.(
    (expected_number_of_injected_dal_slots = List.length published_slots)
      int
      ~error_msg:"Expected published DAL slots is %L. Got %R") ;

  (* Finally, we check that published slots have the expected size w.r.t. to our
     initial packs of messages. *)
  let rec check_packs published_slots annotated_messages =
    match (published_slots, annotated_messages) with
    | [], [] -> ()
    | ( (data_size, _num_messages, _level, _slot_index) :: published_slots,
        pack :: annotated_messages ) ->
        let sz = List.fold_left (fun sz msg -> sz + String.length msg) 0 pack in
        Check.(
          (sz = data_size)
            int
            ~error_msg:"Expected published DAL slots size is %L. Got %R") ;
        check_packs published_slots annotated_messages
    | [], _ | _, [] -> Test.fail "Unreachable case"
  in
  check_packs published_slots annotated_messages ;
  unit

(* We have a bootstrap node, a producer node and an attester node for a new
   attester. We check that as soon as the attester is in the DAL committee it
   attests. *)
let test_new_attester_attests protocol dal_parameters _cryptobox node client
    dal_bootstrap =
  let peer_id dal_node = Dal_node.read_identity dal_node in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let slot_index = 0 in
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let peers = [Dal_node.listen_addr dal_bootstrap] in

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () =
    Dal_node.init_config ~operator_profiles:[slot_index] ~peers producer
  in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator slot_index])
  in
  Log.info "Slot producer DAL node is running" ;

  (* Set up a new account that holds a big amount of tez and make sure it can be
     an attester. *)
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* balance =
    Client.get_balance_for ~account:Constant.bootstrap1.alias client
  in
  let amount = Tez.(balance - one) in
  let* new_account = Client.gen_and_show_keys client in
  let* () =
    Client.transfer
      ~giver:Constant.bootstrap1.alias
      ~receiver:new_account.alias
      ~amount
      ~burn_cap:Tez.one
      client
  in
  let* () = bake_for client in
  let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client in
  let* () = bake_for client in
  let* () = Client.register_key new_account.alias client in
  let* () = bake_for client in
  let* () = Client.stake ~staker:new_account.alias Tez.(amount /! 2L) client in

  let attester = Dal_node.create ~name:"attester" ~node () in
  let* () =
    Dal_node.init_config
      ~attester_profiles:[new_account.public_key_hash]
      ~peers
      attester
  in
  let* () = Dal_node.run ~event_level:`Debug attester in
  let client = Client.with_dal_node client ~dal_node:attester in

  let num_cycles = 1 + consensus_rights_delay in
  let* level = Client.level client in

  let lag = dal_parameters.attestation_lag in
  (* We need to publish at the level [n - lag], where [n] is the first level
     where we should see the attestation of the new attester. Level [n] is
     [first_level_in_committee + 1] (because the attestation is included in the
     following block). *)
  let first_level_in_committee =
    (* plus 1 because the first cycle starts at level 1 *)
    (num_cycles * blocks_per_cycle) + 1
  in
  let published_level = first_level_in_committee + 1 - lag in
  Log.info
    "first_level_in_committee = %d; published_level = %d"
    first_level_in_committee
    published_level ;

  let check_graft_promises =
    let* id_attester = peer_id attester in
    let* id_producer = peer_id producer in
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index
         (attester, id_attester)
         (producer, id_producer)
         new_account.public_key_hash
  in

  Log.info "Bake blocks up to level %d" (published_level - 1) ;
  let* () = bake_for ~count:(published_level - 1 - level) client in

  let wait_for_shards_promises =
    let* assigned_shard_indexes =
      Dal_RPC.(
        call attester
        @@ get_assigned_shard_indices
             ~level:first_level_in_committee
             ~pkh:new_account.public_key_hash)
    in
    wait_for_shards_promises
      ~dal_node:attester
      ~storage_profile:`Cache_only
      ~shards:assigned_shard_indexes
      ~published_level
      ~slot_index
  in
  let* level = Client.level client in
  Log.info
    "Current level is %d, publish a slot for level %d"
    level
    published_level ;
  let* _ =
    Helpers.publish_and_store_slot
      client
      producer
      Constant.bootstrap2
      ~index:slot_index
    @@ Helpers.make_slot ~slot_size "SLOTDATA"
  in
  let* () = bake_for client in
  (* At this point the attester node fetches the DAL committee for level
     [first_level_in_committee] and changes topics. *)
  let* manager_ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:3 ()
  in
  Check.(
    (JSON.as_list manager_ops |> List.length <> 0)
      int
      ~error_msg:
        "Expected the commitment to be published, but no manager operation was \
         included.") ;

  Log.info "Waiting for grafting of the attester - producer connection" ;
  (* This is important because the attester and the producer should connect and
     join the relevant mesh, before the producer attempts to send its
     shards. Normally there the time between two levels to do that, but since
     we're baking in the past, this time is very short, so without this explicit
     wait the test would be flaky. *)
  let* () = check_graft_promises in

  Log.info "Bake another block, so that the producer sends the shards" ;
  let* () = bake_for client in
  let () = Log.info "Waiting for the attester to receive its shards" in
  let* () = wait_for_shards_promises in

  Log.info "Bake blocks up to level %d" (first_level_in_committee - 1) ;
  let* () = bake_for ~count:(lag - 3) client in
  let* level = Client.level client in
  Log.info "Current level is %d" level ;
  Check.(
    (level = first_level_in_committee - 1)
      int
      ~error_msg:"Expected current level to be %R, got %L") ;
  Log.info
    "At level %d the new attester is not yet in the committee, at the next \
     level it will be"
    level ;
  let* () =
    check_in_TB_committee
      ~__LOC__
      ~protocol
      node
      new_account.public_key_hash
      ~inside:false
      ~level
  in
  let* () =
    Dal.Committee.check_is_in
      ~__LOC__
      node
      new_account.public_key_hash
      ~inside:false
      ~level
  in
  Log.info "Bake one more block for %s to be in the committee" new_account.alias ;
  let* () = bake_for client in
  let* () =
    check_in_TB_committee
      ~__LOC__
      ~protocol
      node
      new_account.public_key_hash
      ~level:(level + 1)
  in
  let* () =
    Dal.Committee.check_is_in
      ~__LOC__
      node
      new_account.public_key_hash
      ~level:(level + 1)
  in

  Log.info "Bake a block with all accounts, including the new account" ;
  let* () =
    let bootstrap_accounts =
      Array.to_list Account.Bootstrap.keys
      |> List.map (fun a -> a.Account.public_key_hash)
    in
    bake_for
      ~delegates:(`For (new_account.public_key_hash :: bootstrap_accounts))
      client
  in
  let* json =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
  in
  let* attested_level = Client.level client in
  let dal_attestation_opt =
    List.find_map
      (fun json ->
        let contents = JSON.(json |-> "contents" |> as_list) |> List.hd in
        let delegate =
          JSON.(contents |-> "metadata" |-> "delegate" |> as_string)
        in
        let kind = JSON.(contents |-> "kind" |> as_string) in
        if
          delegate = new_account.public_key_hash
          && kind = "attestation_with_dal"
        then Some JSON.(contents |-> "dal_attestation" |> as_string)
        else None)
      (JSON.as_list json)
  in
  let* new_attester_attested_slot =
    match dal_attestation_opt with
    | None -> return false
    | Some dal_attestation ->
        Dal.is_slot_attested_in_bitset
          ~endpoint:(Node.as_rpc_endpoint node)
          ~protocol
          ~dal_parameters
          ~attested_level
          ~published_level
          ~slot_index
          ~dal_attestation
  in
  Check.is_true
    ~__LOC__
    new_attester_attested_slot
    ~error_msg:
      (Format.sprintf
         "Expected new attester to attest slot %d from published_level %d"
         slot_index
         published_level) ;
  unit

let pair_up ~error_msg =
  let rec pair_up = function
    | [] -> []
    | [_] -> Test.fail ~__LOC__ error_msg
    | x :: y :: rest -> (x, y) :: pair_up rest
  in
  pair_up

let extract_dal_balance_updates balance_updates =
  List.filter_map
    (fun (json1, json2) ->
      let change1 = JSON.(json1 |-> "change" |> as_int) in
      let change2 = JSON.(json2 |-> "change" |> as_int) in
      Check.(
        (-change1 = change2)
          ~__LOC__
          int
          ~error_msg:"Expected 'change' to match; got %L and %R") ;
      let kind = JSON.(json1 |-> "kind" |> as_string) in
      if
        (not (String.equal kind "minted"))
        && not (String.equal kind "accumulator")
      then
        Test.fail
          ~__LOC__
          "Expected a 'minted' or 'accumulator' kind, got '%s'"
          kind ;
      let category1 = JSON.(json1 |-> "category" |> as_string) in
      if String.equal category1 "DAL attesting rewards" then
        let kind2 = JSON.(json2 |-> "kind" |> as_string) in
        match kind2 with
        | "burned" ->
            let category2 = JSON.(json2 |-> "category" |> as_string) in
            Check.(
              (category2 = "lost DAL attesting rewards")
                ~__LOC__
                string
                ~error_msg:
                  "Expected a 'lost DAL attesting rewards' category, got %L") ;
            let delegate = JSON.(json2 |-> "delegate" |> as_string) in
            Some (`Lost (delegate, change2, json2))
        | "freezer" ->
            let delegate =
              JSON.(json2 |-> "staker" |-> "baker_own_stake" |> as_string)
            in
            Some (`Got (delegate, change2, json2))
        | "contract" ->
            let delegate = JSON.(json2 |-> "contract" |> as_string) in
            Some (`Got (delegate, change2, json2))
        | _ -> Test.fail ~__LOC__ "Unexpected balance update kind %s" kind2
      else None)
    balance_updates

(* This function checks the fields of the [dal_participation] RPC result. It is
   supposed to be used only by the test scenario below, as it assumes
   [expected_attested_shards] is either 0 or [expected_attestable_shards],
   [denounced] is [false], and there are only two kinds of transfers, both to
   the delegate itself, as there are no co-stakers in the scenario below. *)
let check_participation_and_rewards participation ~expected_assigned_shards
    ~expected_attestable_slots ~attesting_reward_per_shard dal_balance_updates
    delegate ~sufficient_participation =
  Check.is_false
    participation.RPC.denounced
    ~__LOC__
    ~error_msg:"The delegate was unexpectedly denounced" ;
  Check.(
    (participation.expected_assigned_shards_per_slot = expected_assigned_shards)
      ~__LOC__
      int
      ~error_msg:"Unexpected number of assigned shards. Expected %R, got %L") ;
  let expected_attested_slots =
    if sufficient_participation then expected_attestable_slots else 0
  in
  Check.(
    (participation.delegate_attested_dal_slots = expected_attested_slots)
      ~__LOC__
      int
      ~error_msg:"Expected that the delegate has attested %R slots, got %L") ;
  Check.(
    (participation.delegate_attestable_dal_slots = expected_attestable_slots)
      ~__LOC__
      int
      ~error_msg:"Expected that there are %R attestable slots, got %L") ;
  Check.(
    (participation.sufficient_dal_participation = sufficient_participation)
      ~__LOC__
      bool
      ~error_msg:"Expected sufficient_dal_participation to be %R, got %L") ;
  let dal_rewards = expected_assigned_shards * attesting_reward_per_shard in
  Check.(
    (Tez.to_mutez participation.expected_dal_rewards = dal_rewards)
      ~__LOC__
      int
      ~error_msg:
        ("Unexpected rewards for delegate " ^ delegate ^ ": expected %L, got %R")) ;
  let get_delegate = function
    | `Got (delegate, _amount, _json) | `Lost (delegate, _amount, _json) ->
        delegate
  in
  let dal_rewards =
    List.filter_map
      (fun item ->
        let item_delegate = get_delegate item in
        if String.equal item_delegate delegate then Some item else None)
      dal_balance_updates
  in
  let get_json_list =
    List.map (function `Got (_, _, json) | `Lost (_, _, json) ->
        JSON.encode json)
  in
  let rewards =
    if sufficient_participation then
      match dal_rewards with
      | [`Got (_, amount1, _); `Got (_, amount2, _)] ->
          (* one corresponds to the liquid rewards and one to the frozen ones (but
             we do not care here) *)
          amount1 + amount2
      | _ ->
          Test.fail
            ~__LOC__
            "Unexpected balance updates for the delegate %s: %a"
            delegate
            Format.(pp_print_list pp_print_string)
            (get_json_list dal_rewards)
    else
      match dal_rewards with
      | [`Lost (_, amount, _)] -> amount
      | _ ->
          Test.fail
            ~__LOC__
            "Unexpected balance updates for the delegate %s: %a"
            delegate
            Format.(pp_print_list pp_print_string)
            (get_json_list dal_rewards)
  in
  Check.(
    (Tez.to_mutez participation.expected_dal_rewards = rewards)
      ~__LOC__
      int
      ~error_msg:
        ("Unexpected rewards for delegate " ^ delegate ^ ": expected %L, got %R"))

let check_participation_and_rewards node ~expected_assigned_shards
    ~expected_attestable_slots =
  let* metadata = Node.RPC.call node @@ RPC.get_chain_block_metadata_raw () in
  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  let balance_updates =
    pair_up
      balance_updates
      ~error_msg:"The list of balance updates has an odd number of elements"
  in
  let dal_balance_updates =
    extract_dal_balance_updates balance_updates
    |>
    (* sort them for proper regression output *)
    List.sort (fun e1 e2 ->
        match (e1, e2) with
        | `Got (d1, a1, _), `Got (d2, a2, _)
        | `Lost (d1, a1, _), `Lost (d2, a2, _) ->
            let c = String.compare d1 d2 in
            if c = 0 then a1 - a2 else c
        | `Got _, `Lost _ -> 1
        | `Lost _, `Got _ -> -1)
  in
  List.iter
    (function
      | `Got (_, _, json) | `Lost (_, _, json) ->
          Regression.capture @@ JSON.encode json)
    dal_balance_updates ;
  let* attesting_reward_per_shard =
    let* json =
      Node.RPC.call ~rpc_hooks node
      @@ RPC.get_chain_block_context_issuance_expected_issuance ()
    in
    return @@ JSON.(json |=> 0 |-> "dal_attesting_reward_per_shard" |> as_int)
  in
  return @@ fun delegate ~sufficient_participation ->
  (* Note that at the last level in the cycle we lose information about the
     total_dal_attested_slots *)
  let* participation =
    Node.RPC.call ~rpc_hooks node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         delegate
  in
  return
  @@ check_participation_and_rewards
       participation
       ~expected_assigned_shards
       ~expected_attestable_slots
       ~attesting_reward_per_shard
       dal_balance_updates
       delegate
       ~sufficient_participation

(* We have one DAL attester node. During the second cycle, one of the bakers
   does not DAL attest sufficiently. We check that the attesters receive or not
   the DAL rewards depending on their participation. *)
let test_attesters_receive_dal_rewards _protocol dal_parameters _cryptobox node
    client dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let attestation_lag = dal_parameters.Dal.Parameters.attestation_lag in
  (* This constraint makes the test simpler; it could be lifted. *)
  assert (attestation_lag <= blocks_per_cycle) ;

  let expected_assigned_shards =
    let number_of_shards =
      dal_parameters.Dal.Parameters.cryptobox.number_of_shards
    in
    let num_delegates = Array.length Account.Bootstrap.keys in
    number_of_shards * blocks_per_cycle / num_delegates
  in
  let all_delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun key -> key.Account.public_key_hash)
  in
  let delegate_without_dal = List.hd all_delegates in
  let rest_delegates = List.tl all_delegates in
  let delegate_with_dal = List.hd rest_delegates in
  Log.info
    "delegate not running DAL: %s, delegate running DAL: %s"
    delegate_without_dal
    delegate_with_dal ;
  let* level =
    let* first_level = Node.get_level node in
    let block = string_of_int first_level in
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ~block ()
  in
  let blocks_to_bake = blocks_per_cycle - 1 - level.cycle_position in
  Log.info "Publish a slot for %d levels, up to the end of cycle" blocks_to_bake ;
  let* () =
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    repeat blocks_to_bake (fun () ->
        let wait_slot ~published_level:_ ~slot_index:_ = unit in
        let* _res =
          publish_store_and_wait_slot
            node
            client
            dal_node
            Constant.bootstrap1
            ~index:0
            ~wait_slot
            ~number_of_extra_blocks_to_bake:0
          @@ Helpers.make_slot ~slot_size "content"
        in
        unit)
  in
  let* level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  Log.info "Current level (and last published level) is %d" level.level ;
  assert (level.cycle_position = blocks_per_cycle - 1) ;

  let* check =
    let expected_attestable_slots = 0 in
    check_participation_and_rewards
      node
      ~expected_assigned_shards
      ~expected_attestable_slots
  in
  Log.info "Check that [delegate_without_dal] sufficiently participated" ;
  let* () = check delegate_without_dal ~sufficient_participation:true in

  Log.info "Bake for one more cycle" ;
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint dal_node |> Endpoint.as_string
  in
  let* () =
    bake_for
      ~delegates:(`For rest_delegates)
      ~count:blocks_per_cycle
      ~dal_node_endpoint
      client
  in

  let* check =
    (* [-1] because the participation is obtained one level before the cycle end;
       and another [-1] because the slot for the first level in the cycle was not
       published. *)
    let expected_attestable_slots =
      min attestation_lag (blocks_per_cycle - 2)
    in
    check_participation_and_rewards
      node
      ~expected_assigned_shards
      ~expected_attestable_slots
  in
  Log.info "Check that [delegate_without_dal] did not sufficiently participate" ;
  let* () = check delegate_without_dal ~sufficient_participation:false in
  Log.info "Check that [delegate_with_dal] sufficiently participated" ;
  let* () = check delegate_with_dal ~sufficient_participation:true in
  unit

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7686
   In the following accusation tests, we bake two blocks because we
   need the accusation to be introduced at level at least 10 (2 = 10 -
   attestation_lag). In protocol S we will not need this
   restriction. *)

let test_inject_accusation protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let slot_index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let lag = dal_parameters.attestation_lag in
  let lags = dal_parameters.attestation_lags in
  let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
  let commitment, proof, shards_with_proofs =
    Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
  in
  Log.info "Bake two blocks" ;
  let* () = bake_for ~count:2 client in
  let* _op_hash =
    Helpers.publish_commitment
      ~source:Constant.bootstrap2
      ~index:slot_index
      ~commitment
      ~proof
      client
  in
  (* We need to bake exactly [lag] blocks so that we can inject an attestation
     at the right level (that attests the published slot). *)
  let* () = bake_for ~count:lag client in
  Log.info "Inject an attestation" ;
  let availability = Slots [slot_index] in
  let signer = Constant.bootstrap2 in
  let* attestation, _op_hash =
    inject_dal_attestation_exn
      ~protocol
      ~signer
      availability
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* signature = Operation.sign attestation client in
  let attestation = (attestation, signature) in
  let* shard_assignments =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_dal_shards
         ~delegates:[signer.public_key_hash]
         ()
  in
  let shard_index =
    JSON.(
      shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list
      |> List.hd |> as_int)
  in
  Log.info "First shard index of the attester is %d" shard_index ;
  let shard, proof =
    Seq.find
      (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
      shards_with_proofs
    |> Option.get
  in
  let accusation =
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      ~lag_index:(List.length lags - 1)
      shard
      proof
  in
  Log.info "Inject an accusation" ;
  let* _op_hash = Operation.Anonymous.inject accusation client in
  let* () = bake_for client in
  let* ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass
         ~block:"head"
         ~validation_pass:2
         ()
  in
  Check.(List.length (JSON.as_list ops) = 1)
    ~__LOC__
    Check.(int)
    ~error_msg:"Expected exactly one anonymous op. Got: %L" ;
  unit

(* This test exercises DAL entrapment evidence validation with dynamic,
   multi-lag DAL attestations.

   Context:
   - In dynamic-lag mode, a single attestation can carry DAL slot availability
     information for multiple lags.
   - An entrapment accusation includes:
       (attestation, lag_index, slot_index, shard, proof)
     and must be checked against the publication level derived from the
     attested level and the selected lag index.
   - The goal is to ensure accusation validation is performed against the
     correct (lag, slot, level) tuple, including shard ownership checks.

   Test structure:
   - Scenario A (single-lag attestation content, cross-lag accusations):
     - Publish a on slot 0, then b on slot 0 later.
     - Build Alice's attestation at a level where:
         lag_index=0 points to b, and lag_index=1 points to a.
     - Inject four accusations:
       A.1/A.2: accuse with lag_index=1 while Alice only attested lag_index=0
                -> must fail with "delegate did not attest the DAL slot".
       A.3: accuse with lag_index=0 with a shard assigned to Alice at lag_index=1
            not lag_index=0 -> must fail with "wrong shard owner".
       A.4: accuse with lag_index=0 with an Alice-assigned shard -> must succeed.

   - Scenario B (true multi-lag attestation payload):
     - Publish four commitments:
         a-slot2, a-slot3 and later b-slot2, b-slot3.
     - Craft one attestation where:
         lag_index=0 attests slot 2, and lag_index=1 attests slot 3.
     - Inject five accusations:
       B.1: lag_index=0, slot 3 (not attested at this lag)
            -> must fail with "delegate did not attest the DAL slot".
       B.2: Same as B.1 with lag_index=1, slot 2.
       B.3: lag_index=1, slot 3 with a valid Alice shard -> must succeed.
       B.4: lag_index=0, slot 2 with a shard assigned to Alice at lag_index=1
            not lag_index=0 -> must fail with "wrong shard owner".
       B.5: lag_index=0, slot 2 with an Alice-assigned shard
            -> must succeed.

   - Scenario C (max lag sanity/parity):
     - Publish one slot and craft an attestation targeting the largest lag
       (i.e. lag_index = number_of_lags - 1).
     - Inject one accusation with matching lag/slot/shard
       -> must succeed.

   Each scenario bakes after successful accusations and verifies that a
   dal_entrapment_evidence operation is present in validation pass 2. *)
let test_inject_accusation_dynamic_multi_lag protocol dal_parameters cryptobox
    node client _bootstrap_key =
  let attestation_lags = dal_parameters.Dal.Parameters.attestation_lags in
  let number_of_lags = List.length attestation_lags in
  let* () =
    if number_of_lags < 3 then
      Test.fail
        "Unexpected attestation_lags. Expected at least 3 lags, got [%s]"
        (dal_parameters.attestation_lags |> List.map string_of_int
       |> String.concat "; ")
    else unit
  in
  let lag_with_index_0 = List.nth attestation_lags 0 in
  let lag_with_index_1 = List.nth attestation_lags 1 in
  (* We have the invariant that [dal_parameters.Dal.Parameters.attestation_lag]
     is the same as [List.nth attestation_lags (number_of_lags - 1)]. *)
  let max_lag = dal_parameters.Dal.Parameters.attestation_lag in
  let slot_size = dal_parameters.cryptobox.slot_size in
  let alice = Constant.bootstrap2 in
  let publish_commitment ?(source = Constant.bootstrap1) ~index ~message () =
    let slot = Helpers.(bytes_of_slot (make_slot ~slot_size message)) in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    let* _op_hash =
      Helpers.publish_commitment ~source ~index ~commitment ~proof client
    in
    return (List.of_seq shards_with_proofs)
  in
  let publish_commitment_and_bake ?source ~index ~message () =
    let* shards_with_proofs = publish_commitment ?source ~index ~message () in
    Log.info "Bake after publication for slot index=%d" index ;
    let* () = bake_for client in
    let* published_level = Node.get_level node in
    Log.info
      "Publication included at level=%d for slot index=%d (message=%s)"
      published_level
      index
      message ;
    return (published_level, shards_with_proofs)
  in
  let ensure_level target_level =
    let* current_level = Node.get_level node in
    if current_level > target_level then
      Test.fail
        "Cannot go back in time: current level=%d, target level=%d"
        current_level
        target_level
    else if current_level < target_level then
      bake_for ~count:(target_level - current_level) client
    else unit
  in
  let shard_with_proof_exn shards shard_index =
    match
      List.find_opt
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards
    with
    | Some x -> x
    | None ->
        Test.fail
          "Could not find shard index %d in locally computed shards"
          shard_index
  in
  let get_alice_indexes ~lag_index ~attestation_level =
    let committee_level =
      attestation_level + max_lag - List.nth attestation_lags lag_index
    in
    let* shard_assignments =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards
           ~level:committee_level
           ~delegates:[alice.public_key_hash]
           ()
    in
    match JSON.as_list shard_assignments with
    | [json] ->
        let indexes = JSON.(json |-> "indexes" |> as_list |> List.map as_int) in
        if List.is_empty indexes then
          Test.fail
            "Alice has no shard index at level %d, cannot build accusation \
             scenario"
            attestation_level
        else return indexes
    | _ ->
        Test.fail
          "No DAL shard assignment found for Alice (%s) at level %d"
          alice.public_key_hash
          attestation_level
  in
  (* [first_among_not_in from forbidden] returns the first element from [from]
     not in [forbidden]. It fails if no such element can be found.
     This function assumes both lists are sorted. *)
  let rec first_among_not_in from forbidden =
    match (from, forbidden) with
    | [], _ ->
        Test.fail
          ~__LOC__
          "Cannot find a shard assigned to Alice for lag 1 but not for lag 0."
    | from_hd :: _, [] -> from_hd
    | from_hd :: from_tl, forbidden_hd :: forbidden_tl ->
        if from_hd < forbidden_hd then from_hd
        else if from_hd = forbidden_hd then
          first_among_not_in from_tl forbidden_tl
        else first_among_not_in from forbidden_tl
  in
  let inject_accusation ~label ~error ~attestation ~lag_index ~slot_index
      ~shard_index ~shard ~proof =
    Log.info
      "[%s] Inject accusation: slot_index=%d shard_index=%d expected_error=%s"
      label
      slot_index
      shard_index
      (match error with
      | None -> "none (success expected)"
      | Some _ -> "some (failure expected)") ;
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol
        ~attestation
        ~slot_index
        ~lag_index
        shard
        proof
    in
    let* _op_hash = Operation.Anonymous.inject ?error accusation client in
    Log.info "[%s] Accusation injected as expected" label ;
    unit
  in
  let assert_dal_entrapment_in_head () =
    let* anonymous_ops =
      Node.RPC.call node
      @@ RPC.get_chain_block_operations_validation_pass
           ~block:"head"
           ~validation_pass:2
           ()
    in
    let has_dal_entrapment =
      List.exists
        (fun op ->
          let kind = JSON.(op |-> "contents" |=> 0 |-> "kind" |> as_string) in
          String.equal kind "dal_entrapment_evidence")
        (JSON.as_list anonymous_ops)
    in
    Check.is_true
      has_dal_entrapment
      ~__LOC__
      ~error_msg:"Expected a dal_entrapment_evidence operation in pass 2" ;
    unit
  in

  (* Scenario A *)
  Log.info "=== Scenario A start ===" ;
  let* _lvl_a, shards_a =
    publish_commitment_and_bake ~index:0 ~message:"a-slot0" ()
  in
  let* () =
    if lag_with_index_1 > lag_with_index_0 + 1 then
      bake_for ~count:(lag_with_index_1 - lag_with_index_0 - 1) client
    else unit
  in
  let* lvl_b, shards_b =
    publish_commitment_and_bake ~index:0 ~message:"b-slot0" ()
  in
  (* For accusations, published_level + attestation_lag = attestation_level + 1.
     To target lvl_b with lag=lag_with_index_0 use lvl_b + lag_with_index_0 - 1.
     It is the same as lvl_a + lag_with_index_1 - 1. *)
  let attestation_level_A = lvl_b + (lag_with_index_0 - 1) in
  let* () = ensure_level attestation_level_A in
  Log.info
    "Scenario A: craft attestation at level=%d lag_index=%d slots=[0]"
    attestation_level_A
    0 ;
  let* attestation_a =
    craft_dal_attestation_exn
      ~protocol
      ~level:attestation_level_A
      ~lag_index:0
      ~signer:alice
      (Slots [0])
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* signature_a = Operation.sign attestation_a client in
  let attestation_a = (attestation_a, signature_a) in
  (* To make all this final, one bakes 2 blocks. *)
  let* () = bake_for ~count:2 client in
  let* alice_indexes_lag_0 =
    get_alice_indexes ~lag_index:0 ~attestation_level:attestation_level_A
  in
  let alice_index_lag_0 = List.hd alice_indexes_lag_0 in
  let* alice_indexes_lag_1 =
    get_alice_indexes ~lag_index:1 ~attestation_level:attestation_level_A
  in
  let alice_index_lag_1 =
    first_among_not_in alice_indexes_lag_1 alice_indexes_lag_0
  in
  let shard_a_1, proof_a_1 = shard_with_proof_exn shards_a alice_index_lag_0 in
  let shard_a_2, proof_a_2 = shard_with_proof_exn shards_a alice_index_lag_1 in
  let shard_b_wrong, proof_b_wrong =
    shard_with_proof_exn shards_b alice_index_lag_1
  in
  let shard_b_ok, proof_b_ok =
    shard_with_proof_exn shards_b alice_index_lag_0
  in
  let* () =
    inject_accusation
      ~label:"A.1 not-attested(a, getting shard index with lag index 0)"
      ~error:(Some Operation.dal_entrapment_slot_not_attested)
      ~attestation:attestation_a
      ~lag_index:1
      ~slot_index:0
      ~shard_index:alice_index_lag_0
      ~shard:shard_a_1
      ~proof:proof_a_1
  in
  let* () =
    inject_accusation
      ~label:"A.2 not-attested(a, getting shard index with lag index 1)"
      ~error:(Some Operation.dal_entrapment_slot_not_attested)
      ~attestation:attestation_a
      ~lag_index:1
      ~slot_index:0
      ~shard_index:alice_index_lag_1
      ~shard:shard_a_2
      ~proof:proof_a_2
  in
  let* () =
    inject_accusation
      ~label:"A.3 wrong-owner(b)"
      ~error:(Some Operation.dal_entrapment_wrong_shard_owner)
      ~attestation:attestation_a
      ~lag_index:0
      ~slot_index:0
      ~shard_index:alice_index_lag_1
      ~shard:shard_b_wrong
      ~proof:proof_b_wrong
  in
  let* () =
    inject_accusation
      ~label:"A.4 success(b)"
      ~error:None
      ~attestation:attestation_a
      ~lag_index:0
      ~slot_index:0
      ~shard_index:alice_index_lag_0
      ~shard:shard_b_ok
      ~proof:proof_b_ok
  in
  Log.info "Scenario A: bake inclusion block for successful accusation" ;
  let* () = bake_for client in
  let* () = assert_dal_entrapment_in_head () in
  Log.info "=== Scenario A done ===" ;

  (* Scenario B (+ ownership check) *)
  Log.info "=== Scenario B start ===" ;
  let* shards_a2 =
    publish_commitment
      ~source:Constant.bootstrap1
      ~index:2
      ~message:"a-slot2"
      ()
  in
  let* lvl_a, shards_a3 =
    publish_commitment_and_bake
      ~source:Constant.bootstrap2
      ~index:3
      ~message:"a-slot3"
      ()
  in
  let* () =
    if lag_with_index_1 > lag_with_index_0 + 1 then
      bake_for ~count:(lag_with_index_1 - lag_with_index_0 - 1) client
    else unit
  in
  let* shards_b2 =
    publish_commitment
      ~source:Constant.bootstrap1
      ~index:2
      ~message:"b-slot2"
      ()
  in
  let* lvl_b, shards_b3 =
    publish_commitment_and_bake
      ~source:Constant.bootstrap2
      ~index:3
      ~message:"b-slot3"
      ()
  in
  (* For accusations, published_level + attestation_lag = attestation_level + 1.
     To target B@lvl_b with lag=lag_with_index_0 use lvl_b + lag_with_index_0 - 1. *)
  let attestation_level_B = lvl_b + (lag_with_index_0 - 1) in
  let* () = ensure_level attestation_level_B in
  Log.info
    "Scenario B: collect round/payload/slot at level=%d"
    attestation_level_B ;
  let* round =
    Client.RPC.call_via_endpoint client
    @@ RPC.get_chain_block_helper_round
         ~block:(string_of_int attestation_level_B)
         ()
  in
  let* block_payload_hash =
    Operation.Consensus.get_block_payload_hash
      ~block:(string_of_int attestation_level_B)
      client
  in
  let* slot =
    Operation.Consensus.get_attestation_slot
      ~level:attestation_level_B
      ~delegate:alice
      ~protocol
      client
  in
  let attestation_per_lag =
    Array.init number_of_lags (fun _ ->
        Array.make dal_parameters.number_of_slots false)
  in
  (* We attest A-slot3 and B-slot2.
     We positioned ourselves at a level such that lag_index_0 points to the
     publication level of B and lag_index_1 points to the publication level of A. *)
  attestation_per_lag.(0).(2) <- true ;
  attestation_per_lag.(1).(3) <- true ;
  let* dal_attestation =
    Dal.Attestations.encode
      protocol
      (Node.as_rpc_endpoint node)
      attestation_per_lag
  in
  let* attestation_B =
    Operation.Consensus.operation
      ~signer:alice
      (Operation.Consensus.attestation
         ~level:attestation_level_B
         ~round
         ~dal_attestation
         ~slot
         ~block_payload_hash
         ())
      client
  in
  let* signature_b = Operation.sign attestation_B client in
  let attestation_B = (attestation_B, signature_b) in
  let* alice_indexes_lag_0 =
    get_alice_indexes ~lag_index:0 ~attestation_level:attestation_level_B
  in
  let alice_index_lag_0 = List.hd alice_indexes_lag_0 in
  let* alice_indexes_lag_1 =
    get_alice_indexes ~lag_index:1 ~attestation_level:attestation_level_B
  in
  let alice_index_lag_1 =
    first_among_not_in alice_indexes_lag_1 alice_indexes_lag_0
  in
  let shard_a2, proof_a2 = shard_with_proof_exn shards_a2 alice_index_lag_1 in
  let shard_a3, proof_a3 = shard_with_proof_exn shards_a3 alice_index_lag_1 in
  let shard_b3, proof_b3 = shard_with_proof_exn shards_b3 alice_index_lag_0 in
  let shard_b2_wrong, proof_b2_wrong =
    shard_with_proof_exn shards_b2 alice_index_lag_1
  in
  let shard_b2_ok, proof_b2_ok =
    shard_with_proof_exn shards_b2 alice_index_lag_0
  in
  Check.(
    (attestation_level_B + 1 - lag_with_index_0 = lvl_b)
      int
      ~__LOC__
      ~error_msg:
        "Scenario B lag2 targets unexpected level: actual %L, expected %R") ;
  Check.(
    (attestation_level_B + 1 - lag_with_index_1 = lvl_a)
      int
      ~__LOC__
      ~error_msg:
        "Scenario B lag3 targets unexpected level: actual %L, expected %R") ;
  let* () =
    inject_accusation
      ~label:"B.1 slot-not-attested(b-slot3)"
      ~error:(Some Operation.dal_entrapment_slot_not_attested)
      ~attestation:attestation_B
      ~lag_index:0
      ~slot_index:3
      ~shard_index:alice_index_lag_0
      ~shard:shard_b3
      ~proof:proof_b3
  in
  let* () =
    inject_accusation
      ~label:"B.2 slot-not-attested(a-slot2)"
      ~error:(Some Operation.dal_entrapment_slot_not_attested)
      ~attestation:attestation_B
      ~lag_index:1
      ~slot_index:2
      ~shard_index:alice_index_lag_1
      ~shard:shard_a2
      ~proof:proof_a2
  in
  let* () =
    inject_accusation
      ~label:"B.3 success(a-slot3)"
      ~error:None
      ~attestation:attestation_B
      ~lag_index:1
      ~slot_index:3
      ~shard_index:alice_index_lag_1
      ~shard:shard_a3
      ~proof:proof_a3
  in
  let* () = bake_for client in
  let* () = assert_dal_entrapment_in_head () in
  let* () =
    inject_accusation
      ~label:"B.4 wrong-owner(b-slot2)"
      ~error:(Some Operation.dal_entrapment_wrong_shard_owner)
      ~attestation:attestation_B
      ~lag_index:0
      ~slot_index:2
      ~shard_index:alice_index_lag_1
      ~shard:shard_b2_wrong
      ~proof:proof_b2_wrong
  in
  let* () =
    inject_accusation
      ~label:"B.5 success(b-slot2)"
      ~error:None
      ~attestation:attestation_B
      ~lag_index:0
      ~slot_index:2
      ~shard_index:alice_index_lag_0
      ~shard:shard_b2_ok
      ~proof:proof_b2_ok
  in
  Log.info "Scenario B: bake inclusion block for successful accusation" ;
  let* () = bake_for client in
  let* () = assert_dal_entrapment_in_head () in
  Log.info "=== Scenario B done ===" ;

  (* Scenario C (lag max parity) *)
  Log.info "=== Scenario C start ===" ;
  let* lvl_c, shards_c =
    publish_commitment_and_bake ~index:0 ~message:"c-slot0" ()
  in
  (* [published_level + lag = attestation_level + 1], since attestations included
     in level N attests for block N-1.
     So for lag=5 use +4. *)
  let attestation_level_C = lvl_c + max_lag - 1 in
  let max_index = number_of_lags - 1 in
  let* () = ensure_level attestation_level_C in
  Log.info
    "Scenario C: craft attestation at level=%d lag_index=%d slots=[0]"
    attestation_level_C
    max_index ;
  let* attestation_c =
    craft_dal_attestation_exn
      ~protocol
      ~level:attestation_level_C
      ~lag_index:max_index
      ~signer:alice
      (Slots [0])
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* signature_c = Operation.sign attestation_c client in
  let attestation_c = (attestation_c, signature_c) in
  let* alice_indexes =
    get_alice_indexes ~lag_index:0 ~attestation_level:attestation_level_C
  in
  let alice_index_lag_0 = List.hd alice_indexes in
  let shard_c_ok, proof_c_ok =
    shard_with_proof_exn shards_c alice_index_lag_0
  in
  let* () =
    inject_accusation
      ~label:"C.1 success(slot0 lag5)"
      ~error:None
      ~attestation:attestation_c
      ~lag_index:max_index
      ~slot_index:0
      ~shard_index:alice_index_lag_0
      ~shard:shard_c_ok
      ~proof:proof_c_ok
  in
  Log.info "Scenario C: bake inclusion block for successful accusation" ;
  let* () = bake_for client in
  let* () = assert_dal_entrapment_in_head () in
  Log.info "=== Scenario C done ===" ;
  unit

type tz4_account = {delegate_key : Account.key; companion_key : Account.key}

(* [round_robin n l] distributes the values of [l] in an array of [n] elements.
   so [round_robin 3 [a1; ..; a10] = [|[a1;a4;a7;a10]; [a2;a5;a8]; [a3;a6;a9]|]].
*)
let round_robin n l =
  let rec bis acc k = function
    | [] -> Array.map List.rev acc
    | hd :: tl ->
        let acc_local = acc.(k) in
        let () = acc.(k) <- hd :: acc_local in
        bis acc ((k + 1) mod n) tl
  in
  bis (Array.init n (fun _ -> [])) 0 l

let create_tz4_accounts_stake_and_wait ~funders ~client ~node nb_to_create =
  Log.info "Generate tz4 keys" ;
  let* new_tz4_accounts =
    Lwt_list.map_s
      (fun index ->
        Client.gen_and_show_keys
          ~sig_alg:"bls"
          ~alias:("bls_account_" ^ string_of_int (1 + index))
          client)
      (List.init nb_to_create Fun.id)
  in
  let* () = bake_for client in
  Log.info "We have the accounts, let's fund them" ;
  let* _ophs =
    Lwt.all
    @@ List.map2
         (fun accounds_to_fund source ->
           (* We give 500k tez to each account. *)
           let amount = 500_000 * 1_000_000 in
           let transfers =
             List.map
               (fun dest -> Operation.Manager.transfer ~dest ~amount ())
               accounds_to_fund
           in
           let* counter = Operation.Manager.get_next_counter ~source client in
           let batch =
             Operation.Manager.make_batch ~source ~counter transfers
           in
           Operation.Manager.inject batch client)
         (Array.to_list @@ round_robin (List.length funders) new_tz4_accounts)
         funders
  in
  let* () = bake_for client in
  Log.info "All keys funded, let's set them as delegates" ;
  let* () =
    Lwt_list.iter_s
      (fun account ->
        Client.set_delegate
          ~src:account.Account.alias
          ~delegate:account.Account.alias
          client)
      new_tz4_accounts
  in
  let* () = bake_for client in
  Log.info "All keys are delegate, let's attach a companion key to them" ;
  let* companions =
    Lwt_list.map_s
      (fun account ->
        Client.update_fresh_companion_key ~algo:"bls" account client)
      new_tz4_accounts
  in
  let* () = bake_for client in
  Log.info "All keys have a companion, let's stake" ;
  let* () =
    Lwt_list.iter_s
      (fun source ->
        Client.stake ~staker:source.Account.alias Tez.(of_int 100_000) client)
      new_tz4_accounts
  in
  let* RPC.{cycle; _} =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  Log.info "All keys have staked, let's wait for them to have rights" ;
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let* () =
    Client.bake_until_cycle
      ~target_cycle:(cycle + consensus_rights_delay + 1)
      client
  in
  return
  @@ List.map2
       (fun delegate_key companion_key -> {delegate_key; companion_key})
       new_tz4_accounts
       companions

let test_aggregation_required_to_pass_quorum protocol dal_parameters _cryptobox
    node client dal_node =
  let slot_index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator slot_index])) in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let lag = dal_parameters.attestation_lag in
  let* _tz4_accounts =
    create_tz4_accounts_stake_and_wait
      ~funders:(Account.Bootstrap.keys |> Array.to_list)
      ~node
      ~client
      25
  in
  Log.info "Enough waiting, let's publish a commitment" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      message
  in
  let* () = bake_for client in
  let* published_level = Node.get_level node in
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint dal_node |> Endpoint.as_string
  in
  Log.info "Let's wait for the attestation of this publication to be sent." ;
  let* () = bake_for ~count:lag client ~dal_node_endpoint in
  let* attestations =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
  in
  let aggregated_attestation =
    JSON.(
      List.find
        (fun attestation ->
          attestation |-> "contents" |> as_list |> List.hd |-> "kind"
          |> as_string = "attestations_aggregate")
        (as_list attestations))
  in
  let aggregated_attestation_consensus_power =
    JSON.(
      aggregated_attestation |-> "contents" |> as_list |> List.hd |-> "metadata"
      |-> "total_consensus_power"
      |> fun x ->
      if Protocol.number protocol >= 024 then x |-> "slots" |> as_int
      else x |> as_int)
  in
  let* constants =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_committee_size =
    JSON.(constants |-> "consensus_committee_size" |> as_int)
  in
  Check.(
    aggregated_attestation_consensus_power
    >= consensus_committee_size
       * (100 - dal_parameters.attestation_threshold)
       / 100)
    ~__LOC__
    Check.int
    ~error_msg:
      "The consensus power of the tz4 accounts is not sufficient to ensure \
       that the ability to read inside the aggregated attestations is required \
       for the slot to be protocol attested." ;
  let check_at_level ~level ~lag ~lag_index =
    let* metadata =
      Node.RPC.(
        call node @@ get_chain_block_metadata ~block:(string_of_int level) ())
    in
    match metadata.dal_attestation with
    | None ->
        Test.fail
          "Field dal_attestation in block headers is mandatory when DAL is \
           activated"
    | Some str ->
        let* decoded =
          Dal.Slot_availability.decode
            protocol
            (Node.as_rpc_endpoint node)
            dal_parameters
            str
        in
        let bitset = decoded.(lag_index) in
        let attested_count =
          Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 bitset
        in
        Log.info
          "Check dal_attestation at level=%d (lag=%d, lag_index=%d): \
           attested_count=%d slot[%d]=%b"
          level
          lag
          lag_index
          attested_count
          slot_index
          bitset.(slot_index) ;
        return (attested_count = 1 && bitset.(slot_index))
  in
  let lags = dal_parameters.attestation_lags in
  let* results =
    Lwt_list.mapi_s
      (fun lag_index lag ->
        let attested_level = published_level + lag in
        check_at_level ~level:attested_level ~lag ~lag_index)
      lags
  in
  if List.exists Fun.id results then unit
  else
    Test.fail
      "Slot was NOT protocol attested at any expected attested level \
       (published_level=%d, lags=%s)"
      published_level
      (String.concat "," (List.map string_of_int lags))
      unit

let test_inject_accusation_aggregated_attestation nb_attesting_tz4 protocol
    dal_parameters _cryptobox node client dal_node =
  let slot_index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator slot_index])) in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.number_of_slots in
  let lag = dal_parameters.attestation_lag in
  let* tz4_accounts =
    create_tz4_accounts_stake_and_wait
      ~funders:[Constant.bootstrap1]
      ~node
      ~client
      5
  in
  Log.info "Enough waiting, let's publish a commitment" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      message
  in
  (* We need to bake exactly [lag] blocks so that we can inject an attestation
     at the right level (that attests the published slot). *)
  let* publication_level = Client.bake_for_and_wait_level client in
  Log.info "Attestation lag should pass before sending an attestation" ;
  let* () = bake_for ~count:(lag - 1) client in
  Log.info "Inject an attestation" ;
  let level = publication_level + lag - 1 in
  (* BLS consensus keys are now activated *)
  let* round = Baker_test.fetch_round client in
  let* branch = Operation.Consensus.get_branch ~attested_level:level client in
  let* block_payload_hash =
    Operation.Consensus.get_block_payload_hash
      ~block:(string_of_int level)
      client
  in
  let* dal_attestation =
    Dal.Attestations.encode_for_one_lag
      protocol
      (Node.as_rpc_endpoint node)
      dal_parameters
    @@ Array.init number_of_slots (fun i -> i = slot_index)
  in
  let faulty_bakers, other_tz4 =
    Tezos_stdlib.TzList.split_n nb_attesting_tz4 tz4_accounts
  in
  let* () =
    Lwt_list.iter_s
      (fun {delegate_key; companion_key} ->
        let* first_slot =
          Operation.Consensus.get_attestation_slot
            ~level
            ~protocol
            ~delegate:delegate_key
            client
        in
        let attestation =
          Operation.Consensus.attestation
            ~dal_attestation
            ~slot:first_slot
            ~level
            ~round
            ~block_payload_hash
            ()
        in
        let* _op_hash =
          Operation.Consensus.inject
            ~protocol
            ~branch
            ~signer_companion:companion_key
            ~signer:delegate_key
            attestation
            client
        in
        unit)
      faulty_bakers
  in
  Log.info "Attestation injected" ;
  (* Since we manually injected the attestation for the faulty baker,
     we have to prevent [bake_for] to inject another one in the mempool.
  *)
  let all_other_delegates =
    List.map (fun {delegate_key; _} -> delegate_key) other_tz4
    @ Constant.all_secret_keys
  in
  let* () =
    bake_for
      ~delegates:
        (`For
           (List.map
              (fun account -> account.Account.public_key_hash)
              all_other_delegates))
      client
  in
  (* Let's wait 2 levels for the block to be finalized. *)
  let* () = bake_for ~count:2 client in
  (* Now we expect the DAL node to be crafting the denunciation,
     hence it should be included in next block.
  *)
  let* () = bake_for client in
  let* anonymous_ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass
         ~block:"head"
         ~validation_pass:2
         ()
  in
  Check.(List.length (JSON.as_list anonymous_ops) = nb_attesting_tz4)
    ~__LOC__
    Check.int
    ~error_msg:"Expected exactly %R anonymous op. Got: %L" ;
  let contents = JSON.(anonymous_ops |=> 0 |-> "contents" |=> 0) in
  let kind = JSON.(contents |-> "kind" |> as_string) in
  let attestation_kind =
    JSON.(contents |-> "attestation" |-> "operations" |-> "kind" |> as_string)
  in
  Check.(
    ((kind, attestation_kind)
    = ("dal_entrapment_evidence", "attestations_aggregate"))
      ~__LOC__
      (tuple2 string string))
    ~error_msg:
      "Expected the anonymous operation to be the denunciation of attestation \
       aggregation. Got: %L" ;
  unit

(* Publishing and attesting should work.
   A producer DAL node publishes "Hello world!" (produced with key [bootstrap1]) on a slot.
   An attestation, which attests the block is emitted with key [bootstrap2].
   The published commitment is attested.*)
let test_producer_attester (protocol : Protocol.t)
    (dal_parameters : Dal_common.Parameters.t) (_cryptobox : Cryptobox.t)
    (node : Node.t) (client : Client.t) (_dal_node : Dal_node.t) : unit Lwt.t =
  let {log_step} = init_logger () in
  let index = 3 in
  let slot_size = dal_parameters.cryptobox.slot_size in
  log_step "Declaration of a producer" ;
  let producer_node = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] producer_node in
  let* () = Dal_node.run ~wait_ready:true producer_node in
  log_step "Two blocks are baked, because why not" ;
  let* () = bake_for ~count:2 client in
  log_step "The producer crafts a commitment and publish it" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      producer_node
      Constant.bootstrap1
      ~index
      message
  in
  let* lvl_publish = Client.level client in
  let lag = dal_parameters.attestation_lag in
  Log.info "We are at level %d and the lag is %d" lvl_publish lag ;
  (* We bake [lag] blocks, one which will include the publication of the commitment
     and [lag-1] just to wait. *)
  log_step "Let's wait for the attestation lag while baking" ;
  let* () = bake_for ~count:lag client in
  log_step "It is now time to attest" ;
  (* We want the attestation to be included in the block [lag] after the one containing the shards,
     so we have to inject them now. *)
  let* _ =
    inject_dal_attestations
      ~protocol
        (* Since the attestation threshold of the test is set to 1%,
           having only [bootstrap2] signing is sufficient. *)
      ~signers:[Constant.bootstrap2]
      (Slots [index])
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* lvl_attest = Client.level client in
  Check.(
    (lvl_publish + lag = lvl_attest)
      int
      ~error_msg:"Current level of the client is unexpected") ;
  log_step "Bake a block containing this attestation" ;
  let map_alias = List.map (fun x -> x.Account.alias) in
  (* Since the function [bake_for] crafts the attestation for delegates,
     we do not want it to craft a concurrent attestation for [bootstrap2].
     One which attests a DAL slot has already been injected. *)
  let* () =
    bake_for
      ~delegates:
        (`For
           (map_alias Constant.[bootstrap1; bootstrap3; bootstrap4; bootstrap5]))
      client
  in
  log_step "Bake a few more blocks before calling RPC" ;
  let final_block_promise =
    wait_for_layer1_final_block producer_node (lvl_attest + 2)
  in
  let* () = bake_for ~count:3 client in
  let* () = final_block_promise in
  log_step "Call to the RPC of the DAL node to check that the slot is attested." ;
  let* status =
    Dal_RPC.(
      call producer_node
      (* The level the commitment is included in a block is the first one crafted after publication. *)
      @@ get_level_slot_status ~slot_level:(lvl_publish + 1) ~slot_index:index)
  in
  Log.info "Status is %a" Dal_RPC.pp_slot_id_status status ;
  log_step "Final check." ;
  let* () =
    check_slot_status
      ~__LOC__
      producer_node
      ~expected_status:(Dal_RPC.Attested lag)
      ~check_attested_lag:`At_most
      ~slot_level:(lvl_publish + 1)
      ~slot_index:index
  in
  unit

(* Check if the [attester_did_not_attest] warning is correctly emitted.
   This test is a variation of [test_producer_attester] where an attestation
   not attesting the published DAL slot is injected. *)
let test_attester_did_not_attest (protocol : Protocol.t)
    (dal_parameters : Dal_common.Parameters.t) (_cryptobox : Cryptobox.t)
    (node : Node.t) (client : Client.t) (_dal_node : Dal_node.t) : unit Lwt.t =
  let {log_step} = init_logger () in
  let index = 3 in
  let slot_size = dal_parameters.cryptobox.slot_size in
  log_step "Declaration of a producer" ;
  let producer_node = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] producer_node in
  let* () = Dal_node.run ~wait_ready:true producer_node in
  log_step "Declaration of an attester" ;
  let attester_node = Dal_node.create ~name:"attester" ~node () in
  let* () =
    Dal_node.init_config
      ~peers:[Dal_node.listen_addr producer_node]
      ~attester_profiles:[Constant.bootstrap2.public_key_hash]
      attester_node
  in
  let* () = Dal_node.run ~wait_ready:true attester_node in
  log_step
    "We promise the attester_did_not_attest_slot will be emitted by the \
     [attester node]" ;
  let not_attested_by_bootstrap2_promise =
    Dal_node.wait_for attester_node "dal_attester_did_not_attest.v0" (fun _ ->
        Some ())
  in
  log_step "The producer crafts a commitment and publish it" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      producer_node
      Constant.bootstrap1
      ~index
      message
  in
  let* () = bake_for client in

  let* published_level = Client.level client in
  let lag = dal_parameters.attestation_lag in
  Log.info "We are at level %d and the lag is %d" published_level lag ;
  (* If one wants to see all the node events, they should use [Node.log_events node] *)
  (* We bake [lag] blocks, one which will include the publication of the commitment
     and [lag-1] just to wait. *)
  log_step "Let's wait for the attestation lag while baking" ;
  let* () = bake_for ~count:(lag - 1) client in
  log_step
    "Crafting attestation for [bootstrap3] (with expected DAL attestation)." ;
  let node_endpoint = Node.as_rpc_endpoint node in
  let* op1 =
    craft_dal_attestation_exn
      ~protocol
      ~signer:Constant.bootstrap3
      (Slots [index])
      client
      dal_parameters
      node_endpoint
  in
  let* (`OpHash oph1) = Operation.hash op1 client in
  let op1_promise = wait_for_classified oph1 node in
  log_step "Crafting attestation for [bootstrap2] (with empty DAL attestation)." ;
  let* op2 =
    craft_dal_attestation_exn
      ~protocol
      ~signer:Constant.bootstrap2
      (Slots [])
      client
      dal_parameters
      node_endpoint
  in
  let* (`OpHash oph2) = Operation.hash op2 client in
  let op2_promise = wait_for_classified oph2 node in
  log_step "Injecting the attestations" ;
  let* _ = Operation.inject op1 client in
  let* _ = Operation.inject op2 client in
  log_step "Waiting for the attestations to be classified." ;
  let* () = Lwt.join [op1_promise; op2_promise] in
  let* lvl_attest = Node.get_level node in
  log_step
    "Attestations should be injected in the validated field of the mempool" ;
  let is_operation_in_validated_mempool mempool oph =
    let open JSON in
    let applied_list = as_list (mempool |-> "validated") in
    List.exists (fun e -> e |-> "hash" |> as_string = oph) applied_list
  in
  let* mempool =
    Node.RPC.(call node @@ get_chain_mempool_pending_operations ())
  in
  Check.is_true
    (List.for_all (is_operation_in_validated_mempool mempool) [oph1; oph2])
    ~error_msg:
      "After injection of the attestations, no operations should remain in the \
       mempool." ;
  log_step "Bake a block containing this attestation" ;
  let map_alias = List.map (fun x -> x.Account.alias) in
  (* Since the function [bake_for] crafts the attestation for delegates,
     we do not want it to craft a concurrent attestation for [bootstrap2].
     One which attests a DAL slot has already been injected. *)
  let* () =
    bake_for
      ~delegates:
        (`For (map_alias Constant.[bootstrap1; bootstrap4; bootstrap5]))
      client
  in
  let* current_level = Node.get_level node in
  log_step "Attestation should be included in the head block." ;
  let attested_levels = published_level --> current_level in
  let* all_slot_availabilities =
    Dal.collect_slot_availabilities node ~attested_levels
  in
  let* attestation_is_in_block =
    Dal.is_slot_attested
      ~endpoint:(Node.as_rpc_endpoint node)
      ~published_level
      ~slot_index:index
      ~to_attested_levels:(Dal.to_attested_levels ~protocol ~dal_parameters)
      all_slot_availabilities
  in
  Check.is_true
    attestation_is_in_block
    ~error_msg:"Slot from published level should be attested" ;
  log_step "Mempool should now be empty" ;
  let validated_mempool_is_empty mempool =
    let open JSON in
    let validated_list = as_list (mempool |-> "validated") in
    List.length validated_list = 0
  in
  let* mempool =
    Node.RPC.(call node @@ get_chain_mempool_pending_operations ())
  in
  Check.is_true
    (validated_mempool_is_empty mempool)
    ~error_msg:
      "After injection of the attestations, no operations should remain in the \
       mempool." ;
  log_step "Bake a few more blocks before calling RPC." ;
  let final_block_promise =
    wait_for_layer1_final_block producer_node (lvl_attest + 2)
  in
  let* () = bake_for ~count:3 client in
  let* () = final_block_promise in
  log_step "Call to the RPC of the DAL node to check that the slot is attested." ;
  let* status =
    Dal_RPC.(
      call producer_node
      (* The level the commitment is included in a block is the first one crafted
         after publication. *)
      @@ get_level_slot_status ~slot_level:published_level ~slot_index:index)
  in
  Log.info "Status is %a" Dal_RPC.pp_slot_id_status status ;
  log_step "Final checks." ;
  let* () =
    check_slot_status
      ~__LOC__
      producer_node
      ~expected_status:(Dal_RPC.Attested lag)
      ~check_attested_lag:`At_most
      ~slot_level:published_level
      ~slot_index:index
  in
  let not_attested_by_bootstrap2 =
    match Lwt.state not_attested_by_bootstrap2_promise with
    | Sleep -> false
    | Return () -> true
    | Fail exn ->
        Test.fail "Unexpected exception: '%s'" (Printexc.to_string exn)
  in
  Check.is_true
    ~error_msg:
      "We expected the slot to be protocol attested, but not by this attester \
       node."
    not_attested_by_bootstrap2 ;
  unit

(* [test_duplicate_denunciations] publishes a commitment for slot [0].  After
   baking [attestation_lag] blocks, the test then injects a DAL trapped
   attestation. Then it attempts to:

   - inject a first denunciation that is expected to succeed,

   - inject a second denunciation built using a different shard that
     is a trap as well, but using the same slot and the same l1 level,
     where each try is expected to fail. The injection temptatives are
     done:
     1. at the next block level (expecting delegate already denounced)
     2. at the next cycle (expecting delegate already denounced)
     3. at the next-next cycle (expecting outdated evidence)
*)
let test_duplicate_denunciations protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let slot_index = 0 in
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let attestation_lag = dal_parameters.Dal.Parameters.attestation_lag in
  let lags = dal_parameters.Dal.Parameters.attestation_lags in
  assert (attestation_lag <= blocks_per_cycle) ;
  let blocks_to_bake = 2 + blocks_per_cycle - attestation_lag in
  Log.info
    "Bake %d blocks so that the attestation level is in cycle 1"
    blocks_to_bake ;
  let* () = bake_for ~count:blocks_to_bake client in
  let accused = Constant.bootstrap2 in
  let* current_level = Node.get_level node in
  let* shards_with_proofs =
    let slot_size = dal_parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    Log.info "Publish commitment at level %d" current_level ;
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:attestation_lag client in
  let* current_level = Node.get_level node in
  Log.info "Injecting an attestation at level %d" current_level ;
  let availability = Slots [slot_index] in
  let signer = Constant.bootstrap2 in
  let* attestation, _op_hash =
    inject_dal_attestation_exn
      ~protocol
      ~signer
      availability
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  let* signature = Operation.sign attestation client in
  let attestation = (attestation, signature) in
  let* shard_assignments =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_dal_shards
         ~delegates:[accused.public_key_hash]
         ()
  in
  let shard_indexes =
    JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
  in
  let[@ocaml.warning "-8"] (shard_index_1 :: shard_index_2 :: _) =
    List.map JSON.as_int shard_indexes
  in
  let accusation =
    let shard1, proof1 =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index_1)
        shards_with_proofs
      |> Option.get
    in
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      ~lag_index:(List.length lags - 1)
      shard1
      proof1
  in
  let accusation_dup =
    let shard2, proof2 =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index_2)
        shards_with_proofs
      |> Option.get
    in
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      ~lag_index:(List.length lags - 1)
      shard2
      proof2
  in
  let* level_accusation = Node.get_level node in
  Log.info
    "Injecting a first accusation (level %d) expected to succeed"
    level_accusation ;
  let* (`OpHash _) = Operation.Anonymous.inject accusation client in
  let* () = bake_for client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting the second accusation at the next level (level %d), expected to \
     fail because the faulty delegate is already denounced"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation.already_dal_denounced
  in
  let* () = bake_for ~count:blocks_per_cycle client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting at the next cycle (level %d), expected to fail because the \
     delegate is already denounced for this level"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation.already_dal_denounced
  in
  let* () = bake_for ~count:blocks_per_cycle client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting at the next cycle (level %d), expected to fail because the DAL \
     denunciation is outdated"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation.outdated_dal_denunciation
  in
  unit

(* [test_denunciation_next_cycle] injects two trapped attestations and
   inject an accusation for the first one at cycle `c` and another at
   for the second one at cycle `c + 1`, both injections are expected
   to succeed. *)
let test_denunciation_next_cycle protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let slot_index = 0 in
  let lags = dal_parameters.Dal.Parameters.attestation_lags in
  let accused = Constant.bootstrap2 in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* () = bake_for ~count:2 client in
  let* shards_with_proofs1 =
    let* current_level = Node.get_level node in
    Log.info "Publish first commitment at level %d" current_level ;
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:1 client in
  let* shards_with_proofs2 =
    let* current_level = Node.get_level node in
    Log.info "Publish second commitment at level %d" current_level ;
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:(dal_parameters.attestation_lag - 1) client in
  let node_endpoint = Node.as_rpc_endpoint node in
  let* attestation =
    let* current_level = Node.get_level node in
    Log.info "Injecting a first attestation at level %d" current_level ;
    let availability = Slots [slot_index] in
    let* attestation, _op_hash =
      inject_dal_attestation_exn
        ~protocol
        ~signer:Constant.bootstrap2
        availability
        client
        dal_parameters
        node_endpoint
    in
    let* signature = Operation.sign attestation client in
    return (attestation, signature)
  in
  let* () =
    let* shard_assignments =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards
           ~delegates:[accused.public_key_hash]
           ()
    in
    let shard_indexes =
      JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
    in
    let[@ocaml.warning "-8"] (shard_index :: _) =
      List.map JSON.as_int shard_indexes
    in
    let shard, proof =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards_with_proofs1
      |> Option.get
    in
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol
        ~attestation
        ~slot_index
        ~lag_index:(List.length lags - 1)
        shard
        proof
    in
    let* level_accusation = Node.get_level node in
    Log.info
      "Injecting a first accusation (level %d) expected to succeed"
      level_accusation ;
    let* (`OpHash _) = Operation.Anonymous.inject accusation client in
    unit
  in
  let* () = bake_for ~count:1 client in
  let* attestation =
    let* current_level = Node.get_level node in
    Log.info "Injecting a second attestation at level %d" current_level ;
    let availability = Slots [slot_index] in
    let signer = Constant.bootstrap2 in
    let* attestation, _op_hash =
      inject_dal_attestation_exn
        ~protocol
        ~signer
        availability
        client
        dal_parameters
        node_endpoint
    in
    let* signature = Operation.sign attestation client in
    return (attestation, signature)
  in
  let* () =
    let* shard_assignments =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards
           ~delegates:[accused.public_key_hash]
           ()
    in
    let shard_indexes =
      JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
    in
    let[@ocaml.warning "-8"] (shard_index :: _) =
      List.map JSON.as_int shard_indexes
    in
    let shard, proof =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards_with_proofs2
      |> Option.get
    in
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol
        ~attestation
        ~slot_index
        ~lag_index:(List.length lags - 1)
        shard
        proof
    in
    let* () = bake_for ~count:blocks_per_cycle client in
    let* level = Node.get_level node in
    Log.info
      "Injecting the second accusation (level %d) expected to succeed"
      level ;
    let* (`OpHash _) = Operation.Anonymous.inject accusation client in
    unit
  in
  unit

(**  [test_e2e_trap_faulty_dal_node] verifies a scenario where a
     [faulty_delegate] misbehaves. Specifically, it:

     - Creates a DAL node [proxy] whose role is to mimick the honest
       [dal_node] by forwarding to the honest DAL node each RPC call,
       except for the attestable-slots endpoints for the [faulty_delegate]:
         - the streamed RPC [/profiles/<tz>/monitor/attestable_slots], where
           the proxy rewrites the JSON stream so that, at
           [<n> = 2 * blocks_per_cycle + 1] (the target attested level),
           the delegate appears to have all slots attestable (by adding
           the corresponding slot_ids in the backfill and/or live events);
         - the legacy per-level RPC [/profiles/<tz>/attested_levels/<n>/attestable_slots],
           which is also overridden for compatibility by flipping the returned
           ['attestable_slots_set'] array to all ['true'].

     The test proceeds as follows:

     1. Bakes [blocks_per_cycle] blocks to avoid the period in which
        the DAL node is not able to inject an accusation because of the
        accusation delay introduced by the migration.

     2. Bakes [blocks_per_cycle] blocks while publishing on slot index
        [0] and checks that [faulty_delegate] is not denounced at the end.

     3. Bakes again [blocks_per_cycle] blocks to finally reach [3 *
        blocks_per_cycle] blocks.

     - Finally, the test

        - a) retrieves the balance updates for the last block of
             the third cycle and verifies that:
             - Multiple DAL attesting rewards were minted.
             - The delegates that lost the DAL rewards are the [faulty_delegate]
               and the ones that lost the consensus attesting rewards because they
               haven't revealed nonces.

        - b) retrieves the [faulty_delegate] DAL participation at the
             end of the third cycle, minus one block (i.e "head~1")
             because DAL participation are reset at the end of a block
             preceding a new cycle, and checks that:

             - it has a sufficient DAL participation to get rewards,
             - it is denounced,
             - its expected DAL rewards are [0].
*)
let test_e2e_trap_faulty_dal_node protocol dal_parameters _cryptobox node client
    dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let target_attested_level = (2 * blocks_per_cycle) + 1 in
  let faulty_delegate = Constant.bootstrap1.Account.public_key_hash in
  let proxy =
    Dal_node.Proxy.make
      ~name:"proxy-dal-node"
      ~attestation_lag:dal_parameters.Dal.Parameters.attestation_lag
      ~number_of_slots:dal_parameters.Dal.Parameters.number_of_slots
      ~faulty_delegate
      ~target_attested_level
  in
  let faulty_dal_node =
    Dal_node.create
      ~name:"faulty-dal-node"
      ~listen_addr:(Dal_node.listen_addr dal_node)
      ~node
      ()
  in
  let () =
    Dal_node.Proxy.run ~honest_dal_node:dal_node ~faulty_dal_node proxy
  in
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint faulty_dal_node |> Endpoint.as_string
  in
  let* level =
    let* first_level = Node.get_level node in
    let block = string_of_int first_level in
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ~block ()
  in
  let* () =
    bake_for
      ~count:(blocks_per_cycle - 1 - level.cycle_position)
      ~dal_node_endpoint
      client
  in
  let* _ = Node.wait_for_level node blocks_per_cycle in
  let* () =
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    repeat blocks_per_cycle (fun () ->
        let wait_slot ~published_level ~slot_index:_ =
          wait_for_layer1_final_block dal_node (published_level - 2)
        in
        let* _res =
          publish_store_and_wait_slot
            node
            client
            dal_node
            Constant.bootstrap2
            ~index:0
            ~wait_slot
            ~number_of_extra_blocks_to_bake:0
          @@ Helpers.make_slot ~slot_size "content"
        in
        unit)
  in
  let level = ref (2 * blocks_per_cycle) in
  let* _ = Node.wait_for_level node !level in
  let* faulty_delegate_dal_participation =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         faulty_delegate
  in
  Check.is_false
    faulty_delegate_dal_participation.denounced
    ~__LOC__
    ~error_msg:"Expected the faulty delegate to not be denounced" ;
  let wait_for_trap_injection =
    Dal_node.wait_for dal_node "dal_trap_injection.v0" (fun e ->
        let open JSON in
        let delegate = e |-> "delegate" |> as_string in
        let attested_level = e |-> "attested_level" |> as_int in
        if delegate = faulty_delegate && attested_level = target_attested_level
        then Some ()
        else None)
  in
  let* () =
    repeat blocks_per_cycle (fun () ->
        let wait = wait_for_layer1_final_block dal_node (!level - 1) in
        let* () = bake_for ~dal_node_endpoint client in
        incr level ;
        let* () = wait in
        unit)
  in
  let* () = wait_for_trap_injection in
  let* _ = Node.wait_for_level node (3 * blocks_per_cycle) in
  let* metadata = Node.RPC.call node @@ RPC.get_chain_block_metadata_raw () in
  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  let dal_rewards =
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "minted"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "DAL attesting rewards")
      balance_updates
  in
  Check.(List.length dal_rewards > 1)
    ~__LOC__
    Check.int
    ~error_msg:"Expected %R minted DAL-related balance updates, got %L" ;
  let lost_dal_rewards =
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "burned"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "lost DAL attesting rewards")
      balance_updates
  in
  let lost_consensus_rewards =
    (* filter those that lost consensus attesting rewards because they haven't
       revealed their nonces *)
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "burned"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "lost attesting rewards"
        && (not JSON.(json |-> "participation" |> as_bool))
        && JSON.(json |-> "revelation" |> as_bool))
      balance_updates
  in
  let get_delegates =
    List.map (fun json -> JSON.(json |-> "delegate" |> as_string))
  in
  let sort_unique = List.sort_uniq String.compare in
  let losing_delegates = sort_unique @@ get_delegates lost_dal_rewards in
  let expected_to_lose_delegates =
    if Protocol.number protocol >= 023 then
      sort_unique (faulty_delegate :: get_delegates lost_consensus_rewards)
    else [faulty_delegate]
  in
  Check.(expected_to_lose_delegates = losing_delegates)
    ~__LOC__
    Check.(list string)
    ~error_msg:"Unexpected delegates to lose DAL rewards (got %R expected %L)" ;
  let* faulty_delegate_dal_participation =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         faulty_delegate
  in
  Check.is_true
    faulty_delegate_dal_participation.denounced
    ~__LOC__
    ~error_msg:"Expected the faulty delegate to be denounced" ;
  Check.is_true
    faulty_delegate_dal_participation.sufficient_dal_participation
    ~__LOC__
    ~error_msg:"Expected sufficiant participation for the faulty delegate" ;
  Check.(
    Tez.to_mutez faulty_delegate_dal_participation.expected_dal_rewards = 0)
    ~__LOC__
    Check.int
    ~error_msg:
      "Expected DAL rewards for for the faulty delegate expected to be %R, but \
       got %L" ;
  unit

let create_account ?(source = Constant.bootstrap2) ~amount ~alias client =
  Log.info
    "Create a [%s] account: generate a key, inject a transaction that funds \
     it, and bake a block to apply the transaction."
    alias ;
  let* fresh_account = Client.gen_and_show_keys ~alias client in
  let* _oph =
    Operation.Manager.inject_single_transfer
      client
      ~source
      ~dest:fresh_account
      ~amount
  in
  let* () = bake_for client in
  return fresh_account

let create_account_and_reveal ?source ~amount ~alias client =
  let* fresh_account = create_account ?source ~amount ~alias client in
  Log.info "Reveal pkh of [%s] account." alias ;
  let op_reveal =
    Operation.Manager.(make ~source:fresh_account (reveal fresh_account ()))
  in
  let* _oph = Operation.Manager.inject [op_reveal] client in
  let* () = bake_for client in
  return fresh_account

let create_low_stake node dal_parameters proto_params blocks_per_cycle =
  let minimal_stake = JSON.(proto_params |-> "minimal_stake" |> as_int) in
  let number_of_shards =
    dal_parameters.Dal.Parameters.cryptobox.number_of_shards
  in
  let* bootstrap_info =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_delegate Constant.bootstrap1.public_key_hash
  in
  let bootstrap_baking_power =
    JSON.(bootstrap_info |-> "baking_power" |> as_string) |> int_of_string
  in
  let total_baking_power =
    bootstrap_baking_power * Array.length Account.Bootstrap.keys
  in
  (* The amount is such that this baker has only one assigned shard per cycles
     (on average). Note that if the baker has no assigned shard, the test
     should still pass, because we just check that it gets its DAL rewards.

     Let [t] be total_baking_power, [s] be [small_baker_stake], and [1/n]
     desired shards fraction for the small baker. We want [s / (t + s) = 1 /
     n], ie [t + s = n * s], ie [t = (n-1) * s] ie, [s = t / (n-1)] *)
  let desired_stake =
    total_baking_power / ((blocks_per_cycle * number_of_shards) - 1)
  in
  let small_baker_stake = max desired_stake minimal_stake in
  Log.info
    "total_baking_power = %d, small_baker_stake = %d"
    total_baking_power
    small_baker_stake ;
  return small_baker_stake

(** [test_dal_rewards_distribution _protocol dal_parameters cryptobox node
    client _dal_node] verifies the correct distribution of DAL rewards among
    delegates based on their participation in DAL attestations activity.

    The test uses the 5 bootstrap accounts and a new account with a small stake
    that has on average one assigned shard per level.

    The main steps of the test are:

    1. Initialize delegates: we assign six accounts to specific roles:
    - **Baker:** Always attests both TenderBake (TB) and all DAL slots.
    - **Attesting DAL Slot 10:** Always attests TB and specifically DAL slot 10.
    - **Not Attesting at All:** Does not attest either TB or DAL slots.
    - **Not Attesting DAL:** Does not attest DAL slots, either by not sending
    any DAL attestation or by sending an empty bitset.
    - **Not Sufficiently Attesting DAL Slot 10:** Attests DAL slot 10 only 25%
    of the time.
    - **Small Baker:** It has very few assigned shards, but does the same as the Baker.

    2. Initial balances snapshot: we capture the initial balances of all
    delegates to compare against post-test balances.

    3. Publish DAL slots and inject DAL attestations: we define a helper
    function to inject DAL attestations based on each delegate's role:
    - we publish a dummy DAL slot at index 10.
    - we inject attestations from each delegate according to their assigned
    behavior.

    4. Blocks production until end of cycle: we bake blocks up to the last
    block of the current cycle, ensuring DAL slot publications and attestations
    are appropriately injected. Each time a new block is produced, we check the
    value of the "dal_attestation" bitset in the block's metadata and count the
    number of times the DAL slot at index 10 is attested.

    5. Second balances snapshot: this is done before TB and DAL rewards are
    distributed at the next block, which marks the end of the current cycle.

    6. Bake the final block of the cycle to trigger the distribution of DAL and
    TB rewards.

    7. Final snapshot of delegates' balances to assess the impact of rewards
    distribution.

    8. Balance Checks:
    - Ensure that delegates who did not attest at all (for both TB & DAL) have
    unchanged balances.
    - Verify that delegates with sufficient DAL participation received the
    expected DAL and TB rewards.
    - Confirm that delegates with insufficient or no DAL participation received
    only the expected TB rewards.

    9. Some extra checks related to /dal_participation RPC:
    - DAL participation status of each delegate matches the expected outcome
    based on their attestation behavior.
    - No delegates are denounced (there is no accuser running actually).
    - The attestable slots field of /dal_participation's result is equal to the
    number of attested slots counted in blocks metadata. *)
let test_dal_rewards_distribution protocol dal_parameters cryptobox node client
    _dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  assert (blocks_per_cycle >= dal_parameters.Dal.Parameters.attestation_lag) ;
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let all_slots = List.init number_of_slots Fun.id in

  (* Compute the stake of the small baker. *)
  let* small_baker_stake =
    create_low_stake node dal_parameters proto_params blocks_per_cycle
  in

  (* Each of the 5 bootstrap accounts contributes equally to the new baker's
     account. We give it a bit more tez, for it to be able to pay the fees for
     the pk reveal and stake operations. *)
  let to_transfer = (small_baker_stake + 3_000) / 5 in

  let* level = Node.get_level node in
  assert (level < blocks_per_cycle) ;
  let level = ref level in
  let* small_baker =
    create_account_and_reveal ~amount:to_transfer ~alias:"small_baker" client
  in
  incr level ;

  let* () =
    Lwt_list.iter_s
      (fun bootstrap ->
        if bootstrap <> Constant.bootstrap2 then
          let* _oph =
            Operation.Manager.inject_single_transfer
              client
              ~source:bootstrap
              ~dest:small_baker
              ~amount:to_transfer
          in
          unit
        else unit)
      (Array.to_list Account.Bootstrap.keys)
  in
  let* () = bake_for client in
  incr level ;

  Log.info "Register small_baker as a delegate" ;
  let* _small_baker =
    Client.register_delegate ~delegate:small_baker.alias client
  in
  let* () = bake_for client in
  incr level ;

  Log.info "Stake for small_baker" ;
  let* () =
    Client.stake
      (Tez.of_mutez_int small_baker_stake)
      ~staker:small_baker.public_key_hash
      client
  in

  Log.info
    "Bake (almost) %d cycles to activate the delegate"
    consensus_rights_delay ;
  let* () =
    bake_for
      ~count:((blocks_per_cycle * (1 + consensus_rights_delay)) - !level - 1)
      client
  in
  let* current_level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  assert (current_level.cycle_position = blocks_per_cycle - 1) ;
  level := blocks_per_cycle * (1 + consensus_rights_delay) ;
  assert (!level = current_level.level) ;

  (* We get our available delegates and assign them different roles they'll play
     during the test. *)
  let ( accounts_list,
        ( baker,
          attesting_dal_slot_10,
          not_attesting_at_all,
          not_attesting_dal,
          not_sufficiently_attesting_dal_slot_10,
          small_baker ) ) =
    match Account.Bootstrap.keys with
    | [|d1; d2; d3; d4; d5|] ->
        ( [
            (d1, "baker");
            (d2, "attesting_dal_slot_10");
            (d3, "not_attesting_at_all");
            (d4, "not_attesting_dal");
            (d5, "not_sufficiently_attesting_dal_slot_10");
            (small_baker, "small_baker");
          ],
          (d1, d2, d3, d4, d5, small_baker) )
    | _ -> Test.fail "Expected exactly 5 bootstrap accounts."
  in

  (* A helper function to get balances of the delegates above. *)
  let snapshot_full_balances_helper () =
    let bal account =
      let* b =
        Client.get_full_balance_for client ~account:account.Account.alias
      in
      return (Tez.mutez_int64 b)
    in
    let* l =
      Lwt.all
        [
          bal baker;
          bal attesting_dal_slot_10;
          bal not_attesting_at_all;
          bal not_attesting_dal;
          bal not_sufficiently_attesting_dal_slot_10;
          bal small_baker;
        ]
    in
    match l with
    | [b1; b2; b3; b4; b5; b6] -> return (b1, b2, b3, b4, b5, b6)
    | _ -> Test.fail "Not reachable."
  in

  Log.info "Snapshot the balances of the accounts at startup." ;
  let* ( _baker_bal0,
         attesting_dal_slot_10_bal0,
         not_attesting_at_all_bal0,
         not_attesting_dal_bal0,
         not_sufficiently_attesting_dal_slot_10_bal0,
         small_baker_bal0 ) =
    snapshot_full_balances_helper ()
  in

  let node_endpoint = Node.as_rpc_endpoint node in

  (* This is the main helper function which injects (DAL) attestations for
     delegates depending on their profiles. *)
  let inject_attestations () =
    let count_dal_attesting_bakers = ref 0 in
    (* 1. Baker always attests TB and all DAL slots *)
    let* (_ : Operation.t * [`OpHash of peer_id]) =
      (* The baker delegate will miss 1/10 of its DAL attestations and will send
         [No_dal_attestation] in this case. *)
      let baker_attestation =
        if !level mod 10 = 0 then No_dal_attestation
        else (
          incr count_dal_attesting_bakers ;
          Slots all_slots)
      in
      inject_dal_attestation_exn
        ~protocol
        ~signer:baker
        baker_attestation
        client
        dal_parameters
        node_endpoint
    in
    (* 2. attesting_dal_slot_10 always attests TB and DAL slot 10 *)
    let* (_ : Operation.t * [`OpHash of peer_id]) =
      (* The attesting_dal_slot_10 delegate misses 1/11th of its DAL
         attestations and will send Slots [], but it should be fine for its
         rewards. *)
      let attestation =
        if !level mod 11 = 0 then Slots []
        else (
          incr count_dal_attesting_bakers ;
          Slots [10])
      in
      inject_dal_attestation_exn
        ~protocol
        ~signer:attesting_dal_slot_10
        attestation
        client
        dal_parameters
        node_endpoint
    in
    (* 3. not_attesting_at_all is not attesting neither TB nor DAL slots *)
    (* 4. not_attesting_dal either sends no DAL content or sends bitset 0 *)
    let* (_ : Operation.t * [`OpHash of peer_id]) =
      let dal_attestation =
        if !level mod 2 = 0 then No_dal_attestation else Slots []
      in
      inject_dal_attestation_exn
        ~protocol
        ~signer:not_attesting_dal
        dal_attestation
        client
        dal_parameters
        node_endpoint
    in
    (* 5. not_sufficiently_attesting_dal_slot_10: is attesting DAL slot 10, but only 25%
       of the time. *)
    let* (_ : Operation.t * [`OpHash of peer_id]) =
      let slots_to_attest =
        if !level mod 4 = 0 then (
          incr count_dal_attesting_bakers ;
          Slots [10])
        else No_dal_attestation
      in
      inject_dal_attestation_exn
        ~protocol
        ~signer:not_sufficiently_attesting_dal_slot_10
        slots_to_attest
        client
        dal_parameters
        node_endpoint
    in
    (* 6. small_baker: is always attesting DAL slot 10. *)
    let* () =
      let slots_to_attest = Slots [10] in
      let* res =
        inject_dal_attestation
          ~protocol
          ~signer:small_baker
          slots_to_attest
          client
          dal_parameters
          node_endpoint
      in
      (match res with
      | None ->
          Log.info
            "At level %d, %s could not TB attest"
            !level
            small_baker.alias
      | Some _ -> incr count_dal_attesting_bakers) ;
      unit
    in
    Log.info
      "At level %d, there are %d bakers that DAL attested"
      !level
      !count_dal_attesting_bakers ;
    unit
  in

  (* We'll count the number of times we see that slot 10 is attested in blocks
     metadata. We'll then check the final value against the
     delegate_attestable_dal_slots field of /dal_participation for each
     delegate. *)
  let count_set_dal_attestation_bitset = ref 0 in

  let count_slot_10_if_attested () =
    let* block_json = Node.RPC.(call node @@ get_chain_block_metadata_raw ()) in
    let block_dal_attestation_bitset =
      JSON.(block_json |-> "dal_attestation" |> as_int)
    in
    Log.info
      "At level %d, dal_attestation = %d"
      !level
      block_dal_attestation_bitset ;
    (* count when slot 10 is attested *)
    let expected_bitset =
      if Protocol.number protocol <= 024 then 1024
      else
        (* For the compact multi-lag encoding with 1 lag and slot 10:
           - prefix bit 0 = 1
           - chunks of 8 bits (is_last + 7 slots), slot 10 is in chunk 1
           - is_last bit for chunk 1 at position 1 + 8 = 9
           - slot 10 bit at position 1 + 8 + (1 + (10 mod 7)) = 13
           Result: 2^0 + 2^9 + 2^13 = 1 + 512 + 8192 = 8705 *)
        8705
    in
    if block_dal_attestation_bitset = expected_bitset then
      incr count_set_dal_attestation_bitset ;
    unit
  in

  (* This is the main entry of the test: we start by baking a number of blocks
     in which we inject DAL slots publications at slot index 10 and TB/DAL
     attestations. We stop before baking the last block of the current cycle,
     where DAL rewards are distributed. *)
  Log.info
    "Bake and publish for one cycle minus one block (%d blocks)"
    (blocks_per_cycle - 1) ;
  let* () =
    repeat (blocks_per_cycle - 1) (fun () ->
        let* () = count_slot_10_if_attested () in
        let* (`OpHash _oph1) =
          publish_dummy_slot
            ~source:baker
            ~index:10
            ~message:"hi!"
            cryptobox
            client
        in
        let* () = inject_attestations () in
        let* () =
          (* Do not use [bake_for] because it also injects an attestation
             operation for [baker]. *)
          Client.propose_for_and_wait
            ~key:[baker.Account.public_key_hash]
            client
        in
        incr level ;
        let* manager_ops =
          Node.RPC.call node
          @@ RPC.get_chain_block_operations_validation_pass
               ~validation_pass:3
               ()
        in
        Check.(
          (List.length @@ JSON.as_list manager_ops = 1)
            int
            ~__LOC__
            ~error_msg:"expected 1 manager operation in block, got %L") ;
        let* attestations =
          Node.RPC.call node
          @@ RPC.get_chain_block_operations_validation_pass
               ~validation_pass:0
               ()
        in
        let num_attestations = List.length @@ JSON.as_list attestations in
        (* The small baker may not have rights to inject its attestation *)
        let check_num = num_attestations = 4 || num_attestations = 5 in
        Check.is_true
          check_num
          ~__LOC__
          ~error_msg:"expected 4 or 5 attestations in block" ;
        unit)
  in

  Log.info
    "After this first round of blocks, we snapshot the balances of our \
     delegates again." ;
  let* ( _baker_bal1,
         attesting_dal_slot_10_bal1,
         not_attesting_at_all_bal1,
         not_attesting_dal_bal1,
         not_sufficiently_attesting_dal_slot_10_bal1,
         small_baker_bal1 ) =
    snapshot_full_balances_helper ()
  in

  let* () = count_slot_10_if_attested () in

  let* bootstrap_accounts_participation =
    Lwt_list.map_s
      (fun (account, _account_role) ->
        let* dal_participation =
          Node.RPC.call node
          @@ RPC.get_chain_block_context_delegate_dal_participation
               account.Account.public_key_hash
        in
        return (account, dal_participation))
      accounts_list
  in

  (* We use the 'participation' RPC to get the expected Tenderbake rewards of delegate
     who TB-attested sufficiently. *)
  let snapshot_tb_participation () =
    let participation account =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_delegate_participation
           account.Account.public_key_hash
    in
    let* l =
      Lwt_list.map_s
        participation
        [
          baker;
          attesting_dal_slot_10;
          not_attesting_at_all;
          not_attesting_dal;
          not_sufficiently_attesting_dal_slot_10;
          small_baker;
        ]
    in
    match l with
    | [p1; p2; p3; p4; p5; p6] -> return (p1, p2, p3, p4, p5, p6)
    | _ -> Test.fail "Not reachable."
  in
  let* ( _baker_tb_participation,
         attesting_dal_slot_10_tb_participation,
         _not_attesting_at_all_tb_participation,
         not_attesting_dal_tb_participation,
         not_sufficiently_attesting_dal_slot_10_tb_participation,
         small_baker_tb_participation ) =
    snapshot_tb_participation ()
  in

  Log.info
    "We now bake the last block of the cycle, which should trigger (TB and) \
     DAL rewards distribution." ;
  (* TB rewards are actually set to 0. *)
  let* () = bake_for ~delegates:(`For [baker.Account.public_key_hash]) client in
  let* metadata = Node.RPC.(call node @@ get_chain_block_metadata_raw ()) in
  incr level ;
  let* current_level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  assert (current_level.cycle_position = blocks_per_cycle - 1) ;

  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  let expected_to_lose_attesting_rewards =
    if Protocol.number protocol >= 023 then
      (* keep those that lost consensus attesting rewards because they haven't
         revealed their nonces *)
      List.filter_map
        (fun json ->
          let check json =
            JSON.(json |-> "kind" |> as_string) |> String.equal "burned"
            && JSON.(json |-> "category" |> as_string)
               |> String.equal "lost attesting rewards"
            && (not JSON.(json |-> "participation" |> as_bool))
            && JSON.(json |-> "revelation" |> as_bool)
          in
          if check json then Some JSON.(json |-> "delegate" |> as_string)
          else None)
        balance_updates
    else []
  in
  (* We snapshot the balances of the delegates at the end of the cycle. *)
  let* ( _baker_bal2,
         attesting_dal_slot_10_bal2,
         not_attesting_at_all_bal2,
         not_attesting_dal_bal2,
         not_sufficiently_attesting_dal_slot_10_bal2,
         small_baker_bal2 ) =
    snapshot_full_balances_helper ()
  in

  (* We're now ready to do some checks, both on balances and on
     dal_participation RPC's result. *)
  (* First, we define a function to check expected balances of delegates. *)
  let check_bal_incr ~__LOC__ account bal_before bal_after ~delta =
    let error_msg =
      if delta = 0 then
        "account " ^ account.Account.public_key_hash
        ^ ", expected balance to be unchanged. Got %R, expecting %L"
      else
        "account " ^ account.Account.public_key_hash
        ^ ", unexpected balance: got %R, expecting %L = "
        ^ Int64.to_string bal_before ^ " + " ^ string_of_int delta
    in
    Check.(Int64.(add bal_before (of_int delta)) = bal_after)
      ~__LOC__
      Check.int64
      ~error_msg
  in

  (* Except for the baker, we don't expect the balances of the delegates to
     change before baking the last block of the current cycle. *)
  List.iter
    (fun (account, bal0, bal1) ->
      check_bal_incr ~__LOC__ account bal0 bal1 ~delta:0)
    [
      ( attesting_dal_slot_10,
        attesting_dal_slot_10_bal0,
        attesting_dal_slot_10_bal1 );
      ( not_attesting_at_all,
        not_attesting_at_all_bal0,
        not_attesting_at_all_bal1 );
      (not_attesting_dal, not_attesting_dal_bal0, not_attesting_dal_bal1);
      ( not_sufficiently_attesting_dal_slot_10,
        not_sufficiently_attesting_dal_slot_10_bal0,
        not_sufficiently_attesting_dal_slot_10_bal1 );
      (small_baker, small_baker_bal0, small_baker_bal1);
    ] ;

  (* As all delegates except [small_baker] have the same stake distribution,
     they're expected to have the same number of the assigned shards, to have
     shards at the same level, to have the same rewards allocated, ... *)
  let () =
    match List.rev bootstrap_accounts_participation with
    | (_small_baker, _) :: (_account, dal_part) :: rest ->
        List.iter
          (fun (_account, dal_participation) ->
            (* We transferred funds from [bootstrap2] to [small_baker], so
               [bootstrap_2] has a smaller baking power than the other bootstrap
               delegates. *)
            Check.(
              dal_part.expected_assigned_shards_per_slot
              = dal_participation.RPC.expected_assigned_shards_per_slot)
              ~__LOC__
              Check.int
              ~error_msg:"expected_assigned_shards_per_slot mismatch" ;
            Check.(
              Tez.to_mutez dal_part.expected_dal_rewards
              = Tez.to_mutez dal_participation.expected_dal_rewards)
              ~__LOC__
              Check.int
              ~error_msg:"expected_dal_rewards mismatch" ;
            Check.(
              dal_part.delegate_attestable_dal_slots
              = dal_participation.delegate_attestable_dal_slots)
              ~__LOC__
              Check.int
              ~error_msg:"delegate_attestable_dal_slots mismatch")
          rest
    | _ -> Test.fail "Not reachable."
  in

  (* After baking the last block of the cycle, we check that:
     - the balances of the delegates who didn't attest at all didn't change.
     - the participation RPC's result is aligned with the first check. *)
  List.iter
    (fun (account, bal1, bal2) ->
      check_bal_incr ~__LOC__ account bal1 bal2 ~delta:0 ;
      let dal_participation =
        List.assoc account bootstrap_accounts_participation
      in
      Check.is_false
        dal_participation.sufficient_dal_participation
        ~__LOC__
        ~error_msg:
          ("account " ^ account.Account.public_key_hash
         ^ ", expected to have insufficient DAL participation."))
    [
      ( not_attesting_at_all,
        not_attesting_at_all_bal1,
        not_attesting_at_all_bal2 );
    ] ;

  (* After baking the last block of the cycle, we check that:
     - the balances of the delegates who didn't attest DAL sufficiently or
       didn't attest DAL at all only increased by a delta equal to the expected
       Tenderbake attestation rewards.
     - the participation RPC's result is aligned with the first check. *)
  List.iter
    (fun (account, bal1, bal2, tb_participation, sufficient_dal_participation)
       ->
      let dal_participation =
        List.assoc account bootstrap_accounts_participation
      in
      Check.(
        (dal_participation.sufficient_dal_participation
       = sufficient_dal_participation)
          ~__LOC__
          bool
          ~error_msg:
            ("account " ^ account.Account.public_key_hash
           ^ ", expected to have sufficient DAL participation? %R, but got %L")) ;
      let expecting_attesting_rewards =
        not
        @@ List.mem account.public_key_hash expected_to_lose_attesting_rewards
      in
      let expected_attesting_rewards =
        if expecting_attesting_rewards then
          Tez.to_mutez tb_participation.RPC.expected_attesting_rewards
        else 0
      in
      let expected_dal_rewards =
        if sufficient_dal_participation && expecting_attesting_rewards then
          Tez.to_mutez dal_participation.expected_dal_rewards
        else 0
      in
      let delta = expected_attesting_rewards + expected_dal_rewards in
      Log.info
        "[check] %s %s: %Ld = %Ld + %d + %d"
        account.Account.alias
        account.public_key_hash
        bal1
        bal2
        expected_attesting_rewards
        expected_dal_rewards ;
      check_bal_incr ~__LOC__ account bal1 bal2 ~delta)
    [
      ( not_attesting_dal,
        not_attesting_dal_bal1,
        not_attesting_dal_bal2,
        not_attesting_dal_tb_participation,
        false );
      ( not_sufficiently_attesting_dal_slot_10,
        not_sufficiently_attesting_dal_slot_10_bal1,
        not_sufficiently_attesting_dal_slot_10_bal2,
        not_sufficiently_attesting_dal_slot_10_tb_participation,
        false );
      ( attesting_dal_slot_10,
        attesting_dal_slot_10_bal1,
        attesting_dal_slot_10_bal2,
        attesting_dal_slot_10_tb_participation,
        true );
      ( small_baker,
        small_baker_bal1,
        small_baker_bal2,
        small_baker_tb_participation,
        true );
    ] ;

  (* As a final check, we verify that no delegate is denounced and that we
     report the correct number of attested slots. *)
  List.iter
    (fun (account, dal_participation) ->
      Check.is_false
        dal_participation.RPC.denounced
        ~__LOC__
        ~error_msg:
          ("account " ^ account.Account.public_key_hash
         ^ ", not expected to be denounced.") ;

      (* The number of attestable slots for the small stake baker is smaller
         than for the other bakers and since we did not count them, we don't
         check them. *)
      if not @@ String.equal account.alias small_baker.alias then
        let msg_prefix = sf "For delegate %s: " account.public_key_hash in
        Check.(
          dal_participation.delegate_attestable_dal_slots
          = !count_set_dal_attestation_bitset)
          ~__LOC__
          Check.int
          ~error_msg:
            (msg_prefix
           ^ "expecting %L attestable DAL slots, but %R were reported in \
              blocks metadata"))
    bootstrap_accounts_participation ;
  unit

let use_mockup_node_for_getting_attestable_slots protocol dal_parameters
    cryptobox l1_node client _bootstrap_key =
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let attestation_lag = dal_parameters.attestation_lag in

  Log.info "Start the mocked DAL node" ;
  let dal_node_mockup =
    let attesters =
      Account.Bootstrap.keys |> Array.to_list
      |> List.map (fun b -> b.Account.public_key_hash)
    in
    let attestable_slots ~attester:_ ~attested_level:_ =
      List.init number_of_slots (fun _0 -> true)
    in
    Dal_node.Mockup_for_baker.make
      ~name:"mock-dal-node"
      ~attestation_lag
      ~attesters
      ~attestable_slots
  in
  let port = Port.fresh () in
  let () = Dal_node.Mockup_for_baker.run dal_node_mockup ~port in
  let dal_node_rpc_endpoint =
    Endpoint.make ~host:"localhost" ~scheme:"http" ~port ()
  in
  let baker = Agnostic_baker.create ~dal_node_rpc_endpoint l1_node client in

  Log.info "Publish a slot" ;
  let* (`OpHash _op_hash) =
    publish_dummy_slot
      ~source:Constant.bootstrap1
      ~index:0
      ~message:"a"
      cryptobox
      client
  in
  (* Bake one block manually to include the slot operation, so that
     published_level is deterministic. Without this, the baker might
     bake the next block before the operation reaches the validated
     mempool, pushing it to a later level. *)
  let* () = bake_for client in
  let* published_level = Node.get_level l1_node in

  Log.info "Start the baker" ;
  let* () = Agnostic_baker.run baker in

  (* +2 blocks for the attested block to be final, +1 for some slack *)
  let* _ =
    Node.wait_for_level l1_node (published_level + attestation_lag + 3)
  in
  let* () = Agnostic_baker.terminate baker in
  let () = Dal_node.Mockup_for_baker.stop dal_node_mockup in

  let attested_level = published_level + attestation_lag in
  Log.info
    "Check that the slot published at level %d was attested at level %d"
    published_level
    attested_level ;
  let* {dal_attestation; _} =
    Node.RPC.(
      call l1_node
      @@ get_chain_block_metadata ~block:(string_of_int attested_level) ())
  in
  let* dal_attestation =
    match dal_attestation with
    | None -> return None
    | Some str ->
        let* per_lag =
          Dal.Slot_availability.decode
            protocol
            (Node.as_rpc_endpoint l1_node)
            dal_parameters
            str
        in
        return
        @@ Some
             (Array.init number_of_slots (fun slot_index ->
                  Array.exists (fun lag_array -> lag_array.(slot_index)) per_lag))
  in
  let expected_attestation = expected_attestation dal_parameters [0] in
  Check.((Some expected_attestation = dal_attestation) (option (array bool)))
    ~error_msg:"Unexpected DAL attestation: expected %L, got %R" ;
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

let wait_for_branch_switch node level =
  let filter json =
    match JSON.(json |-> "level" |> as_int_opt) with
    | Some l when l = level -> Some ()
    | Some _ -> None
    | None -> None
  in
  Node.wait_for node "branch_switch.v0" filter

let baker_at_round_n ?level round client : string Lwt.t =
  let* json =
    Client.RPC.call client @@ RPC.get_chain_block_helper_baking_rights ?level ()
  in
  match JSON.(json |=> round |-> "delegate" |> as_string_opt) with
  | Some delegate_id -> return delegate_id
  | None ->
      Test.fail
        "Could not find the baker at round %d for level %s"
        round
        (match level with None -> "head" | Some level -> string_of_int level)

(* Simulate a fork at level n where two competing blocks exist:
   - n
     - [node1]: A1 (round 0, no DAL commitment)
     - [node2]: A2 (round 1, with DAL commitment)
   Then, build one more block on top of A2:
   - n+1
     - [node2]: B (on top of A2)
   After reconnecting the nodes, node1 switches to the A2 -> B branch (due to
   higher fitness).
   Then, test that shards are propagated after one level is baked on top of
   the block which included the commitment publication, at (n+1). *)
let test_dal_one_level_reorg protocol dal_parameters _cryptobox node1 client1
    dal_bootstrap =
  (* Helpers / Constants *)
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let slot_index = 0 in

  (* Spin up a second L1 node and connect it *)
  let nodes_args = Node.[Synchronisation_threshold 0; Connections 1] in
  let* node2, client2 =
    Client.init_with_protocol ~protocol ~nodes_args `Client ()
  in
  let* () =
    Client.Admin.connect_address ~endpoint:(Node node1) ~peer:node2 client1
  in

  (* DAL Bootstrap *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;
  let peers = [Dal_node.listen_addr dal_bootstrap] in

  (* DAL Producer on [node1] *)
  let producer = Dal_node.create ~name:"producer" ~node:node1 () in
  let* () =
    Dal_node.init_config ~operator_profiles:[slot_index] ~peers producer
  in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator slot_index])
  in
  Log.info "Slot producer DAL node is running" ;

  (* DAL Attester on [node1] *)
  let* proto_params =
    Node.RPC.call node1 @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* balance =
    Client.get_balance_for ~account:Constant.bootstrap1.alias client1
  in
  let amount = Tez.(balance - one) in
  let* new_account = Client.gen_and_show_keys client1 in
  let* () =
    Client.transfer
      ~giver:Constant.bootstrap1.alias
      ~receiver:new_account.alias
      ~amount
      ~burn_cap:Tez.one
      client1
  in
  let* () = bake_for client1 in
  let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client1 in
  let* () = bake_for client1 in
  let* () = Client.register_key new_account.alias client1 in
  let* () = bake_for client1 in
  let* () = Client.stake ~staker:new_account.alias Tez.(amount /! 2L) client1 in
  let attester = Dal_node.create ~name:"attester" ~node:node1 () in
  let* () =
    Dal_node.init_config
      ~attester_profiles:[new_account.public_key_hash]
      ~peers
      attester
  in
  let* () = Dal_node.run ~event_level:`Debug attester in
  let client1 = Client.with_dal_node client1 ~dal_node:attester in

  (* Compute the DAL attestation level for publication *)
  let num_cycles = 1 + consensus_rights_delay in
  let* level = Client.level client1 in
  let lag = dal_parameters.attestation_lag in
  let attestation_level = (num_cycles * blocks_per_cycle) + 1 in
  let published_level = attestation_level + 1 - lag in
  Log.info "Bake blocks up to level %d" (published_level - 1) ;
  let* () = bake_for ~count:(published_level - 1 - level) client1 in
  let* current_level = Client.level client1 in
  Log.info
    "current_level = %d; published_level = %d; attestation_level = %d"
    current_level
    published_level
    attestation_level ;

  (* Align node2 to current level, then disconnect to fork *)
  let* _ = Node.wait_for_level node2 current_level in
  let* node2_id = Node.wait_for_identity node2 in
  let* () =
    Client.Admin.kick_peer ~endpoint:(Node node1) ~peer:node2_id client1
  in

  (* Branch 1 on node1: bake A1 at round 0 (lower fitness) *)
  let* baker_low_round = baker_at_round_n ~level:published_level 0 client1 in
  let* () = bake_for ~delegates:(`For [baker_low_round]) client1 in
  Log.info "node1 baked A1 at (level = %d, round = 0)" published_level ;

  (* Branch 2 on node2: bake A2 with commitment at round 1, then B *)
  let* _ =
    Helpers.publish_and_store_slot
      client2
      producer
      Constant.bootstrap2
      ~index:slot_index
    @@ Helpers.make_slot ~slot_size "REORG-SLOTDATA"
  in
  let* baker_high_round = baker_at_round_n ~level:published_level 1 client2 in
  let* () = bake_for ~delegates:(`For [baker_high_round]) client2 in
  Log.info "node1 baked A2 at (level = %d, round = 1)" published_level ;
  let* manager_ops =
    Node.RPC.call node2
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:3 ()
  in
  Check.(
    (JSON.as_list manager_ops |> List.length <> 0)
      int
      ~error_msg:
        "Expected the commitment to be published, but no manager operation was \
         included.") ;
  let* assigned_shard_indexes =
    Dal_RPC.(
      call attester
      @@ get_assigned_shard_indices
           ~level:attestation_level
           ~pkh:new_account.public_key_hash)
  in
  let wait_for_shards_promises =
    wait_for_shards_promises
      ~dal_node:attester
      ~storage_profile:`Cache_only
      ~shards:assigned_shard_indexes
      ~published_level
      ~slot_index
  in
  let* () = bake_for client2 in
  Log.info "node2 baked B at level = %d" (published_level + 1) ;

  (* Reconnect & wait for node1 to switch to node2’s higher-fitness branch *)
  let wait_switch = wait_for_branch_switch node1 published_level in
  let* () =
    Client.Admin.connect_address ~endpoint:(Node node1) ~peer:node2 client1
  in
  let* () = wait_switch in
  Log.info "node1 switched to branch with round = 1 at level %d" published_level ;

  let* _ = Node.wait_for_level node1 (published_level + 1) in
  Log.info "node1 synchronised with node2" ;
  Log.info "Waiting for attester to receive its assigned shards" ;
  let* () = wait_for_shards_promises in
  unit

(* [test_denunciation_when_all_bakers_attest] checks that a delegate who
   wrongly attests is denounced by the DAL node, and that this denunciation is
   included in a block by the protocol.

   The test consists of:
   - enabling the "all bakers attest" feature,
   - publishing commitments,
   - forcing a baker to attest to those commitments, despite the traps_fraction
     being set to 1 (so all shards are traps),
   - letting the publisher DAL node identify that those attestations are wrong
     and denounce them,
   - and checking that the denunciations are indeed included in a block by the
     protocol.

   The rationale for this test is that before the "all bakers attest" feature,
   the slot index contained in an attestation was the same as the smallest shard
   index assigned to a baker.

   This property was used in the `get_committees` function of the protocol
   plugin. Hence, before https://gitlab.com/tezos/tezos/-/merge_requests/19698,
   the DAL accuser would associate attestations with the wrong bakers when
   "all bakers attest" was enabled. This test exposes that issue.

   It should be noted that it may still happen that the slot index in an
   attestation coincides with the smallest shard index. To prevent such a
   collision from making the test pass by chance, three publications are
   attested and denounced at three consecutive slots, making the probability
   of a false positive (a test passing by luck) very low.
*)
let test_denunciation_when_all_bakers_attest protocol dal_parameters _cryptobox
    l1_node client dal_node =
  let slot_index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator slot_index])) in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let attestation_lag = dal_parameters.attestation_lag in
  (* To trigger the "all bakers attest" feature, one has to create enough tz4
     and stake with them. *)
  let* tz4_accounts =
    create_tz4_accounts_stake_and_wait
      ~funders:[Constant.bootstrap1; Constant.bootstrap2]
      ~node:l1_node
      ~client
      10
  in
  let all_delegates =
    Array.to_list Account.Bootstrap.keys
    @ List.map (fun {delegate_key; _} -> delegate_key) tz4_accounts
  in
  let delegate_wrongly_attestating = List.hd all_delegates in
  let rest_delegates_pkhs =
    List.map (fun key -> key.Account.public_key_hash) (List.tl all_delegates)
  in
  Log.info "Enough waiting, let's publish 3 commitments" ;
  let* () =
    repeat 3 (fun () ->
        let message = Helpers.make_slot ~slot_size "Hello world!" in
        let* _pid =
          Helpers.publish_and_store_slot
            client
            dal_node
            delegate_wrongly_attestating
            ~index:slot_index
            message
        in
        bake_for client)
  in
  let* () = bake_for ~count:(attestation_lag - 3) client in
  Log.info "Our byzantine baker to DAL attest" ;
  let* () =
    repeat 3 (fun () ->
        let* _attestation, _op_hash =
          inject_dal_attestation_exn
            ~protocol
            ~signer:delegate_wrongly_attestating
            (Slots [slot_index])
            client
            dal_parameters
            (Node.as_rpc_endpoint l1_node)
        in
        let* () = bake_for ~delegates:(`For rest_delegates_pkhs) client in
        let* _ops =
          Node.RPC.call l1_node @@ RPC.get_chain_block_operations ()
        in
        unit)
  in
  (* As soon as the attestations are final, the DAL node should see that they
     are denounceable and publish the denunciation. *)
  Log.info "Denunciations should start to arrive." ;
  let* () =
    repeat 3 (fun () ->
        let* () = bake_for client in
        let* _mempool =
          Client.RPC.call client @@ RPC.get_chain_mempool_pending_operations ()
        in
        let* ops =
          Node.RPC.call l1_node
          @@ RPC.get_chain_block_operations_validation_pass
               ~block:"head"
               ~validation_pass:2
               ()
        in
        Check.(List.length (JSON.as_list ops) = 1)
          ~__LOC__
          Check.(int)
          ~error_msg:"Expected exactly one anonymous op. Got: %L" ;
        unit)
  in
  unit

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

let test_dal_low_stake_attester_attestable_slots protocol dal_parameters
    _cryptobox node client dal_node =
  let {log_step} = init_logger () in
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let slot_index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in

  let* low_stake =
    create_low_stake node dal_parameters proto_params blocks_per_cycle
  in
  let* new_account = Client.gen_and_show_keys client in
  let amount = Tez.of_mutez_int (low_stake + 3_000_000) in
  let* () =
    Client.transfer
      ~giver:Constant.bootstrap1.alias
      ~receiver:new_account.alias
      ~amount
      ~burn_cap:Tez.one
      client
  in
  let* () = bake_for client in
  let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client in
  let* () = bake_for client in
  let* _ = Client.register_delegate ~delegate:new_account.alias client in
  let* () = bake_for client in
  let* () =
    Client.stake
      (Tez.of_mutez_int low_stake)
      ~staker:new_account.public_key_hash
      client
  in
  let* () = bake_for client in
  let client = Client.with_dal_node client ~dal_node in
  log_step "Attester DAL node started (operator + attester)." ;

  let* current_level = Node.get_level node in
  log_step
    "Bake (almost) %d cycles to activate the delegate"
    consensus_rights_delay ;
  let* () =
    bake_for
      ~count:
        ((blocks_per_cycle * (1 + consensus_rights_delay)) - current_level - 1)
      client
  in

  (* Find DAL attestation bitset in the last block for the delegate, if an
     [attestation_with_dal] exists. *)
  let get_delegate_dal_attestation_opt () =
    let* json =
      Node.RPC.call node
      @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
    in
    let dal_attestation_opt =
      List.find_map
        (fun json ->
          let contents = JSON.(json |-> "contents" |> as_list) |> List.hd in
          let delegate =
            JSON.(contents |-> "metadata" |-> "delegate" |> as_string)
          in
          let kind = JSON.(contents |-> "kind" |> as_string) in
          if
            String.equal delegate new_account.public_key_hash
            && String.equal kind "attestation_with_dal"
          then Some JSON.(contents |-> "dal_attestation" |> as_string)
          else None)
        (JSON.as_list json)
    in
    return dal_attestation_opt
  in

  let count_not_in_committee = ref 0 in
  let count_attestable_slots = ref 0 in
  let count_traps = ref 0 in

  let* first_level = Client.level client in

  let check_one_level () =
    (* Publish a slot, then bake a block with all delegates. *)
    let* _ =
      Helpers.publish_and_store_slot
        client
        dal_node
        Constant.bootstrap2
        ~index:slot_index
        (Helpers.make_slot ~slot_size "SLOTDATA")
    in
    let* () = bake_for client in
    let* attested_level = Client.level client in
    let published_level = attested_level - dal_parameters.attestation_lag in

    let* expected_dal_attestable_slots =
      Dal_RPC.(
        call dal_node
        @@ get_attestable_slots ~attester:new_account ~attested_level)
    in
    let* actual_dal_attestable_slots_opt =
      get_delegate_dal_attestation_opt ()
    in
    let expected_bit_opt =
      match expected_dal_attestable_slots with
      | Not_in_committee -> None
      | Attestable_slots slots ->
          Some
            (match List.nth_opt slots slot_index with
            | Some b -> b
            | None -> false)
    in
    (match expected_bit_opt with
    | None -> incr count_not_in_committee
    | Some true -> incr count_attestable_slots
    | _ -> ()) ;

    (* Check if delegate actually attested our slot when they included DAL content *)
    let* actual_bit_opt =
      match actual_dal_attestable_slots_opt with
      | None -> return None
      | Some dal_attestation ->
          let* b =
            Dal.is_slot_attested_in_bitset
              ~endpoint:(Node.as_rpc_endpoint node)
              ~protocol
              ~dal_parameters
              ~attested_level
              ~published_level
              ~slot_index
              ~dal_attestation
          in
          return (Some b)
    in

    match (expected_dal_attestable_slots, actual_bit_opt) with
    | Not_in_committee, None ->
        log_step "Level %d: Not_in_committee" attested_level ;
        unit
    | Not_in_committee, Some true ->
        Test.fail
          "Level %d: Not_in_committee but delegate attested slot %d from \
           published_level=%d"
          attested_level
          slot_index
          published_level
    | Not_in_committee, Some false ->
        log_step
          "Level %d: Not_in_committee (attestation for other lags/levels)"
          attested_level ;
        unit
    | Attestable_slots _slots, None ->
        if published_level > first_level then
          Test.fail
            "Level %d: delegate %s is in committee but no DAL attestation found"
            attested_level
            new_account.public_key_hash
        else unit
    | Attestable_slots _slots, Some actual_bit ->
        let expected_bit =
          match expected_bit_opt with Some b -> b | None -> false
        in
        if actual_bit <> expected_bit then
          Test.fail
            "Level %d: DAL node says bit(%d)=%b for published_level=%d, but \
             delegate's attestation shows bit=%b"
            attested_level
            slot_index
            expected_bit
            published_level
            actual_bit
        else
          let* has_traps =
            let* traps =
              Dal_RPC.(
                call dal_node
                @@ get_published_level_known_traps
                     ~published_level
                     ~pkh:new_account.public_key_hash
                     ~slot_index)
            in
            return @@ not @@ List.is_empty traps
          in
          if has_traps then incr count_traps ;
          if published_level <= first_level then (
            Check.(
              (actual_bit = false)
                ~__LOC__
                bool
                ~error_msg:
                  (sf "Level %d: Expected false, got true" attested_level)) ;
            unit)
          else if actual_bit <> not has_traps then
            Test.fail
              "Level %d, slot %d, published_level %d: bit=[%b], has_traps=%b"
              attested_level
              slot_index
              published_level
              actual_bit
              has_traps
          else unit
  in

  (* Run until we've seen all three cases, or give up after a limit. *)
  let max_steps = 10 * blocks_per_cycle in
  log_step
    "Running measurement for at most %d steps (up to %d cycles)."
    max_steps
    (max_steps / blocks_per_cycle) ;

  let rec loop step =
    if
      !count_not_in_committee > 0
      && !count_attestable_slots > 0
      && !count_traps > 0
    then unit
    else if step > max_steps then
      Test.fail
        "Reached max_steps=%d without seeing all event kinds. Summary: \
         Not_in_committee=%d, Attestable_slots=%d, Traps=%d"
        max_steps
        !count_not_in_committee
        !count_attestable_slots
        !count_traps
    else
      let* () = check_one_level () in
      loop (step + 1)
  in
  let* () = loop 1 in

  log_step
    "Final summary: Not_in_committee = %d, Attestable_slots = %d, Traps = %d"
    !count_not_in_committee
    !count_attestable_slots
    !count_traps ;

  unit

(** Test encode/decode round-trip for single lag. *)
let test_attestations_encode_decode_single_lag =
  Protocol.register_test
    ~__FILE__
    ~title:"DAL attestations encode/decode single lag"
    ~tags:["dal"; "attestations"; "encode"; "decode"; "single_lag"]
    ~uses_node:true
    ~uses_client:true
  @@ fun protocol ->
  let* node, _client = Client.init_with_protocol `Client ~protocol () in
  let attestation_lag, attestation_lags =
    if Protocol.number protocol < 025 then (8, [8]) else (5, [2; 3; 5])
  in
  let dal_parameters : Dal.Parameters.t =
    {
      feature_enabled = true;
      incentives_enabled = false;
      cryptobox =
        {
          number_of_shards = 2048;
          redundancy_factor = 16;
          slot_size = 65536;
          page_size = 4096;
        };
      number_of_slots = 4;
      attestation_lag;
      attestation_lags;
      attestation_threshold = 50;
    }
  in
  let number_of_lags = List.length dal_parameters.attestation_lags in
  Log.info
    "Parameters: number_of_slots=%d, number_of_lags=%d, attestation_lags=[%s]"
    dal_parameters.number_of_slots
    number_of_lags
    (dal_parameters.attestation_lags |> List.map string_of_int
   |> String.concat "; ") ;

  Log.info "Create a test attestation: first and last slot attested" ;
  let original = Array.make dal_parameters.number_of_slots false in
  original.(0) <- true ;
  original.(dal_parameters.number_of_slots - 1) <- true ;

  let* () =
    Lwt_list.iter_s
      (fun lag_index ->
        Log.info "Testing encoding/decoding at lag_index: %d" lag_index ;
        let* encoded =
          Dal.Attestations.encode_for_one_lag
            protocol
            (Node.as_rpc_endpoint node)
            dal_parameters
            ~lag_index
            original
        in
        Log.info "Encoded (lag_index: %d): %s" lag_index encoded ;

        let* decoded =
          Dal.Attestations.decode
            protocol
            (Node.as_rpc_endpoint node)
            dal_parameters
            encoded
        in
        let decoded_for_lag = decoded.(lag_index) in
        Check.(
          (Array.length decoded_for_lag = dal_parameters.number_of_slots) int)
          ~error_msg:"Expected %R slots, got %L" ;
        Array.iteri
          (fun i expected ->
            Check.(decoded_for_lag.(i) = expected)
              Check.bool
              ~error_msg:(Format.sprintf "Slot %d mismatch after round-trip" i))
          original ;
        unit)
      (List.init number_of_lags Fun.id)
  in

  Log.info "Single-lag encode/decode succeeded" ;
  unit

(** Test encode/decode round-trip for multiple lags. *)
let test_attestations_encode_decode_multiple_lags =
  Protocol.register_test
    ~__FILE__
    ~title:"DAL attestations encode/decode multiple lags"
    ~tags:["dal"; "attestations"; "encode"; "decode"; "multiple_lags"]
    ~uses_node:true
    ~uses_client:true
    ~supports:(Protocol.From_protocol 025)
  @@ fun protocol ->
  let* node, _client = Client.init_with_protocol `Client ~protocol () in
  let dal_parameters : Dal.Parameters.t =
    {
      feature_enabled = true;
      incentives_enabled = false;
      cryptobox =
        {
          number_of_shards = 2048;
          redundancy_factor = 16;
          slot_size = 65536;
          page_size = 4096;
        };
      number_of_slots = 4;
      attestation_lag = 5;
      attestation_lags = [2; 3; 5];
      attestation_threshold = 50;
    }
  in
  let number_of_lags = List.length dal_parameters.attestation_lags in
  Log.info
    "Parameters: number_of_slots=%d, number_of_lags=%d, attestation_lags=[%s]"
    dal_parameters.number_of_slots
    number_of_lags
    (dal_parameters.attestation_lags |> List.map string_of_int
   |> String.concat "; ") ;

  (* Create test attestations: different slots attested at different lags *)
  let attestations_per_lag =
    Array.init number_of_lags (fun lag_index ->
        let arr = Array.make dal_parameters.number_of_slots false in
        (* Attest slot [lag_index mod number_of_slots] at each lag *)
        arr.(lag_index mod dal_parameters.number_of_slots) <- true ;
        (* Also attest slot 1 at lag 0 if we have multiple slots *)
        if lag_index = 0 && dal_parameters.number_of_slots > 1 then
          arr.(1) <- true ;
        arr)
  in
  let* encoded =
    Dal.Attestations.encode
      protocol
      (Node.as_rpc_endpoint node)
      attestations_per_lag
  in
  Log.info "Encoded multi-lag: %s" encoded ;

  let* decoded =
    Dal.Attestations.decode
      protocol
      (Node.as_rpc_endpoint node)
      dal_parameters
      encoded
  in
  Check.((Array.length decoded = number_of_lags) int)
    ~error_msg:"Expected %R lags, got %L" ;
  Array.iteri
    (fun lag_index original_arr ->
      let decoded_arr = decoded.(lag_index) in
      Check.((Array.length decoded_arr = dal_parameters.number_of_slots) int)
        ~error_msg:
          (Format.sprintf "Lag %d: expected %%R slots, got %%L" lag_index) ;
      Array.iteri
        (fun slot_index expected ->
          Check.(decoded_arr.(slot_index) = expected)
            Check.bool
            ~error_msg:
              (Format.sprintf
                 "Lag %d, slot %d mismatch: expected %b"
                 lag_index
                 slot_index
                 expected))
        original_arr)
    attestations_per_lag ;

  Log.info "Multi-lag encode/decode succeeded" ;
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

let register ~protocols =
  (* Tests with Layer1 node only *)
  Dal_l1.register ~__FILE__ ~protocols ;
  scenario_with_layer1_node
    ~__FILE__
    "attesters receive expected DAL rewards depending on participation"
    test_dal_rewards_distribution
    (List.filter (fun p -> Protocol.number p >= 022) protocols)
    (* We set attestation threshold to 30% because we'll have 2 regular bakers
       who attest sufficiently. *)
    ~attestation_threshold:30
    ~attestation_lag:2
    ~blocks_per_cycle:16
    ~blocks_per_commitment:17 (* so that there's no nonce revelation required *) ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    "slot is protocol attested even if attestations are aggregated"
    test_aggregation_required_to_pass_quorum
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~traps_fraction:Q.one
    "inject accusation"
    test_inject_accusation
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~traps_fraction:Q.one
    "inject accusation with dynamic multi-lag attestations"
    ~tags:["traps"; "denunciation"; "multi_lag"]
    test_inject_accusation_dynamic_multi_lag
    (List.filter (fun p -> Protocol.number p >= 025) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~traps_fraction:Q.one
    ~operator_profiles:[0]
    "inject accusation of aggregated attestation"
    (test_inject_accusation_aggregated_attestation 1)
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~traps_fraction:Q.one
    ~operator_profiles:[0]
    "inject several accusations for the same aggregated attestation"
    (test_inject_accusation_aggregated_attestation 2)
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~traps_fraction:Q.one
    "inject a duplicated denunciation at different steps"
    test_duplicate_denunciations
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_node
    ~__FILE__
    ~traps_fraction:Q.one
    "inject a denunciation at the next cycle"
    test_denunciation_next_cycle
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  (* Tests with layer1 and dal nodes *)
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~number_of_slots:1
    ~operator_profiles:[0]
    ~traps_fraction:Q.one
    "faulty DAL node entrapment"
    test_e2e_trap_faulty_dal_node
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  test_dal_node_startup protocols ;
  test_dal_node_invalid_config () ;
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

  (* Tests with layer1 and dal nodes (with p2p/GS) *)
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~prover:false
    ~tags:["gossipsub"]
    "GS/P2P connection and disconnection"
    test_dal_node_p2p_connection_and_disconnection
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~prover:false
    ~tags:["gossipsub"]
    "GS join topic"
    test_dal_node_join_topic
    protocols ;
  List.iter
    (fun batching_time_interval ->
      scenario_with_layer1_and_dal_nodes
        ~__FILE__
        ~tags:["gossipsub"]
        ~batching_time_interval
        ~operator_profiles:[0]
        (Format.sprintf
           "GS valid messages exchange (with batching %s)"
           batching_time_interval)
        (test_dal_node_gs_valid_messages_exchange ~batching_time_interval)
        protocols ;
      scenario_with_layer1_and_dal_nodes
        ~__FILE__
        ~tags:["gossipsub"]
        ~batching_time_interval
        (Format.sprintf
           "GS invalid messages exchange (with batching %s)"
           batching_time_interval)
        ~operator_profiles:[0]
        (test_dal_node_gs_invalid_messages_exchange ~batching_time_interval)
        protocols ;
      scenario_with_layer1_and_dal_nodes
        ~__FILE__
        ~tags:["attestation"; "p2p"]
        ~batching_time_interval
        ~attestation_threshold:100
        ~bootstrap_profile:true
        ~l1_history_mode:Default_with_refutation
        (Format.sprintf
           "attestation through p2p (with batching %s)"
           batching_time_interval)
        (test_attestation_through_p2p ~batching_time_interval)
        protocols)
    ["disabled"; "100"; "20"] ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~attestation_threshold:1
    ~l1_history_mode:Default_with_refutation
    "Attester attests produced slot"
    test_producer_attester
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~attestation_threshold:1
    ~l1_history_mode:Default_with_refutation
    ~event_sections_levels:[("prevalidator", `Debug)]
    "attester_did_not_attest warning is emitted"
    test_attester_did_not_attest
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    "baker registers profiles with dal node"
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~activation_timestamp:Now
    ~prover:false
    test_baker_registers_profiles
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["bootstrap"]
    ~bootstrap_profile:true
    ~prover:false
    ~l1_history_mode:Default_with_refutation
    "peer discovery via bootstrap node"
    test_peer_discovery_via_bootstrap_node
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
    ~tags:["bootstrap"; "trusted"; "connection"]
    ~bootstrap_profile:true
    "trusted peers reconnection"
    ~prover:false
    ~l1_history_mode:Default_with_refutation
    test_peers_reconnection
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["operator"; "profile"]
    "operator profile"
    ~prover:false
    test_operator_profile
    protocols ;
  Dal_skip_list.register ~__FILE__ ~protocols ;
  Dal_amplification.register ~__FILE__ ~protocols ;
  Dal_gc.register ~__FILE__ ~protocols ;
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
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~traps_fraction:Q.zero
    ~number_of_slots:1
    "new attester attests"
    test_new_attester_attests
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~l1_history_mode:Default_with_refutation
    ~traps_fraction:(Q.of_float 0.5)
    ~number_of_slots:1
    "low stake attester attests (with traps)"
    test_dal_low_stake_attester_attestable_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~operator_profiles:[0]
    ~l1_history_mode:Default_with_refutation
    ~traps_fraction:(Q.of_float 0.5)
    ~all_bakers_attest_activation_threshold:Q.zero
    ~number_of_slots:1
    "low stake attester attests (with traps and ABA activated)"
    test_dal_low_stake_attester_attestable_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~number_of_slots:1
    "publish slot in one level reorganisation"
    test_dal_one_level_reorg
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~number_of_slots:1
    ~operator_profiles:[0]
    ~regression:true
    "attesters receive DAL rewards"
    test_attesters_receive_dal_rewards
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
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
    ~tags:["traps"]
    ~operator_profiles:[0]
    ~traps_fraction:Q.one
    ~all_bakers_attest_activation_threshold:Q.(Z.one /// Z.of_int 2)
    "Trap is denounced when all bakers attest"
    test_denunciation_when_all_bakers_attest
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["restart"; "statuses"]
    "Status information is backfilled at restart"
    ~operator_profiles:[0]
    test_statuses_backfill_at_restart
    protocols ;

  (* Tests with all nodes *)
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
  Dal_tx_kernel.register ~__FILE__ ~protocols ;

  (* Register tutorial test *)
  scenario_tutorial_dal_baker protocols ;

  scenario_with_all_nodes
    ~__FILE__
    "Rollup injects DAL slots"
    ~regression:false
    ~pvm_name:"wasm_2_0_0"
    ~commitment_period:5
    rollup_node_injects_dal_slots
    ~operator_profiles:[0]
      (* It it sufficient for a single baker here to receive some shards here to
         declare the slot available. Otherwise the test might be flaky as we
         bake with a timestamp in the past. *)
    ~attestation_threshold:1
    protocols ;

  scenario_with_all_nodes
    ~__FILE__
    "Rollup batches and injects optimal DAL slots"
    ~regression:false
    ~pvm_name:"wasm_2_0_0"
    ~commitment_period:5
    rollup_batches_and_publishes_optimal_dal_slots
    ~operator_profiles:[0; 1; 2]
      (* It it sufficient for a single baker to receive some shards here to
         declare the slot available. Otherwise the test might be flaky as we
         bake with a timestamp in the past. *)
    ~attestation_threshold:1
    protocols ;

  dal_crypto_benchmark () ;
  scenario_with_layer1_node
    ~__FILE__
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~activation_timestamp:Now
    "mockup get_attestable_slots"
    use_mockup_node_for_getting_attestable_slots
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
    protocols ;

  test_attestations_encode_decode_single_lag protocols ;
  test_attestations_encode_decode_multiple_lags protocols

let register_migration ~migrate_from ~migrate_to =
  Dal_skip_list.register_migration ~__FILE__ ~migrate_from ~migrate_to ;
  Dal_amplification.register_migration ~__FILE__ ~migrate_from ~migrate_to ;
  Dal_migration.register_migration ~__FILE__ ~migrate_from ~migrate_to

let () =
  Regression.register
    ~__FILE__
    ~title:"DAL Node: debug print store schemas"
    ~tags:["dal"; "store"; "schemas"]
    ~uses:[Constant.octez_dal_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let hooks = Tezos_regression.hooks in
  Dal_node.debug_print_store_schemas ~hooks ()

let () =
  Regression.register
    ~__FILE__
    ~title:"DAL Node: P2P message encoding"
    ~tags:["dal"; "gossipsub"; "p2p"]
    ~uses:[Constant.octez_codec]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* output = Codec.describe_binary_schema ~id:"dal_p2p_message" () in
  Regression.capture output ;
  let* output = Codec.describe_json_schema ~id:"dal_p2p_message" () in
  Regression.capture output ;
  unit
