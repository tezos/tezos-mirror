(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL skip-list RPC tests. *)

open Dal_helpers
module Dal = Dal_common

(* In the following function, no migration is performed (expect from genesis
     to alpha) when [migration_level] is equal to or smaller than 1. When there
     is no migration, [migrate_from = migrate_to]. *)
let main_scenario ~__FILE__ ?(migration_level = 1) ~slot_index
    ~last_confirmed_published_level ~migrate_from ~migrate_to dal_parameters
    client node dal_node =
  let module Map_int = Map.Make (Int) in
  Log.info "slot_index = %d" slot_index ;
  let client = Client.with_dal_node client ~dal_node in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let lag = dal_parameters.attestation_lag in
  let number_of_slots = dal_parameters.number_of_slots in

  Log.info
    "attestation_lag = %d, number_of_slots = %d, slot_size = %d"
    lag
    number_of_slots
    slot_size ;
  let* starting_level = Client.level client in
  let rec publish ~max_level level commitments =
    (* Try to publish a slot at each level *)
    if level > max_level then return commitments
    else
      let published_level = level + 1 in
      let wait_mempool_injection =
        Node.wait_for node "operation_injected.v0" (fun _ -> Some ())
      in
      let wait_for_dal_node =
        wait_for_layer1_final_block dal_node (level - 1)
      in
      let* commitment =
        Helpers.publish_and_store_slot
          client
          dal_node
          Constant.bootstrap1
          ~index:slot_index
          ~force:true
        @@ Helpers.make_slot ~slot_size ("slot " ^ string_of_int level)
      in
      let* () = wait_mempool_injection in
      let* () = bake_for client in
      let* _level = Node.wait_for_level node (level + 1) in
      let* () = if level > 2 then wait_for_dal_node else unit in
      publish
        ~max_level
        (level + 1)
        (Map_int.add published_level commitment commitments)
  in
  if migration_level > 1 then
    Log.info
      "Publishing commitments in the previous protocol, from published level \
       %d to %d"
      (starting_level + 1)
      migration_level ;
  let* commitments =
    publish ~max_level:(migration_level - 1) starting_level Map_int.empty
  in

  if migration_level > 1 then Log.info "Migrated to the next protocol." ;

  let* new_proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let new_dal_params =
    Dal.Parameters.from_protocol_parameters new_proto_params
  in
  let new_lag = new_dal_params.attestation_lag in
  let new_attestation_lags =
    JSON.(
      new_proto_params |-> "dal_parametric" |-> "attestation_lags" |> as_list
      |> List.map as_int)
  in
  if new_lag <> lag then Log.info "new attestation_lag = %d" new_lag ;

  let new_dal_parameters =
    {
      dal_parameters with
      attestation_lag = new_lag;
      attestation_lags = new_attestation_lags;
    }
  in

  let new_number_of_slots = new_dal_params.number_of_slots in
  if new_number_of_slots <> number_of_slots then
    Log.info "new number_of_slots = %d" new_number_of_slots ;
  let last_attested_level = last_confirmed_published_level + new_lag in
  (* The maximum level that needs to be reached (we use +2 to make last
       attested level final). *)
  let max_level = last_attested_level + 2 in
  Log.info
    "last published_level = %d, last attested_level = %d, last level = %d"
    last_confirmed_published_level
    last_attested_level
    max_level ;

  let* second_level_new_proto = Client.level client in
  assert (second_level_new_proto = migration_level) ;
  Log.info
    "Publish commitments in the new protocol, from published level %d to %d"
    (second_level_new_proto + 1)
    last_confirmed_published_level ;
  let* commitments =
    publish
      ~max_level:(last_confirmed_published_level - 1)
      second_level_new_proto
      commitments
  in

  (* The maximum level that needs to be reached (we use +2 to make last
       attested level final). *)
  let* () =
    let* current_level = Node.get_level node in
    let count = max_level - current_level in
    Log.info "Current level is %d. Bake %d more blocks." current_level count ;
    let wait_for_level = ref (current_level - 1) in
    repeat count (fun () ->
        let wait_for_dal_node =
          wait_for_layer1_final_block dal_node !wait_for_level
        in
        incr wait_for_level ;
        let* () = bake_for client in
        wait_for_dal_node)
  in

  let module SeenIndexes = Set.Make (struct
    type t = int

    let compare = compare
  end) in
  let seen_indexes = ref SeenIndexes.empty in
  let at_least_one_attested_status = ref false in

  let published cell_level cell_slot_index =
    (* - Cond 1: we publish at [slot_index]
         - Cond 2: the (published) [cell_level] is greater than [starting_level]
         - Cond 3: the (published) [cell_level] is smaller or equal to
           [last_confirmed_published_level] *)
    cell_slot_index = slot_index
    && cell_level > starting_level
    && cell_level <= last_confirmed_published_level
  in

  let rec check_cell cell ~check_level =
    let skip_list_kind = JSON.(cell |-> "kind" |> as_string) in
    let expected_skip_list_kind = "dal_skip_list" in
    Check.(
      (skip_list_kind = expected_skip_list_kind)
        string
        ~__LOC__
        ~error_msg:"Unexpected skip list kind: got %L, expected %R") ;
    let skip_list = JSON.(cell |-> "skip_list") in
    let cell_index = JSON.(skip_list |-> "index" |> as_int) in
    if SeenIndexes.mem cell_index !seen_indexes then unit
    else (
      seen_indexes := SeenIndexes.add cell_index !seen_indexes ;
      let content = JSON.(skip_list |-> "content") in
      let cell_level = JSON.(content |-> "level" |> as_int) in
      let cell_slot_index = JSON.(content |-> "index" |> as_int) in
      let expected_published = published cell_level cell_slot_index in

      let () =
        match check_level with
        | None -> ()
        | Some level ->
            let current_number_of_slots =
              if level > migration_level then new_number_of_slots
              else number_of_slots
            in
            let expected_slot_index =
              if level = 1 then
                (* the "slot index" of genesis *)
                0
              else current_number_of_slots - 1
            in
            Check.(
              (cell_slot_index = expected_slot_index)
                int
                ~__LOC__
                ~error_msg:"Unexpected slot index: got %L, expected %R")
      in
      (match check_level with
      | Some level ->
          assert (level >= 1) ;
          let applied_lag = if level > migration_level then new_lag else lag in
          let expected_published_level =
            if level = 1 then (* the "level" of genesis *) 0
            else if Protocol.number migrate_to < 025 then level - applied_lag
            else if expected_published || level <= migration_level then
              level - applied_lag
            else level
          in
          Check.(
            (cell_level = expected_published_level)
              int
              ~__LOC__
              ~error_msg:
                "Unexpected cell's published level: got %L, expected %R")
      | None -> ()) ;
      let cell_slot_index = JSON.(content |-> "index" |> as_int) in
      let () =
        match check_level with
        | None -> ()
        | Some level ->
            let expected_published_level =
              if level = 1 then (* the "level" of genesis *) 0
              else if
                Protocol.number migrate_to >= 025
                && level > migration_level && not expected_published
              then
                (* With dynamic lags, unpublished cells have lag=0, so
                     published_level = level. *)
                level
              else if level > migration_level then level - new_lag
              else level - lag
            in
            let current_number_of_slots =
              if expected_published_level > migration_level then
                new_number_of_slots
              else number_of_slots
            in
            let expected_slot_index =
              if level = 1 then
                (* the "slot index" of genesis *)
                0
              else current_number_of_slots - 1
            in
            let error_msg =
              Format.sprintf "For level %d, " level
              ^ "Unexpected slot index: got %L, expected %R"
            in
            Check.(
              (cell_slot_index = expected_slot_index) int ~__LOC__ ~error_msg)
      in
      (if cell_index > 0 && Protocol.number migrate_to < 025 then
         (* With dynamic lags (protocol >= 025), the cell index relationship
              becomes too complex for a simple formula.  The cell_level,
              cell_slot_index, cell_kind, and back_pointer checks are sufficient
              to verify the skip list structure. *)
         let accumulated_nb_of_slots =
           if cell_level > migration_level then
             (migration_level * number_of_slots)
             + ((cell_level - migration_level - 1) * new_number_of_slots)
           else (cell_level - 1) * number_of_slots
         in
         let expected_cell_index = accumulated_nb_of_slots + cell_slot_index in
         Check.(
           (cell_index = expected_cell_index)
             int
             ~__LOC__
             ~error_msg:"Unexpected cell index: got %L, expected %R")) ;
      let cell_kind = JSON.(content |-> "kind" |> as_string) in
      let expected_kind =
        if not expected_published then "unpublished"
        else (
          at_least_one_attested_status := true ;
          "published")
      in
      Check.(
        (cell_kind = expected_kind)
          string
          ~__LOC__
          ~error_msg:"Unexpected cell kind: got %L, expected %R") ;
      (if cell_kind = "published" || cell_kind = "attested" then
         let commitment = JSON.(content |-> "commitment" |> as_string) in
         Check.(
           (commitment = Map_int.find cell_level commitments)
             string
             ~__LOC__
             ~error_msg:"Unexpected commitment: got %L, expected %R")) ;
      let back_pointers =
        JSON.(skip_list |-> "back_pointers" |> as_list)
        |> List.map JSON.as_string
      in
      let expecting_no_back_pointers = cell_index = 0 in
      let no_back_pointers = back_pointers = [] in
      Check.(
        (no_back_pointers = expecting_no_back_pointers)
          bool
          ~error_msg:
            "Unexpected non-existence of back_pointers: got %L, expected %R") ;
      Lwt_list.iter_s
        (fun hash ->
          let* cell =
            Dal_RPC.(
              call dal_node
              @@ get_plugin_commitments_history_hash
                   ~proto_hash:(Protocol.hash migrate_to)
                   ~hash
                   ())
          in
          check_cell cell ~check_level:None)
        back_pointers)
  in
  let rec check_history level =
    if level > last_attested_level then unit
    else
      let* cell =
        Node.RPC.call node
        @@ RPC.get_chain_block_context_dal_commitments_history
             ~block:(string_of_int level)
             ()
      in
      let* () = check_cell cell ~check_level:(Some level) in
      check_history (level + 1)
  in
  Log.info "Check skip-list using commitments_history RPCs" ;
  let* () = check_history 1 in

  Check.(
    (!at_least_one_attested_status = true)
      bool
      ~__LOC__
      ~error_msg:"No cell with the 'attested' status has been visited") ;

  let rec call_cells_of_level level =
    if level > last_confirmed_published_level then unit
    else
      let* cells =
        Node.RPC.call node
        @@ RPC.get_chain_block_context_dal_cells_of_level
             ~block:(string_of_int level)
             ()
      in
      let cells = JSON.as_list cells in
      let num_cells = List.length cells in
      let common_tail () =
        if level = migration_level + 1 then
          if Protocol.number migrate_to < 025 then
            (lag - new_lag + 1) * number_of_slots
          else
            (* The cardinal of the set of :
                 * [(lag - new_lag + 1) * number_of_slots]: all slots for all
                   levels between [migration_level - lag + 1] and
                   [migration_level - new_lag + 1];
                 * [(new_lag - 1) * (number_of_slots - 1)]: all unpublished
                   slots between [migration_level - new_lag + 2] and
                   [migration_level] (old number_of_slots);
                 * [(new_number_of_slots - 1)]: all unpublished slots at
                   [migration_level + 1] (new number_of_slots). *)
            ((lag - new_lag + 1) * number_of_slots)
            + ((new_lag - 1) * (number_of_slots - 1))
            + (new_number_of_slots - 1)
        else if level > migration_level then new_number_of_slots
        else number_of_slots
      in
      let expected_num_cells =
        if Protocol.number migrate_to >= 025 then
          if level < max 2 (min migration_level lag) then 0
          else if level < lag then number_of_slots - 1
          else common_tail ()
        else if level < lag then 0
        else common_tail ()
      in
      Check.(
        (num_cells = expected_num_cells)
          int
          ~__LOC__
          ~error_msg:"Unexpected number of cells: got %L, expected %R") ;
      let* () =
        Lwt_list.iter_s
          (fun hash_cell_tuple ->
            let cell = JSON.geti 1 hash_cell_tuple in
            check_cell cell ~check_level:(Some level))
          cells
      in
      call_cells_of_level (level + 1)
  in
  Log.info
    "Call cells_of_level on each relevant level, and check the number of cells \
     returned" ;
  let* () = call_cells_of_level 1 in

  let rec call_get_metadata attested_level =
    if attested_level > last_attested_level then unit
    else
      let* metadata =
        Node.RPC.call node
        @@ RPC.get_chain_block_metadata ~block:(string_of_int attested_level) ()
      in
      let protocol, params_to_use =
        if attested_level <= migration_level then (migrate_from, dal_parameters)
        else (migrate_to, new_dal_parameters)
      in
      let* attested =
        match metadata.dal_attestation with
        | None ->
            Test.fail
              "At attested level %d, unexpected missing slot attestability \
               information"
              attested_level
        | Some str ->
            let* decoded =
              Dal.Slot_availability.decode
                protocol
                (Node.as_rpc_endpoint node)
                params_to_use
                str
            in
            return
              (List.mapi
                 (fun lag_index attestation_lag ->
                   let published_level = attested_level - attestation_lag in
                   if
                     published_level > starting_level
                     && published_level <= last_confirmed_published_level
                   then
                     let vec = decoded.(lag_index) in
                     Array.length vec > slot_index && vec.(slot_index)
                   else false)
                 params_to_use.attestation_lags
              |> List.exists Fun.id)
      in
      (* Checks if a [level] is in the migration transition gap *)
      let in_migration_gap level lag =
        level > migration_level && level <= migration_level + lag
      in
      (* Checks if a slot at [published_level] would be attested at
           a smaller lag before reaching the current attested_level *)
      let already_attested_at_smaller_lag published_level current_lag =
        List.exists
          (fun smaller_lag ->
            smaller_lag < current_lag
            && not (in_migration_gap (published_level + smaller_lag) new_lag))
          params_to_use.attestation_lags
      in
      let expected_attested =
        if in_migration_gap attested_level new_lag then false
        else
          List.exists
            (fun attestation_lag ->
              let published_level = attested_level - attestation_lag in
              published_level > starting_level
              && published_level <= last_confirmed_published_level
              && Map_int.mem published_level commitments
              && not
                   (already_attested_at_smaller_lag
                      published_level
                      attestation_lag))
            params_to_use.attestation_lags
      in
      Check.(
        (attested = expected_attested)
          bool
          ~__LOC__
          ~error_msg:
            (let msg = sf "Attested at level %d: " attested_level in
             msg ^ "got %L, expected %R")) ;
      call_get_metadata (attested_level + 1)
  in
  Log.info "Check slot availability information" ;
  let* () = call_get_metadata (2 + lag) in

  let rec call_get_commitment level =
    if level > last_confirmed_published_level then unit
    else
      let* commitment =
        Dal_RPC.(
          call dal_node
          @@ get_level_slot_commitment ~slot_level:level ~slot_index)
      in
      let expected_commitment =
        match Map_int.find_opt level commitments with
        | Some c -> c
        | None -> Test.fail "Commitment not found at level %d" level
      in
      Check.(
        (commitment = expected_commitment)
          string
          ~__LOC__
          ~error_msg:
            (let msg = sf "Unexpected commitment at level %d: " level in
             msg ^ "got %L, expected %R")) ;
      call_get_commitment (level + 1)
  in
  Log.info "Check fetching commitments from the skip-list store" ;
  let* () = call_get_commitment 2 in

  let rec call_get_status level =
    (* At [max_level] we'd get 404; at [max_level - 1] we get answer because
         statuses are inserted at L1 head level + 1. *)
    if level >= max_level then unit
    else
      let expected_status =
        if level <= migration_level - lag then Dal_RPC.Attested lag
        else if level == migration_level + 1 - lag then Dal_RPC.Unattested
        else if new_lag <> lag && level <= migration_level then
          Dal_RPC.Unattested
        else if level <= last_confirmed_published_level then
          Dal_RPC.Attested new_lag
        else Dal_RPC.Unpublished
      in
      let* () =
        check_slot_status
          ~__LOC__
          dal_node
          ~expected_status
          ~check_attested_lag:`At_most
          ~slot_level:level
          ~slot_index
      in
      call_get_status (level + 1)
  in
  Log.info "Check fetching statuses from the skip-list store" ;
  let* () = call_get_status 2 in
  unit

let test_skip_list_rpcs ~__FILE__ protocols =
  let scenario protocol dal_parameters _ client node dal_node =
    main_scenario
      ~__FILE__
      ~slot_index:3
      ~last_confirmed_published_level:3
      ~migrate_from:protocol
      ~migrate_to:protocol
      dal_parameters
      node
      client
      dal_node
  in
  let tags = ["rpc"; "skip_list"] in
  let description = "skip-list RPCs" in
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags
    ~operator_profiles:[3; 15]
    description
    scenario
    protocols

let test_skip_list_rpcs_with_migration ~__FILE__ ~migrate_from ~migrate_to
    ~migration_level =
  let slot_index = 3 in
  let scenario ~migrate_to ~migration_level dal_parameters =
    let lag = dal_parameters.Dal.Parameters.attestation_lag in
    Check.(
      (migration_level > lag)
        int
        ~error_msg:
          "The migration level (%L) should be greater than the attestation lag \
           (%R)") ;
    (* The first cell level has this value, if the previous protocol
         doesn't have the DAL activated. *)
    (* We'll have 3 levels with a published and attested slot. *)
    let last_confirmed_published_level = migration_level + 3 in
    main_scenario
      ~__FILE__
      ~slot_index
      ~last_confirmed_published_level
      ~migration_level
      ~migrate_from
      ~migrate_to
      dal_parameters
  in

  let description = "test skip-list RPCs with migration" in
  let tags = ["rpc"; "skip_list"] in
  test_l1_migration_scenario
    ~__FILE__
    ~migrate_from
    ~migrate_to
    ~migration_level
    ~scenario:(fun ~migration_level -> scenario ~migrate_to ~migration_level)
    ~tags
    ~description
    ~operator_profiles:[slot_index] (* use the same parameters as Alpha *)
    ()

let register ~__FILE__ ~protocols = test_skip_list_rpcs ~__FILE__ protocols

let register_migration ~__FILE__ ~migrate_from ~migrate_to =
  if not (migrate_from = Protocol.U025 && migrate_to = Protocol.Alpha) then
    test_skip_list_rpcs_with_migration
      ~__FILE__
      ~migration_level:11
      ~migrate_from
      ~migrate_to
