(* Testing
   -------
   Component:    Protocol, delegate
   Invocation:   dune exec src/proto_alpha/lib_delegate/test/main.exe \
                  -- --file test_scenario.ml
   Subject:      Test different scenario for delegate
*)

open Mockup_simulator

let bootstrap1 = Signature.Public_key.hash bootstrap1

let bootstrap2 = Signature.Public_key.hash bootstrap2

let bootstrap3 = Signature.Public_key.hash bootstrap3

let bootstrap4 = Signature.Public_key.hash bootstrap4

let bootstrap5 = Signature.Public_key.hash bootstrap5

let some_seed s = Some (Protocol.State_hash.of_b58check_exn s)

(*

Test that the chain reaches the 5th level.

*)

let test_level_5 () =
  let open Lwt_result_syntax in
  let level_to_reach = 5l in
  let module Hooks : Hooks = struct
    include Default_hooks

    let stop_on_event = function
      | Baking_state.New_head_proposal {block; _} ->
          (* Stop the node as soon as we receive a proposal with a level
             higher than [level_to_reach]. *)
          block.shell.level > level_to_reach
      | _ -> false

    let check_chain_on_success ~chain =
      (* Make sure that all decided blocks have been decided at round 0. *)
      let round_is_zero block =
        let level = block.rpc_context.block_header.level in
        let* round = get_block_round block in
        if Int32.equal round 0l then return_unit
        else failwith "block at level %ld was selected at round %ld" level round
      in
      List.iter_es round_is_zero chain
  end in
  (* Here we start two bakers, one with 3 delegates (bootstrap1, bootstrap2,
     bootstrap3) and the other with 2 delegates (bootstrap4, bootstrap5).
     The simulation continues till both nodes stop, see [stop_on_event]
     above. *)
  let config =
    {
      default_config with
      timeout = Int32.to_int level_to_reach * 3 * 2;
      round0 = 2L;
      round1 = 3L;
    }
  in
  run ~config [(3, (module Hooks)); (2, (module Hooks))]

let test_preattest_on_valid () =
  let open Lwt_result_syntax in
  let level_to_reach = 2l in
  let round_to_reach = 1l in
  let module Hooks : Hooks = struct
    include Default_hooks

    let on_new_head ~block_hash ~block_header =
      (* Stop notifying heads on the level to reach, only notify that
         it has been validated *)
      if block_header.Block_header.shell.level < level_to_reach then
        Lwt.return_some (block_hash, block_header)
      else Lwt.return_none

    let seen_candidate = ref None

    let pqc_noticed = ref false

    let qc_noticed = ref false

    let stop_on_event = function
      | Baking_state.Prequorum_reached (candidate, _) ->
          (* Register the PQC notice. *)
          (match !seen_candidate with
          | Some seen_candidate
            when Block_hash.(candidate.hash = seen_candidate) ->
              pqc_noticed := true
          | _ -> ()) ;
          false
      | Baking_state.Quorum_reached (candidate, _) ->
          (* Because attestations are sent regardless of whether
             a head has been applied, we can expect a quorum to
             be received regardless of a head not being produced. *)
          (match !seen_candidate with
          | Some seen_candidate
            when Block_hash.(candidate.hash = seen_candidate) ->
              qc_noticed := true
          | _ -> ()) ;
          false
      | New_head_proposal {block; _} ->
          (* Ensure that we never notice a new head at the level where
             we are not supposed to. *)
          if block.shell.level = level_to_reach then
            Stdlib.failwith "Unexpected new head event"
          else false
      | New_valid_proposal {block; _} ->
          (* Register the seen valid proposal candidate. *)
          if
            block.shell.level = level_to_reach
            && Protocol.Alpha_context.Round.to_int32 block.round = 0l
          then seen_candidate := Some block.hash ;
          (* Stop the node when we reach level 2 / round 2. *)
          block.shell.level = level_to_reach
          && Protocol.Alpha_context.Round.to_int32 block.round >= round_to_reach
      | _ -> false

    let check_chain_on_success ~chain:_ =
      assert (!seen_candidate <> None) ;
      assert !pqc_noticed ;
      assert !qc_noticed ;
      return_unit
  end in
  let config = {default_config with timeout = 10} in
  run ~config [(1, (module Hooks))]

let test_reset_delayed_pqc () =
  let open Lwt_syntax in
  let module Hooks : Hooks = struct
    include Default_hooks

    let should_wait = ref true

    let trigger = ref false

    let on_new_operation x =
      let* () =
        if !should_wait then Lwt_unix.sleep 0.5 else Lwt_unix.sleep 0.2
      in
      if !trigger then (
        trigger := false ;
        Lwt.return_none)
      else Lwt.return_some x

    let on_new_head ~block_hash ~(block_header : Block_header.t) =
      let block_round =
        match
          Protocol.Alpha_context.Fitness.round_from_raw
            block_header.shell.fitness
        with
        | Error _ -> assert false
        | Ok x -> x
      in
      if
        block_header.Block_header.shell.level = 1l
        && Protocol.Alpha_context.Round.(block_round = zero)
      then (
        let* () = Lwt_unix.sleep 1. in
        should_wait := false ;
        trigger := true ;
        Lwt.return_some (block_hash, block_header))
      else Lwt.return_some (block_hash, block_header)

    let stop_on_event = function
      | Baking_state.New_valid_proposal {block; _} ->
          let is_high_round =
            let open Protocol.Alpha_context.Round in
            match of_int 5 with
            | Ok high_round -> block.round = high_round
            | _ -> assert false
          in
          (block.shell.level = 1l && is_high_round) || block.shell.level > 1l
      | _ -> false

    let check_chain_on_success ~chain =
      let head = Stdlib.List.hd chain in
      if head.rpc_context.block_header.level = 1l then failwith "baker is stuck"
      else return_ok_unit
  end in
  let config = {default_config with round0 = 2L; round1 = 3L; timeout = 50} in
  run ~config [(1, (module Hooks))]

(*

Scenario T1

1. Node A proposes at the round 0.
2. Both node A and node B preattest.
3. Node A stops.
4. Node B attests in the round 0 and locks. No decision is taken at the
   round 0 because A did not attest.
5. We check that in round 1 (the next slot for B), B proposes the same
   value as A proposed in the round 0, not a new proposal.
*)

let test_scenario_t1 () =
  let open Lwt_result_syntax in
  let original_proposal = ref None in
  let a_preattested = ref false in
  let b_preattested = ref false in
  let b_attested = ref false in
  let b_reproposed = ref false in
  (* Here we use custom hooks to make each node/baker behave according to
     its role in the scenario. *)
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let check_mempool_after_processing ~mempool =
      mempool_has_op_ref
        ~mempool
        ~predicate:
          (op_is_both
             (op_is_signed_by ~public_key:Mockup_simulator.bootstrap1)
             (op_is_preattestation ~level:1l ~round:0l))
        ~var:a_preattested

    let stop_on_event _ = !a_preattested
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let check_block_before_processing ~level ~round ~block_hash ~block_header
        ~(protocol_data : Protocol.Alpha_context.Block_header.protocol_data) =
      let* () =
        match (!b_attested, level, round) with
        | false, 1l, 0l ->
            (* If any of the checks fails the whole scenario will fail. *)
            let* () =
              check_block_signature
                ~block_hash
                ~block_header
                ~public_key:Mockup_simulator.bootstrap1
            in
            save_proposal_payload ~protocol_data ~var:original_proposal
        | true, 1l, 1l ->
            let* () =
              check_block_signature
                ~block_hash
                ~block_header
                ~public_key:Mockup_simulator.bootstrap2
            in
            let* () =
              verify_payload_hash
                ~protocol_data
                ~original_proposal
                ~message:"a new block proposed instead of reproposal"
            in
            b_reproposed := true ;
            return_unit
        | _ -> failwith "unexpected level = %ld / round = %ld" level round
      in
      return_unit

    let check_mempool_after_processing ~mempool =
      let* () =
        mempool_has_op_ref
          ~mempool
          ~predicate:
            (op_is_both
               (op_is_signed_by ~public_key:Mockup_simulator.bootstrap2)
               (op_is_preattestation ~level:1l ~round:0l))
          ~var:b_preattested
      in
      mempool_has_op_ref
        ~mempool
        ~predicate:
          (op_is_both
             (op_is_signed_by ~public_key:Mockup_simulator.bootstrap2)
             (op_is_preattestation ~level:1l ~round:0l))
        ~var:b_attested

    let stop_on_event _ = !b_reproposed
  end in
  let config =
    {
      default_config with
      initial_seed = None;
      delegate_selection = [(1l, [(0l, bootstrap1); (1l, bootstrap2)])];
    }
  in
  run ~config [(1, (module Node_a_hooks)); (1, (module Node_b_hooks))]

(*

Scenario T2

1. Node A should propose at the round 0, but it is dead.
2. Node B waits til it has its proposal slot at round 1 and proposes then.

*)

let test_scenario_t2 () =
  let open Lwt_result_syntax in
  let b_proposed = ref false in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let stop_on_event _ = true (* Node A stops immediately. *)
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let check_block_before_processing ~level ~round ~block_hash ~block_header
        ~protocol_data:_ =
      (* Here we test that the only block that B observes is its own
         proposal for level 1 at round 1. *)
      match (level, round) with
      | 1l, 1l ->
          let* () =
            check_block_signature
              ~block_hash
              ~block_header
              ~public_key:Mockup_simulator.bootstrap2
          in
          b_proposed := true ;
          return_unit
      | _ -> failwith "unexpected level = %ld / round = %ld" level round

    let stop_on_event _ =
      (* Stop as soon as B has proposed. This ends the test. *)
      !b_proposed
  end in
  let config =
    {
      default_config with
      initial_seed = None;
      delegate_selection = [(1l, [(0l, bootstrap1); (1l, bootstrap2)])];
    }
  in
  run ~config [(1, (module Node_a_hooks)); (1, (module Node_b_hooks))]

(*

Scenario T3

1. There are four nodes: A, B, C, and D.
2. C is the proposer at the round 0. It sends the proposal, which is
   received by all bakers except for D.
3. Due to how the messages propagate, only B sees 3 preattestations. It
   attests and locks. Other nodes all see fewer than 3 preattestations.

   A -> A and B
   B -> B
   C -> C and B

4. D proposes at the round 1. Its message reaches 3 nodes, including B.

   D -> D, B, C

5. B does not preattest because it is locked.
6. No decision is taken at the round 1.
7. B proposes at the round 2. There are no more problems with propagation of
   messages, so a decision is reached.

*)

let test_scenario_t3 () =
  let open Lwt_result_syntax in
  let b_observed_pqc = ref false in
  let original_proposal = ref None in
  let we_are_done = ref false in
  let stop_on_event0 _ = !we_are_done in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_operation ~op_hash ~op =
      if !b_observed_pqc then return (op_hash, op, [Pass; Pass; Pass; Pass])
      else
        let* is_preattestation =
          op_is_preattestation ~level:1l ~round:0l op_hash op
        in
        if is_preattestation then
          return (op_hash, op, [Pass; Pass; Block; Block])
        else failwith "unexpected operation from the node D"

    let stop_on_event = stop_on_event0
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~(protocol_data : Protocol.Alpha_context.Block_header.protocol_data) =
      match (level, round) with
      | 1l, 2l ->
          let* () =
            check_block_signature
              ~block_hash
              ~block_header
              ~public_key:Mockup_simulator.bootstrap2
          in
          we_are_done := true ;
          let* () =
            verify_payload_hash
              ~protocol_data
              ~original_proposal
              ~message:"a new block proposed instead of reproposal"
          in
          return (block_hash, block_header, operations, [Pass; Pass; Pass; Pass])
      | _ ->
          failwith
            "unexpected injection on the node B, level = %ld / round = %ld"
            level
            round

    let on_inject_operation ~op_hash ~op =
      if !b_observed_pqc then return (op_hash, op, [Pass; Pass; Pass; Pass])
      else
        let* is_preattestation =
          op_is_preattestation ~level:1l ~round:0l op_hash op
        in
        if is_preattestation then
          return (op_hash, op, [Block; Pass; Block; Block])
        else failwith "unexpected operation from the node B"

    let check_mempool_after_processing ~mempool =
      let predicate op_hash op =
        op_is_preattestation ~level:1l ~round:0l op_hash op
      in
      let* n = mempool_count_ops ~mempool ~predicate in
      if n > 3 then
        failwith "B received too many preattestations, expected to see only 3"
      else if n = 3 then (
        b_observed_pqc := true ;
        return_unit)
      else return_unit

    let stop_on_event = stop_on_event0
  end in
  let module Node_c_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~(protocol_data : Protocol.Alpha_context.Block_header.protocol_data) =
      match (level, round) with
      | 1l, 0l ->
          let* () =
            check_block_signature
              ~block_hash
              ~block_header
              ~public_key:Mockup_simulator.bootstrap3
          in
          let* () =
            save_proposal_payload ~protocol_data ~var:original_proposal
          in
          return
            (block_hash, block_header, operations, [Pass; Pass; Pass; Block])
      | _ ->
          failwith
            "unexpected injection on the node C, level = %ld / round = %ld"
            level
            round

    let on_inject_operation ~op_hash ~op =
      if !b_observed_pqc then return (op_hash, op, [Pass; Pass; Pass; Pass])
      else
        let* is_preattestation =
          op_is_preattestation ~level:1l ~round:0l op_hash op
        in
        if is_preattestation then
          return (op_hash, op, [Block; Pass; Pass; Block])
        else failwith "unexpected operation from the node C"

    let stop_on_event = stop_on_event0
  end in
  let module Node_d_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      match (level, round) with
      | 1l, 1l ->
          return
            (block_hash, block_header, operations, [Block; Pass; Pass; Pass])
      | _ ->
          failwith
            "unexpected injection on the node D, level = %ld / round = %ld"
            level
            round

    let on_inject_operation ~op_hash ~op =
      if !b_observed_pqc then return (op_hash, op, [Pass; Pass; Pass; Pass])
      else return (op_hash, op, [Block; Block; Block; Block])

    let stop_on_event = stop_on_event0
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngFtAUcm1EneHCCrxxSWAaxSukwEhSPvpTnFjVdKLEjgkapUy1pP";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap3);
              (1l, bootstrap4);
              (2l, bootstrap2);
              (3l, bootstrap1);
            ] );
          ( 2l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
        ];
    }
  in
  run
    ~config
    [
      (1, (module Node_a_hooks));
      (1, (module Node_b_hooks));
      (1, (module Node_c_hooks));
      (1, (module Node_d_hooks));
    ]

(* Scenario T4

   1. There are two nodes A B:
      A can propose at level 1, round 0 and level 2, round 1
      B can propose at level 1, round 1 and level 2, round 0
   4. Node A proposes at level 1, round 0,
   5. Both nodes preattest the proposal at level 1, round 0,
   6. Both nodes attest the proposal at level 1, round 0,
   7. Only A receives the block application at level 1, round 0,
   8. B does not propose at level 2, round 0,
   9. A proposes at level 2, round 1.
      Predecessor hash is block at level 1, round 0.
*)

let test_scenario_t4 () =
  let open Lwt_result_syntax in
  let level_to_reach = 4l in
  let a_proposal_level_1 = ref None in
  let a_proposal_level_2_predecessor = ref None in
  let b_attested_level_1 = ref false in
  let b_proposed_level_2 = ref false in

  let stop_on_event0 = function
    | Baking_state.New_head_proposal {block; _} ->
        block.shell.level >= level_to_reach
    | _ -> false
  in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let () =
        match (level, round) with
        | 1l, 0l -> a_proposal_level_1 := Some block_hash
        | 2l, 1l ->
            a_proposal_level_2_predecessor :=
              Some block_header.Block_header.shell.predecessor
        | _ -> ()
      in
      return (block_hash, block_header, operations, [Pass; Pass])

    let stop_on_event = stop_on_event0
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let on_new_head ~block_hash ~block_header =
      (* Stop notifying heads to node B for block proposed at level 1, round 0. *)
      match !a_proposal_level_1 with
      | Some a_proposal_level_1_block_hash
        when Block_hash.(a_proposal_level_1_block_hash = block_hash) ->
          Lwt.return_none
      | _ -> Lwt.return_some (block_hash, block_header)

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let () =
        match (level, round) with
        | 2l, 0l -> b_proposed_level_2 := true
        | _ -> ()
      in
      return (block_hash, block_header, operations, [Pass; Pass])

    let on_inject_operation ~op_hash ~op =
      let* is_attestation = op_is_attestation ~level:1l ~round:0l op_hash op in
      if is_attestation then b_attested_level_1 := true ;

      return (op_hash, op, [Pass; Pass; Pass; Pass])

    let check_chain_on_success ~chain:_ =
      if not !b_attested_level_1 then
        failwith "Node B did not attest proposal at level 1, round 0"
      else if
        not
        @@ Option.equal
             Block_hash.equal
             !a_proposal_level_2_predecessor
             !a_proposal_level_1
      then
        failwith
          "Invalid predecessor block for A's proposal at level 2: proposal had \
           predecessor %a, expected %a\n"
          (Format.pp_print_option Block_hash.pp_short)
          !a_proposal_level_1
          (Format.pp_print_option Block_hash.pp_short)
          !a_proposal_level_2_predecessor
      else return_unit

    let stop_on_event = stop_on_event0
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngG9pS9mbDWnz6YLUFrd8sbb9KMUzfAUMpSpnxNHY9BFnSB8L3zq";
      delegate_selection =
        [
          (1l, [(0l, bootstrap1); (1l, bootstrap2)]);
          (2l, [(0l, bootstrap2); (1l, bootstrap1)]);
        ];
    }
  in
  run ~config [(1, (module Node_a_hooks)); (1, (module Node_b_hooks))]

(*

Scenario F1

1. Node C (bootstrap3) proposes at level 1 round 0, its proposal reaches all
   nodes.
2. Propagation of preattestations happens in such a way that only Node A
   (bootstrap1) observes PQC:

   A -> A
   B -> B and A
   C -> C and A
   D -> D and A

   Node A locks.

3. At the level 1 round 1 node D (bootstrap4) proposes. Propagation of
   messages is normal.

4. Node A (bootstrap1) should propose at level 2 round 0.

*)

let test_scenario_f1 () =
  let open Lwt_result_syntax in
  let c_proposed_l1_r0 = ref false in
  let d_proposed_l1_r1 = ref false in
  let a_proposed_l2_r0 = ref false in
  let stop_on_event0 _ = !a_proposed_l2_r0 in
  let pass =
    (* This is to be sure that the proposal for round r arrives while
       the baker is in round r, and not in round r-1. (See related
       issue:
       https://gitlab.com/tezos/tezos/-/issues/4143). Otherwise, the
       test does not perform as expected. *)
    Delay 0.5
  in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      match (!c_proposed_l1_r0, !d_proposed_l1_r1, level, round) with
      | true, true, 2l, 0l ->
          let* () =
            check_block_signature
              ~block_hash
              ~block_header
              ~public_key:Mockup_simulator.bootstrap1
          in
          let* () =
            a_proposed_l2_r0 := true ;
            return_unit
          in
          return (block_hash, block_header, operations, [pass; pass; pass; pass])
      | _ ->
          failwith
            "unexpected injection on the node A, level = %ld / round = %ld"
            level
            round

    let on_inject_operation ~op_hash ~op =
      match (!c_proposed_l1_r0, !d_proposed_l1_r1) with
      | true, false -> return (op_hash, op, [Pass; Block; Block; Block])
      | _ -> return (op_hash, op, [Pass; Pass; Pass; Pass])

    let stop_on_event = stop_on_event0
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let on_inject_operation ~op_hash ~op =
      match (!c_proposed_l1_r0, !d_proposed_l1_r1) with
      | true, false -> return (op_hash, op, [Pass; Pass; Block; Block])
      | _ -> return (op_hash, op, [Pass; Pass; Pass; Pass])

    let stop_on_event = stop_on_event0
  end in
  let module Node_c_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      match (!c_proposed_l1_r0, !d_proposed_l1_r1, level, round) with
      | false, false, 1l, 0l ->
          let* () =
            check_block_signature
              ~block_hash
              ~block_header
              ~public_key:Mockup_simulator.bootstrap3
          in
          let* () =
            c_proposed_l1_r0 := true ;
            return_unit
          in
          return (block_hash, block_header, operations, [pass; pass; pass; pass])
      | _ ->
          failwith
            "unexpected injection on the node C, level = %ld / round = %ld"
            level
            round

    let on_inject_operation ~op_hash ~op =
      match (!c_proposed_l1_r0, !d_proposed_l1_r1) with
      | true, false -> return (op_hash, op, [Pass; Block; Pass; Block])
      | _ -> return (op_hash, op, [Pass; Pass; Pass; Pass])

    let stop_on_event = stop_on_event0
  end in
  let module Node_d_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      match (!d_proposed_l1_r1, level, round) with
      | false, 1l, 1l ->
          let* () =
            check_block_signature
              ~block_hash
              ~block_header
              ~public_key:Mockup_simulator.bootstrap4
          in
          let* () =
            d_proposed_l1_r1 := true ;
            return_unit
          in
          return (block_hash, block_header, operations, [pass; pass; pass; pass])
      | _ ->
          failwith
            "unexpected injection on the node D, level = %ld / round = %ld"
            level
            round

    let on_inject_operation ~op_hash ~op =
      match (!c_proposed_l1_r0, !d_proposed_l1_r1) with
      | true, false -> return (op_hash, op, [Pass; Block; Block; Pass])
      | _ -> return (op_hash, op, [Pass; Pass; Pass; Pass])

    let stop_on_event = stop_on_event0
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngGohKUZjXzv69sxvDqAYRd4XPDQSxDoEpP72znu2jduBuhcYiSE";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap3);
              (1l, bootstrap4);
              (2l, bootstrap1);
              (3l, bootstrap2);
            ] );
          ( 2l,
            [
              (0l, bootstrap1);
              (1l, bootstrap4);
              (2l, bootstrap2);
              (3l, bootstrap3);
            ] );
        ];
      timeout = 30;
    }
  in
  run
    ~config
    [
      (1, (module Node_a_hooks));
      (1, (module Node_b_hooks));
      (1, (module Node_c_hooks));
      (1, (module Node_d_hooks));
    ]

(*

Scenario F2

1. There are four nodes: A, B, C, and D.
2. A proposes at 1.0 and observes EQC.
3. A has the slot at 2.0 but somehow it doesn't propose or its proposal is lost.
4. B, C, and D have the rounds 1, 2, and 3 respectively, but they also do not propose.
5. A should still propose at 2.4.

*)

let test_scenario_f2 () =
  let open Lwt_result_syntax in
  let proposal_2_4_observed = ref false in
  let module Hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let propagation_vector =
        match (level, round) with
        | 1l, 0l -> [Pass; Pass; Pass; Pass]
        | 2l, 0l -> [Pass; Block; Block; Block]
        | 2l, 4l ->
            proposal_2_4_observed := true ;
            [Pass; Pass; Pass; Pass]
        | _ -> [Block; Block; Block; Block]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let stop_on_event _ = !proposal_2_4_observed
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngGPSm87ZqWxJmZu7rewiLiyKY72ffCQQvxDuWmFBw59dWAL5VTB";
      delegate_selection =
        [
          (1l, [(0l, bootstrap1)]);
          ( 2l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
              (4l, bootstrap1);
            ] );
        ];
      timeout = 60;
      round0 = 2L;
      round1 = 3L;
    }
  in
  run
    ~config
    [
      (1, (module Hooks));
      (1, (module Hooks));
      (1, (module Hooks));
      (1, (module Hooks));
    ]

(*

Scenario M1

1. Four nodes start, each with 1 delegate.
2. As soon as 2nd level is proposed all communication between nodes becomes
   impossible.
3. The situation continues for 5 seconds.
4. After communication is resumed the bakers must continue making progress.

*)

let test_scenario_m1 () =
  let open Lwt_result_syntax in
  let observed_level2_timestamp = ref None in
  let network_down_sec = 5. in
  let module Hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let propagation_vector =
        match !observed_level2_timestamp with
        | None ->
            if Compare.Int32.(level >= 2l) then (
              observed_level2_timestamp := Some (Unix.time ()) ;
              [Pass; Pass; Pass; Pass])
            else [Pass; Pass; Pass; Pass]
        | Some level2_observed ->
            if Unix.time () -. level2_observed < network_down_sec then
              [Block; Block; Block; Block]
            else [Pass; Pass; Pass; Pass]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let on_inject_operation ~op_hash ~op =
      let propagation_vector =
        match !observed_level2_timestamp with
        | None -> [Pass; Pass; Pass; Pass]
        | Some level2_observed ->
            if Unix.time () -. level2_observed < network_down_sec then
              [Block; Block; Block; Block]
            else [Pass; Pass; Pass; Pass]
      in
      return (op_hash, op, propagation_vector)

    let stop_on_event = function
      | Baking_state.New_head_proposal {block; _} -> block.shell.level > 4l
      | _ -> false
  end in
  let config = {default_config with timeout = 60} in
  run
    ~config
    [
      (1, (module Hooks));
      (1, (module Hooks));
      (1, (module Hooks));
      (1, (module Hooks));
    ]

(*

Scenario M2

1. Five nodes start (single delegate per node).
2. They decide level 1.
3. However, the node that has the slot for level 2 round 0 is not there
   to participate.
4. We check that the chain continues advancing despite that.

*)

let test_scenario_m2 () =
  let module Normal_node : Hooks = struct
    include Default_hooks

    let stop_on_event = function
      | Baking_state.New_head_proposal {block; _} -> block.shell.level > 5l
      | _ -> false
  end in
  let module Missing_node : Hooks = struct
    include Default_hooks

    let stop_on_event _ = true (* stop immediately *)
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngGo77zNC59bYiQMk2M14aDZZu4KXG8BV1C8pi7afjJ7cXyqB3M1";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
          ( 2l,
            [
              (0l, bootstrap5);
              (1l, bootstrap1);
              (2l, bootstrap2);
              (3l, bootstrap3);
              (4l, bootstrap4);
            ] );
        ];
      round0 = 2L;
      round1 = 3L;
      timeout = 60;
    }
  in
  run
    ~config
    [
      (1, (module Normal_node));
      (1, (module Normal_node));
      (1, (module Normal_node));
      (1, (module Normal_node));
      (1, (module Missing_node));
    ]

(*

Scenario M3

1. There are four nodes: A, B, C, and D.
2. A and B propose in turns. Messages from A reach every node, but messages
   from other nodes only go to A.
3. The chain should not make progress. Since we have both bootstrap1 and
   bootstrap2 in delegate selection they have equal voting power. Therefore
   it is necessary to have 2 votes for prequorums (which is achieved when A
   is proposing) and 2 votes for quorums (impossible because B has no way to
   obtain PQC and thus cannot send attestations).

*)

let test_scenario_m3 () =
  let open Lwt_result_syntax in
  let stop_on_event0 = function
    | Baking_state.New_head_proposal {block; _} ->
        block.shell.level = 1l
        && Protocol.Alpha_context.Round.to_int32 block.round = 6l
    | _ -> false
  in

  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let stop_on_event = stop_on_event0
  end in
  let module Other_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      return (block_hash, block_header, operations, [Pass; Block; Block; Block])

    let on_inject_operation ~op_hash ~op =
      return (op_hash, op, [Pass; Block; Block; Block])

    let stop_on_event = stop_on_event0
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngGaxNJcwEVJLgQXmnN8KN5skn6fhU4Awtu8zVDKViTd5gsfT51M";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap1);
              (3l, bootstrap2);
              (4l, bootstrap1);
              (5l, bootstrap2);
              (6l, bootstrap1);
            ] );
        ];
      round0 = 2L;
      round1 = 3L;
      timeout = 60;
    }
  in
  run
    ~config
    [
      (1, (module Node_a_hooks));
      (1, (module Other_hooks));
      (1, (module Other_hooks));
      (1, (module Other_hooks));
    ]

(*

Scenario M4

1. There are four bakers: A, B, C, and D.
2. A proposes at level 1 round 0. Its proposal reaches A, B, C, and D, but
   with a delay of 0.5 seconds.
3. 3 votes are enough for consensus, because voting powers of all delegates
   are equal. Preattestations propagate freely, however attestations from C
   are blocked.
4. Check that at level 1 round 0 quorum is reached (from the point of view
   of A). This means that D sends an attestation despite receiving
   preattestations before the proposal.

*)

let test_scenario_m4 () =
  let open Lwt_result_syntax in
  let a_observed_qc = ref false in
  let stop_on_event0 _ = !a_observed_qc in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      match (level, round) with
      | 1l, 0l ->
          let* () =
            check_block_signature
              ~block_hash
              ~block_header
              ~public_key:Mockup_simulator.bootstrap1
          in
          return
            (block_hash, block_header, operations, [Pass; Pass; Pass; Delay 0.5])
      | _ ->
          failwith
            "unexpected injection on the node A, level = %ld / round = %ld"
            level
            round

    let check_mempool_after_processing ~mempool =
      let predicate op_hash op =
        op_is_attestation ~level:1l ~round:0l op_hash op
      in
      let* n = mempool_count_ops ~mempool ~predicate in
      if n > 3 then
        failwith "A received too many attestations, expected to see only 3"
      else if n = 3 then (
        a_observed_qc := true ;
        return_unit)
      else return_unit

    let stop_on_event = stop_on_event0
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let stop_on_event = stop_on_event0
  end in
  let module Node_c_hooks : Hooks = struct
    include Default_hooks

    let on_inject_operation ~op_hash ~op =
      let* is_attestation = op_is_attestation ~level:1l ~round:0l op_hash op in
      return
        ( op_hash,
          op,
          if is_attestation then [Block; Block; Block; Block]
          else [Pass; Pass; Pass; Pass] )

    let stop_on_event = stop_on_event0
  end in
  let module Node_d_hooks : Hooks = struct
    include Default_hooks

    let stop_on_event = stop_on_event0
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngGJmwLi7kPvGwV2LR3kjNQ6xamGPCZ9ooep9QcafbqRXZhYEciT";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
        ];
    }
  in
  run
    ~config
    [
      (1, (module Node_a_hooks));
      (1, (module Node_b_hooks));
      (1, (module Node_c_hooks));
      (1, (module Node_d_hooks));
    ]

(*

Scenario M5

1. There are four bakers: A, B, C, and D.
2. A proposes at level 1 round 0. Its proposal reaches A, B, C, and D, but with
   a delay of 1 second. There are no problems with propagation of
   preattestations and attestations.
3. At the level 1 all four bakers have proposer slots, however we block possible
   proposals from B and C at higher rounds.
4. Check that D proposes at the level 2 round 0, which means that it has
   observed QC.

*)

let test_scenario_m5 () =
  let open Lwt_result_syntax in
  let stop_on_event0 = function
    | Baking_state.New_head_proposal {block; _} -> block.shell.level >= 2l
    | _ -> false
  in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      match (level, round) with
      | 1l, 0l ->
          let* () =
            check_block_signature
              ~block_hash
              ~block_header
              ~public_key:Mockup_simulator.bootstrap1
          in
          return
            (block_hash, block_header, operations, [Pass; Pass; Pass; Delay 1.0])
      | _ ->
          failwith
            "unexpected injection on the node A, level = %ld / round = %ld"
            level
            round

    let stop_on_event = stop_on_event0
  end in
  let module Other_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      return (block_hash, block_header, operations, [Block; Block; Block; Block])

    let stop_on_event = stop_on_event0
  end in
  let module Node_d_hooks : Hooks = struct
    include Default_hooks

    let stop_on_event = stop_on_event0
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngGJmwLi7kPvGwV2LR3kjNQ6xamGPCZ9ooep9QcafbqRXZhYEciT";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
        ];
      round0 = 3L;
      round1 = 4L;
    }
  in
  run
    ~config
    [
      (1, (module Node_a_hooks));
      (1, (module Other_hooks));
      (1, (module Other_hooks));
      (1, (module Node_d_hooks));
    ]

(*

Scenario M6

1. There are four bakers: A, B, C, and D.
2. A proposes at level 1 round 0. Its proposal reaches all nodes, and they
   observe PQC. Only A observes a QC.
3. At level 1 round 1 it is B's turn to propose. Since it has observed the
   PQC, it reproposes A's proposal. A does not see it.
4. B observes PQC and QC for its proposal.
5. A proposes at level 2 round 0. No one sees the proposal.
6. B proposes at level 2 round 1. A sees B's proposal and switches its branch.
7. We wait 2 more levels before checking A's chain to verify that it has
   adopted B's proposal.

*)

let test_scenario_m6 () =
  let open Lwt_result_syntax in
  let b_proposal_2_1 = ref None in
  let stop_on_event0 = function
    | Baking_state.New_head_proposal {block; _} -> block.shell.level > 4l
    | _ -> false
  in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let propagation_vector =
        match (level, round) with
        | 2l, 0l -> [Pass; Block; Block; Block]
        | _ -> [Pass; Pass; Pass; Pass]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let on_inject_operation ~op_hash ~op =
      let* is_a10_attestation =
        op_is_attestation ~level:1l ~round:0l op_hash op
      in
      return
        ( op_hash,
          op,
          if is_a10_attestation then [Pass; Block; Block; Block]
          else [Pass; Pass; Pass; Pass] )

    let stop_on_event = stop_on_event0

    let check_chain_on_success ~chain =
      match List.nth (List.rev chain) 2 with
      | None -> failwith "Node A has empty chain"
      | Some (block : block) ->
          verify_payload_hash
            ~protocol_data:block.protocol_data
            ~original_proposal:b_proposal_2_1
            ~message:"A did not switch to B's proposal (level 2, round 1)"
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data =
      let* propagation_vector =
        match (level, round) with
        | 1l, 1l -> return [Block; Delay 0.1; Delay 0.1; Delay 0.1]
        | 2l, 1l ->
            let* () =
              save_proposal_payload ~protocol_data ~var:b_proposal_2_1
            in
            return [Pass; Pass; Pass; Pass]
        | _ -> return [Pass; Pass; Pass; Pass]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let on_inject_operation ~op_hash ~op =
      let* is_a10_attestation =
        op_is_attestation ~level:1l ~round:0l op_hash op
      in
      return
        ( op_hash,
          op,
          if is_a10_attestation then [Pass; Block; Block; Block]
          else [Pass; Pass; Pass; Pass] )

    let stop_on_event = stop_on_event0
  end in
  let module Other_node : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      return (block_hash, block_header, operations, [Pass; Pass; Pass; Pass])

    let on_inject_operation ~op_hash ~op =
      let* is_a10_attestation =
        op_is_attestation ~level:1l ~round:0l op_hash op
      in
      return
        ( op_hash,
          op,
          if is_a10_attestation then [Pass; Block; Block; Block]
          else [Pass; Pass; Pass; Pass] )

    let stop_on_event = stop_on_event0
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngGnwG2gApiRzo1kdbCgQheqtZroUsAjsJzyw2RBbtg3gtTeMQ9F";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
          ( 2l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
        ];
      timeout = 60;
    }
  in
  run
    ~config
    [
      (1, (module Node_a_hooks));
      (1, (module Node_b_hooks));
      (1, (module Other_node));
      (1, (module Other_node));
    ]

(*

Scenario M7

The same as M6, but:

5. B proposes at level 2 round 0 (A does not see the proposal).
6. A proposes at 2.1. B switches to A's branch when it receives 2.1.
7. We wait 2 more levels before checking everyone's chain to verify that
   A's proposal has been selected.

*)

let test_scenario_m7 () =
  let open Lwt_result_syntax in
  let a_proposal_2_1 = ref None in
  let c_received_2_1 = ref false in
  let d_received_2_1 = ref false in
  let stop_on_event0 = function
    | Baking_state.New_head_proposal {block; _} -> block.shell.level > 4l
    | _ -> false
  in
  let check_chain_on_success0 node_label ~chain =
    match List.nth (List.rev chain) 2 with
    | None -> failwith "Node %s has empty chain" node_label
    | Some (block : block) ->
        verify_payload_hash
          ~protocol_data:block.protocol_data
          ~original_proposal:a_proposal_2_1
          ~message:
            (Format.sprintf
               "%s did not switch to A's proposal (level 2, round 1)"
               node_label)
  in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data =
      let* () =
        match (level, round) with
        | 2l, 1l -> save_proposal_payload ~protocol_data ~var:a_proposal_2_1
        | _ -> return_unit
      in
      return (block_hash, block_header, operations, [Pass; Pass; Pass; Pass])

    let on_inject_operation ~op_hash ~op =
      let* is_a10_attestation =
        op_is_attestation ~level:1l ~round:0l op_hash op
      in
      return
        ( op_hash,
          op,
          if is_a10_attestation then [Pass; Block; Block; Block]
          else [Pass; Pass; Pass; Pass] )

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "A"
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let* propagation_vector =
        match (level, round) with
        | 1l, 1l -> return [Block; Delay 0.1; Delay 0.1; Delay 0.1]
        | 2l, 0l -> return [Block; Pass; Pass; Pass]
        | _ -> return [Pass; Pass; Pass; Pass]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let on_inject_operation ~op_hash ~op =
      let* is_a10_attestation =
        op_is_attestation ~level:1l ~round:0l op_hash op
      in
      let* level2_preattestation = op_is_preattestation ~level:2l op_hash op in
      let* level2_attestation = op_is_attestation ~level:2l op_hash op in
      let propagation_vector =
        match
          (is_a10_attestation, level2_preattestation, level2_attestation)
        with
        | true, _, _ -> [Pass; Block; Block; Block]
        | _, true, _ | _, _, true -> [Block; Block; Block; Block]
        | _, _, _ -> [Pass; Pass; Pass; Pass]
      in
      return (op_hash, op, propagation_vector)

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "B"
  end in
  let module Node_c_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let propagation_vector =
        if !c_received_2_1 then [Pass; Pass; Pass; Pass]
        else [Block; Block; Block; Block]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let check_chain_after_processing ~level ~round ~chain:_ =
      match (level, round) with
      | 2l, 1l ->
          c_received_2_1 := true ;
          return_unit
      | _ -> return_unit

    let on_inject_operation ~op_hash ~op =
      let* is_a10_attestation =
        op_is_attestation ~level:1l ~round:0l op_hash op
      in
      let* level2_preattestation = op_is_preattestation ~level:2l op_hash op in
      let* level2_attestation = op_is_attestation ~level:2l op_hash op in
      let propagation_vector =
        match
          ( is_a10_attestation,
            !c_received_2_1,
            level2_preattestation,
            level2_attestation )
        with
        | true, _, _, _ -> [Pass; Block; Block; Block]
        | _, false, true, _ | _, false, _, true -> [Block; Block; Block; Block]
        | _, _, _, _ -> [Pass; Pass; Pass; Pass]
      in
      return (op_hash, op, propagation_vector)

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "C"
  end in
  let module Node_d_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let propagation_vector =
        if !d_received_2_1 then [Pass; Pass; Pass; Pass]
        else [Block; Block; Block; Block]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let check_chain_after_processing ~level ~round ~chain:_ =
      match (level, round) with
      | 2l, 1l ->
          d_received_2_1 := true ;
          return_unit
      | _ -> return_unit

    let on_inject_operation ~op_hash ~op =
      let* is_a10_attestation =
        op_is_attestation ~level:1l ~round:0l op_hash op
      in
      let* level2_preattestation = op_is_preattestation ~level:2l op_hash op in
      let* level2_attestation = op_is_attestation ~level:2l op_hash op in
      let propagation_vector =
        match
          ( is_a10_attestation,
            !d_received_2_1,
            level2_preattestation,
            level2_attestation )
        with
        | true, _, _, _ -> [Pass; Block; Block; Block]
        | _, false, true, _ | _, false, _, true -> [Block; Block; Block; Block]
        | _, _, _, _ -> [Pass; Pass; Pass; Pass]
      in
      return (op_hash, op, propagation_vector)

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "D"
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngGJ7ReXwsjWuzpeqCgHAjudFwJtxdYz44Genz1FnyJ8R226hoKh";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
          ( 2l,
            [
              (0l, bootstrap2);
              (1l, bootstrap1);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
        ];
      timeout = 60;
    }
  in
  run
    ~config
    [
      (1, (module Node_a_hooks));
      (1, (module Node_b_hooks));
      (1, (module Node_c_hooks));
      (1, (module Node_d_hooks));
    ]

(*

Scenario M8

5. B proposes at 2.0 and observes PQC but not QC.
6. C re-proposes at 2.1 and similarly observes PQC but not QC.
7. A proposes at 2.2. B, C, and D do not switch to A's branch; moreover A
   switches to their branch when it receives the next proposal (2.3). This
   happens because B, C, and D have PQC despite A having a higher round (2 > 1).
8. We wait 2 more levels before checking everyone's chain to verify that
   B's proposal has been selected.

*)

let test_scenario_m8 () =
  let open Lwt_result_syntax in
  let b_proposal_2_0 = ref None in
  let stop_on_event0 = function
    | Baking_state.New_head_proposal {block; _} -> block.shell.level > 4l
    | _ -> false
  in
  let on_inject_operation0 ~op_hash ~op =
    let* is_a10_attestation =
      op_is_attestation ~level:1l ~round:0l op_hash op
    in
    let* is_b20_attestation =
      op_is_attestation ~level:2l ~round:0l op_hash op
    in
    let* is_c21_attestation =
      op_is_attestation ~level:2l ~round:1l op_hash op
    in
    let propagation_vector =
      if is_a10_attestation then [Pass; Block; Block; Block]
      else if is_b20_attestation || is_c21_attestation then
        [Block; Block; Block; Block]
      else [Pass; Pass; Pass; Pass]
    in
    return (op_hash, op, propagation_vector)
  in
  let check_chain_on_success0 node_label ~chain =
    match List.nth (List.rev chain) 2 with
    | None -> failwith "Node %s has empty chain" node_label
    | Some (block : block) ->
        verify_payload_hash
          ~protocol_data:block.protocol_data
          ~original_proposal:b_proposal_2_0
          ~message:
            (Format.sprintf
               "%s did not switch to B's proposal (level 2, round 0)"
               node_label)
  in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      return (block_hash, block_header, operations, [Pass; Pass; Pass; Pass])

    let on_inject_operation = on_inject_operation0

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "A"
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data =
      let* propagation_vector =
        match (level, round) with
        | 1l, 1l -> return [Block; Delay 0.1; Delay 0.1; Delay 0.1]
        | 2l, 0l ->
            let* () =
              save_proposal_payload ~protocol_data ~var:b_proposal_2_0
            in
            return [Block; Pass; Pass; Pass]
        | _ -> return [Pass; Pass; Pass; Pass]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let on_inject_operation = on_inject_operation0

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "B"
  end in
  let module Node_c_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      let propagation_vector =
        match (level, round) with
        | 2l, 1l -> [Block; Pass; Pass; Pass]
        | _ -> [Pass; Pass; Pass; Pass]
      in
      return (block_hash, block_header, operations, propagation_vector)

    let on_inject_operation = on_inject_operation0

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "C"
  end in
  let module Node_d_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      return (block_hash, block_header, operations, [Pass; Pass; Pass; Pass])

    let on_inject_operation = on_inject_operation0

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "D"
  end in
  let config =
    {
      default_config with
      initial_seed =
        some_seed "rngFy2zFmgg25SXrE6aawqQVhD1kdw9eCCRxc843RLQjz5MZ6MGER";
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
          ( 2l,
            [
              (0l, bootstrap2);
              (1l, bootstrap3);
              (2l, bootstrap1);
              (3l, bootstrap4);
            ] );
        ];
      timeout = 60;
    }
  in
  run
    ~config
    [
      (1, (module Node_a_hooks));
      (1, (module Node_b_hooks));
      (1, (module Node_c_hooks));
      (1, (module Node_d_hooks));
    ]

(*
  Scenario M9

  Two nodes: A, B

  1. L1 - A proposes and reaches QC.
  2. L1 - When QC is reached, observe that B emits time to forge event. Shortly after, observe
          that B emits time to bake event.
  3. L2 - A observes the block from B and ends.

*)
let test_scenario_m9 () =
  let stop_level = Int32.of_int 2 in
  let node_b_qc = ref false in
  let node_b_ttf = ref false in
  let node_b_level = ref Int32.zero in
  let bh : Block_hash.t option ref = ref None in
  let stop_on_event0 = function
    | Baking_state.New_head_proposal {block; _} ->
        block.shell.level >= stop_level
    | _ -> false
  in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let raise_error : string option ref = ref None

    let block_round block_header =
      let open Block_header in
      match
        Protocol.Alpha_context.Fitness.round_from_raw block_header.shell.fitness
      with
      | Error _ -> assert false
      | Ok x -> x

    let on_new_head ~block_hash ~block_header =
      let open Block_header in
      let block_round = block_round block_header in
      let level = block_header.shell.level in
      let is_round0 =
        Protocol.Alpha_context.Round.equal
          block_round
          Protocol.Alpha_context.Round.zero
      in
      match level with
      | 2l ->
          raise_error :=
            if
              is_round0
              && not (Block_hash.equal block_hash (Stdlib.Option.get !bh))
            then Some "Block hash was not equal"
            else if not is_round0 then
              Some "Level 2 expected to have a block at round 0"
            else None ;
          Lwt.return @@ Some (block_hash, block_header)
      | _ -> Lwt.return @@ Some (block_hash, block_header)

    let stop_on_event = stop_on_event0

    let check_chain_on_success ~chain:_ =
      match !raise_error with
      | Some err -> Stdlib.failwith err
      | _ -> return_unit
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      bh := Some block_hash ;
      return (block_hash, block_header, operations, [Pass; Pass])

    let stop_on_event = function
      | Baking_state.Quorum_reached _ when !node_b_level = 1l ->
          node_b_qc := true ;
          false
      | Baking_state.Timeout timeout when !node_b_level = 1l -> (
          match timeout with
          | Time_to_prepare_next_level_block _ ->
              if !node_b_qc && !node_b_ttf then false
              else
                Stdlib.failwith
                  "time to bake emitted without observing qc or time to forge \
                   event"
          | End_of_round _ ->
              Stdlib.failwith "End of round timeout not expected")
      | Baking_state.New_valid_proposal {block; _} ->
          node_b_level := block.shell.level ;
          false
      | event -> stop_on_event0 event
  end in
  let config =
    {
      default_config with
      delegate_selection = [(1l, [(0l, bootstrap1)]); (2l, [(0l, bootstrap2)])];
      round0 = 3L;
      round1 = 4L;
    }
  in
  run ~config [(1, (module Node_a_hooks)); (1, (module Node_b_hooks))]

(*
   Scenario M10

   Two nodes : A, B

   1. Node A is the proposer at level 1, round 0, but is dead
   2. Node B proposes at level 1, round 1, therefore the
      Time_to_forge_block was not called.
*)
let test_scenario_m10 () =
  let stop_level = 1l in
  let stop_round = Protocol.Alpha_context.Round.(succ zero) in
  let node_b_ttf = ref false in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let stop_on_event _ = true (* Node A stops immediately. *)
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let stop_on_event = function
      | Baking_state.Timeout (Time_to_prepare_next_level_block _) ->
          node_b_ttf := true ;
          false
      (* When we get to level = 1, round = 1, the time to forge timeout should
         not have been called *)
      | Baking_state.New_head_proposal {block; _} ->
          let block_round = block.round in
          if block.shell.level >= stop_level && block_round = stop_round then (
            assert (not !node_b_ttf) ;
            true)
          else false
      | _ -> false
  end in
  let config =
    {
      default_config with
      round0 = 3L;
      round1 = 4L;
      delegate_selection = [(1l, [(0l, bootstrap1); (1l, bootstrap2)])];
    }
  in
  run ~config [(1, (module Node_a_hooks)); (1, (module Node_b_hooks))]

let () =
  let open Lwt_result_syntax in
  (* Activate a sink to record baker's events *)
  let t = lazy (Tezt_sink.activate ()) in
  let proto_name =
    String.lowercase_ascii Protocol.name
    |> String.map (function '-' -> '_' | x -> x)
  in
  let register_test (title, test) =
    Test.register
      ~__FILE__
      ~title
      ~tags:[proto_name; "baker"; "mockup"; Tag.time_sensitive]
    @@ fun () ->
    let*! () = Lazy.force t in
    let*! r = test () in
    match r with
    | Ok () -> unit
    | Error errs -> Test.fail ~__LOC__ "%a" pp_print_trace errs
  in
  List.iter
    register_test
    [
      (Protocol.name ^ ": reaches level 5", test_level_5);
      ( Protocol.name ^ ": cannot progress without new head",
        test_preattest_on_valid );
      (Protocol.name ^ ": reset delayed pqc", test_reset_delayed_pqc);
      (Protocol.name ^ ": scenario t1", test_scenario_t1);
      (Protocol.name ^ ": scenario t2", test_scenario_t2);
      (Protocol.name ^ ": scenario t3", test_scenario_t3);
      (Protocol.name ^ ": scenario t4", test_scenario_t4);
      (Protocol.name ^ ": scenario f1", test_scenario_f1);
      (Protocol.name ^ ": scenario f2", test_scenario_f2);
      (Protocol.name ^ ": scenario m1", test_scenario_m1);
      (Protocol.name ^ ": scenario m2", test_scenario_m2);
      (Protocol.name ^ ": scenario m3", test_scenario_m3);
      (Protocol.name ^ ": scenario m4", test_scenario_m4);
      (Protocol.name ^ ": scenario m5", test_scenario_m5);
      (Protocol.name ^ ": scenario m6", test_scenario_m6);
      (Protocol.name ^ ": scenario m7", test_scenario_m7);
      (Protocol.name ^ ": scenario m8", test_scenario_m8);
      (Protocol.name ^ ": scenario m9", test_scenario_m9);
      (Protocol.name ^ ": scenario m10", test_scenario_m10);
    ]
