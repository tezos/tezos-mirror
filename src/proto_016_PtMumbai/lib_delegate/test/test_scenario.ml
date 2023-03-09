(* Testing
   -------
   Component:    Protocol, delegate
   Invocation:   dune exec src/proto_016_PtMumbai/lib_delegate/test/main.exe
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
        get_block_round block >>=? fun round ->
        if Int32.equal round 0l then return ()
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

let test_preendorse_on_valid () =
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
          (* Ensure that we never see a QC on the seen candidate. *)
          (match !seen_candidate with
          | Some seen_candidate
            when Block_hash.(candidate.hash = seen_candidate) ->
              Stdlib.failwith "Quorum occured on the seen candidate"
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
      return_unit
  end in
  let config = {default_config with timeout = 10} in
  run ~config [(1, (module Hooks))]

let test_reset_delayed_pqc () =
  let module Hooks : Hooks = struct
    include Default_hooks

    let should_wait = ref true

    let trigger = ref false

    let on_new_operation x =
      (if !should_wait then Lwt_unix.sleep 0.5 else Lwt_unix.sleep 0.2)
      >>= fun () ->
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
        Lwt_unix.sleep 1. >>= fun () ->
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
      else return_unit
  end in
  let config = {default_config with round0 = 2L; round1 = 3L; timeout = 50} in
  run ~config [(1, (module Hooks))]

(*

Scenario T1

1. Node A proposes at the round 0.
2. Both node A and node B preendorse.
3. Node A stops.
4. Node B endorses in the round 0 and locks. No decision is taken at the
   round 0 because A did not endorse.
5. We check that in round 1 (the next slot for B), B proposes the same
   value as A proposed in the round 0, not a new proposal.
*)

let test_scenario_t1 () =
  let original_proposal = ref None in
  let a_preendorsed = ref false in
  let b_preendorsed = ref false in
  let b_endorsed = ref false in
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
             (op_is_preendorsement ~level:1l ~round:0l))
        ~var:a_preendorsed

    let stop_on_event _ = !a_preendorsed
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let check_block_before_processing ~level ~round ~block_hash ~block_header
        ~(protocol_data : Protocol.Alpha_context.Block_header.protocol_data) =
      (match (!b_endorsed, level, round) with
      | false, 1l, 0l ->
          (* If any of the checks fails the whole scenario will fail. *)
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap1
          >>=? fun () ->
          save_proposal_payload ~protocol_data ~var:original_proposal
      | true, 1l, 1l ->
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap2
          >>=? fun () ->
          verify_payload_hash
            ~protocol_data
            ~original_proposal
            ~message:"a new block proposed instead of reproposal"
          >>=? fun () ->
          b_reproposed := true ;
          return_unit
      | _ -> failwith "unexpected level = %ld / round = %ld" level round)
      >>=? fun () -> return_unit

    let check_mempool_after_processing ~mempool =
      mempool_has_op_ref
        ~mempool
        ~predicate:
          (op_is_both
             (op_is_signed_by ~public_key:Mockup_simulator.bootstrap2)
             (op_is_preendorsement ~level:1l ~round:0l))
        ~var:b_preendorsed
      >>=? fun () ->
      mempool_has_op_ref
        ~mempool
        ~predicate:
          (op_is_both
             (op_is_signed_by ~public_key:Mockup_simulator.bootstrap2)
             (op_is_preendorsement ~level:1l ~round:0l))
        ~var:b_endorsed

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
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap2
          >>=? fun () ->
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
3. Due to how the messages propagate, only B sees 3 preendorsements. It
   endorses and locks. Other nodes all see fewer than 3 preendorsements.

   A -> A and B
   B -> B
   C -> C and B

4. D proposes at the round 1. Its message reaches 3 nodes, including B.

   D -> D, B, C

5. B does not preendorse because it is locked.
6. No decision is taken at the round 1.
7. B proposes at the round 2. There are no more problems with propagation of
   messages, so a decision is reached.

*)

let test_scenario_t3 () =
  let b_observed_pqc = ref false in
  let original_proposal = ref None in
  let we_are_done = ref false in
  let stop_on_event0 _ = !we_are_done in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_operation ~op_hash ~op =
      if !b_observed_pqc then return (op_hash, op, [Pass; Pass; Pass; Pass])
      else
        op_is_preendorsement ~level:1l ~round:0l op_hash op
        >>=? fun is_preendorsement ->
        if is_preendorsement then
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
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap2
          >>=? fun () ->
          we_are_done := true ;
          verify_payload_hash
            ~protocol_data
            ~original_proposal
            ~message:"a new block proposed instead of reproposal"
          >>=? fun () ->
          return (block_hash, block_header, operations, [Pass; Pass; Pass; Pass])
      | _ ->
          failwith
            "unexpected injection on the node B, level = %ld / round = %ld"
            level
            round

    let on_inject_operation ~op_hash ~op =
      if !b_observed_pqc then return (op_hash, op, [Pass; Pass; Pass; Pass])
      else
        op_is_preendorsement ~level:1l ~round:0l op_hash op
        >>=? fun is_preendorsement ->
        if is_preendorsement then
          return (op_hash, op, [Block; Pass; Block; Block])
        else failwith "unexpected operation from the node B"

    let check_mempool_after_processing ~mempool =
      let predicate op_hash op =
        op_is_preendorsement ~level:1l ~round:0l op_hash op
      in
      mempool_count_ops ~mempool ~predicate >>=? fun n ->
      if n > 3 then
        failwith "B received too many preendorsements, expected to see only 3"
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
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap3
          >>=? fun () ->
          save_proposal_payload ~protocol_data ~var:original_proposal
          >>=? fun () ->
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
        op_is_preendorsement ~level:1l ~round:0l op_hash op
        >>=? fun is_preendorsement ->
        if is_preendorsement then
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

(*

Scenario F1

1. Node C (bootstrap3) proposes at level 1 round 0, its proposal reaches all
   nodes.
2. Propagation of preendorsements happens in such a way that only Node A
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
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap1
          >>=? fun () ->
          (a_proposed_l2_r0 := true ;
           return_unit)
          >>=? fun () ->
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
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap3
          >>=? fun () ->
          (c_proposed_l1_r0 := true ;
           return_unit)
          >>=? fun () ->
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
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap4
          >>=? fun () ->
          (d_proposed_l1_r1 := true ;
           return_unit)
          >>=? fun () ->
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
      debug = true;
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
   obtain PQC and thus cannot send endorsements).

*)

let test_scenario_m3 () =
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
   are equal. Preendorsements propagate freely, however endorsements from C
   are blocked.
4. Check that at level 1 round 0 quorum is reached (from the point of view
   of A). This means that D sends an endorsement despite receiving
   preendorsements before the proposal.

*)

let test_scenario_m4 () =
  let a_observed_qc = ref false in
  let stop_on_event0 _ = !a_observed_qc in
  let module Node_a_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      match (level, round) with
      | 1l, 0l ->
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap1
          >>=? fun () ->
          return
            (block_hash, block_header, operations, [Pass; Pass; Pass; Delay 0.5])
      | _ ->
          failwith
            "unexpected injection on the node A, level = %ld / round = %ld"
            level
            round

    let check_mempool_after_processing ~mempool =
      let predicate op_hash op =
        op_is_endorsement ~level:1l ~round:0l op_hash op
      in
      mempool_count_ops ~mempool ~predicate >>=? fun n ->
      if n > 3 then
        failwith "A received too many endorsements, expected to see only 3"
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
      op_is_endorsement ~level:1l ~round:0l op_hash op
      >>=? fun is_endorsement ->
      return
        ( op_hash,
          op,
          if is_endorsement then [Block; Block; Block; Block]
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
   preendorsements and endorsements.
3. At the level 1 all four bakers have proposer slots, however we block possible
   proposals from B and C at higher rounds.
4. Check that D proposes at the level 2 round 0, which means that it has
   observed QC.

*)

let test_scenario_m5 () =
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
          check_block_signature
            ~block_hash
            ~block_header
            ~public_key:Mockup_simulator.bootstrap1
          >>=? fun () ->
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
      op_is_endorsement ~level:1l ~round:0l op_hash op
      >>=? fun is_a10_endorsement ->
      return
        ( op_hash,
          op,
          if is_a10_endorsement then [Pass; Block; Block; Block]
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
      (match (level, round) with
      | 1l, 1l -> return [Block; Delay 0.1; Delay 0.1; Delay 0.1]
      | 2l, 1l ->
          save_proposal_payload ~protocol_data ~var:b_proposal_2_1
          >>=? fun () -> return [Pass; Pass; Pass; Pass]
      | _ -> return [Pass; Pass; Pass; Pass])
      >>=? fun propagation_vector ->
      return (block_hash, block_header, operations, propagation_vector)

    let on_inject_operation ~op_hash ~op =
      op_is_endorsement ~level:1l ~round:0l op_hash op
      >>=? fun is_a10_endorsement ->
      return
        ( op_hash,
          op,
          if is_a10_endorsement then [Pass; Block; Block; Block]
          else [Pass; Pass; Pass; Pass] )

    let stop_on_event = stop_on_event0
  end in
  let module Other_node : Hooks = struct
    include Default_hooks

    let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      return (block_hash, block_header, operations, [Pass; Pass; Pass; Pass])

    let on_inject_operation ~op_hash ~op =
      op_is_endorsement ~level:1l ~round:0l op_hash op
      >>=? fun is_a10_endorsement ->
      return
        ( op_hash,
          op,
          if is_a10_endorsement then [Pass; Block; Block; Block]
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
      (match (level, round) with
      | 2l, 1l -> save_proposal_payload ~protocol_data ~var:a_proposal_2_1
      | _ -> return_unit)
      >>=? fun () ->
      return (block_hash, block_header, operations, [Pass; Pass; Pass; Pass])

    let on_inject_operation ~op_hash ~op =
      op_is_endorsement ~level:1l ~round:0l op_hash op
      >>=? fun is_a10_endorsement ->
      return
        ( op_hash,
          op,
          if is_a10_endorsement then [Pass; Block; Block; Block]
          else [Pass; Pass; Pass; Pass] )

    let stop_on_event = stop_on_event0

    let check_chain_on_success = check_chain_on_success0 "A"
  end in
  let module Node_b_hooks : Hooks = struct
    include Default_hooks

    let on_inject_block ~level ~round ~block_hash ~block_header ~operations
        ~protocol_data:_ =
      (match (level, round) with
      | 1l, 1l -> return [Block; Delay 0.1; Delay 0.1; Delay 0.1]
      | 2l, 0l -> return [Block; Pass; Pass; Pass]
      | _ -> return [Pass; Pass; Pass; Pass])
      >>=? fun propagation_vector ->
      return (block_hash, block_header, operations, propagation_vector)

    let on_inject_operation ~op_hash ~op =
      op_is_endorsement ~level:1l ~round:0l op_hash op
      >>=? fun is_a10_endorsement ->
      op_is_preendorsement ~level:2l op_hash op
      >>=? fun level2_preendorsement ->
      op_is_endorsement ~level:2l op_hash op >>=? fun level2_endorsement ->
      let propagation_vector =
        match
          (is_a10_endorsement, level2_preendorsement, level2_endorsement)
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
      op_is_endorsement ~level:1l ~round:0l op_hash op
      >>=? fun is_a10_endorsement ->
      op_is_preendorsement ~level:2l op_hash op
      >>=? fun level2_preendorsement ->
      op_is_endorsement ~level:2l op_hash op >>=? fun level2_endorsement ->
      let propagation_vector =
        match
          ( is_a10_endorsement,
            !c_received_2_1,
            level2_preendorsement,
            level2_endorsement )
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
      op_is_endorsement ~level:1l ~round:0l op_hash op
      >>=? fun is_a10_endorsement ->
      op_is_preendorsement ~level:2l op_hash op
      >>=? fun level2_preendorsement ->
      op_is_endorsement ~level:2l op_hash op >>=? fun level2_endorsement ->
      let propagation_vector =
        match
          ( is_a10_endorsement,
            !d_received_2_1,
            level2_preendorsement,
            level2_endorsement )
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
  let b_proposal_2_0 = ref None in
  let stop_on_event0 = function
    | Baking_state.New_head_proposal {block; _} -> block.shell.level > 4l
    | _ -> false
  in
  let on_inject_operation0 ~op_hash ~op =
    op_is_endorsement ~level:1l ~round:0l op_hash op
    >>=? fun is_a10_endorsement ->
    op_is_endorsement ~level:2l ~round:0l op_hash op
    >>=? fun is_b20_endorsement ->
    op_is_endorsement ~level:2l ~round:1l op_hash op
    >>=? fun is_c21_endorsement ->
    let propagation_vector =
      if is_a10_endorsement then [Pass; Block; Block; Block]
      else if is_b20_endorsement || is_c21_endorsement then
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
      (match (level, round) with
      | 1l, 1l -> return [Block; Delay 0.1; Delay 0.1; Delay 0.1]
      | 2l, 0l ->
          save_proposal_payload ~protocol_data ~var:b_proposal_2_0
          >>=? fun () -> return [Block; Pass; Pass; Pass]
      | _ -> return [Pass; Pass; Pass; Pass])
      >>=? fun propagation_vector ->
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

let () =
  Alcotest_lwt.run "mockup_baking" ~__FILE__
  @@ List.map
       (fun (title, body) ->
         let open Tezos_base_test_helpers.Tztest in
         (title, [tztest title `Quick body]))
       [
         (Protocol.name ^ ": reaches level 5", test_level_5);
         ( Protocol.name ^ ": cannot progress without new head",
           test_preendorse_on_valid );
         (Protocol.name ^ ": reset delayed pqc", test_reset_delayed_pqc);
         (Protocol.name ^ ": scenario t1", test_scenario_t1);
         (Protocol.name ^ ": scenario t2", test_scenario_t2);
         (Protocol.name ^ ": scenario t3", test_scenario_t3);
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
       ]
  |> Lwt_main.run
