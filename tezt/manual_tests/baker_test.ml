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
   Component:    Baker
   Invocation:   dune exec tezt/manual_tests/main.exe -- --file baker_test.ml
   Subject:      Ensure that the baker behave properly
*)

let craft_heavy_operation source branch contract_hash client =
  let arg = `O [("int", `String "9606473")] in
  let* counter = Operation.get_next_counter ~source client in
  let operation =
    Operation.Manager.make ~source ~counter ~fee:1_000_000 ~gas_limit:1_000_000
    @@ Operation.Manager.call
         ~amount:0
         ~entrypoint:"default"
         ~arg
         ~dest:contract_hash
         ()
  in
  Operation.Manager.operation ~branch ~signer:source [operation] client

let craft_heavy_mempool client contract_hash bootstraps =
  let* branch =
    RPC.Client.call client @@ RPC.get_chain_block_hash ~block:"0" ()
  in
  Lwt_list.map_s
    (fun source -> craft_heavy_operation source branch contract_hash client)
    bootstraps

type status = {
  validated : (string * bool) option;
  applied : string option;
  expected_consensus_ops : int;
  attestations : String_set.t;
  preattestations : String_set.t;
}

let init_status expected_consensus_ops =
  {
    validated = None;
    applied = None;
    expected_consensus_ops;
    attestations = String_set.empty;
    preattestations = String_set.empty;
  }

(* This function only returns when all operations have been processed by the
   prevalidator, this should avoid analysing mempool where operation are not yet
   classified *)
let rec get_mempool ?applied ?branch_delayed client =
  let* mempool =
    Mempool.get_mempool
      ?applied
      ?branch_delayed
      ~refused:false
      ~outdated:false
      ~branch_refused:false
      client
  in
  if mempool.unprocessed <> [] then get_mempool ?applied ?branch_delayed client
  else return mempool

(* This function checks that all [ophs] are included in the applied or
   branch_delayed mempool. *)
let check_ophs_in_mempool ~applied ophs (mempool : Mempool.t) =
  let ophs_seen =
    List.for_all
      (fun op ->
        List.mem
          op
          (if applied then mempool.applied else mempool.branch_delayed))
      ophs
  in
  if not ophs_seen then
    failwith
      (sf
         "Not all expected operations are classified as %s in the mempool"
         (if applied then "applied" else "branch_delayed"))

(* This test checks that the baker can start to pre-attest a proposal before
   the block application finishes in the node. On Mumbai, pre-attestation should
   not be propagated and PQC should not be reached before the block
   application. *)
let baker_early_preattestation_test =
  Protocol.register_test
    ~__FILE__
    ~title:"Test baker early pre-attestation"
    ~tags:["node"; "baker"; "early"; "preattestation"]
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  Log.info
    "Initialize two connected nodes that activate the protocol %s."
    (Protocol.name protocol) ;
  let* node1, client1 =
    Client.init_with_node
      ~event_sections_levels:[("prevalidator", `Debug)]
      ~nodes_args:[Connections 1; Synchronisation_threshold 0]
      `Client
      ()
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      (* Hard_gas_limit_per_block is increased to be able to propose block that
         take multiple second to be applied by a node. *)
      [(["hard_gas_limit_per_block"], `String "10_000_000")]
  in
  let* () =
    Client.activate_protocol_and_wait
      ~protocol
      ~parameter_file
      ~timestamp:Now
      client1
  in
  let* node2, client2 =
    Client.init_with_node
      ~event_sections_levels:[("prevalidator", `Debug)]
      ~nodes_args:[Connections 1; Synchronisation_threshold 0]
      `Client
      ()
  in
  let* () = Client.Admin.connect_address ~peer:node2 client1 in
  let* proto_activation_lvl = Node.wait_for_level node2 1 in

  Log.info {|Originate contract "loop" that can consume a great amount of gas.|} ;
  let* _, contract_hash =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~burn_cap:(Tez.of_int 99999)
      ~src:Constant.bootstrap1.alias
      client1
      ["mini_scenarios"; "loop"]
      protocol
  in

  let bootstraps =
    Constant.[bootstrap1; bootstrap2; bootstrap3; bootstrap4; bootstrap5]
  in
  let baker1_delegates =
    [Constant.bootstrap1; Constant.bootstrap3; Constant.bootstrap4]
  in
  let baker1_delegates_alias =
    List.map (fun (bootstrap : Account.key) -> bootstrap.alias) baker1_delegates
  in
  let baker2_delegates = [Constant.bootstrap2; Constant.bootstrap5] in
  let baker2_delegates_alias =
    List.map (fun (bootstrap : Account.key) -> bootstrap.alias) baker2_delegates
  in
  let pp_list =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ';')
      Format.pp_print_string
  in
  Log.info
    "Start a baker on the %s with 60%% of voting power with delegates [%a]."
    (Node.name node1)
    pp_list
    baker1_delegates_alias ;
  let* baker1 =
    Baker.init
      ~delegates:
        (List.map
           (fun (bootstrap : Account.key) -> bootstrap.public_key_hash)
           baker1_delegates)
      ~protocol
      node1
      client1
  in
  Log.info
    "Start a second baker on the %s with 40%% of the voting power with \
     delegates [%a]."
    (Node.name node2)
    pp_list
    baker2_delegates_alias ;
  let* baker2 =
    Baker.init
      ~event_sections_levels:
        [
          ( String.concat
              "."
              [Protocol.encoding_prefix protocol; "baker"; "operation_worker"],
            `Debug );
          (* we need to set this section to Debug level to be able to wait for
             the qc_reached event. *)
        ]
      ~delegates:
        (List.map
           (fun (bootstrap : Account.key) -> bootstrap.public_key_hash)
           baker2_delegates)
      ~protocol
      node2
      client2
  in

  let recover_baking_rights ~max_round client level =
    let* baking_rights =
      RPC.Client.call client
      @@ RPC.get_chain_block_helper_baking_rights ~level ()
    in
    return
    @@ List.filter_map
         (fun attestation_right ->
           let round = JSON.(attestation_right |-> "round" |> as_int) in
           if round > max_round then None
           else
             let key =
               JSON.(attestation_right |-> "consensus_key" |> as_string)
             in
             List.find_map
               (fun (bootstrap : Account.key) ->
                 if String.equal bootstrap.public_key_hash key then
                   Some (round, bootstrap.alias)
                 else None)
               bootstraps)
         JSON.(baking_rights |> as_list)
  in
  (* This levels have been chosen such as the first baker can propose at round 0
     on the [prev_level] and the [test_level], and that the second baker can
     propose on the [prev_lvl] a round 1. *)
  let prev_lvl = succ (succ proto_activation_lvl) in
  let test_lvl = succ prev_lvl in
  let next_lvl = succ test_lvl in
  Log.info
    "Ensure that the test is in an state where %s propose at round 0 for level \
     %d and %d. And that %s propose at level %d round 1. "
    (Baker.name baker1)
    prev_lvl
    test_lvl
    (Baker.name baker2)
    test_lvl ;
  let* baking_rights_prev_lvl =
    recover_baking_rights ~max_round:1 client1 prev_lvl
  in
  let* baking_rights_test_lvl =
    recover_baking_rights ~max_round:0 client1 test_lvl
  in
  let print_baking_rights level rights =
    Log.info
      "Baking rights at level %d:%a"
      level
      pp_list
      (List.map (fun (round, key) -> sf " round:%d -> key:%s" round key) rights)
  in
  print_baking_rights prev_lvl baking_rights_prev_lvl ;
  print_baking_rights test_lvl baking_rights_test_lvl ;
  assert (
    List.for_all
      (fun (round, key) ->
        if round = 0 then List.mem key baker1_delegates_alias
        else List.mem key baker2_delegates_alias)
      (baking_rights_prev_lvl @ baking_rights_test_lvl)) ;

  Log.info "Wait for the nodes to reach level %d" prev_lvl ;
  Log.info
    "Ensure that %s as reached the EQC at level %d round 0 and is locked on a \
     payload before starting to inject costly manager operations"
    (Baker.name baker2)
    prev_lvl ;
  let is_proposed = ref false in
  let baker_on_event resolver Baker.{name; value; timestamp = _} =
    match name with
    | "new_valid_proposal.v0" ->
        let level = JSON.(value |-> "level" |> as_int) in
        let round = JSON.(value |-> "round" |> as_int) in
        if level = prev_lvl && round = 0 then is_proposed := true
    | "qc_reached.v0" ->
        if !is_proposed then (
          is_proposed := false ;
          Lwt.wakeup resolver ())
    | _ -> ()
  in
  let baker_t, baker_u = Lwt.task () in
  Baker.on_event baker2 (baker_on_event baker_u) ;
  let* () = baker_t in

  Log.info
    "While waiting for the round 0 to finish, craft multiple operations that \
     consume a lot of gas unit and inject them on %s. The goal is to craft a \
     block that take a significant time to be applied at level: %d."
    (Node.name node1)
    test_lvl ;
  let* ops = craft_heavy_mempool client1 contract_hash bootstraps in
  let* transactions_ophs =
    Operation.inject_operations
      ~request:`Inject
      ~use_tmp_file:true
      ~protocol:Protocol.Alpha
      ~force:false
      ops
      client1
  in

  Log.info
    "Ensure that these operations are applied in the %s mempool."
    (Node.name node1) ;
  let* mempool = get_mempool ~branch_delayed:false client1 in
  check_ophs_in_mempool
    ~applied:true
    (List.map (fun (`OpHash oph) -> oph) transactions_ophs)
    mempool ;

  Log.info
    "Stop the %s once these operations are included in the %s mempool."
    (Baker.name baker1)
    (Node.name node1) ;
  let* () = Baker.stop baker1 in

  Log.info
    "Wait for %s to propose a block at level:%d round 1."
    (Baker.name baker2)
    prev_lvl ;
  let is_proposed = ref false in
  let baker_on_event resolver Baker.{name; value; timestamp = _} =
    match name with
    | "new_valid_proposal.v0" ->
        let level = JSON.(value |-> "level" |> as_int) in
        let round = JSON.(value |-> "round" |> as_int) in
        if level = prev_lvl && round = 1 then is_proposed := true
    | "applied_expected_proposal_received.v0" ->
        if !is_proposed then (
          is_proposed := false ;
          Lwt.wakeup resolver ())
    | _ -> ()
  in
  let baker_t, baker_u = Lwt.task () in
  Baker.on_event baker2 (baker_on_event baker_u) ;
  let* () = baker_t in

  let baker_on_event ?(log = false) (status : status ref)
      preattestations_resolver pqc_resolver attestations_resolver
      Baker.{name; value; timestamp = _} =
    match name with
    | "new_valid_proposal.v0" ->
        let block = JSON.(value |-> "block" |> as_string) in
        let level = JSON.(value |-> "level" |> as_int) in
        let round = JSON.(value |-> "round" |> as_int) in
        if level = test_lvl && round = 0 then (
          if log then Log.info "Proposal for level %d received." level ;
          status := {!status with validated = Some (block, false)})
        else
          let validated =
            Option.map (fun v -> (fst v, true)) !status.validated
          in
          status := {!status with validated}
    | "preattestation_injected.v0" | "preendorsement_injected.v0" -> (
        match (!status.validated, !status.applied) with
        | Some (_, false), Some _ ->
            failwith
              "Pre-attestation should be injected before application of the \
               block"
        | Some (_, false), None ->
            status :=
              {
                !status with
                preattestations =
                  String_set.add
                    JSON.(value |-> "ophash" |> as_string)
                    !status.preattestations;
              } ;
            if
              String_set.cardinal !status.preattestations
              = !status.expected_consensus_ops
            then Lwt.wakeup preattestations_resolver ()
        | None, _ | Some (_, true), _ ->
            () (* Only pre-attestation for the wanted proposal are recorded *))
    | "attestation_injected.v0" | "endorsement_injected.v0" -> (
        match (!status.validated, !status.applied) with
        | Some (_, false), None ->
            failwith
              "Attestation can only be injected once the block is applied"
        | Some (b1, false), Some b2 when String.equal b1 b2 ->
            status :=
              {
                !status with
                attestations =
                  String_set.add
                    JSON.(value |-> "ophash" |> as_string)
                    !status.attestations;
              } ;
            if
              String_set.cardinal !status.attestations
              = !status.expected_consensus_ops
            then Lwt.wakeup attestations_resolver ()
        | None, _ | Some (_, true), _ | Some (_, false), Some _ ->
            () (* Only attestation for the wanted proposal are recorded *))
    | "pqc_while_waiting_for_application.v0" -> (
        (* This will never be reached on Mumbai *)
        match (!status.validated, !status.applied) with
        | Some (_, false), Some _ ->
            failwith "PQC should not be reached after the block application"
        | Some (bhash, _), None ->
            let bhash_pqc = JSON.(value |> as_string) in
            if String.equal bhash bhash_pqc then (
              if log then Log.info "PQC for proposal at level %d seen" test_lvl ;
              Lwt.wakeup pqc_resolver ())
        | None, _ | Some (_, true), Some _ ->
            () (* Only PQC for the wanted proposal is monitored *))
    | "applied_expected_proposal_received.v0" -> (
        match (!status.validated, !status.applied) with
        | Some (b1, false), Some b2 when String.equal b1 b2 ->
            failwith "block should only be applied once"
        | Some (bhash, _), None ->
            let bhash_applied = JSON.(value |> as_string) in
            if String.equal bhash bhash_applied then (
              if log then
                Log.info "Proposal for level %d applied in the node." test_lvl ;
              status := {!status with applied = Some bhash_applied})
        | None, _ | Some _, Some _ ->
            () (* Only application for the wanted proposal is monitored *))
    | _ -> ()
  in

  (* Watch the baker events until the new block is applied and consensus
     operations from their delegates are injected in the node *)
  let status1 = ref (init_status 3) in
  let preattestation_t1, preattestation_u1 = Lwt.task () in
  let pqc_t1, pqc_u1 = Lwt.task () in
  let attestations_t1, attestations_u1 = Lwt.task () in
  Baker.on_event
    baker1
    (baker_on_event ~log:true status1 preattestation_u1 pqc_u1 attestations_u1) ;

  (* Watch the baker events until the new block is applied and consensus
     operations from their delegates are injected in the node *)
  let status2 = ref (init_status 2) in
  let preattestation_t2, preattestation_u2 = Lwt.task () in
  let pqc_t2, pqc_u2 = Lwt.task () in
  let attestations_t2, attestations_u2 = Lwt.task () in
  Baker.on_event
    baker2
    (baker_on_event status2 preattestation_u2 pqc_u2 attestations_u2) ;

  Log.info
    "Continue %s and wait for it to propose a block at level:%d that contains \
     the gas consuming manager operations."
    (Baker.name baker1)
    test_lvl ;
  let* _baker = Baker.continue baker1 in

  let preattestation_should_be_applied = Protocol.(number protocol > 16) in

  (* Wait for the pre-attestations to be included and for the pqc to be reached
     before application *)
  let get_preattestation_mempool =
    get_mempool
      ~applied:preattestation_should_be_applied
      ~branch_delayed:(not preattestation_should_be_applied)
  in
  let* () = if preattestation_should_be_applied then pqc_t1 else unit
  and* () = preattestation_t1 in
  let* mempool1 = get_preattestation_mempool client1 in

  let* () = if preattestation_should_be_applied then pqc_t2 else unit
  and* () = preattestation_t2 in
  let* mempool2 = get_preattestation_mempool client2 in
  Log.info
    "%s have proposed, both baker have pre-attested for their delegates%s"
    (Baker.name baker1)
    (if preattestation_should_be_applied then
     " and the pqc is reached. Ensure that the pre-attestations are injected \
      in the nodes as applied operation"
    else
      ". Ensure that the pre-attestations are injected in the nodes as \
       branch_delayed operation") ;

  let expected_preattestations status_a status_b =
    List.of_seq
    @@
    if preattestation_should_be_applied then
      String_set.to_seq
        (String_set.union !status_a.preattestations !status_b.preattestations)
    else String_set.to_seq !status_a.preattestations
  in
  check_ophs_in_mempool
    ~applied:preattestation_should_be_applied
    (expected_preattestations status1 status2)
    mempool1 ;
  check_ophs_in_mempool
    ~applied:preattestation_should_be_applied
    (expected_preattestations status2 status1)
    mempool2 ;

  Log.info
    "Wait for the block to be applied by the nodes and wait for them to attest \
     it." ;
  (* Wait for the attestations to be included *)
  let* () = attestations_t1 in
  let* mempool1 = get_mempool ~applied:true ~branch_delayed:false client1 in

  let* () = attestations_t2 in
  let* mempool2 = get_mempool ~applied:true ~branch_delayed:false client2 in
  Log.info
    "Ensure that the attestations as well as the pre-attestations are applied \
     in the mempools" ;
  check_ophs_in_mempool
    ~applied:true
    (List.of_seq
    @@ Seq.append
         (String_set.to_seq !status1.preattestations)
         (String_set.to_seq !status1.attestations))
    mempool1 ;
  check_ophs_in_mempool
    ~applied:true
    (List.of_seq
    @@ Seq.append
         (String_set.to_seq !status2.preattestations)
         (String_set.to_seq !status2.attestations))
    mempool2 ;

  Log.info
    "Ensure that the block at level: %d contain the manager operations"
    test_lvl ;
  let* manager_ops =
    RPC.Client.call client1
    @@ RPC.get_chain_block_operation_hashes_of_validation_pass
         ~block:(string_of_int test_lvl)
         3
  in
  Check.(List.compare_lengths manager_ops transactions_ophs = 0)
    Check.int
    ~error_msg:"Block should contain all the transactions" ;

  Log.info
    "Wait for lvl %d and ensure that it contain all the attestations"
    next_lvl ;
  let* _ = Node.wait_for_level node2 5 in
  let* consensus_ops =
    RPC.Client.call client1
    @@ RPC.get_chain_block_operation_hashes_of_validation_pass
         ~block:(string_of_int next_lvl)
         0
  in
  Check.(List.compare_length_with consensus_ops 5 = 0)
    Check.int
    ~error_msg:(sf "Block should contain 5 consensus operations") ;
  unit

let register ~protocols = baker_early_preattestation_test protocols
