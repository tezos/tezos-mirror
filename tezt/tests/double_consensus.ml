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
   Component:    Accuser
   Invocation:   dune exec tezt/tests/main.exe -- --file double_consensus.ml
   Subject:      Detect double (pre)attestation through the accuser.
*)

let team = Tag.layer1

let double_attestation_waiter accuser =
  Accuser.wait_for accuser (sf "double_attestation_denounced.v0") (fun _ ->
      Some ())

let double_preattestation_waiter accuser =
  Accuser.wait_for accuser (sf "double_preattestation_denounced.v0") (fun _ ->
      Some ())

let double_consensus_already_denounced_waiter accuser oph =
  Accuser.wait_for accuser "double_consensus_already_denounced.v0" (fun json ->
      if String.equal (JSON.as_string json) oph then Some () else None)

let get_double_consensus_denounciation_hash protocol consensus_name client =
  let double_consensus_kind =
    if Protocol.(number protocol > number R022) then
      "double_consensus_operation_evidence"
    else sf "double_%s_evidence" consensus_name
  in
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  let ops = JSON.(mempool |-> "validated" |> as_list) in
  let op =
    List.find_map
      (fun op ->
        let kind =
          JSON.(op |-> "contents" |> as_list |> List.hd |-> "kind" |> as_string)
        in
        if String.equal kind double_consensus_kind then
          Some JSON.(op |-> "hash" |> as_string)
        else None)
      ops
  in
  match op with
  | None -> failwith "Denunciation not found in the mempool"
  | Some op -> return op

let double_attestation_init
    (consensus_for :
      ?endpoint:Client.endpoint ->
      ?protocol:Protocol.t ->
      ?key:string list ->
      ?force:bool ->
      Client.t ->
      unit Lwt.t) ?parameter_file consensus_name protocol () =
  let* node, client =
    Client.init_with_protocol ?parameter_file ~protocol `Client ()
  in
  let* accuser = Accuser.init ~event_level:`Debug ~protocol node in
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in
  Log.info "Recover available slots for %s." Constant.bootstrap1.alias ;
  let* slots =
    Client.RPC.call client
    @@ RPC.get_chain_block_helper_validators
         ~delegate:Constant.bootstrap1.public_key_hash
         ()
  in
  let slots =
    List.map
      JSON.as_int
      JSON.(List.hd JSON.(slots |> as_list) |-> "slots" |> as_list)
  in
  Log.info "Inject valid %s." consensus_name ;
  let waiter = Node.wait_for_request ~request:`Inject node in
  let* () =
    consensus_for ~protocol ~force:true ~key:[Constant.bootstrap1.alias] client
  in
  let* () = waiter in
  Log.info "Get mempool and recover consensus information." ;
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  let op = List.hd JSON.(mempool |-> "validated" |> as_list) in
  let branch = JSON.(op |-> "branch" |> as_string) in
  let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
  let level = JSON.(content |-> "level" |> as_int) in
  let round = JSON.(content |-> "round" |> as_int) in
  let block_payload_hash =
    JSON.(content |-> "block_payload_hash" |> as_string)
  in
  return ((client, accuser), (branch, level, round, slots, block_payload_hash))

let attest_utils =
  ( Client.attest_for,
    (fun ~slot ~level ~round ~block_payload_hash ->
      Operation.Consensus.attestation ~slot ~level ~round ~block_payload_hash ()),
    double_attestation_waiter,
    "attestation" )

let preattest_utils =
  ( Client.preattest_for,
    Operation.Consensus.preattestation,
    double_preattestation_waiter,
    "preattestation" )

let double_consensus_wrong_block_payload_hash
    (consensus_for, mk_consensus, consensus_waiter, consensus_name) protocol =
  let* (client, accuser), (branch, level, round, slots, _block_payload_hash) =
    double_attestation_init consensus_for consensus_name protocol ()
  in
  let* header =
    Client.RPC.call client @@ RPC.get_chain_block_header ~block:"head~2" ()
  in
  let block_payload_hash = JSON.(header |-> "payload_hash" |> as_string) in
  Log.info "Inject an invalid %s and wait for denounciation" consensus_name ;
  let op =
    mk_consensus ~slot:(List.nth slots 0) ~level ~round ~block_payload_hash
  in
  let waiter = consensus_waiter accuser in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter in
  Log.info
    "Inject another invalid %s and wait for already_denounced event"
    consensus_name ;
  let* header =
    Client.RPC.call client @@ RPC.get_chain_block_header ~block:"head~3" ()
  in
  let block_payload_hash = JSON.(header |-> "payload_hash" |> as_string) in
  let op =
    mk_consensus ~slot:(List.nth slots 0) ~level ~round ~block_payload_hash
  in
  let* oph =
    get_double_consensus_denounciation_hash protocol consensus_name client
  in
  let waiter_already_denounced =
    double_consensus_already_denounced_waiter accuser oph
  in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter_already_denounced in
  unit

let double_attestation_wrong_block_payload_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"double attestation using wrong block_payload_hash"
    ~tags:
      [
        Tag.layer1;
        "double";
        "attestation";
        "accuser";
        "block_payload_hash";
        "node";
      ]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  double_consensus_wrong_block_payload_hash attest_utils protocol

let double_preattestation_wrong_block_payload_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"double preattestation using wrong block_payload_hash"
    ~tags:
      [
        Tag.layer1;
        "double";
        "preattestation";
        "accuser";
        "block_payload_hash";
        "node";
      ]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  double_consensus_wrong_block_payload_hash preattest_utils protocol

let double_consensus_wrong_branch
    (consensus_for, mk_consensus, consensus_waiter, consensus_name) protocol =
  let* (client, accuser), (_branch, level, round, slots, block_payload_hash) =
    double_attestation_init consensus_for consensus_name protocol ()
  in
  let* branch = Operation.Manager.get_branch ~offset:4 client in
  Log.info "Inject an invalid %s and wait for denounciation" consensus_name ;
  let op =
    mk_consensus ~slot:(List.nth slots 0) ~level ~round ~block_payload_hash
  in
  let waiter = consensus_waiter accuser in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter in
  Log.info
    "Inject another invalid %s and wait for already_denounced event"
    consensus_name ;
  let* branch = Operation.Manager.get_branch ~offset:5 client in
  let op =
    mk_consensus ~slot:(List.nth slots 0) ~level ~round ~block_payload_hash
  in
  let* oph =
    get_double_consensus_denounciation_hash protocol consensus_name client
  in
  let waiter_already_denounced =
    double_consensus_already_denounced_waiter accuser oph
  in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter_already_denounced in
  unit

let double_attestation_wrong_branch =
  Protocol.register_test
    ~__FILE__
    ~title:"double attestation using wrong branch"
    ~tags:[Tag.layer1; "double"; "attestation"; "accuser"; "branch"; "node"]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol -> double_consensus_wrong_branch attest_utils protocol

let double_preattestation_wrong_branch =
  Protocol.register_test
    ~__FILE__
    ~title:"double preattestation using wrong branch"
    ~tags:[Tag.layer1; "double"; "preattestation"; "accuser"; "branch"; "node"]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol -> double_consensus_wrong_branch preattest_utils protocol

let consensus_operation_too_old_waiter accuser =
  Accuser.wait_for accuser "consensus_operation_too_old.v0" (fun _ -> Some ())

let operation_too_old =
  Protocol.register_test
    ~__FILE__
    ~title:"operation too old"
    ~tags:[Tag.layer1; "accuser"; "old"; "operation"]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let* accuser =
    (* We set the preserved_levels to 0 to ensure that an operation for the
       previous level is accepted by the mempool and discarded by the
       accuser. *)
    Accuser.init ~preserved_levels:0 ~event_level:`Debug ~protocol node
  in
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  Log.info "Inject valid attestation." ;
  let waiter = Node.wait_for_request ~request:`Inject node in
  let* () =
    Client.attest_for
      ~protocol
      ~force:true
      ~key:[Constant.bootstrap1.alias]
      client
  in
  let* () = waiter in
  Log.info "Get mempool and recover consensus information." ;
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  let op = List.hd JSON.(mempool |-> "validated" |> as_list) in
  let branch = JSON.(op |-> "branch" |> as_string) in
  let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
  let level = JSON.(content |-> "level" |> as_int) in
  let slot = JSON.(content |-> "slot" |> as_int) in
  let block_payload_hash =
    JSON.(content |-> "block_payload_hash" |> as_string)
  in
  Log.info "Bake 1 block." ;
  let* () = Client.bake_for_and_wait client in
  Log.info
    "Craft and inject an attestation 1 level in the past and wait for \
     [consensus_operation_too_old.v0] event from the accuser." ;
  let op =
    Operation.Consensus.attestation ~slot ~level ~round:3 ~block_payload_hash ()
  in
  let waiter = consensus_operation_too_old_waiter accuser in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter in
  unit

let consensus_operation_too_far_in_future_waiter accuser =
  Accuser.wait_for accuser "consensus_operation_too_far_in_future.v0" (fun _ ->
      Some ())

let operation_too_far_in_future =
  Protocol.register_test
    ~__FILE__
    ~title:"operation too far in the future"
    ~tags:[Tag.layer1; "accuser"; "future"; "operation"]
    ~uses:(fun protocol -> [Protocol.accuser protocol])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let* accuser =
    Accuser.init ~preserved_levels:2 ~event_level:`Debug ~protocol node
  in
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  Log.info "Inject valid attestation." ;
  let waiter = Node.wait_for_request ~request:`Inject node in
  let* () =
    Client.attest_for
      ~protocol
      ~force:true
      ~key:[Constant.bootstrap1.alias]
      client
  in
  let* () = waiter in
  Log.info "Get mempool and recover consensus information." ;
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
  in
  let op = List.hd JSON.(mempool |-> "validated" |> as_list) in
  let branch = JSON.(op |-> "branch" |> as_string) in
  let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
  let block_payload_hash =
    JSON.(content |-> "block_payload_hash" |> as_string)
  in
  let level = 6 in
  Log.info
    "Recover available slots for %s at level %d."
    Constant.bootstrap1.alias
    level ;
  let* slots =
    Client.RPC.call client
    @@ RPC.get_chain_block_helper_validators
         ~delegate:Constant.bootstrap1.public_key_hash
         ~level
         ()
  in
  let slots =
    List.map
      JSON.as_int
      JSON.(List.hd JSON.(slots |> as_list) |-> "slots" |> as_list)
  in
  Log.info
    "Craft and inject an attestation 3 levels in the future and wait for \
     [consensus_operation_too_far_in_future.v0] event from the accuser." ;
  let op =
    Operation.Consensus.attestation
      ~slot:(List.hd slots)
      ~level
      ~round:0
      ~block_payload_hash
      ()
  in
  let waiter = consensus_operation_too_far_in_future_waiter accuser in
  let* _ =
    Operation.Consensus.inject
      ~force:true
      ~protocol
      ~branch
      ~signer:Constant.bootstrap1
      op
      client
  in
  let* () = waiter in
  unit

let register ~protocols =
  double_attestation_wrong_block_payload_hash protocols ;
  double_preattestation_wrong_block_payload_hash protocols ;
  double_attestation_wrong_branch protocols ;
  double_preattestation_wrong_branch protocols ;
  operation_too_old protocols ;
  operation_too_far_in_future protocols
