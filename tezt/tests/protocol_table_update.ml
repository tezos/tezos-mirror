(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Store/protocol activation
   Invocation:   dune exec tezt/tests/main.exe -- --file protocol_table_update.ml
   Subject:      Checks if a protocol activation is well handled in the store
*)

let check_protocol_activation ~migrate_from ~migrate_to ~block client =
  let* migration_block =
    Client.RPC.call client @@ RPC.get_chain_block_metadata ~block ()
  in
  Check.(
    (migration_block.protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (migration_block.next_protocol = Protocol.hash migrate_to)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  Lwt.return_unit

let wait_for_protocol_table_update node =
  let filter json =
    let proto_hash = JSON.(json |-> "proto_hash" |> as_string_opt) in
    let block_hash = JSON.(json |-> "block_hash" |> as_string_opt) in
    match (proto_hash, block_hash) with
    | Some ph, Some bh -> Some (ph, bh)
    | _ -> None
  in
  let* activation_block =
    Node.wait_for node "update_protocol_table.v0" filter
  in
  Lwt.return activation_block

(* This test aims to check that the protocol table of the store which
   maintains an associative list between each protocol and its
   associated activation block is well updated, even if there are
   activation block siblings for the activation block of a protocol.

   To do so, after some initialization, we are going to:
   - activate the protocol [migrate_to] on node_1 only
   - activate the protocol [migrate_to] on node_2 only (the activation
     blocks on the nodes must differ)
   - bake some blocks on node_2 to increase the fitness of that branch
   - restart node_1 and make sure that it switches to the node_2
     branch and then, check that it triggers a protocol table update
*)
let test_protocol_table_update ~migrate_from ~migrate_to =
  Test.register
    ~__FILE__
    ~title:"protocol activation"
    ~tags:["protocol"; "protocol_table"; "update"]
  @@ fun () ->
  let migration_level =
    (* NOTE: Migration to Tenderbake is only supported after the first
       cycle, therefore at [migration_level >= blocks_per_cycle]. *)
    8
  in
  let migration_block = string_of_int migration_level in
  let* node_1 =
    Node.init
      ~patch_config:
        (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
           [(migration_level, migrate_to)])
      [Synchronisation_threshold 0]
  and* node_2 =
    Node.init
      ~patch_config:
        (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
           [(migration_level, migrate_to)])
      [Synchronisation_threshold 0]
  in
  let* client_1 = Client.(init ~endpoint:(Node node_1) ()) in
  let* client_2 = Client.(init ~endpoint:(Node node_2) ()) in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  (* Initializing the common chain history. *)
  Log.info "Activating protocol %s" (Protocol.name migrate_from) ;
  let* () = Client.activate_protocol ~protocol:migrate_from client_1 in
  let* () =
    repeat (migration_level - 2) (fun () -> Client.bake_for_and_wait client_1)
  in
  let toward_activation = migration_level - 1 in
  let* _ = Node.wait_for_level node_1 toward_activation
  and* _ = Node.wait_for_level node_2 toward_activation in
  Log.info "Both nodes are at level %d." toward_activation ;
  (* Shutting down node_2 to make an activation on node_1 only. *)
  let* () = Node.terminate node_2 in
  let activation_promise_node_1 = wait_for_protocol_table_update node_1 in
  let* () =
    Client.bake_for_and_wait
      ~keys:[Constant.bootstrap1.public_key_hash]
      client_1
  in
  let* () =
    check_protocol_activation
      ~migrate_from
      ~migrate_to
      ~block:migration_block
      client_1
  in
  let* ph_n1_alt, bh_n1_alt = activation_promise_node_1 in
  Log.info "Node 1 activates protocol %s on block %s" ph_n1_alt bh_n1_alt ;
  (* Shutdown node_1 and make an alternate activation on node_2. *)
  let* () = Node.terminate node_1 in
  let* () = Node.run node_2 [] in
  let* () = Node.wait_for_ready node_2 in
  let activation_promise_node_2 = wait_for_protocol_table_update node_2 in
  (* Bake the activation block with a different key to ensure divergence. *)
  let* () =
    Client.bake_for_and_wait
      ~keys:[Constant.bootstrap2.public_key_hash]
      client_2
  in
  let* () =
    check_protocol_activation
      ~migrate_from
      ~migrate_to
      ~block:migration_block
      client_2
  in
  let* ph_n2, bh_n2 = activation_promise_node_2 in
  Log.info "Node 2 activates protocol %s on block %s" ph_n2 bh_n2 ;
  if String.equal bh_n1_alt bh_n2 then Test.fail "Activation block must differ." ;
  (* Bake a few blocks (eg [num_blocks]) to increase the fitness of node's 2 chain. *)
  let num_blocks = 5 in
  let target_level = migration_level + 5 in
  let* () =
    repeat num_blocks (fun () ->
        if String.equal ph_n2 (Protocol.hash Alpha) then
          Client.bake_for_and_wait ~keys:[] client_2
        else Client.bake_for_and_wait client_2)
  in
  let activation_promise_switch = wait_for_protocol_table_update node_1 in
  (* Restart node_1 and make it switches to node's 2 chain and update
     it's protocol table well.*)
  let* () = Node.run node_1 [] in
  let* () = Node.wait_for_ready node_1 in
  let* () = Client.Admin.connect_address ~peer:node_2 client_1 in
  let* _ = Node.wait_for_level node_1 target_level
  and* _ = Node.wait_for_level node_2 8 in
  Log.info "Both nodes are at level %d." target_level ;
  let* ph_n1, bh_n1 = activation_promise_switch in
  Log.info
    "Node 1 updated its protocol table activation block for protocol %s at \
     block %s"
    ph_n1
    bh_n1 ;
  if not (String.equal bh_n1 bh_n2) then
    Test.fail "Activation block must be equal." ;
  Lwt.return_unit

(** Checks that a protocol table is correctly updated after a branch
   switch where the new branch has a protocol activation through an
   user activated upgrade. *)
let test_branch_switch ~migrate_from ~migrate_to =
  Test.register
    ~__FILE__
    ~title:"protocol table"
    ~tags:["protocol"; "protocol_table"; "branch"]
  @@ fun () ->
  let migration_level = 2 in
  let uau =
    Node.Config_file.set_sandbox_network_with_user_activated_upgrades
      [(migration_level, migrate_to)]
  in
  let* node1 = Node.init ~patch_config:uau [Synchronisation_threshold 0] in
  let* client1 = Client.(init ~endpoint:(Node node1) ()) in
  let* () = Client.activate_protocol ~protocol:migrate_from client1 in
  let* node2 = Node.init [Synchronisation_threshold 0] in
  let* client2 = Client.(init ~endpoint:(Node node2) ()) in
  let* () = Client.Admin.connect_address ~peer:node1 client2 in
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client1) in
  let* _ = Node.wait_for_level node1 3 in
  let* () = Client.bake_for_and_wait client2 in
  let* _ = Node.wait_for_level node2 2 in
  let* () = Node.terminate node2 in
  let () = Node.Config_file.update node2 uau in
  let* () = Node.run node2 [Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node2 in
  (* The block is invalid but is still accepted. *)
  let* () = Client.Admin.trust_address ~peer:node1 client2 in
  let* () = Client.Admin.connect_address ~peer:node1 client2 in
  let* _ = Node.wait_for_level node2 3 in
  let path = ["chains"; "main"; "blocks"; "head"; "protocols"] in
  let* _ = Client.rpc GET path client1 in
  let path = ["chains"; "main"; "blocks"; "head"; "protocols"] in
  let* _ = Client.rpc GET path client2 in
  unit

let register ~migrate_from ~migrate_to =
  test_protocol_table_update ~migrate_from ~migrate_to ;
  test_branch_switch ~migrate_from ~migrate_to
