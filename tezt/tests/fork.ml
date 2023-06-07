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
   Component:    Node and baker
   Invocation:   dune exec tezt/tests/main.exe -- --file fork.ml
   Subject:      Create a fork and assert its resolution
*)

(* Copy-pasted from tenderbake.ml. *)
let baker_at_round_n ?level round client =
  let* json =
    RPC.Client.call client @@ RPC.get_chain_block_helper_baking_rights ?level ()
  in
  match JSON.(json |=> round |-> "delegate" |> as_string_opt) with
  | Some delegate_id -> return delegate_id
  | None ->
      Test.fail
        "Could not find the baker at round %d for level %s"
        round
        (match level with None -> "head" | Some level -> string_of_int level)

let wait_for_ignore_head node level =
  let filter json =
    match JSON.(json |-> "level" |> as_int_opt) with
    | Some l when l = level -> Some ()
    | Some _ -> None
    | None -> None
  in
  Node.wait_for node "ignore_head.v0" filter

let wait_for_branch_switch node level =
  let filter json =
    match JSON.(json |-> "level" |> as_int_opt) with
    | Some l when l = level -> Some ()
    | Some _ -> None
    | None -> None
  in
  Node.wait_for node "branch_switch.v0" filter

(* Constructs two independent branches with different fitness on two
   disconnected nodes. When connecting the nodes, checks that the
   highest fitness head is the chosen branch. *)
let test_fork =
  Protocol.register_test ~__FILE__ ~title:"fork" ~tags:["fork"]
  @@ fun protocol ->
  let nodes_arguments = Node.[Synchronisation_threshold 0] in
  let restart node =
    let* () = Node.run node nodes_arguments in
    Node.wait_for_ready node
  in
  Log.info "Initialize two nodes" ;
  let* node1, client1 =
    Client.init_with_node ~nodes_args:nodes_arguments `Client ()
  in
  let* node2, client2 =
    Client.init_with_node ~nodes_args:nodes_arguments `Client ()
  in
  let* () =
    Client.Admin.connect_address ~endpoint:(Node node1) ~peer:node2 client1
  in
  let parameters = [(["consensus_threshold"], `Int 0)] in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (protocol, None)) parameters
  in
  let* () =
    Client.activate_protocol_and_wait ~parameter_file ~protocol client1
  in
  Log.info "Wait until all nodes are on level 1" ;
  let* (_ : int) = Node.wait_for_level node1 1 in
  let* (_ : int) = Node.wait_for_level node2 1 in

  Log.info "Terminate node 2" ;
  let* () = Node.terminate node2 in

  (* Bake two blocks at round 0 so that the fitness is maximal. *)
  Log.info "Bake a branch with a high fitness on node 1" ;
  let level = 1 in
  Check.(level = Node.get_level node1)
    Check.int
    ~__LOC__
    ~error_msg:"Level is expected to be %L but is %R" ;
  let* baker = baker_at_round_n ~level:(level + 1) 0 client1 in
  let* () = Client.bake_for_and_wait ~keys:[baker] client1 in
  let* baker = baker_at_round_n ~level:(level + 2) 0 client1 in
  let* () = Client.bake_for_and_wait ~keys:[baker] client1 in
  let* head_1 = RPC.call node1 @@ RPC.get_chain_block_header () in

  Log.info "Terminate node 1 and restart node 2" ;
  let* () = Node.terminate node1 in
  let* () = restart node2 in

  (* Bake two blocks. The first is at any round that is not associated
     to the round of the baker of round 0 , so that building a second
     block on top of it generates a block with a lower fitness. *)
  Log.info "Bake a branch with a low fitness on node2" ;
  let level = 1 in
  let* node2_level = Client.level client2 in
  Check.(level = node2_level)
    Check.int
    ~__LOC__
    ~error_msg:"Level is expected to be %L but is %R" ;
  let* baker =
    let* baker_at_round0 = baker_at_round_n ~level:(level + 1) 0 client2 in
    (* We take any baker that is not at round 0 to avoid flakiness in
       case of a baker with both round 0 and 1. *)
    let non_round0_baker =
      Array.find_opt
        (fun a -> not (String.equal baker_at_round0 a.Account.public_key_hash))
        Account.Bootstrap.keys
    in
    match non_round0_baker with
    | Some baker -> return baker.Account.public_key_hash
    | None -> assert false
  in
  let* () = Client.bake_for_and_wait ~keys:[baker] client2 in
  let* baker = baker_at_round_n ~level:(level + 2) 0 client2 in
  let* () = Client.bake_for_and_wait ~keys:[baker] client2 in
  let* head_2 = RPC.call node2 @@ RPC.get_chain_block_header () in

  (* Check that the fitness of the first branch (node1) is higher than
     that of the second branch (node2) *)
  let () =
    let open Tezos_base in
    let fitness_typ = Check.comparable Fitness.pp Fitness.compare in
    let json_1 = JSON.(head_1 |-> "fitness") in
    let json_2 = JSON.(head_2 |-> "fitness") in
    let fitness1 =
      Data_encoding.Json.destruct Fitness.encoding (JSON.unannotate json_1)
    in
    let fitness2 =
      Data_encoding.Json.destruct Fitness.encoding (JSON.unannotate json_2)
    in
    Check.(
      (fitness1 > fitness2)
        fitness_typ
        ~__LOC__
        ~error_msg:
          "Expected fitness of node1's branch (%L) to be higher than fitness \
           of node2's (%R)")
  in

  Log.info "Connect node 1 and 2" ;
  (* Make sure that the block at level 2 is ignored (fitness too low). *)
  let wait_ignored_head = wait_for_ignore_head node2 2 in
  (* Make sure that the block at level 3 (head) triggers a branch
     switch (better fitness). *)
  let wait_branch_switch = wait_for_branch_switch node2 3 in
  let* () = restart node1 in
  let* () = wait_ignored_head in
  let* () = wait_branch_switch in
  (* Make sure heads are synchronized. *)
  let* () =
    let* new_head_2 = RPC.call node2 @@ RPC.get_chain_block_header () in
    let head_hash_1 = JSON.(head_1 |-> "hash" |> as_string) in
    let head_hash_2 = JSON.(new_head_2 |-> "hash" |> as_string) in
    Check.(head_hash_1 = head_hash_2)
      Check.string
      ~__LOC__
      ~error_msg:"Heads are different: expected %L but got %R" ;
    unit
  in
  unit

let register ~protocols = test_fork protocols
