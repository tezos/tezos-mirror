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
   Component:    Consensus
   Invocation:   dune exec tezt/tests/main.exe -- --file nonce_seed_revelation.ml
   Subject:      Tests injection of nonce revelations
*)

let first_protocol_block = 1

let minimal_block_delay = 1

let delay_increment_per_round = 1

let num_nodes = 5

type block = {
  cycle : int option;
  cycle_position : int option;
  seed_nonce_hash : string option;
  operations : JSON.t list;
}

(* Test baker injection of nonce revelations.
   See http://tezos.gitlab.io/alpha/proof_of_stake.html
   Runs a node and a baker. The baker bakes two full cycles.
   We collect nonce hashes from the first cycle. And check
   that they are revealed in the second cycle *)
let test_nonce_seed_revelation =
  Protocol.register_test
    ~__FILE__
    ~title:"Nonce seed revelation"
    ~tags:["nonce"; "seed"; "revelation"; Tag.memory_3k]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  (* Run a node and a baker.
     The node runs in archive mode to obtain metadata with [RPC.get_chain_block]. *)
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [
        (["minimal_block_delay"], `String_of_int minimal_block_delay);
        (["delay_increment_per_round"], `String_of_int delay_increment_per_round);
      ]
  in
  (* Get protocol parameters *)
  let blocks_per_cycle, blocks_per_commitment =
    let parameters = JSON.parse_file parameter_file in
    JSON.
      ( parameters |-> "blocks_per_cycle" |> as_int,
        parameters |-> "blocks_per_commitment" |> as_int )
  in
  let target_level = first_protocol_block + (2 * blocks_per_cycle) in
  let nodes =
    Cluster.create num_nodes [Synchronisation_threshold 0; History_mode Archive]
  in
  let head_node = List.hd nodes in
  Cluster.clique nodes ;
  let* () = Cluster.start nodes in
  let* client = Client.init ~endpoint:(Node head_node) () in
  (* Set up promise to wait for level, before starting bakers *)
  let cycle_two_promise =
    Lwt_list.iter_p
      (fun node ->
        let* (_ : int) = Node.wait_for_level node target_level in
        unit)
      nodes
  in
  (* Start bakers before activating alpha *)
  let bakers_promise =
    Lwt_list.mapi_p
      (fun i node ->
        let* client = Client.init ~endpoint:(Node node) () in
        let delegates = [Account.Bootstrap.keys.(i).alias] in
        Baker.init ~protocol ~delegates node client)
      nodes
  in
  let* () =
    Client.activate_protocol_and_wait
      ~timestamp:Now
      ~parameter_file
      ~protocol
      client
  in
  (* Wait for the bakers to start *)
  let* bakers = bakers_promise in
  Log.info "Wait for two cycles" ;
  (* Wait until target level is reached *)
  let* () = cycle_two_promise in
  (* No need to bake further *)
  let* () = Lwt_list.iter_p Baker.terminate bakers in
  Log.info "Get all blocks" ;
  (* Retrieve all blocks for two full cycles. *)
  let* blocks =
    Lwt_list.map_p
      (fun level ->
        let* block =
          Node.RPC.(
            call ~log_request:false head_node
            @@ get_chain_block ~block:(string_of_int level) ())
        in
        let level_info = JSON.(block |-> "metadata" |-> "level_info") in
        return
          JSON.
            {
              cycle = level_info |-> "cycle" |> as_int_opt;
              cycle_position = level_info |-> "cycle_position" |> as_int_opt;
              seed_nonce_hash =
                block |-> "header" |-> "seed_nonce_hash" |> as_string_opt;
              operations = block |-> "operations" |=> 2 |> as_list;
            })
      (range first_protocol_block (2 * blocks_per_cycle))
  in
  let blocks = Array.of_list blocks in
  Log.info "Cycle alignment" ;
  (* Test that cycles start where they are supposed to start.
     Not really needed but helps clarifying cycles positions. *)
  (* blocks[0] is considered cycle = 0, cycle_position = 0 for the new
     protocol, but because it is a protocol transition block, it
     doesn't have the "cycle" and "cycle_position" metadata (unlike
     the remaining blocks) *)
  let initial_block = blocks.(1) in
  let final_block = blocks.(blocks_per_cycle) in
  Check.(
    (initial_block.cycle = Some 0)
      (option int)
      ~__LOC__
      ~error_msg:"Expected %R, got %L" ;
    (initial_block.cycle_position = Some 1)
      (option int)
      ~__LOC__
      ~error_msg:"Expected %R, got %L" ;
    (final_block.cycle = Some 1)
      (option int)
      ~__LOC__
      ~error_msg:"Expected %R, got %L" ;
    (final_block.cycle_position = Some 0)
      (option int)
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let start_collect = 0 in
  let stop_collect = (blocks_per_cycle / blocks_per_commitment) - 1 in
  Log.info "Collect seed nonce hashes in first cycle" ;
  (* Collect nonce hashes in the block headers in the first cycle *)
  let seed_nonce_hashes : (int, string) Hashtbl.t = Hashtbl.create 5 in
  let () =
    List.iter
      (fun i ->
        let level = ((i + 1) * blocks_per_commitment) - first_protocol_block in
        Log.info "Collecting seed nonce in block %d" level ;
        match blocks.(level).seed_nonce_hash with
        | Some seed_nonce_hash ->
            Hashtbl.add seed_nonce_hashes level seed_nonce_hash
        | None ->
            Test.fail "Expected to find a seed nonce hash at level %d" level)
      (range start_collect stop_collect)
  in
  let start_collect_revelations = blocks_per_cycle in
  let len_collect_revelations = blocks_per_cycle in
  Log.info
    "Check revelations in blocks [%d..%d]"
    start_collect_revelations
    (start_collect_revelations + len_collect_revelations) ;
  (* Collect reveal ops in second cycle and check that their level match
     those of the nonce hashes from first cycle. *)
  let ops =
    List.concat_map
      (fun block -> block.operations)
      (Array.sub blocks start_collect_revelations len_collect_revelations
      |> Array.to_list)
  in
  let reveal_ops : (int, string) Hashtbl.t = Hashtbl.create 5 in
  let () =
    List.iter
      (fun op ->
        let contents = JSON.(op |-> "contents" |=> 0) in
        let kind = JSON.(contents |-> "kind" |> as_string) in
        let level =
          JSON.(contents |-> "level" |> as_int) - first_protocol_block
        in
        Check.(
          (kind = "seed_nonce_revelation")
            string
            ~__LOC__
            ~error_msg:"Expected %R, got %L" ;
          is_false
            (Hashtbl.mem reveal_ops level)
            ~__LOC__
            ~error_msg:"Cannot submit twice the same reveal operation" ;
          is_true
            (Hashtbl.mem seed_nonce_hashes level)
            ~__LOC__
            ~error_msg:"Level should match a seed") ;
        Log.info "Checked revelation for level %d" level ;
        Hashtbl.add reveal_ops level JSON.(contents |-> "nonce" |> as_string))
      ops
  in
  Check.(
    Hashtbl.(length reveal_ops = length seed_nonce_hashes)
      int
      ~__LOC__
      ~error_msg:
        "Expected the same number of reveal operations (%L) as seed nonce \
         hashes (%R)") ;
  unit

let register ~protocols = test_nonce_seed_revelation protocols
