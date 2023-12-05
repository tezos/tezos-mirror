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

let originate_new_rollup ?(alias = "rollup")
    ?(boot_sector = Constant.wasm_echo_kernel_boot_sector)
    ?(parameters_ty = "bytes") ~src client =
  let* rollup =
    Client.Sc_rollup.originate
      client
      ~wait:"2"
      ~alias
      ~src
      ~kind:"wasm_2_0_0"
      ~parameters_ty
      ~boot_sector
      ~burn_cap:(Tez.of_int 2)
  in
  Log.info "Rollup %s originated" rollup ;
  return rollup

let setup_l2_node ?(mode = Sc_rollup_node.Operator) ?runner ?name ?loser_mode
    ~operator client node rollup =
  let rollup_node =
    Sc_rollup_node.create
      ?runner
      ?name
      ~base_dir:(Client.base_dir client)
      ~default_operator:operator
      mode
      node
  in

  let* _ = Sc_rollup_node.config_init ?loser_mode rollup_node rollup in
  Log.info "Starting a smart rollup node to track %s" rollup ;
  let* () = Sc_rollup_node.run rollup_node rollup [] in
  let* () = Sc_rollup_node.wait_for_ready rollup_node in
  Log.info "Smart rollup node started." ;
  return rollup_node

let game_in_progress ~staker rollup_address client =
  let* game =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_staker_games
         ~staker
         rollup_address
         ()
  in
  return (not ([] = JSON.as_list game))

let rec wait_for_game ~staker rollup_address client node =
  let* in_progress = game_in_progress ~staker rollup_address client in
  if not in_progress then (
    let* current_level = Node.get_level node in
    Log.info "Still no game at level %d" current_level ;
    let* _ = Node.wait_for_level node (current_level + 1) in
    wait_for_game ~staker rollup_address client node)
  else unit

let rec wait_for_end_of_game ~staker rollup_address client node =
  let* in_progress = game_in_progress ~staker rollup_address client in
  if in_progress then (
    let* current_level = Node.get_level node in
    Log.info "Game still in progress at level %d" current_level ;
    let* _ = Node.wait_for_level node (current_level + 1) in
    wait_for_end_of_game ~staker rollup_address client node)
  else unit

let rejection_with_proof ~(testnet : unit -> Testnet.t) () =
  (* We expect each player to have at least 11,000 xtz. This is enough
     to originate a rollup (1.68 xtz for one of the player), commit
     (10,000 xtz for both player), and play the game (each
     [Smart_rollup_refute] operation should be relatively cheap). *)
  let testnet = testnet () in
  let min_balance = Tez.(of_mutez_int 11_000_000_000) in
  let* client, node = Helpers.setup_octez_node ~testnet () in
  let* honest_operator = Client.gen_and_show_keys client in
  let* dishonest_operator = Client.gen_and_show_keys client in
  let* () =
    Lwt.join
      [
        Helpers.wait_for_funded_key node client min_balance honest_operator;
        Helpers.wait_for_funded_key node client min_balance dishonest_operator;
      ]
  in
  let* rollup_address =
    originate_new_rollup ~src:honest_operator.alias client
  in
  let* level = Node.get_level node in
  let fault_level = level + 5 in
  Log.info
    "Dishonest operator expected to inject an error at level %d"
    fault_level ;
  let* _rollup_nodes =
    Lwt.all
      [
        setup_l2_node
          ~name:"honest-node"
          ~operator:honest_operator.alias
          client
          node
          rollup_address;
        setup_l2_node
          ~name:"dishonest-node"
          ~loser_mode:Format.(sprintf "%d 0 0" fault_level)
          ~operator:dishonest_operator.alias
          client
          node
          rollup_address;
      ]
  in
  let* () =
    wait_for_game
      ~staker:honest_operator.public_key_hash
      rollup_address
      client
      node
  in
  let* () =
    wait_for_end_of_game
      ~staker:honest_operator.public_key_hash
      rollup_address
      client
      node
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4929
     Should the scenario checks if the game ended with the expected
     result? *)
  unit

let register ~testnet =
  Test.register
    ~__FILE__
    ~title:"Rejection with proof"
    ~tags:["rejection"]
    (rejection_with_proof ~testnet)
