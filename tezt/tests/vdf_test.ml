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
   Component:    Basic
   Invocation:   dune exec tezt/tests/main.exe -- --file vdf_test.ml
   Subject:      Test that an instance of the VDF computation daemon can
                 compute and inject the VDF at the right time and that the
                 [get_seed_status] RPC reports the seed computation
                 status as expected throughout the cycle.
*)

(* Stripped down version of [Seed_storage.seed_computation_status] *)
type seed_computation_status =
  | Nonce_revelation_stage
  | Vdf_revelation_stage
  | Computation_finished

let get_seed_computation_status client level =
  let* seed_status =
    RPC.Seed.get_seed_status ~block:(string_of_int level) client
  in
  return
    (match List.map fst (JSON.as_object seed_status) with
    | ["nonce_revelation_stage"] -> Nonce_revelation_stage
    | ["seed_discriminant"; "seed_challenge"] -> Vdf_revelation_stage
    | ["computation_finished"] -> Computation_finished
    | _ -> assert false)

let assert_computation_status client level status =
  let* current_status = get_seed_computation_status client level in
  return @@ assert (current_status = status)

let assert_level actual expected =
  if actual <> expected then (
    Log.info "Expected to be at level %d, actually at level %d" expected actual ;
    assert false)
  else ()

(* Bakes at most `max_level - min_starting_level + 1` blocks, starting from
 * a level not lower than `min_starting_level` and finishing exactly
 * at `max_level` *)
let bake_until min_starting_level max_level client node =
  let rec loop level =
    if level < max_level then
      let* () = Client.bake_for client in
      let* level = Node.wait_for_level node (level + 1) in
      loop level
    else return level
  in
  let* level = loop min_starting_level in
  let* level = Node.wait_for_level node level in
  assert_level level max_level ;
  return level

(* Checks that we are in the last block of the nonce revelation period,
 * sets an event handler for the VDF revelation operation, then bakes
 * the whole of the VDF revelation period. Checks that (at least one)
 * VDF revelation operation has been injected *)
let bake_vdf_revelation_period level max_level client node vdf_baker =
  let injected = ref false in

  let* () = assert_computation_status client level Nonce_revelation_stage in
  Vdf.on_event vdf_baker (fun Vdf.{name; _} ->
      if name = "vdf_revelation_injected.v0" then injected := true) ;
  let* () = Client.bake_for client in
  let* level = Node.wait_for_level node (level + 1) in
  let* () = assert_computation_status client level Vdf_revelation_stage in

  let* _level = bake_until level max_level client node in
  return @@ assert !injected

let check_cycle (blocks_per_cycle, nonce_revelation_threshold) starting_level
    client node vdf_baker =
  (* Check that at the beginning of the cycle we are in the nonce
     revelation period *)
  let* level = Node.wait_for_level node starting_level in
  assert_level level starting_level ;
  let* () = assert_computation_status client level Nonce_revelation_stage in

  (* Bake until the end of the nonce revelation period *)
  let* level =
    bake_until
      level
      (starting_level + nonce_revelation_threshold - 1)
      client
      node
  in

  (* Bake throughout the VDF revelation period, checking that a VDF revelation
   * operation is injected at some point. Check that the seed computation
   * status is set to [Computation_finished] at the end of the period. *)
  let* () =
    bake_vdf_revelation_period
      level
      (starting_level + blocks_per_cycle - 1)
      client
      node
      vdf_baker
  in
  let* level =
    Node.wait_for_level node (starting_level + blocks_per_cycle - 1)
  in
  assert_level level (starting_level + blocks_per_cycle - 1) ;
  let* () = assert_computation_status client level Computation_finished in

  (* Bake one more block, and check that it is the first block of
   * the following cycle *)
  let* () = Client.bake_for client in
  let* level = Node.wait_for_level node (starting_level + blocks_per_cycle) in
  assert_level level (starting_level + blocks_per_cycle) ;
  return level

let check_n_cycles n constants starting_level client node vdf_baker =
  let rec loop n level =
    if n > 0 then
      let* level = check_cycle constants level client node vdf_baker in
      loop (n - 1) level
    else return level
  in
  loop n starting_level

(* In total, [test_vdf] bakes `2 * (n_cycles + 1)` cycles *)
let n_cycles = 5

let test_vdf : Protocol.t list -> unit =
  (* [check_n_cycles] requires that the starting_level is the beginning of
   * a new cycle. Since that's not the case when the VDF daemon is launched,
   * the first cycle is checked here directly and the following cycles are
   * checked using [check_n_cycles] *)
  Protocol.register_test ~__FILE__ ~title:"VDF daemon" ~tags:["vdf"]
  @@ fun protocol ->
  let* node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol ~protocol client
  and* vdf_baker = Vdf.init ~protocol node in

  let* constants = RPC.get_constants client in
  let* blocks_per_cycle =
    return JSON.(constants |-> "blocks_per_cycle" |> as_int)
  in
  let* nonce_revelation_threshold =
    return JSON.(constants |-> "nonce_revelation_threshold" |> as_int)
  in

  (* Bake and check that we are in the nonce revelation period *)
  let* () = Client.bake_for client in
  let* level = Node.wait_for_level node 1 in
  let* () = assert_computation_status client level Nonce_revelation_stage in

  (* Bake until the end of the nonce revelation period *)
  let* level = bake_until level nonce_revelation_threshold client node in

  (* Bake until the end of the cycle, checking that a VDF revelation
     operation was injected during the VDF revelation period and that
     the computation status is set to finished at the end of the cycle *)
  let* () =
    bake_vdf_revelation_period level blocks_per_cycle client node vdf_baker
  in
  let* level = Node.wait_for_level node blocks_per_cycle in
  let* () = assert_computation_status client level Computation_finished in

  (* Bake, check that we are in the first block of the following cycle *)
  let* () = Client.bake_for client in
  let* level = Node.wait_for_level node (blocks_per_cycle + 1) in
  assert_level level (blocks_per_cycle + 1) ;

  (* Check correct behaviour for the following cycles *)
  let* level =
    check_n_cycles
      n_cycles
      (blocks_per_cycle, nonce_revelation_threshold)
      level
      client
      node
      vdf_baker
  in

  (* Kill the VDF daemon and bake one cycle with no VDF submission.
     Check that since no VDF daemon is running the computation status is
     never "finished" in this cycle. *)
  let* () = Vdf.terminate vdf_baker in
  let* () =
    repeat blocks_per_cycle (fun () ->
        let* () = Client.bake_for client in
        let* level = Node.wait_for_level node 1 in
        let* current_status = get_seed_computation_status client level in
        return @@ assert (current_status <> Computation_finished))
  in
  let* level = Node.wait_for_level node (level + blocks_per_cycle) in

  (* Restart a VDF daemon and check correct behaviour after a RANDAO cycle *)
  let* vdf_baker = Vdf.init ~protocol node in
  assert_level level (((n_cycles + 2) * blocks_per_cycle) + 1) ;
  let* _level =
    check_n_cycles
      n_cycles
      (blocks_per_cycle, nonce_revelation_threshold)
      level
      client
      node
      vdf_baker
  in

  Vdf.terminate vdf_baker

let register ~protocols = test_vdf protocols
