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

let get_seed_computation_status ?(info = false) client level =
  let* seed_status =
    RPC.Seed.get_seed_status ~block:(string_of_int level) client
  in
  let status =
    match List.map fst (JSON.as_object seed_status) with
    | ["nonce_revelation_stage"] -> Nonce_revelation_stage
    | ["seed_discriminant"; "seed_challenge"] -> Vdf_revelation_stage
    | ["computation_finished"] -> Computation_finished
    | _ -> assert false
  in
  let pp_status = function
    | Nonce_revelation_stage -> "nonce revelation stage"
    | Vdf_revelation_stage -> "vdf revelation stage"
    | Computation_finished -> "computation finished"
  in
  if info then Log.info "At level %d we are in %s" level (pp_status status) ;
  return status

let assert_computation_status ?(info = false) ?(assert_is_not = false)
    nonce_revelation_threshold client status =
  let comp = if assert_is_not then ( <> ) else ( = ) in
  let* level =
    RPC.Client.call client @@ RPC.get_chain_block_helper_current_level ()
  in
  let* current_status = get_seed_computation_status ~info client level.level in
  (if current_status = Nonce_revelation_stage then
   (* For levels in the nonce revelation stage, we also check the consistency
    * of [Vdf.Helpers.is_in_nonce_revelation_stage] with
    * the [Seed_computation] RPC. *)
   let nonce_revelation_threshold = Int32.of_int nonce_revelation_threshold in
   if
     not
       (Vdf.Helpers.is_in_nonce_revelation_stage
          ~nonce_revelation_threshold
          ~level)
   then
     failwith
       (Printf.sprintf
          "Vdf.Helpers.is_in_nonce_revelation_stage is inconsistent with the \
           Seed_computation RPC: returned false on level %d"
          level.level)) ;
  return @@ assert (comp current_status status)

let assert_not_computation_status =
  assert_computation_status ~assert_is_not:true

let assert_level client actual expected =
  let* level =
    RPC.Client.call client @@ RPC.get_chain_block_helper_current_level ()
  in
  if actual <> expected || level.level <> expected then (
    Log.info "Expected to be at level %d, actually at level %d" expected actual ;
    return @@ assert false)
  else Lwt.return_unit

let init_vdf_event_listener vdf_baker injected =
  Vdf.on_event vdf_baker (fun Vdf.{name; _} ->
      if name = "vdf_revelation_injected.v0" then injected := true)

(* Bakes at most [max_level - min_starting_level + 1] blocks, starting from
 * a level not lower than [min_starting_level] and finishing exactly
 * at [max_level].
 * Optionally checks that the computation status is never [Computation_finished]
 * (used when checking a cycle with no VDF daemon running) *)
let bake_until min_starting_level max_level client node status_check
    nonce_revelation_threshold =
  let rec loop level =
    if level < max_level then
      let* () = Client.bake_for client in
      let* level = Node.wait_for_level node (level + 1) in
      let* () =
        if status_check then
          assert_not_computation_status
            nonce_revelation_threshold
            client
            Computation_finished
        else Lwt.return_unit
      in
      loop level
    else return level
  in
  let* level = Node.wait_for_level node min_starting_level in
  let* level = loop level in
  let* level = Node.wait_for_level node level in
  let* () = assert_level client level max_level in
  return level

(* Checks that we are in the last block of the nonce revelation stage,
 * sets an event handler for the VDF revelation operation, then bakes
 * the whole of the VDF revelation stage. Checks that (at least one)
 * VDF revelation operation has been injected *)
let bake_vdf_revelation_stage nonce_revelation_threshold level max_level client
    node injected =
  injected := false ;
  let* () =
    assert_computation_status
      nonce_revelation_threshold
      client
      Nonce_revelation_stage
  in
  let* () = Client.bake_for client in
  let* level = Node.wait_for_level node (level + 1) in
  let* () =
    assert_computation_status
      nonce_revelation_threshold
      client
      Vdf_revelation_stage
  in
  let* _level =
    bake_until level max_level client node false nonce_revelation_threshold
  in
  return @@ assert !injected

let check_cycle (blocks_per_cycle, nonce_revelation_threshold) starting_level
    client node injected =
  (* Check that at the beginning of the cycle we are in the nonce
     revelation stage *)
  let* level = Node.wait_for_level node starting_level in
  let* () = assert_level client level starting_level in
  let* () =
    assert_computation_status
      nonce_revelation_threshold
      client
      Nonce_revelation_stage
  in

  (* Bake until the end of the nonce revelation stage *)
  let* level =
    bake_until
      level
      (starting_level + nonce_revelation_threshold - 1)
      client
      node
      false
      nonce_revelation_threshold
  in

  (* Bake throughout the VDF revelation stage, checking that a VDF revelation
   * operation is injected at some point. Check that the seed computation
   * status is set to [Computation_finished] at the end of the stage. *)
  let* () =
    bake_vdf_revelation_stage
      nonce_revelation_threshold
      level
      (starting_level + blocks_per_cycle - 1)
      client
      node
      injected
  in
  let* level =
    Node.wait_for_level node (starting_level + blocks_per_cycle - 1)
  in
  let* () = assert_level client level (starting_level + blocks_per_cycle - 1) in
  let* () =
    assert_computation_status
      nonce_revelation_threshold
      client
      Computation_finished
  in

  (* Bake one more block, and check that it is the first block of
   * the following cycle *)
  let* () = Client.bake_for client in
  let* level = Node.wait_for_level node (starting_level + blocks_per_cycle) in
  let* () = assert_level client level (starting_level + blocks_per_cycle) in
  return level

let check_n_cycles n constants starting_level client node injected =
  let rec loop n level =
    if n > 0 then
      let* level = check_cycle constants level client node injected in
      loop (n - 1) level
    else return level
  in
  loop n starting_level

(* In total, [test_vdf] bakes [2 * (n_cycles + 1)] cycles *)
let n_cycles = 3

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

  (* Track whether a VDF revelation has been injected during the correct stage.
   * It is set to [false] at the beginning of [bake_vdf_revelation_stage] and
   * to [true] by a listener for [vdf_revelation_injected] events. *)
  let injected = ref false in
  init_vdf_event_listener vdf_baker injected ;

  let* constants =
    RPC.Client.call client @@ RPC.get_chain_block_context_constants ()
  in
  let* blocks_per_cycle =
    return JSON.(constants |-> "blocks_per_cycle" |> as_int)
  in
  let* nonce_revelation_threshold =
    return JSON.(constants |-> "nonce_revelation_threshold" |> as_int)
  in

  (* Bake and check that we are in the nonce revelation stage *)
  let* () = Client.bake_for client in
  let* level = Node.wait_for_level node 1 in
  let* () =
    assert_computation_status
      nonce_revelation_threshold
      client
      Nonce_revelation_stage
  in

  (* Bake until the end of the nonce revelation stage *)
  let* level =
    bake_until
      level
      nonce_revelation_threshold
      client
      node
      false
      nonce_revelation_threshold
  in

  (* Bake until the end of the cycle, checking that a VDF revelation
     operation was injected during the VDF revelation stage and that
     the computation status is set to finished at the end of the cycle *)
  let* () =
    bake_vdf_revelation_stage
      nonce_revelation_threshold
      level
      blocks_per_cycle
      client
      node
      injected
  in
  let* _level = Node.wait_for_level node blocks_per_cycle in
  let* () =
    assert_computation_status
      nonce_revelation_threshold
      client
      Computation_finished
  in

  (* Bake, check that we are in the first block of the following cycle *)
  let* () = Client.bake_for client in
  let* level = Node.wait_for_level node (blocks_per_cycle + 1) in
  let* () = assert_level client level (blocks_per_cycle + 1) in

  (* Check correct behaviour for the following cycles *)
  let* level =
    check_n_cycles
      n_cycles
      (blocks_per_cycle, nonce_revelation_threshold)
      level
      client
      node
      injected
  in

  (* Kill the VDF daemon and bake one cycle with no VDF submission.
     Check that since no VDF daemon is running the computation status is
     never "finished" in this cycle. *)
  let* () = Vdf.terminate vdf_baker in
  let* () =
    assert_level client level ((blocks_per_cycle * (n_cycles + 1)) + 1)
  in
  let* level =
    bake_until
      level
      ((blocks_per_cycle * (n_cycles + 2)) + 1)
      client
      node
      true
      nonce_revelation_threshold
  in

  (* Bake through most of a new cycle and restart the VDF daemon right before
   * the end of the cycle so that the VDF is computed too late for injection. *)
  let* level =
    bake_until
      level
      ((blocks_per_cycle * (n_cycles + 3)) - 1)
      client
      node
      true
      nonce_revelation_threshold
  in
  let* vdf_baker = Vdf.init ~protocol node in
  init_vdf_event_listener vdf_baker injected ;
  let* vdf_baker2 = Vdf.init ~protocol node in
  init_vdf_event_listener vdf_baker2 injected ;

  (* Bake to the end of the cycle and check that the VDF was not injected. *)
  let* level =
    bake_until
      level
      ((blocks_per_cycle * (n_cycles + 3)) + 1)
      client
      node
      true
      nonce_revelation_threshold
  in

  (* Check correct behaviour for another [n_cycles] after the RANDAO cycle and
   * the failed injection cycle. *)
  let* _level =
    check_n_cycles
      n_cycles
      (blocks_per_cycle, nonce_revelation_threshold)
      level
      client
      node
      injected
  in

  let* () = Vdf.terminate vdf_baker in
  let* () = Vdf.terminate vdf_baker2 in
  Lwt.return_unit

let register ~protocols = test_vdf protocols
