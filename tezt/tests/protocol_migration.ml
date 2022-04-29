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
   Component:    Protocol
   Invocation:   dune exec tezt/tests/main.exe -- protocol migration
   Subject:      Checks the migration of protocol alpha
*)

(* Migration to Tenderbake is only supported after the first cycle,
   therefore at [migration_level >= blocks_per_cycle].  *)
let test_protocol_migration ~blocks_per_cycle ~migration_level ~migrate_from
    ~migrate_to =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "protocol migration at level %d" migration_level)
    ~tags:["protocol"; "migration"; "sandbox"]
  @@ fun () ->
  assert (migration_level >= blocks_per_cycle) ;
  let node = Node.create [] in
  let* () = Node.config_init node [] in
  Node.Config_file.(
    update
      node
      (set_sandbox_network_with_user_activated_upgrades
         [(migration_level, migrate_to)])) ;
  Log.info "Node starting" ;
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  Log.info "Node initialized" ;
  let* client = Client.(init ~endpoint:(Node node) ()) in
  let* () = Client.activate_protocol ~protocol:migrate_from client in
  Log.info "Protocol activated" ;
  (* Bake until migration *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  (* Ensure that we did migrate *)
  let* migration_block = RPC.get_block_metadata ~block:"2" client in
  let protocol = JSON.(migration_block |-> "protocol" |> as_string) in
  Log.info "checking migration block consistency" ;
  Check.(
    (protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  let next_protocol = JSON.(migration_block |-> "next_protocol" |> as_string) in
  Check.(
    (next_protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  (* Test that we can still bake after migration *)
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in
  unit

(* Test all levels for one cycle, after the first cycle. *)
let test_migration_for_whole_cycle ~migrate_from ~migrate_to =
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  for migration_level = blocks_per_cycle to 2 * blocks_per_cycle do
    test_protocol_migration
      ~blocks_per_cycle
      ~migration_level
      ~migrate_from
      ~migrate_to
  done

let register ~migrate_from ~migrate_to =
  test_migration_for_whole_cycle ~migrate_from ~migrate_to
