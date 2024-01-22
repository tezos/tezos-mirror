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
   Component:    Basic
   Invocation:   dune exec tezt/tests/main.exe -- --file user_activated_upgrade.ml
   Subject:      .
*)

let test_metadata_consistency ~migrate_from ~migrate_to =
  Test.register
    ~__FILE__
    ~title:"metadata consistency"
    ~tags:["rpc"; "metadata"; "migration"]
  @@ fun () ->
  let migration_level = 3 in
  let* node =
    Node.init
      ~patch_config:
        (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
           [(migration_level, migrate_to)])
      []
  in
  let* client = Client.(init ~endpoint:(Node node) ()) in
  let* () = Client.activate_protocol ~protocol:migrate_from client in
  let* () =
    repeat (migration_level - 1) (fun () -> Client.bake_for_and_wait client)
  in
  let* non_migration_block =
    Client.RPC.call client
    @@ RPC.get_chain_block_metadata
         ~block:(string_of_int (migration_level - 1))
         ()
  in
  Log.info "checking consistency of block at level %d" (migration_level - 1) ;
  Check.(
    (non_migration_block.protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (non_migration_block.next_protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  Log.info "checking consistency of block at level %d" migration_level ;
  let* migration_block =
    Client.RPC.call client
    @@ RPC.get_chain_block_metadata ~block:(string_of_int migration_level) ()
  in
  Check.(
    (migration_block.protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (migration_block.next_protocol = Protocol.hash migrate_to)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  (* We call the RPC again to make sure that no side-effect occured
     that would break the next_protocol expected field. *)
  Log.info
    "checking consistency of block at level %d (again)"
    (migration_level - 1) ;
  let* non_migration_block =
    Client.RPC.call client
    @@ RPC.get_chain_block_metadata
         ~block:(string_of_int (migration_level - 1))
         ()
  in
  Check.(
    (non_migration_block.protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (non_migration_block.next_protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  unit

let register ~migrate_from ~migrate_to =
  test_metadata_consistency ~migrate_from ~migrate_to
