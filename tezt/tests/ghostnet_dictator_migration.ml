(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe  <gb.fefe@protonmail.com>                    *)
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
   Component:    Testnet dictator key
   Invocation:   dune exec tezt/tests/main.exe -- --file ghostnet_dictator_migration.ml
   Subject:      This tests that the migration from J to K on a chain whose
                 id is the one of Ghostsnet sets the contant `testnet_dictator`
                 to the expected value (i.e. a Oxhead alpha key).

                 This also test than any other migration, leave the constant
                 unset, especialy on mainnet.
*)

open Testnet_dictator

let decode_dictator json =
  Option.map JSON.as_string JSON.(json |-> "testnet_dictator" |> as_opt)

let get_dictator client =
  let* json =
    RPC.Client.call client @@ RPC.get_chain_block_context_constants ()
  in
  return (decode_dictator json)

let check_dictator client expected_dictator =
  let* dictator = get_dictator client in
  Check.((dictator = expected_dictator) (option string))
    ~error_msg:"expected testnet dictator = %R, got %L" ;
  unit

let init chain_id ~from_protocol ~to_protocol =
  let user_activated_upgrades = [(11, to_protocol)] in
  let patch_config, timestamp =
    match chain_id with
    | Chain_id_mainnet ->
        ( Some
            (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
               user_activated_upgrades),
          None )
    | Chain_id_ghostnet ->
        ( Some
            (Node.Config_file.set_ghostnet_sandbox_network
               ~user_activated_upgrades
               ()),
          Some
            (* Ghostnet was started at 2022-01-25 (as Ithacanet) and the
               default timestamp is one year ago. This ad-hoc case could
               be removed after 2023-01-25. *)
            (Client.At
               (Tezos_base.Time.System.of_notation_exn "2022-01-26T15:00:00Z"))
        )
  in
  let* node = Node.init ?patch_config [Synchronisation_threshold 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  let parameters =
    [(["blocks_per_cycle"], `Int 4); (["cycles_per_voting_period"], `Int 2)]
  in
  let parameters =
    if Protocol.number from_protocol >= 014 then
      parameters @ [(["nonce_revelation_threshold"], `Int 2)]
    else parameters
  in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (from_protocol, None)) parameters
  in
  let* () =
    Client.activate_protocol
      ?timestamp
      ~protocol:from_protocol
      ~parameter_file
      client
  in
  return (node, client)

let may_apply opt f = match opt with None -> Lwt.return_unit | Some v -> f v

let register_migration_test chain_id =
  Protocol.register_test
    ~__FILE__
    ~title:(sf "testnet dictator (%s, migration)" (string_of_chain_id chain_id))
    ~tags:["amendment"; string_of_chain_id chain_id; "migration"]
    ~supports:(From_protocol 014)
  @@ fun to_protocol ->
  may_apply (Protocol.previous_protocol to_protocol) @@ fun from_protocol ->
  let* node, client = init chain_id ~from_protocol ~to_protocol in
  let* () = repeat 10 (fun () -> bake node client) in
  Log.info "Checking that migration occurred..." ;
  let* () =
    Voting.check_protocols
      client
      (Protocol.hash from_protocol, Protocol.hash to_protocol)
  in
  let expected_dictator, expected_period =
    match (chain_id, to_protocol) with
    | Chain_id_ghostnet, Kathmandu ->
        ( Some "tz1Xf8zdT3DbAX9cHw3c3CXh79rc4nK4gCe8",
          {
            Voting.index = 1;
            kind = Proposal;
            start_position = 8;
            position = 2;
            remaining = 1;
          } )
    | Chain_id_ghostnet, Lima ->
        ( None,
          {
            Voting.index = 1;
            kind = Proposal;
            start_position = 3;
            position = 7;
            remaining = 0;
          } )
    | _, _ ->
        ( None,
          {
            Voting.index = 1;
            kind = Proposal;
            start_position = 8;
            position = 2;
            remaining = 5;
          } )
  in

  let* () = check_dictator client expected_dictator in
  let* () = Voting.check_current_period client expected_period in
  return ()

let register ~protocols =
  (* Testing migration with the chain_id of Ghostnet.
     From J to K it should set the constant `testnet_dictator`.
     On any other migrations, it should leave it unset *)
  register_migration_test Chain_id_ghostnet protocols ;
  (* Testing migration with the chain_id of Mainnet.
     It should leave `testnet_dictator` unset. *)
  register_migration_test Chain_id_mainnet protocols
