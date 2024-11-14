(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Client commands
   Invocation:   dune exec tezt/tests/main.exe -- --file yes_crypto.ml
   Subject:      Test yes_crypto
*)

let team = Tag.layer1

let test_check_signature =
  Protocol.register_test
    ~__FILE__
    ~title:"Test yes crypto signatures can only be used with yes crypto checks"
    ~tags:[team; "client"; "signature"; "check"; "yes_crypto"]
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in

  Log.info
    "Check operation signed by a vanilla client is accepted by a vanilla node" ;
  let transfer =
    Client.spawn_transfer
      client
      ~amount:(Tez.of_int 1)
      ~giver:"bootstrap2"
      ~receiver:"bootstrap1"
  in
  let bake1 = Client.spawn_bake_for client in
  let* () = Process.check transfer in
  let* () = Process.check bake1 in

  Log.info "Check operation signed by a yes-client is refused by a vanilla node" ;
  let env =
    String_map.add
      Tezos_crypto.Helpers.yes_crypto_environment_variable
      "y"
      String_map.empty
  in

  let yes_transfer =
    Client.spawn_transfer
      ~env
      client
      ~amount:(Tez.of_int 1)
      ~giver:"bootstrap3"
      ~receiver:"bootstrap1"
  in
  let* () = Process.check_error yes_transfer in

  Log.info "Restart node with yes_crypto enabled" ;
  let* () = Node.kill node in
  let* () = Node.run ~env node [Node.Allow_yes_crypto] in
  let* () = Node.wait_for_ready node in

  Log.info "Check operation signed by a yes-client is accepted by a yes node" ;
  let yes_transfer2 =
    Client.spawn_transfer
      ~env
      client
      ~amount:(Tez.of_int 1)
      ~giver:"bootstrap4"
      ~receiver:"bootstrap1"
  in
  let bake2 = Client.spawn_bake_for client in
  let* () = Process.check bake2 in
  let* () = Process.check yes_transfer2 in

  Log.info
    "Check operation signed by a vanilla-client is accepted by a yes node" ;
  let transfer2 =
    Client.spawn_transfer
      client
      ~amount:(Tez.of_int 1)
      ~giver:"bootstrap5"
      ~receiver:"bootstrap1"
  in

  let bake3 = Client.spawn_bake_for client in
  let* () = Process.check bake3 in
  let* () = Process.check transfer2 in

  Log.info
    "Check that running a node with only environement variable is refused" ;
  let* () = Node.kill node in
  let* () = Node.run ~env node [] in
  let* () =
    Node.check_error
      node
      ~msg:
        (rex
           ".* Yes crypto is enabled but the option '--allow-yes-crypto' was \
            not provided.*")
  in

  return ()

let register ~protocols = test_check_signature protocols
