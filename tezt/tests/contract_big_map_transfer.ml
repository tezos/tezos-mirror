(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_big_map_transfer.ml
   Subject:      Tests sending big_maps from a contract to another
*)

let team = Tag.layer1

let originate_contract ~client ~protocol ~filename ~storage =
  let* _alias, address =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:storage
      ~burn_cap:Tez.one
      ~force:true
      client
      ["big_maps"; filename]
      protocol
  in
  return address

let call_contract ~client ~address ~arg =
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:"bootstrap1"
      ~receiver:address
      ~arg
      ~hooks:Tezos_regression.hooks
      client
  in
  unit

let test_big_map_transfer =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test sending a big map from a contract to another"
    ~tags:[team; "client"; "michelson"; "big_map"; "transfer"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  Lwt_list.iter_s
    (fun (sender_filename, sender_storage) ->
      Lwt_list.iter_s
        (fun (receiver_filename, receiver_storage) ->
          let () =
            Regression.capture
              (sf
                 "Test transferring big map from %S to %S"
                 sender_filename
                 receiver_filename)
          in
          let* receiver =
            originate_contract
              ~client
              ~protocol
              ~filename:receiver_filename
              ~storage:receiver_storage
          in
          let* sender =
            originate_contract
              ~client
              ~protocol
              ~filename:sender_filename
              ~storage:sender_storage
          in
          let* () =
            call_contract ~client ~address:sender ~arg:(sf "%S" receiver)
          in
          unit)
        [
          ("receiver_drop", "Unit");
          ("receiver_store", "{}");
          ("receiver_store_updated", "{}");
        ])
    [
      ("sender_fresh", "Unit");
      ("sender_stored", "{Elt \"d\" 0x; }");
      ("sender_stored_updated", "{Elt \"b\" 0x; Elt \"d\" 0x; }");
    ]

let test_big_map_internal_origination =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test sending a big map during contract internal origination"
    ~tags:[team; "client"; "michelson"; "big_map"; "origination"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* address =
    originate_contract
      ~client
      ~protocol
      ~filename:"originator"
      ~storage:"{Elt \"b\" 0x; Elt \"d\" 0x; }"
  in
  let* () = call_contract ~client ~address ~arg:"Unit" in
  unit

let register ~protocols =
  test_big_map_transfer protocols ;
  test_big_map_internal_origination protocols
