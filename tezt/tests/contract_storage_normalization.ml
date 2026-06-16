(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_storage_normalization.ml
   Subject:      Test the use of the unparsing-mode option when reading contract storage.
*)

let team = Tag.layer1

let test_contract_storage_normalization =
  Protocol.register_test
    ~__FILE__
    ~title:
      "Test the use of the unparsing-mode option when reading contract storage"
    ~tags:[team; "client"; "michelson"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let address_readable = sf "%S" Constant.bootstrap1.public_key_hash in
  let* address_optimized =
    Client.normalize_data
      ~mode:Optimized
      ~data:address_readable
      ~typ:"address"
      client
  in
  let address_optimized = String.trim address_optimized in
  let* _alias, contract =
    Client.originate_contract_at
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:(sf "Pair %s Unit Unit Unit" address_readable)
      client
      ["mini_scenarios"; "storage_normalization"]
      protocol
  in
  Lwt_list.iter_s
    (fun (unparsing_mode, expected) ->
      let* storage = Client.contract_storage ~unparsing_mode contract client in
      Log.info
        "Checking contract storage using unparsing mode %s"
        (Client.normalize_mode_to_string unparsing_mode) ;
      Check.(
        (String.trim storage = expected)
          string
          ~__LOC__
          ~error_msg:"expected storage %R, got %L") ;
      unit)
    [
      (Readable, sf "Pair %s Unit Unit Unit" address_readable);
      (Optimized, sf "{ %s ; Unit ; Unit ; Unit }" address_optimized);
      ( Optimized_legacy,
        sf "Pair %s (Pair Unit (Pair Unit Unit))" address_optimized );
    ]

let register ~protocols = test_contract_storage_normalization protocols
