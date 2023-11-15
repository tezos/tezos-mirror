(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Client
   Invocation:   dune exec tezt/tests/main.exe -- --file run_code.ml
   Subject:      Check that run code command to octez-client behaves correctly
*)

let test_balance_and_self_address =
  Protocol.register_test
    ~__FILE__
    ~title:"Run code with balance and self address"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 018)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  (* With no parameters, the default BALANCE is 4 000 000 ꜩ. *)
  let* stack = Client.run_code ~src:"BALANCE" ~stack:"{}" client in
  assert (stack = "{ Stack_elt mutez 4000000000000 }") ;

  (* When --balance is given, BALANCE should match the expected value. *)
  let* stack =
    Client.run_code ~balance:(Tez.of_int 1) ~src:"BALANCE" ~stack:"{}" client
  in
  assert (stack = "{ Stack_elt mutez 1000000 }") ;

  let* self_address =
    Client.originate_contract
      ~burn_cap:(Tez.of_int 1)
      ~alias:"test_contract"
      ~amount:(Tez.of_int 100)
      ~src:"bootstrap1"
      ~prg:{|parameter unit; storage unit; code {CDR; NIL operation; PAIR}|}
      client
  in

  (* When --self-address is given, SELF_ADDRESS should match with it. *)
  let* stack =
    Client.run_code ~self_address ~src:"SELF_ADDRESS" ~stack:"{}" client
  in
  assert (stack = Format.sprintf "{ Stack_elt address %S }" self_address) ;
  (* When --self-address is given, BALANCE should be equal to that of the
     given account. *)
  let* stack =
    Client.run_code ~self_address ~src:"BALANCE" ~stack:"{}" client
  in
  assert (stack = "{ Stack_elt mutez 100000000 }") ;

  (* When both --self-address and --balance are given, the BALANCE should be
     equal to the given value and SELF_ADDRESS should still match the given one. *)
  let* stack =
    Client.run_code
      ~balance:(Tez.of_int 1)
      ~self_address
      ~src:"SELF_ADDRESS"
      ~stack:"{}"
      client
  in
  assert (stack = Format.sprintf "{ Stack_elt address %S }" self_address) ;
  let* stack =
    Client.run_code
      ~balance:(Tez.of_int 1)
      ~self_address
      ~src:"BALANCE"
      ~stack:"{}"
      client
  in
  assert (stack = "{ Stack_elt mutez 1000000 }") ;
  unit

let test_source_and_sender =
  Protocol.register_test
    ~__FILE__
    ~title:"Run code with source and sender"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 018)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* bootstrap1 = Client.show_address ~alias:"bootstrap1" client in
  let* bootstrap2 = Client.show_address ~alias:"bootstrap2" client in

  (* When --payer is absent, --source sets SENDER, but SOURCE is the
     zero address. *)
  let expected_source = "0x00000000000000000000000000000000000000000000" in
  let* stack =
    Client.run_code
      ~source:"bootstrap1"
      ~src:"SOURCE"
      ~stack:"{}"
      ~mode:Optimized
      client
  in
  assert (stack = Format.sprintf "{ Stack_elt address %s }" expected_source) ;
  let* stack =
    Client.run_code ~source:"bootstrap1" ~src:"SENDER" ~stack:"{}" client
  in
  assert (
    stack = Format.sprintf "{ Stack_elt address %S }" bootstrap1.public_key_hash) ;

  (* When --source is absent, --payer sets *both* SENDER and SOURCE. *)
  let* stack =
    Client.run_code ~payer:"bootstrap1" ~src:"SOURCE" ~stack:"{}" client
  in
  assert (
    stack = Format.sprintf "{ Stack_elt address %S }" bootstrap1.public_key_hash) ;
  let* stack =
    Client.run_code ~payer:"bootstrap1" ~src:"SENDER" ~stack:"{}" client
  in
  assert (
    stack = Format.sprintf "{ Stack_elt address %S }" bootstrap1.public_key_hash) ;

  (* When both --source and --payer are given, their values may differ. *)
  let* stack =
    Client.run_code
      ~payer:"bootstrap1"
      ~source:"bootstrap2"
      ~src:"SOURCE"
      ~stack:"{}"
      client
  in
  assert (
    stack = Format.sprintf "{ Stack_elt address %S }" bootstrap1.public_key_hash) ;
  let* stack =
    Client.run_code
      ~payer:"bootstrap1"
      ~source:"bootstrap2"
      ~src:"SENDER"
      ~stack:"{}"
      client
  in
  assert (
    stack = Format.sprintf "{ Stack_elt address %S }" bootstrap2.public_key_hash) ;
  unit

let test_other_contracts =
  Protocol.register_test
    ~__FILE__
    ~title:"Run code with other_contracts"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 018)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let unused_address = "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d" in
  let* stack =
    Client.run_code
      ~src:"CONTRACT nat"
      ~stack:(Format.sprintf "{ Stack_elt address %S }" unused_address)
      ~other_contracts:(Printf.sprintf "{Contract %S nat}" unused_address)
      client
  in
  assert (
    stack
    = Format.sprintf
        "{ Stack_elt (option (contract nat)) (Some %S) }"
        unused_address) ;
  unit

let test_extra_big_maps =
  Protocol.register_test
    ~__FILE__
    ~title:"Run code with extra_big_maps"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 018)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* stack =
    Client.run_code
      ~src:"GET"
      ~stack:{|{ Stack_elt nat 42; Stack_elt (big_map nat string) 4 }|}
      ~extra_big_maps:{|{Big_map 4 nat string {Elt 42 "foobar"}}|}
      client
  in
  assert (stack = {|{ Stack_elt (option string) (Some "foobar") }|}) ;
  unit

let test_amount =
  Protocol.register_test
    ~__FILE__
    ~title:"Run code with amount"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 018)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* stack = Client.run_code ~src:"AMOUNT" ~stack:"{}" client in
  assert (stack = {|{ Stack_elt mutez 50000 }|}) ;
  let* stack =
    Client.run_code ~src:"AMOUNT" ~stack:"{}" ~amount:Tez.one client
  in
  assert (stack = {|{ Stack_elt mutez 1000000 }|}) ;
  unit

let test_level =
  Protocol.register_test
    ~__FILE__
    ~title:"Run code with level"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 018)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* stack = Client.run_code ~src:"LEVEL" ~stack:"{}" client in
  assert (stack = {|{ Stack_elt nat 0 }|}) ;
  let* stack = Client.run_code ~src:"LEVEL" ~stack:"{}" ~level:1000 client in
  assert (stack = {|{ Stack_elt nat 1000 }|}) ;
  unit

let test_now =
  Protocol.register_test
    ~__FILE__
    ~title:"Run code with now"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 018)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* stack = Client.run_code ~src:"NOW" ~stack:"{}" client in
  assert (stack = {|{ Stack_elt timestamp "1970-01-01T00:00:01Z" }|}) ;
  let now = "2023-11-07T16:39:20Z" in
  let* stack = Client.run_code ~src:"NOW" ~stack:"{}" ~now client in
  assert (stack = Format.sprintf {|{ Stack_elt timestamp %S }|} now) ;
  unit

let test_long_output =
  Protocol.register_test
    ~__FILE__
    ~title:"Run code outputing a long stack"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 018)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* stack =
    Client.run_code
      ~src:"UNPAIR 10"
      ~stack:
        {|{Stack_elt (pair nat bool string int mutez bytes (option nat) (list string) (set bool) (map string nat)) {42; False; "foo"; -24; 1000; 0x00; None; {"foo"; "bar"}; {False; True}; {Elt "bar" 42} }}|}
      client
  in
  assert (
    stack
    = {|{ Stack_elt nat 42 ;
    Stack_elt bool False ;
    Stack_elt string "foo" ;
    Stack_elt int -24 ;
    Stack_elt mutez 1000 ;
    Stack_elt bytes 0x00 ;
    Stack_elt (option nat) None ;
    Stack_elt (list string) { "foo" ; "bar" } ;
    Stack_elt (set bool) { False ; True } ;
    Stack_elt (map string nat) { Elt "bar" 42 } }|}) ;
  unit

let register ~protocols =
  test_balance_and_self_address protocols ;
  test_source_and_sender protocols ;
  test_other_contracts protocols ;
  test_extra_big_maps protocols ;
  test_amount protocols ;
  test_level protocols ;
  test_now protocols ;
  test_long_output protocols
