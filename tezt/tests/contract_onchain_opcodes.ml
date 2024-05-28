(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_onchain_opcodes.ml
   Subject:      Tests for individual opcodes that require origination.
*)

let hooks = Tezos_regression.hooks

let quote s = sf "%S" s

let originate ?(src = "bootstrap1") ?storage ?(amount = Tez.zero) ?alias
    ?(burn_cap = Tez.of_int 10) client protocol script_name =
  (* We refer to contracts by address only so for simplicity we can
     just override aliases by setting [~force:true] *)
  let* _alias, address =
    Client.originate_contract_at
      ~hooks
      ?init:storage
      ~force:true
      ~burn_cap
      ~amount
      ~src
      ?alias
      client
      script_name
      protocol
  in
  return address

let spawn_originate ?(src = "bootstrap1") ?storage ?(amount = Tez.zero) ?alias
    ?(burn_cap = Tez.of_int 10) client protocol script_name =
  (* We refer to contracts by address only so for simplicity we can
     just override aliases by setting [~force:true] *)
  let _alias, process =
    Client.spawn_originate_contract_at
      ~hooks
      ?init:storage
      ~force:true
      ~burn_cap
      ~amount
      ~src
      ?alias
      client
      script_name
      protocol
  in
  process

let transfer ?(amount = Tez.zero) ?(giver = "bootstrap1")
    ?(burn_cap = Tez.of_int 10) ~contract ?(arg = "Unit") =
  Client.transfer ~hooks ~burn_cap ~amount ~giver ~receiver:contract ~arg

let spawn_transfer ?(amount = Tez.zero) ?(giver = "bootstrap1")
    ?(burn_cap = Tez.of_int 10) ~contract ?(arg = "Unit") =
  Client.spawn_transfer ~hooks ~burn_cap ~amount ~giver ~receiver:contract ~arg

let get_storage client ~contract =
  let* storage = Client.contract_storage ~hooks contract client in
  return (String.trim storage)

let get_storage_json client ~contract =
  Client.RPC.call ~hooks client
  @@ RPC.get_chain_block_context_contract_storage ~id:contract ()

let check_storage ~__LOC__ client ~contract expected_storage =
  let* storage = get_storage client ~contract in
  Check.(
    (storage = expected_storage)
      string
      ~__LOC__
      ~error_msg:"Expected storage %R, got %L") ;
  unit

let check_balance ~__LOC__ client ~contract expected_balance =
  let* balance = Client.get_balance_for client ~account:contract in
  Check.(
    (balance = expected_balance)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected balance %R, got %L") ;
  unit

let script_failwith =
  Process.check_error ~msg:(rex "script reached FAILWITH instruction")

let test_store_input protocol client =
  let* contract =
    originate client protocol ["opcodes"; "store_input"] ~storage:{|""|}
  in
  let* () = transfer client ~contract ~arg:{|"abcdefg"|} in
  let* () = check_storage ~__LOC__ client ~contract {|"abcdefg"|} in
  let* () = transfer client ~contract ~arg:{|"xyz"|} in
  let* () = check_storage ~__LOC__ client ~contract {|"xyz"|} in
  unit

let test_transfer_amount protocol client =
  let* contract =
    originate client protocol ["opcodes"; "transfer_amount"] ~storage:"0"
  in
  let amount = Tez.of_int 500 in
  let amount_s = Tez.to_mutez amount |> string_of_int in
  let* () = transfer client ~contract ~amount in
  let* () = check_storage ~__LOC__ client ~contract amount_s in
  unit

let test_now protocol client =
  let* contract =
    originate
      client
      protocol
      ["opcodes"; "store_now"]
      ~storage:{|"2017-07-13T09:19:01Z"|}
  in
  let* () = transfer client ~contract in
  let parse_protocol_time timestamp =
    match Tezos_base.Time.Protocol.of_notation timestamp with
    | Some t -> t
    | None ->
        Test.fail
          "Could not parse %S with [Tezos_base.Time.Protocol.of_notation]"
          timestamp
  in
  let* expected_now =
    (* Returns the timestamp of next-to-last block, offset by
       the minimum time between blocks. *)
    let* minimal_block_delay =
      let* constants =
        Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
      in
      return JSON.(constants |-> "minimal_block_delay" |> as_int)
    in
    let* shell_timestamp =
      let* shell_timestamp_s = Client.get_timestamp ~block:"head~1" client in
      return (parse_protocol_time shell_timestamp_s)
    in
    return
      (Tezos_base.Time.Protocol.add
         shell_timestamp
         (Int64.of_int minimal_block_delay))
  in
  let* now_in_storage = get_storage_json client ~contract in
  let now_in_storage =
    JSON.(now_in_storage |-> "string" |> as_string) |> parse_protocol_time
  in
  Check.(
    is_true
      (Tezos_base.Time.Protocol.equal now_in_storage expected_now)
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  unit

let test_transfer_tokens protocol client =
  let* transfer_contract1 =
    originate
      ~amount:(Tez.of_int 100)
      client
      protocol
      ["opcodes"; "noop"]
      ~alias:"test_transfer_contract1"
  in
  let* transfer_contract2 =
    originate
      ~amount:(Tez.of_int 20)
      client
      protocol
      ["opcodes"; "noop"]
      ~alias:"test_transfer_contract2"
  in
  (* [transfer_tokens] transfers 100 tez to the contract in parameter *)
  let* contract =
    originate
      ~amount:(Tez.of_int 1000)
      client
      protocol
      ["opcodes"; "transfer_tokens"]
  in
  (* Check initial balances *)
  let* () =
    check_balance ~__LOC__ client ~contract:transfer_contract1 (Tez.of_int 100)
  in
  let* () =
    check_balance ~__LOC__ client ~contract:transfer_contract2 (Tez.of_int 20)
  in
  let* () = check_balance ~__LOC__ client ~contract (Tez.of_int 1000) in
  (* Make transfers *)
  let* () = transfer client ~contract ~arg:(quote transfer_contract1) in
  let* () =
    check_balance ~__LOC__ client ~contract:transfer_contract1 (Tez.of_int 200)
  in
  let* () = transfer client ~contract ~arg:(quote transfer_contract2) in
  let* () =
    check_balance ~__LOC__ client ~contract:transfer_contract2 (Tez.of_int 120)
  in
  unit

let test_self protocol client =
  let* contract =
    originate
      client
      protocol
      ["opcodes"; "self"]
      ~storage:{|"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"|}
  in
  let* () = transfer client ~contract in
  let* () = check_storage ~__LOC__ client ~contract (quote contract) in
  unit

let test_contract_fails protocol client =
  (* This tests the [CONTRACT] instruction. The [opcodes/contract] script takes
     an address in parameter and checks that it corresponds to a contract of
     type [unit]. We pass in the address of the contract itself
     (which has a type of [address]), and therefore the transfer fails. *)
  let* contract = originate client protocol ["opcodes"; "contract"] in
  spawn_transfer client ~contract ~arg:(quote contract)
  |> Process.check_error ~msg:(rex "script reached FAILWITH instruction")

let test_source protocol client =
  let* proxy = originate client ~alias:"proxy" protocol ["opcodes"; "proxy"] in
  let* source =
    originate
      client
      protocol
      ["opcodes"; "source"]
      ~storage:(quote Constant.bootstrap4.public_key_hash)
  in
  (* direct transfer to the contract *)
  let transfer_source = Constant.bootstrap2.public_key_hash in
  let* () = transfer client ~giver:transfer_source ~contract:source in
  let* () =
    check_storage ~__LOC__ client ~contract:source (quote transfer_source)
  in
  (* indirect transfer to the contract through proxy *)
  let* () =
    transfer client ~giver:transfer_source ~contract:proxy ~arg:(quote source)
  in
  check_storage ~__LOC__ client ~contract:source (quote transfer_source)

let test_sender protocol client =
  let* proxy = originate client ~alias:"proxy" protocol ["opcodes"; "proxy"] in
  let* sender =
    originate
      client
      protocol
      ["opcodes"; "sender"]
      ~storage:(quote Constant.bootstrap4.public_key_hash)
  in
  (* direct transfer to the contract *)
  let transfer_sender = Constant.bootstrap2.public_key_hash in
  let* () = transfer client ~giver:transfer_sender ~contract:sender in
  let* () =
    check_storage ~__LOC__ client ~contract:sender (quote transfer_sender)
  in
  (* indirect transfer to the contract through proxy *)
  let* () =
    transfer client ~giver:transfer_sender ~contract:proxy ~arg:(quote sender)
  in
  check_storage ~__LOC__ client ~contract:sender (quote proxy)

let test_slice protocol client =
  let* contract =
    originate
      client
      (* [slices.tz] needs a non-zero balance to transfer in the success case to
         the contract denoted by its storage. *)
      ~amount:Tez.one
      ~storage:{|"sppk7dBPqMPjDjXgKbb5f7V3PuKUrA4Zuwc3c3H7XqQerqPUWbK7Hna"|}
      protocol
      ["opcodes"; "slices"]
  in
  let build_arg ~bytes ~signature = sf {|(Pair %s "%s")|} bytes signature in
  let* () =
    Lwt_list.iter_s
      (fun (bytes, signature) ->
        spawn_transfer client ~contract ~arg:(build_arg ~bytes ~signature)
        |> Process.check_error ~msg:(rex "script reached FAILWITH instruction"))
      [
        ( "0xe009ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2de22d01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150742eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345",
          "p2sigsceCzcDw2AeYDzUonj4JT341WC9Px4wdhHBxbZcG1FhfqFVuG7f2fGCzrEHSAZgrsrQWpxduDPk9qZRgrpzwJnSHC3gZJ"
        );
        ( "0xeaa9ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2de22d01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150742eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345",
          "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm"
        );
        ( "0xe009ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2deaad01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150742eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345",
          "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm"
        );
        ( "0xe009ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2de22d01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150733eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345",
          "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm"
        );
        ( "0xe009ab79e8b84ef0",
          "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm"
        );
      ]
  in
  transfer
    client
    ~contract
    ~arg:
      (build_arg
         ~bytes:
           "0xe009ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2de22d01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150742eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345"
         ~signature:
           "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm")

let test_split_string protocol client =
  let* contract =
    originate client ~storage:"{}" protocol ["opcodes"; "split_string"]
  in
  let* () = transfer client ~contract ~arg:{|"abc"|} in
  let* () = check_storage ~__LOC__ client ~contract {|{ "a" ; "b" ; "c" }|} in
  let* () = transfer client ~contract ~arg:{|"def"|} in
  check_storage
    ~__LOC__
    client
    ~contract
    {|{ "a" ; "b" ; "c" ; "d" ; "e" ; "f" }|}

let test_split_bytes protocol client =
  let* contract =
    originate client ~storage:"{}" protocol ["opcodes"; "split_bytes"]
  in
  let* () = transfer client ~contract ~arg:{|0xaabbcc|} in
  let* () =
    check_storage ~__LOC__ client ~contract {|{ 0xaa ; 0xbb ; 0xcc }|}
  in

  let* () = transfer client ~contract ~arg:{|0xddeeff|} in
  check_storage
    ~__LOC__
    client
    ~contract
    {|{ 0xaa ; 0xbb ; 0xcc ; 0xdd ; 0xee ; 0xff }|}

let test_set_delegate protocol client =
  let* contract = originate client protocol ["opcodes"; "set_delegate"] in
  let check_delegate ~__LOC__ expected_delegate =
    let* delegate = Client.get_delegate client ~src:contract in
    Check.(
      (delegate = expected_delegate)
        (option string)
        ~__LOC__
        ~error_msg:"Expected delegate %R, got %L") ;
    unit
  in
  let* () = check_delegate ~__LOC__ None in
  let bootstrap5 = Constant.bootstrap5.public_key_hash in
  let* () = transfer client ~contract ~arg:(sf {|(Some "%s")|} bootstrap5) in
  let* () = check_delegate ~__LOC__ (Some bootstrap5) in
  let* () = transfer client ~contract ~arg:"None" in
  check_delegate ~__LOC__ None

let test_trace_origination script_name protocol client =
  let* _contract = originate client protocol script_name in
  unit

let test_level protocol client =
  (* This tests a contract storing the result of the Michelson
     instruction [LEVEL], which is the current block level. We bake
     blocks to increase the level and verify that the storage (after
     calling the contract) is coherent with the level. *)
  let* contract =
    originate client ~storage:"9999999" protocol ["opcodes"; "level"]
  in
  (* Note: [bake_for_and_wait] is unneeded (and unusable) in mockup mode. *)
  let bake () = Client.bake_for ~minimal_timestamp:true client in
  (* Now at level 1 *)
  let* () = transfer client ~contract in
  let* () = bake () in
  (* Now at level 2, but the contract's storage contains the previous
     level at which it was called. *)
  let* () = check_storage ~__LOC__ client ~contract (string_of_int 1) in
  let* () = bake () in
  (* Now at level 3 *)
  let* () = bake () in
  (* Now at level 4 *)
  (* Checks the storage hasn't changed even though the current level has. *)
  let* () = check_storage ~__LOC__ client ~contract (string_of_int 1) in
  (* Run again to check the storage gets updated. *)
  let* () = transfer client ~contract in
  let* () = bake () in
  (* Now at level 5, but the contract's storage contains the previous
     level at which it was called. *)
  let* () = check_storage ~__LOC__ client ~contract (string_of_int 4) in
  unit

let test_big_map_origination protocol client =
  (* originate the first version of the contract from a literal so that a big_map
     with id 0 exists. *)
  let forge_error = Process.check_error ~msg:(rex "Unexpected forged value") in
  let originate_big_map_name = ["opcodes"; "originate_big_map"] in
  let* contract =
    originate client protocol originate_big_map_name ~storage:"{Elt 0 0}"
  in
  (* originate again the same script from the big-map id 0 *)
  let* () =
    spawn_originate client protocol originate_big_map_name ~storage:"0"
    |> forge_error
  in
  (* originate again the same script from a big-diff *)
  let* () =
    spawn_originate
      client
      protocol
      originate_big_map_name
      ~storage:"Pair 0 {Elt 1 (Some 4)}"
    |> forge_error
  in
  (* call the first contract, passing an id as parameter *)
  let* () = spawn_transfer client ~contract ~arg:"0" |> forge_error in
  (* call the first contract, passing a diff as parameter *)
  let* () =
    spawn_transfer client ~contract ~arg:"Pair 0 {Elt 1 (Some 4)}"
    |> forge_error
  in
  unit

module Tickets = struct
  let ticket_forgery_attempt_error =
    Process.check_error ~msg:(rex "Unexpected forged value")

  let make_runnable process = {value = process; run = Process.check}

  let runnable_script_failwith {value = process; run = _} =
    process |> script_failwith

  let test_ticket_user_forge protocol client =
    let* contract =
      originate client protocol ["opcodes"; "ticket_store-2"] ~storage:"None"
    in
    let* () = transfer client ~contract ~arg:"None" in
    let* () =
      spawn_transfer client ~contract ~arg:"Some 1"
      |> ticket_forgery_attempt_error
    in
    let* () =
      spawn_originate
        client
        protocol
        ["opcodes"; "ticket_bad"]
        ~storage:{|"ticket_bad"|}
      |> ticket_forgery_attempt_error
    in
    unit

  let test_ticket_user_big_forge protocol client =
    (* This test attempts to forge a ticket by copying a big_map containing a ticket. *)
    let script_name = ["opcodes"; "ticket_big_store"] in
    (* Originate a contract with an empty big map of tickets. *)
    let* contract = originate client protocol script_name ~storage:"{}" in
    (* Ask the contract to mint a ticket and store it in its big map. *)
    let* () = transfer client ~contract ~arg:"42" in
    (* Get the id of the big map. *)
    let* storage = get_storage client ~contract in
    let* () =
      (* Try to use the big map id as initial storage in an origination. *)
      spawn_originate client protocol script_name ~storage
      |> ticket_forgery_attempt_error
    in
    let* () =
      (* Same but instead of using the big map id directly we pair it with an empty
         big map diff (this exercises another code path).*)
      spawn_originate
        client
        protocol
        script_name
        ~storage:(sf "Pair %s {}" storage)
      |> ticket_forgery_attempt_error
    in
    unit

  let test_ticket_read protocol client =
    (* Test that tickets can be transferred between contracts.
       The ticketer_read contract mints a ticket and sends it
       to the reader contract which reads it and stores the
       ticketer. *)
    let* ticketer_read =
      originate client protocol ["opcodes"; "ticketer"] ~storage:"42"
    in
    let* reader =
      originate
        client
        protocol
        ["opcodes"; "ticket_read"]
        ~storage:(quote ticketer_read)
    in
    let* () = transfer client ~contract:ticketer_read ~arg:(quote reader) in
    let* () =
      check_storage ~__LOC__ client ~contract:reader (quote ticketer_read)
    in
    unit

  let test_bad_ticket protocol client =
    (* Calling the reader contract from an implicit account is forbidden
       as a forgery attempt. *)
    let* ticketer_bad =
      originate client protocol ["opcodes"; "ticketer"] ~storage:"42"
    in
    let* reader_bad =
      originate
        client
        protocol
        ["opcodes"; "ticket_read"]
        ~storage:(quote ticketer_bad)
    in
    spawn_transfer client ~contract:reader_bad ~arg:"1"
    |> ticket_forgery_attempt_error

  let test_utxo protocol client =
    (* The utxor contract mints a 5-ticket, splits it into a 2-ticket and a 3-ticket
       and send them to the two reader contracts which read the received tickets,
       check their payload and amounts, and store the ticketer address. *)
    let* utxor = originate client protocol ["opcodes"; "utxor"] ~storage:"42" in
    let* reader_a =
      originate client protocol ["opcodes"; "utxo_read"] ~storage:(quote utxor)
    in
    let* reader_b =
      originate client protocol ["opcodes"; "utxo_read"] ~storage:(quote utxor)
    in
    transfer
      client
      ~contract:utxor
      ~arg:(sf "Pair %s %s" (quote reader_a) (quote reader_b))

  let test_ticket_split protocol client =
    (* ticketer_2 mints a ticket described in the second and third parameters and sends it
       to the first parameter. *)
    let* ticketer_2 = originate client protocol ["opcodes"; "ticketer-2"] in
    (* ticket_split splits input ticket into a 1-ticket and a 2-ticket and checks
       that the resulting tickets have the expected amounts. *)
    let* ticket_split = originate client protocol ["opcodes"; "ticket_split"] in
    let build_arg target_addr param utxo_amount =
      sf "Pair (Pair %s %d) %d" (quote target_addr) param utxo_amount
    in
    let* () =
      transfer client ~contract:ticketer_2 ~arg:(build_arg ticket_split 42 3)
    in
    (* Incorrect Split Amount, ticket_split expects a 3-ticket. *)
    spawn_transfer
      client
      ~contract:ticketer_2
      ~arg:(build_arg ticket_split 42 4)
    |> script_failwith

  let test_ticket_join protocol client =
    let* ticketer_a = originate client protocol ["opcodes"; "ticketer-2"] in
    let* ticket_join =
      originate client protocol ~storage:"None" ["opcodes"; "ticket_join"]
    in
    let build_arg target_addr param utxo_amount =
      sf "Pair (Pair %s %d) %d" (quote target_addr) param utxo_amount
    in
    let* () =
      (* first call from the ticketer to ticket_join replaces its None initial storage *)
      transfer client ~contract:ticketer_a ~arg:(build_arg ticket_join 1 42)
    in
    let* () =
      (* second call from ticketer to ticket_join joins the ticket of the first call
         with the ticket of the second call. *)
      transfer client ~contract:ticketer_a ~arg:(build_arg ticket_join 1 144)
    in
    (* Wrong ticketer *)
    let* () =
      let* ticketer_b = originate client protocol ["opcodes"; "ticketer-2"] in
      spawn_transfer
        client
        ~contract:ticketer_b
        ~arg:(build_arg ticket_join 1 23)
      |> script_failwith
    in
    (* Wrong contents *)
    spawn_transfer
      client
      ~contract:ticketer_a
      ~arg:(build_arg ticket_join 23 21)
    |> script_failwith

  let test_ticket_fungible protocol client =
    (* Test the origination of builder and wallet contracts for fungible
       tokens implemented using tickets. *)
    let script_failwith = runnable_script_failwith in
    let manager_address = Constant.bootstrap1.public_key_hash in
    let originate_builder () =
      (* Create a fungible token contract managed by bootstrap1. *)
      originate
        client
        protocol
        ["mini_scenarios"; "ticket_builder_fungible"]
        ~storage:(quote manager_address)
    in
    let originate_wallet () =
      (* Create a fungible token wallet managed by bootstrap1. *)
      originate
        client
        protocol
        ["mini_scenarios"; "ticket_wallet_fungible"]
        ~storage:(sf "Pair %s {}" (quote manager_address))
    in
    (* Create 3 token contracts "A", "B", and "C". *)
    let* builder_a = originate_builder () in
    let* builder_b = originate_builder () in
    let* builder_c = originate_builder () in
    let* alice = originate_wallet () in
    let* bob = originate_wallet () in
    let mint ~builder ~wallet amount =
      (* Mint fungible tokens. *)
      transfer
        ~giver:manager_address
        ~contract:builder
        ~entrypoint:"mint"
        ~arg:(sf {|Pair "%s%%receive" %d|} wallet amount)
        client
    in
    let burn ~builder ~wallet amount =
      (* Burn fungible tokens. *)
      make_runnable
      @@ spawn_transfer
           ~giver:manager_address
           ~contract:wallet
           ~entrypoint:"send"
           ~arg:(sf {|Pair "%s%%burn" %d "%s"|} builder amount builder)
           client
    in
    let transfer_fungible_tokens ~builder ~src ~dst amount =
      (* Transfer fungible tokens. *)
      make_runnable
      @@ spawn_transfer
           ~giver:manager_address
           ~contract:src
           ~entrypoint:"send"
           ~arg:(sf {|Pair "%s%%receive" %d "%s"|} dst amount builder)
           client
    in
    Log.info "100A --> Alice" ;
    let* () = mint ~builder:builder_a ~wallet:alice 100 in
    Log.info "100B --> Alice" ;
    let* () = mint ~builder:builder_b ~wallet:alice 100 in
    Log.info "Fail: Alice --1C--> Bob" ;
    let* () =
      transfer_fungible_tokens ~builder:builder_c ~src:alice ~dst:bob 1
      |> script_failwith
    in
    Log.info "Fail: Alice --0C--> Bob" ;
    let* () =
      transfer_fungible_tokens ~builder:builder_c ~src:alice ~dst:bob 0
      |> script_failwith
    in
    Log.info "Fail: Alice --150A--> Bob" ;
    let* () =
      transfer_fungible_tokens ~builder:builder_a ~src:alice ~dst:bob 150
      |> script_failwith
    in
    Log.info "Fail: Bob --50A--> Alice" ;
    let* () =
      transfer_fungible_tokens ~builder:builder_a ~src:bob ~dst:alice 50
      |> script_failwith
    in
    Log.info "Alice --50A--> Bob" ;
    let*! () =
      transfer_fungible_tokens ~builder:builder_a ~src:alice ~dst:bob 50
    in
    Log.info "Alice --50A--> Bob" ;
    let*! () =
      transfer_fungible_tokens ~builder:builder_a ~src:alice ~dst:bob 50
    in
    Log.info "Fail: Alice --0A--> Bob" ;
    (* This fails because we are not allowed to keep a ticket with zero amount in
       big maps. In the last transfer, Alice's wallet contract has depleted all A-tokens.
       Therefore, this contract call fails on A-token look-up. *)
    let* () =
      transfer_fungible_tokens ~builder:builder_a ~src:alice ~dst:bob 0
      |> script_failwith
    in
    Log.info "Fail: Alice --1A--> Bob" ;
    (* Similarly, this contract call fails because there is no big map entry for
       A-tokens in Alice's wallet contract. *)
    let* () =
      transfer_fungible_tokens ~builder:builder_a ~src:alice ~dst:bob 1
      |> script_failwith
    in
    Log.info "Bob --100A--> Bob" ;
    let*! () =
      transfer_fungible_tokens ~builder:builder_a ~src:bob ~dst:bob 100
    in
    Log.info "Fail: Bob --150A--> Bob" ;
    let* () =
      transfer_fungible_tokens ~builder:builder_a ~src:bob ~dst:bob 150
      |> script_failwith
    in
    Log.info "Bob --100A-->" ;
    let*! () = burn ~builder:builder_a ~wallet:bob 100 in
    Log.info "Fail: Bob --0A-->" ;
    (* The last `burn` call depletes all A-tokens in Bob's wallet. Since no
       ticket of amount zero is allowed in big map, this call fails because
       there is no A-token entry in Bob's wallet contract. *)
    let* () = burn ~builder:builder_a ~wallet:bob 0 |> script_failwith in
    Log.info "Fail: Bob --1A-->" ;
    let* () = burn ~builder:builder_a ~wallet:bob 1 |> script_failwith in
    unit

  let test_ticket_non_fungible protocol client =
    (* Test the origination of builder and wallet contracts for non-fungible tokens implemented
       using tickets. *)
    let manager_address = Constant.bootstrap1.public_key_hash in
    let script_failwith = runnable_script_failwith in
    let originate_builder () =
      (* Create a non-fungible token contract managed by bootstrap1. *)
      originate
        client
        protocol
        ["mini_scenarios"; "ticket_builder_non_fungible"]
        ~storage:(sf {|Pair "%s" 0|} manager_address)
    in
    let originate_wallet () =
      (* Create a non-fungible token wallet managed by bootstrap1. *)
      originate
        client
        protocol
        ["mini_scenarios"; "ticket_wallet_non_fungible"]
        ~storage:(sf "Pair %s {}" (quote manager_address))
    in
    (* Create 3 token contracts "A", "B", and "C". *)
    let* builder_a = originate_builder () in
    let* builder_b = originate_builder () in
    let* builder_c = originate_builder () in
    let* alice = originate_wallet () in
    let* bob = originate_wallet () in
    let mint ~builder ~wallet token_id =
      (* Mint a non-fungible token and assert that it has the
         expected id. *)
      let* () =
        check_storage
          ~__LOC__
          client
          ~contract:builder
          (sf {|Pair "%s" %d|} manager_address token_id)
      in
      transfer
        ~giver:manager_address
        ~contract:builder
        ~entrypoint:"mint_destination"
        ~arg:(sf {|"%s%%receive"|} wallet)
        client
    in
    let burn ~builder ~wallet token_id =
      (* Burn a non-fungible token. *)
      make_runnable
      @@ spawn_transfer
           ~giver:manager_address
           ~contract:wallet
           ~entrypoint:"send"
           ~arg:(sf {|Pair "%s%%burn" "%s" %d|} builder builder token_id)
           client
    in
    let transfer_nft ~builder ~src ~dst token_id =
      (* Transfer a non-fungible token. *)
      make_runnable
      @@ spawn_transfer
           ~giver:manager_address
           ~contract:src
           ~entrypoint:"send"
           ~arg:(sf {|Pair "%s%%receive" "%s" %d|} dst builder token_id)
           client
    in
    Log.info "A0 --> Alice" ;
    let* () = mint ~builder:builder_a ~wallet:alice 0 in
    Log.info "A1 --> Alice" ;
    let* () = mint ~builder:builder_a ~wallet:alice 1 in
    Log.info "B0 --> Alice" ;
    let* () = mint ~builder:builder_b ~wallet:alice 0 in
    Log.info "Fail: Alice --C0--> Bob " ;
    let* () =
      transfer_nft ~builder:builder_c ~src:alice ~dst:bob 1 |> script_failwith
    in
    Log.info "Fail: Alice --A2--> Bob" ;
    let* () =
      transfer_nft ~builder:builder_a ~src:alice ~dst:bob 2 |> script_failwith
    in
    Log.info "Fail: Bob --A0--> Alice" ;
    let* () =
      transfer_nft ~builder:builder_a ~src:bob ~dst:alice 0 |> script_failwith
    in
    Log.info "Fail: Bob --A1--> Bob" ;
    let* () =
      transfer_nft ~builder:builder_a ~src:bob ~dst:bob 1 |> script_failwith
    in
    Log.info "Alice --A1--> Bob" ;
    let*! () = transfer_nft ~builder:builder_a ~src:alice ~dst:bob 1 in
    Log.info "Alice --A0--> Bob" ;
    let*! () = transfer_nft ~builder:builder_a ~src:alice ~dst:bob 0 in
    Log.info "Fail: Alice --A1--> Bob" ;
    let* () =
      transfer_nft ~builder:builder_a ~src:alice ~dst:bob 1 |> script_failwith
    in
    Log.info "Bob --A0--> Bob" ;
    let*! () = transfer_nft ~builder:builder_a ~src:bob ~dst:bob 0 in
    Log.info "Bob --A0-->" ;
    let*! () = burn ~builder:builder_a ~wallet:bob 0 in
    Log.info "Bob --A1-->" ;
    let*! () = burn ~builder:builder_a ~wallet:bob 1 in
    Log.info "Fail: Bob --B0-->" ;
    let* () = burn ~builder:builder_b ~wallet:bob 0 |> script_failwith in
    Log.info "Alice --B0-->" ;
    let*! () = burn ~builder:builder_b ~wallet:alice 0 in
    unit

  let register ~protocols =
    List.iter
      (fun (title, body) ->
        Protocol.register_test
          ~__FILE__
          ~title:("Contract onchain opcodes: " ^ title)
          ~tags:["contract"; "onchain"; "opcodes"]
          ~uses_node:false
          (fun protocol ->
            let* client = Client.init_mockup ~protocol () in
            body protocol client)
          protocols)
      [
        ("ticket_user_forge", test_ticket_user_forge);
        ("ticket_user_big_forge", test_ticket_user_big_forge);
        ("ticket_read", test_ticket_read);
        ("bad_ticket", test_bad_ticket);
        ("ticket_utxo", test_utxo);
        ("ticket_split", test_ticket_split);
        ("ticket_join", test_ticket_join);
        ("ticket_fungible", test_ticket_fungible);
        ("ticket_non_fungible", test_ticket_non_fungible);
      ]
end

let register ~protocols =
  List.iter
    (fun (title, body) ->
      Protocol.register_regression_test
        ~__FILE__
        ~title:("Contract onchain opcodes: " ^ title)
        ~tags:["contract"; "onchain"; "opcodes"]
        ~uses_node:false
        (fun protocol ->
          let* client = Client.init_mockup ~protocol () in
          body protocol client)
        protocols)
    [
      ("test_store_input", test_store_input);
      ("test_transfer_amount", test_transfer_amount);
      ("test_now", test_now);
      ("test_transfer_tokens", test_transfer_tokens);
      ("test_self", test_self);
      ("test_contract_fails", test_contract_fails);
      ("test_source", test_source);
      ("test_sender", test_sender);
      ("test_slice", test_slice);
      ("test_split_string", test_split_string);
      ("test_split_bytes", test_split_bytes);
      ("test_set_delegate", test_set_delegate);
      ( "test_trace_origination_compare_big_type",
        test_trace_origination ["opcodes"; "compare_big_type"] );
      ( "test_trace_origination_compare_big_type2",
        test_trace_origination ["opcodes"; "compare_big_type2"] );
      ("test_level", test_level);
      ("test_big_map_origination", test_big_map_origination);
    ] ;
  Tickets.register ~protocols
