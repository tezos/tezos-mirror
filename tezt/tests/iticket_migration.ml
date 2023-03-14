(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Invocation:   dune exec tezt/tests/main.exe -- --file iticket_migration.ml
   Subject:      Checks the migration of ITicket in protocol Lima, to make sure
                 that the old TICKET instruction is mapped to TICKET_DEPRECATED,
                 and the old contracts continue to function provided that they
                 avoid creating zero tickets.
*)

(* This test originates a contract that creates a zero ticket with the old TICKET
   instruction, performs a protocol upgrade and inspect the script to make sure
   TICKET is mapped to TICKET_DEPRECATED. It also calls this contract which does
   not mitigate creation of zero tickets and this call should fail.
*)
let test_ticket_migration ~blocks_per_cycle ~migration_level ~migrate_from
    ~migrate_to =
  Test.register
    ~__FILE__
    ~title:
      (Printf.sprintf
         "ITicket migration at level %d from %s to %s"
         migration_level
         (Protocol.name migrate_from)
         (Protocol.name migrate_to))
    ~tags:["protocol"; "migration"; "sandbox"]
  @@ fun () ->
  assert (migration_level >= blocks_per_cycle) ;
  Log.info "Node starting" ;
  let* node =
    Node.init
      ~patch_config:
        (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
           [(migration_level, migrate_to)])
      [Synchronisation_threshold 0; Connections 0]
  in
  Log.info "Node initialized" ;
  let* client = Client.(init ~endpoint:(Node node) ()) in
  let* () = Client.activate_protocol ~protocol:migrate_from client in
  Log.info "Protocol activated" ;
  let* contract_id =
    Client.(
      originate_contract
        ~init:"Unit"
        ~burn_cap:Tez.one
        ~alias:"ticket"
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~prg:
          {|
            storage unit;
            parameter unit;
            code
              {
                DROP;
                PUSH nat 0;
                PUSH string "hello";
                TICKET;
                DROP;
                UNIT;
                NIL operation;
                PAIR
              }
            |}
        client)
  in
  Log.info "Contract %s originated" contract_id ;
  (* Bake until migration *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let* code_before =
    Client.(contract_code ~unparsing_mode:Optimized) contract_id client
  in
  Log.info "code: %s" code_before ;
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract_id
      ~arg:"Unit"
      client
  in
  (* Ensure that we did migrate *)
  let* migration_block =
    RPC.Client.call client @@ RPC.get_chain_block_metadata ~block:"2" ()
  in
  Log.info "Checking migration block consistency" ;
  Check.(
    (migration_block.protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (migration_block.next_protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  (* Test that we can still bake after migration *)
  let rec wait_for_migration_block () =
    let* () = Client.bake_for_and_wait client in
    let* migration_block =
      RPC.(Client.call client @@ get_chain_block_header ())
    in
    let level = JSON.(migration_block |-> "level" |> as_int) in
    if level >= migration_level then
      RPC.(
        Client.call client
        @@ get_chain_block_metadata ~block:(string_of_int level) ())
    else wait_for_migration_block ()
  in
  let* migration_block = wait_for_migration_block () in
  Log.info
    "protocol: %s, next_protocol: %s"
    migration_block.protocol
    migration_block.next_protocol ;
  (* Bake one more time to complete the protocol upgrade *)
  let* () = Client.bake_for_and_wait client in
  let* code_after =
    Client.(contract_code ~unparsing_mode:Optimized) contract_id client
  in
  Log.info "code: %s" code_after ;
  Check.(
    (code_after =~ rex "\\bTICKET_DEPRECATED\\s*;")
      ~error_msg:"expecting TICKET_DEPRECATED instruction, got code %L") ;
  (* This transfer is now expected to fail because the contract produces a zero
     amount ticket. *)
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract_id
      ~arg:"Unit"
      ~expect_failure:true
      client
  in
  unit

(* This test originates a contract that creates, extracts and splits tickets in storage.
   It then performs a protocol upgrade and calls the contract again to ensure it is
   still functioning.
*)
let test_ticket_migration_in_storage_with_zero_tickets ~blocks_per_cycle
    ~migration_level ~migrate_from ~migrate_to =
  Test.register
    ~__FILE__
    ~title:
      (Printf.sprintf
         "ITicket migration at level %d with zero ticket in storage"
         migration_level)
    ~tags:["protocol"; "migration"; "sandbox"]
  @@ fun () ->
  assert (migration_level >= blocks_per_cycle) ;
  Log.info "Node starting" ;
  let* node =
    Node.init
      ~patch_config:
        (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
           [(migration_level, migrate_to)])
      [Synchronisation_threshold 0; Connections 0]
  in
  Log.info "Node initialized" ;
  let* client = Client.(init ~endpoint:(Node node) ()) in
  let* () = Client.activate_protocol ~protocol:migrate_from client in
  Log.info "Protocol activated" ;
  let* contract_id =
    Client.(
      originate_contract
        ~init:"{}"
        ~burn_cap:Tez.one
        ~alias:"ticket"
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~prg:
          {|
            storage (list (ticket string));
            parameter (or (unit %push) (unit %pop));
            code
              {
                UNPAIR;
                IF_LEFT
                  {
                    DROP;
                    # forge a ticket of amount 4
                    PUSH nat 0;
                    PUSH string "hello";
                    TICKET;
                    CONS;
                  }
                  {
                    DROP;
                    # decrement the ticket at the stack top by 1
                    IF_CONS
                      { DROP }
                      { PUSH string "empty stack"; FAILWITH }
                  };
                NIL operation;
                PAIR
              }
            |}
        client)
  in
  Log.info "Contract %s originated" contract_id ;
  (* Bake until migration *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let* code_before =
    Client.(contract_code ~unparsing_mode:Optimized) contract_id client
  in
  Log.info "code: %s" code_before ;
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract_id
      ~entrypoint:"push"
      ~arg:"Unit"
      client
  in
  (* Ensure that we did migrate *)
  let* migration_block =
    RPC.Client.call client @@ RPC.get_chain_block_metadata ~block:"2" ()
  in
  Log.info "Checking migration block consistency" ;
  Check.(
    (migration_block.protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (migration_block.next_protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  (* Test that we can still bake after migration *)
  let rec wait_for_migration_block () =
    let* () = Client.bake_for_and_wait client in
    let* migration_block =
      RPC.(Client.call client @@ get_chain_block_header ())
    in
    let level = JSON.(migration_block |-> "level" |> as_int) in
    if level >= migration_level then
      RPC.(
        Client.call client
        @@ get_chain_block_metadata ~block:(string_of_int level) ())
    else wait_for_migration_block ()
  in
  let* migration_block = wait_for_migration_block () in
  Log.info
    "protocol: %s, next_protocol: %s"
    migration_block.protocol
    migration_block.next_protocol ;
  (* Bake one more time to complete the protocol upgrade *)
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract_id
      ~entrypoint:"pop"
      ~expect_failure:true
      ~arg:"Unit"
      client
  in
  unit

(* This test originates a contract that creates, extracts and splits tickets in storage.
   It then performs a protocol upgrade and calls the contract again to ensure it is
   still functioning.
*)
let test_ticket_migration_in_storage ~blocks_per_cycle ~migration_level
    ~migrate_from ~migrate_to =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "ITicket migration at level %d" migration_level)
    ~tags:["protocol"; "migration"; "sandbox"]
  @@ fun () ->
  assert (migration_level >= blocks_per_cycle) ;
  Log.info "Node starting" ;
  let* node =
    Node.init
      ~patch_config:
        (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
           [(migration_level, migrate_to)])
      [Synchronisation_threshold 0; Connections 0]
  in
  Log.info "Node initialized" ;
  let* client = Client.(init ~endpoint:(Node node) ()) in
  let* () = Client.activate_protocol ~protocol:migrate_from client in
  Log.info "Protocol activated" ;
  let* contract_id =
    Client.(
      originate_contract
        ~init:"{}"
        ~burn_cap:Tez.one
        ~alias:"ticket"
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~prg:
          {|
            storage (list (ticket string));
            parameter (or (unit %forge) (unit %split));
            code
              {
                UNPAIR;
                IF_LEFT
                  {
                    DROP;
                    # forge a ticket of amount 4
                    PUSH nat 4;
                    PUSH string "hello";
                    TICKET;
                    CONS;
                  }
                  {
                    DROP;
                    # decrement the ticket at the stack top by 1
                    IF_CONS
                      {
                        READ_TICKET;
                        GET 4;
                        PUSH nat 1;
                        SWAP;
                        SUB;
                        DUP;
                        # drop the ticket if it will go down to 0
                        EQ;
                        IF
                          { DROP 2 }
                          {
                            ISNAT;
                            ASSERT_SOME;
                            PUSH nat 1;
                            PAIR;
                            SWAP;
                            SPLIT_TICKET;
                            ASSERT_SOME;
                            CDR;
                            CONS
                          }
                      }
                      { PUSH string "empty stack"; FAILWITH }
                  };
                NIL operation;
                PAIR
              }
            |}
        client)
  in
  Log.info "Contract %s originated" contract_id ;
  (* Bake until migration *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let* code_before =
    Client.(contract_code ~unparsing_mode:Optimized) contract_id client
  in
  Log.info "code: %s" code_before ;
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract_id
      ~entrypoint:"forge"
      ~arg:"Unit"
      client
  in
  (* Ensure that we did migrate *)
  let* migration_block =
    RPC.Client.call client @@ RPC.get_chain_block_metadata ~block:"2" ()
  in
  Log.info "Checking migration block consistency" ;
  Check.(
    (migration_block.protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected protocol = %R, got %L") ;
  Check.(
    (migration_block.next_protocol = Protocol.hash migrate_from)
      string
      ~error_msg:"expected next_protocol = %R, got %L") ;
  (* Test that we can still bake after migration *)
  let rec wait_for_migration_block () =
    let* () = Client.bake_for_and_wait client in
    let* migration_block =
      RPC.(Client.call client @@ get_chain_block_header ())
    in
    let level = JSON.(migration_block |-> "level" |> as_int) in
    if level >= migration_level then
      RPC.(
        Client.call client
        @@ get_chain_block_metadata ~block:(string_of_int level) ())
    else wait_for_migration_block ()
  in
  let* migration_block = wait_for_migration_block () in
  Log.info
    "protocol: %s, next_protocol: %s"
    migration_block.protocol
    migration_block.next_protocol ;
  (* Bake one more time to complete the protocol upgrade *)
  let* () = Client.bake_for_and_wait client in
  let* code_after =
    Client.(contract_code ~unparsing_mode:Optimized) contract_id client
  in
  Log.info "code: %s" code_after ;
  Check.(
    (code_after =~ rex "\\bTICKET_DEPRECATED\\s*;")
      ~error_msg:"expecting TICKET_DEPRECATED instruction, got code %L") ;
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract_id
      ~entrypoint:"forge"
      ~arg:"Unit"
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract_id
      ~entrypoint:"split"
      ~arg:"Unit"
      client
  in
  unit

(* This test activates Lima protocol and asserts that it is impossible to
   originate contracts containing the deprecated TICKET instruction.
*)
let test_iticket_deprecation ~protocol =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "ITicket deprecation")
    ~tags:["protocol"; "migration"; "sandbox"]
  @@ fun () ->
  let* _, client = Client.init_with_protocol `Client ~protocol () in
  let* _ = Client.gen_and_show_keys client in
  let process =
    Client.(
      spawn_originate_contract
        ~init:"Unit"
        ~burn_cap:Tez.one
        ~alias:"ticket"
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~log_output:true
        ~prg:
          {|
            storage unit;
            parameter unit;
            code
              {
                DROP;
                PUSH nat 0;
                PUSH string "hello";
                TICKET_DEPRECATED;
                DROP;
                UNIT;
                NIL operation;
                PAIR
              }
            |}
        client)
  in
  Process.check_error
    ~msg:(rex "Use of deprecated instruction: TICKET_DEPRECATED")
    process

(* This test checks that after migration, the new ITicket instruction pushes
   an option, and it correctly pushes some ticket if the ticket is well-formed.*)
let test_iticket_after_migration ~protocol =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "ITicket semantics after migration")
    ~tags:["protocol"; "migration"; "sandbox"]
  @@ fun () ->
  let* _, client = Client.init_with_protocol `Client ~protocol () in
  let* _ = Client.gen_and_show_keys client in
  let* contract_id =
    Client.(
      originate_contract
        ~init:"{}"
        ~burn_cap:Tez.one
        ~alias:"ticket"
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~prg:
          {|
            storage (list (ticket string));
            parameter unit;
            code
              {
                CDR;
                PUSH nat 1;
                PUSH string "hello";
                TICKET;
                ASSERT_SOME;
                CONS;
                NIL operation;
                PAIR
              }
            |}
        client)
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract_id
      ~arg:"Unit"
      client
  in
  unit

let register ~migrate_from ~migrate_to =
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  match migrate_to with
  | Protocol.Lima ->
      test_ticket_migration
        ~blocks_per_cycle
        ~migration_level:(2 * blocks_per_cycle)
        ~migrate_from
        ~migrate_to ;
      test_iticket_deprecation ~protocol:Protocol.Lima ;
      test_ticket_migration_in_storage
        ~blocks_per_cycle
        ~migration_level:(2 * blocks_per_cycle)
        ~migrate_from
        ~migrate_to ;
      test_ticket_migration_in_storage_with_zero_tickets
        ~blocks_per_cycle
        ~migration_level:(2 * blocks_per_cycle)
        ~migrate_from
        ~migrate_to ;
      test_iticket_after_migration ~protocol:Protocol.Lima
  | _ -> ()
