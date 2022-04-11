(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Testing
    -------
    Component: Protocol (Ticket_balance_key)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                -- test "^ticket balance"
    Subject: Ticket balance key hashing
*)

open Protocol
open Alpha_context
open Lwt_tzresult_syntax

let wrap m = m >|= Environment.wrap_tzresult

type init_env = {
  block : Block.t;
  baker : Signature.public_key_hash;
  contract : Contract.t;
}

let init_env () =
  let* (block, baker, contract, _src2) = Contract_helpers.init () in
  return {block; baker; contract}

let transaction block ~baker ~sender ~entrypoint ~recipient ~parameters =
  let parameters = Script.lazy_expr @@ Expr.from_string parameters in
  let* operation =
    Op.transaction
      (B block)
      ~entrypoint
      ~parameters
      ~fee:Tez.one
      sender
      recipient
      (Tez.of_mutez_exn 0L)
  in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  let* incr = Incremental.add_operation incr operation in
  Incremental.finalize_block incr

let originate = Contract_helpers.originate_contract_from_string

let get_balance ctxt ~token ~owner =
  let* (key_hash, ctxt) =
    wrap @@ Ticket_balance_key.of_ex_token ctxt ~owner token
  in
  wrap (Ticket_balance.get_balance ctxt key_hash)

let assert_token_balance ~loc block token owner expected =
  let* incr = Incremental.begin_construction block in
  let ctxt = Incremental.alpha_ctxt incr in
  let* (balance, _) =
    get_balance ctxt ~token ~owner:(Destination.Contract owner)
  in
  match (balance, expected) with
  | (Some b, Some e) -> Assert.equal_int ~loc (Z.to_int b) e
  | (Some b, None) ->
      failwith "%s: Expected no balance but got some %d" loc (Z.to_int b)
  | (None, Some b) -> failwith "%s: Expected balance %d but got none" loc b
  | (None, None) -> return ()

let string_token ~ticketer content =
  let contents =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Alpha_context.Script_string.of_string content
  in
  Ticket_token.Ex_token
    {ticketer; contents_type = Script_typed_ir.string_key; contents}

let unit_ticket ~ticketer =
  Ticket_token.Ex_token
    {ticketer; contents_type = Script_typed_ir.unit_key; contents = ()}

let new_contracts ~before ~after =
  let all_contracts current_block =
    let* ctxt =
      Incremental.begin_construction current_block >|=? Incremental.alpha_ctxt
    in
    Lwt.map Result.ok (Contract.list ctxt)
  in
  let* cs1 = all_contracts before in
  let* cs2 = all_contracts after in
  let not_in_cs1 =
    let module S = Set.Make (String) in
    let set = S.of_list @@ List.map Contract.to_b58check cs1 in
    fun c -> not @@ S.mem (Contract.to_b58check c) set
  in
  return (List.filter not_in_cs1 cs2)

let get_new_contract before f =
  let* after = f before in
  let* contracts = new_contracts ~before ~after in
  match contracts with
  | c :: _ -> return (c, after)
  | _ -> failwith "Expected one new contracts"

(** Test adding a ticket to a strict storage. *)
let test_add_strict () =
  let* {block; baker; contract = source_contract} = init_env () in
  (* Originate *)
  let* (contract, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter unit;
            storage (list (ticket string));
            code
             {  CDR;
                PUSH nat 1;
                PUSH string "Red";
                TICKET;
                CONS;
                NIL operation ;
                PAIR } }
        |}
      ~storage:"{}"
      block
  in
  let token_red = string_token ~ticketer:contract "Red" in
  (* Before calling the contract the balance should be empty. *)
  let* () = assert_token_balance ~loc:__LOC__ block token_red contract None in
  (* Run the script *)
  let* block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:contract
      ~parameters:"Unit"
  in
  (* After calling the contract, one ticket is added and balance is one. *)
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red contract (Some 1)
  in
  (* Calling the contract again should increase the balance once more. *)
  let* block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:contract
      ~parameters:"Unit"
  in
  assert_token_balance ~loc:__LOC__ block token_red contract (Some 2)

(** Test adding and removing tickets from a list in the storage. *)
let test_add_remove () =
  let* {block; baker; contract = source_contract} = init_env () in
  (* Originate *)
  let* (contract, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter (or (unit %add) (unit %remove));
            storage (list (ticket string)) ;
            code { UNPAIR ;
                   IF_LEFT
                     { DROP ;
                       PUSH nat 1 ;
                       PUSH string "Red" ;
                       TICKET ;
                       CONS ;
                       NIL operation ;
                       PAIR }
                     { DROP 2 ; NIL (ticket string) ; NIL operation ; PAIR } } }
        |}
      ~storage:"{}"
      block
  in
  let add_one block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:contract
      ~parameters:"Left Unit"
  in
  let clear block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:contract
      ~parameters:"Right Unit"
  in
  let token_red = string_token ~ticketer:contract "Red" in
  (* Before calling the contract the balance should be empty *)
  let* () = assert_token_balance ~loc:__LOC__ block token_red contract None in
  (* Call the contract twice *)
  let* block = add_one block in
  let* block = add_one block in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red contract (Some 2)
  in
  (* Remove tickets from the contract *)
  let* block = clear block in
  assert_token_balance ~loc:__LOC__ block token_red contract None

(** Test adding multiple tickets to a big-map. *)
let test_add_to_big_map () =
  let* {block; baker; contract = source_contract} = init_env () in
  let* (contract, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter int ;
            storage (big_map int (ticket string)) ;
            code { LEFT (big_map int (ticket string)) ;
                   LOOP_LEFT
                     { UNPAIR ;
                       PUSH int 0 ;
                       SWAP ;
                       DUP ;
                       DUG 2 ;
                       COMPARE ;
                       LE ;
                       IF { DROP ; RIGHT (pair int (big_map int (ticket string))) }
                          { SWAP ;
                            PUSH nat 1 ;
                            PUSH string "Red" ;
                            TICKET ;
                            SOME ;
                            DUP 3 ;
                            GET_AND_UPDATE ;
                            DROP ;
                            PUSH int 1 ;
                            DIG 2 ;
                            SUB ;
                            PAIR ;
                            LEFT (big_map int (ticket string)) } } ;
                   NIL operation ;
                   PAIR } }
        |}
      ~storage:"{}"
      block
  in
  let* block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:contract
      ~parameters:"100"
  in
  let token_red = string_token ~ticketer:contract "Red" in
  assert_token_balance ~loc:__LOC__ block token_red contract (Some 100)

(** Test adding, swapping and clearing big-maps from storage.
    The script contains in its storage two big-maps:

    pair
      (big_map %map1 int (ticket string))
      (big_map %map2 int (ticket string)))

    And takes three actions:
     1) Add one ticket to map1
     2) Swap map1 and map2
     3) Clear map1
 *)
let test_swap_big_map () =
  let* {block; baker; contract = source_contract} = init_env () in
  let* (contract, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter (or (or (int %add) (unit %clear)) (unit %swap)) ;
            storage (pair (big_map %map1 int (ticket string)) (big_map %map2 int (ticket string))) ;
            code { UNPAIR ;
                   NIL operation ;
                   SWAP ;
                   IF_LEFT
                     { IF_LEFT
                         { DIG 2 ;
                           UNPAIR ;
                           PUSH nat 1 ;
                           PUSH string "Red" ;
                           TICKET ;
                           SOME ;
                           DIG 3 ;
                           GET_AND_UPDATE ;
                           DROP ;
                           PAIR ;
                           SWAP ;
                           PAIR }
                         { DROP ; SWAP ; CDR ; EMPTY_BIG_MAP int (ticket string) ;
                           PAIR ; SWAP ; PAIR } }
                     { DROP ; SWAP ; UNPAIR ; SWAP ; PAIR ; SWAP ; PAIR } } }
        |}
      ~storage:"Pair {} {}"
      block
  in
  let add_to_index block ix =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:contract
      ~parameters:(Printf.sprintf "Left (Left %d)" ix)
  in
  let swap_big_maps block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:contract
      ~parameters:"Right Unit"
  in
  let clear_left_big_map block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:contract
      ~parameters:"Left (Right Unit)"
  in
  (* Add three tickets to [map1] *)
  let* block = add_to_index block 1 in
  let* block = add_to_index block 2 in
  let* block = add_to_index block 3 in
  let token_red = string_token ~ticketer:contract "Red" in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red contract (Some 3)
  in
  (* Swap [map1] and [map2]. This should not impact the ticket balance. *)
  let* block = swap_big_maps block in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red contract (Some 3)
  in
  (* Remove all tickets from [map1] (which is empty). *)
  let* block = clear_left_big_map block in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red contract (Some 3)
  in
  (* Swap [map1] and [map2]. Now, [map1] contains three tickets. *)
  let* block = swap_big_maps block in
  (* Clear all tickets from [map1]. *)
  let* block = clear_left_big_map block in
  assert_token_balance ~loc:__LOC__ block token_red contract None

(* Test sending a ticket to an address *)
let test_send_tickets () =
  let* {block; baker; contract = source_contract} = init_env () in
  (* A contract that can receive a ticket and store it in a list. *)
  let* (ticket_receiver, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter (ticket string) ;
            storage (list (ticket string)) ;
            code { UNPAIR ; CONS ; NIL operation ; PAIR } }
        |}
      ~storage:"{}"
      block
  in
  (* A contract that, given an address to a contract that receives tickets,
     mints a ticket and sends it over. *)
  let* (ticket_sender, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter address ;
            storage unit ;
            code { CAR ;
                   CONTRACT (ticket string) ;
                   IF_NONE
                     { PUSH string "Contract of type `ticket(string)' not found" ;
                       FAILWITH }
                     { PUSH mutez 0 ;
                       PUSH nat 1 ;
                       PUSH string "Red" ;
                       TICKET ;
                       TRANSFER_TOKENS ;
                       PUSH unit Unit ;
                       NIL operation ;
                       DIG 2 ;
                       CONS ;
                       PAIR } } }
        |}
      ~storage:"Unit"
      block
  in
  let mint_and_send block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_sender
      ~parameters:
        (Printf.sprintf {|"%s"|} @@ Contract.to_b58check ticket_receiver)
  in
  (* Call ticket-sender twice in order to transfer tickets to ticket-receiver *)
  let* block = mint_and_send block in
  let* block = mint_and_send block in
  let token_red = string_token ~ticketer:ticket_sender "Red" in
  assert_token_balance ~loc:__LOC__ block token_red ticket_receiver (Some 2)

(** Test sending tickets in a big-map. *)
let test_send_tickets_in_big_map () =
  let* {block; baker; contract = source_contract} = init_env () in
  (* A contract that can receive a big-map with tickets. *)
  let* (ticket_receiver, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter (big_map int (ticket string)) ;
            storage (big_map int (ticket string)) ;
            code { CAR ; NIL operation ; PAIR } }
        |}
      ~storage:"{}"
      block
  in
  (* A contract with two actions:
      - [mint_and_save(key, content)] for creating and saving a new ticket in
        a big-map.
      - [send (address)] for transferring the big-map to the given address.
  *)
  let* (ticket_manager, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter (or (pair %mint_and_save int string) (address %send)) ;
            storage (big_map int (ticket string)) ;
            code { UNPAIR ;
                   IF_LEFT
                     { UNPAIR ;
                       DIG 2 ;
                       PUSH nat 1 ;
                       DIG 3 ;
                       TICKET ;
                       SOME ;
                       DIG 2 ;
                       GET_AND_UPDATE ;
                       DROP ;
                       NIL operation ;
                       PAIR }
                     { CONTRACT (big_map int (ticket string)) ;
                       IF_NONE
                         { PUSH string "Contract of type `ticket(string)` not found" ;
                           FAILWITH }
                         { PUSH mutez 0 ;
                           DIG 2 ;
                           TRANSFER_TOKENS ;
                           EMPTY_BIG_MAP int (ticket string) ;
                           NIL operation ;
                           DIG 2 ;
                           CONS ;
                           PAIR } } } }
        |}
      ~storage:"{}"
      block
  in
  let mint_and_save key content block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_manager
      ~parameters:(Printf.sprintf {|Left (Pair %d "%s")|} key content)
  in
  let send block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_manager
      ~parameters:
        (Printf.sprintf {|Right "%s"|} @@ Contract.to_b58check ticket_receiver)
  in
  let token_red = string_token ~ticketer:ticket_manager "Red" in
  let token_blue = string_token ~ticketer:ticket_manager "Blue" in
  let token_yellow = string_token ~ticketer:ticket_manager "Yellow" in
  (* Call ticket manager to mint and save three tickets in a big-map. *)
  let* block = mint_and_save 1 "Red" block in
  let* block = mint_and_save 2 "Blue" block in
  let* block = mint_and_save 3 "Yellow" block in
  (* Verify that all three tickets are accounted for and belong to
     ticket-manager. *)
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red ticket_manager (Some 1)
  in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_blue ticket_manager (Some 1)
  in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_yellow ticket_manager (Some 1)
  in
  (* Send over the big-map with tickets to ticket-receiver. *)
  let* block = send block in
  (* Verify that all three tickets now belong to the ticket-receiver contract. *)
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red ticket_receiver (Some 1)
  in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_blue ticket_receiver (Some 1)
  in
  let* () =
    assert_token_balance
      ~loc:__LOC__
      block
      token_yellow
      ticket_receiver
      (Some 1)
  in
  (* Finally test that the ticket-manager no longer holds any of the tickets *)
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red ticket_manager None
  in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_blue ticket_manager None
  in
  assert_token_balance ~loc:__LOC__ block token_yellow ticket_manager None

(* Test sending tickets in a big-map. *)
let test_modify_big_map () =
  let* {block; baker; contract = source_contract} = init_env () in
  (* A contract with two actions:
       - [Add ((int, string))] for adding a ticket to the big-map.
       - [Remove(int)] for removing an index from the big-map.
  *)
  let* (ticket_manager, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter (or (pair %add int string) (int %remove)) ;
            storage (big_map int (ticket string)) ;
            code { UNPAIR ;
                   IF_LEFT
                     { UNPAIR ;
                       DIG 2 ;
                       PUSH nat 1 ;
                       DIG 3 ;
                       TICKET ;
                       SOME ;
                       DIG 2 ;
                       GET_AND_UPDATE ;
                       DROP ;
                       NIL operation ;
                       PAIR }
                     { SWAP ;
                       NONE (ticket string) ;
                       DIG 2 ;
                       GET_AND_UPDATE ;
                       DROP ;
                       NIL operation ;
                       PAIR } } }
        |}
      ~storage:"{}"
      block
  in
  let add key content block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_manager
      ~parameters:(Printf.sprintf {|Left (Pair %d "%s")|} key content)
  in
  let remove key block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_manager
      ~parameters:(Printf.sprintf {|Right %d|} key)
  in
  let token_red = string_token ~ticketer:ticket_manager "Red" in
  let token_blue = string_token ~ticketer:ticket_manager "Blue" in
  let token_yellow = string_token ~ticketer:ticket_manager "Yellow" in
  let token_green = string_token ~ticketer:ticket_manager "Green" in
  (* Add a red, blue and a yellow ticket *)
  let* block = add 1 "Red" block in
  let* block = add 2 "Blue" block in
  let* block = add 3 "Yellow" block in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red ticket_manager (Some 1)
  in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_blue ticket_manager (Some 1)
  in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_yellow ticket_manager (Some 1)
  in
  (* Replace the red ticket at index 1 with a green one. *)
  let* block = add 1 "Green" block in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red ticket_manager None
  in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_green ticket_manager (Some 1)
  in
  (* Remove the blue ticket at index 2. *)
  let* block = remove 2 block in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_blue ticket_manager None
  in
  (* Add one more green ticket at index 4 and verify that the total count is 2. *)
  let* block = add 4 "Green" block in
  assert_token_balance ~loc:__LOC__ block token_green ticket_manager (Some 2)

(* Test sending tickets in a big-map to a receiver that drops it. *)
let test_send_tickets_in_big_map_and_drop () =
  let* {block; baker; contract = source_contract} = init_env () in
  (* A contract that can receive a big-map with tickets but drops it. *)
  let* (ticket_receiver, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter (big_map int (ticket string)) ;
            storage unit;
            code { DROP; PUSH unit Unit; NIL operation ; PAIR } }
        |}
      ~storage:"Unit"
      block
  in
  (* A contract that, given an address, creates a ticket and sends it to the
     corresponding contract in a big-map. *)
  let* (ticket_sender, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter address ;
            storage unit ;
            code { CAR ;
                   EMPTY_BIG_MAP int (ticket string) ;
                   PUSH nat 1 ;
                   PUSH string "Red" ;
                   TICKET ;
                   SOME ;
                   PUSH int 1 ;
                   GET_AND_UPDATE ;
                   DROP ;
                   SWAP ;
                   CONTRACT (big_map int (ticket string)) ;
                   IF_NONE
                     { DROP ;
                       PUSH string "Contract of type `ticket(string)` not found" ;
                       FAILWITH }
                     { PUSH mutez 0 ;
                       DIG 2 ;
                       TRANSFER_TOKENS ;
                       PUSH unit Unit ;
                       NIL operation ;
                       DIG 2 ;
                       CONS ;
                       PAIR } } }

        |}
      ~storage:"Unit"
      block
  in
  let token_red = string_token ~ticketer:ticket_sender "Red" in
  (* Call ticket-sender to send a ticket to ticket-receiver. *)
  let* block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_sender
      ~parameters:
        (Printf.sprintf {|"%s"|} @@ Contract.to_b58check ticket_receiver)
  in
  (* Verify that neither ticket-sender nor ticket-receiver holds any balance
     for the ticket. *)
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red ticket_sender None
  in
  assert_token_balance ~loc:__LOC__ block token_red ticket_receiver None

(* Test create contract with tickets *)
let test_create_contract_with_ticket () =
  let* {block; baker; contract = source_contract} = init_env () in
  let* (ticket_creator, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter (pair (pair string nat) key_hash) ;
            storage unit ;
            code { UNPAIR ;
                   UNPAIR ;
                   UNPAIR ;
                   TICKET ;
                   PUSH mutez 0 ;
                   DIG 2 ;
                   SOME ;
                   CREATE_CONTRACT
                     { parameter (ticket string) ;
                       storage (ticket string) ;
                       code { CAR ; NIL operation ; PAIR } } ;
                   SWAP ;
                   DROP ;
                   SWAP ;
                   NIL operation ;
                   DIG 2 ;
                   CONS ;
                   PAIR } }
        |}
      ~storage:"Unit"
      block
  in
  let token_red = string_token ~ticketer:ticket_creator "Red" in
  (* Call ticket-creator to originate a new contract with one ticket *)
  let* (new_contract, block) =
    get_new_contract block (fun block ->
        transaction
          ~entrypoint:Entrypoint.default
          ~baker
          ~sender:source_contract
          block
          ~recipient:ticket_creator
          ~parameters:
            (Printf.sprintf
               {|Pair (Pair "Red" 1) "%s"|}
               (Contract.to_b58check source_contract)))
  in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red new_contract (Some 1)
  in
  assert_token_balance ~loc:__LOC__ block token_red ticket_creator None

let test_join_tickets () =
  let* {block; baker; contract = source_contract} = init_env () in
  let* (ticket_joiner, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:
        {|
          { parameter unit ;
            storage (option (ticket string)) ;
            code { CDR ;
                   IF_NONE
                     { PUSH nat 1 ; PUSH string "Red" ;
                       TICKET ; SOME ; NIL operation ; PAIR }
                     { PUSH nat 1 ;
                       PUSH string "Red" ;
                       TICKET ;
                       PAIR ;
                       JOIN_TICKETS ;
                       NIL operation ;
                       PAIR } } }
        |}
      ~storage:"None"
      block
  in
  let token_red = string_token ~ticketer:ticket_joiner "Red" in
  (* Call ticket joiner to create and join an additional ticket. *)
  let add block =
    transaction
      ~entrypoint:Entrypoint.default
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_joiner
      ~parameters:"Unit"
  in
  (* Add three tickets *)
  let* block = add block in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red ticket_joiner (Some 1)
  in
  let* block = add block in
  let* () =
    assert_token_balance ~loc:__LOC__ block token_red ticket_joiner (Some 2)
  in
  let* block = add block in
  assert_token_balance ~loc:__LOC__ block token_red ticket_joiner (Some 3)

(* A simple fungible token contract implemented using tickets of type
   [ticket unit].
   parameter:
    - burn: ticket unit
    - mint: (contract %destination (ticket unit)) x (nat %amount)
*)
let ticket_builder =
  {|

      parameter
        (or (ticket %burn unit)
            (pair %mint (contract %destination (ticket unit)) (nat %amount)));
      storage address;
      code
        {
          AMOUNT; PUSH mutez 0; ASSERT_CMPEQ;

          UNPAIR;
          IF_LEFT
            {
              # Burn entrypoint
              # Check that the ticket is ticketed by ourselves
              READ_TICKET; CAR; SELF_ADDRESS; ASSERT_CMPEQ;

              # Drop the ticket
              DROP;

              # Finish
              NIL operation
            }
            {
              # Mint entrypoint
              # Authenticate SENDER
              DUP @manager 2; SENDER; ASSERT_CMPEQ;

              UNPAIR;
              SWAP; UNIT; TICKET;
              PUSH mutez 0; SWAP; TRANSFER_TOKENS;
              NIL operation; SWAP; CONS
            };
          PAIR
        }

  |}

(* A simple wallet for fungible tokens implemented using tickets of
   type [ticket unit].
   parameter:
    - receive: ticket unit
    - send:
        * destination: (contract (ticket unit))
        * amount: nat
        * ticketer: address
*)
let ticket_wallet =
  {|
    parameter
      (or
        (ticket %receive unit)
        (pair %send (contract %destination (ticket unit)) (nat %amount) (address %ticketer)));
    storage (pair (address %manager) (big_map %tickets address (ticket unit)));
    code
      {
        AMOUNT; PUSH mutez 0; ASSERT_CMPEQ;

        UNPAIR 3;
        IF_LEFT
          {
            # Receive entrypoint

            # Get the ticketer
            READ_TICKET; CAR @ticketer; DUP;

            # Extract the associated ticket, if any, from the stored big map
            DIG 4;
            NONE (ticket unit);
            DIG 2;
            GET_AND_UPDATE;

            # Join it with the parameter
            IF_SOME
              {
                DIG 3;
                PAIR;
                JOIN_TICKETS;
                ASSERT_SOME
              }
              { DIG 2 };
            SOME;
            DIG 2;
            GET_AND_UPDATE;
            ASSERT_NONE;
            SWAP;
            PAIR;
            NIL operation
          }
          {
            # Send entrypoints

            # Authenticate SENDER
            DUP @manager 2; SENDER; ASSERT_CMPEQ;

            UNPAIR 3;

            # Get the ticket associated to the requested ticketer
            DIG 4;
            NONE (ticket unit);
            DUP @ticketer 5;
            GET_AND_UPDATE;
            ASSERT_SOME;

            # Substract the requested amount
            READ_TICKET;
            GET @total_amount 4;
            DUP @amount 5;
            SWAP; SUB; ISNAT; ASSERT_SOME @remaining_amount;

            # Split the ticket
            DIG 4; PAIR; SWAP; SPLIT_TICKET;
            ASSERT_SOME; UNPAIR @to_send @to_keep;

            # Store the ticket to keep
            DUG 5;
            SOME;
            DIG 3;
            GET_AND_UPDATE;
            ASSERT_NONE;
            DIG 2; PAIR;

            # Send the ticket
            SWAP;
            PUSH mutez 0;
            DIG 3;
            TRANSFER_TOKENS;
            NIL operation;
            SWAP;
            CONS;
          };
        PAIR
      }
  |}

(** Test ticket wallet implementation including sending tickets to self. *)
let test_ticket_wallet () =
  let* {block; baker; contract = source_contract} = init_env () in
  let* (ticket_builder, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:ticket_builder
      ~storage:(Printf.sprintf "%S" @@ Contract.to_b58check source_contract)
      block
  in
  let* (ticket_wallet, _script, block) =
    originate
      ~baker
      ~source_contract
      ~script:ticket_wallet
      ~storage:
        (Printf.sprintf "Pair %S {}" @@ Contract.to_b58check source_contract)
      block
  in
  (* Call ticket-builder to mint one ticket and send to ticket-wallet. *)
  let ticket_builder_token = unit_ticket ~ticketer:ticket_builder in
  let send_one block =
    transaction
      ~entrypoint:(Entrypoint.of_string_strict_exn "mint")
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_builder
      ~parameters:
        (Printf.sprintf
           {|Pair "%s%sreceive" 1|}
           (Contract.to_b58check ticket_wallet)
           "%")
  in
  (* Call ticket wallet to send a ticket to ticket-builder's burn address
     entrypoint. *)
  let send_to_burn block =
    transaction
      ~entrypoint:(Entrypoint.of_string_strict_exn "send")
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_wallet
      ~parameters:
        (Printf.sprintf
           {|Pair "%s%sburn" 1 %S|}
           (Contract.to_b58check ticket_builder)
           "%"
           (Contract.to_b58check ticket_builder))
  in
  let send_to_self block =
    transaction
      ~entrypoint:(Entrypoint.of_string_strict_exn "send")
      ~baker
      ~sender:source_contract
      block
      ~recipient:ticket_wallet
      ~parameters:
        (Printf.sprintf
           {|Pair "%s%sreceive" 1 %S|}
           (Contract.to_b58check ticket_wallet)
           "%"
           (Contract.to_b58check ticket_builder))
  in
  let assert_balance block balance =
    assert_token_balance
      ~loc:__LOC__
      block
      ticket_builder_token
      ticket_wallet
      balance
  in
  (* Mint and send tickets to wallet. *)
  let* block = send_one block in
  let* () = assert_balance block (Some 1) in
  let* block = send_one block in
  let* () = assert_balance block (Some 2) in
  (* Send to self should not affect the balance. *)
  let* block = send_to_self block in
  let* () = assert_balance block (Some 2) in
  (* Burn tickets by sending to burn address. *)
  let* block = send_to_burn block in
  let* () = assert_balance block (Some 1) in
  let* block = send_to_burn block in
  assert_balance block None

let tests =
  [
    Tztest.tztest "Test add strict" `Quick test_add_strict;
    Tztest.tztest "Test add and remove" `Quick test_add_remove;
    Tztest.tztest "Test add to big-map" `Quick test_add_to_big_map;
    Tztest.tztest "Test swap big-map" `Quick test_swap_big_map;
    Tztest.tztest "Test send ticket" `Quick test_send_tickets;
    Tztest.tztest
      "Test send tickets in big-map"
      `Quick
      test_send_tickets_in_big_map;
    Tztest.tztest "Test modify big-map" `Quick test_modify_big_map;
    Tztest.tztest "Test send drop" `Quick test_send_tickets_in_big_map_and_drop;
    Tztest.tztest "Test send drop" `Quick test_create_contract_with_ticket;
    Tztest.tztest "Test join" `Quick test_join_tickets;
    Tztest.tztest "Test wallet" `Quick test_ticket_wallet;
  ]
