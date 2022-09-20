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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file ticket_updates_in_receipt.ml
   Subject:      Regression tests for ticket updates in receipt
*)

let hooks = Tezos_regression.hooks

(* Checks how the ticket updates in receipt look like in various cases, such as:
   - Contract stores multiple tickets of different types.
   - Contract stores multiple tickets of the same type.
   - Contract sends a ticket to another contract.

   To do this, we originate three contracts where:
   - KT_A (originated from tickets_create_and_send.tz) creates one red ticket, two green tickets, and two blue tickets, then:
     - Stores the red and green tickets.
     - Sends both blue tickets to KT_B.
   - KT_B (originated from tickets_store_fst_and_rely_snd.tz) stores one blue ticket and sends the other to KT_C.
   - KT_C (originated from tickets_receive_and_store.tz) stores the blue ticket.

                                  (one blue)-> KT_C
                                   |           (store 1 blue)
                 (two blue) -> KT_B
                 |             (store 1 blue)
      tz_a -> KT_A
              (store 1 red and 2 green)
*)
let test_ticket_updates_in_receipt =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Ticket updates in receipt"
    ~tags:["client"; "michelson"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* create_and_send =
    Client.originate_contract
      ~alias:"tickets_create_and_send.tz"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/tickets_create_and_send.tz"
      ~init:"{}"
      ~burn_cap:Tez.one
      client
  in
  let* store_fst_and_rely_snd =
    Client.originate_contract
      ~alias:"tickets_store_fst_and_rely_snd.tz"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:
        "file:./tezt/tests/contracts/proto_alpha/tickets_store_fst_and_rely_snd.tz"
      ~init:"None"
      ~burn_cap:Tez.one
      client
  in
  let* receive_and_store =
    Client.originate_contract
      ~alias:"tickets_receive_and_store.tz"
      ~amount:Tez.one
      ~src:"bootstrap1"
      ~prg:
        "file:./tezt/tests/contracts/proto_alpha/tickets_receive_and_store.tz"
      ~init:"None"
      ~burn_cap:Tez.one
      client
  in
  let* () =
    Client.transfer
      ~burn_cap:(Tez.of_int 2)
      ~amount:Tez.zero
      ~giver:"bootstrap2"
      ~receiver:create_and_send
      ~arg:
        (Format.sprintf
           {|(Pair "%s" "%s")|}
           store_fst_and_rely_snd
           receive_and_store)
      ~hooks
      client
  in
  unit

let register ~protocols = test_ticket_updates_in_receipt protocols
