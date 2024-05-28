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
   Invocation:   dune exec tezt/tests/main.exe -- --file ticket_receipt_and_rpc.ml
   Subject:      Regression tests for ticket receipt/RPC/CLI
*)

let hooks = Tezos_regression.hooks

let rpc_check_ticket_balance client ~contract ~ticketer ~content_type ~content
    ~expected =
  let post_body : RPC_core.data =
    Data
      (Ezjsonm.value_from_string
      @@ sf
           {|{"ticketer": "%s", "content_type": {"prim": "%s"}, "content": {"string": "%s"}}|}
           ticketer
           content_type
           content)
  in
  let* actual =
    Client.RPC.call client
    @@ RPC.post_chain_block_context_contract_ticket_balance
         ~id:contract
         ~data:post_body
         ()
  in
  return
  @@ Check.(
       (actual = expected)
         int
         ~__LOC__
         ~error_msg:"expected ticket amount %R, got %L")

let cli_check_ticket_balance client ~hooks ~contract ~ticketer ~content_type
    ~content ~expected =
  let* actual =
    Client.ticket_balance
      client
      ~hooks
      ~contract
      ~ticketer
      ~content_type
      ~content:(sf {|"%s"|} content)
  in
  return
  @@ Check.(
       (String.trim actual = string_of_int expected)
         string
         ~__LOC__
         ~error_msg:"expected ticket amount %R, got %L")

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
let test_ticket_receipt_and_rpc =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Ticket updates in receipt"
    ~tags:["client"; "michelson"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, kt_a =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"{}"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "tickets_create_and_send"]
      protocol
  in
  let* _alias, kt_b =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"None"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "tickets_store_fst_and_rely_snd"]
      protocol
  in
  let* _alias, kt_c =
    Client.originate_contract_at
      ~amount:Tez.one
      ~src:"bootstrap1"
      ~init:"None"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "tickets_receive_and_store"]
      protocol
  in
  let tz_a = Constant.bootstrap2 in
  let* () =
    (* Check regressions for ticket updates in the transaction receipt. *)
    Client.transfer
      ~burn_cap:(Tez.of_int 2)
      ~amount:Tez.zero
      ~giver:tz_a.alias
      ~receiver:kt_a
      ~arg:(Format.sprintf {|(Pair "%s" "%s")|} kt_b kt_c)
      ~hooks
      client
  in
  let* () =
    (* Check that the ticket balance are expected via [ticket_balance] RPC and CLI. *)
    [
      (kt_a, kt_a, "string", "red", 1);
      (kt_a, kt_a, "string", "green", 2);
      (kt_a, kt_a, "string", "blue", 0);
      (kt_b, kt_a, "string", "blue", 1);
      (kt_c, kt_a, "string", "blue", 1);
    ]
    |> Lwt_list.iter_s
       @@ fun (contract, ticketer, content_type, content, expected) ->
       let* () =
         rpc_check_ticket_balance
           client
           ~contract
           ~ticketer
           ~content_type
           ~content
           ~expected
       in
       let* () =
         cli_check_ticket_balance
           client
           ~hooks
           ~contract
           ~ticketer
           ~content_type
           ~content
           ~expected
       in
       unit
  in
  let* () =
    (* Check regressions for the [all_ticket_balances] RPC and CLI when called for
       originated. *)
    [kt_a; kt_b; kt_c]
    |> Lwt_list.iter_s @@ fun contract ->
       let* _ =
         Client.RPC.call ~hooks client
         @@ RPC.get_chain_block_context_contract_all_ticket_balances
              ~id:contract
              ()
       in
       let*! _ = Client.all_ticket_balances ~hooks ~contract client in
       unit
  in
  let* () =
    (* Check regressions for the [all_ticket_balances] RPC when called for
       implicit. *)
    let*? process =
      Client.RPC.spawn ~hooks client
      @@ RPC.get_chain_block_context_contract_all_ticket_balances
           ~id:tz_a.public_key_hash
           ()
    in
    Process.check_error
      ~msg:
        (rex
           "(No service found at this URL|Did not find service|Rpc request \
            failed)")
      process
  in
  unit

let register ~protocols = test_ticket_receipt_and_rpc protocols
