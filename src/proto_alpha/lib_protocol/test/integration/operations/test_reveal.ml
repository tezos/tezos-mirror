(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
    Component:  Protocol (revelation)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                -- test "^revelation$"
    Subject:    On the reveal operation.
*)

(** Test for the [Reveal] operation. *)

open Protocol
open Alpha_context
open Test_tez

let ten_tez = of_int 10

let test_simple_reveal () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract Tez.one >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  (Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation"
   | false -> ())
  >>=? fun () ->
  (* Reveal the contract *)
  Op.revelation (B blk) new_c.pk >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
  | true -> ()
  | false -> Stdlib.failwith "New contract revelation failed."

let test_empty_account_on_reveal () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  let amount = Tez.one_mutez in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract amount >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  (Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation"
   | false -> ())
  >>=? fun () ->
  (* Reveal the contract *)
  Op.revelation ~fee:amount (B blk) new_c.pk >>=? fun operation ->
  Incremental.begin_construction blk >>=? fun inc ->
  Incremental.add_operation inc operation >>=? fun _ ->
  Block.bake blk ~operation >>=? fun blk ->
  Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
  | false -> ()
  | true -> Stdlib.failwith "Empty account still exists and is revealed."

let test_not_enough_found_for_reveal () =
  Context.init1 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract Tez.one_mutez >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  (Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation"
   | false -> ())
  >>=? fun () ->
  (* Reveal the contract *)
  Op.revelation ~fee:Tez.fifty_cents (B blk) new_c.pk >>=? fun operation ->
  Block.bake blk ~operation >>= fun res ->
  Assert.proto_error_with_info ~loc:__LOC__ res "Balance too low"

let test_transfer_fees_emptying_after_reveal_batched () =
  Context.init1 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract Tez.one >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  Incremental.begin_construction blk >>=? fun inc ->
  Op.revelation ~fee:Tez.zero (I inc) new_c.pk >>=? fun reveal ->
  Incremental.add_operation inc reveal >>=? fun tmp_inc ->
  Op.transaction ~fee:Tez.one (I tmp_inc) new_contract c Tez.one
  >>=? fun transaction ->
  Op.batch_operations ~source:new_contract (I inc) [reveal; transaction]
  >>=? fun op ->
  (* This operation is expected to fail at application time, not
     during validation. *)
  Incremental.add_operation ~expect_failure:(fun _ -> return_unit) inc op
  >>=? fun _inc -> return_unit

let tests =
  [
    Tztest.tztest "simple reveal" `Quick test_simple_reveal;
    Tztest.tztest "empty account on reveal" `Quick test_empty_account_on_reveal;
    Tztest.tztest
      "not enough found for reveal"
      `Quick
      test_not_enough_found_for_reveal;
    Tztest.tztest
      "transfer fees emptying balance after reveal in batch"
      `Quick
      test_transfer_fees_emptying_after_reveal_batched;
  ]
