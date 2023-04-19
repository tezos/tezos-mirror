(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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
    Component:  Protocol (Alpha_context.Ticket_balance)
    Invocation: dune exec src/proto_017_PtNairob/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_ticket_storage.ml
    Subject:    Ticket storage functions tested using the Ticket_balance module in Alpha_context.
*)

open Protocol
open Alpha_context

let make_context () =
  let open Lwt_result_wrap_syntax in
  let* block, _contract = Context.init1 () in
  let* incr = Incremental.begin_construction block in
  return (Incremental.alpha_ctxt incr)

let hash_key ctxt ~ticketer ~ty ~contents ~owner =
  let ticketer = Micheline.root @@ Expr.from_string ticketer in
  let ty = Micheline.root @@ Expr.from_string ty in
  let contents = Micheline.root @@ Expr.from_string contents in
  let owner = Micheline.root @@ Expr.from_string owner in
  Alpha_context.Ticket_hash.make ctxt ~ticketer ~ty ~contents ~owner

let assert_balance ctxt ~loc key expected =
  let open Lwt_result_wrap_syntax in
  let*@ balance, _ = Ticket_balance.get_balance ctxt key in
  match balance with
  | Some b -> Assert.equal_int ~loc (Z.to_int b) expected
  | None -> failwith "Expected balance %d" expected

let assert_no_balance ctxt key =
  let open Lwt_result_wrap_syntax in
  let*@ balance, _ = Ticket_balance.get_balance ctxt key in
  match balance with
  | Some b -> failwith "Expected empty (none) balance but got %d" (Z.to_int b)
  | None -> return ()

let adjust_balance ctxt key delta =
  Ticket_balance.adjust_balance ctxt key ~delta:(Z.of_int delta)

let assert_non_overlapping_keys ~loc ~ticketer1 ~ticketer2 ~contents1 ~contents2
    ~ty1 ~ty2 ~owner1 ~owner2 =
  let open Lwt_result_wrap_syntax in
  let* ctxt = make_context () in
  let*?@ k1, ctxt =
    hash_key ctxt ~ticketer:ticketer1 ~ty:ty1 ~contents:contents1 ~owner:owner1
  in
  let*?@ k2, _ctxt =
    hash_key ctxt ~ticketer:ticketer2 ~ty:ty2 ~contents:contents2 ~owner:owner2
  in
  Assert.not_equal
    ~loc
    Ticket_hash.equal
    "Keys should not overlap"
    Ticket_hash.pp
    k1
    k2

let make_key ctxt content =
  hash_key
    ctxt
    ~ticketer:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~ty:"string"
    ~contents:(Printf.sprintf {|"%s"|} content)
    ~owner:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}

(** Test that key-hashes constructed from different ticketers don't overlap. *)
let test_non_overlapping_keys_ticketer () =
  assert_non_overlapping_keys
    ~loc:__LOC__
    ~ticketer1:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~ticketer2:{|"KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn"|}
    ~ty1:"nat"
    ~ty2:"int"
    ~contents1:{|"1"|}
    ~contents2:{|"1"|}
    ~owner1:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~owner2:{|"KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn"|}

(** Test that key-hashes constructed from different contents don't overlap. *)
let test_non_overlapping_keys_contents () =
  assert_non_overlapping_keys
    ~loc:__LOC__
    ~ticketer1:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~ticketer2:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~ty1:"string"
    ~ty2:"string"
    ~contents1:{|"red"|}
    ~contents2:{|"blue"|}
    ~owner1:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~owner2:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}

(** Test that key-hashes constructed from different content-types don't overlap. *)
let test_non_overlapping_keys_type () =
  assert_non_overlapping_keys
    ~loc:__LOC__
    ~ticketer1:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~ticketer2:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~ty1:"nat"
    ~ty2:"int"
    ~contents1:{|"1"|}
    ~contents2:{|"1"|}
    ~owner1:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~owner2:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}

(** Test that key-hashes constructed from different owners don't overlap. *)
let test_non_overlapping_keys_owner () =
  assert_non_overlapping_keys
    ~loc:__LOC__
    ~ticketer1:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~ticketer2:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~ty1:"nat"
    ~ty2:"int"
    ~contents1:{|"1"|}
    ~contents2:{|"1"|}
    ~owner1:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~owner2:{|"KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn"|}

(** Test that updating the ticket balance table has
    the intended effect.
  *)
let test_ticket_balance_single_update () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = make_context () in
  let*?@ alice_red, ctxt = make_key ctxt "alice_red" in
  let*@ _, ctxt = adjust_balance ctxt alice_red 1 in
  assert_balance ctxt ~loc:__LOC__ alice_red 1

(** Test that updating the ticket-balance table with different keys
    updates both entries. *)
let test_ticket_balance_different_owners () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = make_context () in
  let*?@ alice_red, ctxt = make_key ctxt "alice_red" in
  let*?@ alice_blue, ctxt = make_key ctxt "alice_blue" in
  let*@ _, ctxt = adjust_balance ctxt alice_red 1 in
  let*@ _, ctxt = adjust_balance ctxt alice_blue 1 in
  let* () = assert_balance ctxt ~loc:__LOC__ alice_red 1 in
  let* () = assert_balance ctxt ~loc:__LOC__ alice_blue 1 in
  return ()

(** Test updating the same entry with multiple updates yields
    the net result of all balance updates *)
let test_ticket_balance_multiple_updates () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = make_context () in
  let*?@ alice_red, ctxt = make_key ctxt "alice_red" in
  let*@ _, ctxt = adjust_balance ctxt alice_red 1 in
  let*@ _, ctxt = adjust_balance ctxt alice_red 2 in
  let*@ _, ctxt = adjust_balance ctxt alice_red (-1) in
  assert_balance ctxt ~loc:__LOC__ alice_red 2

(** Test that with no updates to the table, no balance is present in
    the table *)
let test_empty_balance () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = make_context () in
  let*?@ alice_red, ctxt = make_key ctxt "alice_red" in
  assert_no_balance ctxt alice_red

(** Test that adding one entry with positive balance and then
    updating with a negative balance also removes the entry *)
let test_empty_balance_after_update () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = make_context () in
  let*?@ alice_red, ctxt = make_key ctxt "alice_red" in
  let*@ _, ctxt = adjust_balance ctxt alice_red 1 in
  let*@ _, ctxt = adjust_balance ctxt alice_red (-1) in
  assert_no_balance ctxt alice_red

(** Test that attempting to update an entry with a negative balance
    results in an error. *)
let test_negative_balance () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = make_context () in
  let*?@ alice_red, ctxt = make_key ctxt "alice_red" in
  let*! res = wrap @@ adjust_balance ctxt alice_red (-1) in
  Assert.proto_error ~loc:__LOC__ res (fun _err -> true)

(** Test that positive storage spaces are returned for operations
    resulting in extra storage space and negative for ones that frees up storage.
    *)
let test_storage_space () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = make_context () in
  let*?@ alice_red, ctxt = make_key ctxt "alice_red" in
  (* Space for adding an entry is 65 for the key plus 1 for the value. *)
  let*@ space, ctxt = adjust_balance ctxt alice_red 1 in
  let* () = Assert.equal_int ~loc:__LOC__ 66 (Z.to_int space) in
  (* Adding one does not consume additional space. *)
  let*@ space, ctxt = adjust_balance ctxt alice_red 1 in
  let* () = Assert.equal_int ~loc:__LOC__ 0 (Z.to_int space) in
  (* Adding a big balance costs extra. *)
  let*@ space, ctxt = adjust_balance ctxt alice_red 1000 in
  let* () = Assert.equal_int ~loc:__LOC__ 1 (Z.to_int space) in
  (* Reset balance to zero should free up space.
     The freed up space is 65 for the key + 2 for the value *)
  let*@ b, ctxt = Ticket_balance.get_balance ctxt alice_red in
  let*@ space, ctxt =
    Ticket_balance.adjust_balance
      ctxt
      alice_red
      ~delta:(Z.neg @@ Option.value ~default:Z.zero b)
  in
  let* () = Assert.equal_int ~loc:__LOC__ (-67) (Z.to_int space) in
  (* Adjusting the space to 0 again should not free anything *)
  let*@ space, ctxt = adjust_balance ctxt alice_red 0 in
  let* () = Assert.equal_int ~loc:__LOC__ 0 (Z.to_int space) in
  (* Adding a balance requiers extra space. *)
  let*@ space, _ = adjust_balance ctxt alice_red 10 in
  Assert.equal_int ~loc:__LOC__ 66 (Z.to_int space)

let tests =
  [
    Tztest.tztest
      "no overlapping keys for ticketer"
      `Quick
      test_non_overlapping_keys_ticketer;
    Tztest.tztest
      "no overlapping keys for content"
      `Quick
      test_non_overlapping_keys_contents;
    Tztest.tztest
      "no overlapping keys for content type"
      `Quick
      test_non_overlapping_keys_type;
    Tztest.tztest
      "no overlapping keys for owner"
      `Quick
      test_non_overlapping_keys_owner;
    Tztest.tztest
      "ticket balance single update"
      `Quick
      test_ticket_balance_single_update;
    Tztest.tztest "empty balance" `Quick test_empty_balance;
    Tztest.tztest
      "empty balance after update"
      `Quick
      test_empty_balance_after_update;
    Tztest.tztest "negative balance" `Quick test_negative_balance;
    Tztest.tztest
      "ticket balance multiple updates"
      `Quick
      test_ticket_balance_multiple_updates;
    Tztest.tztest
      "ticket balance different owners"
      `Quick
      test_ticket_balance_different_owners;
    Tztest.tztest "ticket storage space" `Quick test_storage_space;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("ticket storage", tests)]
  |> Lwt_main.run
