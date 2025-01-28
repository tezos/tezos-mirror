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
    Component: Protocol (Ticket_balance_key)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_ticket_balance_key.ml
    Subject: Ticket balance key hashing
*)

open Protocol
open Alpha_context

let new_ctxt () =
  let open Lwt_result_wrap_syntax in
  let* block, _contract = Context.init1 () in
  let* incr = Incremental.begin_construction block in
  return @@ Incremental.alpha_ctxt incr

let make_contract ticketer =
  let open Lwt_result_wrap_syntax in
  let*?@ x = Contract.of_b58check ticketer in
  return x

let make_ex_token ctxt ~ticketer ~ty ~content =
  let open Lwt_result_wrap_syntax in
  let*?@ Script_ir_translator.Ex_comparable_ty cty, ctxt =
    let node = Micheline.root @@ Expr.from_string ty in
    Script_ir_translator.parse_comparable_ty ctxt node
  in
  let* ticketer = make_contract ticketer in
  let*@ contents, ctxt =
    let node = Micheline.root @@ Expr.from_string content in
    Script_ir_translator.parse_comparable_data ctxt cty node
  in
  return (Ticket_token.Ex_token {contents_type = cty; ticketer; contents}, ctxt)

let make_key ctxt ~ticketer ~ty ~content ~owner =
  let open Lwt_result_wrap_syntax in
  let* ex_token, ctxt = make_ex_token ctxt ~ticketer ~ty ~content in
  let* owner = make_contract owner in
  let*@ key, ctxt =
    Ticket_balance_key.of_ex_token
      ctxt
      ~owner:(Destination.Contract owner)
      ex_token
  in
  return (key, ctxt)

let equal_script_hash ~loc msg key1 key2 =
  Assert.equal ~loc Ticket_hash.equal msg Ticket_hash.pp key1 key2

let not_equal_script_hash ~loc msg key1 key2 =
  Assert.not_equal ~loc Ticket_hash.equal msg Ticket_hash.pp key1 key2

let assert_keys ~ticketer1 ~ticketer2 ~ty1 ~ty2 ~amount1 ~amount2 ~content1
    ~content2 ~owner1 ~owner2 assert_condition =
  let open Lwt_result_wrap_syntax in
  let* ctxt = new_ctxt () in
  let* key1, ctxt =
    make_key ctxt ~ticketer:ticketer1 ~ty:ty1 ~content:content1 ~owner:owner1
  in
  let* key2, _ =
    make_key ctxt ~ticketer:ticketer2 ~ty:ty2 ~content:content2 ~owner:owner2
  in
  assert_condition (key1, amount1) (key2, amount2)

let assert_keys_not_equal ~loc =
  assert_keys (fun (key1, _) (key2, _) ->
      not_equal_script_hash ~loc "Assert that keys are not equal" key1 key2)

let assert_keys_equal ~loc =
  assert_keys (fun (key1, _) (key2, _) ->
      equal_script_hash ~loc "Assert that keys are equal" key1 key2)

(** Test that tickets with two different amounts map to the same hash.
    The amount is not part of the ticket balance key. *)
let test_different_amounts () =
  assert_keys_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"unit"
    ~ty2:"unit"
    ~content1:"Unit"
    ~content2:"Unit"
    ~amount1:1
    ~amount2:2
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that two tickets with different ticketers map to different hashes. *)
let test_different_ticketers () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~ty1:"nat"
    ~ty2:"nat"
    ~content1:"1"
    ~content2:"1"
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that two tickets with different owners map to different hashes. *)
let test_different_owners () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"nat"
    ~ty2:"nat"
    ~content1:"1"
    ~content2:"1"
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"

(** Test that two tickets with different contents map to different hashes. *)
let test_different_content () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"nat"
    ~ty2:"nat"
    ~content1:"1"
    ~content2:"2"
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type nat and a ticket of type int, with the same
    content, map to different hashes. *)
let test_nat_int () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"nat"
    ~ty2:"int"
    ~content1:"1"
    ~content2:"1"
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type nat and a ticket of type mutez, with the same
    content, map to different hashes. *)
let test_nat_mutez () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"nat"
    ~ty2:"mutez"
    ~content1:"1"
    ~content2:"1"
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type nat and a ticket of type bool, with the
    contents (False/0), map to different hashes. *)
let test_bool_nat () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"bool"
    ~ty2:"nat"
    ~content1:"False"
    ~content2:"0"
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type nat and a ticket of type bytes, with the
    contents (0/0x), map to different hashes. *)
let test_nat_bytes () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"nat"
    ~ty2:"bytes"
    ~content1:"0"
    ~content2:"0x"
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type string and a chain_id with same content
    map to different hashes. *)
let test_string_chain_id () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"string"
    ~ty2:"chain_id"
    ~content1:{|"NetXynUjJNZm7wi"|}
    ~content2:{|"NetXynUjJNZm7wi"|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type string and a key_hash with same content
    map to different hashes. *)
let test_string_key_hash () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"string"
    ~ty2:"key_hash"
    ~content1:{|"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"|}
    ~content2:{|"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type string and a key with same content
    map to different hashes. *)
let test_string_key () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"string"
    ~ty2:"key"
    ~content1:{|"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"|}
    ~content2:{|"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type string and a timestamp with same content
    map to different hashes. *)
let test_string_timestamp () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"string"
    ~ty2:"timestamp"
    ~content1:{|"2019-09-26T10:59:51Z"|}
    ~content2:{|"2019-09-26T10:59:51Z"|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type string and a address with same content
    map to different hashes. *)
let test_string_address () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"string"
    ~ty2:"address"
    ~content1:{|"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi%entrypoint"|}
    ~content2:{|"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi%entrypoint"|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type string and a signature with same content
    map to different hashes. *)
let test_string_signature () =
  let signature =
    {|"edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7"|}
  in
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"string"
    ~ty2:"signature"
    ~content1:signature
    ~content2:signature
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Tests that annotations are not taken into account when hashing keys.
    Two comparable types that only differ in their annotations should
    map to to the same hash. Here, the type [pair int string] is identical to
    [pair (int %id) (string %name)].
    *)
let test_annotation_pair () =
  assert_keys_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"(pair int string)"
    ~ty2:{|(pair (int %id) (string %name))|}
    ~content1:{|Pair 1 "hello"|}
    ~content2:{|Pair 1 "hello"|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Tests that annotations are not taken into account when hashing keys.
    Here the types [or int string] and [or (int %id) (string %name)]
    should hash to the same key.
   *)
let test_annotation_or () =
  assert_keys_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"(or int string)"
    ~ty2:{|(or (int %id) (string %name))|}
    ~content1:{|Left 1|}
    ~content2:{|Left 1|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Tests that annotations are not taken into account when hashing keys.
    Here the types [int] and [(int :int_alias)] should hash to the same key.
   *)
let test_annotation_type_alias () =
  assert_keys_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"int"
    ~ty2:"(int :int_alias)"
    ~content1:"0"
    ~content2:"0"
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Tests that annotations are not taken into account when hashing keys.
    Here the types [pair (or int string) int] and
    [pair (or (int %id) (string %name)) int] should hash to the same key.
   *)
let test_annotation_pair_or () =
  assert_keys_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"pair (or int string) int"
    ~ty2:{|pair (or (int %id) (string %name)) int|}
    ~content1:{|Pair (Left 1) 2|}
    ~content2:{|Pair (Left 1) 2|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type [option int] and [option nat] with the same
    content, [None], don't map to the same hash. *)
let test_option_none () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"option int"
    ~ty2:"option nat"
    ~content1:{|None|}
    ~content2:{|None|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

(** Test that a ticket of type [option int] and [option nat] with the same
    content, [Some 0], don't map to the same hash. *)
let test_option_some () =
  assert_keys_not_equal
    ~loc:__LOC__
    ~ticketer1:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ticketer2:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
    ~ty1:"option int"
    ~ty2:"option nat"
    ~content1:{|Some 0|}
    ~content2:{|Some 0|}
    ~amount1:1
    ~amount2:1
    ~owner1:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
    ~owner2:"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

let tests =
  [
    Tztest.tztest "different ticketers" `Quick test_different_ticketers;
    Tztest.tztest "different owners" `Quick test_different_owners;
    Tztest.tztest "different content" `Quick test_different_content;
    Tztest.tztest "different amounts" `Quick test_different_amounts;
    Tztest.tztest "nat int" `Quick test_nat_int;
    Tztest.tztest "nat mutez" `Quick test_nat_mutez;
    Tztest.tztest "not bool" `Quick test_bool_nat;
    Tztest.tztest "nat bytes" `Quick test_nat_bytes;
    Tztest.tztest "string chain_id" `Quick test_string_chain_id;
    Tztest.tztest "string key_hash" `Quick test_string_key_hash;
    Tztest.tztest "string timestamp" `Quick test_string_timestamp;
    Tztest.tztest "string address" `Quick test_string_address;
    Tztest.tztest "string key" `Quick test_string_key;
    Tztest.tztest "string signature" `Quick test_string_signature;
    Tztest.tztest "annotations for pair" `Quick test_annotation_pair;
    Tztest.tztest "annotations for or" `Quick test_annotation_or;
    Tztest.tztest "annotations for type alias" `Quick test_annotation_type_alias;
    Tztest.tztest "annotations for paired ors" `Quick test_annotation_pair_or;
    Tztest.tztest "option none" `Quick test_option_none;
    Tztest.tztest "option some" `Quick test_option_some;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("ticket balance key", tests)]
  |> Lwt_main.run
