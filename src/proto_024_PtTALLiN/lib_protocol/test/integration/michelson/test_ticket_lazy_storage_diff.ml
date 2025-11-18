(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:  Protocol (Ticket_scanner)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_ticket_lazy_storage_diff.ml
    Subject:    Ticket scanner tests
*)

open Protocol
open Alpha_context

let assert_equal_string_list ~loc msg =
  Assert.assert_equal_list ~loc String.equal msg Format.pp_print_string

let string_list_of_ex_token_diffs ctxt token_diffs =
  let open Lwt_result_wrap_syntax in
  let accum (xs, ctxt)
      (Ticket_token.Ex_token {ticketer; contents_type; contents}, amount) =
    let*@ x, ctxt =
      Script_ir_unparser.unparse_comparable_data
        ctxt
        Script_ir_unparser.Readable
        contents_type
        contents
    in
    let str =
      Format.asprintf
        "((%a, %a), %a)"
        Contract.pp
        ticketer
        Michelson_v1_printer.print_expr
        x
        Z.pp_print
        amount
    in
    return (str :: xs, ctxt)
  in
  let* xs, ctxt = List.fold_left_es accum ([], ctxt) token_diffs in
  return (List.rev xs, ctxt)

let make_ex_token ctxt ~ticketer ~type_exp ~content_exp =
  let open Lwt_result_wrap_syntax in
  let*?@ Script_ir_translator.Ex_comparable_ty contents_type, ctxt =
    let node = Micheline.root @@ Expr.from_string type_exp in
    Script_ir_translator.parse_comparable_ty ctxt node
  in
  let*@ ticketer = Lwt.return @@ Contract.of_b58check ticketer in
  let*@ contents, ctxt =
    let node = Micheline.root @@ Expr.from_string content_exp in
    Script_ir_translator.parse_comparable_data ctxt contents_type node
  in
  return (Ticket_token.Ex_token {ticketer; contents_type; contents}, ctxt)

let assert_equal_balances ~loc ctxt given expected =
  let open Lwt_result_wrap_syntax in
  let* ctxt, tbs1 =
    List.fold_left_map_es
      (fun ctxt ((ticketer, content), delta) ->
        let+ token, ctxt =
          make_ex_token
            ctxt
            ~ticketer
            ~type_exp:"string"
            ~content_exp:(Printf.sprintf "%S" content)
        in
        (ctxt, (token, Z.of_int delta)))
      ctxt
      expected
  in
  let* tbs1, ctxt = string_list_of_ex_token_diffs ctxt tbs1 in
  let* tbs2, _ctxt = string_list_of_ex_token_diffs ctxt given in
  assert_equal_string_list
    ~loc
    "Compare token balances"
    (List.sort String.compare tbs1)
    (List.sort String.compare tbs2)

let updates_of_key_values ctxt key_values =
  let open Lwt_result_wrap_syntax in
  List.fold_right_es
    (fun (key, value) (kvs, ctxt) ->
      let*@ key_hash, ctxt =
        Script_ir_translator.hash_comparable_data
          ctxt
          Script_typed_ir.int_t
          (Script_int.of_int key)
      in
      return
        ( {
            Big_map.key = Expr.from_string @@ string_of_int key;
            key_hash;
            value = Option.map Expr.from_string value;
          }
          :: kvs,
          ctxt ))
    key_values
    ([], ctxt)

let make_alloc big_map_id alloc updates =
  Lazy_storage.make
    Lazy_storage.Kind.Big_map
    big_map_id
    (Update {init = Lazy_storage.Alloc alloc; updates})

let init () =
  let open Lwt_result_wrap_syntax in
  let* block, source = Context.init1 () in
  let* operation, originated =
    Op.contract_origination_hash (B block) source ~script:Op.dummy_script
  in
  let* block = Block.bake ~operation block in
  let* inc = Incremental.begin_construction block in
  return (originated, Incremental.alpha_ctxt inc)

let setup ctxt contract ~key_type ~value_type entries =
  let open Lwt_result_wrap_syntax in
  let*@ ctxt, big_map_id = Big_map.fresh ~temporary:false ctxt in
  let key_type = Expr.from_string key_type in
  let value_type = Expr.from_string value_type in
  let* updates, ctxt = updates_of_key_values ctxt entries in
  let alloc = make_alloc big_map_id Big_map.{key_type; value_type} updates in
  return (alloc, big_map_id, contract, ctxt)

let new_big_map ctxt contract ~key_type ~value_type entries =
  let open Lwt_result_wrap_syntax in
  let* alloc, big_map_id, contract, ctxt =
    setup ctxt contract ~key_type ~value_type
    @@ List.map (fun (k, v) -> (k, Some v)) entries
  in
  let storage = Expr.from_string "{}" in
  let*@ ctxt =
    Contract.update_script_storage ctxt contract storage (Some [alloc])
  in
  return (big_map_id, ctxt)

let alloc_diff ctxt contract ~key_type ~value_type entries =
  let open Lwt_result_wrap_syntax in
  let* allocations, _, _, ctxt =
    setup
      ctxt
      contract
      ~key_type
      ~value_type
      (List.map (fun (k, v) -> (k, Some v)) entries)
  in
  return (allocations, ctxt)

let remove_diff ctxt contract ~key_type ~value_type ~existing_entries =
  let open Lwt_result_wrap_syntax in
  let* big_map_id, ctxt =
    new_big_map ctxt contract ~key_type ~value_type existing_entries
  in
  return (Lazy_storage.make Lazy_storage.Kind.Big_map big_map_id Remove, ctxt)

let copy_diff ctxt contract ~key_type ~value_type ~existing_entries ~updates =
  let open Lwt_result_wrap_syntax in
  let* big_map_id, ctxt =
    new_big_map ctxt contract ~key_type ~value_type existing_entries
  in
  let* updates, ctxt = updates_of_key_values ctxt updates in
  let*@ ctxt, new_big_map_id = Big_map.fresh ctxt ~temporary:false in
  return
    ( Lazy_storage.make
        Lazy_storage.Kind.Big_map
        new_big_map_id
        (Update {init = Lazy_storage.Copy {src = big_map_id}; updates}),
      ctxt )

let existing_diff ctxt contract ~key_type ~value_type ~existing_entries ~updates
    =
  let open Lwt_result_wrap_syntax in
  let* big_map_id, ctxt =
    new_big_map ctxt contract ~key_type ~value_type existing_entries
  in
  let* updates, ctxt = updates_of_key_values ctxt updates in
  return
    ( Lazy_storage.make
        Lazy_storage.Kind.Big_map
        big_map_id
        (Update {init = Lazy_storage.Existing; updates}),
      ctxt )

(** Test that no ticket-tokens are extracted from a diff for allocating an empty
    big-map. *)
let test_allocate_new_empty () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    alloc_diff ctxt contract ~key_type:"int" ~value_type:"ticket string" []
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances ~loc:__LOC__ ctxt diff []

(** Test that no ticket-tokens are extracted from a lazy-diff of a big-map
    that does not contain tickets. *)
let test_allocate_new_no_tickets () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    alloc_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"string"
      [(1, {|"A"|}); (2, {|"B"|}); (3, {|"C"|})]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances ~loc:__LOC__ ctxt diff []

(** Test that ticket-tokens can be extracted from a lazy-diff for allocating a
    new big-map. *)
let test_allocate_new () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    alloc_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      [
        (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|});
        (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2|});
        (3, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3|});
      ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), 2);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), 3);
    ]

(** Test that ticket-tokens with negative balances are extracted from a
    lazy-diff that removes a big-map. *)
let test_remove_big_map () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    remove_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [
          (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|});
          (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2|});
          (3, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3|});
        ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), -2);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), -3);
    ]

(** Test that there are no ticket-token balance deltas extracted from a
    lazy-diff that applies no updates. *)
let test_no_updates_to_existing_big_map () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    existing_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [
          (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|});
          (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2|});
          (3, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3|});
        ]
      ~updates:[]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances ~loc:__LOC__ ctxt diff []

(** Test that ticket-tokens extracted reflect the balance diffs that are
    extracted from a lazy-diff that modifies an existing big-map.
 *)
let test_update_existing_big_map () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    existing_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [
          (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|});
          (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2|});
          (3, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3|});
        ]
      ~updates:
        [
          (* Replace entry at index 1 *)
          (1, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "yellow" 4|});
          (* Remove entry at index 2 *)
          (2, None);
          (* Add new entry at index 4 *)
          (4, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "pink" 5|});
        ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), -2);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), 4);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "pink"), 5);
    ]

(** Test that ticket-tokens extracted reflect the balance diffs that are
    extracted from a lazy-diff that modifies an existing big-map and with
    multiple updates to the same key.
 *)
let test_update_same_key_multiple_times_existing_big_map () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    existing_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [(1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|})]
      ~updates:
        [
          (* Replace entry at index 1 *)
          (1, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "yellow" 1|});
          (* Replace entry at index 1 *)
          (1, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1|});
        ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), 1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), 1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), -1);
    ]

(** Test that ticket-tokens extracted reflect the balance diffs that are
    extracted from a lazy-diff that modifies an existing big-map and with
    multiple removals of the same item.
 *)
let test_remove_same_key_multiple_times_existing_big_map () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    existing_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [(1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|})]
      ~updates:
        [
          (* Remove entry at index 1 *)
          (1, None);
          (* Remove entry at index 1 again *)
          (1, None);
        ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [(("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1)]

(** Test that ticket-tokens extracted reflect the balance diffs that are
    extracted from a lazy-diff that modifies an existing big-map and with
    multiple additions and removals of the same item.
 *)
let test_update_and_remove_same_key_multiple_times_existing_big_map () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    existing_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [(1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|})]
      ~updates:
        [
          (* Remove entry at index 1 *)
          (1, None);
          (* Add a yellow ticket at index 1. *)
          (1, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "yellow" 1|});
          (* Remove entry at index 1 again *)
          (1, None);
          (* Add a green ticket at index 1. *)
          (1, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1|});
        ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), 1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), 1);
    ]

(** Test that the extracted ticket-tokens from a lazy diff for copying a big-map
    reflects the tokens of the source as well as the updates. *)
let test_copy_big_map () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    copy_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [
          (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|});
          (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2|});
          (3, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3|});
        ]
      ~updates:[]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), 2);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), 3);
    ]

(** Test that the extracted ticket-tokens from a lazy diff for copying a big-map
    reflects the tokens of the source as well as the updates. *)
let test_copy_big_map_with_updates () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    copy_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [
          (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|});
          (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2|});
          (3, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3|});
        ]
      ~updates:
        [
          (* Remove element at index 1*)
          (1, None);
          (* Replace element at index 3 *)
          (3, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "yellow" 4|});
          (* Add a new element at index 4 *)
          (4, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "pink" 5|});
        ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), 2);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), 3);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), -3);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), 4);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "pink"), 5);
    ]

(** Test that the extracted ticket-tokens from a lazy diff for copying a big-map
    with multiple updates to the same key reflects the tokens of the source as
    well as the updates. *)
let test_copy_big_map_with_updates_to_same_key () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff, ctxt =
    copy_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [(1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|})]
      ~updates:
        [
          (* Remove element at index 1 *)
          (1, None);
          (* Add element at index 1 *)
          (1, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "yellow" 2|});
          (* Remove again *)
          (1, None);
        ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff ctxt [diff]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (* From the copy of the big-map *)
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 1);
      (* From removing the element from the copied big-map *)
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (* From adding to the copied big-map. *)
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), 2);
      (* From removing from the copied big-map *)
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), -2);
    ]

(** Test combinations of lazy-diffs. *)
let test_mix_lazy_diffs () =
  let open Lwt_result_wrap_syntax in
  let* contract, ctxt = init () in
  let* diff_copy, ctxt =
    copy_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [(1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|})]
      ~updates:
        [
          (* Remove element at index 1 *)
          (1, None);
          (* Replace element at index 2 *)
          (2, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2|});
        ]
  in
  let* diff_existing, ctxt =
    existing_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [(1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|})]
      ~updates:
        [
          (* Remove entry at index 2 *)
          (2, None);
          (* Add new entry at index 3 *)
          (3, Some {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3|});
        ]
  in
  let* diff_remove, ctxt =
    remove_diff
      ctxt
      contract
      ~key_type:"int"
      ~value_type:"ticket string"
      ~existing_entries:
        [
          (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "white" 1|});
          (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "black" 1|});
        ]
  in
  let*@ diff, ctxt =
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff
      ctxt
      [diff_copy; diff_existing; diff_remove]
  in
  assert_equal_balances
    ~loc:__LOC__
    ctxt
    diff
    [
      (* From the copy *)
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), 2);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 1);
      (* From updating an existing *)
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), 3);
      (* From the remove *)
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "white"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "black"), -1);
    ]

let tests =
  [
    Tztest.tztest "allocate new empty" `Quick test_allocate_new_empty;
    Tztest.tztest "allocate new" `Quick test_allocate_new;
    Tztest.tztest "allocate new no tickets" `Quick test_allocate_new_no_tickets;
    Tztest.tztest "Remove" `Quick test_remove_big_map;
    Tztest.tztest
      "no updates to existing"
      `Quick
      test_no_updates_to_existing_big_map;
    Tztest.tztest "update existing big-map" `Quick test_update_existing_big_map;
    Tztest.tztest
      "update same key multiple times on existing big-map"
      `Quick
      test_update_same_key_multiple_times_existing_big_map;
    Tztest.tztest
      "remove same key multiple times on existing big-map"
      `Quick
      test_remove_same_key_multiple_times_existing_big_map;
    Tztest.tztest
      "update and remove same key multiple times on existing big-map"
      `Quick
      test_update_and_remove_same_key_multiple_times_existing_big_map;
    Tztest.tztest "copy" `Quick test_copy_big_map;
    Tztest.tztest "copy with updates" `Quick test_copy_big_map_with_updates;
    Tztest.tztest
      "copy with multiple updates to same key"
      `Quick
      test_copy_big_map_with_updates_to_same_key;
    Tztest.tztest "mix lazy diffs" `Quick test_mix_lazy_diffs;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("ticket lazy storage diff", tests)]
  |> Lwt_main.run
