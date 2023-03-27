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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/main.exe
    Subject:    Ticket scanner tests
*)

open Protocol
open Alpha_context

let assert_fails ~loc ?error m =
  let open Lwt_result_syntax in
  let*! res = m in
  let rec aux err_res =
    match (err_res, error) with
    | Environment.Ecoproto_error err' :: rest, Some err ->
        if err = err' then return_unit else aux rest
    | _, Some _ ->
        (* Expected a different error. *)
        let msg =
          Printf.sprintf "Expected a different error at location %s" loc
        in
        Stdlib.failwith msg
    | _, None ->
        (* Any error is ok. *)
        return ()
  in
  match res with
  | Ok _ -> Stdlib.failwith "Expected failure"
  | Error err_res -> aux err_res

let new_ctxt () =
  let open Lwt_result_wrap_syntax in
  let* block, _contract = Context.init1 () in
  let* incr = Incremental.begin_construction block in
  return @@ Incremental.alpha_ctxt incr

let assert_equal_string_list ~loc msg =
  Assert.assert_equal_list ~loc String.equal msg Format.pp_print_string

let string_list_of_ex_tickets ctxt tickets =
  let open Lwt_result_wrap_syntax in
  let accum (xs, ctxt)
      (Ticket_scanner.Ex_ticket
        (cty, {Script_typed_ir.ticketer; contents; amount})) =
    let*@ x, ctxt =
      Script_ir_translator.unparse_data
        ctxt
        Script_ir_unparser.Readable
        cty
        contents
    in
    let content =
      Format.kasprintf Fun.id "%a" Michelson_v1_printer.print_expr x
    in
    let str =
      Format.kasprintf
        Fun.id
        "(%a, %s, %a)"
        Contract.pp
        ticketer
        content
        Z.pp_print
        Script_int.(to_zint (amount :> n num))
    in
    return (str :: xs, ctxt)
  in
  let* xs, ctxt = List.fold_left_es accum ([], ctxt) tickets in
  return (List.rev xs, ctxt)

let make_ex_ticket ctxt ~ticketer ~type_exp ~content_exp ~amount =
  let open Lwt_result_wrap_syntax in
  let*?@ Script_ir_translator.Ex_comparable_ty cty, ctxt =
    let node = Micheline.root @@ Expr.from_string type_exp in
    Script_ir_translator.parse_comparable_ty ctxt node
  in
  let*?@ ticketer = Contract.of_b58check ticketer in
  let*@ contents, ctxt =
    let node = Micheline.root @@ Expr.from_string content_exp in
    Script_ir_translator.parse_comparable_data ctxt cty node
  in
  let amount = Script_int.(abs @@ of_int amount) in
  let amount =
    WithExceptions.Option.get ~loc:__LOC__ @@ Ticket_amount.of_n amount
  in
  let ticket = Script_typed_ir.{ticketer; contents; amount} in
  return (Ticket_scanner.Ex_ticket (cty, ticket), ctxt)

let assert_equals_ex_tickets ctxt ~loc ex_tickets expected =
  let open Lwt_result_wrap_syntax in
  let* str_tickets, ctxt = string_list_of_ex_tickets ctxt ex_tickets in
  let* str_tickets_expected, _ctxt = string_list_of_ex_tickets ctxt expected in
  assert_equal_string_list
    ~loc
    "Compare with expected tickets"
    (List.sort String.compare str_tickets)
    (List.sort String.compare str_tickets_expected)

let tickets_of_value ctxt ~include_lazy ~type_exp ~value_exp =
  let open Lwt_result_wrap_syntax in
  let Script_typed_ir.Ex_ty ty, ctxt =
    let node = Micheline.root @@ Expr.from_string type_exp in
    Result.value_f
      ~default:(fun () -> Stdlib.failwith "Failed to parse")
      (Script_ir_translator.parse_any_ty ctxt ~legacy:false node)
  in
  let node = Micheline.root @@ Expr.from_string value_exp in
  let*@ value, ctxt =
    Script_ir_translator.parse_data
      ctxt
      ~elab_conf:(Script_ir_translator_config.make ~legacy:false ())
      ~allow_forged:true
      ty
      node
  in
  let*?@ ht, ctxt = Ticket_scanner.type_has_tickets ctxt ty in
  wrap @@ Ticket_scanner.tickets_of_value ctxt ~include_lazy ht value

let assert_contains_tickets ctxt ~loc ~include_lazy ~type_exp ~value_exp
    expected =
  let open Lwt_result_wrap_syntax in
  let* ex_tickets, _ =
    tickets_of_value ctxt ~include_lazy ~type_exp ~value_exp
  in
  assert_equals_ex_tickets ctxt ~loc ex_tickets expected

let assert_fail_non_empty_overlay ctxt ~loc ~include_lazy ~type_exp ~value_exp =
  tickets_of_value ctxt ~include_lazy ~type_exp ~value_exp >>= fun res ->
  match res with
  | Error [x] ->
      let x = Format.kasprintf Fun.id "%a" Error_monad.pp x in
      Assert.equal
        ~loc
        String.equal
        ""
        Format.pp_print_string
        "Unsupported big-map value with non-empty overlay"
        x
  | _ -> failwith "Expected an error at %s" loc

let make_string_tickets ctxt ticketer_amounts =
  let open Lwt_result_wrap_syntax in
  List.fold_right_es
    (fun (ticketer, content, amount) (tickets, ctxt) ->
      let* ticket, ctxt =
        make_ex_ticket
          ctxt
          ~ticketer
          ~type_exp:"string"
          ~content_exp:(Printf.sprintf {|"%s"|} content)
          ~amount
      in
      return (ticket :: tickets, ctxt))
    ticketer_amounts
    ([], ctxt)

let tickets_from_big_map_ref ~pre_populated value_exp =
  let open Lwt_result_wrap_syntax in
  let* block, source = Context.init1 () in
  let* operation, originated =
    Op.contract_origination_hash (B block) source ~script:Op.dummy_script
  in
  let* block = Block.bake ~operation block in
  let* inc = Incremental.begin_construction block in
  let ctxt = Incremental.alpha_ctxt inc in
  let*@ ctxt, big_map_id = Big_map.fresh ~temporary:false ctxt in
  let int_ty_expr = Expr.from_string "int" in
  let* diffs, ctxt =
    let* updates, ctxt =
      List.fold_left_es
        (fun (kvs, ctxt) (key, value) ->
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
                value = Some (Expr.from_string value);
              }
              :: kvs,
              ctxt ))
        ([], ctxt)
        pre_populated
    in
    let alloc =
      Big_map.
        {key_type = int_ty_expr; value_type = Expr.from_string "ticket string"}
    in
    return
      ( [
          Lazy_storage.make
            Lazy_storage.Kind.Big_map
            big_map_id
            (Update {init = Lazy_storage.Alloc alloc; updates});
        ],
        ctxt )
  in
  let*@ ctxt =
    Contract.update_script_storage ctxt originated int_ty_expr (Some diffs)
  in
  let value_exp =
    value_exp @@ Z.to_string (Big_map.Id.unparse_to_z big_map_id)
  in
  return (value_exp, ctxt)

let assert_big_map_int_ticket_string_ref ~loc ~pre_populated ~big_map_exp
    ex_tickets =
  let open Lwt_result_wrap_syntax in
  let* value_exp, ctxt = tickets_from_big_map_ref ~pre_populated big_map_exp in
  let* ex_tickets, ctxt = make_string_tickets ctxt ex_tickets in
  assert_contains_tickets
    ctxt
    ~include_lazy:true
    ~loc
    ~type_exp:"big_map int (ticket string)"
    ~value_exp
    ex_tickets

let assert_fail_non_empty_overlay_with_big_map_ref ~loc ~pre_populated
    ~big_map_exp =
  let open Lwt_result_wrap_syntax in
  let* value_exp, ctxt = tickets_from_big_map_ref ~pre_populated big_map_exp in
  assert_fail_non_empty_overlay
    ctxt
    ~include_lazy:true
    ~loc
    ~type_exp:"big_map int (ticket string)"
    ~value_exp

let assert_string_tickets ~loc ~include_lazy ~type_exp ~value_exp ~expected =
  let open Lwt_result_wrap_syntax in
  let* ctxt = new_ctxt () in
  let* ex_tickets, ctxt = make_string_tickets ctxt expected in
  let* () =
    assert_contains_tickets
      ctxt
      ~include_lazy
      ~loc
      ~type_exp
      ~value_exp
      ex_tickets
  in
  assert_contains_tickets
    ctxt
    ~include_lazy
    ~loc
    ~type_exp
    ~value_exp
    ex_tickets

(** Test that the ticket can be extracted from a a single unit ticket *)
let test_tickets_in_unit_ticket () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = new_ctxt () in
  let type_exp = "ticket(unit)" in
  let value_exp = {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" Unit 10|} in
  let* ex_ticket, ctxt =
    make_ex_ticket
      ctxt
      ~ticketer:"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"
      ~type_exp:"unit"
      ~content_exp:"Unit"
      ~amount:10
  in
  assert_contains_tickets
    ctxt
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp
    ~value_exp
    [ex_ticket]

let assert_string_tickets_fail_on_zero_amount ~loc ~include_lazy ~type_exp
    ~value_exp =
  let open Lwt_result_wrap_syntax in
  let* ctxt = new_ctxt () in
  assert_fails ~loc ~error:Script_tc_errors.Forbidden_zero_ticket_quantity
  @@ tickets_of_value ctxt ~include_lazy ~type_exp ~value_exp

let test_tickets_in_list_with_zero_amount () =
  assert_string_tickets_fail_on_zero_amount
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"list(ticket(string))"
    ~value_exp:
      {|
        {
          Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1;
          Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2;
          Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3;
          Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "orange" 0;
        }
      |}

(** Test that all tickets can be extracted from a list of tickets *)
let test_tickets_in_list () =
  assert_string_tickets
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"list(ticket(string))"
    ~value_exp:
      {|
        {
          Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1;
          Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2;
          Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3;
        }
      |}
    ~expected:
      [
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red", 1);
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green", 2);
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue", 3);
      ]

let test_tickets_in_pair_with_zero_amount () =
  assert_string_tickets_fail_on_zero_amount
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"pair (ticket string) (ticket string) (ticket string)"
    ~value_exp:
      {|
        Pair
          (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1)
          (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2)
          (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 0)
      |}

(** Test that all tickets can be extracted from a pair of tickets *)
let test_tickets_in_pair () =
  assert_string_tickets
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"pair (ticket string) (ticket string)"
    ~value_exp:
      {|
        Pair
          (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1)
          (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2)
      |}
    ~expected:
      [
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red", 1);
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green", 2);
      ]

let test_tickets_in_map_with_zero_amount () =
  assert_string_tickets_fail_on_zero_amount
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"map int (ticket string)"
    ~value_exp:
      {|
        {
          Elt 1 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1);
          Elt 2 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2);
          Elt 3 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 0);
        }
    |}

(** Test that all tickets from a map can be extracted. *)
let test_tickets_in_map () =
  assert_string_tickets
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"map int (ticket string)"
    ~value_exp:
      {|
        {
          Elt 1 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1);
          Elt 2 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2);
        }
    |}
    ~expected:
      [
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red", 1);
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green", 2);
      ]

(** Test that all tickets from a big-map with non-empty overlay fails.
    If we extend the ticket scanner function to support non-empty overlays
    this test needs to be adapted.
  *)
let test_tickets_in_big_map () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = new_ctxt () in
  assert_fail_non_empty_overlay
    ctxt
    ~loc:__LOC__
    ~include_lazy:true
    ~type_exp:"big_map int (ticket string)"
    ~value_exp:
      {|
        {
          Elt 1 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1);
          Elt 2 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2);
        }
      |}

(** Test that tickets are not extracted from big-map with [include_lazy] set
    to false. *)
let test_tickets_in_big_map_strict_only () =
  assert_string_tickets
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"big_map int (ticket string)"
    ~value_exp:
      {|
        {
          Elt 1 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1);
          Elt 2 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 2);
          Elt 3 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 3);
        }
      |}
    ~expected:[]

(** Test that tickets can be extracted from a list of tickets inside a big-map
    This fails due to non-empty overlay. If we extend the ticket scanner
    function to support non-empty overlays this test needs to be adapted.
*)
let test_tickets_in_list_in_big_map () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = new_ctxt () in
  assert_fail_non_empty_overlay
    ctxt
    ~loc:__LOC__
    ~include_lazy:true
    ~type_exp:"(big_map int (list(ticket string)))"
    ~value_exp:
      {|
        {
          Elt 1 {
            Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 ;
            Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1
          };
          Elt 2 {
            Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1 ;
            Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "orange" 1
          }
        }
      |}

(** Test that tickets can be extracted from a combination of a list and lazy structure
    and that only the strict part is considered with [include_lazy] set to fasle *)
let test_tickets_in_pair_big_map_and_list_strict_only () =
  assert_string_tickets
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"pair (big_map int (ticket string)) (list (ticket string))"
    ~value_exp:
      {|
        Pair
          {
            Elt 1 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1);
            Elt 2 (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1)
          }
          {
            Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1;
            Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "orange" 1
          }
      |}
    ~expected:
      [
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue", 1);
        ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "orange", 1);
      ]

(** Test that tickets can be extracted from the left side of an or-expression. *)
let test_tickets_in_or_left () =
  assert_string_tickets
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"or (ticket string) int"
    ~value_exp:{| Left (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1) |}
    ~expected:[("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red", 1)]

(** Test that tickets from the left side of an or-expression with zero amount
    are rejected. *)
let test_tickets_in_or_left_with_zero_amount () =
  assert_string_tickets_fail_on_zero_amount
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"or (ticket string) int"
    ~value_exp:{| Left (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 0) |}

(** Test that tickets can be extracted from the right side of an or-expression. *)
let test_tickets_in_or_right () =
  assert_string_tickets
    ~loc:__LOC__
    ~include_lazy:false
    ~type_exp:"or int (ticket string)"
    ~value_exp:{| Right (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1) |}
    ~expected:[("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red", 1)]

(*
  Big maps have three possible representations. Either as a list of key-value
  pairs, as an identifier (int), or as a pair of identifier and overrides.
  Example values:

    1) { Elt "bar" True ; Elt "foo" False }
    2)  42
    3) Pair 42 { Elt "foo" (Some False) }
  *)

(** Test tickets from empty big_map when passed by reference. *)
let test_tickets_in_empty_big_map_ref () =
  assert_big_map_int_ticket_string_ref
    ~loc:__LOC__
    ~pre_populated:[]
    ~big_map_exp:(Printf.sprintf "%s")
    []

(** Test tickets from non-empty big-map when passed by reference.
    Here, tickets are scanned from the context. *)
let test_tickets_in_non_empty_big_map_ref () =
  assert_big_map_int_ticket_string_ref
    ~loc:__LOC__
    ~pre_populated:
      [
        (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|});
        (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1|});
        (3, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1|});
      ]
    ~big_map_exp:(Printf.sprintf "%s")
    [
      ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red", 1);
      ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green", 1);
      ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue", 1);
    ]

(** Test tickets from empty big-map when passed as a pair of identifier
    and overrides. Here, the scanned tickets are only contained in the overlay
    why ticket-scanning fails.

    If we extend the ticket scanner function to support non-empty overlays
    this test needs to be adapted.
    *)
let test_tickets_overlay_in_empty_big_map_ref () =
  assert_fail_non_empty_overlay_with_big_map_ref
    ~loc:__LOC__
    ~pre_populated:[]
    ~big_map_exp:
      (Printf.sprintf
         {|Pair %s { Elt 1 (Some (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1))}|})

(** Test tickets from non-empty big-map when passed as a pair of identifier
    and overrides. The scanned tickets are contained in the context as well as
    in the overlay. Since overlay is non-empty is non-empty, ticket scanning
    fails.

    If we extend the ticket scanner function to support non-empty overlays
    this test needs to be adapted
    *)
let test_tickets_overlay_in_non_empty_in_big_map_ref () =
  assert_fail_non_empty_overlay_with_big_map_ref
    ~loc:__LOC__
    ~pre_populated:
      [
        (1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|});
        (2, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1|});
        (3, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1|});
      ]
    ~big_map_exp:
      (Printf.sprintf
         {| Pair
            %s
            { Elt 4 (Some (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "orange" 1))}
       |})

(** Test tickets from non-empty big-map when passed as a pair of identifier
    and overrides, and where the override replaces an existing ticket.
    Ticket scanning fails due to non-empty overlay.

    If we extend the ticket scanner function to support non-empty overlays
    this test needs to be adapted.
    *)
let test_tickets_replace_overlay_in_non_empty_in_big_map_ref () =
  assert_fail_non_empty_overlay_with_big_map_ref
    ~loc:__LOC__
    ~pre_populated:
      [(1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|})]
    ~big_map_exp:
      (Printf.sprintf
         {| Pair
            %s
            { Elt 1 (Some (Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1))}
       |})

(** Test tickets from non-empty big-map when passed as a pair of identifier
    and overrides, and where the override removes an existing ticket.
    Ticket scanning fails due to non-empty overlay.

    If we extend the ticket scanner function to support non-empty overlays
    this test needs to be adapted.
    *)
let test_tickets_remove_overlay_in_non_empty_in_big_map_ref () =
  assert_fail_non_empty_overlay_with_big_map_ref
    ~loc:__LOC__
    ~pre_populated:
      [(1, {|Pair "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1|})]
    ~big_map_exp:(Printf.sprintf {| Pair %s { Elt 1 None} |})

let tests =
  [
    Tztest.tztest
      "Test tickets in unit ticket"
      `Quick
      test_tickets_in_unit_ticket;
    Tztest.tztest "Test tickets in list" `Quick test_tickets_in_list;
    Tztest.tztest
      "Test tickets in list with zero amount"
      `Quick
      test_tickets_in_list_with_zero_amount;
    Tztest.tztest "Test tickets in pair" `Quick test_tickets_in_pair;
    Tztest.tztest
      "Test tickets in pair with zero amount"
      `Quick
      test_tickets_in_pair_with_zero_amount;
    Tztest.tztest "Test tickets in map" `Quick test_tickets_in_map;
    Tztest.tztest
      "Test tickets in map with zero amount"
      `Quick
      test_tickets_in_map_with_zero_amount;
    Tztest.tztest "Test tickets in big map" `Quick test_tickets_in_big_map;
    Tztest.tztest
      "Test tickets in big map with include lazy set to false"
      `Quick
      test_tickets_in_big_map_strict_only;
    Tztest.tztest
      "Test tickets in list in big map"
      `Quick
      test_tickets_in_list_in_big_map;
    Tztest.tztest
      "Test tickets in a pair of big-map and list with include lazy set to \
       false"
      `Quick
      test_tickets_in_pair_big_map_and_list_strict_only;
    Tztest.tztest "Test tickets in or left" `Quick test_tickets_in_or_left;
    Tztest.tztest
      "Test tickets in or left with zero amount"
      `Quick
      test_tickets_in_or_left_with_zero_amount;
    Tztest.tztest "Test tickets in or right" `Quick test_tickets_in_or_right;
    Tztest.tztest
      "Test tickets in empty big-map ref"
      `Quick
      test_tickets_overlay_in_empty_big_map_ref;
    Tztest.tztest
      "Test tickets in big-map ref"
      `Quick
      test_tickets_in_empty_big_map_ref;
    Tztest.tztest
      "Test tickets in non-empty big-map ref"
      `Quick
      test_tickets_in_non_empty_big_map_ref;
    Tztest.tztest
      "Test tickets in non-empty big-map ref with overlay"
      `Quick
      test_tickets_overlay_in_non_empty_in_big_map_ref;
    Tztest.tztest
      "Test tickets replace existing value from overlay"
      `Quick
      test_tickets_replace_overlay_in_non_empty_in_big_map_ref;
    Tztest.tztest
      "Test tickets remove existing value from overlay"
      `Quick
      test_tickets_remove_overlay_in_non_empty_in_big_map_ref;
  ]
