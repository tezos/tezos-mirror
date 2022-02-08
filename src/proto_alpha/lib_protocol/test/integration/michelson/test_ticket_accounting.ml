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
    Component:  Protocol (Ticket_scanner)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                -- test "^ticket accounting"
    Subject:    Ticket scanner tests
*)

open Protocol
open Alpha_context
open Script_typed_ir

let wrap m = m >|= Environment.wrap_tzresult

let assert_equal_string_list ~loc msg =
  Assert.assert_equal_list ~loc String.equal msg Format.pp_print_string

let assert_fail_with ~loc ~msg f =
  let open Lwt_syntax in
  let* res = wrap @@ f () in
  match res with
  | Error [x] ->
      let x = Format.asprintf "%a" Error_monad.pp x in
      Assert.equal ~loc String.equal "" Format.pp_print_string msg x
  | Ok _ -> failwith "Expected an error at %s, but got `Ok'." loc
  | Error _ -> failwith "Expected a single error at %s." loc

let string_list_of_ex_token_diffs ctxt token_diffs =
  let open Lwt_tzresult_syntax in
  let accum (xs, ctxt)
      (Ticket_token.Ex_token {ticketer; contents_type; contents}, amount) =
    let* (x, ctxt) =
      wrap
      @@ Script_ir_translator.unparse_comparable_data
           ~loc:()
           ctxt
           Script_ir_translator.Readable
           contents_type
           contents
    in
    let str =
      Format.asprintf
        {|{ticketer: "%a"; contents: %a; amount: %a}|}
        Contract.pp
        ticketer
        Michelson_v1_printer.print_expr
        (Micheline.strip_locations x)
        Z.pp_print
        amount
    in
    return (str :: xs, ctxt)
  in
  let* (xs, ctxt) = List.fold_left_es accum ([], ctxt) token_diffs in
  return (List.rev xs, ctxt)

let make_ex_token ctxt ~ticketer ~type_exp ~content_exp =
  let open Lwt_tzresult_syntax in
  wrap
  @@ let*? (Script_ir_translator.Ex_comparable_ty contents_type, ctxt) =
       let node = Micheline.root @@ Expr.from_string type_exp in
       Script_ir_translator.parse_comparable_ty ctxt node
     in
     let*? ticketer = Contract.of_b58check ticketer in
     let* (contents, ctxt) =
       let node = Micheline.root @@ Expr.from_string content_exp in
       Script_ir_translator.parse_comparable_data ctxt contents_type node
     in
     return (Ticket_token.Ex_token {ticketer; contents_type; contents}, ctxt)

let assert_equal_ticket_diffs ~loc ctxt given expected =
  let open Lwt_tzresult_syntax in
  let* (ctxt, tbs1) =
    List.fold_left_map_es
      (fun ctxt ((ticketer, content), delta) ->
        make_ex_token
          ctxt
          ~ticketer
          ~type_exp:"string"
          ~content_exp:(Printf.sprintf "%S" content)
        >|=? fun (token, ctxt) -> (ctxt, (token, Z.of_int delta)))
      ctxt
      expected
  in
  let* (tbs1, ctxt) = string_list_of_ex_token_diffs ctxt tbs1 in
  let* (tbs2, _ctxt) = string_list_of_ex_token_diffs ctxt given in
  assert_equal_string_list
    ~loc
    "Compare token balances"
    (List.sort String.compare tbs1)
    (List.sort String.compare tbs2)

let updates_of_key_values ctxt ~key_type ~value_type key_values =
  let open Lwt_tzresult_syntax in
  List.fold_right_es
    (fun (key, value) (kvs, ctxt) ->
      let* (key_hash, ctxt) =
        wrap (Script_ir_translator.hash_comparable_data ctxt key_type key)
      in
      let* (key_node, ctxt) =
        wrap
          (Script_ir_translator.unparse_comparable_data
             ~loc:Micheline.dummy_location
             ctxt
             Script_ir_translator.Readable
             key_type
             key)
      in
      let* (value, ctxt) =
        match value with
        | None -> return (None, ctxt)
        | Some value ->
            let* (value_node, ctxt) =
              wrap
                (Script_ir_translator.unparse_data
                   ctxt
                   Script_ir_translator.Readable
                   value_type
                   value)
            in
            let value = Micheline.strip_locations value_node in
            return (Some value, ctxt)
      in
      let key = Micheline.strip_locations key_node in
      return ({Big_map.key; key_hash; value} :: kvs, ctxt))
    key_values
    ([], ctxt)

let make_alloc big_map_id alloc updates =
  Lazy_storage.make
    Lazy_storage.Kind.Big_map
    big_map_id
    (Update {init = Lazy_storage.Alloc alloc; updates})

let init () =
  let open Lwt_tzresult_syntax in
  let* (block, contracts) = Context.init 1 in
  let source = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
  let* (operation, originated) =
    Op.contract_origination (B block) source ~script:Op.dummy_script
  in
  let* block = Block.bake ~operation block in
  let* inc = Incremental.begin_construction block in
  return (originated, Incremental.alpha_ctxt inc)

(** Initializes one address for operations and one baker. *)
let init_for_operation () =
  Context.init ~consensus_threshold:0 2 >|=? fun (block, contracts) ->
  let (src0, src1) =
    match contracts with src0 :: src1 :: _ -> (src0, src1) | _ -> assert false
  in
  let baker =
    match Alpha_context.Contract.is_implicit src0 with
    | Some v -> v
    | None -> assert false
  in
  (baker, src1, block)

let two_ticketers block =
  let open Lwt_tzresult_syntax in
  let* ctxt =
    Incremental.begin_construction block >|=? Incremental.alpha_ctxt
  in
  let*! cs = Contract.list ctxt in
  match cs with c1 :: c2 :: _ -> return (c1, c2) | _ -> assert false

let ticket_list_script =
  {|
        { parameter (list (ticket string));
          storage (list (ticket string));
          code { CAR; NIL operation ; PAIR } }
  |}

let setup ctxt ~key_type ~value_type entries =
  let open Lwt_tzresult_syntax in
  let* (ctxt, big_map_id) = wrap @@ Big_map.fresh ~temporary:false ctxt in
  let* (updates, ctxt) =
    updates_of_key_values
      ctxt
      ~key_type
      ~value_type
      (List.map (fun (k, v) -> (k, Some v)) entries)
  in
  let*? (key_type_node, ctxt) =
    Environment.wrap_tzresult
    @@ Script_ir_translator.unparse_comparable_ty
         ~loc:Micheline.dummy_location
         ctxt
         key_type
  in
  let*? (value_type_node, ctxt) =
    Environment.wrap_tzresult
    @@ Script_ir_translator.unparse_ty
         ~loc:Micheline.dummy_location
         ctxt
         value_type
  in
  let key_type = Micheline.strip_locations key_type_node in
  let value_type = Micheline.strip_locations value_type_node in
  let alloc = make_alloc big_map_id Big_map.{key_type; value_type} updates in
  return (alloc, big_map_id, ctxt)

let new_big_map ctxt contract ~key_type ~value_type entries =
  let open Lwt_tzresult_syntax in
  let* (alloc, big_map_id, ctxt) = setup ctxt ~key_type ~value_type entries in
  let storage = Expr.from_string "{}" in
  let* ctxt =
    wrap @@ Contract.update_script_storage ctxt contract storage (Some [alloc])
  in
  return (big_map_id, ctxt)

let alloc_diff ctxt ~key_type ~value_type entries =
  let open Lwt_tzresult_syntax in
  let* (allocations, _, ctxt) = setup ctxt ~key_type ~value_type entries in
  return (allocations, ctxt)

let remove_diff ctxt contract ~key_type ~value_type ~existing_entries =
  let open Lwt_tzresult_syntax in
  let* (big_map_id, ctxt) =
    new_big_map ctxt contract ~key_type ~value_type existing_entries
  in
  return (Lazy_storage.make Lazy_storage.Kind.Big_map big_map_id Remove, ctxt)

let copy_diff ctxt contract ~key_type ~value_type ~existing_entries ~updates =
  let open Lwt_tzresult_syntax in
  let* (big_map_id, ctxt) =
    new_big_map ctxt contract ~key_type ~value_type existing_entries
  in
  let* (updates, ctxt) =
    updates_of_key_values ctxt ~key_type ~value_type updates
  in
  let* (ctxt, new_big_map_id) = wrap @@ Big_map.fresh ctxt ~temporary:false in
  return
    ( Lazy_storage.make
        Lazy_storage.Kind.Big_map
        new_big_map_id
        (Update {init = Lazy_storage.Copy {src = big_map_id}; updates}),
      ctxt )

let existing_diff ctxt contract ~key_type ~value_type ~existing_entries ~updates
    =
  let open Lwt_tzresult_syntax in
  let* (big_map_id, ctxt) =
    new_big_map ctxt contract ~key_type ~value_type existing_entries
  in
  let* (updates, ctxt) =
    updates_of_key_values ctxt ~key_type ~value_type updates
  in
  return
    ( Lazy_storage.make
        Lazy_storage.Kind.Big_map
        big_map_id
        (Update {init = Lazy_storage.Existing; updates}),
      ctxt )

let empty_big_map ctxt ~key_type ~value_type =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (ctxt, big_map_id) = wrap @@ Big_map.fresh ~temporary:false ctxt in
  return
    ( Big_map
        {
          id = Some big_map_id;
          diff = {map = Big_map_overlay.empty; size = 0};
          key_type;
          value_type;
        },
      ctxt )

let make_big_map ctxt contract ~key_type ~value_type entries =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (big_map_id, ctxt) =
    new_big_map ctxt contract ~key_type ~value_type entries
  in
  return
    ( Big_map
        {
          id = Some big_map_id;
          diff = {map = Big_map_overlay.empty; size = 0};
          key_type;
          value_type;
        },
      ctxt )

let originate_script block ~script ~storage ~src ~baker ~forges_tickets =
  let open Lwt_tzresult_syntax in
  let code = Expr.toplevel_from_string script in
  let storage = Expr.from_string storage in
  let script =
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}
  in
  let* (operation, destination) =
    Op.contract_origination (B block) src ~fee:(Test_tez.of_int 10) ~script
  in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  let* incr =
    Incremental.add_operation
      ?expect_failure:
        (if forges_tickets then Some (fun _ -> return ()) else None)
      incr
      operation
  in
  Incremental.finalize_block incr >|=? fun block -> (destination, script, block)

let origination_operation ctxt ~src ~script ~orig_contract =
  let open Lwt_tzresult_syntax in
  let* ( Script_ir_translator.Ex_script
           (Script
             {
               storage_type;
               storage;
               code = _;
               arg_type = _;
               views = _;
               entrypoints = _;
               code_size = _;
             }),
         ctxt ) =
    wrap
    @@ Script_ir_translator.parse_script
         ctxt
         ~legacy:true
         ~allow_forged_in_storage:true
         script
  in
  let operation =
    Internal_operation
      {
        source = src;
        operation =
          Origination
            {
              origination = {delegate = None; script; credit = Tez.one};
              preorigination = orig_contract;
              storage_type;
              storage;
            };
        nonce = 1;
      }
  in
  return (operation, ctxt)

let originate block ~src ~baker ~script ~storage ~forges_tickets =
  let open Lwt_tzresult_syntax in
  let* (orig_contract, script, block) =
    originate_script block ~script ~storage ~src ~baker ~forges_tickets
  in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  return (orig_contract, script, incr)

let transfer_operation ctxt ~src ~destination ~arg_type ~arg =
  let open Lwt_tzresult_syntax in
  let* (params_node, ctxt) =
    wrap
      (Script_ir_translator.unparse_data
         ctxt
         Script_ir_translator.Readable
         arg_type
         arg)
  in
  return
    ( Internal_operation
        {
          source = src;
          operation =
            Transaction
              {
                transaction =
                  {
                    amount = Tez.zero;
                    parameters =
                      Script.lazy_expr @@ Micheline.strip_locations params_node;
                    entrypoint = Entrypoint.default;
                    destination = Destination.Contract destination;
                  };
                location = Micheline.dummy_location;
                parameters_ty = arg_type;
                parameters = arg;
              };
          nonce = 1;
        },
      ctxt )

let ticket_string_type =
  WithExceptions.Result.get_ok ~loc:__LOC__
  @@ Script_typed_ir.(ticket_t (-1) string_key)

let ticket_string_list_type =
  Result.value_f ~default:(fun _ -> assert false)
  @@ Script_typed_ir.list_t (-1) ticket_string_type

let boxed_list elements =
  {Script_typed_ir.elements; length = List.length elements}

let big_map_type ~key_type ~value_type =
  Environment.wrap_tzresult
  @@ Script_typed_ir.big_map_t (-1) key_type value_type

let type_has_tickets ctxt ty =
  Environment.wrap_tzresult @@ Ticket_scanner.type_has_tickets ctxt ty

(** Test that adding a ticket to a lazy storage diff is picked up. *)
let assert_ticket_diffs ctxt ~loc ~arg_type ~storage_type ~arg ~old_storage
    ~new_storage ~lazy_storage_diff expected =
  let open Lwt_tzresult_syntax in
  let*? (arg_type_has_tickets, ctxt) = type_has_tickets ctxt arg_type in
  let*? (storage_type_has_tickets, ctxt) = type_has_tickets ctxt storage_type in
  let* (ticket_diff, ctxt) =
    wrap
      (Ticket_accounting.ticket_diffs
         ctxt
         ~arg_type_has_tickets
         ~storage_type_has_tickets
         ~arg
         ~old_storage
         ~new_storage
         ~lazy_storage_diff)
  in
  let*? (ticket_diffs, ctxt) =
    Environment.wrap_tzresult @@ Ticket_token_map.to_list ctxt ticket_diff
  in
  assert_equal_ticket_diffs ~loc ctxt ticket_diffs expected

let assert_balance ctxt ~loc key expected =
  let open Lwt_tzresult_syntax in
  let* (balance, _) = wrap @@ Ticket_balance.get_balance ctxt key in
  match (balance, expected) with
  | (Some b, Some eb) -> Assert.equal_int ~loc (Z.to_int b) eb
  | (None, Some eb) -> failwith "Expected balance %d" eb
  | (Some eb, None) -> failwith "Expected None but got %d" (Z.to_int eb)
  | (None, None) -> return ()

let string_ticket ticketer contents amount =
  let amount = Script_int.abs @@ Script_int.of_int amount in
  let ticketer =
    Result.value_f ~default:(fun _ -> assert false)
    @@ Contract.of_b58check ticketer
  in
  let contents =
    Result.value_f ~default:(fun _ -> assert false)
    @@ Script_string_repr.of_string contents
  in
  Script_typed_ir.{ticketer; contents; amount}

let string_ticket_token ticketer content =
  let open Lwt_tzresult_syntax in
  let contents =
    Result.value_f ~default:(fun _ -> assert false)
    @@ Alpha_context.Script_string.of_string content
  in
  let*? ticketer = Environment.wrap_tzresult @@ Contract.of_b58check ticketer in
  return
    (Ticket_token.Ex_token
       {ticketer; contents_type = Script_typed_ir.string_key; contents})

let test_diffs_empty () =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (_contract, ctxt) = init () in
  let*? int_ticket_big_map_ty =
    big_map_type ~key_type:int_key ~value_type:ticket_string_type
  in
  (* Start with an empty big-map *)
  let* (empty_big_map, ctxt) =
    empty_big_map ctxt ~key_type:int_key ~value_type:ticket_string_type
  in
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:unit_t
    ~storage_type:int_ticket_big_map_ty
    ~arg:()
    ~old_storage:empty_big_map
    ~new_storage:empty_big_map
    ~lazy_storage_diff:[]
    []

(** Test that sending one ticket as an argument, when the new storage is empty
      results in an negative diff. *)
let test_diffs_tickets_in_args () =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (_contract, ctxt) = init () in
  let arg = string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 in
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:ticket_string_type
    ~storage_type:unit_t
    ~arg
    ~old_storage:()
    ~new_storage:()
    ~lazy_storage_diff:[]
    [(("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1)]

(** Test adding a ticket to the args, which is also accounted for in the new
      storage, results in an empty diff. *)
let test_diffs_tickets_in_args_and_storage () =
  let open Lwt_tzresult_syntax in
  let* (_contract, ctxt) = init () in
  let arg = string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 in
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:ticket_string_type
    ~storage_type:ticket_string_list_type
    ~arg
    ~old_storage:(boxed_list [])
    ~new_storage:(boxed_list [arg])
    ~lazy_storage_diff:[]
    [(("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 0)]

(** Test that adding two tickets in the args, and only one new ticket in the
      storage results in a negative diff. *)
let test_diffs_drop_one_ticket () =
  let open Lwt_tzresult_syntax in
  let* (_contract, ctxt) = init () in
  let arg =
    boxed_list
      [
        string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1;
        string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1;
      ]
  in
  let new_storage =
    boxed_list [string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1]
  in
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:ticket_string_list_type
    ~storage_type:ticket_string_list_type
    ~arg
    ~old_storage:(boxed_list [])
    ~new_storage
    ~lazy_storage_diff:[]
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 0);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), -1);
    ]

(** Test that adding a new ticket to the storage results in a positive
    balance. *)
let test_diffs_adding_new_ticket_to_storage () =
  let open Lwt_tzresult_syntax in
  let* (_contract, ctxt) = init () in
  let new_storage =
    boxed_list [string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1]
  in
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:Script_typed_ir.unit_t
    ~storage_type:ticket_string_list_type
    ~arg:()
    ~old_storage:(boxed_list [])
    ~new_storage
    ~lazy_storage_diff:[]
    [(("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 1)]

(** Test that removing one ticket from the storage results in a negative
    diff. *)
let test_diffs_remove_from_storage () =
  let open Lwt_tzresult_syntax in
  let* (_contract, ctxt) = init () in
  let old_storage =
    boxed_list
      [
        string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1;
        string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 2;
      ]
  in
  let new_storage =
    boxed_list [string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1]
  in
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:Script_typed_ir.unit_t
    ~storage_type:ticket_string_list_type
    ~arg:()
    ~old_storage
    ~new_storage
    ~lazy_storage_diff:[]
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 0);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), -2);
    ]

(* Test adding ticket through lazy-storage diff results in a positive diff. *)
let test_diffs_lazy_storage_alloc () =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (_contract, ctxt) = init () in
  let*? int_ticket_big_map_ty =
    big_map_type ~key_type:int_key ~value_type:ticket_string_type
  in
  (* Start with an empty big-map *)
  let* (empty_big_map, ctxt) =
    empty_big_map ctxt ~key_type:int_key ~value_type:ticket_string_type
  in
  (* We add one ticket to the storage. *)
  let* (lazy_storage_diff, ctxt) =
    alloc_diff
      ctxt
      ~key_type:int_key
      ~value_type:ticket_string_type
      [
        ( Script_int.of_int 1,
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 );
      ]
  in
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:int_ticket_big_map_ty
    ~storage_type:int_ticket_big_map_ty
    ~arg:empty_big_map
    ~old_storage:empty_big_map
    ~new_storage:empty_big_map
    ~lazy_storage_diff:[lazy_storage_diff]
    [(("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 1)]

(* Test removing a big map containing a ticket results in a negative diff. *)
let test_diffs_remove_from_big_map () =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (contract, ctxt) = init () in
  let*? int_ticket_big_map_ty =
    big_map_type ~key_type:int_key ~value_type:ticket_string_type
  in
  (* Start with an empty big-map *)
  let* (empty_big_map, ctxt) =
    empty_big_map ctxt ~key_type:int_key ~value_type:ticket_string_type
  in
  (* Remove one ticket from the lazy storage. *)
  let* (lazy_storage_diff, ctxt) =
    remove_diff
      ctxt
      contract
      ~key_type:int_key
      ~value_type:ticket_string_type
      ~existing_entries:
        [
          ( Script_int.of_int 1,
            string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 );
        ]
  in
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:unit_t
    ~storage_type:int_ticket_big_map_ty
    ~arg:()
    ~old_storage:empty_big_map
    ~new_storage:empty_big_map
    ~lazy_storage_diff:[lazy_storage_diff]
    [(("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1)]

(** Test copying a big-map. *)
let test_diffs_copy_big_map () =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (contract, ctxt) = init () in
  let*? int_ticket_big_map_ty =
    big_map_type ~key_type:int_key ~value_type:ticket_string_type
  in
  (* Start with an empty big-map *)
  let* (empty_big_map, ctxt) =
    empty_big_map ctxt ~key_type:int_key ~value_type:ticket_string_type
  in
  (* We add one ticket to the storage. *)
  let* (lazy_storage_diff, ctxt) =
    copy_diff
      ctxt
      contract
      ~key_type:int_key
      ~value_type:ticket_string_type
      ~existing_entries:
        [
          ( Script_int.of_int 1,
            string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 );
        ]
      ~updates:
        [
          ( Script_int.of_int 2,
            Some (string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1)
          );
        ]
  in
  (* We copy a big-map with one ticket inside (this is illegal in Michelson).
     Then we add a new ticket to the map.  The result is two new tickets.
  *)
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:unit_t
    ~storage_type:int_ticket_big_map_ty
    ~arg:()
    ~old_storage:empty_big_map
    ~new_storage:empty_big_map
    ~lazy_storage_diff:[lazy_storage_diff]
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), 1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), 1);
    ]

(** Test that adding and removing items from an existing big-map results
      yield corresponding ticket-token diffs. *)
let test_diffs_add_to_existing_big_map () =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (contract, ctxt) = init () in
  let*? int_ticket_big_map_ty =
    big_map_type ~key_type:int_key ~value_type:ticket_string_type
  in
  let* (old_storage, ctxt) =
    make_big_map
      ctxt
      contract
      ~key_type:int_key
      ~value_type:ticket_string_type
      [
        (* It doesn't matter what the old entries are. They are never traversed *)
        ( Script_int.of_int 1,
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 );
        ( Script_int.of_int 2,
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1 );
        ( Script_int.of_int 3,
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1 );
      ]
  in
  (* We add one ticket to the storage. *)
  let* (lazy_storage_diff, ctxt) =
    existing_diff
      ctxt
      contract
      ~key_type:int_key
      ~value_type:ticket_string_type
      ~existing_entries:
        [
          ( Script_int.of_int 1,
            string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 );
        ]
      ~updates:
        [
          (* Add one new ticket to the big-map. *)
          ( Script_int.of_int 2,
            Some (string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 2)
          );
          (* Remove a ticket *)
          (Script_int.of_int 1, None);
        ]
  in
  (* Even if the old and the new storage are the same (and contains tickets)
     we should still detect the diff from the lazy-storage diff.
     Since the old and new storage are lazy, they should never be traversed.
  *)
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:unit_t
    ~storage_type:int_ticket_big_map_ty
    ~arg:()
    ~old_storage
    ~new_storage:old_storage
    ~lazy_storage_diff:[lazy_storage_diff]
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), 2);
    ]

(** Test a combination of updates. *)
let test_diffs_args_storage_and_lazy_diffs () =
  let open Lwt_tzresult_syntax in
  let open Script_typed_ir in
  let* (contract, ctxt) = init () in
  let*? int_ticket_big_map_ty =
    big_map_type ~key_type:int_key ~value_type:ticket_string_type
  in
  let*? (Ty_ex_c list_big_map_pair_type) =
    Environment.wrap_tzresult
    @@ pair_t (-1) ticket_string_list_type int_ticket_big_map_ty
  in
  let* (empty_big_map, ctxt) =
    empty_big_map ctxt ~key_type:int_key ~value_type:ticket_string_type
  in
  (* We send two tickets in the args. *)
  let arg =
    boxed_list
      [
        string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1;
        string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1;
      ]
  in
  (* We add three tickets to the storage. *)
  let* (lazy_storage_diff, ctxt) =
    existing_diff
      ctxt
      contract
      ~key_type:int_key
      ~value_type:ticket_string_type
      ~existing_entries:[]
      ~updates:
        [
          ( Script_int.of_int 1,
            Some (string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1)
          );
          ( Script_int.of_int 2,
            Some (string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1)
          );
          ( Script_int.of_int 3,
            Some
              (string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1)
          );
        ]
  in
  (* We have three tickets in the old storage. *)
  let old_storage =
    ( boxed_list
        [
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1;
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "blue" 1;
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1;
        ],
      empty_big_map )
  in
  let new_storage =
    ( boxed_list
        [
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "green" 1;
          string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "yellow" 1;
        ],
      empty_big_map )
  in
  (*
      Before script execution:
       - Args: 1 red, 1 blue
       - Old storage : 1 red, 1 blue, 1 green
       - Total: 2 red, 2 blue, 1 green
      After execution:
       - New_storage: 1 green, 1 yellow
       - Lazy-diff: 1 red, 1 blue, 1 green
       - Total: 1 red, 1 blue, 2 green, 1 yellow
      Net diff:
        - -1 red, -1 blue, +1 green, +1 yellow
    *)
  assert_ticket_diffs
    ctxt
    ~loc:__LOC__
    ~arg_type:ticket_string_list_type
    ~storage_type:list_big_map_pair_type
    ~arg
    ~old_storage
    ~new_storage
    ~lazy_storage_diff:[lazy_storage_diff]
    [
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "red"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "blue"), -1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "green"), 1);
      (("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq", "yellow"), 1);
    ]

(** Test that attempting to transfer a ticket that exceeds the budget fails. *)
let test_update_invalid_transfer () =
  let open Lwt_tzresult_syntax in
  let* (baker, src, block) = init_for_operation () in
  let* (destination, _script, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let ctxt = Incremental.alpha_ctxt incr in
  let arg_type = ticket_string_list_type in
  let arg =
    boxed_list [string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1]
  in
  let* (operation, ctxt) =
    transfer_operation ctxt ~src ~destination ~arg_type ~arg
  in
  assert_fail_with
    ~loc:__LOC__
    ~msg:
      "Attempted to send 1 unit(s) of a ticket created by \
       KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq."
    (fun () ->
      Ticket_accounting.update_ticket_balances
        ctxt
        ~self:src
        ~ticket_diffs:Ticket_token_map.empty
        [operation])

(** Test that adding more tickets created by the [self] contract is valid and
    results in a balance update. *)
let test_update_ticket_self_diff () =
  let open Lwt_tzresult_syntax in
  let* (baker, src, block) = init_for_operation () in
  let* (self, _script, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let ticketer = Contract.to_b58check self in
  let ctxt = Incremental.alpha_ctxt incr in
  let* red_token = string_ticket_token ticketer "red" in
  let* (ticket_diffs, ctxt) =
    wrap
      (Ticket_token_map.of_list
         ctxt
         ~merge_overlap:(fun _ -> assert false)
         [(red_token, Z.of_int 10)])
  in
  let* (_, ctxt) =
    wrap (Ticket_accounting.update_ticket_balances ctxt ~self ~ticket_diffs [])
  in
  (* After update, we should have 10 added red tokens. *)
  let* (red_self_token_hash, ctxt) =
    wrap
    @@ Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Contract self)
         red_token
  in
  assert_balance ~loc:__LOC__ ctxt red_self_token_hash (Some 10)

(* Test that sending tickets to self succeed (there are no budget constraints). *)
let test_update_self_ticket_transfer () =
  let open Lwt_tzresult_syntax in
  let* (baker, self, block) = init_for_operation () in
  let* (ticket_receiver, _script, incr) =
    originate
      block
      ~src:self
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  (* Ticket is self. That means we can transfer an unlimited amounts of such
     ticket-tokens. *)
  let ticketer = Contract.to_b58check self in
  let ctxt = Incremental.alpha_ctxt incr in
  let* red_token = string_ticket_token ticketer "red" in
  let* (operation, ctxt) =
    let arg_type = ticket_string_list_type in
    let arg =
      boxed_list
        [
          (* Send a total of 10 units of ticket-tokens. *)
          string_ticket ticketer "red" 1;
          string_ticket ticketer "red" 2;
          string_ticket ticketer "red" 3;
          string_ticket ticketer "red" 4;
        ]
    in
    transfer_operation
      ctxt
      ~src:self
      ~destination:ticket_receiver
      ~arg_type
      ~arg
  in
  let* (_, ctxt) =
    wrap
      (Ticket_accounting.update_ticket_balances
         ctxt
         ~self
         ~ticket_diffs:Ticket_token_map.empty
         [operation])
  in
  (* Once we're done with the update, we expect ticket-receiver to have been
     credited with 10 units of ticket-tokens. *)
  let* () =
    let* (red_receiver_token_hash, ctxt) =
      wrap
      @@ Ticket_balance_key.of_ex_token
           ctxt
           ~owner:(Destination.Contract ticket_receiver)
           red_token
    in
    assert_balance ~loc:__LOC__ ctxt red_receiver_token_hash (Some 10)
  in
  return ()

(** Test that transferring a ticket that does not exceed the budget succeeds. *)
let test_update_valid_transfer () =
  let open Lwt_tzresult_syntax in
  let* (baker, self, block) = init_for_operation () in
  let* (destination, _script, incr) =
    originate
      block
      ~src:self
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let ticketer = "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" in
  assert (ticketer <> Contract.to_b58check self) ;
  let ctxt = Incremental.alpha_ctxt incr in
  let* red_token = string_ticket_token ticketer "red" in
  let* (red_self_token_hash, ctxt) =
    wrap
    @@ Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Contract self)
         red_token
  in
  let* (red_receiver_token_hash, ctxt) =
    wrap
    @@ Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Contract destination)
         red_token
  in
  (* Set up the balance so that the self contract owns one ticket. *)
  let* (_, ctxt) =
    wrap @@ Ticket_balance.adjust_balance ctxt red_self_token_hash ~delta:Z.one
  in
  let* (operation, ctxt) =
    let arg_type = ticket_string_list_type in
    let arg = boxed_list [string_ticket ticketer "red" 1] in
    transfer_operation ctxt ~src:self ~destination ~arg_type ~arg
  in
  let* (_, ctxt) =
    let* (ticket_diffs, ctxt) =
      wrap
        (Ticket_token_map.of_list
           ctxt
           ~merge_overlap:(fun _ -> assert false)
           [(red_token, Z.of_int (-1))])
    in
    wrap
      (Ticket_accounting.update_ticket_balances
         ctxt
         ~self
         ~ticket_diffs
         [operation])
  in
  (* Once we're done with the update, we expect the balance to have been moved
     from [self] to [destination]. *)
  let* () = assert_balance ~loc:__LOC__ ctxt red_self_token_hash None in
  let* () = assert_balance ~loc:__LOC__ ctxt red_receiver_token_hash (Some 1) in
  return ()

(** Test that transferring a ticket to itself is allowed and does not impact
    the balance. *)
let test_update_transfer_tickets_to_self () =
  let open Lwt_tzresult_syntax in
  let* (baker, src, block) = init_for_operation () in
  let* (self, _script, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let ticketer = "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" in
  assert (ticketer <> Contract.to_b58check self) ;
  let ctxt = Incremental.alpha_ctxt incr in
  let* red_token = string_ticket_token ticketer "red" in
  let* (red_self_token_hash, ctxt) =
    wrap
    @@ Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Contract self)
         red_token
  in
  (* Set up the balance so that the self contract owns ten tickets. *)
  let* (_, ctxt) =
    wrap
    @@ Ticket_balance.adjust_balance
         ctxt
         red_self_token_hash
         ~delta:(Z.of_int 10)
  in
  let* (operation, ctxt) =
    let arg_type = ticket_string_list_type in
    let arg = boxed_list [string_ticket ticketer "red" 1] in
    transfer_operation ctxt ~src:self ~destination:self ~arg_type ~arg
  in
  let* (_, ctxt) =
    (* Ticket diff removes 5 tickets. *)
    let* (ticket_diffs, ctxt) =
      wrap
        (Ticket_token_map.of_list
           ctxt
           ~merge_overlap:(fun _ -> assert false)
           [(red_token, Z.of_int (-5))])
    in
    wrap
      (Ticket_accounting.update_ticket_balances
         ctxt
         ~self
         ~ticket_diffs
         [operation])
  in
  (* We started with 10 units. Removed 5 from storage and sent one to [self].
     Therefore we expect 10 - 5 + 1 = 6 units remaining. *)
  let* () = assert_balance ~loc:__LOC__ ctxt red_self_token_hash (Some 6) in
  return ()

(** Test that attempting to originate a contract with tickets that exceed the
    budget fails. *)
let test_update_invalid_origination () =
  let open Lwt_tzresult_syntax in
  let* (baker, src, block) = init_for_operation () in
  let* (destination, script, incr) =
    let storage =
      let ticketer = "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" in
      Printf.sprintf
        {|{ Pair %S "red" 1; Pair %S "green" 1; Pair %S "blue" 1; } |}
        ticketer
        ticketer
        ticketer
    in
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let ctxt = Incremental.alpha_ctxt incr in
  let* (operation, ctxt) =
    origination_operation ctxt ~src ~orig_contract:destination ~script
  in
  assert_fail_with
    ~loc:__LOC__
    ~msg:
      "Attempted to send 1 unit(s) of a ticket created by \
       KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq."
    (fun () ->
      Ticket_accounting.update_ticket_balances
        ctxt
        ~self:src
        ~ticket_diffs:Ticket_token_map.empty
        [operation])

(** Test update valid origination. *)
let test_update_valid_origination () =
  let open Lwt_tzresult_syntax in
  let* (baker, self, block) = init_for_operation () in
  let ticketer = "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" in
  assert (ticketer <> Contract.to_b58check self) ;
  let* (originated, script, incr) =
    let storage = Printf.sprintf {|{ Pair %S "red" 1; }|} ticketer in
    originate
      block
      ~src:self
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let ctxt = Incremental.alpha_ctxt incr in
  let* red_token = string_ticket_token ticketer "red" in
  let* (red_self_token_hash, ctxt) =
    wrap
    @@ Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Contract self)
         red_token
  in
  (* Set up the balance so that the self contract owns one ticket. *)
  let* (_, ctxt) =
    wrap @@ Ticket_balance.adjust_balance ctxt red_self_token_hash ~delta:Z.one
  in
  let* (operation, ctxt) =
    origination_operation ctxt ~src:self ~orig_contract:originated ~script
  in
  let* (_, ctxt) =
    let* (ticket_diffs, ctxt) =
      wrap
        (Ticket_token_map.of_list
           ctxt
           ~merge_overlap:(fun _ -> assert false)
           [(red_token, Z.of_int (-1))])
    in
    wrap
      (Ticket_accounting.update_ticket_balances
         ctxt
         ~self
         ~ticket_diffs
         [operation])
  in
  (* Once we're done with the update, we expect the balance to have been moved
     from [self] to [destination]. *)
  let* (red_originated_token_hash, ctxt) =
    wrap
    @@ Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Contract originated)
         red_token
  in
  assert_balance ~loc:__LOC__ ctxt red_originated_token_hash (Some 1)

let test_update_self_origination () =
  let open Lwt_tzresult_syntax in
  let* (baker, self, block) = init_for_operation () in
  let ticketer = Contract.to_b58check self in
  let* (originated, script, incr) =
    let storage = Printf.sprintf {|{ Pair %S "red" 1; }|} ticketer in
    originate
      block
      ~src:self
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let ctxt = Incremental.alpha_ctxt incr in
  let* red_token = string_ticket_token ticketer "red" in
  let* (red_originated_token_hash, ctxt) =
    wrap
    @@ Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Contract originated)
         red_token
  in
  let* (operation, ctxt) =
    origination_operation ctxt ~src:self ~orig_contract:originated ~script
  in
  let* (_, ctxt) =
    wrap
      (Ticket_accounting.update_ticket_balances
         ctxt
         ~self
         ~ticket_diffs:Ticket_token_map.empty
         [operation])
  in
  (* Once we're done with the update, we expect the balance to have been
     credited to the originated contract. *)
  assert_balance ~loc:__LOC__ ctxt red_originated_token_hash (Some 1)

(** Test ticket-token map of list with duplicates.  *)
let test_ticket_token_map_of_list_with_duplicates () =
  let open Lwt_tzresult_syntax in
  let* (baker, src, block) = init_for_operation () in
  let* (self, _script, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let ticketer = Contract.to_b58check self in
  let ctxt = Incremental.alpha_ctxt incr in
  let* red_token = string_ticket_token ticketer "red" in
  let* (ticket_diffs, ctxt) =
    wrap
      (Ticket_token_map.of_list
         ctxt
         ~merge_overlap:(fun ctxt v1 v2 -> ok (Z.add v1 v2, ctxt))
         [(red_token, Z.of_int 10); (red_token, Z.of_int 5)])
  in
  let* (_, ctxt) =
    wrap (Ticket_accounting.update_ticket_balances ctxt ~self ~ticket_diffs [])
  in
  (* After update, we should have 10 + 5 added red tokens. *)
  let* (red_self_token_hash, ctxt) =
    wrap
    @@ Ticket_balance_key.of_ex_token
         ctxt
         ~owner:(Destination.Contract self)
         red_token
  in
  assert_balance ~loc:__LOC__ ctxt red_self_token_hash (Some 15)

let tests =
  [
    Tztest.tztest "Diffs empty" `Quick test_diffs_empty;
    Tztest.tztest "Diffs for tickets in args" `Quick test_diffs_tickets_in_args;
    Tztest.tztest
      "Diffs for tickets in args and storage"
      `Quick
      test_diffs_tickets_in_args_and_storage;
    Tztest.tztest "Diffs for dropped ticket" `Quick test_diffs_drop_one_ticket;
    Tztest.tztest
      "Diffs for adding new ticket to storage"
      `Quick
      test_diffs_adding_new_ticket_to_storage;
    Tztest.tztest
      "Diffs for removing from storage"
      `Quick
      test_diffs_remove_from_storage;
    Tztest.tztest
      "Diffs for lazy storage allocation"
      `Quick
      test_diffs_lazy_storage_alloc;
    Tztest.tztest
      "Diffs for removing from big-map"
      `Quick
      test_diffs_remove_from_big_map;
    Tztest.tztest "Diffs for copying a big-map" `Quick test_diffs_copy_big_map;
    Tztest.tztest
      "Diffs for adding to an existing big-map"
      `Quick
      test_diffs_add_to_existing_big_map;
    Tztest.tztest
      "Diffs for args, storage and lazy-diff"
      `Quick
      test_diffs_args_storage_and_lazy_diffs;
    Tztest.tztest
      "Update tickets balances with invalid transfer"
      `Quick
      test_update_invalid_transfer;
    Tztest.tztest "Update ticket self diff" `Quick test_update_ticket_self_diff;
    Tztest.tztest
      "Update ticket balances for valid transfer"
      `Quick
      test_update_valid_transfer;
    Tztest.tztest
      "Update ticket balances for transfer with 'self' tickets"
      `Quick
      test_update_self_ticket_transfer;
    Tztest.tztest
      "Update transfer tickets to self"
      `Quick
      test_update_transfer_tickets_to_self;
    Tztest.tztest
      "Update invalid origination"
      `Quick
      test_update_invalid_origination;
    Tztest.tztest
      "Update valid origination"
      `Quick
      test_update_valid_origination;
    Tztest.tztest
      "Update valid self origination"
      `Quick
      test_update_self_origination;
    Tztest.tztest
      "Test ticket-token map with duplicate keys"
      `Quick
      test_ticket_token_map_of_list_with_duplicates;
  ]
