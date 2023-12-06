(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    Component:    Protocol (type-checking)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_typechecking.ml
    Subject:      Type-checking
*)

open Protocol
open Alpha_context
open Micheline

let context_init_with_sc_rollup_arith_enabled tup =
  Context.init_with_constants_gen
    tup
    {
      Context.default_test_constants with
      consensus_threshold = 0;
      sc_rollup =
        {Context.default_test_constants.sc_rollup with arith_pvm_enable = true};
    }

let sc_originate block contract parameters_ty =
  let open Lwt_result_syntax in
  let kind = Sc_rollup.Kind.Example_arith in
  let* operation, rollup =
    Sc_rollup_helpers.origination_op ~parameters_ty (B block) contract kind
  in
  let* incr = Incremental.begin_construction block in
  let* incr = Incremental.add_operation incr operation in
  let* block = Incremental.finalize_block incr in
  return (block, rollup)

(* Test for Script_ir_translator.parse_and_unparse_script_unaccounted on a
   script declaring views. *)
let test_unparse_view () =
  let open Lwt_result_wrap_syntax in
  let dummy_contract =
    "{parameter unit; storage unit; code { CAR; NIL operation; PAIR }; view \
     \"v0\" unit unit { DROP; UNIT }; view \"v1\" nat nat {CAR}}"
  in
  let contract_expr = Expr.from_string dummy_contract in
  let storage_expr = Expr.from_string "Unit" in
  let bef = Script.lazy_expr contract_expr |> Data_encoding.force_bytes in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  let* b, _cs = Context.init3 () in
  let* v = Incremental.begin_construction b in
  let ctx = Incremental.alpha_ctxt v in
  let*@ unparsed_script, _ctx =
    Script_ir_translator.parse_and_unparse_script_unaccounted
      ctx
      ~legacy:true
      ~allow_forged_in_storage:false
      Readable
      ~normalize_types:true
      script
  in
  let aft = Data_encoding.force_bytes unparsed_script.code in
  Alcotest.(check bytes) "didn't match" bef aft |> return

let test_context () =
  let open Lwt_result_syntax in
  let* b, _cs = Context.init3 ~consensus_threshold:0 () in
  let* v = Incremental.begin_construction b in
  return (Incremental.alpha_ctxt v)

let test_context_with_nat_nat_big_map () =
  let open Lwt_result_wrap_syntax in
  let* b, source = Context.init1 () in
  let* operation, originated =
    Op.contract_origination_hash (B b) source ~script:Op.dummy_script
  in
  let* b = Block.bake ~operation b in
  let* v = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt v in
  let*@ ctxt, id = Big_map.fresh ~temporary:false ctxt in
  let nat_ty = Script_typed_ir.nat_t in
  let*?@ nat_ty_node, ctxt =
    Script_ir_unparser.unparse_ty ctxt ~loc:() nat_ty
  in
  let nat_ty_expr = Micheline.strip_locations nat_ty_node in
  let alloc = Big_map.{key_type = nat_ty_expr; value_type = nat_ty_expr} in
  let init = Lazy_storage.Alloc alloc in
  let diffs =
    [
      Lazy_storage.make
        Lazy_storage.Kind.Big_map
        id
        (Update {init; updates = []});
    ]
  in
  let*@ ctxt =
    Contract.update_script_storage ctxt originated nat_ty_expr (Some diffs)
  in
  return (ctxt, id)

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  s

let path = project_root // Filename.dirname __FILE__

(** Check that the custom stack overflow exception is triggered when
   it should be. *)
let test_typecheck_stack_overflow () =
  let open Lwt_result_syntax in
  let* ctxt = test_context () in
  let storage = "Unit" in
  let parameter = "Unit" in
  let script = read_file (path // "contracts/big_interpreter_stack.tz") in
  let*! result =
    Contract_helpers.run_script ctxt script ~storage ~parameter ()
  in
  match result with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error lst
    when List.mem
           ~equal:( = )
           (Environment.Ecoproto_error
              Script_tc_errors.Typechecking_too_many_recursive_calls)
           lst ->
      return_unit
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_trace errs

(* NOTE: this test fails with an out-of-memory exception. *)
let _test_unparse_stack_overflow () =
  let open Lwt_result_syntax in
  let* ctxt = test_context () in
  (* Meme *)
  let enorme_et_seq n =
    let rec aux n acc = aux (n - 1) @@ Micheline.Seq (0, [acc]) in
    aux n (Micheline.Int (0, Z.zero))
  in
  let*! result =
    Script_ir_translator.(unparse_code ctxt Readable (enorme_et_seq 10_001))
  in
  match result with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error trace ->
      let trace_string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace trace
      in
      let expect_id = "michelson_v1.unparsing_stack_overflow" in
      let expect_descrfiption =
        "Too many recursive calls were needed for unparsing"
      in
      if
        Astring.String.is_infix ~affix:expect_id trace_string
        && Astring.String.is_infix ~affix:expect_descrfiption trace_string
      then return_unit
      else
        Alcotest.failf
          "Unexpected error (%s) at %s"
          trace_string
          __LOC__
          return_unit

let location = function
  | Prim (loc, _, _, _)
  | Int (loc, _)
  | String (loc, _)
  | Bytes (loc, _)
  | Seq (loc, _) ->
      loc

let test_parse_ty (type exp expc) ctxt node
    (expected : (exp, expc) Script_typed_ir.ty) =
  let open Result_wrap_syntax in
  let legacy = false in
  let allow_lazy_storage = true in
  let allow_operation = true in
  let allow_contract = true in
  let allow_ticket = true in
  let@ result =
    let* Script_typed_ir.Ex_ty actual, ctxt =
      Script_ir_translator.parse_ty
        ctxt
        ~legacy
        ~allow_lazy_storage
        ~allow_operation
        ~allow_contract
        ~allow_ticket
        node
    in
    let* eq, ctxt =
      Gas_monad.run ctxt
      @@ Script_ir_translator.ty_eq
           ~error_details:(Informative (location node))
           actual
           expected
    in
    let+ Eq = eq in
    ctxt
  in
  result

let test_parse_comb_type () =
  let open Lwt_result_wrap_syntax in
  let open Script in
  let open Script_typed_ir in
  let nat_prim = Prim (-1, T_nat, [], []) in
  let nat_prim_a = Prim (-1, T_nat, [], ["%a"]) in
  let nat_prim_b = Prim (-1, T_nat, [], ["%b"]) in
  let nat_prim_c = Prim (-1, T_nat, [], ["%c"]) in
  let nat_ty = nat_t in
  let pair_prim l = Prim (-1, T_pair, l, []) in
  let pair_ty ty1 ty2 = pair_t (-1) ty1 ty2 in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_nat_nat_prim = pair_prim2 nat_prim nat_prim in
  let*?@ (Ty_ex_c pair_nat_nat_ty) = pair_ty nat_ty nat_ty in
  let* ctxt = test_context () in
  (* pair nat nat *)
  let*? ctxt = test_parse_ty ctxt pair_nat_nat_prim pair_nat_nat_ty in
  (* pair (pair nat nat) nat *)
  let*?@ (Ty_ex_c pair_pair_nat_nat_nat_ty) = pair_ty pair_nat_nat_ty nat_ty in
  let*? ctxt =
    test_parse_ty
      ctxt
      (pair_prim2 pair_nat_nat_prim nat_prim)
      pair_pair_nat_nat_nat_ty
  in
  (* pair nat (pair nat nat) *)
  let*?@ (Ty_ex_c pair_nat_pair_nat_nat_ty) = pair_ty nat_ty pair_nat_nat_ty in
  let*? ctxt =
    test_parse_ty
      ctxt
      (pair_prim2 nat_prim pair_nat_nat_prim)
      pair_nat_pair_nat_nat_ty
  in
  (* pair nat nat nat *)
  let*?@ (Ty_ex_c pair_nat_nat_nat_ty) = pair_ty nat_ty pair_nat_nat_ty in
  let*? ctxt =
    test_parse_ty
      ctxt
      (pair_prim [nat_prim; nat_prim; nat_prim])
      pair_nat_nat_nat_ty
  in
  (* pair (nat %a) nat *)
  let*?@ (Ty_ex_c pair_nat_a_nat_ty) = pair_t (-1) nat_ty nat_ty in
  let*? ctxt =
    test_parse_ty ctxt (pair_prim2 nat_prim_a nat_prim) pair_nat_a_nat_ty
  in
  (* pair nat (nat %b) *)
  let*?@ (Ty_ex_c pair_nat_nat_b_ty) = pair_t (-1) nat_ty nat_ty in
  let*? ctxt =
    test_parse_ty ctxt (pair_prim2 nat_prim nat_prim_b) pair_nat_nat_b_ty
  in
  (* pair (nat %a) (nat %b) *)
  let*?@ (Ty_ex_c pair_nat_a_nat_b_ty) = pair_t (-1) nat_ty nat_ty in
  let*? ctxt =
    test_parse_ty ctxt (pair_prim2 nat_prim_a nat_prim_b) pair_nat_a_nat_b_ty
  in
  (* pair (nat %a) (nat %b) (nat %c) *)
  let*?@ (Ty_ex_c pair_nat_b_nat_c_ty) = pair_t (-1) nat_ty nat_ty in
  let*?@ (Ty_ex_c pair_nat_a_nat_b_nat_c_ty) =
    pair_t (-1) nat_ty pair_nat_b_nat_c_ty
  in
  let*? ctxt =
    test_parse_ty
      ctxt
      (pair_prim [nat_prim_a; nat_prim_b; nat_prim_c])
      pair_nat_a_nat_b_nat_c_ty
  in
  (* pair (nat %a) (pair %b nat nat) *)
  let*?@ (Ty_ex_c pair_b_nat_nat_ty) = pair_t (-1) nat_ty nat_ty in
  let*?@ (Ty_ex_c pair_nat_a_pair_b_nat_nat_ty) =
    pair_t (-1) nat_ty pair_b_nat_nat_ty
  in
  let*? (_ : context) =
    test_parse_ty
      ctxt
      (pair_prim2 nat_prim_a (Prim (-1, T_pair, [nat_prim; nat_prim], ["%b"])))
      pair_nat_a_pair_b_nat_nat_ty
  in
  return_unit

let test_unparse_ty loc ctxt expected ty =
  let open Result_syntax in
  let* actual, ctxt = Script_ir_unparser.unparse_ty ctxt ~loc:() ty in
  if actual = expected then Ok ctxt
  else Alcotest.failf "Unexpected error: %s" loc

let test_unparse_comb_type () =
  let open Lwt_result_wrap_syntax in
  let open Script in
  let open Script_typed_ir in
  let nat_prim = Prim ((), T_nat, [], []) in
  let nat_ty = nat_t in
  let pair_prim l = Prim ((), T_pair, l, []) in
  let pair_ty ty1 ty2 = pair_t (-1) ty1 ty2 in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_nat_nat_prim = pair_prim2 nat_prim nat_prim in
  let*?@ (Ty_ex_c pair_nat_nat_ty) = pair_ty nat_ty nat_ty in
  let* ctxt = test_context () in
  (* pair nat nat *)
  let*?@ ctxt =
    test_unparse_ty __LOC__ ctxt pair_nat_nat_prim pair_nat_nat_ty
  in
  (* pair (pair nat nat) nat *)
  let*?@ (Ty_ex_c pair_pair_nat_nat_nat_ty) = pair_ty pair_nat_nat_ty nat_ty in
  let*?@ ctxt =
    test_unparse_ty
      __LOC__
      ctxt
      (pair_prim2 pair_nat_nat_prim nat_prim)
      pair_pair_nat_nat_nat_ty
  in
  (* pair nat nat nat *)
  let*?@ (Ty_ex_c pair_nat_nat_nat_ty) = pair_ty nat_ty pair_nat_nat_ty in
  let*?@ (_ : context) =
    test_unparse_ty
      __LOC__
      ctxt
      (pair_prim [nat_prim; nat_prim; nat_prim])
      pair_nat_nat_nat_ty
  in
  return_unit

let test_unparse_comparable_ty loc ctxt expected ty =
  (* unparse_comparable_ty is not exported, the simplest way to call it is to
     call parse_ty on a set type *)
  let open Result_syntax in
  let open Script_typed_ir in
  let* set_ty_ty = set_t (-1) ty in
  let* actual, ctxt = Script_ir_unparser.unparse_ty ctxt ~loc:() set_ty_ty in
  if actual = Prim ((), T_set, [expected], []) then return ctxt
  else Alcotest.failf "Unexpected error: %s" loc

let test_unparse_comb_comparable_type () =
  let open Lwt_result_wrap_syntax in
  let open Script in
  let open Script_typed_ir in
  let nat_prim = Prim ((), T_nat, [], []) in
  let nat_ty = nat_t in
  let pair_prim l = Prim ((), T_pair, l, []) in
  let pair_ty ty1 ty2 = comparable_pair_t (-1) ty1 ty2 in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_nat_nat_prim = pair_prim2 nat_prim nat_prim in
  let*?@ pair_nat_nat_ty = pair_ty nat_ty nat_ty in
  let* ctxt = test_context () in
  (* pair nat nat *)
  let*?@ ctxt =
    test_unparse_comparable_ty __LOC__ ctxt pair_nat_nat_prim pair_nat_nat_ty
  in
  (* pair (pair nat nat) nat *)
  let*?@ pair_pair_nat_nat_nat_ty = pair_ty pair_nat_nat_ty nat_ty in
  let*?@ ctxt =
    test_unparse_comparable_ty
      __LOC__
      ctxt
      (pair_prim2 pair_nat_nat_prim nat_prim)
      pair_pair_nat_nat_nat_ty
  in
  (* pair nat nat nat *)
  let*?@ pair_nat_nat_nat_ty = pair_ty nat_ty pair_nat_nat_ty in
  let*?@ (_ : context) =
    test_unparse_comparable_ty
      __LOC__
      ctxt
      (pair_prim [nat_prim; nat_prim; nat_prim])
      pair_nat_nat_nat_ty
  in
  return_unit

let test_parse_data ?(equal = Stdlib.( = )) loc ctxt ty node expected =
  let open Lwt_result_wrap_syntax in
  let elab_conf = Script_ir_translator_config.make ~legacy:false () in
  let allow_forged = true in
  let*@ actual, ctxt =
    Script_ir_translator.parse_data ctxt ~elab_conf ~allow_forged ty node
  in
  if equal actual expected then return ctxt
  else Alcotest.failf "Unexpected error: %s" loc

let test_parse_data_fails loc ctxt ty node =
  let open Lwt_result_wrap_syntax in
  let elab_conf = Script_ir_translator_config.make ~legacy:false () in
  let allow_forged = false in
  let*! result =
    Script_ir_translator.parse_data ctxt ~elab_conf ~allow_forged ty node
  in
  match result with
  | Ok _ -> Alcotest.failf "Unexpected typechecking success: %s" loc
  | Error trace ->
      let trace_string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace trace
      in
      let expect_id = "michelson_v1.invalid_constant" in
      let expect_descrfiption =
        "A data expression was invalid for its expected type."
      in
      if
        Astring.String.is_infix ~affix:expect_id trace_string
        && Astring.String.is_infix ~affix:expect_descrfiption trace_string
      then return_unit
      else
        Alcotest.failf
          "Unexpected error (%s) at %s"
          trace_string
          __LOC__
          return_unit

let test_parse_comb_data () =
  let open Lwt_result_wrap_syntax in
  let open Script in
  let open Script_typed_ir in
  let z = Script_int.zero_n in
  let z_prim = Micheline.Int (-1, Z.zero) in
  let nat_ty = nat_t in
  let pair_prim l = Prim (-1, D_Pair, l, []) in
  let pair_ty ty1 ty2 = pair_t (-1) ty1 ty2 in
  let*?@ (Ty_ex_c pair_nat_nat_ty) = pair_ty nat_ty nat_ty in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_z_z_prim = pair_prim2 z_prim z_prim in
  let*?@ list_nat_ty = list_t (-1) nat_ty in
  let*?@ big_map_nat_nat_ty = big_map_t (-1) nat_ty nat_ty in
  let* ctxt, big_map_id = test_context_with_nat_nat_big_map () in
  (* Pair 0 0 *)
  let* ctxt =
    test_parse_data __LOC__ ctxt pair_nat_nat_ty pair_z_z_prim (z, z)
  in
  (* {0; 0} *)
  let* ctxt =
    test_parse_data
      __LOC__
      ctxt
      pair_nat_nat_ty
      (Micheline.Seq (-1, [z_prim; z_prim]))
      (z, z)
  in
  (* Pair (Pair 0 0) 0 *)
  let*?@ (Ty_ex_c pair_pair_nat_nat_nat_ty) = pair_ty pair_nat_nat_ty nat_ty in
  let* ctxt =
    test_parse_data
      __LOC__
      ctxt
      pair_pair_nat_nat_nat_ty
      (pair_prim2 pair_z_z_prim z_prim)
      ((z, z), z)
  in
  (* Pair 0 (Pair 0 0) *)
  let*?@ (Ty_ex_c pair_nat_pair_nat_nat_ty) = pair_ty nat_ty pair_nat_nat_ty in
  let* ctxt =
    test_parse_data
      __LOC__
      ctxt
      pair_nat_pair_nat_nat_ty
      (pair_prim2 z_prim pair_z_z_prim)
      (z, (z, z))
  in
  (* Pair 0 0 0 *)
  let* ctxt =
    test_parse_data
      __LOC__
      ctxt
      pair_nat_pair_nat_nat_ty
      (pair_prim [z_prim; z_prim; z_prim])
      (z, (z, z))
  in
  (* {0; 0; 0} *)
  let* ctxt =
    test_parse_data
      __LOC__
      ctxt
      pair_nat_pair_nat_nat_ty
      (Micheline.Seq (-1, [z_prim; z_prim; z_prim]))
      (z, (z, z))
  in
  (* Should fail: {0} against pair nat (list nat) *)
  let*?@ (Ty_ex_c pair_nat_list_nat_ty) = pair_ty nat_ty list_nat_ty in
  let* () =
    test_parse_data_fails
      __LOC__
      ctxt
      pair_nat_list_nat_ty
      (Micheline.Seq (-1, [z_prim]))
  in
  (* Should fail: {0; 0; 0} against pair nat (list nat) *)
  let* () =
    test_parse_data_fails
      __LOC__
      ctxt
      pair_nat_list_nat_ty
      (Micheline.Seq (-1, [z_prim; z_prim; z_prim]))
  in
  (* check Pair 0 (Pair 0 {}) against pair nat (big_map nat nat)
     so that the following test fails for the good reason and not because
     the big map doesn't exist
  *)
  let id_z = Big_map.Id.unparse_to_z big_map_id in
  let id_prim = Int (-1, id_z) in
  let expected_big_map =
    let open Script_typed_ir in
    let diff = {map = Big_map_overlay.empty; size = 0} in
    Big_map {id = Some big_map_id; diff; key_type = nat_ty; value_type = nat_ty}
  in
  let ty_equal :
      type a ac1 ac2.
      (a, ac1) Script_typed_ir.ty -> (a, ac2) Script_typed_ir.ty -> bool =
   fun ty1 ty2 ->
    match Script_typed_ir.(is_comparable ty1, is_comparable ty2) with
    | Yes, Yes -> ty1 = ty2
    | No, No -> ty1 = ty2
    | Yes, No -> assert false
    | No, Yes -> assert false
   (*
      These last two cases can't happen because the comparable character of a
      type is a function of its concrete type.
      It is possible to write a function that proves it but it is not needed
      in the protocol for the moment.
   *)
  in
  let equal (nat1, Big_map big_map1) (nat2, Big_map big_map2) =
    (* Custom equal needed because big maps contain boxed maps containing functional values *)
    nat1 = nat2 && big_map1.id = big_map2.id
    && big_map1.key_type = big_map2.key_type
    && ty_equal big_map1.value_type big_map2.value_type
    && big_map1.diff.size = big_map2.diff.size
    && Big_map_overlay.bindings big_map1.diff.map
       = Big_map_overlay.bindings big_map2.diff.map
  in
  let*?@ (Ty_ex_c pair_nat_big_map_nat_nat_ty) =
    pair_ty nat_ty big_map_nat_nat_ty
  in
  let* ctxt =
    test_parse_data
      ~equal
      __LOC__
      ctxt
      pair_nat_big_map_nat_nat_ty
      (pair_prim2 z_prim (pair_prim2 id_prim (Seq (-1, []))))
      (Script_int.zero_n, expected_big_map)
  in
  (* Should fail: Pair 0 0 {} against pair nat (big_map nat nat) *)
  test_parse_data_fails
    __LOC__
    ctxt
    pair_nat_big_map_nat_nat_ty
    (pair_prim [z_prim; id_prim; Seq (-1, [])])

let test_parse_address () =
  let open Lwt_result_wrap_syntax in
  let open Script_typed_ir in
  let* ctxt, _big_map_id = test_context_with_nat_nat_big_map () in
  (* KT1% (empty entrypoint) *)
  let*?@ kt1fake =
    Contract.of_b58check "KT1FAKEFAKEFAKEFAKEFAKEFAKEFAKGGSE2x"
  in
  let* ctxt =
    test_parse_data
      __LOC__
      ctxt
      address_t
      (String (-1, "KT1FAKEFAKEFAKEFAKEFAKEFAKEFAKGGSE2x%"))
      {destination = Contract kt1fake; entrypoint = Entrypoint.default}
  in
  (* tz1% (empty entrypoint) *)
  let*?@ tz1fake =
    Contract.of_b58check "tz1fakefakefakefakefakefakefakcphLA5"
  in
  let* ctxt =
    test_parse_data
      __LOC__
      ctxt
      address_t
      (String (-1, "tz1fakefakefakefakefakefakefakcphLA5%"))
      {destination = Contract tz1fake; entrypoint = Entrypoint.default}
  in
  (* scr1% (empty entrypoint) *)
  let*?@ scr1 =
    Destination.of_b58check "sr1JPVatbbPoGp4vb6VfQ1jzEPMrYFcKq6VG"
  in
  let* ctxt =
    test_parse_data
      __LOC__
      ctxt
      address_t
      (String (-1, "sr1JPVatbbPoGp4vb6VfQ1jzEPMrYFcKq6VG"))
      {destination = scr1; entrypoint = Entrypoint.default}
  in
  (* scr1% (default entrypoint) *)
  let+ (_ctxt : context) =
    test_parse_data
      __LOC__
      ctxt
      address_t
      (String (-1, "sr1JPVatbbPoGp4vb6VfQ1jzEPMrYFcKq6VG%"))
      {destination = scr1; entrypoint = Entrypoint.default}
  in
  ()

let test_unparse_data loc ctxt ty x ~expected_readable ~expected_optimized =
  let open Lwt_result_wrap_syntax in
  let*@ actual_readable, ctxt =
    Script_ir_translator.unparse_data ctxt Script_ir_unparser.Readable ty x
  in
  let*@ ctxt =
    if actual_readable = Micheline.strip_locations expected_readable then
      return ctxt
    else Alcotest.failf "Error in readable unparsing: %s" loc
  in
  let*@ actual_optimized, ctxt =
    Script_ir_translator.unparse_data ctxt Script_ir_unparser.Optimized ty x
  in
  if actual_optimized = Micheline.strip_locations expected_optimized then
    return ctxt
  else Alcotest.failf "Error in optimized unparsing: %s" loc

let test_unparse_comb_data () =
  let open Lwt_result_wrap_syntax in
  let open Script in
  let open Script_typed_ir in
  let z = Script_int.zero_n in
  let z_prim = Micheline.Int (-1, Z.zero) in
  let nat_ty = nat_t in
  let pair_prim l = Prim (-1, D_Pair, l, []) in
  let pair_ty ty1 ty2 = pair_t (-1) ty1 ty2 in
  let*?@ (Ty_ex_c pair_nat_nat_ty) = pair_ty nat_ty nat_ty in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_z_z_prim = pair_prim2 z_prim z_prim in
  let* ctxt = test_context () in
  (* Pair 0 0 *)
  let* ctxt =
    test_unparse_data
      __LOC__
      ctxt
      pair_nat_nat_ty
      (z, z)
      ~expected_readable:pair_z_z_prim
      ~expected_optimized:pair_z_z_prim
  in
  (* Pair (Pair 0 0) 0 *)
  let*?@ (Ty_ex_c pair_pair_nat_nat_nat_ty) = pair_ty pair_nat_nat_ty nat_ty in
  let* ctxt =
    test_unparse_data
      __LOC__
      ctxt
      pair_pair_nat_nat_nat_ty
      ((z, z), z)
      ~expected_readable:(pair_prim2 pair_z_z_prim z_prim)
      ~expected_optimized:(pair_prim2 pair_z_z_prim z_prim)
  in
  (* Readable: Pair 0 0 0; Optimized: Pair 0 (Pair 0 0) *)
  let*?@ (Ty_ex_c pair_nat_pair_nat_nat_ty) = pair_ty nat_ty pair_nat_nat_ty in
  let* ctxt =
    test_unparse_data
      __LOC__
      ctxt
      pair_nat_pair_nat_nat_ty
      (z, (z, z))
      ~expected_readable:(pair_prim [z_prim; z_prim; z_prim])
      ~expected_optimized:(pair_prim2 z_prim pair_z_z_prim)
  in
  (* Readable: Pair 0 0 0 0; Optimized: {0; 0; 0; 0} *)
  let*?@ (Ty_ex_c pair_nat_pair_nat_pair_nat_nat_ty) =
    pair_ty nat_ty pair_nat_pair_nat_nat_ty
  in
  let* (_ : context) =
    test_unparse_data
      __LOC__
      ctxt
      pair_nat_pair_nat_pair_nat_nat_ty
      (z, (z, (z, z)))
      ~expected_readable:(pair_prim [z_prim; z_prim; z_prim; z_prim])
      ~expected_optimized:(Micheline.Seq (-1, [z_prim; z_prim; z_prim; z_prim]))
  in
  return_unit

(* Generate all the possible syntaxes for pairs *)
let gen_pairs left right =
  [Prim ((), Script.D_Pair, [left; right], []); Seq ((), [left; right])]

(* Generate all the possible syntaxes for combs *)
let rec gen_combs leaf arity =
  assert (arity >= 2) ;
  if arity = 2 then gen_pairs leaf leaf
  else
    gen_combs leaf (arity - 1)
    |> List.map (fun smaller ->
           (match smaller with
           | Prim (loc, Script.D_Pair, vs, []) ->
               Prim (loc, Script.D_Pair, leaf :: vs, [])
           | Seq (loc, vs) -> Seq (loc, leaf :: vs)
           | _ -> assert false)
           :: gen_pairs leaf smaller)
    |> List.flatten

(* Checks the optimality of the Optimized Micheline representation for combs *)
let test_optimal_comb () =
  let open Lwt_result_wrap_syntax in
  let open Script_typed_ir in
  let leaf_ty = nat_t in
  let leaf_mich = Int ((), Z.zero) in
  let leaf_v = Script_int.zero_n in
  let size_of_micheline mich =
    let canonical = Micheline.strip_locations mich in
    ( canonical,
      Bytes.length
      @@ Data_encoding.Binary.to_bytes_exn Script.expr_encoding canonical )
  in
  let check_optimal_comb loc ctxt ty v arity =
    let*@ unparsed, ctxt =
      Script_ir_translator.unparse_data ctxt Script_ir_unparser.Optimized ty v
    in
    let unparsed_canonical, unparsed_size =
      size_of_micheline (Micheline.root unparsed)
    in
    let*@ () =
      List.iter_es (fun other_repr ->
          let other_repr_canonical, other_repr_size =
            size_of_micheline other_repr
          in
          if other_repr_size < unparsed_size then
            Alcotest.failf
              "At %s, for comb of arity %d, representation %a (size %d bytes) \
               is shorter than representation %a (size %d bytes) returned by \
               unparse_data in Optimized mode"
              loc
              arity
              Michelson_v1_printer.print_expr
              other_repr_canonical
              other_repr_size
              Michelson_v1_printer.print_expr
              unparsed_canonical
              unparsed_size
          else return_unit)
      @@ gen_combs leaf_mich arity
    in
    return ctxt
  in
  let pair_ty ty1 ty2 = pair_t (-1) ty1 ty2 in
  let* ctxt = test_context () in
  let*?@ (Ty_ex_c comb2_ty) = pair_ty leaf_ty leaf_ty in
  let comb2_v = (leaf_v, leaf_v) in
  let* ctxt = check_optimal_comb __LOC__ ctxt comb2_ty comb2_v 2 in
  let*?@ (Ty_ex_c comb3_ty) = pair_ty leaf_ty comb2_ty in
  let comb3_v = (leaf_v, comb2_v) in
  let* ctxt = check_optimal_comb __LOC__ ctxt comb3_ty comb3_v 3 in
  let*?@ (Ty_ex_c comb4_ty) = pair_ty leaf_ty comb3_ty in
  let comb4_v = (leaf_v, comb3_v) in
  let* ctxt = check_optimal_comb __LOC__ ctxt comb4_ty comb4_v 4 in
  let*?@ (Ty_ex_c comb5_ty) = pair_ty leaf_ty comb4_ty in
  let comb5_v = (leaf_v, comb4_v) in
  let* (_ : context) = check_optimal_comb __LOC__ ctxt comb5_ty comb5_v 5 in
  return_unit

(* Check that UNPACK on contract is forbidden.
   See https://gitlab.com/tezos/tezos/-/issues/301 for the motivation
   behind this restriction.
*)
let test_contract_not_packable () =
  let open Lwt_result_syntax in
  let elab_conf = Script_ir_translator_config.make ~legacy:false () in
  let contract_unit =
    Prim (0, Script.T_contract, [Prim (0, T_unit, [], [])], [])
  in
  let* ctxt = test_context () in
  (* Test that [contract_unit] is parsable *)
  let* () =
    match
      Script_ir_translator.parse_any_ty ctxt ~legacy:false contract_unit
    with
    | Ok _ -> Lwt_result_syntax.return_unit
    | Error _ -> Alcotest.failf "Could not parse (contract unit)"
  in
  (* Test that [contract_unit] is not packable *)
  let* () =
    match
      Script_ir_translator.parse_packable_ty ctxt ~legacy:false contract_unit
    with
    | Ok _ ->
        Alcotest.failf
          "(contract unit) should not be packable, see \
           https://gitlab.com/tezos/tezos/-/issues/301"
    | Error _ -> return_unit
  in
  (* Test that elaboration of the [UNPACK unit] instruction succeeds *)
  let* () =
    let*! result =
      Script_ir_translator.parse_instr
        Script_tc_context.data
        ctxt
        ~elab_conf
        (Prim (0, I_UNPACK, [Prim (0, T_unit, [], [])], []))
        (Item_t (Script_typed_ir.bytes_t, Bot_t))
    in
    match result with
    | Ok _ -> return_unit
    | Error _ -> Alcotest.failf "Could not parse UNPACK unit"
  in
  (* Test that elaboration of the [UNPACK (contract unit)] instruction fails *)
  let*! result =
    Script_ir_translator.parse_instr
      Script_tc_context.data
      ctxt
      ~elab_conf
      (Prim (0, I_UNPACK, [contract_unit], []))
      (Item_t (Script_typed_ir.bytes_t, Bot_t))
  in
  match result with
  | Ok _ ->
      Alcotest.failf
        "UNPACK (contract unit) should not be allowed, see \
         https://gitlab.com/tezos/tezos/-/issues/301"
  | Error _ -> return_unit

(* This test function is used to checks forbidden operations in views. *)
let test_forbidden_op_in_view op () =
  let open Lwt_result_syntax in
  let prefix = path // "contracts/forbidden_op_in_view_" in
  let script = read_file (prefix ^ op ^ ".tz") in
  let contract_expr = Expr.from_string script in
  let* ctxt = test_context () in
  let*! result =
    Script_ir_translator.typecheck_code
      ~legacy:false
      ~show_types:false
      ctxt
      contract_expr
  in
  match result with
  | Ok _ ->
      Alcotest.failf
        "%s should not be allowed in views, see \
         https://gitlab.com/tezos/tezos/-/issues/1922"
        op
  | Error _ -> return_unit

(** Test [parse_contract_data] for rollup with unit type. *)
let test_parse_contract_data_for_unit_rollup () =
  let open Lwt_result_wrap_syntax in
  let* block, (contract, _) = context_init_with_sc_rollup_arith_enabled T2 in
  let* block, rollup = sc_originate block contract "unit" in
  let* incr = Incremental.begin_construction block in
  let ctxt = Incremental.alpha_ctxt incr in
  let*@ _ctxt, typed_contract =
    Script_ir_translator.parse_contract_data
      ctxt
      (-1)
      Script_typed_ir.unit_t
      (Destination.Sc_rollup rollup)
      ~entrypoint:Entrypoint.default
  in
  let (Ty_ex_c Script_typed_ir.Unit_t) =
    Script_typed_ir.Typed_contract.arg_ty typed_contract
  in
  let destination = Script_typed_ir.Typed_contract.destination typed_contract in
  let entrypoint = Script_typed_ir.Typed_contract.entrypoint typed_contract in
  (* Check that the destinations match. *)
  let* () =
    Assert.equal_string
      ~loc:__LOC__
      (Destination.to_b58check destination)
      (Sc_rollup.Address.to_b58check rollup)
  in
  (* Check that entrypoints match. *)
  let* () =
    Assert.equal_string ~loc:__LOC__ (Entrypoint.to_string entrypoint) "default"
  in
  return_unit

(** Test that [parse_contract_data] for rollup with invalid type fails. *)
let test_parse_contract_data_for_rollup_with_invalid_type () =
  let open Lwt_result_wrap_syntax in
  let* block, (contract, _) = context_init_with_sc_rollup_arith_enabled T2 in
  let* block, rollup = sc_originate block contract "string" in
  let* incr = Incremental.begin_construction block in
  let ctxt = Incremental.alpha_ctxt incr in
  let entrypoint = Entrypoint.of_string_strict_exn "add" in
  let*!@ res =
    Script_ir_translator.parse_contract_data
      ctxt
      (-1)
      Script_typed_ir.unit_t
      (Destination.Sc_rollup rollup)
      ~entrypoint
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (( = ) (Script_tc_errors.No_such_entrypoint entrypoint))

let test_contract path ~ok ~ko () =
  let open Lwt_result_syntax in
  let contract = path in
  let script = read_file contract in
  let contract_expr = Expr.from_string script in
  let* ctxt = test_context () in
  let*! result =
    Script_ir_translator.typecheck_code
      ~legacy:false
      ~show_types:false
      ctxt
      contract_expr
  in
  match result with Ok _ -> ok () | Error t -> ko t

let test_contract_success path =
  let open Lwt_result_syntax in
  test_contract path ~ok:return ~ko:(fun t ->
      Alcotest.failf "Unexpected error: %a" Environment.Error_monad.pp_trace t)

let test_contract_failure path =
  let open Lwt_result_syntax in
  test_contract
    path
    ~ok:(fun () ->
      Alcotest.failf
        "Unexpected success: typechecking %s should have failed"
        path)
    ~ko:(fun _ -> return_unit)

let tests =
  [
    Tztest.tztest "unparse view" `Quick test_unparse_view;
    Tztest.tztest
      "typecheck stack overflow error"
      `Quick
      test_typecheck_stack_overflow;
    Tztest.tztest "comb type parsing" `Quick test_parse_comb_type;
    Tztest.tztest "comb type unparsing" `Quick test_unparse_comb_type;
    Tztest.tztest
      "comb comparable type unparsing"
      `Quick
      test_unparse_comb_comparable_type;
    Tztest.tztest "comb data parsing" `Quick test_parse_comb_data;
    Tztest.tztest "comb data unparsing" `Quick test_unparse_comb_data;
    Tztest.tztest "optimal comb data unparsing" `Quick test_optimal_comb;
    Tztest.tztest "parse address" `Quick test_parse_address;
    Tztest.tztest
      "unpackability of the contract type"
      `Quick
      test_contract_not_packable;
    Tztest.tztest
      "forbidden SELF in view"
      `Quick
      (test_forbidden_op_in_view "SELF");
    Tztest.tztest
      "forbidden SET_DELEGATE in view"
      `Quick
      (test_forbidden_op_in_view "SET_DELEGATE");
    Tztest.tztest
      "forbidden TRANSFER_TOKENS in view"
      `Quick
      (test_forbidden_op_in_view "TRANSFER_TOKENS");
    Tztest.tztest
      "forbidden CREATE_CONTRACT in view"
      `Quick
      (test_forbidden_op_in_view "CREATE_CONTRACT");
    Tztest.tztest
      "parse contract data for rollup"
      `Quick
      test_parse_contract_data_for_unit_rollup;
    Tztest.tztest
      "parse contract data for rollup with entrypoint invalid type"
      `Quick
      test_parse_contract_data_for_rollup_with_invalid_type;
    Tztest.tztest
      "lambda_rec instruction"
      `Quick
      (test_contract_success (path // "contracts/rec_fact.tz"));
    Tztest.tztest
      "lambda_rec instruction with apply"
      `Quick
      (test_contract_success (path // "contracts/rec_fact_apply.tz"));
    Tztest.tztest
      "lambda_rec with type error"
      `Quick
      (test_contract_failure (path // "contracts/fail_rec.tz"));
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("typechecking", tests)]
  |> Lwt_main.run
