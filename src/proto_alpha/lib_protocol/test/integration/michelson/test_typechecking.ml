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
    Invocation:   cd src/proto_alpha/lib_protocol/test/integration/michelson && \
                  dune exec ./main.exe -- test "^typechecking$"
    Subject:      Type-checking
*)

open Protocol
open Alpha_context
open Micheline
open Error_monad_operators

let wrap_error_lwt x = x >>= fun x -> Lwt.return @@ Environment.wrap_tzresult x

(* Test for Script_ir_translator.unparse_script on a script declaring views. *)
let test_unparse_view () =
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
  Context.init 3 >>=? fun (b, _cs) ->
  Incremental.begin_construction b >>=? fun v ->
  let ctx = Incremental.alpha_ctxt v in
  Script_ir_translator.parse_script
    ctx
    ~legacy:true
    ~allow_forged_in_storage:false
    script
  >>=?? fun (Ex_script script, ctx) ->
  Script_ir_translator.unparse_script ctx Readable script
  >>=?? fun (unparse_script, _ctx) ->
  let aft = Data_encoding.force_bytes unparse_script.code in
  Alcotest.(check bytes) "didn't match" bef aft |> return

let test_context () =
  Context.init ~consensus_threshold:0 3 >>=? fun (b, _cs) ->
  Incremental.begin_construction b >>=? fun v ->
  return (Incremental.alpha_ctxt v)

let test_context_with_nat_nat_big_map () =
  Context.init 3 >>=? fun (b, contracts) ->
  let source = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
  Op.contract_origination (B b) source ~script:Op.dummy_script
  >>=? fun (operation, originated) ->
  Block.bake ~operation b >>=? fun b ->
  Incremental.begin_construction b >>=? fun v ->
  let ctxt = Incremental.alpha_ctxt v in
  wrap_error_lwt @@ Big_map.fresh ~temporary:false ctxt >>=? fun (ctxt, id) ->
  let nat_ty = Script_typed_ir.nat_t ~annot:None in
  wrap_error_lwt @@ Lwt.return
  @@ Script_ir_translator.unparse_ty ~loc:() ctxt nat_ty
  >>=? fun (nat_ty_node, ctxt) ->
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
  wrap_error_lwt
  @@ Contract.update_script_storage ctxt originated nat_ty_expr (Some diffs)
  >>=? fun ctxt -> return (ctxt, id)

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  s

(** Check that the custom stack overflow exception is triggered when
   it should be. *)
let test_typecheck_stack_overflow () =
  test_context () >>=? fun ctxt ->
  let storage = "Unit" in
  let parameter = "Unit" in
  let script = read_file "./contracts/big_interpreter_stack.tz" in
  Contract_helpers.run_script ctxt script ~storage ~parameter () >>= function
  | Ok _ -> Alcotest.fail "expected an error"
  | Error lst
    when List.mem
           ~equal:( = )
           (Environment.Ecoproto_error
              Script_tc_errors.Typechecking_too_many_recursive_calls)
           lst ->
      return ()
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_trace errs

(* NOTE: this test fails with an out-of-memory exception. *)
let _test_unparse_stack_overflow () =
  test_context () >>=? fun ctxt ->
  (* Meme *)
  let enorme_et_seq n =
    let rec aux n acc = aux (n - 1) @@ Micheline.Seq (0, [acc]) in
    aux n (Micheline.Int (0, Z.zero))
  in
  Script_ir_translator.(unparse_code ctxt Readable (enorme_et_seq 10_001))
  >>= function
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

let test_parse_ty ctxt node expected =
  let legacy = false in
  let allow_lazy_storage = true in
  let allow_operation = true in
  let allow_contract = true in
  let allow_ticket = true in
  Environment.wrap_tzresult
    ( Script_ir_translator.parse_ty
        ctxt
        ~legacy
        ~allow_lazy_storage
        ~allow_operation
        ~allow_contract
        ~allow_ticket
        node
    >>? fun (Script_ir_translator.Ex_ty actual, ctxt) ->
      Script_ir_translator.ty_eq ctxt (location node) actual expected
      >|? fun (_, ctxt) -> ctxt )

let type_annot = Script_ir_annot.FOR_TESTS.unsafe_type_annot_of_string

let field_annot = Script_ir_annot.FOR_TESTS.unsafe_field_annot_of_string

let test_parse_comb_type () =
  let open Script in
  let open Script_typed_ir in
  let nat_prim = Prim (-1, T_nat, [], []) in
  let nat_prim_a = Prim (-1, T_nat, [], ["%a"]) in
  let nat_prim_b = Prim (-1, T_nat, [], ["%b"]) in
  let nat_prim_c = Prim (-1, T_nat, [], ["%c"]) in
  let nat_ty = nat_t ~annot:None in
  let pair_prim l = Prim (-1, T_pair, l, []) in
  let pair_ty ty1 ty2 = pair_t (-1) (ty1, None) (ty2, None) ~annot:None in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_nat_nat_prim = pair_prim2 nat_prim nat_prim in
  pair_ty nat_ty nat_ty >>??= fun pair_nat_nat_ty ->
  test_context () >>=? fun ctxt ->
  (* pair nat nat *)
  test_parse_ty ctxt pair_nat_nat_prim pair_nat_nat_ty >>?= fun ctxt ->
  (* pair (pair nat nat) nat *)
  pair_ty pair_nat_nat_ty nat_ty >>??= fun pair_pair_nat_nat_nat_ty ->
  test_parse_ty
    ctxt
    (pair_prim2 pair_nat_nat_prim nat_prim)
    pair_pair_nat_nat_nat_ty
  >>?= fun ctxt ->
  (* pair nat (pair nat nat) *)
  pair_ty nat_ty pair_nat_nat_ty >>??= fun pair_nat_pair_nat_nat_ty ->
  test_parse_ty
    ctxt
    (pair_prim2 nat_prim pair_nat_nat_prim)
    pair_nat_pair_nat_nat_ty
  >>?= fun ctxt ->
  (* pair nat nat nat *)
  pair_ty nat_ty pair_nat_nat_ty >>??= fun pair_nat_nat_nat_ty ->
  test_parse_ty
    ctxt
    (pair_prim [nat_prim; nat_prim; nat_prim])
    pair_nat_nat_nat_ty
  >>?= fun ctxt ->
  (* pair (nat %a) nat *)
  pair_t (-1) (nat_ty, Some (field_annot "a")) (nat_ty, None) ~annot:None
  >>??= fun pair_nat_a_nat_ty ->
  test_parse_ty ctxt (pair_prim2 nat_prim_a nat_prim) pair_nat_a_nat_ty
  >>?= fun ctxt ->
  (* pair nat (nat %b) *)
  pair_t (-1) (nat_ty, None) (nat_ty, Some (field_annot "b")) ~annot:None
  >>??= fun pair_nat_nat_b_ty ->
  test_parse_ty ctxt (pair_prim2 nat_prim nat_prim_b) pair_nat_nat_b_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (nat %b) *)
  pair_t
    (-1)
    (nat_ty, Some (field_annot "a"))
    (nat_ty, Some (field_annot "b"))
    ~annot:None
  >>??= fun pair_nat_a_nat_b_ty ->
  test_parse_ty ctxt (pair_prim2 nat_prim_a nat_prim_b) pair_nat_a_nat_b_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (nat %b) (nat %c) *)
  pair_t
    (-1)
    (nat_ty, Some (field_annot "b"))
    (nat_ty, Some (field_annot "c"))
    ~annot:None
  >>??= fun pair_nat_b_nat_c_ty ->
  pair_t
    (-1)
    (nat_ty, Some (field_annot "a"))
    (pair_nat_b_nat_c_ty, None)
    ~annot:None
  >>??= fun pair_nat_a_nat_b_nat_c_ty ->
  test_parse_ty
    ctxt
    (pair_prim [nat_prim_a; nat_prim_b; nat_prim_c])
    pair_nat_a_nat_b_nat_c_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (pair %b nat nat) *)
  pair_t (-1) (nat_ty, None) (nat_ty, None) ~annot:None
  >>??= fun pair_b_nat_nat_ty ->
  pair_t
    (-1)
    (nat_ty, Some (field_annot "a"))
    (pair_b_nat_nat_ty, Some (field_annot "b"))
    ~annot:None
  >>??= fun pair_nat_a_pair_b_nat_nat_ty ->
  test_parse_ty
    ctxt
    (pair_prim2 nat_prim_a (Prim (-1, T_pair, [nat_prim; nat_prim], ["%b"])))
    pair_nat_a_pair_b_nat_nat_ty
  >>?= fun _ -> return_unit

let test_unparse_ty loc ctxt expected ty =
  Environment.wrap_tzresult
    ( Script_ir_translator.unparse_ty ~loc:() ctxt ty >>? fun (actual, ctxt) ->
      if actual = expected then ok ctxt
      else Alcotest.failf "Unexpected error: %s" loc )

let test_unparse_comb_type () =
  let open Script in
  let open Script_typed_ir in
  let nat_prim = Prim ((), T_nat, [], []) in
  let nat_prim_a = Prim ((), T_nat, [], ["%a"]) in
  let nat_prim_b = Prim ((), T_nat, [], ["%b"]) in
  let nat_prim_c = Prim ((), T_nat, [], ["%c"]) in
  let nat_ty = nat_t ~annot:None in
  let pair_prim l = Prim ((), T_pair, l, []) in
  let pair_ty ty1 ty2 = pair_t (-1) (ty1, None) (ty2, None) ~annot:None in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_nat_nat_prim = pair_prim2 nat_prim nat_prim in
  pair_ty nat_ty nat_ty >>??= fun pair_nat_nat_ty ->
  test_context () >>=? fun ctxt ->
  (* pair nat nat *)
  test_unparse_ty __LOC__ ctxt pair_nat_nat_prim pair_nat_nat_ty
  >>?= fun ctxt ->
  (* pair (pair nat nat) nat *)
  pair_ty pair_nat_nat_ty nat_ty >>??= fun pair_pair_nat_nat_nat_ty ->
  test_unparse_ty
    __LOC__
    ctxt
    (pair_prim2 pair_nat_nat_prim nat_prim)
    pair_pair_nat_nat_nat_ty
  >>?= fun ctxt ->
  (* pair nat nat nat *)
  pair_ty nat_ty pair_nat_nat_ty >>??= fun pair_nat_nat_nat_ty ->
  test_unparse_ty
    __LOC__
    ctxt
    (pair_prim [nat_prim; nat_prim; nat_prim])
    pair_nat_nat_nat_ty
  >>?= fun ctxt ->
  (* pair (nat %a) nat *)
  pair_t (-1) (nat_ty, Some (field_annot "a")) (nat_ty, None) ~annot:None
  >>??= fun pair_nat_a_nat_ty ->
  test_unparse_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim_a nat_prim)
    pair_nat_a_nat_ty
  >>?= fun ctxt ->
  (* pair nat (nat %b) *)
  pair_t (-1) (nat_ty, None) (nat_ty, Some (field_annot "b")) ~annot:None
  >>??= fun pair_nat_nat_b_ty ->
  test_unparse_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim nat_prim_b)
    pair_nat_nat_b_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (nat %b) *)
  pair_t
    (-1)
    (nat_ty, Some (field_annot "a"))
    (nat_ty, Some (field_annot "b"))
    ~annot:None
  >>??= fun pair_nat_a_nat_b_ty ->
  test_unparse_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim_a nat_prim_b)
    pair_nat_a_nat_b_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (nat %b) (nat %c) *)
  pair_t
    (-1)
    (nat_ty, Some (field_annot "b"))
    (nat_ty, Some (field_annot "c"))
    ~annot:None
  >>??= fun pair_nat_b_nat_c_ty ->
  pair_t
    (-1)
    (nat_ty, Some (field_annot "a"))
    (pair_nat_b_nat_c_ty, None)
    ~annot:None
  >>??= fun pair_nat_a_nat_b_nat_c_ty ->
  test_unparse_ty
    __LOC__
    ctxt
    (pair_prim [nat_prim_a; nat_prim_b; nat_prim_c])
    pair_nat_a_nat_b_nat_c_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (pair %b nat nat) *)
  pair_t (-1) (nat_ty, None) (nat_ty, None) ~annot:None
  >>??= fun pair_nat_nat_ty ->
  pair_t
    (-1)
    (nat_ty, Some (field_annot "a"))
    (pair_nat_nat_ty, Some (field_annot "b"))
    ~annot:None
  >>??= fun pair_nat_a_pair_b_nat_nat_ty ->
  test_unparse_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim_a (Prim ((), T_pair, [nat_prim; nat_prim], ["%b"])))
    pair_nat_a_pair_b_nat_nat_ty
  >>?= fun ctxt ->
  (* pair nat (pair :b nat nat) *)
  pair_t (-1) (nat_ty, None) (nat_ty, None) ~annot:(Some (type_annot "b"))
  >>??= fun pair_b_nat_nat_ty ->
  pair_t (-1) (nat_ty, None) (pair_b_nat_nat_ty, None) ~annot:None
  >>??= fun pair_nat_pair_b_nat_nat_ty ->
  test_unparse_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim (Prim ((), T_pair, [nat_prim; nat_prim], [":b"])))
    pair_nat_pair_b_nat_nat_ty
  >>?= fun _ -> return_unit

let test_unparse_comparable_ty loc ctxt expected ty =
  (* unparse_comparable_ty is not exported, the simplest way to call it is to
     call parse_ty on a set type *)
  let open Script_typed_ir in
  Environment.wrap_tzresult
    ( set_t (-1) ty ~annot:None >>? fun set_ty_ty ->
      Script_ir_translator.unparse_ty ~loc:() ctxt set_ty_ty
      >>? fun (actual, ctxt) ->
      if actual = Prim ((), T_set, [expected], []) then ok ctxt
      else Alcotest.failf "Unexpected error: %s" loc )

let test_unparse_comb_comparable_type () =
  let open Script in
  let open Script_typed_ir in
  let nat_prim = Prim ((), T_nat, [], []) in
  let nat_prim_a = Prim ((), T_nat, [], ["%a"]) in
  let nat_prim_b = Prim ((), T_nat, [], ["%b"]) in
  let nat_prim_c = Prim ((), T_nat, [], ["%c"]) in
  let nat_ty = nat_key ~annot:None in
  let pair_prim l = Prim ((), T_pair, l, []) in
  let pair_ty ty1 ty2 = pair_key (-1) (ty1, None) (ty2, None) ~annot:None in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_nat_nat_prim = pair_prim2 nat_prim nat_prim in
  pair_ty nat_ty nat_ty >>??= fun pair_nat_nat_ty ->
  test_context () >>=? fun ctxt ->
  (* pair nat nat *)
  test_unparse_comparable_ty __LOC__ ctxt pair_nat_nat_prim pair_nat_nat_ty
  >>?= fun ctxt ->
  (* pair (pair nat nat) nat *)
  pair_ty pair_nat_nat_ty nat_ty >>??= fun pair_pair_nat_nat_nat_ty ->
  test_unparse_comparable_ty
    __LOC__
    ctxt
    (pair_prim2 pair_nat_nat_prim nat_prim)
    pair_pair_nat_nat_nat_ty
  >>?= fun ctxt ->
  (* pair nat nat nat *)
  pair_ty nat_ty pair_nat_nat_ty >>??= fun pair_nat_nat_nat_ty ->
  test_unparse_comparable_ty
    __LOC__
    ctxt
    (pair_prim [nat_prim; nat_prim; nat_prim])
    pair_nat_nat_nat_ty
  >>?= fun ctxt ->
  (* pair (nat %a) nat *)
  pair_key (-1) (nat_ty, Some (field_annot "a")) (nat_ty, None) ~annot:None
  >>??= fun pair_nat_a_nat_ty ->
  test_unparse_comparable_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim_a nat_prim)
    pair_nat_a_nat_ty
  >>?= fun ctxt ->
  (* pair nat (nat %b) *)
  pair_key (-1) (nat_ty, None) (nat_ty, Some (field_annot "b")) ~annot:None
  >>??= fun pair_nat_nat_b_ty ->
  test_unparse_comparable_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim nat_prim_b)
    pair_nat_nat_b_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (nat %b) *)
  pair_key
    (-1)
    (nat_ty, Some (field_annot "a"))
    (nat_ty, Some (field_annot "b"))
    ~annot:None
  >>??= fun pair_nat_a_nat_b_ty ->
  test_unparse_comparable_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim_a nat_prim_b)
    pair_nat_a_nat_b_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (nat %b) (nat %c) *)
  pair_key
    (-1)
    (nat_ty, Some (field_annot "b"))
    (nat_ty, Some (field_annot "c"))
    ~annot:None
  >>??= fun pair_nat_b_nat_c_ty ->
  pair_key
    (-1)
    (nat_ty, Some (field_annot "a"))
    (pair_nat_b_nat_c_ty, None)
    ~annot:None
  >>??= fun pair_nat_a_nat_b_nat_c_ty ->
  test_unparse_comparable_ty
    __LOC__
    ctxt
    (pair_prim [nat_prim_a; nat_prim_b; nat_prim_c])
    pair_nat_a_nat_b_nat_c_ty
  >>?= fun ctxt ->
  (* pair (nat %a) (pair %b nat nat) *)
  pair_key
    (-1)
    (nat_ty, Some (field_annot "a"))
    (pair_nat_nat_ty, Some (field_annot "b"))
    ~annot:None
  >>??= fun pair_nat_a_pair_b_nat_nat_ty ->
  test_unparse_comparable_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim_a (Prim ((), T_pair, [nat_prim; nat_prim], ["%b"])))
    pair_nat_a_pair_b_nat_nat_ty
  >>?= fun ctxt ->
  (* pair nat (pair :b nat nat) *)
  pair_key (-1) (nat_ty, None) (nat_ty, None) ~annot:(Some (type_annot "b"))
  >>??= fun pair_b_nat_nat_ty ->
  pair_key (-1) (nat_ty, None) (pair_b_nat_nat_ty, None) ~annot:None
  >>??= fun pair_nat_pair_b_nat_nat_ty ->
  test_unparse_comparable_ty
    __LOC__
    ctxt
    (pair_prim2 nat_prim (Prim ((), T_pair, [nat_prim; nat_prim], [":b"])))
    pair_nat_pair_b_nat_nat_ty
  >>?= fun _ -> return_unit

let test_parse_data ?(equal = Stdlib.( = )) loc ctxt ty node expected =
  let legacy = false in
  let allow_forged = true in
  wrap_error_lwt
    ( Script_ir_translator.parse_data ctxt ~legacy ~allow_forged ty node
    >>=? fun (actual, ctxt) ->
      if equal actual expected then return ctxt
      else Alcotest.failf "Unexpected error: %s" loc )

let test_parse_data_fails loc ctxt ty node =
  let legacy = false in
  let allow_forged = false in
  wrap_error_lwt
    (Script_ir_translator.parse_data ctxt ~legacy ~allow_forged ty node
     >>= function
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
             return_unit)

let test_parse_comb_data () =
  let open Script in
  let open Script_typed_ir in
  let z = Script_int.zero_n in
  let z_prim = Micheline.Int (-1, Z.zero) in
  let nat_ty = nat_t ~annot:None in
  let pair_prim l = Prim (-1, D_Pair, l, []) in
  let pair_ty ty1 ty2 = pair_t (-1) (ty1, None) (ty2, None) ~annot:None in
  pair_ty nat_ty nat_ty >>??= fun pair_nat_nat_ty ->
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_z_z_prim = pair_prim2 z_prim z_prim in
  list_t (-1) nat_ty ~annot:None >>??= fun list_nat_ty ->
  big_map_t (-1) (nat_key ~annot:None) nat_ty ~annot:None
  >>??= fun big_map_nat_nat_ty ->
  test_context_with_nat_nat_big_map () >>=? fun (ctxt, big_map_id) ->
  (* Pair 0 0 *)
  test_parse_data __LOC__ ctxt pair_nat_nat_ty pair_z_z_prim (z, z)
  >>=? fun ctxt ->
  (* {0; 0} *)
  test_parse_data
    __LOC__
    ctxt
    pair_nat_nat_ty
    (Micheline.Seq (-1, [z_prim; z_prim]))
    (z, z)
  >>=? fun ctxt ->
  (* Pair (Pair 0 0) 0 *)
  pair_ty pair_nat_nat_ty nat_ty >>??= fun pair_pair_nat_nat_nat_ty ->
  test_parse_data
    __LOC__
    ctxt
    pair_pair_nat_nat_nat_ty
    (pair_prim2 pair_z_z_prim z_prim)
    ((z, z), z)
  >>=? fun ctxt ->
  (* Pair 0 (Pair 0 0) *)
  pair_ty nat_ty pair_nat_nat_ty >>??= fun pair_nat_pair_nat_nat_ty ->
  test_parse_data
    __LOC__
    ctxt
    pair_nat_pair_nat_nat_ty
    (pair_prim2 z_prim pair_z_z_prim)
    (z, (z, z))
  >>=? fun ctxt ->
  (* Pair 0 0 0 *)
  test_parse_data
    __LOC__
    ctxt
    pair_nat_pair_nat_nat_ty
    (pair_prim [z_prim; z_prim; z_prim])
    (z, (z, z))
  >>=? fun ctxt ->
  (* {0; 0; 0} *)
  test_parse_data
    __LOC__
    ctxt
    pair_nat_pair_nat_nat_ty
    (Micheline.Seq (-1, [z_prim; z_prim; z_prim]))
    (z, (z, z))
  >>=? fun ctxt ->
  (* Should fail: {0} against pair nat (list nat) *)
  pair_ty nat_ty list_nat_ty >>??= fun pair_nat_list_nat_ty ->
  test_parse_data_fails
    __LOC__
    ctxt
    pair_nat_list_nat_ty
    (Micheline.Seq (-1, [z_prim]))
  >>=? fun () ->
  (* Should fail: {0; 0; 0} against pair nat (list nat) *)
  test_parse_data_fails
    __LOC__
    ctxt
    pair_nat_list_nat_ty
    (Micheline.Seq (-1, [z_prim; z_prim; z_prim]))
  >>=? fun () ->
  (* check Pair 0 (Pair 0 {}) against pair nat (big_map nat nat)
     so that the following test fails for the good reason and not because
     the big map doesn't exist
  *)
  let id_z = Big_map.Id.unparse_to_z big_map_id in
  let id_prim = Int (-1, id_z) in
  let expected_big_map =
    let open Script_typed_ir in
    let diff = {map = Big_map_overlay.empty; size = 0} in
    let nat_key_ty = nat_key ~annot:None in
    {id = Some big_map_id; diff; key_type = nat_key_ty; value_type = nat_ty}
  in
  let equal (nat1, big_map1) (nat2, big_map2) =
    (* Custom equal needed because big maps contain boxed maps containing functional values *)
    nat1 = nat2 && big_map1.id = big_map2.id
    && big_map1.key_type = big_map2.key_type
    && big_map1.value_type = big_map2.value_type
    && big_map1.diff.size = big_map2.diff.size
    && Big_map_overlay.bindings big_map1.diff.map
       = Big_map_overlay.bindings big_map2.diff.map
  in
  pair_ty nat_ty big_map_nat_nat_ty >>??= fun pair_nat_big_map_nat_nat_ty ->
  test_parse_data
    ~equal
    __LOC__
    ctxt
    pair_nat_big_map_nat_nat_ty
    (pair_prim2 z_prim (pair_prim2 id_prim (Seq (-1, []))))
    (Script_int.zero_n, expected_big_map)
  >>=? fun ctxt ->
  (* Should fail: Pair 0 0 {} against pair nat (big_map nat nat) *)
  test_parse_data_fails
    __LOC__
    ctxt
    pair_nat_big_map_nat_nat_ty
    (pair_prim [z_prim; id_prim; Seq (-1, [])])

let test_parse_address () =
  let open Script_typed_ir in
  test_context_with_nat_nat_big_map () >>=? fun (ctxt, _big_map_id) ->
  (* KT1% (empty entrypoint) *)
  wrap_error_lwt
    (Lwt.return (Contract.of_b58check "KT1FAKEFAKEFAKEFAKEFAKEFAKEFAKGGSE2x"))
  >>=? fun kt1fake ->
  test_parse_data
    __LOC__
    ctxt
    (address_t ~annot:None)
    (String (-1, "KT1FAKEFAKEFAKEFAKEFAKEFAKEFAKGGSE2x%"))
    {contract = kt1fake; entrypoint = Entrypoint.default}
  >>=? fun ctxt ->
  (* tz1% (empty entrypoint) *)
  wrap_error_lwt
    (Lwt.return (Contract.of_b58check "tz1fakefakefakefakefakefakefakcphLA5"))
  >>=? fun tz1fake ->
  test_parse_data
    __LOC__
    ctxt
    (address_t ~annot:None)
    (String (-1, "tz1fakefakefakefakefakefakefakcphLA5%"))
    {contract = tz1fake; entrypoint = Entrypoint.default}
  >|=? fun _ctxt -> ()

let test_unparse_data loc ctxt ty x ~expected_readable ~expected_optimized =
  wrap_error_lwt
    ( Script_ir_translator.unparse_data ctxt Script_ir_translator.Readable ty x
    >>=? fun (actual_readable, ctxt) ->
      (if actual_readable = expected_readable then return ctxt
      else Alcotest.failf "Error in readable unparsing: %s" loc)
      >>=? fun ctxt ->
      Script_ir_translator.unparse_data ctxt Script_ir_translator.Optimized ty x
      >>=? fun (actual_optimized, ctxt) ->
      if actual_optimized = expected_optimized then return ctxt
      else Alcotest.failf "Error in optimized unparsing: %s" loc )

let test_unparse_comb_data () =
  let open Script in
  let open Script_typed_ir in
  let z = Script_int.zero_n in
  let z_prim = Micheline.Int (-1, Z.zero) in
  let nat_ty = nat_t ~annot:None in
  let pair_prim l = Prim (-1, D_Pair, l, []) in
  let pair_ty ty1 ty2 = pair_t (-1) (ty1, None) (ty2, None) ~annot:None in
  pair_ty nat_ty nat_ty >>??= fun pair_nat_nat_ty ->
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_z_z_prim = pair_prim2 z_prim z_prim in
  test_context () >>=? fun ctxt ->
  (* Pair 0 0 *)
  test_unparse_data
    __LOC__
    ctxt
    pair_nat_nat_ty
    (z, z)
    ~expected_readable:pair_z_z_prim
    ~expected_optimized:pair_z_z_prim
  >>=? fun ctxt ->
  (* Pair (Pair 0 0) 0 *)
  pair_ty pair_nat_nat_ty nat_ty >>??= fun pair_pair_nat_nat_nat_ty ->
  test_unparse_data
    __LOC__
    ctxt
    pair_pair_nat_nat_nat_ty
    ((z, z), z)
    ~expected_readable:(pair_prim2 pair_z_z_prim z_prim)
    ~expected_optimized:(pair_prim2 pair_z_z_prim z_prim)
  >>=? fun ctxt ->
  (* Readable: Pair 0 0 0; Optimized: Pair 0 (Pair 0 0) *)
  pair_ty nat_ty pair_nat_nat_ty >>??= fun pair_nat_pair_nat_nat_ty ->
  test_unparse_data
    __LOC__
    ctxt
    pair_nat_pair_nat_nat_ty
    (z, (z, z))
    ~expected_readable:(pair_prim [z_prim; z_prim; z_prim])
    ~expected_optimized:(pair_prim2 z_prim pair_z_z_prim)
  >>=? fun ctxt ->
  (* Readable: Pair 0 0 0 0; Optimized: {0; 0; 0; 0} *)
  pair_ty nat_ty pair_nat_pair_nat_nat_ty
  >>??= fun pair_nat_pair_nat_pair_nat_nat_ty ->
  test_unparse_data
    __LOC__
    ctxt
    pair_nat_pair_nat_pair_nat_nat_ty
    (z, (z, (z, z)))
    ~expected_readable:(pair_prim [z_prim; z_prim; z_prim; z_prim])
    ~expected_optimized:(Micheline.Seq (-1, [z_prim; z_prim; z_prim; z_prim]))
  >>=? fun _ -> return_unit

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
  let open Script_typed_ir in
  let leaf_ty = nat_t ~annot:None in
  let leaf_mich = Int ((), Z.zero) in
  let leaf_v = Script_int.zero_n in
  let size_of_micheline mich =
    let canonical = Micheline.strip_locations mich in
    ( canonical,
      Bytes.length
      @@ Data_encoding.Binary.to_bytes_exn Script.expr_encoding canonical )
  in
  let check_optimal_comb loc ctxt ty v arity =
    wrap_error_lwt
      ( Script_ir_translator.unparse_data
          ctxt
          Script_ir_translator.Optimized
          ty
          v
      >>=? fun (unparsed, ctxt) ->
        let (unparsed_canonical, unparsed_size) = size_of_micheline unparsed in
        List.iter_es (fun other_repr ->
            let (other_repr_canonical, other_repr_size) =
              size_of_micheline other_repr
            in
            if other_repr_size < unparsed_size then
              Alcotest.failf
                "At %s, for comb of arity %d, representation %a (size %d \
                 bytes) is shorter than representation %a (size %d bytes) \
                 returned by unparse_data in Optimized mode"
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
        >>=? fun () -> return ctxt )
  in
  let pair_ty ty1 ty2 = pair_t (-1) (ty1, None) (ty2, None) ~annot:None in
  test_context () >>=? fun ctxt ->
  pair_ty leaf_ty leaf_ty >>??= fun comb2_ty ->
  let comb2_v = (leaf_v, leaf_v) in
  check_optimal_comb __LOC__ ctxt comb2_ty comb2_v 2 >>=? fun ctxt ->
  pair_ty leaf_ty comb2_ty >>??= fun comb3_ty ->
  let comb3_v = (leaf_v, comb2_v) in
  check_optimal_comb __LOC__ ctxt comb3_ty comb3_v 3 >>=? fun ctxt ->
  pair_ty leaf_ty comb3_ty >>??= fun comb4_ty ->
  let comb4_v = (leaf_v, comb3_v) in
  check_optimal_comb __LOC__ ctxt comb4_ty comb4_v 4 >>=? fun ctxt ->
  pair_ty leaf_ty comb4_ty >>??= fun comb5_ty ->
  let comb5_v = (leaf_v, comb4_v) in
  check_optimal_comb __LOC__ ctxt comb5_ty comb5_v 5 >>=? fun _ctxt ->
  return_unit

(* Check that UNPACK on contract is forbidden.
   See https://gitlab.com/tezos/tezos/-/issues/301 for the motivation
   behind this restriction.
  *)
let test_contract_not_packable () =
  let contract_unit =
    Prim (0, Script.T_contract, [Prim (0, T_unit, [], [])], [])
  in
  test_context () >>=? fun ctxt ->
  (* Test that [contract_unit] is parsable *)
  (match Script_ir_translator.parse_any_ty ctxt ~legacy:false contract_unit with
  | Ok _ -> return_unit
  | Error _ -> Alcotest.failf "Could not parse (contract unit)")
  >>=? fun () ->
  (* Test that [contract_unit] is not packable *)
  (match
     Script_ir_translator.parse_packable_ty ctxt ~legacy:false contract_unit
   with
  | Ok _ ->
      Alcotest.failf
        "(contract unit) should not be packable, see \
         https://gitlab.com/tezos/tezos/-/issues/301"
  | Error _ -> return_unit)
  >>=? fun () ->
  (* Test that elaboration of the [UNPACK unit] instruction succeeds *)
  (Script_ir_translator.parse_instr
     Script_tc_context.data
     ctxt
     ~legacy:false
     (Prim (0, I_UNPACK, [Prim (0, T_unit, [], [])], []))
     (Item_t (Script_typed_ir.bytes_t ~annot:None, Bot_t))
   >>= function
   | Ok _ -> return_unit
   | Error _ -> Alcotest.failf "Could not parse UNPACK unit")
  >>=? fun () ->
  (* Test that elaboration of the [UNPACK (contract unit)] instruction fails *)
  Script_ir_translator.parse_instr
    Script_tc_context.data
    ctxt
    ~legacy:false
    (Prim (0, I_UNPACK, [contract_unit], []))
    (Item_t (Script_typed_ir.bytes_t ~annot:None, Bot_t))
  >>= function
  | Ok _ ->
      Alcotest.failf
        "UNPACK (contract unit) should not be allowed, see \
         https://gitlab.com/tezos/tezos/-/issues/301"
  | Error _ -> return_unit

(* This test function is used to checks forbidden operations in views. *)
let test_forbidden_op_in_view op () =
  let prefix = "./contracts/forbidden_op_in_view_" in
  let script = read_file (prefix ^ op ^ ".tz") in
  let contract_expr = Expr.from_string script in
  test_context () >>=? fun ctxt ->
  Script_ir_translator.typecheck_code
    ~legacy:false
    ~show_types:false
    ctxt
    contract_expr
  >>= function
  | Ok _ ->
      Alcotest.failf
        "%s should not be allowed in views, see \
         https://gitlab.com/tezos/tezos/-/issues/1922"
        op
  | Error _ -> return_unit

let tests =
  [
    Tztest.tztest "test unparse view" `Quick test_unparse_view;
    Tztest.tztest
      "test typecheck stack overflow error"
      `Quick
      test_typecheck_stack_overflow;
    Tztest.tztest "test comb type parsing" `Quick test_parse_comb_type;
    Tztest.tztest "test comb type unparsing" `Quick test_unparse_comb_type;
    Tztest.tztest
      "test comb comparable type unparsing"
      `Quick
      test_unparse_comb_comparable_type;
    Tztest.tztest "test comb data parsing" `Quick test_parse_comb_data;
    Tztest.tztest "test comb data unparsing" `Quick test_unparse_comb_data;
    Tztest.tztest "test optimal comb data unparsing" `Quick test_optimal_comb;
    Tztest.tztest "test parse address" `Quick test_parse_address;
    Tztest.tztest
      "test unpackability of the contract type"
      `Quick
      test_contract_not_packable;
    Tztest.tztest
      "test forbidden SELF in view"
      `Quick
      (test_forbidden_op_in_view "SELF");
    Tztest.tztest
      "test forbidden SET_DELEGATE in view"
      `Quick
      (test_forbidden_op_in_view "SET_DELEGATE");
    Tztest.tztest
      "test forbidden TRANSFER_TOKENS in view"
      `Quick
      (test_forbidden_op_in_view "TRANSFER_TOKENS");
    Tztest.tztest
      "test forbidden CREATE_CONTRACT in view"
      `Quick
      (test_forbidden_op_in_view "CREATE_CONTRACT");
  ]
