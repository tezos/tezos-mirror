open Protocol
open Alpha_context
open Script_interpreter
open Micheline

exception Expression_from_string

let expression_from_string str : Script.expr tzresult Lwt.t =
  let (ast, errs) = Michelson_v1_parser.parse_expression ~check:false str in
  ( match errs with
  | [] ->
      ()
  | lst ->
      Format.printf "expr_from_string: %a\n" Error_monad.pp_print_error lst ;
      raise Expression_from_string ) ;
  return ast.expanded

let ( >>=?? ) x y =
  x
  >>= function
  | Ok s ->
      y s
  | Error errs ->
      Lwt.return
      @@ Error (List.map (fun x -> Environment.Ecoproto_error x) errs)

let test_context () =
  Context.init 3
  >>=? fun (b, _cs) ->
  Incremental.begin_construction b
  >>=? fun v -> return (Incremental.alpha_ctxt v)

let default_source = Contract.implicit_contract Signature.Public_key_hash.zero

let default_step_constants =
  {
    source = default_source;
    payer = default_source;
    self = default_source;
    amount = Tez.zero;
    chain_id = Chain_id.zero;
  }

(** Helper function that parses and types a script, its initial storage and
   parameters from strings. It then executes the typed script with the storage
   and parameter and returns the result. *)
let run_script ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = "default") ~storage ~parameter () =
  expression_from_string contract
  >>=? fun contract_expr ->
  expression_from_string storage
  >>=? fun storage_expr ->
  expression_from_string parameter
  >>=? fun parameter_expr ->
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  Script_interpreter.execute
    ctx
    Readable
    step_constants
    ~script
    ~entrypoint
    ~parameter:parameter_expr
  >>=?? fun res -> return res

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

(* Check that the custom stack overflow exception is triggered when it should be *)
let test_typecheck_stack_overflow () =
  test_context ()
  >>=? fun ctxt ->
  let storage = "Unit" in
  let parameter = "Unit" in
  let script = read_file "./contracts/big_interpreter_stack.tz" in
  run_script ctxt script ~storage ~parameter ()
  >>= function
  | Ok _ ->
      Alcotest.fail "expected an error"
  | Error lst
    when List.mem
           (Environment.Ecoproto_error
              Script_tc_errors.Typechecking_too_many_recursive_calls)
           lst ->
      return ()
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

let test_unparse_stack_overflow () =
  test_context ()
  >>=? fun ctxt ->
  (* Meme *)
  let enorme_et_seq n =
    let rec aux n acc = aux (n - 1) @@ Micheline.Seq (0, [acc]) in
    aux n (Micheline.Int (0, Z.zero))
  in
  Script_ir_translator.(unparse_code ctxt Readable (enorme_et_seq 10_001))
  >>= function
  | Ok _ ->
      Alcotest.fail "expected an error"
  | Error lst
    when List.mem Script_tc_errors.Unparsing_too_many_recursive_calls lst ->
      return ()
  | Error _ ->
      Alcotest.failf "Unexpected error: %s" __LOC__

let wrap_error_lwt x = x >>= fun x -> Lwt.return @@ Environment.wrap_error x

let test_parse_data loc ctxt ty node expected =
  let legacy = false in
  wrap_error_lwt
    ( Script_ir_translator.parse_data ctxt ~legacy ty node
    >>=? fun (actual, ctxt) ->
    if actual = expected then return ctxt
    else Alcotest.failf "Unexpected error: %s" loc )

let test_parse_data_fails loc ctxt ty node =
  let legacy = false in
  wrap_error_lwt
    ( Script_ir_translator.parse_data ctxt ~legacy ty node
    >>= function
    | Ok _ ->
        Alcotest.failf "Unexpected typechecking success: %s" loc
    | Error (Tezos_raw_protocol_alpha.Script_tc_errors.Invalid_constant _ :: _)
      ->
        return_unit
    | Error _ as res ->
        Lwt.return res )

let test_parse_comb_data () =
  let open Script in
  let open Script_typed_ir in
  let z = Script_int.zero_n in
  let z_prim = Micheline.Int (-1, Z.zero) in
  let nat_ty = Nat_t None in
  let pair_prim l = Prim (-1, D_Pair, l, []) in
  let pair_ty ty1 ty2 = Pair_t ((ty1, None, None), (ty2, None, None), None) in
  let pair_nat_nat_ty = pair_ty nat_ty nat_ty in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_z_z_prim = pair_prim2 z_prim z_prim in
  test_context ()
  >>=? fun ctxt ->
  (* Pair 0 0 *)
  test_parse_data __LOC__ ctxt (pair_ty nat_ty nat_ty) pair_z_z_prim (z, z)
  >>=? fun ctxt ->
  (* Pair (Pair 0 0) 0 *)
  test_parse_data
    __LOC__
    ctxt
    (pair_ty pair_nat_nat_ty nat_ty)
    (pair_prim2 pair_z_z_prim z_prim)
    ((z, z), z)
  >>=? fun ctxt ->
  (* Pair 0 (Pair 0 0) *)
  test_parse_data
    __LOC__
    ctxt
    (pair_ty nat_ty pair_nat_nat_ty)
    (pair_prim2 z_prim pair_z_z_prim)
    (z, (z, z))
  >>=? fun ctxt ->
  (* Pair 0 0 0 *)
  test_parse_data
    __LOC__
    ctxt
    (pair_ty nat_ty pair_nat_nat_ty)
    (pair_prim [z_prim; z_prim; z_prim])
    (z, (z, z))
  >>=? fun ctxt ->
  (* Should fail: Pair 0 0 {} against pair nat (big_map nat nat) *)
  test_parse_data_fails
    __LOC__
    ctxt
    (pair_ty nat_ty big_map_nat_nat_ty)
    (pair_prim [z_prim; z_prim; Micheline.Seq (-1, [])])

let test_unparse_data loc ctxt ty x ~expected_readable ~expected_optimized =
  wrap_error_lwt
    ( Script_ir_translator.unparse_data ctxt Script_ir_translator.Readable ty x
    >>=? fun (actual_readable, ctxt) ->
    ( if actual_readable = expected_readable then return ctxt
    else Alcotest.failf "Error in readable unparsing: %s" loc )
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
  let nat_ty = Nat_t None in
  let pair_prim l = Prim (-1, D_Pair, l, []) in
  let pair_ty ty1 ty2 = Pair_t ((ty1, None, None), (ty2, None, None), None) in
  let pair_nat_nat_ty = pair_ty nat_ty nat_ty in
  let pair_prim2 a b = pair_prim [a; b] in
  let pair_z_z_prim = pair_prim2 z_prim z_prim in
  test_context ()
  >>=? fun ctxt ->
  (* Pair 0 0 *)
  test_unparse_data
    __LOC__
    ctxt
    (pair_ty nat_ty nat_ty)
    (z, z)
    ~expected_readable:pair_z_z_prim
    ~expected_optimized:pair_z_z_prim
  >>=? fun ctxt ->
  (* Pair (Pair 0 0) 0 *)
  test_unparse_data
    __LOC__
    ctxt
    (pair_ty pair_nat_nat_ty nat_ty)
    ((z, z), z)
    ~expected_readable:(pair_prim2 pair_z_z_prim z_prim)
    ~expected_optimized:(pair_prim2 pair_z_z_prim z_prim)
  >>=? fun ctxt ->
  (* Readable: Pair 0 0 0; Optimized: Pair 0 (Pair 0 0) *)
  test_unparse_data
    __LOC__
    ctxt
    (pair_ty nat_ty pair_nat_nat_ty)
    (z, (z, z))
    ~expected_readable:(pair_prim [z_prim; z_prim; z_prim])
    ~expected_optimized:(pair_prim2 z_prim pair_z_z_prim)
  >>=? fun _ -> return_unit

let tests =
  [ Test.tztest
      "test typecheck stack overflow error"
      `Quick
      test_typecheck_stack_overflow;
    Test.tztest
      "test unparsing stack overflow error"
      `Quick
      test_typecheck_stack_overflow;
    Test.tztest "test comb data parsing" `Quick test_parse_comb_data;
    Test.tztest "test comb data unparsing" `Quick test_unparse_comb_data ]
