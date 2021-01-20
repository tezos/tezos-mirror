open Protocol
open Alpha_context
open Script_interpreter

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

let tests =
  [ Test_services.tztest
      "test typecheck stack overflow error"
      `Quick
      test_typecheck_stack_overflow;
    Test_services.tztest
      "test unparsing stack overflow error"
      `Quick
      test_typecheck_stack_overflow ]
