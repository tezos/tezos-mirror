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

(** Runs a script with an ill-typed parameter and verifies that a
   Bad_contract_parameter error is returned *)
let test_bad_contract_parameter () =
  test_context ()
  >>=? fun ctx ->
  (* Run script with a parameter of wrong type *)
  run_script
    ctx
    "{parameter unit; storage unit; code { CAR; NIL operation; PAIR }}"
    ~storage:"Unit"
    ~parameter:"0"
    ()
  >>= function
  | Ok _ ->
      Alcotest.fail "expected an error"
  | Error (Environment.Ecoproto_error (Bad_contract_parameter source') :: _) ->
      Test_services.(check Testable.contract)
        "incorrect field in Bad_contract_parameter"
        default_source
        source' ;
      return ()
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

(** Test the encoding/decoding of script_interpreter.ml specific errors *)

let test_json_roundtrip name testable enc v =
  let v' =
    Data_encoding.Json.destruct enc (Data_encoding.Json.construct enc v)
  in
  Alcotest.check
    testable
    (Format.asprintf "round trip should not change value of %s" name)
    v
    v' ;
  return_unit

let test_json_roundtrip_err name e () =
  test_json_roundtrip
    name
    Testable.protocol_error
    Environment.Error_monad.error_encoding
    e

let error_encoding_tests =
  let contract_zero =
    Contract.implicit_contract Signature.Public_key_hash.zero
  in
  let script_expr_int =
    Micheline.strip_locations (Micheline.Int (0, Z.zero))
  in
  List.map
    (fun (name, e) ->
      Test.tztest
        (Format.asprintf "test error encoding: %s" name)
        `Quick
        (test_json_roundtrip_err name e))
    [ ("Reject", Reject (0, script_expr_int, None));
      ("Overflow", Overflow (0, None));
      ( "Runtime_contract_error",
        Runtime_contract_error (contract_zero, script_expr_int) );
      ("Bad_contract_parameter", Bad_contract_parameter contract_zero);
      ("Cannot_serialize_log", Cannot_serialize_log);
      ("Cannot_serialize_failure", Cannot_serialize_failure);
      ("Cannot_serialize_storage", Cannot_serialize_storage) ]

let tests =
  [Test.tztest "test bad contract error" `Quick test_bad_contract_parameter]
  @ error_encoding_tests
