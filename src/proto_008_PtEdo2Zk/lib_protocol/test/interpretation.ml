open Protocol
open Alpha_context
open Script_interpreter

let ( >>=?? ) x y =
  x >>= function
  | Ok s -> y s
  | Error errs ->
      Lwt.return
      @@ Error (List.map (fun x -> Environment.Ecoproto_error x) errs)

let test_context () =
  Context.init 3 >>=? fun (b, _cs) ->
  Incremental.begin_construction b >>=? fun v ->
  return (Incremental.alpha_ctxt v)

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
  let contract_expr = Expr.from_string contract in
  let storage_expr = Expr.from_string storage in
  let parameter_expr = Expr.from_string parameter in
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
    ~internal:false
  >>=?? fun res -> return res

module Logger : STEP_LOGGER = struct
  let log_interp _ctxt _descr _stack = ()

  let log_entry _ctxt _descr _stack = ()

  let log_exit _ctxt _descr _stack = ()

  let get_log () = Lwt.return (Ok None)
end

let run_step ctxt code param =
  Script_interpreter.step (module Logger) ctxt default_step_constants code param

(** Runs a script with an ill-typed parameter and verifies that a
   Bad_contract_parameter error is returned *)
let test_bad_contract_parameter () =
  test_context () >>=? fun ctx ->
  (* Run script with a parameter of wrong type *)
  run_script
    ctx
    "{parameter unit; storage unit; code { CAR; NIL operation; PAIR }}"
    ~storage:"Unit"
    ~parameter:"0"
    ()
  >>= function
  | Ok _ -> Alcotest.fail "expected an error"
  | Error (Environment.Ecoproto_error (Bad_contract_parameter source') :: _) ->
      Test_services.(check Testable.contract)
        "incorrect field in Bad_contract_parameter"
        default_source
        source' ;
      return_unit
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  s

(* Check that too many recursive calls of the Michelson interpreter result in an error *)
let test_stack_overflow () =
  test_context () >>=? fun ctxt ->
  let descr instr =
    Script_typed_ir.{loc = 0; bef = Empty_t; aft = Empty_t; instr}
  in
  let enorme_et_seq n =
    let rec aux n acc =
      if n = 0 then acc else aux (n - 1) (descr (Seq (acc, descr Nop)))
    in
    aux n (descr Nop)
  in
  run_step ctxt (enorme_et_seq 10_001) () >>= function
  | Ok _ -> Alcotest.fail "expected an error"
  | Error lst
    when List.mem
           ~equal:( = )
           Script_interpreter.Michelson_too_many_recursive_calls
           lst ->
      return ()
  | Error _ -> Alcotest.failf "Unexpected error (%s)" __LOC__

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
  let script_expr_int = Micheline.strip_locations (Micheline.Int (0, Z.zero)) in
  List.map
    (fun (name, e) ->
      Test_services.tztest
        (Format.asprintf "test error encoding: %s" name)
        `Quick
        (test_json_roundtrip_err name e))
    [
      ("Reject", Reject (0, script_expr_int, None));
      ("Overflow", Overflow (0, None));
      ( "Runtime_contract_error",
        Runtime_contract_error (contract_zero, script_expr_int) );
      ("Bad_contract_parameter", Bad_contract_parameter contract_zero);
      ( "Cannot_serialize_log",
        Helpers_services.Scripts.Traced_interpreter.Cannot_serialize_log );
      ("Cannot_serialize_failure", Cannot_serialize_failure);
      ("Cannot_serialize_storage", Cannot_serialize_storage);
    ]

let tests =
  [
    Test_services.tztest
      "test bad contract error"
      `Quick
      test_bad_contract_parameter;
    Test_services.tztest "test stack overflow error" `Slow test_stack_overflow;
  ]
  @ error_encoding_tests
