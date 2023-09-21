(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
    Component:    Protocol (interpretation)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_interpretation.ml
    Subject:      Interpretation of Michelson scripts
*)

open Protocol
open Alpha_context
open Script_interpreter

let test_context () =
  let open Lwt_result_syntax in
  let* b, _cs = Context.init3 () in
  let* v = Incremental.begin_construction b in
  return (Incremental.alpha_ctxt v)

let logger =
  Script_interpreter_logging.make
    (module struct
      let log_interp _ _ _ _ _ = ()

      let log_entry _ _ _ _ _ = ()

      let log_exit _ _ _ _ _ = ()

      let log_control _ = ()

      let get_log () = Lwt_result_syntax.return_none
    end)

let run_step ctxt code accu stack =
  let open Lwt_result_syntax in
  let open Script_interpreter in
  let open Contract_helpers in
  let* ((_, _, ctxt') as r) =
    Internals.step_descr None ctxt default_step_constants code accu stack
  in
  let* _, _, ctxt'' =
    Internals.step_descr
      (Some logger)
      ctxt
      default_step_constants
      code
      accu
      stack
  in
  if Gas.(remaining_operation_gas ctxt' <> remaining_operation_gas ctxt'') then
    Alcotest.failf "Logging should not have an impact on gas consumption." ;
  return r

(** Runs a script with an ill-typed parameter and verifies that a
    Bad_contract_parameter error is returned. *)
let test_bad_contract_parameter () =
  let open Lwt_result_syntax in
  let* ctx = test_context () in
  (* Run script with a parameter of wrong type *)
  let*! result =
    Contract_helpers.run_script
      ctx
      "{parameter unit; storage unit; code { CAR; NIL operation; PAIR }}"
      ~storage:"Unit"
      ~parameter:"0"
      ()
  in
  match result with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error (Environment.Ecoproto_error (Bad_contract_parameter source') :: _) ->
      Alcotest.(check Testable.contract)
        "incorrect field in Bad_contract_parameter"
        (Contract.Originated Contract_helpers.default_self)
        source' ;
      return_unit
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_trace errs

let test_multiplication_close_to_overflow_passes () =
  let open Lwt_result_syntax in
  let* ctx = test_context () in
  (* Get sure that multiplication deals with numbers between 2^62 and
     2^63 without overflowing *)
  let*! result =
    Contract_helpers.run_script
      ctx
      "{parameter unit;storage unit;code {DROP; PUSH mutez \
       2944023901536524477; PUSH nat 2; MUL; DROP; UNIT; NIL operation; PAIR}}"
      ~storage:"Unit"
      ~parameter:"Unit"
      ()
  in
  match result with
  | Ok _ -> return_unit
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_trace errs

let dummy_loc = -1

(** The purpose of these two tests is to check that the Michelson interpreter is
    stack-safe (because it is tail-recursive).

    This requires to confront it to deep recursions, typically deeper than what
    the gas limit allows. Unfortunately we cannot run the interpreter in
    unaccounted gas mode because for efficiency it uses a custom gas management
    that represents the gas counter as a mere integer. Instead we set the gas
    counter to the highest possible value ([Saturation_repr.saturated]); with
    the current gas costs and limits this enables more than a million recursive
    calls which is larger than the stack size. *)

let test_stack_overflow () =
  let open Lwt_result_syntax in
  let open Script_typed_ir in
  let* ctxt = test_context () in
  (* Set the gas counter to the maximum value *)
  let ctxt =
    Gas.update_remaining_operation_gas ctxt
    @@ Gas.fp_of_milligas_int (Saturation_repr.saturated :> int)
  in
  let stack = Bot_t in
  let descr kinstr = {kloc = 0; kbef = stack; kaft = stack; kinstr} in
  let enorme_et_seq n =
    let rec aux n acc =
      if n = 0 then acc
      else aux (n - 1) (IPush (dummy_loc, Bool_t, true, IDrop (dummy_loc, acc)))
    in
    aux n (IHalt dummy_loc)
  in
  let*! result =
    run_step ctxt (descr (enorme_et_seq 1_000_000)) EmptyCell EmptyCell
  in
  match result with
  | Ok _ -> return_unit
  | Error trace ->
      let trace_string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace trace
      in
      Alcotest.failf "Unexpected error (%s) at %s" trace_string __LOC__

(** The stack-safety of the interpreter relies a lot on the stack-safety of
    Lwt.bind. This second test is similar to the previous one but uses an
    instruction (IBig_map_mem) for which the interpreter calls Lwt.bind. *)

let test_stack_overflow_in_lwt () =
  let open Lwt_result_syntax in
  let open Script_typed_ir in
  let* ctxt = test_context () in
  let ctxt =
    Gas.update_remaining_operation_gas ctxt
    @@ Gas.fp_of_milligas_int (Saturation_repr.saturated :> int)
  in
  let stack = Bot_t in
  let descr kinstr = {kloc = 0; kbef = stack; kaft = stack; kinstr} in
  let push_empty_big_map k = IEmpty_big_map (dummy_loc, unit_t, unit_t, k) in
  let large_mem_seq n =
    let rec aux n acc =
      if n = 0 then acc
      else
        aux
          (n - 1)
          (IDup
             ( dummy_loc,
               IPush
                 ( dummy_loc,
                   Unit_t,
                   (),
                   IBig_map_mem (dummy_loc, IDrop (dummy_loc, acc)) ) ))
    in
    aux n (IDrop (dummy_loc, IHalt dummy_loc))
  in
  let script = push_empty_big_map (large_mem_seq 1_000_000) in
  let*! result = run_step ctxt (descr script) EmptyCell EmptyCell in
  match result with
  | Ok _ -> return_unit
  | Error trace ->
      let trace_string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace trace
      in
      Alcotest.failf "Unexpected error (%s) at %s" trace_string __LOC__

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

(** Encoding/decoding of script_interpreter.ml specific errors. *)
let test_json_roundtrip_err name e () =
  test_json_roundtrip
    name
    Testable.protocol_error
    Environment.Error_monad.error_encoding
    e

let error_encoding_tests =
  let contract_zero = Contract.Implicit Signature.Public_key_hash.zero in
  let script_expr_int = Micheline.strip_locations (Micheline.Int (0, Z.zero)) in
  List.map
    (fun (name, e) ->
      Tztest.tztest
        (Format.asprintf "test error encoding: %s" name)
        `Quick
        (test_json_roundtrip_err name e))
    [
      ("Reject", Reject (0, script_expr_int, None));
      ("Overflow", Overflow (0, None));
      ("Runtime_contract_error", Runtime_contract_error Contract_hash.zero);
      ("Bad_contract_parameter", Bad_contract_parameter contract_zero);
      ("Cannot_serialize_failure", Cannot_serialize_failure);
      ("Cannot_serialize_storage", Cannot_serialize_storage);
    ]

module Test_map_instr_on_options = struct
  type storage = {prev : int option; total : int}

  (* storage: (last input * total); param replaces the last input and
     if some – gets added to the total. *)
  let test_map_option_script =
    {| { parameter (option int);
         storage (pair (option int) int);
         code {
           UNPAIR ;
           DIP { CDR } ;
           MAP {
             DUP ;
             DIP { ADD } ;
           } ;
           PAIR ;
           NIL operation ;
           PAIR ;
        }
      } |}

  let run_test_map_opt_script param {prev; total} =
    let open Lwt_result_syntax in
    let storage =
      Option.fold
        ~none:(Format.sprintf "Pair None %d" total)
        ~some:(fun p -> Format.sprintf "Pair (Some %d) %d" p total)
        prev
    in
    let parameter =
      Option.fold ~none:"None" ~some:(Format.sprintf "Some %d") param
    in
    let* ctxt = test_context () in
    Contract_helpers.run_script
      ctxt
      test_map_option_script
      ~storage
      ~parameter
      ()

  let assume_storage_shape =
    let open Micheline in
    let open Michelson_v1_primitives in
    function
    | Prim (_, D_Pair, [Prim (_, D_None, [], _); Int (_, total)], _) ->
        {prev = None; total = Z.to_int total}
    | Prim (_, D_Pair, [Prim (_, D_Some, [Int (_, prev)], _); Int (_, total)], _)
      ->
        {prev = Some (Z.to_int prev); total = Z.to_int total}
    | _ -> QCheck2.assume_fail ()

  let assertions storage_before storage_after =
    let open Lwt_result_syntax in
    function
    | None ->
        let* () =
          Assert.is_none ~loc:__LOC__ ~pp:Format.pp_print_int storage_after.prev
        in
        Assert.equal_int ~loc:__LOC__ storage_before.total storage_after.total
    | Some input ->
        let* prev_aft = Assert.get_some ~loc:__LOC__ storage_after.prev in
        let* () = Assert.equal_int ~loc:__LOC__ input prev_aft in
        Assert.equal_int
          ~loc:__LOC__
          (storage_before.total + input)
          storage_after.total

  let test_mapping (input, prev, total) =
    let open Lwt_result_syntax in
    let storage_before = {prev; total} in
    let* {storage; _}, _ = run_test_map_opt_script input storage_before in
    let new_storage = assume_storage_shape (Micheline.root storage) in
    assertions storage_before new_storage input
end

let test_contract path storage param ~entrypoint_str ~ok ~ko =
  let open Lwt_result_syntax in
  let entrypoint =
    match entrypoint_str with
    | None -> Entrypoint.default
    | Some str -> Entrypoint.of_string_strict_exn str
  in
  let* ctx = test_context () in
  let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch ;
    s
  in
  let script = read_file path in
  let*! result =
    Contract_helpers.run_script
      ctx
      script
      ~storage
      ~parameter:param
      ~entrypoint
      ()
  in
  match result with Ok (res, _) -> ok res | Error t -> ko t

let fail_with_trace trace =
  Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_trace trace

let test_contract_success path storage param expected_storage_str
    ?entrypoint_str () =
  let expected_storage = Expr.from_string expected_storage_str in
  test_contract
    path
    storage
    param
    ~ok:(fun real ->
      if real.storage = expected_storage then return_unit
      else Alcotest.fail "Unexpected result")
    ~ko:fail_with_trace
    ~entrypoint_str

let test_contract_fail path storage param ?entrypoint_str () =
  test_contract
    path
    storage
    param
    ~ok:(fun _ ->
      Alcotest.failf
        "Unexpected success: interpreting %s should have failed."
        path)
    ~ko:(fun _ -> return_unit)
    ~entrypoint_str

let test_store_and_reload path ~init_storage ~entrypoint_str_1 ~param_1
    ~expected_storage_str_1 ~entrypoint_str_2 ~param_2 ~expected_storage_str_2
    () =
  let expected_storage_1 = Expr.from_string expected_storage_str_1 in
  test_contract
    path
    init_storage
    param_1
    ~entrypoint_str:(Some entrypoint_str_1)
    ~ok:(fun real ->
      if real.storage = expected_storage_1 then
        test_contract_success
          path
          expected_storage_str_1
          param_2
          expected_storage_str_2
          ~entrypoint_str:entrypoint_str_2
          ()
      else
        Alcotest.failf
          "Unexpected result. \n Expected :\n %s \n Real : \n %s \n"
          (Expr.to_string expected_storage_1)
          (Expr.to_string real.storage))
    ~ko:fail_with_trace

let path = project_root // Filename.dirname __FILE__

let tests =
  [
    Tztest.tztest "bad contract error" `Quick test_bad_contract_parameter;
    Tztest.tztest "check robustness overflow error" `Slow test_stack_overflow;
    Tztest.tztest
      "check robustness overflow error in lwt"
      `Slow
      test_stack_overflow_in_lwt;
    Tztest.tztest
      "multiplication no illegitimate overflow"
      `Quick
      test_multiplication_close_to_overflow_passes;
    Tztest.tztest "stack overflow error" `Slow test_stack_overflow;
    Tztest.tztest_qcheck2
      ~name:"map instr against options"
      QCheck2.Gen.(
        triple (opt small_signed_int) (opt small_signed_int) small_signed_int)
      Test_map_instr_on_options.test_mapping;
    Tztest.tztest
      "lambda_rec instruction"
      `Quick
      (test_contract_success (path // "contracts/rec_fact.tz") "0" "5" "120");
    Tztest.tztest
      "lambda_rec instruction with apply"
      `Quick
      (test_contract_success
         (path // "contracts/rec_fact_apply.tz")
         "0"
         "5"
         "120");
    Tztest.tztest
      "lambda_rec instruction with an infinite recursion"
      `Quick
      (test_contract_fail (path // "contracts/omega.tz") "Unit" "Unit");
    Tztest.tztest
      "lambda_rec instruction storage"
      `Quick
      (test_store_and_reload
         (path // "contracts/rec_fact_store.tz")
         ~init_storage:"Left 0"
         ~entrypoint_str_1:"gen"
         ~param_1:"Unit"
         ~expected_storage_str_1:
           {|Right
             (Lambda_rec
                { DUP ;
                  EQ ;
                  IF { PUSH int 1 } { DUP ; DUP 3 ; PUSH int 1 ; DUP 4 ; SUB
            ; EXEC ; MUL } ;
                  DIP { DROP 2 } })|}
         ~entrypoint_str_2:"exec"
         ~param_2:"5"
         ~expected_storage_str_2:"Left 120");
    Tztest.tztest
      "lambda_rec instruction storage apply store"
      `Quick
      (test_store_and_reload
         (path // "contracts/rec_fact_apply_store.tz")
         ~init_storage:"Left 0"
         ~entrypoint_str_1:"gen"
         ~param_1:"Unit"
         ~expected_storage_str_1:
           {|Right
              { PUSH unit Unit ;
                PAIR ;
                LAMBDA_REC
                  (pair unit int)
                  int
                  { UNPAIR ;
                    DUP 2 ;
                    EQ ;
                    IF { PUSH int 1 }
                       { DUP 2 ; DUP 4 ; DUP 3 ; APPLY ; PUSH int 1 ; DUP 3 ;
            SUB ; EXEC ; MUL } ;
                    DIP { DROP 3 } } ;
                SWAP ;
                EXEC }|}
         ~entrypoint_str_2:"exec"
         ~param_2:"5"
         ~expected_storage_str_2:"Left 120");
  ]
  @ error_encoding_tests

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("interpretation", tests)]
  |> Lwt_main.run
