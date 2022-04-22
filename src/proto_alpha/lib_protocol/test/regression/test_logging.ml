(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
    Component:    Protocol (type-checking)
    Invocation:   cd src/proto_alpha/lib_protocol/test/regression && \
                  dune exec ./main.exe
    Subject:      Type-checking
 *)

open Lwt_result_syntax
open Protocol
open Alpha_context
open Tezt

module Traced_interpreter = Plugin.RPC.Scripts.Traced_interpreter (struct
  let unparsing_mode = Script_ir_translator.Readable
end)

type contract = {
  filename : string;
  amount : Tez.t;
  storage : string;
  parameter : string;
}

type element_kind = Interp | Entry | Exit

type log_element =
  | With_stack :
      context
      * ('a, 'b, 'c, 'd) Script_typed_ir.kinstr
      * Script.location
      * ('e * 'f)
      * ('e, 'f) Script_typed_ir.stack_ty
      * element_kind
      -> log_element
  | Ctrl : ('a, 'b, 'c, 'd) Script_typed_ir.continuation -> log_element

type trace_element =
  | TInstr :
      Script.location
      * Gas.t
      * ('a, 'b, 'c, 'd) Script_typed_ir.kinstr
      * Script.expr list
      * element_kind
      -> trace_element
  | TCtrl : ('a, 'b, 'c, 'd) Script_typed_ir.continuation -> trace_element

let with_indentation fmt = function
  | Interp ->
      Format.fprintf
        fmt
        "- @[<v 0>%a (interp) @@ location: %d@,[ @[<v 0>%a ]@]@]"
  | Exit ->
      Format.fprintf
        fmt
        "- @[<v 0>%a (exit) @@ location: %d@,[ @[<v 0>%a ]@]@]@]"
  | Entry ->
      Format.fprintf
        fmt
        "@[<v 2>- @[<v 0>%a (entry) @@ location: %d@,[ @[<v 0>%a ]@]@]"

let pp_trace fmt = function
  | TInstr (loc, _gas, instr, stack, element_kind) ->
      with_indentation
        fmt
        element_kind
        Plugin.RPC.Scripts.pp_instr_name
        instr
        loc
        (Format.pp_print_list (fun ppf e ->
             Format.fprintf ppf "@[<v 0>%a@]" Michelson_v1_printer.print_expr e))
        stack
  | TCtrl continuation -> (
      Format.fprintf fmt "- @[<v 0>control: %s@]"
      @@
      match continuation with
      | KNil -> "KNil"
      | KCons _ -> "KCons"
      | KReturn _ -> "KReturn"
      | KView_exit _ -> "KView_exit"
      | KMap_head _ -> "KMap_head"
      | KUndip _ -> "KUndip"
      | KLoop_in _ -> "KLoop_in"
      | KLoop_in_left _ -> "KLoop_in_left"
      | KIter _ -> "KIter"
      | KList_enter_body _ -> "KList_enter_body"
      | KList_exit_body _ -> "KList_exit_body"
      | KMap_enter_body _ -> "KMap_enter_body"
      | KMap_exit_body _ -> "KMap_exit_body"
      | KLog _ -> "KLog")

let logger () :
    (unit -> trace_element list tzresult Lwt.t) * Script_typed_ir.logger =
  let open Script_typed_ir in
  let log : log_element list ref = ref [] in
  let log_interp : type a s b f c u. (a, s, b, f, c, u) logging_function =
   fun instr ctxt loc sty stack ->
    log := With_stack (ctxt, instr, loc, stack, sty, Interp) :: !log
  in
  let log_entry instr ctxt loc sty stack =
    log := With_stack (ctxt, instr, loc, stack, sty, Entry) :: !log
  in
  let log_exit instr ctxt loc sty stack =
    log := With_stack (ctxt, instr, loc, stack, sty, Exit) :: !log
  in
  let log_control cont = log := Ctrl cont :: !log in
  let get_log () = return_none in
  let assemble_log () =
    let open Environment.Error_monad in
    let+ l =
      List.map_es
        (function
          | With_stack (ctxt, instr, loc, stack, stack_ty, indent) ->
              let+ stack =
                Lwt.map Environment.wrap_tzresult
                @@ Traced_interpreter.unparse_stack ctxt (stack, stack_ty)
              in
              TInstr (loc, Gas.level ctxt, instr, stack, indent)
          | Ctrl cont -> return @@ TCtrl cont)
        !log
    in
    List.rev l
  in
  (assemble_log, {log_exit; log_entry; log_interp; get_log; log_control})

let test_context () =
  let open Environment.Error_monad in
  let* b, _contract = Context.init1 ~consensus_threshold:0 () in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  return @@ Alpha_context.Origination_nonce.init ctxt Operation_hash.zero

let run_script {filename; amount; storage; parameter} () =
  let get_log, logger = logger () in
  let script =
    Contract_helpers.read_file @@ Base.(("contracts" // filename) ^ ".tz")
  in
  let* ctxt = test_context () in
  let step_constants =
    Contract_helpers.
      {
        default_step_constants with
        amount;
        now = Script_timestamp.of_int64 1649939559L;
      }
  in
  let* _res, _ctxt =
    Contract_helpers.run_script
      ctxt
      script
      ~logger
      ~storage
      ~parameter
      ~step_constants
      ~internal:true (* Allow for forged values (e.g. tickets). *)
      ()
  in
  let* log = get_log () in
  Format.kasprintf
    Regression.capture
    "@,@[<v 2>trace@,%a@]"
    (Format.pp_print_list pp_trace)
    log ;
  return_unit

let fail_on_error f () =
  let open Lwt_syntax in
  let* result = f () in
  match result with
  | Ok () -> return ()
  | Error e -> Test.fail "%a" Error_monad.pp_print_trace e

let register_script contract =
  Regression.register
    ~__FILE__
    ~title:contract.filename
    ~tags:["protocol"; "regression"; "logging"]
    ~output_file:contract.filename
    (fail_on_error @@ run_script contract)

(* These tests should always cover:
    - every instruction type, which means an example of each group of instructions
      which are similar to each other with respect to logging; no need to cover every
      instruction whatsoever, but just every distinct kind ;
    - every continuation and control structure in Michelson, because those impact
      what is being logged and what is not.
   We are not concerned with gas, because that's kept track of by regular regression
   tests. Actually, gas is unaccounted for in all the tests in this module. *)
let register () =
  Array.iter
    register_script
    [|
      {
        filename = "accounts";
        amount = Tez.zero;
        parameter = "Left \"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\"";
        storage = "{}";
      };
      {
        filename = "append";
        amount = Tez.zero;
        parameter = "Pair {7; 8; 9} {4; 5; 6}";
        storage = "{1; 2; 3}";
      };
      {
        filename = "auction";
        amount = Tez.of_mutez_exn 100_000_000L;
        parameter = "\"tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv\"";
        storage =
          "Pair \"2099-12-31T23:59:59Z\" (Pair 50000000 \
           \"tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU\")";
      };
      {
        filename = "big_map_union";
        amount = Tez.zero;
        parameter = "{Pair \"string\" 12; Pair \"abc\" 99; Pair \"def\" 3}";
        storage = "Pair { Elt \"123\" 123 } Unit";
      };
      {
        filename = "check_signature";
        amount = Tez.zero;
        parameter = "\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\"";
        storage =
          "Pair \
           \"edsigu6Ue4mQgPC5aCFqqjitU9pCs5VErXrfPTAZffyJepccGzDEEBExtuPjGuMc2ZRSTBUDR7tJMLVTeJzZn7p9jN9inh4ooV1\" \
           \"TEZOS\"";
      };
      {
        filename = "comb-get";
        amount = Tez.zero;
        parameter = "Pair 1 4 2 Unit";
        storage = "Unit";
      };
      {
        filename = "comb-set";
        amount = Tez.zero;
        parameter = "Unit";
        storage = "Pair 1 4 2 Unit";
      };
      {
        filename = "concat";
        amount = Tez.zero;
        parameter = "\"abcd\"";
        storage = "\"efgh\"";
      };
      {
        filename = "conditionals";
        amount = Tez.zero;
        parameter = "Right (Some 23)";
        storage = "\"\"";
      };
      {
        filename = "cps_fact";
        amount = Tez.zero;
        parameter = "2";
        storage = "60";
      };
      {
        filename = "dign";
        amount = Tez.zero;
        parameter = "Pair (Pair (Pair (Pair 0 1) 2) 3) 4";
        storage = "7";
      };
      {
        filename = "dipn";
        amount = Tez.zero;
        parameter = "Pair (Pair (Pair (Pair 0 1) 2) 3) 4";
        storage = "7";
      };
      {
        filename = "dugn";
        amount = Tez.zero;
        parameter = "Pair (Pair (Pair (Pair 0 1) 2) 3) 4";
        storage = "7";
      };
      {
        filename = "ediv";
        amount = Tez.zero;
        parameter = "Pair 127 11";
        storage = "Pair None None None None";
      };
      {
        filename = "faucet";
        amount = Tez.zero;
        parameter = "\"tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU\"";
        storage = "\"2020-01-01T00:00:00Z\"";
      };
      {
        filename = "get_and_update_map";
        amount = Tez.zero;
        parameter = "\"abc\"";
        storage = "Pair (Some 321) {Elt \"def\" 123}";
      };
      {filename = "if"; amount = Tez.zero; parameter = "True"; storage = "None"};
      {
        filename = "insertion_sort";
        amount = Tez.zero;
        parameter = "{8; 3; 2; 7; 6; 9; 5; 1; 4; 0}";
        storage = "{}";
      };
      {
        filename = "list_map_block";
        amount = Tez.zero;
        parameter = "{1; 2; 3; 4; 5; 6; 7}";
        storage = "{}";
      };
      {
        filename = "loop_left";
        amount = Tez.zero;
        parameter = "{\"abc\"; \"xyz\"}";
        storage = "{\"zyx\"; \"cba\"}";
      };
      {
        filename = "packunpack";
        amount = Tez.zero;
        parameter =
          "Pair (Pair (Pair \"abc\" {1; 2; 3}) {4; 5; 6}) \
           0x0507070707010000000361626302000000060001000200030200000006000400050006";
        storage = "Unit";
      };
      {filename = "pexec"; amount = Tez.zero; parameter = "7"; storage = "77"};
      {
        filename = "reverse_loop";
        amount = Tez.zero;
        parameter = "{\"abc\" ; \"def\" ; \"ghi\"}";
        storage = "{}";
      };
      {
        filename = "set_delegate";
        amount = Tez.zero;
        parameter = "Some \"tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN\"";
        storage = "Unit";
      };
      {
        filename = "shifts";
        amount = Tez.zero;
        parameter = "Right (Pair 3 2)";
        storage = "None";
      };
      {
        filename = "spawn_identities";
        amount = Tez.of_mutez_exn 1_200_00L;
        parameter = "7";
        storage = "{}";
      };
      {
        filename = "ticket_join";
        amount = Tez.zero;
        parameter = "Pair \"KT1Ln1MPvHDJ1phLL8dNL4jrKF6Q1yQCBG1v\" 17 3";
        storage = "None";
      };
      {
        filename = "ticket_split";
        amount = Tez.zero;
        parameter = "Pair \"KT1Ln1MPvHDJ1phLL8dNL4jrKF6Q1yQCBG1v\" 17 3";
        storage = "Unit";
      };
      {
        filename = "view_toplevel_lib";
        amount = Tez.zero;
        parameter = "5";
        storage = "3";
      };
      {
        filename = "xor";
        amount = Tez.zero;
        parameter = "Left (Pair True False)";
        storage = "None";
      };
    |]
