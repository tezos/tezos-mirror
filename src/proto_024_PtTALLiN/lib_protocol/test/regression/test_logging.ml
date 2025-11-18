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
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/regression/main.exe
    Subject:      Type-checking
 *)

open Protocol
open Alpha_context
open Tezt

module Traced_interpreter = Plugin.RPC.Scripts.Traced_interpreter (struct
  let unparsing_mode = Script_ir_unparser.Readable
end)

type contract = {filename : string; storage : string}

type transaction =
  | Simple of {dst : contract; amount : Tez.t; parameter : string}
  | With_lib of {
      dst : contract;
      lib : contract;
      amount : Tez.t;
      parameter : Contract_hash.t -> string;
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

let transaction ?(amount = Tez.zero) ~parameter ~storage filename =
  Simple {amount; parameter; dst = {filename; storage}}

let with_lib ?(amount = Tez.zero) ~parameter ~storage ~lib ~lib_storage filename
    =
  With_lib
    {
      amount;
      parameter;
      dst = {storage; filename};
      lib = {filename = lib; storage = lib_storage};
    }

let filename = function
  | Simple {dst = {filename; _}; _} | With_lib {dst = {filename; _}; _} ->
      filename

let amount = function Simple {amount; _} | With_lib {amount; _} -> amount

let storage = function
  | Simple {dst = {storage; _}; _} | With_lib {dst = {storage; _}; _} -> storage

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
  let open Lwt_result_wrap_syntax in
  let open Script_typed_ir in
  let log : log_element list ref = ref [] in
  let logger =
    Script_interpreter_logging.make
      (module struct
        let log_interp : type a s b f c u. (a, s, b, f, c, u) logging_function =
         fun instr ctxt loc sty stack ->
          log := With_stack (ctxt, instr, loc, stack, sty, Interp) :: !log

        let log_entry instr ctxt loc sty stack =
          log := With_stack (ctxt, instr, loc, stack, sty, Entry) :: !log

        let log_exit instr ctxt loc sty stack =
          log := With_stack (ctxt, instr, loc, stack, sty, Exit) :: !log

        let log_control cont = log := Ctrl cont :: !log

        let get_log () = return_none
      end)
  in
  let assemble_log () =
    let+ l =
      List.map_es
        (function
          | With_stack (ctxt, instr, loc, stack, stack_ty, indent) ->
              let+@ stack =
                Traced_interpreter.unparse_stack ctxt (stack, stack_ty)
              in
              TInstr (loc, Gas.level ctxt, instr, stack, indent)
          | Ctrl cont -> return @@ TCtrl cont)
        !log
    in
    List.rev l
  in
  (assemble_log, logger)

(* [with_logger ~mask f] creates a fresh logger and passes it to [f].
   After [f] finishes, logs are gathered and each occurrence of each
   string in [mask] list is being replaced with asterisks. Thus processed
   log is captured as regression output. *)
let with_logger f =
  let open Lwt_result_syntax in
  let get_log, logger = logger () in
  let* () = f logger in
  let* log = get_log () in
  let capture s = Tezos_regression.replace_variables s |> Regression.capture in
  Format.kasprintf
    capture
    "@,@[<v 2>trace@,%a@]"
    (Format.pp_print_list pp_trace)
    log ;
  return_unit

let read_code filename =
  let filename =
    project_root // Filename.dirname __FILE__ // "contracts"
    // (filename ^ ".tz")
  in
  Contract_helpers.read_file filename

let run_script transaction () =
  let open Lwt_result_syntax in
  let script = read_code @@ filename transaction in
  let* parameter, ctxt =
    match transaction with
    | With_lib {lib = {filename; storage}; parameter; _} ->
        let* block, baker, _contract, _src2 =
          Contract_helpers.init
            ~hard_gas_limit_per_block:(Gas.Arith.integral_of_int_exn 10_000_000)
            ()
        in
        let sender = Contract.Implicit baker in
        let* src_addr, _script, block =
          Contract_helpers.originate_contract_from_string_hash
            ~baker
            ~source_contract:sender
            ~script:(read_code filename)
            ~storage
            block
        in
        let* incr = Incremental.begin_construction block in
        return (parameter src_addr, Incremental.alpha_ctxt incr)
    | Simple {parameter; _} ->
        let* b, _contract = Context.init1 ~consensus_threshold_size:0 () in
        let* inc = Incremental.begin_construction b in
        let ctxt = Incremental.alpha_ctxt inc in
        let ctxt =
          Alpha_context.Origination_nonce.init ctxt Operation_hash.zero
        in
        return (parameter, ctxt)
  in
  with_logger @@ fun logger ->
  let step_constants =
    Contract_helpers.
      {
        default_step_constants with
        amount = amount transaction;
        now = Script_timestamp.of_int64 1649939559L;
      }
  in
  let* _res, _ctxt =
    Contract_helpers.run_script
      ctxt
      script
      ~logger
      ~storage:(storage transaction)
      ~parameter
      ~step_constants
      ~internal:true (* Allow for forged values (e.g. tickets). *)
      ()
  in
  return_unit

let fail_on_error f () =
  let open Lwt_syntax in
  let* result = f () in
  match result with
  | Ok () -> return_unit
  | Error e -> Test.fail "%a" Error_monad.pp_print_trace e

(* Make sure that after a snapshot the snapshotted version of the test
   has a different [~title], because all tests are linked in [tezt/tests/main.exe]. *)
let protocol =
  match __FILE__ =~* rex "^src/proto_([0-9a-zA-Z_]*)/" with
  | None ->
      Stdlib.failwith ("failed to extract protocol name from path: " ^ __FILE__)
  | Some name -> name

let register_script transaction =
  (* [~title] must be unique across the codebase, so we prefix it with the protocol name.
     [~file] however is better kept the same across protocols to simplify snapshotting. *)
  let file = filename transaction in
  Regression.register
    ~__FILE__
    ~title:(protocol ^ ": " ^ file)
    ~tags:["protocol"; "regression"; "logging"]
    ~file
    (fail_on_error @@ run_script transaction)

(* These tests should always cover:
    - every instruction type, which means an example of each group of instructions
      which are similar to each other with respect to logging; no need to cover every
      instruction whatsoever, but just every distinct kind ;
    - every continuation and control structure in Michelson, because those impact
      what is being logged and what is not.
   We are not concerned with gas, because that's kept track of by regular regression
   tests. Actually, gas is unaccounted for in all the tests in this module. *)
let () =
  Array.iter
    register_script
    [|
      transaction
        ~storage:"{}"
        ~parameter:"Left \"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\""
        "accounts";
      transaction
        ~storage:"{1; 2; 3}"
        ~parameter:"Pair {7; 8; 9} {4; 5; 6}"
        "append";
      transaction
        ~amount:(Tez.of_mutez_exn 100_000_000L)
        ~parameter:"\"tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv\""
        ~storage:
          "Pair \"2099-12-31T23:59:59Z\" (Pair 50000000 \
           \"tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU\")"
        "auction";
      transaction
        ~parameter:"{Pair \"string\" 12; Pair \"abc\" 99; Pair \"def\" 3}"
        ~storage:"Pair { Elt \"123\" 123 } Unit"
        "big_map_union";
      transaction
        ~parameter:"\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\""
        ~storage:
          "Pair \
           \"edsigu6Ue4mQgPC5aCFqqjitU9pCs5VErXrfPTAZffyJepccGzDEEBExtuPjGuMc2ZRSTBUDR7tJMLVTeJzZn7p9jN9inh4ooV1\" \
           \"TEZOS\""
        "check_signature";
      transaction ~parameter:"Pair 1 4 2 Unit" ~storage:"Unit" "comb-get";
      transaction ~parameter:"Unit" ~storage:"Pair 1 4 2 Unit" "comb-set";
      transaction ~parameter:"\"abcd\"" ~storage:"\"efgh\"" "concat";
      transaction ~parameter:"Right (Some 23)" ~storage:"\"\"" "conditionals";
      transaction ~parameter:"2" ~storage:"60" "cps_fact";
      transaction
        ~parameter:"Pair (Pair (Pair (Pair 0 1) 2) 3) 4"
        ~storage:"7"
        "dign";
      transaction
        ~parameter:"Pair (Pair (Pair (Pair 0 1) 2) 3) 4"
        ~storage:"7"
        "dipn";
      transaction
        ~parameter:"Pair (Pair (Pair (Pair 0 1) 2) 3) 4"
        ~storage:"7"
        "dugn";
      transaction
        ~parameter:"Pair 127 11"
        ~storage:"Pair None None None None"
        "ediv";
      transaction
        ~parameter:"\"tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU\""
        ~storage:"\"2020-01-01T00:00:00Z\""
        "faucet";
      transaction
        ~parameter:"\"abc\""
        ~storage:"Pair (Some 321) {Elt \"def\" 123}"
        "get_and_update_map";
      transaction ~parameter:"True" ~storage:"None" "if";
      transaction
        ~parameter:"{8; 3; 2; 7; 6; 9; 5; 1; 4; 0}"
        ~storage:"{}"
        "insertion_sort";
      transaction
        ~parameter:"{1; 2; 3; 4; 5; 6; 7}"
        ~storage:"{}"
        "list_map_block";
      transaction
        ~parameter:"{\"abc\"; \"xyz\"}"
        ~storage:"{\"zyx\"; \"cba\"}"
        "loop_left";
      transaction
        ~parameter:
          "Pair (Pair (Pair \"abc\" {1; 2; 3}) {4; 5; 6}) \
           0x0507070707010000000361626302000000060001000200030200000006000400050006"
        ~storage:"Unit"
        "packunpack";
      transaction ~parameter:"7" ~storage:"77" "pexec";
      transaction
        ~parameter:"{\"abc\"; \"def\" ; \"ghi\"}"
        ~storage:"{}"
        "reverse_loop";
      transaction
        ~parameter:"Some \"tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN\""
        ~storage:"Unit"
        "set_delegate";
      transaction ~parameter:"Right (Pair 3 2)" ~storage:"None" "shifts";
      transaction
        ~amount:(Tez.of_mutez_exn 1_200_00L)
        ~parameter:"7"
        ~storage:"{}"
        "spawn_identities";
      transaction
        ~parameter:"Pair \"KT1Ln1MPvHDJ1phLL8dNL4jrKF6Q1yQCBG1v\" 17 3"
        ~storage:"None"
        "ticket_join";
      transaction
        ~parameter:"Pair \"KT1Ln1MPvHDJ1phLL8dNL4jrKF6Q1yQCBG1v\" 17 3"
        ~storage:"Unit"
        "ticket_split";
      transaction ~parameter:"5" ~storage:"3" "view_toplevel_lib";
      transaction ~parameter:"Left (Pair True False)" ~storage:"None" "xor";
      transaction ~parameter:"7" ~storage:"Some 3" "opt_map";
      with_lib
        ~parameter:(Format.asprintf "Pair 8 \"%a\"" Contract_hash.pp)
        ~storage:"0"
        ~lib:"view_toplevel_lib"
        ~lib_storage:"0"
        "view_fib";
      transaction ~parameter:"Unit" ~storage:"Unit" "rec_id_unit";
    |]
