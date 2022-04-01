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
        "- @[<v 0>%a (interp) @@ location: %d (remaining gas: %a)@,\
         [ @[<v 0>%a ]@]@]"
  | Exit ->
      Format.fprintf
        fmt
        "- @[<v 0>%a (exit) @@ location: %d (remaining gas: %a)@,\
         [ @[<v 0>%a ]@]@]@]"
  | Entry ->
      Format.fprintf
        fmt
        "@[<v 2>- @[<v 0>%a (entry) @@ location: %d (remaining gas: %a)@,\
         [ @[<v 0>%a ]@]@]"

let pp_trace fmt = function
  | TInstr (loc, gas, instr, stack, element_kind) ->
      with_indentation
        fmt
        element_kind
        Plugin.RPC.Scripts.pp_instr_name
        instr
        loc
        Gas.pp
        gas
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
  let get_log () = assert false in
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
  let* b, _cs = Context.init1 ~consensus_threshold:0 () in
  let* v = Incremental.begin_construction b in
  return (Incremental.alpha_ctxt v)

let first_reg_test () =
  let get_log, logger = logger () in
  let* ctxt = test_context () in
  let script = Contract_helpers.read_file "contracts/add_to_store.tz" in
  let* _res, _ctxt =
    Contract_helpers.run_script
      ctxt
      script
      ~logger
      ~storage:"5"
      ~parameter:"3"
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

let register () =
  Regression.register
    ~__FILE__
    ~title:"first_regression_test"
    ~tags:["protocol"; "regression"]
    ~output_file:"test_logging"
    (fail_on_error first_reg_test)
