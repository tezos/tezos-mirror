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

type log_element =
  | With_stack :
      context
      * Script.location
      * ('a * 's)
      * ('a, 's) Script_typed_ir.stack_ty
      * int (* indentation change *)
      -> log_element

type trace_element =
  | TInstr : Script.location * Gas.t * Script.expr list -> trace_element

let pp_trace fmt = function
  | TInstr (loc, gas, stack) ->
      Format.fprintf
        fmt
        "- @[<v 0>location: %d (remaining gas: %a)@,[ @[<v 0>%a ]@]@]"
        loc
        Gas.pp
        gas
        (Format.pp_print_list (fun ppf e ->
             Format.fprintf ppf "@[<v 0>%a@]" Michelson_v1_printer.print_expr e))
        stack

let logger () :
    (unit -> trace_element list tzresult Lwt.t) * Script_typed_ir.logger =
  let log : log_element list ref = ref [] in
  let log_interp _ ctxt loc sty stack =
    log := With_stack (ctxt, loc, stack, sty, 0) :: !log
  in
  let log_entry _ _ctxt _loc _sty _stack = () in
  let log_exit _ ctxt loc sty stack =
    log := With_stack (ctxt, loc, stack, sty, 0) :: !log
  in
  let log_control _ = () in
  let get_log () = assert false in
  let assemble_log () =
    let+ l =
      List.map_es
        (fun (With_stack (ctxt, loc, stack, stack_ty, _)) ->
          let+ stack =
            Lwt.map Environment.wrap_tzresult
            @@ Traced_interpreter.unparse_stack ctxt (stack, stack_ty)
          in
          TInstr (loc, Gas.level ctxt, stack))
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
