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
open Tezt

let logger : Script_typed_ir.logger =
  let log = Regression.capture in
  let log_interp _ _ctxt _loc _sty _stack = log "interp" in
  let log_entry _ _ctxt _loc _sty _stack = log "entry" in
  let log_exit _ _ctxt _loc _sty _stack = log "exit" in
  let log_control _ = log "ctrl" in
  let get_log () = return_none in
  {log_exit; log_entry; log_interp; get_log; log_control}

let test_context () =
  let* b, _cs = Context.init1 ~consensus_threshold:0 () in
  let* v = Incremental.begin_construction b in
  return (Incremental.alpha_ctxt v)

let first_reg_test () =
  let* ctxt = test_context () in
  let script = Contract_helpers.read_file "contracts/add_to_store.tz" in
  let* _res, _ctxt =
    Contract_helpers.run_script
      ctxt
      script
      ~logger
      ~storage:"0"
      ~parameter:"2"
      ()
  in
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
