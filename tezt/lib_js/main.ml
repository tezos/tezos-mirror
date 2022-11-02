(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Base

module Scheduler : Test.SCHEDULER = struct
  type request = Run_test of {test_title : string}

  type response = Test_result of Log.test_result

  let run_test test_title =
    match Test.get_test_by_title test_title with
    | None ->
        Log.error
          "scheduler requested to run test %S, but worker doesn't know about \
           this test"
          test_title ;
        exit 1
    | Some test ->
        let clean_up () = unit in
        let* test_result =
          Test.run_one
            ~sleep:Js_of_ocaml_lwt.Lwt_js.sleep
            ~clean_up
            ~temp_start:(fun () ->
              "(temporary files are not implemented for JS)")
            ~temp_stop:(fun () -> ())
            ~temp_clean_up:(fun () -> ())
            test
        in
        return (Test_result test_result)

  let rec run ~on_worker_available continue =
    match on_worker_available () with
    | Some (Run_test {test_title}, handle_response) ->
        let* response = run_test test_title in
        handle_response response ;
        run ~on_worker_available continue
    | None ->
        continue () ;
        unit

  let run ~on_worker_available ~worker_count continue =
    if worker_count <> 1 then
      Log.warn "The -j argument is ignored when using Tezt_js.run." ;
    let (_ : unit Lwt.t) = run ~on_worker_available continue in
    ()

  let get_current_worker_id () = None
end

let run () = Test.run_with_scheduler (module Scheduler)
