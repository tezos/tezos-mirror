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

open Tezt_core
open Tezt_core.Base

type return = unit Lwt.t

type speed_level = [`Quick | `Slow]

type 'a test_case = string * speed_level * ('a -> return)

let test_case name speed_level body =
  (* Not sure this is the right way to create the switch, maybe it should have
     a larger scope. *)
  let body args = Lwt_switch.with_switch (fun sw -> body sw args) in
  (name, speed_level, body)

let test_case_sync name speed_level body =
  let body arg =
    body arg ;
    unit
  in
  (name, speed_level, body)

type 'a test = string * 'a test_case list

let run ~__FILE__ library_name tests =
  let proto_tags = Alcotezt_utils.is_proto_test __FILE__ in
  (tests
  |> List.iter @@ fun (test_name, test_cases) ->
     test_cases
     |> List.iter @@ fun (test_case_name, speed_level, body) ->
        let tags =
          "alcotezt"
          :: (match speed_level with `Quick -> ["quick"] | `Slow -> ["slow"])
          @ proto_tags
        in
        Test.register
          ~__FILE__
          ~title:(library_name ^ ": " ^ test_name ^ " (" ^ test_case_name ^ ")")
          ~tags
        @@ fun () -> body ()) ;
  unit
