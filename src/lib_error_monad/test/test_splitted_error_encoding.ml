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

(** Testing
    -------
    Component:    Error Monad
    Invocation:   dune exec src/lib_error_monad/test/main.exe \
                  -- --file test_splitted_error_encoding.ml
    Subject:      On the wrapping of error_encoding in a splitted.
*)

open TzCore

let splitted_error_encoding =
  (* As documented in [Sig.Core.error_encoding], this use of [error_encoding]
     suffers from significant drawbacks. Specifically, The json-encoding via
     [splitted] is not updated when new errors are registered. *)
  Data_encoding.splitted ~json:error_encoding ~binary:error_encoding

type error += Foo

let test_unregistered_binary () =
  (* The error was declared but not registered so we expect all encoding
     attempts to fail. *)
  let e = Foo in
  let blob = Data_encoding.Binary.to_string_exn error_encoding e in
  let ee = Data_encoding.Binary.of_string_exn error_encoding blob in
  (match ee with Unregistered_error _ -> () | _ -> assert false) ;
  let sblob = Data_encoding.Binary.to_string_exn splitted_error_encoding e in
  let see = Data_encoding.Binary.of_string_exn error_encoding sblob in
  (match see with Unregistered_error _ -> () | _ -> assert false) ;
  ()

let test_unregistered_json () =
  (* The error was declared but not registered so we expect all encoding
     attempts to fail. *)
  let e = Foo in
  let json = Data_encoding.Json.construct error_encoding e in
  let ee = Data_encoding.Json.destruct error_encoding json in
  (match ee with Unregistered_error _ -> () | _ -> assert false) ;
  let sjson = Data_encoding.Json.construct splitted_error_encoding e in
  let see = Data_encoding.Json.destruct error_encoding sjson in
  (match see with Unregistered_error _ -> () | _ -> assert false) ;
  ()

let test_registered () =
  (* We register the error. *)
  register_error_kind
    `Permanent
    ~id:"test.Foo"
    ~title:"test-foo"
    ~description:"Test Foo"
    Data_encoding.(obj1 (req "test-foo" Data_encoding.unit))
    (function Foo -> Some () | _ -> None)
    (fun () -> Foo) ;
  (* We check that binary encoding works fine. *)
  let e = Foo in
  let blob = Data_encoding.Binary.to_string_exn error_encoding e in
  let ee = Data_encoding.Binary.of_string_exn error_encoding blob in
  assert (ee = Foo) ;
  let see = Data_encoding.Binary.of_string_exn splitted_error_encoding blob in
  assert (see = Foo) ;
  let sblob = Data_encoding.Binary.to_string_exn splitted_error_encoding e in
  let ssee = Data_encoding.Binary.of_string_exn error_encoding sblob in
  assert (ssee = Foo) ;
  (* and now JSON *)
  let json = Data_encoding.Json.construct error_encoding e in
  let ee = Data_encoding.Json.destruct error_encoding json in
  assert (ee = Foo) ;
  (* and now JSON with splitted. This fail as per the documentation of
     [error_encoding] (see [sig.ml]). *)
  let sjson = Data_encoding.Json.construct splitted_error_encoding e in
  let see = Data_encoding.Json.destruct error_encoding sjson in
  (match see with Unregistered_error _ -> () | _ -> assert false) ;
  let ssee = Data_encoding.Json.destruct splitted_error_encoding json in
  (match ssee with Unregistered_error _ -> () | _ -> assert false) ;
  ()

let tests =
  [
    Alcotest.test_case "unregistered(binary)" `Quick test_unregistered_binary;
    Alcotest.test_case "unregistered(json)" `Quick test_unregistered_json;
    Alcotest.test_case "registered" `Quick test_registered;
  ]

let () = Alcotest.run ~__FILE__ "splitted" [("splitted", tests)]
