(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(* Testing
   -------
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file bad_annot.ml
   Subject:      Tests the UNPACK instruction on ill-annotated input
*)

let test_bad_annot client protocol () =
  (* This was produced by running "octez-client hash data '{ UNIT
     ; PAIR ; CAR %faa }' of type 'lambda unit unit'" and
     replacing the two last bytes (that correspond to the two
     'a's at the end of the annotation) by the 0xff byte which is
     not a valid UTF8-encoding of a string *)
  let input = "0x05020000000e034f03420416000000042566ffff" in
  let* {storage; _} =
    Client.run_script_at
      ~storage:"None"
      ~input
      client
      ["non_regression"; "bad_annot_contract"]
      protocol
  in
  Check.(
    ("None" = storage) ~__LOC__ string ~error_msg:"Expected result %R, got %L") ;
  unit

let register ~protocols =
  List.iter
    (fun (title, test_function) ->
      Protocol.register_test
        ~__FILE__
        ~title
        ~tags:["client"; "michelson"]
        (fun protocol ->
          let* client = Client.init_mockup ~protocol () in
          test_function client protocol ())
        protocols)
    [("Test bad annotation", test_bad_annot)]
