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

open Data_encoding

let test1 () =
  match check_size (-1) string with
  | exception Invalid_argument _ -> ()
  | e -> (
      let v = "12345" in
      match Binary.to_string e v with
      | Error Size_limit_exceeded -> ()
      | Error _ -> failwith "different error than expected"
      | Ok _ -> failwith "unexcepted success")

let test2 () =
  match check_size (-2) string with
  | exception Invalid_argument _ -> ()
  | e -> (
      let v = "12345" in
      match Binary.to_string e v with
      | Error Size_limit_exceeded -> ()
      | Error _ -> failwith "different error than expected"
      | Ok _ -> failwith "unexcepted success")

let test3 () =
  let v = "12345" in
  let s = Binary.to_string_exn string v in
  match check_size (-1) string with
  | exception Invalid_argument _ -> ()
  | e -> (
      match Binary.of_string e s with
      | Error Size_limit_exceeded -> ()
      | Error _ -> failwith "different error than expected"
      | Ok _ -> failwith "unexcepted success")

let tests =
  [
    ("write fails", `Quick, test1);
    ("write fails (with -2)", `Quick, test2);
    ("read fails", `Quick, test3);
  ]
