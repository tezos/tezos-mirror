(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

let parse url process =
  let* output = Process.check_and_read_stdout process in
  return (JSON.parse ~origin:url output)

let get0 ?name ?runner ?(args = []) url format =
  let process = Process.spawn ?name ?runner "curl" (args @ ["-s"; url]) in
  Runnable.{value = process; run = format}

let get ?name ?runner ?args url = get0 ?name ?runner ?args url (parse url)

let get_raw ?name ?runner ?args url =
  get0 ?name ?runner ?args url Process.check_and_read_stdout

let post_put ?name meth ?runner ?(args = []) url data format =
  let process =
    Process.spawn
      ?runner
      ?name
      "curl"
      (args
      @ [
          "-X";
          meth;
          "-H";
          "Content-Type: application/json";
          "-s";
          url;
          "-d";
          JSON.encode data;
        ])
  in
  Runnable.{value = process; run = format}

let post ?name ?runner ?args url data =
  post_put ?name "POST" ?runner ?args url data (parse url)

let post_raw ?name ?runner ?args url data =
  post_put ?name "POST" ?runner ?args url data Process.check_and_read_stdout

let put ?name ?runner ?args url data =
  post_put ?name "PUT" ?runner ?args url data (parse url)

let put_raw ?name ?runner ?args url data =
  post_put ?name "PUT" ?runner ?args url data Process.check_and_read_stdout
