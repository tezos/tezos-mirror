(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* This test is for stress-testing the cases of strings-heavy objects with
   various sizes of blitting. It only searches for exceptions in the serialising
   process, not for round-trip errors. *)

type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

let strings =
  (* statically allocated collection *)
  let char n =
    [|'0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'|].(n mod 10)
  in
  Array.init 146 (fun i -> String.init i char)

let string =
  let open Crowbar in
  map [range 146] (Array.get strings)

let ezjson : json Crowbar.gen =
  let open Crowbar in
  fix (fun json ->
      let field = map [string; json] (fun n j -> (n, j)) in
      choose
        [
          map [list json] (fun jsons -> `A jsons);
          map [string] (fun s -> `String s);
          map
            [string; string; string; string; string; string]
            (fun s1 s2 s3 s4 s5 s6 ->
              `A
                [
                  `String s1;
                  `String s2;
                  `String s3;
                  `String s4;
                  `String s5;
                  `String s6;
                ]);
          const `Null;
          map [list field] (fun kvs -> `O kvs);
        ])

let () =
  let open Crowbar in
  add_test
    ~name:"blitting with a lot of string boundaries"
    [range ~min:32 16; ezjson]
    (fun buff_size ezj ->
      let j = Json_encoding.jsonm_lexeme_seq_of_ezjson ezj in
      let buffer = Bytes.create buff_size in
      let s =
        Data_encoding.Json.blit_instructions_seq_of_jsonm_lexeme_seq
          ~newline:false
          ~buffer
          j
      in
      Seq.iter ignore s)

let () =
  let open Crowbar in
  add_test
    ~name:"string-seq with a lot of string boundaries"
    [range ~min:32 16; ezjson]
    (fun chunk_size_hint ezj ->
      let j = Json_encoding.jsonm_lexeme_seq_of_ezjson ezj in
      let s =
        Data_encoding.Json.string_seq_of_jsonm_lexeme_seq
          ~newline:false
          ~chunk_size_hint
          j
      in
      Seq.iter ignore s)
