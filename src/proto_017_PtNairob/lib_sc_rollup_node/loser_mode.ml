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

type failure = {level : int; message_index : int; message_tick : int64}

let failure_encoding =
  let open Data_encoding in
  conv
    (fun {level; message_index; message_tick} ->
      (level, message_index, message_tick))
    (fun (level, message_index, message_tick) ->
      {level; message_index; message_tick})
    (obj3
       (req "level" int31)
       (req "message_index" int31)
       (req "message_tick" int64))

let compare_failure {level; message_index; message_tick} f2 =
  let open Compare.Int in
  match compare level f2.level with
  | 0 -> (
      match compare message_index f2.message_index with
      | 0 -> Int64.compare message_tick f2.message_tick
      | n -> n)
  | n -> n

type t = failure list

let encoding = Data_encoding.list failure_encoding

let no_failures = []

let make s =
  let tokens = String.split_on_char ' ' s in
  let rec chop = function
    | [] | [""] -> []
    | level :: message_index :: message_tick :: rest ->
        {
          level = int_of_string level;
          message_index = int_of_string message_index;
          message_tick = Int64.of_string message_tick;
        }
        :: chop rest
    | _ -> raise Not_found
  in
  try Some (chop tokens |> List.sort compare_failure) with _ -> None

let is_failure failures ~level ~message_index =
  List.filter_map
    (fun f ->
      if Compare.Int.(f.level = level && f.message_index = message_index) then
        Some f.message_tick
      else None)
    failures
