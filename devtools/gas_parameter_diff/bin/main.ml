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

type csv_input_column = string * string

type csv_input_table = csv_input_column list

let parse_input_csv file : csv_input_table =
  let chan = open_in file in
  let first_line = String.split_on_char ',' (input_line chan) in
  let second_line = String.split_on_char ',' (input_line chan) in
  assert (List.length first_line = List.length second_line) ;
  List.combine first_line second_line

let () =
  assert (Array.length Sys.argv = 3) ;
  let file1 = Sys.argv.(1) in
  let file2 = Sys.argv.(2) in
  let table1 = parse_input_csv file1 in
  let table2 = parse_input_csv file2 in
  if List.length table1 <> List.length table2 then
    Printf.eprintf
      "%s has %d entries but %s has %d entries\n"
      file1
      (List.length table1)
      file2
      (List.length table2) ;
  List.iter
    (fun (name, _value2) ->
      if not (List.mem_assoc name table1) then
        Printf.eprintf "%s not found in %s\n" name file1)
    table2 ;
  List.iter
    (fun (name, value1) ->
      let value2 = List.assoc_opt name table2 in
      match value2 with
      | None -> Printf.eprintf "%s not found in %s\n" name file2
      | Some value2 -> Printf.printf "%s,%s,%s\n" name value1 value2)
    table1
