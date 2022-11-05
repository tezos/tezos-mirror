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

(* The input CSV files have only two rows: one for the parameter name
   and one for the inferred value. *)
type csv_input_column = string * string

(* But they can have any number of columns. *)
type csv_input_table = csv_input_column list

let parse_input_csv file : csv_input_table =
  let chan = open_in file in
  let first_line = String.split_on_char ',' (input_line chan) in
  let second_line = String.split_on_char ',' (input_line chan) in
  assert (List.length first_line = List.length second_line) ;
  List.combine first_line second_line

(* Returns names1 @ List.map fst table2 but without the duplicates *)
let merge_params names1 table2 =
  names1
  @ List.filter_map
      (fun (name, _value) -> if List.mem name names1 then None else Some name)
      table2

let () =
  let len = Array.length Sys.argv - 1 in
  let tables = Array.init len (fun i -> parse_input_csv Sys.argv.(i + 1)) in
  let all_param_names = Array.fold_left merge_params [] tables in
  List.iter
    (fun name ->
      Printf.printf "%s," name ;
      Array.iter
        (fun table ->
          let value = List.assoc_opt name table in
          (match value with
          | None -> ()
          | Some value -> Printf.printf "%s" value) ;
          Printf.printf ",")
        tables ;
      Printf.printf "\n")
    all_param_names
