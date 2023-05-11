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
  let usage_error_string = "Expecting a CSV file with exactly two lines." in
  let read_a_line () =
    try input_line chan
    with _ ->
      Printf.eprintf
        "Not enough lines in input %s. %s\n"
        file
        usage_error_string ;
      exit 1
  in
  let first_line = String.split_on_char ',' (read_a_line ()) in
  let second_line = String.split_on_char ',' (read_a_line ()) in
  (try
     let _ = input_line chan in
     Printf.eprintf "Too many lines in input %s. %s\n" file usage_error_string ;
     exit 1
   with _ -> ()) ;
  if List.length first_line <> List.length second_line then (
    Printf.eprintf
      "The two lines of the input CSV file %s must have the same number of \
       columns.\n"
      file ;
    exit 1)
  else List.combine first_line second_line

(* Returns names1 @ List.map fst table2 but without the duplicates *)
let merge_params names1 (table2, _file_name) =
  names1
  @ List.filter_map
      (fun (name, _value) -> if List.mem name names1 then None else Some name)
      table2

let read_num s = try Some (float_of_string s) with _ -> None

let lift f a b =
  match (a, b) with
  | Some a, Some b -> Some (f a b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> None

let () =
  let len = Array.length Sys.argv - 1 in
  let tables =
    Array.init len (fun i ->
        let file = Sys.argv.(i + 1) in
        (parse_input_csv file, file))
  in
  let all_param_names = Array.fold_left merge_params [] tables in

  (* Output the title line *)
  Printf.printf "," ;
  Array.iter (fun (_table, file_name) -> Printf.printf "%s," file_name) tables ;
  Printf.printf ",MIN,MAX,DIFF,CHANGE (%%)\n" ;

  (* Output the content lines *)
  List.iter
    (fun name ->
      Printf.printf "%s," name ;
      let current_min = ref None in
      let current_max = ref None in
      Array.iter
        (fun (table, _file_name) ->
          let value = List.assoc_opt name table in
          (match value with
          | None -> ()
          | Some value ->
              let v = read_num value in
              Printf.printf "%s" value ;
              current_min := lift min !current_min v ;
              current_max := lift max !current_max v) ;
          Printf.printf ",")
        tables ;
      match (!current_min, !current_max) with
      | Some final_min, Some final_max ->
          let diff = final_max -. final_min in
          let change =
            let divisor = max (abs_float final_min) (abs_float final_max) in
            if divisor = 0. then (* in that case all values are null *) 0.
            else 100. *. diff /. divisor
          in
          Printf.printf ",%f,%f,%f,%f\n" final_min final_max diff change ;
          let is_const =
            String.ends_with ~suffix:"const" name
            || String.ends_with ~suffix:"intercept" name
          in
          if change > 20. && ((not is_const) || diff > 2.) then
            Printf.eprintf "%f%% regression for %s.\n" change name
      | _, _ -> Printf.printf ",,,,\n")
    all_param_names ;

  (* Report parameters disappearing or appearing between the last two files. *)
  if len >= 2 then
    let before_last, _ = tables.(len - 2) in
    let last, _ = tables.(len - 1) in
    List.iter
      (fun name ->
        match (List.mem_assoc name before_last, List.mem_assoc name last) with
        | false, false | true, true -> ()
        | true, false -> Printf.eprintf "%s is removed.\n" name
        | false, true -> Printf.eprintf "%s is new.\n" name)
      all_param_names
