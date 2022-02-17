(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type csv = string list list

let all_equal (l : int list) =
  let rec loop l (elt : int) =
    match l with [] -> true | hd :: tl -> hd = elt && loop tl elt
  in
  match l with [] -> true | hd :: tl -> loop tl hd

(* Horizontally concat CSVs *)
let concat (csv1 : csv) (csv2 : csv) : csv =
  (* Check that both CSVs have the same number of lines. *)
  if Compare.List_lengths.(csv1 <> csv2) then
    Stdlib.failwith "Csv.concat: CSVs have different length"
  else
    (* Check that each CSV has the same number of *)
    let lengths1 = List.map List.length csv1 in
    let lengths2 = List.map List.length csv1 in
    if not (all_equal lengths1) then
      let msg = "Csv.concat: first argument has uneven # of lines" in
      Stdlib.failwith msg
    else if not (all_equal lengths2) then
      let msg = "Csv.concat: first argument has uneven # of lines" in
      Stdlib.failwith msg
    else
      (* see top if condition *)
      WithExceptions.List.map2
        ~loc:__LOC__
        (fun line1 line2 -> line1 @ line2)
        csv1
        csv2

let export ~filename ?(separator = ',') ?(linebreak = '\n') (data : csv) =
  Format.eprintf "Exporting to %s@." filename ;
  let sep_str = String.make 1 separator in
  let outfile = open_out filename in
  let fmtr = Format.formatter_of_out_channel outfile in
  List.iter
    (fun line ->
      match line with
      | [] -> ()
      | _ ->
          let s = String.concat sep_str line in
          Format.fprintf fmtr "%s%c@?" s linebreak)
    data ;
  close_out outfile

(* shamelessly stolen from
   https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
let read_lines name : string list =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic ;
        List.rev acc
  in
  loop []

exception Empty_csv_file

let import ~filename ?(separator = ',') () : csv =
  Format.eprintf "Importing %s@." filename ;
  let lines = read_lines filename in
  let (header, rows) =
    match lines with
    | [] -> raise Empty_csv_file
    | header :: tail -> (header, tail)
  in
  let header = String.split_on_char separator header in
  let ncols = List.length header in
  let rows = List.map (String.split_on_char separator) rows in
  if not (List.for_all (fun l -> Compare.List_length_with.(l = ncols)) rows)
  then Stdlib.failwith "Csv.import: mismatch between header width and row width" ;
  header :: rows

let append_columns ~filename ?(separator = ',') ?(linebreak = '\n') (data : csv)
    =
  let file_data =
    try import ~filename ~separator ()
    with Sys_error _ | Empty_csv_file ->
      (* If the target file does not exist or is empty, we create a dummy
         CSV matrix with the expected dimensions. *)
      List.map (fun _ -> []) data
  in
  let csv_data = concat file_data data in
  export ~filename ~separator ~linebreak csv_data

let export_stdout ?(separator = ',') ?(linebreak = '\n') (data : csv) =
  Format.eprintf "Exporting to stdout@." ;
  let sep_str = String.make 1 separator in
  List.iter
    (fun line ->
      let s = String.concat sep_str line in
      Format.printf "%s%c" s linebreak)
    data ;
  flush stdout
