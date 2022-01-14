(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let error message =
  prerr_endline message;
  exit 1

let () =
  if Array.length Sys.argv <> 3 then
    error
      "Usage: dune exec \
       scripts/declare-new-protocol-unit-test/main.exe -- XXX YYYYYYYY";
  let proto_version = Sys.argv.(1) in
  let proto_short_hash = Sys.argv.(2) in
  let new_proto_name = proto_version ^ "_" ^ proto_short_hash in
  let in_ch = open_in ".gitlab/ci/test/unit.yml" in
  Fun.protect ~finally: (fun () -> close_in in_ch) @@ fun () ->
  let out_ch = open_out ".gitlab/ci/test/unit2.yml" in
  Fun.protect ~finally: (fun () -> close_out out_ch) @@ fun () ->
  let output_line line =
    output_string out_ch line;
    output_char out_ch '\n'
  in
  let replace line =
    Re.replace_string (Re.compile (Re.str "alpha")) ~by: new_proto_name line
  in
  let rec read_and_write_the_rest () =
    match input_line in_ch with
      | exception End_of_file ->
          ()
      | line ->
          output_line line;
          read_and_write_the_rest ()
  in
  let rec continue_reading_unit_alpha unit_alpha_lines =
    match input_line in_ch with
      | exception End_of_file | "" ->
          (* Found the end of what we were looking for. *)
          (* Write unit:alpha: job, and the rest of the file. *)
          output_line "";
          List.iter output_line (List.rev unit_alpha_lines);
          read_and_write_the_rest ()
      | line ->
          (* Still parsing the unit:alpha: job, replace it as we go. *)
          output_line (replace line);
          continue_reading_unit_alpha (line :: unit_alpha_lines)
  in
  let rec find_unit_alpha () =
    match input_line in_ch with
      | exception End_of_file ->
          error
            "End of file reached before seeing unit:alpha: - check \
             .gitlab/ci/test/unit.yml"
      | "unit:alpha:" as line ->
          (* Found the job we were looking for, start replacing it. *)
          output_line (replace line);
          continue_reading_unit_alpha [ line ]
      | line ->
          output_line line;
          find_unit_alpha ()
  in
  find_unit_alpha ();
  Sys.rename ".gitlab/ci/test/unit2.yml" ".gitlab/ci/test/unit.yml"
