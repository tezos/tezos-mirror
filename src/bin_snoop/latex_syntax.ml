(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@tezos.com>                       *)
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

type t = {title : string; sections : section list}

and section = Section of string * section_content list

and section_content =
  | Text of text
  | Figure of caption * image
  | Table of table

and text = blob list

and blob = Text_blob of style * string | Inline_math_blob of string

and style = Normal | Emph | Bold

and image = {filename : string; size : image_size option}

and image_size = Width_cm of int

and caption = text

and table = table_spec * row list

and row = Row of text list | Hline

and table_spec = spec list

and spec = L | C | R | Vbar

let spec_width : table_spec -> int =
 fun spec ->
  List.fold_left
    (fun acc spec -> match spec with L | C | R -> acc + 1 | _ -> acc)
    0
    spec

let row_width : row -> int option = function
  | Row texts -> Some (List.length texts)
  | Hline -> None

let rec map_string f (toplevel : t) =
  {
    title = f toplevel.title;
    sections = List.map (map_string_section f) toplevel.sections;
  }

and map_string_section f (s : section) =
  match s with
  | Section (name, contents) ->
      Section (f name, List.map (map_string_section_content f) contents)

and map_string_section_content f (c : section_content) =
  match c with
  | Text blobs -> Text (map_text f blobs)
  | Figure (caption, image) -> Figure (map_text f caption, image)
  | Table (spec, rows) -> Table (spec, List.map (map_string_row f) rows)

and map_text f blobs = List.map (map_string_blob f) blobs

and map_string_blob f (b : blob) =
  match b with
  | Text_blob (style, s) -> Text_blob (style, f s)
  | Inline_math_blob s -> Inline_math_blob (f s)

and map_string_row f (r : row) =
  match r with Row texts -> Row (List.map (map_text f) texts) | Hline -> r
