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

(* Pretty printing of the generic syntax to latex *)

let rec pp : Format.formatter -> Latex_syntax.t -> unit =
 fun fmtr text ->
  pp_preamble fmtr text.title ;
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "@.@.")
    pp_section
    fmtr
    text.sections ;
  pp_conclusion fmtr

and pp_preamble fmtr title =
  Format.fprintf
    fmtr
    "\\documentclass[10pt]{article}\n\n\
     \\usepackage[a4paper,top=2cm,bottom=2cm,outer=3cm,inner=3cm]{geometry}\n\n\
     \\usepackage{times}\n\
     \\usepackage{listings}\n\
     \\usepackage{url,xspace}\n\
     \\usepackage{graphicx}\n\
     \\usepackage{amsmath}\n\
     \\usepackage{amsfonts}\n\
     \\usepackage{amssymb}\n\
     \\usepackage{color}\n\
     \\usepackage{rotating}\n\
     \\usepackage[absolute]{textpos}\n\
     \\usepackage{helvet}\n\
     \\usepackage{wrapfig}\n\
     \\usepackage{array}\n\
     \\usepackage{hyperref}\n\
     \\usepackage[all]{xy}\n\
     \\usepackage{mathrsfs}\n\
     \\usepackage{longtable}\n\
     \\usepackage{pbox}\n\
     \\usepackage{float}\n\
     \\usepackage[export]{adjustbox}\n\
     \\usepackage{libertine}\n\n\
     \\title{%s}\n\n\
     \\begin{document}\n\n\
     \\maketitle"
    title

and pp_conclusion fmtr =
  Format.fprintf
    fmtr
    "\\small\n\
     \\bibliography{notes}\n\
     \\bibliographystyle{plain}\n\n\
     \\end{document}\n"

and pp_section : Format.formatter -> Latex_syntax.section -> unit =
 fun fmtr section ->
  match section with
  | Section (name, contents) ->
      Format.fprintf fmtr "\\section{%s}@." name ;
      Format.pp_print_list
        ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "@.@.")
        pp_section_content
        fmtr
        contents

and pp_section_content :
    Format.formatter -> Latex_syntax.section_content -> unit =
 fun fmtr section_content ->
  match section_content with
  | Text text -> pp_text fmtr text
  | Table table -> pp_table fmtr table
  | Figure (caption, {filename; size}) ->
      Format.fprintf fmtr "\\begin{figure}[H]@." ;
      Format.fprintf
        fmtr
        "\\includegraphics%a{%s}@."
        pp_image_size
        size
        filename ;
      Format.fprintf fmtr "\\caption{%a}@." pp_text caption ;
      Format.fprintf fmtr "\\end{figure}@."

and pp_image_size : Format.formatter -> Latex_syntax.image_size option -> unit =
 fun fmtr image_size_opt ->
  match image_size_opt with
  | None -> Format.fprintf fmtr ""
  | Some (Width_cm i) -> Format.fprintf fmtr "[width=%dcm]" i

and pp_text : Format.formatter -> Latex_syntax.text -> unit =
 fun fmtr text ->
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr " ")
    pp_blob
    fmtr
    text

and pp_blob : Format.formatter -> Latex_syntax.blob -> unit =
 fun fmtr blob ->
  match blob with
  | Text_blob (style, text) -> (
      match style with
      | Normal -> Format.fprintf fmtr "%s" text
      | Emph -> Format.fprintf fmtr "\\textit{%s}" text
      | Bold -> Format.fprintf fmtr "\\textbf{%s}" text)
  | Inline_math_blob text -> Format.fprintf fmtr "$%s$" text

and pp_table : Format.formatter -> Latex_syntax.table -> unit =
 fun fmtr table ->
  match table with
  | spec, rows ->
      let width = Latex_syntax.spec_width spec in
      if
        not
          (List.for_all
             (fun row ->
               match Latex_syntax.row_width row with
               | None -> true
               | Some w -> w = width)
             rows)
      then Stdlib.failwith "ill-formed table (bad width)" ;
      Format.fprintf fmtr "\\begin{center}@." ;
      Format.fprintf fmtr "\\begin{longtable}{%a}@." pp_table_spec spec ;
      Format.pp_print_list pp_row fmtr rows ;
      Format.fprintf fmtr "\\end{longtable}@." ;
      Format.fprintf fmtr "\\end{center}@."

and pp_table_spec : Format.formatter -> Latex_syntax.table_spec -> unit =
 fun fmtr table_spec ->
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "")
    pp_spec
    fmtr
    table_spec

and pp_spec : Format.formatter -> Latex_syntax.spec -> unit =
 fun fmtr spec ->
  match spec with
  | L -> Format.fprintf fmtr "l"
  | C -> Format.fprintf fmtr "c"
  | R -> Format.fprintf fmtr "r"
  | Vbar -> Format.fprintf fmtr "|"

and pp_row : Format.formatter -> Latex_syntax.row -> unit =
 fun fmtr row ->
  match row with
  | Row texts ->
      Format.pp_print_list
        ~pp_sep:(fun fmtr () -> Format.fprintf fmtr " & ")
        pp_cell_text
        fmtr
        texts ;
      Format.fprintf fmtr " \\\\@."
  | Hline -> Format.fprintf fmtr "\\hline@."

and pp_cell_text : Format.formatter -> Latex_syntax.text -> unit =
 fun fmtr text ->
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr " ")
    pp_cell_blob
    fmtr
    text

and pp_cell_blob : Format.formatter -> Latex_syntax.blob -> unit =
 fun fmtr blob ->
  let blob =
    match blob with
    | Latex_syntax.Text_blob (style, text) ->
        let chunks = String.split_on_char '\n' text in
        let text =
          match chunks with
          | [] | [_] -> text
          | _ ->
              let s = String.concat " \\\\ " chunks in
              Format.asprintf "\\pbox{20cm}{%s}" s
        in
        Latex_syntax.Text_blob (style, text)
    | Inline_math_blob _ -> blob
  in
  pp_blob fmtr blob
