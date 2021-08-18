(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pp_print_quoted_string ppf str = Format.fprintf ppf "\"%s\"" str

let pp_print_quoted_string_list ppf strs =
  Format.pp_print_list
    ~pp_sep:(fun ppf () ->
      Format.pp_print_string ppf ";" ;
      Format.pp_print_space ppf ())
    pp_print_quoted_string
    ppf
    strs

let read_lines filename =
  let ic = open_in filename in
  let rec loop acc ic =
    match input_line ic with
    | exception End_of_file ->
        close_in ic ;
        List.rev acc
    | exception exn ->
        close_in ic ;
        raise exn
    | x -> loop (x :: acc) ic
  in
  loop [] ic

let gen ml =
  let txt = "gen/" ^ Filename.remove_extension ml ^ ".txt" in
  let words = read_lines txt in
  let oc = open_out ml in
  let ppf = Format.formatter_of_out_channel oc in
  let () =
    Format.fprintf
      ppf
      "@[<v 2>let words =@ [ @[<v 0>%a@] ]@]@."
      pp_print_quoted_string_list
      words
  in
  close_out oc

let () = Array.to_list Sys.argv |> List.tl |> List.iter gen

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
