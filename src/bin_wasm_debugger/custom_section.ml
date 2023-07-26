(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(**
   This files implements the parsing of custom subsection, especially the `name`
   custom section (see
   https://webassembly.github.io/spec/core/appendix/custom.html#name-section).

   The `name` section has the following format:
   [h] [len] [vec_len:n] ([index] [name_len] [name])^n
   where
   - [h] is a tag encoded in a single byte (`1` for the functions subsection)
   - [len] is a variable-length unsigned 32bits integer (`vu32`), which is
     the length of the subsection
   - [vec_len] (`vu32`) encoding the number of values in the vector
   then for each value of the vector:
     - [index] (`vu32`) encoding the function representation
     - [name_len] (`vu32`) encoding the length in bytes of the name
     - [name] (`utf8`) bytes of length `name_len` encoding an utf8
       representation of the symbol

*)

(* Adapted from {Tezos_lib_webassembly.Decode} *)
let rec vuN n bytes index =
  let b, next_index = (String.get bytes index |> Char.code, succ index) in
  assert (n >= 7 || b land 0x7f < 1 lsl n) ;
  let x = Int64.of_int (b land 0x7f) in
  if b land 0x80 = 0 then (x, next_index)
  else
    let v, next_index = vuN (n - 7) bytes next_index in
    (Int64.(logor x (shift_left v 7)), next_index)

let vu32 bytes index =
  let value, next_index = vuN 32 bytes index in
  (Int64.to_int32 value, next_index)

(** [parse_subsection_header bytes index] reads the tag for the subsection and
    its length, and returns the next index to continue reading. Returns `None`
    if there are not at least 2 bytes to read. *)
let parse_subsection_header bytes start =
  (* At least two string: one for the header, and at least one for the length of
     the subsection. *)
  if String.length bytes < start + 2 then None
  else
    let len, next_index = vu32 bytes (start + 1) in
    Some (String.get bytes 0, len, next_index)

let u32_to_int u =
  match Int32.unsigned_to_int u with None -> assert false | Some i -> i

(** [get_function_name_section_indexes bytes] returns the starting index of the
    `functions` subsection and its length. *)
let get_function_name_section_indexes bytes =
  let rec parse next_index =
    match parse_subsection_header bytes next_index with
    | None -> None
    | Some ('\001', len, next_index) -> Some (next_index, len)
    | Some (_, len, next_index) -> parse (next_index + u32_to_int len)
  in
  parse 0

(** [parse_nameassoc bytes index] parses a `(index, name)` encoded value and
    returns the index to continue the reading. *)
let parse_nameassoc bytes start =
  let idx, next_index = vu32 bytes start in
  let name_len, start_index = vu32 bytes next_index in
  let name_len = u32_to_int name_len in
  let buffer = Buffer.create name_len in
  let rec decode string index =
    if index >= name_len + start_index then index
    else
      let uchar = String.get_utf_8_uchar string index in
      (if Uchar.utf_decode_is_valid uchar then
       let u = Uchar.utf_decode_uchar uchar in
       if Uchar.is_char u then Buffer.add_char buffer (Uchar.to_char u)) ;
      decode string (index + Uchar.utf_decode_length uchar)
  in
  let index = decode bytes start_index in
  let name = Buffer.contents buffer in
  ((idx, name), index)

module FuncMap = Map.Make (Int32)

(** [parse_vec bytes start parse_value] parses an encoded vector and its values
    with [parse_value]. *)
let parse_vec bytes start parse_value =
  let len, next_index = vu32 bytes start in
  let len = u32_to_int len in
  let rec parse_values index nth acc =
    if nth >= len then acc
    else
      let value, next_index = parse_value bytes index in
      parse_values next_index (succ nth) (Seq.cons value acc)
  in
  parse_values next_index 0 Seq.empty

(** [parse_function_subsection bytes] parse and returns the `functions`
    subsection, as described by the reference documentation. *)
let parse_function_subsection subsection =
  match get_function_name_section_indexes subsection with
  | None -> FuncMap.empty
  | Some (start, _len) ->
      parse_vec subsection start parse_nameassoc |> FuncMap.of_seq

(** [pp_function_subsection ppf map] pretty-prints the parsed functions
    subsection. *)
let pp_function_subsection ppf map =
  let pp_assoc ppf (idx, name) =
    Format.fprintf ppf " - func[%ld] <%s>" idx name
  in
  FuncMap.to_seq map
  |> Format.pp_print_seq
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
       pp_assoc
       ppf

let parse_custom_sections name module_ =
  let open Lwt_syntax in
  let bytes = Tezos_lazy_containers.Chunked_byte_vector.of_string module_ in
  let+ custom =
    Tezos_webassembly_interpreter.Decode.decode_custom "name" ~name ~bytes
  in
  let functions_section =
    List.map parse_function_subsection custom
    |> List.fold_left (FuncMap.merge (fun _ -> Option.either)) FuncMap.empty
  in
  functions_section
