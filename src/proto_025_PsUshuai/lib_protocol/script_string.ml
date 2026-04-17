(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Strings of printable characters *)

type repr = string (* Invariant: contains only printable characters *)

type t = String_tag of repr [@@ocaml.unboxed]

type error += Non_printable_character of (int * string)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"michelson_v1.non_printable_character"
    ~title:"Non printable character in a Michelson string"
    ~description:
      "Michelson strings are only allowed to contain printable characters \
       (either the newline character or characters in the [32, 126] ASCII \
       range)."
    ~pp:(fun ppf (pos, s) ->
      Format.fprintf
        ppf
        "In Michelson string \"%s\", character at position %d has ASCII code \
         %d. Expected: either a newline character (ASCII code 10) or a \
         printable character (ASCII code between 32 and 126)."
        s
        pos
        (Char.code s.[pos]))
    (obj2 (req "position" int31) (req "string" (string Plain)))
    (function Non_printable_character (pos, s) -> Some (pos, s) | _ -> None)
    (fun (pos, s) -> Non_printable_character (pos, s))

let empty = String_tag ""

let of_string v =
  let open Result_syntax in
  let rec check_printable_ascii i =
    if Compare.Int.(i < 0) then return (String_tag v)
    else
      match v.[i] with
      | '\n' | '\x20' .. '\x7E' -> check_printable_ascii (i - 1)
      | _ -> tzfail @@ Non_printable_character (i, v)
  in
  check_printable_ascii (String.length v - 1)

let to_string (String_tag s) = s

let compare (String_tag x) (String_tag y) = Compare.String.compare x y

let length (String_tag s) = String.length s

let concat_pair (String_tag x) (String_tag y) = String_tag (x ^ y)

let concat l =
  let l = List.map (fun (String_tag s) -> s) l in
  String_tag (String.concat "" l)

let sub (String_tag s) offset length = String_tag (String.sub s offset length)
