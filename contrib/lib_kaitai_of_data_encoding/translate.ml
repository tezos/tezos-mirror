(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Identifiers have a strict pattern to follow, this function removes the
   irregularities.

   Without escaping, the kaitai-struct-compiler throws the following error
   message: [error: invalid meta ID, expected /^[a-z][a-z0-9_]*$/] *)
let escape_id id =
  if String.length id < 1 then raise (Invalid_argument "empty id") ;
  let b = Buffer.create (String.length id) in
  if match id.[0] with 'a' .. 'z' | 'A' .. 'Z' -> false | _ -> true then
    Buffer.add_string b "id_" ;
  String.iter
    (function
      | ('a' .. 'z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | 'A' .. 'Z' as c -> Buffer.add_char b (Char.lowercase_ascii c)
      | '.' | '-' | ' ' -> Buffer.add_string b "__"
      | c ->
          (* we print [%S] to force escaping of the character *)
          raise
            (Failure
               (Format.asprintf
                  "Unsupported: special character (%C) in id (%S)"
                  c
                  id)))
    id ;
  Buffer.contents b

open Kaitai.Types

(* We need to access the definition of data-encoding's [descr] type. For this
   reason we alias the private/internal module [Data_encoding__Encoding] (rather
   than the public module [Data_encoding.Encoding]. *)
module DataEncoding = Data_encoding__Encoding

let rec seq_field_of_data_encoding :
    type a.
    Ground.Enum.assoc ->
    a DataEncoding.t ->
    string ->
    Helpers.tid_gen option ->
    Ground.Enum.assoc * AttrSpec.t list =
 fun enums {encoding; _} id tid_gen ->
  let id = escape_id id in
  match encoding with
  | Null -> (enums, [])
  | Empty -> (enums, [])
  | Ignore -> (enums, [])
  | Constant _ -> (enums, [])
  | Bool ->
      (Helpers.add_uniq_assoc enums Ground.Enum.bool, [Ground.Attr.bool ~id])
  | Uint8 -> (enums, [Ground.Attr.uint8 ~id])
  | Int8 -> (enums, [Ground.Attr.int8 ~id])
  | Uint16 -> (enums, [Ground.Attr.uint16 ~id])
  | Int16 -> (enums, [Ground.Attr.int16 ~id])
  | Int32 -> (enums, [Ground.Attr.int32 ~id])
  | Int64 -> (enums, [Ground.Attr.int64 ~id])
  | Int31 -> (enums, [Ground.Attr.int31 ~id])
  | Float -> (enums, [Ground.Attr.float ~id])
  | Conv {encoding; _} -> seq_field_of_data_encoding enums encoding id tid_gen
  | Tup e ->
      (* This case corresponds to a [tup1] combinator being called inside a
         [tup*] combinator. It's probably never used, but it's still a valid use
         of data-encoding. Note that we erase the information that there is an
         extraneous [tup1] in the encoding. *)
      let id = match tid_gen with None -> id | Some tid_gen -> tid_gen () in
      seq_field_of_data_encoding enums e id tid_gen
  | Tups {kind = _; left; right} ->
      (* This case corresponds to a [tup*] combinator being called inside a
         [tup*] combinator. It's probably never used, but it's still a valid use
         of data-encoding. Note that we erase the information that there is an
         extraneous [tup*] in the encoding. *)
      let tid_gen =
        match tid_gen with
        | None -> Some (Helpers.mk_tid_gen id)
        | Some _ as tid_gen -> tid_gen
      in
      let enums, left = seq_field_of_data_encoding enums left id tid_gen in
      let enums, right = seq_field_of_data_encoding enums right id tid_gen in
      let seq = left @ right in
      (enums, seq)
  | _ -> failwith "Not implemented"

let rec from_data_encoding :
    type a.
    encoding_name:string ->
    ?description:string ->
    a DataEncoding.t ->
    ClassSpec.t =
 fun ~encoding_name ?description {encoding; _} ->
  let encoding_name = escape_id encoding_name in
  match encoding with
  | Bool -> Ground.Class.bool ~encoding_name ?description ()
  | Uint8 -> Ground.Class.uint8 ~encoding_name ?description ()
  | Int8 -> Ground.Class.int8 ~encoding_name ?description ()
  | Uint16 -> Ground.Class.uint16 ~encoding_name ?description ()
  | Int16 -> Ground.Class.int16 ~encoding_name ?description ()
  | Int32 -> Ground.Class.int32 ~encoding_name ?description ()
  | Int64 -> Ground.Class.int64 ~encoding_name ?description ()
  | Int31 -> Ground.Class.int31 ~encoding_name ?description ()
  | Float -> Ground.Class.float ~encoding_name ?description ()
  | Bytes (_kind_length, _) -> Ground.Class.bytes ~encoding_name ?description ()
  | String (_kind_length, _) ->
      Ground.Class.string ~encoding_name ?description ()
  | N -> Ground.Class.n ~encoding_name ?description ()
  | Z -> Ground.Class.z ~encoding_name ?description ()
  | Tup e ->
      (* Naked Tup likely due to [tup1]. We simply ignore this constructor. *)
      from_data_encoding ~encoding_name e
  | Tups {kind = _; left; right} ->
      let tid_gen = Some (Helpers.mk_tid_gen encoding_name) in
      let enums, left =
        seq_field_of_data_encoding [] left encoding_name tid_gen
      in
      let enums, right =
        seq_field_of_data_encoding enums right encoding_name tid_gen
      in
      let seq = left @ right in
      {(Helpers.default_class_spec ~encoding_name ()) with seq; enums}
  | Conv {encoding; _} -> from_data_encoding ~encoding_name encoding
  | Describe {encoding; description; _} ->
      from_data_encoding ~encoding_name ?description encoding
  | Dynamic_size {kind = _; encoding} ->
      from_data_encoding ~encoding_name encoding
  | _ -> failwith "Not implemented"
