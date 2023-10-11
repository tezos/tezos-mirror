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
    Ground.Type.assoc ->
    a DataEncoding.t ->
    string ->
    Helpers.tid_gen option ->
    Ground.Enum.assoc * Ground.Type.assoc * AttrSpec.t list =
 fun enums types {encoding; _} id tid_gen ->
  let id = escape_id id in
  match encoding with
  | Null -> (enums, types, [])
  | Empty -> (enums, types, [])
  | Ignore -> (enums, types, [])
  | Constant _ -> (enums, types, [])
  | Bool ->
      let enums = Helpers.add_uniq_assoc enums Ground.Enum.bool in
      (enums, types, [Ground.Attr.bool ~id])
  | Uint8 -> (enums, types, [Ground.Attr.uint8 ~id])
  | Int8 -> (enums, types, [Ground.Attr.int8 ~id])
  | Uint16 -> (enums, types, [Ground.Attr.uint16 ~id])
  | Int16 -> (enums, types, [Ground.Attr.int16 ~id])
  | Int32 -> (enums, types, [Ground.Attr.int32 ~id])
  | Int64 -> (enums, types, [Ground.Attr.int64 ~id])
  | Int31 -> (enums, types, [Ground.Attr.int31 ~id])
  | Float -> (enums, types, [Ground.Attr.float ~id])
  | Bytes (`Fixed n, _) -> (enums, types, [Ground.Attr.bytes_fixed ~id n])
  | Bytes (`Variable, _) -> (enums, types, [Ground.Attr.bytes_eos ~id])
  | String (`Fixed n, _) -> (enums, types, [Ground.Attr.string_fixed ~id n])
  | String (`Variable, _) -> (enums, types, [Ground.Attr.string_eos ~id])
  | N ->
      let types = Helpers.add_uniq_assoc types Ground.Type.n in
      (enums, types, [Ground.Attr.n ~id])
  | Z ->
      let types = Helpers.add_uniq_assoc types Ground.Type.n in
      (enums, types, [Ground.Attr.z ~id])
  | Conv {encoding; _} ->
      seq_field_of_data_encoding enums types encoding id tid_gen
  | Tup e ->
      (* This case corresponds to a [tup1] combinator being called inside a
         [tup*] combinator. It's probably never used, but it's still a valid use
         of data-encoding. Note that we erase the information that there is an
         extraneous [tup1] in the encoding. *)
      let id = match tid_gen with None -> id | Some tid_gen -> tid_gen () in
      seq_field_of_data_encoding enums types e id tid_gen
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
      let enums, types, left =
        seq_field_of_data_encoding enums types left id tid_gen
      in
      let enums, types, right =
        seq_field_of_data_encoding enums types right id tid_gen
      in
      let seq = left @ right in
      (enums, types, seq)
  | Dynamic_size {kind; encoding} ->
      (* TODO: special case for [encoding=Bytes] and [encoding=String] *)
      let len_id = "len_" ^ id in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id tid_gen
      in
      let attr =
        {
          Helpers.default_attr_spec with
          id;
          dataType =
            DataType.(
              ComplexDataType
                (UserType
                   (Helpers.class_spec_of_attrs
                      ~encoding_name:id
                      ~enums:[]
                      ~types:[]
                      ~instances:[]
                      attrs)));
          size = Some (Ast.Name len_id);
        }
      in
      let len_attr =
        match kind with
        | `N -> failwith "Not implemented"
        | `Uint30 -> Ground.Attr.uint30 ~id:len_id
        | `Uint16 -> Ground.Attr.uint16 ~id:len_id
        | `Uint8 -> Ground.Attr.uint8 ~id:len_id
      in
      (enums, types, [len_attr; attr])
  | Describe {encoding; id; description; title} ->
      let id = escape_id id in
      let description =
        match (title, description) with
        | None, None -> None
        | None, (Some _ as s) | (Some _ as s), None -> s
        | Some t, Some d -> Some (t ^ ": " ^ d)
      in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id tid_gen
      in
      let attr =
        {
          Helpers.default_attr_spec with
          id;
          dataType =
            DataType.(
              ComplexDataType
                (UserType
                   (Helpers.class_spec_of_attrs
                      ~encoding_name:id
                      ?description
                      ~enums:[]
                      ~types:[]
                      ~instances:[]
                      attrs)));
        }
      in
      (enums, types, [attr])
  | _ -> failwith "Not implemented"

let from_data_encoding :
    type a.
    encoding_name:string ->
    ?description:string ->
    a DataEncoding.t ->
    ClassSpec.t =
 fun ~encoding_name ?description encoding ->
  let encoding_name = escape_id encoding_name in
  match encoding.encoding with
  | Describe {encoding; description; id; _} ->
      (* TODO: accumulate descriptions rather than replace it *)
      let enums, types, attrs =
        seq_field_of_data_encoding [] [] encoding id None
      in
      Helpers.class_spec_of_attrs
        ~encoding_name
        ?description
        ~enums
        ~types
        ~instances:[]
        attrs
  | _ ->
      let enums, types, attrs =
        seq_field_of_data_encoding [] [] encoding encoding_name None
      in
      Helpers.class_spec_of_attrs
        ~encoding_name
        ?description
        ~enums
        ~types
        ~instances:[]
        attrs
