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

let summary ~title ~description =
  match (title, description) with
  | None, None -> None
  | None, (Some _ as s) | (Some _ as s), None -> s
  | Some t, Some d -> Some (t ^ ": " ^ d)

(* in kaitai-struct, some fields can be added to single attributes but not to a
   group of them. When we want to attach a field to a group of attributes, we
   need to create an indirection to a named type. [redirect_if_many] is a
   function for adding a field which, on a by-need basis, introduces an
   intermediate type. *)
let redirect_if_many :
    Ground.Type.assoc ->
    AttrSpec.t list ->
    (AttrSpec.t -> AttrSpec.t) ->
    string ->
    Ground.Type.assoc * AttrSpec.t =
 fun types attrs fattr id ->
  match attrs with
  | [] -> failwith "Not supported"
  | [attr] -> (types, {(fattr attr) with id})
  | _ :: _ :: _ as attrs ->
      let ((_, user_type) as type_) =
        (id, Helpers.class_spec_of_attrs ~id attrs)
      in
      let types = Helpers.add_uniq_assoc types type_ in
      let attr =
        fattr
          {
            (Helpers.default_attr_spec ~id) with
            dataType = DataType.(ComplexDataType (UserType user_type));
          }
      in
      (types, attr)

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
  | RangedInt {minimum; maximum} -> (
      (* TODO: support shifted RangedInt
         - make the de/serialiser shift the value
         - make the [valid] field check the shifted value *)
      if minimum > 0 then failwith "Not supported (shifted range)" ;
      let size = Data_encoding__Binary_size.range_to_size ~minimum ~maximum in
      let valid =
        Some
          (ValidationSpec.ValidationRange
             {min = Ast.IntNum minimum; max = Ast.IntNum maximum})
      in
      let uvalid =
        if minimum = 0 then
          Some (ValidationSpec.ValidationMax (Ast.IntNum maximum))
        else valid
      in
      match size with
      | `Uint8 -> (enums, types, [{(Ground.Attr.uint8 ~id) with valid = uvalid}])
      | `Uint16 ->
          (enums, types, [{(Ground.Attr.uint16 ~id) with valid = uvalid}])
      | `Uint30 ->
          (enums, types, [{(Ground.Attr.uint30 ~id) with valid = uvalid}])
      | `Int8 -> (enums, types, [{(Ground.Attr.int8 ~id) with valid}])
      | `Int16 -> (enums, types, [{(Ground.Attr.int16 ~id) with valid}])
      | `Int31 -> (enums, types, [{(Ground.Attr.int31 ~id) with valid}]))
  | Float -> (enums, types, [Ground.Attr.float ~id])
  | RangedFloat {minimum; maximum} ->
      let valid =
        Some
          (ValidationSpec.ValidationRange
             {min = Ast.FloatNum minimum; max = Ast.FloatNum maximum})
      in
      (enums, types, [{(Ground.Attr.float ~id) with valid}])
  | Bytes (`Fixed n, _) -> (enums, types, [Ground.Attr.bytes ~id (Fixed n)])
  | Bytes (`Variable, _) -> (enums, types, [Ground.Attr.bytes ~id Variable])
  | Dynamic_size {kind; encoding = {encoding = Bytes (`Variable, _); _}} ->
      let size_id = "len_" ^ id in
      let size_attr = Ground.Attr.binary_length_kind ~id:size_id kind in
      (enums, types, [size_attr; Ground.Attr.bytes ~id (Dynamic size_id)])
  | String (`Fixed n, _) -> (enums, types, [Ground.Attr.string ~id (Fixed n)])
  | String (`Variable, _) -> (enums, types, [Ground.Attr.string ~id Variable])
  | Dynamic_size {kind; encoding = {encoding = String (`Variable, _); _}} ->
      let size_id = "len_" ^ id in
      let size_attr = Ground.Attr.binary_length_kind ~id:size_id kind in
      (enums, types, [size_attr; Ground.Attr.string ~id (Dynamic size_id)])
  | N ->
      let types = Helpers.add_uniq_assoc types Ground.Type.n_chunk in
      let types = Helpers.add_uniq_assoc types Ground.Type.n in
      (enums, types, [Ground.Attr.n ~id])
  | Z ->
      let types = Helpers.add_uniq_assoc types Ground.Type.n_chunk in
      let types = Helpers.add_uniq_assoc types Ground.Type.z in
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
  | Obj f -> seq_field_of_field enums types f
  | Objs {kind = _; left; right} ->
      let enums, types, left =
        seq_field_of_data_encoding enums types left id None
      in
      let enums, types, right =
        seq_field_of_data_encoding enums types right id None
      in
      let seq = left @ right in
      (enums, types, seq)
  | Dynamic_size {kind; encoding} ->
      let len_id = "len_" ^ id in
      let len_attr =
        match kind with
        | `N -> failwith "Not implemented"
        | `Uint30 -> Ground.Attr.uint30 ~id:len_id
        | `Uint16 -> Ground.Attr.uint16 ~id:len_id
        | `Uint8 -> Ground.Attr.uint8 ~id:len_id
      in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id tid_gen
      in
      let types, attr =
        redirect_if_many
          types
          attrs
          (fun attr -> {attr with size = Some (Ast.Name len_id)})
          id
      in
      (enums, types, [len_attr; attr])
  | Splitted {encoding; json_encoding = _; is_obj = _; is_tup = _} ->
      seq_field_of_data_encoding enums types encoding id tid_gen
  | Describe {encoding; id; description; title} -> (
      let id = escape_id id in
      let description = summary ~title ~description in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id tid_gen
      in
      match attrs with
      | [] -> failwith "Not supported"
      | [attr] -> (enums, types, [Helpers.merge_summaries attr description])
      | _ :: _ :: _ as attrs ->
          let attr =
            {
              (Helpers.default_attr_spec ~id) with
              dataType =
                DataType.(
                  ComplexDataType
                    (UserType
                       (Helpers.class_spec_of_attrs
                          ~id
                          ?description
                          ~enums:[]
                          ~types:[]
                          ~instances:[]
                          attrs)));
            }
          in
          (enums, types, [attr]))
  | Check_size {limit = _; encoding} ->
      (* TODO: Add a guard for check size.*)
      seq_field_of_data_encoding enums types encoding id tid_gen
  | _ -> failwith "Not implemented"

and seq_field_of_field :
    type a.
    Ground.Enum.assoc ->
    Ground.Type.assoc ->
    a DataEncoding.field ->
    Ground.Enum.assoc * Ground.Type.assoc * AttrSpec.t list =
 fun enums types f ->
  match f with
  | Req {name; encoding; title; description} ->
      let id = escape_id name in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id None
      in
      let summary = summary ~title ~description in
      let types, attr =
        redirect_if_many
          types
          attrs
          (fun attr -> Helpers.merge_summaries attr summary)
          id
      in
      (enums, types, [attr])
  | Opt {name; kind = _; encoding; title; description} ->
      let cond_id = escape_id (name ^ "_tag") in
      let enums = Helpers.add_uniq_assoc enums Ground.Enum.bool in
      let cond_attr = Ground.Attr.bool ~id:cond_id in
      let cond =
        {
          Helpers.cond_no_cond with
          ifExpr =
            Some
              Ast.(
                Compare
                  {
                    left = Name cond_id;
                    ops = Eq;
                    right =
                      EnumByLabel
                        {
                          enumName = fst Ground.Enum.bool;
                          label = Ground.Enum.bool_true_name;
                          inType =
                            {
                              absolute = true;
                              names = [fst Ground.Enum.bool];
                              isArray = false;
                            };
                        };
                  });
        }
      in
      let id = escape_id name in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id None
      in
      let summary = summary ~title ~description in
      let types, attr =
        redirect_if_many
          types
          attrs
          (fun attr -> {(Helpers.merge_summaries attr summary) with cond})
          id
      in
      (enums, types, [cond_attr; attr])
  | Dft {name; encoding; default = _; title; description} ->
      (* NOTE: in binary format Dft is the same as Req *)
      let id = escape_id name in
      let enums, types, attrs =
        seq_field_of_data_encoding enums types encoding id None
      in
      let summary = summary ~title ~description in
      let types, attr =
        redirect_if_many
          types
          attrs
          (fun attr -> Helpers.merge_summaries attr summary)
          id
      in
      (enums, types, [attr])

let from_data_encoding :
    type a. id:string -> ?description:string -> a DataEncoding.t -> ClassSpec.t
    =
 fun ~id ?description encoding ->
  let encoding_name = escape_id id in
  match encoding.encoding with
  | Describe {encoding; description; id; _} ->
      let enums, types, attrs =
        seq_field_of_data_encoding [] [] encoding id None
      in
      Helpers.class_spec_of_attrs
        ~top_level:true
        ~id:encoding_name
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
        ~top_level:true
        ~id:encoding_name
        ?description
        ~enums
        ~types
        ~instances:[]
        attrs
