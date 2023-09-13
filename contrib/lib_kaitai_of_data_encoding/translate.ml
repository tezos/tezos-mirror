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

open Kaitai.Types

(* We need to access the definition of data-encoding's [descr] type. For this
   reason we open the private/internal module [Data_encoding__Encoding] (rather
   than the public module [Data_encoding.Encoding]. *)
open Data_encoding__Encoding

let default_meta_spec ~encoding_name =
  MetaSpec.
    {
      path = [];
      isOpaque = false;
      id = Some encoding_name;
      endian = Some `BE;
      bitEndian = None;
      encoding = None;
      forceDebug = false;
      opaqueTypes = None;
      zeroCopySubstream = None;
      imports = [];
    }

let default_class_spec ~encoding_name =
  ClassSpec.
    {
      fileName = None;
      path = [];
      meta = default_meta_spec ~encoding_name;
      doc = Ground.default_doc_spec;
      toStringExpr = None;
      params = [];
      seq = [];
      types = [];
      instances = [];
      enums = [];
    }

let rec seq_field_of_data_encoding :
    type a.
    (string * EnumSpec.t) list ->
    a Data_encoding.t ->
    (string * EnumSpec.t) list * AttrSpec.t list =
 fun enums {encoding; json_encoding = _} ->
  match encoding with
  | Null -> (enums, [])
  | Empty -> (enums, [])
  | Ignore -> (enums, [])
  | Constant _ -> (enums, [])
  | Bool -> (Ground.Enum.add enums Ground.Enum.bool, [Ground.Attr.bool])
  | Uint8 -> (enums, [Ground.Attr.u1])
  | Conv {encoding; _} -> seq_field_of_data_encoding enums encoding
  | Tup e ->
      (* This case corresponds to a [tup1] combinator being called inside a
         [tup*] combinator. It's probably never used, but it's still a valid use
         of data-encoding. Note that we erase the information that there is an
         extraneous [tup1] in the encoding. *)
      seq_field_of_data_encoding enums e
  | Tups {kind = _; left; right} ->
      (* This case corresponds to a [tup*] combinator being called inside a
         [tup*] combinator. It's probably never used, but it's still a valid use
         of data-encoding. Note that we erase the information that there is an
         extraneous [tup*] in the encoding. *)
      let enums, left = seq_field_of_data_encoding enums left in
      let enums, right = seq_field_of_data_encoding enums right in
      let seq = left @ right in
      (enums, seq)
  | _ -> failwith "Not implemented"

let rec from_data_encoding :
    type a. encoding_name:string -> a Data_encoding.t -> ClassSpec.t =
 fun ~encoding_name {encoding; json_encoding = _} ->
  let class_spec_of_ground ?(enums = []) ground =
    {(default_class_spec ~encoding_name) with seq = [ground]; enums}
  in
  match encoding with
  | Bool -> class_spec_of_ground ~enums:[Ground.Enum.bool] Ground.Attr.bool
  | Uint8 -> class_spec_of_ground Ground.Attr.u1
  | Int8 -> class_spec_of_ground Ground.Attr.s1
  | Uint16 -> class_spec_of_ground Ground.Attr.u2
  | Int16 -> class_spec_of_ground Ground.Attr.s2
  | Int32 -> class_spec_of_ground Ground.Attr.s4
  | Int64 -> class_spec_of_ground Ground.Attr.s8
  | Int31 -> class_spec_of_ground Ground.Attr.int31
  | Float -> class_spec_of_ground Ground.Attr.f8
  | Bytes (_kind_length, _) -> class_spec_of_ground Ground.Attr.bytes
  | String (_kind_length, _) -> class_spec_of_ground Ground.Attr.string
  | Tup e ->
      (* Naked Tup likely due to [tup1]. We simply ignore this constructor. *)
      from_data_encoding ~encoding_name e
  | Tups {kind = _; left; right} ->
      let enums, left = seq_field_of_data_encoding [] left in
      let enums, right = seq_field_of_data_encoding enums right in
      let seq = left @ right in
      let seq =
        List.mapi
          (fun i attr -> AttrSpec.{attr with id = Printf.sprintf "field_%d" i})
          seq
      in
      {(default_class_spec ~encoding_name) with seq; enums}
  | Conv {encoding; _} -> from_data_encoding ~encoding_name encoding
  | Describe {encoding; _} ->
      (* TODO: patch the documentation to include available information *)
      from_data_encoding ~encoding_name encoding
  | Dynamic_size {kind = _; encoding} ->
      from_data_encoding ~encoding_name encoding
  | _ -> failwith "Not implemented"
