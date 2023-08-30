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
      endian = None;
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

let from_data_encoding :
    type a. encoding_name:string -> a Data_encoding.t -> ClassSpec.t =
 fun ~encoding_name {encoding; json_encoding = _} ->
  match encoding with
  | Bool ->
      {
        (default_class_spec ~encoding_name) with
        seq = [Ground.Attr.bool];
        enums = [Ground.Enum.bool];
      }
  | Uint8 -> {(default_class_spec ~encoding_name) with seq = [Ground.Attr.u1]}
  | _ -> failwith "Not implemented"
