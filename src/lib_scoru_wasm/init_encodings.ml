(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Tezos_webassembly_interpreter.Eval
module Parser = Binary_parser_encodings
open Tree_encoding

let tag_encoding = value [] Data_encoding.string

let fold_right2_kont_encoding enc_a enc_b enc_acc =
  conv
    (fun (acc, lv, rv, offset) -> {acc; lv; rv; offset})
    (fun {acc; lv; rv; offset} -> (acc, lv, rv, offset))
  @@ tup4
       ~flatten:true
       (scope ["acc"] enc_acc)
       (scope ["left_vector"] enc_a)
       (scope ["right_vector"] enc_b)
       (value ["offset"] @@ Data_encoding.int32)

let map_kont_encoding enc_a enc_b =
  conv
    (fun (origin, destination, offset) -> {origin; destination; offset})
    (fun {origin; destination; offset} -> (origin, destination, offset))
  @@ tup3
       ~flatten:true
       (scope ["origin"] enc_a)
       (scope ["destination"] enc_b)
       (value ["offset"] Data_encoding.int32)

let lazy_vec_encoding enc = int32_lazy_vector (value [] Data_encoding.int32) enc

let init_kont_encoding =
  tagged_union
    tag_encoding
    [
      case
        "IK_Start"
        (value_option [] Data_encoding.unit)
        (function IK_Start -> Some None | _ -> None)
        (function _ -> IK_Start);
      case
        "IK_Add_import"
        (fold_right2_kont_encoding
           (lazy_vec_encoding Wasm_encoding.extern_encoding)
           (lazy_vec_encoding
              Parser.(no_region_encoding Import.import_encoding))
           Wasm_encoding.module_instance_encoding)
        (function IK_Add_import m -> Some m | _ -> None)
        (function m -> IK_Add_import m);
      case
        "IK_Types"
        (tup2
           ~flatten:true
           (scope ["module"] Wasm_encoding.module_instance_encoding)
           (scope
              ["kont"]
              (map_kont_encoding
                 (lazy_vec_encoding
                    Parser.(no_region_encoding Wasm_encoding.func_type_encoding))
                 Wasm_encoding.function_type_vector_encoding)))
        (function IK_Type (m, t) -> Some (m, t) | _ -> None)
        (function m, t -> IK_Type (m, t));
      case
        "IK_Remaining"
        Wasm_encoding.module_instance_encoding
        (function IK_Remaining m -> Some m | _ -> None)
        (function m -> IK_Remaining m);
      case
        "IK_Stop"
        Wasm_encoding.module_instance_encoding
        (function IK_Stop m -> Some m | _ -> None)
        (function m -> IK_Stop m);
    ]
