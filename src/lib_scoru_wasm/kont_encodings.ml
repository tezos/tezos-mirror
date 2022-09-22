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
open Tezos_tree_encoding

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

let tick_map_kont_encoding enc_kont enc_a enc_b =
  conv (fun (tick, map) -> {tick; map}) (fun {tick; map} -> (tick, map))
  @@ tup2
       ~flatten:true
       (option (scope ["inner_kont"] enc_kont))
       (scope ["map_kont"] (map_kont_encoding enc_a enc_b))

let concat_kont_encoding enc_a =
  conv
    (fun (lv, rv, res, offset) -> {lv; rv; res; offset})
    (fun {lv; rv; res; offset} -> (lv, rv, res, offset))
  @@ tup4
       ~flatten:true
       (scope ["lv"] enc_a)
       (scope ["rv"] enc_a)
       (scope ["res"] enc_a)
       (value ["offset"] Data_encoding.int32)

let fold_left_kont_encoding enc_a enc_acc =
  conv
    (fun (origin, acc, offset) -> {origin; acc; offset})
    (fun {origin; acc; offset} -> (origin, acc, offset))
  @@ tup3
       ~flatten:true
       (scope ["origin"] enc_a)
       (scope ["acc"] enc_acc)
       (value ["offset"] Data_encoding.int32)
