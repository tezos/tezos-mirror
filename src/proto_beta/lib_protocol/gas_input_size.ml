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

include Gas_comparable_input_size

let list (list : 'a Script_list.t) : t = list.Script_list.length

let set (set : 'a Script_typed_ir.set) : t =
  let res = Script_int.to_int (Script_set.size set) in
  match res with None -> assert false | Some x -> x

let map (map : ('a, 'b) Script_typed_ir.map) : t =
  let res = Script_int.to_int (Script_map.size map) in
  match res with None -> assert false | Some x -> x

(* ------------------------------------------------------------------------- *)
(* Micheline/Michelson-related *)

let micheline_zero = {traversal = 0; int_bytes = 0; string_bytes = 0}

let ( ++ ) x y =
  {
    traversal = x.traversal + y.traversal;
    int_bytes = x.int_bytes + y.int_bytes;
    string_bytes = x.string_bytes + y.string_bytes;
  }

let node leaves =
  let r = List.fold_left ( ++ ) micheline_zero leaves in
  {r with traversal = r.traversal + 1}

let rec of_micheline (x : ('a, 'b) Micheline.node) =
  match x with
  | Micheline.Int (_loc, z) ->
      let int_bytes = integer (Script_int.of_zint z) in
      {traversal = 1; int_bytes; string_bytes = 0}
  | Micheline.String (_loc, s) ->
      let string_bytes = String.length s in
      {traversal = 1; int_bytes = 0; string_bytes}
  | Micheline.Bytes (_loc, b) ->
      let string_bytes = bytes b in
      {traversal = 1; int_bytes = 0; string_bytes}
  | Micheline.Prim (_loc, _prim, subterms, _annot) ->
      node (List.map of_micheline subterms)
  | Micheline.Seq (_loc, subterms) -> node (List.map of_micheline subterms)

(* ------------------------------------------------------------------------- *)
(* Sapling-related *)

let sapling_transaction_inputs : Alpha_context.Sapling.transaction -> t =
 fun tx -> List.length tx.inputs

let sapling_transaction_outputs : Alpha_context.Sapling.transaction -> t =
 fun tx -> List.length tx.outputs

let sapling_transaction_bound_data : Alpha_context.Sapling.transaction -> t =
 fun tx -> String.length tx.bound_data
