(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type sint = Saturation_repr.may_saturate Saturation_repr.t

let ( !! ) = Saturation_repr.safe_int

let ( +! ) = Saturation_repr.add

let ( +? ) s x = Saturation_repr.add s !!x

let ( *? ) s x = Saturation_repr.mul s !!x

let ( /? ) s x = Saturation_repr.ediv s !!x

let zero = !!0

let word_size = !!8

let header_size = word_size

let int64_size = header_size +! (word_size *? 2)

let z_size z =
  let numbits = Z.numbits z in
  if Compare.Int.(numbits <= 62) then zero else (word_size *? Z.size z) +? 32

let string_size_gen len = header_size +? len +? (8 - (len mod 8))

let bytes_size b = string_size_gen (Bytes.length b)

let string_size s = string_size_gen (String.length s)

let option_size some x =
  let some x = header_size +! word_size +! some x in
  Option.fold ~none:zero ~some x

let list_cell_size elt_size =
  header_size +! word_size +! word_size +! elt_size
  [@@ocaml.inline always]

let list_fold_size elt_size list =
  List.fold_left
    (fun accu elt ->
      accu +! header_size +! word_size +! word_size +! elt_size elt)
    zero
    list

let boxed_tup2 x y =
  header_size +! word_size +! word_size +! x +! y
  [@@ocaml.inline always]

let node_size =
  let open Micheline in
  let annotation_size a = list_fold_size string_size a in
  let internal_node_size = function
    | Int (_, z) -> header_size +! (word_size *? 2) +! z_size z
    | String (_, s) -> header_size +! (word_size *? 2) +! string_size s
    | Bytes (_, s) -> header_size +! (word_size *? 2) +! bytes_size s
    | Prim (_, _, _, a) -> header_size +! (word_size *? 4) +! annotation_size a
    | Seq (_, _) -> header_size +! (word_size *? 2)
  in
  fun node ->
    Script_repr.fold node zero @@ fun size node ->
    size +! internal_node_size node

let expr_size expr = node_size (Micheline.root expr)
