(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make (E : Set.OrderedType) = struct
  (* Implementation of a min-heap for bounded multisets of values *)

  (* Invariants :
     - all indices between 0 and size (excluded) have data:
       size =
         Array.fold_left
         (fun count e -> if Option.is_some e then count + 1 else count) 0 data
     - the heap property itself:
        let P(i, j) =
          0 <= i < size /\ 0 <= j < size =>
          data.(j) = None \/ E.compare data.(i) data.(j) <= 0
       in \forall 0 <= i < size, P(i, 2 * i + 1) /\ P(i, 2 * (i + 1))
  *)
  type t = {mutable size : int; mutable data : E.t option array}

  let create bound =
    if bound < 0 || bound > Sys.max_array_length then
      invalid_arg (Format.sprintf "Bounded_heap.Make(_).create. bound %d" bound) ;
    {size = 0; data = Array.make bound None}

  let peek {size; data} = if size = 0 then None else data.(0)

  let get_data data i = Option.get data.(i)

  let swap i j data =
    let tmp = data.(j) in
    data.(j) <- data.(i) ;
    data.(i) <- tmp

  (* Bubble up the value located at index [i] such until the heap property is
     locally fulfilled *)
  let bubble_up i {data; size = _} =
    let rec loop i =
      if i > 0 then
        let j = (i - 1) / 2 in
        if E.compare (get_data data j) (get_data data i) > 0 then (
          swap i j data ;
          loop j)
    in
    loop i

  (* Bubble down the value at index [i] until the heap property is locally
     fulfilled, aka the value at index [i] is smaller than the one of its two
     children *)
  let bubble_down i {size; data} =
    let rec loop i =
      let left_index = (2 * i) + 1 in
      if left_index < size then (
        assert (data.(left_index) <> None) ;
        let left_value = get_data data left_index in
        let v = get_data data i in
        let right_index = 2 * (i + 1) in
        if right_index < size then (
          (* swap the value with the smallest of its two children *)
          assert (data.(right_index) <> None) ;
          let right_value = get_data data right_index in
          if E.compare right_value left_value < 0 then (
            if E.compare v right_value > 0 then (
              swap i right_index data ;
              loop right_index))
          else if E.compare v left_value > 0 then (
            swap i left_index data ;
            loop left_index))
        else if
          E.compare v left_value > 0
          (* swap the value with its left child, since the right one does not exist *)
        then (
          swap i left_index data ;
          loop left_index))
    in
    loop i

  let insert x t =
    let pos = t.size in
    let data = t.data in
    if pos < Array.length data then (
      data.(pos) <- Some x ;
      t.size <- pos + 1 ;
      bubble_up pos t)
    else
      match peek t with
      | None ->
          assert (t.size = 0) ;
          (* This should only happen in the case of the empty structure *)
          ()
      | Some hd ->
          (* if the inserted element is greater than the one at the top of the heap,
             put it it in its place and bubble it down where it belongs *)
          if E.compare x hd > 0 then (
            data.(0) <- Some x ;
            bubble_down 0 t)

  let get t =
    let a = Array.init t.size (fun i -> get_data t.data i) in
    Array.sort E.compare a ;
    Array.to_list a
end
