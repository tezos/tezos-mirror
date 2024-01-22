(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

module Context = Tezos_protocol_environment.Context

type t = {
  total : int;
  keys : int;
  dirs : int;
  degrees : int list;
  depths : int list;
  sizes : int list;
}

let min_max (l : int list) =
  let rec loop l mn mx =
    match l with
    | [] -> (mn, mx)
    | x :: tl ->
        let mn = min mn x in
        let mx = max mx x in
        loop tl mn mx
  in
  loop l max_int ~-1

let pp fmtr {total; keys; dirs; degrees = _; depths = _; sizes} =
  let min_size, max_size = min_max sizes in
  Format.fprintf
    fmtr
    "{ total = %d; keys = %d ; dirs = %d; sizes in [%d; %d] degrees = ...; \
     depths = _}"
    total
    keys
    dirs
    min_size
    max_size

let empty_stats () =
  {total = 0; keys = 0; dirs = 0; degrees = []; depths = []; sizes = []}

let tree_statistics key_map =
  let open Io_helpers.Key_map in
  let nodes = ref 0 in
  let keys = ref 0 in
  let dirs = ref 0 in
  let rec loop tree depth degrees depths sizes =
    match tree with
    | Leaf size ->
        incr nodes ;
        incr keys ;
        (degrees, depth :: depths, size :: sizes)
    | Node map ->
        let degree = Io_helpers.Key_map.String_map.cardinal map in
        let degrees = degree :: degrees in
        incr nodes ;
        incr dirs ;
        Io_helpers.Key_map.String_map.fold
          (fun _ tree (degrees, depths, sizes) ->
            loop tree (depth + 1) degrees depths sizes)
          map
          (degrees, depths, sizes)
  in
  let degrees, depths, sizes = loop key_map 0 [] [] [] in
  {total = !nodes; keys = !keys; dirs = !dirs; degrees; depths; sizes}

let load_tree context key =
  let open Lwt_syntax in
  Context.fold
    context
    key
    ~order:`Undefined
    ~init:Io_helpers.Key_map.empty
    ~f:(fun path t tree ->
      let+ o = Context.Tree.to_value t in
      match o with
      | Some bytes ->
          let len = Bytes.length bytes in
          Io_helpers.Key_map.insert (key @ path) len tree
      | None -> tree)

let context_statistics base_dir context_hash =
  let open Lwt_syntax in
  let context, index =
    Io_helpers.load_context_from_disk base_dir context_hash
  in
  let* tree = load_tree context [] in
  let* () = Tezos_context.Context.close index in
  Lwt.return (tree_statistics tree)

let array_of_int_list (l : int list) = Array.map float_of_int (Array.of_list l)

let plot_histograms pdf_file {degrees; depths; sizes; _} =
  let open Plot in
  let degree =
    let points =
      degrees |> List.to_seq |> Seq.map float_of_int |> Seq.map r1
      |> Data.of_seq
    in
    plot2
      ~title:"Tree degree distribution"
      ~xaxis:"degree"
      ~yaxis:"freq"
      [Histogram.hist ~bins:50 ~points ()]
  in
  let depth =
    let points =
      depths |> List.to_seq |> Seq.map float_of_int |> Seq.map r1 |> Data.of_seq
    in
    plot2
      ~title:"Key depth distribution"
      ~xaxis:"depth"
      ~yaxis:"freq"
      [Histogram.hist ~bins:50 ~points ()]
  in
  let size =
    let points =
      sizes |> List.to_seq |> Seq.map float_of_int |> Seq.map r1 |> Data.of_seq
    in
    plot2
      ~title:"Data size distribution"
      ~xaxis:"size"
      ~yaxis:"freq"
      [Histogram.hist ~bins:50 ~points ()]
  in
  let plots = [|[|Some degree; Some depth; Some size|]|] in
  run_matrix ~target:(pdf ~cm_size:(30.0, 20.0) ~pdf_file ()) plots
