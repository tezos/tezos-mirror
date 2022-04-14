(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module IntSet = TzLwtreslib.Set.Make (Int)
module SizedSet = Sized.MakeSizedSet (IntSet)
module IntMap = TzLwtreslib.Map.Make (Int)
module SizedMap = Sized.MakeSizedMap (IntMap)

module SizedSet_test = struct
  let assert_consistent t =
    SizedSet.cardinal t = IntSet.cardinal (SizedSet.to_set t)

  let test_empty () = SizedSet.is_empty SizedSet.empty

  let test_add_element () = SizedSet.add 1 SizedSet.empty |> assert_consistent

  let test_duplicate_element () =
    SizedSet.add 1 SizedSet.empty |> SizedSet.add 1 |> assert_consistent

  let test_remove () =
    SizedSet.add 1 SizedSet.empty |> SizedSet.remove 1 |> assert_consistent

  let test_remove_non_existent () =
    let s = SizedSet.remove 1 SizedSet.empty in
    SizedSet.cardinal s = 0

  let test_filter () =
    let s = SizedSet.of_seq (List.to_seq [1; 2; 4; 4; 5; 6; 7]) in
    let filtered_s_1 = SizedSet.filter (fun e -> e mod 2 = 0) s in
    let filtered_s_2 = SizedSet.filter (fun e -> e mod 2 = 1) s in
    assert_consistent filtered_s_1 && assert_consistent filtered_s_2

  let test_filter_map () =
    let s = SizedSet.of_seq (List.to_seq [1; 2; 4; 4; 5; 6; 7]) in
    let filtered_s =
      SizedSet.filter_map
        (fun n -> if n mod 2 = 0 then Some (n / 2) else None)
        s
    in
    assert_consistent filtered_s

  let test_partition () =
    let s = SizedSet.of_seq (List.to_seq [1; 2; 4; 4; 5; 6; 7]) in
    let (partition_1, partition_2) =
      SizedSet.partition (fun e -> e mod 2 = 0) s
    in
    assert_consistent partition_1 && assert_consistent partition_2

  let test_split_with_element_contained () =
    let s = SizedSet.of_seq (List.to_seq [1; 2; 4; 4; 5; 6; 7]) in
    let (split_1, b, split_2) = SizedSet.split 2 s in
    assert_consistent split_1 && assert_consistent split_2 && b

  let test_split_without_element_contained () =
    let s = SizedSet.of_seq (List.to_seq [1; 2; 4; 4; 5; 6; 7]) in
    let (split_1, b, split_2) = SizedSet.split 3 s in
    assert_consistent split_1 && assert_consistent split_2 && not b

  let test =
    let open Alcotest in
    [
      test_case "empty" `Quick (fun () -> assert (test_empty ()));
      test_case "add element" `Quick (fun () -> assert (test_add_element ()));
      test_case "duplicate element" `Quick (fun () ->
          assert (test_duplicate_element ()));
      test_case "remove element" `Quick (fun () -> assert (test_remove ()));
      test_case "remove non existent element" `Quick (fun () ->
          assert (test_remove_non_existent ()));
      test_case "filter" `Quick (fun () -> assert (test_filter ()));
      test_case "filter_map" `Quick (fun () -> assert (test_filter_map ()));
      test_case "partition" `Quick (fun () -> assert (test_partition ()));
      test_case "split with the element contained" `Quick (fun () ->
          assert (test_split_with_element_contained ()));
      test_case "split with the element not contained" `Quick (fun () ->
          assert (test_split_without_element_contained ()));
    ]
end

module SizedMap_test = struct
  let assert_consistent t =
    SizedMap.cardinal t = IntMap.cardinal (SizedMap.to_map t)

  let test_empty () = SizedMap.is_empty SizedMap.empty

  let test_add_element () =
    SizedMap.add 1 "_" SizedMap.empty |> assert_consistent

  let test_duplicate_element () =
    SizedMap.add 1 "_" SizedMap.empty |> SizedMap.add 1 "_" |> assert_consistent

  let test_remove () =
    SizedMap.add 1 "_" SizedMap.empty |> SizedMap.remove 1 |> assert_consistent

  let test_remove_non_existent () =
    SizedMap.remove 1 SizedMap.empty |> assert_consistent

  let test_replace_binding () =
    SizedMap.add 1 "old_binding" SizedMap.empty
    |> SizedMap.add 1 "new_binding"
    |> assert_consistent

  let test_filter () =
    let m =
      SizedMap.of_seq
        (List.to_seq [(1, 1); (2, 2); (4, 4); (4, 4); (6, 6); (7, 7); (5, 5)])
    in
    let filtered_s_1 = SizedMap.filter (fun _ b -> b mod 2 = 0) m in
    let filtered_s_2 = SizedMap.filter (fun _ b -> b mod 2 = 1) m in
    assert_consistent filtered_s_1 && assert_consistent filtered_s_2

  let test_filter_map () =
    let m =
      SizedMap.of_seq
        (List.to_seq [(1, 1); (2, 2); (4, 4); (4, 4); (6, 6); (7, 7); (5, 5)])
    in
    let filtered_s =
      SizedMap.filter_map
        (fun _ b -> if b mod 2 = 0 then Some (b / 2) else None)
        m
    in
    assert_consistent filtered_s

  let test_partition () =
    let m =
      SizedMap.of_seq
        (List.to_seq [(1, 1); (2, 2); (4, 4); (4, 4); (6, 6); (7, 7); (5, 5)])
    in
    let (partition_1, partition_2) =
      SizedMap.partition (fun _ b -> b mod 2 = 0) m
    in
    assert_consistent partition_1 && assert_consistent partition_2

  let test_split_with_element_contained () =
    let m =
      SizedMap.of_seq
        (List.to_seq [(1, 1); (2, 2); (4, 4); (4, 4); (6, 6); (7, 7); (5, 5)])
    in
    let (split_1, _, split_2) = SizedMap.split 2 m in
    assert_consistent split_1 && assert_consistent split_2

  let test_split_without_element_contained () =
    let m =
      SizedMap.of_seq
        (List.to_seq [(1, 1); (2, 2); (4, 4); (4, 4); (6, 6); (7, 7); (5, 5)])
    in
    let (split_1, _, split_2) = SizedMap.split 3 m in
    assert_consistent split_1 && assert_consistent split_2

  let test =
    let open Alcotest in
    [
      test_case "empty" `Quick (fun () -> assert (test_empty ()));
      test_case "add element" `Quick (fun () -> assert (test_add_element ()));
      test_case "duplicate element" `Quick (fun () ->
          assert (test_duplicate_element ()));
      test_case "remove element" `Quick (fun () -> assert (test_remove ()));
      test_case "remove non existent element" `Quick (fun () ->
          assert (test_remove_non_existent ()));
      test_case "replace binding" `Quick (fun () ->
          assert (test_replace_binding ()));
      test_case "update" `Quick (fun () -> assert (test_filter ()));
      test_case "filter" `Quick (fun () -> assert (test_filter ()));
      test_case "filter_map" `Quick (fun () -> assert (test_filter_map ()));
      test_case "partition" `Quick (fun () -> assert (test_partition ()));
      test_case "split with the element contained" `Quick (fun () ->
          assert (test_split_with_element_contained ()));
      test_case "split with the element not contained" `Quick (fun () ->
          assert (test_split_without_element_contained ()));
    ]
end

let () =
  Alcotest.run
    "Sized"
    [("SizedSet", SizedSet_test.test); ("SizedMap", SizedMap_test.test)]
