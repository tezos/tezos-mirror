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

(** Testing
    -------
    Component:    Protocol Library
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/pbt/test_carbonated_map.exe
    Subject:      Operations in Carbonated_map
*)

open Qcheck2_helpers
open QCheck2
open Protocol

let new_ctxt () =
  let open Lwt_result_syntax in
  let* block, _contract = Context.init1 () in
  let* incr = Incremental.begin_construction block in
  return @@ Incremental.alpha_ctxt incr

module Compare_int = struct
  type t = int

  let compare = Int.compare

  let compare_cost _ = Saturation_repr.safe_int 10
end

module CM =
  Carbonated_map.Make
    (struct
      type context = Alpha_context.context

      let consume = Alpha_context.Gas.consume
    end)
    (Compare_int)

let unsafe_new_context () =
  Result.value_f
    ~default:(fun () -> Stdlib.failwith "Failed to create context")
    (Lwt_main.run @@ new_ctxt ())

let int_map_gen =
  let ctxt = unsafe_new_context () in
  Gen.small_list (Gen.pair Gen.small_int Gen.small_int)
  |> Gen.map (fun kvs ->
         let merge_overlap ctxt x y = Ok (x + y, ctxt) in
         match CM.of_list ctxt ~merge_overlap kvs with
         | Ok (map, _) -> map
         | Error _ -> Stdlib.failwith "Failed to construct map")

let pp_int_map fmt map =
  let open Lwt_result_wrap_syntax in
  let pp =
    Assert.pp_print_list (fun fmt (k, v) -> Format.fprintf fmt "(%d, %d)" k v)
  in
  Lwt_main.run
    (let open Lwt_result_syntax in
    let* ctxt = new_ctxt () in
    let*?@ kvs, _ = CM.to_list ctxt map in
    return kvs)
  |> Result.value_f ~default:(fun () -> assert false)
  |> Format.fprintf fmt "%a" pp

let int_map_test name f =
  Test.make
    ~print:(Format.asprintf "%a" pp_int_map)
    ~count:100
    ~name
    int_map_gen
    (fun map -> match f map with Ok b -> b | Error _ -> false)

let int_map_pair_test name f =
  Test.make
    ~print:(fun (map1, map2) ->
      Format.asprintf "(%a, %a)" pp_int_map map1 pp_int_map map2)
    ~count:100
    ~name
    (Gen.pair int_map_gen int_map_gen)
    (fun (map1, map2) -> match f map1 map2 with Ok b -> b | Error _ -> false)

let unit_test name f =
  Alcotest.test_case name `Quick (fun () ->
      match f () with Ok b -> assert b | _ -> assert false)

type Environment.Error_monad.error += Dummy_error

let dummy_fail =
  Result.error (Environment.Error_monad.trace_of_error Dummy_error)

let assert_map_contains ctxt map expected =
  let open Result_syntax in
  let* kvs, _ctxt = CM.to_list ctxt map in
  Ok (List.sort compare kvs = List.sort compare expected)

let assert_equal_map ctxt map expected =
  let open Result_syntax in
  let* kvs, ctxt = CM.to_list ctxt expected in
  assert_map_contains ctxt map kvs

(** Test that the size of an empty map is 0. *)
let test_empty =
  unit_test "Size of empty map is 0" (fun () -> Ok (CM.size CM.empty = 0))

(** Test adding a new element *)
let test_update_add =
  let open Result_syntax in
  unit_test "Update add" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun _ _ _ -> dummy_fail)
      [(1, 1); (2, 2); (3, 3)]
  in
  let update_replace ctxt key value map =
    CM.update ctxt key (fun ctxt _ -> Ok (Some value, ctxt)) map
  in
  let* map, ctxt = update_replace ctxt 4 4 map in
  assert_map_contains ctxt map [(1, 1); (2, 2); (3, 3); (4, 4)]

(** Test replacing an existing element. *)
let test_update_replace =
  let open Result_syntax in
  unit_test "Update replace" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun _ _ _ -> dummy_fail)
      [(1, 1); (2, 2); (3, 3)]
  in
  let update_replace ctxt key value map =
    CM.update ctxt key (fun ctxt _ -> Ok (Some value, ctxt)) map
  in
  let* map, ctxt = update_replace ctxt 1 42 map in
  assert_map_contains ctxt map [(1, 42); (2, 2); (3, 3)]

(** Test merging when ignoring new overlapping keys. *)
let test_merge_overlaps_left =
  let open Result_syntax in
  unit_test "Merge overlaps left" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun ctxt left _ -> Ok (left, ctxt))
      [(1, 1); (2, 2); (3, 3); (1, 11); (2, 22); (3, 33); (4, 44)]
  in
  assert_map_contains ctxt map [(1, 1); (2, 2); (3, 3); (4, 44)]

(** Test merging when replacing the element of a new overlapping key. *)
let test_merge_overlaps_right =
  let open Result_syntax in
  unit_test "Merge overlap replace" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun ctxt _ right -> Ok (right, ctxt))
      [(1, 1); (2, 2); (3, 3); (1, 11); (2, 22); (3, 33); (4, 44)]
  in
  assert_map_contains ctxt map [(1, 11); (2, 22); (3, 33); (4, 44)]

(** Test merging when combining elements of overlapping keys. *)
let test_merge_overlaps_add =
  let open Result_syntax in
  unit_test "Merge overlap by adding" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun ctxt left right -> Ok (left + right, ctxt))
      [(1, 1); (2, 2); (3, 3); (1, 1); (2, 2); (3, 3); (4, 4)]
  in
  assert_map_contains ctxt map [(1, 2); (2, 4); (3, 6); (4, 4)]

(** Test update with merging elements of new and existing keys by adding them. *)
let test_update_merge =
  let open Result_syntax in
  unit_test "Update with merge add" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun _ _ _ -> dummy_fail)
      [(1, 1); (2, 2); (3, 3)]
  in
  let update_merge ctxt key new_value map =
    CM.update
      ctxt
      key
      (fun ctxt existing ->
        match existing with
        | None -> Ok (Some new_value, ctxt)
        | Some old_value -> Ok (Some (new_value + old_value), ctxt))
      map
  in
  let* map, ctxt = update_merge ctxt 1 1 map in
  let* map, ctxt = update_merge ctxt 4 4 map in
  assert_map_contains ctxt map [(1, 2); (2, 2); (3, 3); (4, 4)]

(** Test merging two maps when keeping the original value for overlapping keys. *)
let test_merge_map_keep_existing =
  let open Result_syntax in
  unit_test "Merge overlap keep existing" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map1, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun _ _ _ -> dummy_fail)
      [(1, "a"); (2, "b"); (3, "c")]
  in
  let* map2, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun _ _ _ -> dummy_fail)
      [(2, "b'"); (3, "c'"); (4, "d'")]
  in
  let* map, ctxt =
    CM.merge ctxt ~merge_overlap:(fun ctxt left _ -> Ok (left, ctxt)) map1 map2
  in
  assert_map_contains ctxt map [(1, "a"); (2, "b"); (3, "c"); (4, "d'")]

(** Test merging two maps when replacing the value for overlapping keys. *)
let test_merge_map_replace_existing =
  let open Result_syntax in
  unit_test "Merge overlap replace existing" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map1, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun _ _ _ -> dummy_fail)
      [(1, "a"); (2, "b"); (3, "c")]
  in
  let* map2, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun _ _ _ -> dummy_fail)
      [(2, "b'"); (3, "c'"); (4, "d'")]
  in
  let* map, ctxt =
    CM.merge
      ctxt
      ~merge_overlap:(fun ctxt _ right -> Ok (right, ctxt))
      map1
      map2
  in
  assert_map_contains ctxt map [(1, "a"); (2, "b'"); (3, "c'"); (4, "d'")]

(** Test deleting existing and non-existing keys. *)
let test_update_delete =
  let open Result_syntax in
  unit_test "Update delete" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* map, ctxt =
    CM.of_list
      ctxt
      ~merge_overlap:(fun _ _ _ -> dummy_fail)
      [(1, 1); (2, 2); (3, 3)]
  in
  let delete ctxt key map =
    CM.update ctxt key (fun ctxt _ -> Ok (None, ctxt)) map
  in
  let* map, ctxt = delete ctxt 1 map in
  let* map, ctxt = delete ctxt 4 map in
  assert_map_contains ctxt map [(2, 2); (3, 3)]

(** Test that merging [empty] with a map returns the same map. *)
let test_empty_left_identity_for_merge =
  let open Result_syntax in
  int_map_test "Empty map is left identity for merge" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* map', ctxt =
    CM.merge ctxt ~merge_overlap:(fun _ _ _ -> dummy_fail) map CM.empty
  in
  assert_equal_map ctxt map map'

(** Test that merging a map with [empty] returns the same map. *)
let test_empty_right_identity_for_merge =
  let open Result_syntax in
  int_map_test "Empty map is right identity for merge" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* map', ctxt =
    CM.merge ctxt ~merge_overlap:(fun _ _ _ -> dummy_fail) CM.empty map
  in
  assert_equal_map ctxt map map'

(** Test that [size] returns the number of key value pairs of a map. *)
let test_size =
  let open Result_syntax in
  int_map_test "Size returns the number of elements" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* kvs, _ = CM.to_list ctxt map in
  Result.ok Compare.List_length_with.(kvs = CM.size map)

(** Test that all keys of a map are found. *)
let test_find_existing =
  let open Result_syntax in
  int_map_test "Find all elements" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* kvs, _ = CM.to_list ctxt map in
  let* (_ : CM.context) =
    List.fold_left_e
      (fun ctxt (k, v) ->
        let* v_opt, ctxt = CM.find ctxt k map in
        match v_opt with Some v' when v = v' -> Ok ctxt | _ -> dummy_fail)
      ctxt
      kvs
  in
  Ok true

(** Test that find returns [None] for non-existing keys. *)
let test_find_non_existing =
  let open Result_syntax in
  int_map_test "Should not find non-existing" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* kvs, _ = CM.to_list ctxt map in
  let key = 42 in
  let* v_opt, _ = CM.find ctxt key map in
  match List.find_opt (fun (k, _) -> k = key) kvs with
  | Some (_, value) -> Ok (Some value = v_opt)
  | None -> Ok (None = v_opt)

(** Test that [to_list] followed by [of_list] returns the same map. *)
let test_to_list_of_list =
  let open Result_syntax in
  int_map_test "To-list/of-list roundtrip" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let merge_overlap ctxt x y = Ok (x + y, ctxt) in
  let* kvs, ctxt = CM.to_list ctxt map in
  let* map', ctxt = CM.of_list ctxt ~merge_overlap kvs in
  assert_equal_map ctxt map map'

(** Test that merging two maps is equivalent to merging the concatenated
    key-value lists of both maps. *)
let test_merge_against_list =
  let open Result_syntax in
  int_map_pair_test "Merge compared with list operation" @@ fun map1 map2 ->
  let ctxt = unsafe_new_context () in
  let merge_overlap ctxt x y = Ok (x + y, ctxt) in
  let* kvs1, ctxt = CM.to_list ctxt map1 in
  let* kvs2, ctxt = CM.to_list ctxt map2 in
  let* map_merged1, ctxt = CM.merge ctxt ~merge_overlap map1 map2 in
  let* map_merged2, ctxt = CM.of_list ~merge_overlap ctxt (kvs1 @ kvs2) in
  assert_equal_map ctxt map_merged1 map_merged2

(** Test that merging a map with itself does not alter its size. *)
let test_size_merge_self =
  let open Result_syntax in
  int_map_test "Size should not change when map is merging with itself"
  @@ fun map ->
  let ctxt = unsafe_new_context () in
  let size1 = CM.size map in
  let* map2, _ =
    CM.merge
      ctxt
      ~merge_overlap:(fun ctxt left right -> Ok (left + right, ctxt))
      map
      map
  in
  let size2 = CM.size map2 in
  Ok (size1 = size2)

(** Test that merging with a failing merge operation yields an error. *)
let test_merge_fail =
  int_map_test "Merging with failing merge-overlap" @@ fun map ->
  let ctxt = unsafe_new_context () in
  Result.ok
    (match CM.merge ctxt ~merge_overlap:(fun _ _ _ -> dummy_fail) map map with
    | Ok _ when CM.size map = 0 -> true
    | Ok _ -> false
    | Error _ -> true)

(** Test that adding one key-value pair to a map increases its size by one iff
    the key already exists. *)
let test_size_add_one =
  let open Result_syntax in
  int_map_test "Add a new element increases size by one" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let key = 42 in
  let* val_opt, ctxt = CM.find ctxt key map in
  let* map', _ctxt =
    CM.update
      ctxt
      key
      (fun ctxt existing ->
        match existing with
        | None -> Ok (Some 42, ctxt)
        | Some old_value -> Ok (Some old_value, ctxt))
      map
  in
  let size = CM.size map in
  let size' = CM.size map' in
  match val_opt with
  | None -> Ok (size' = size + 1)
  | Some _ -> Ok (size' = size)

(** Test that mapping over a map is equivalent to mapping over the list of
    key-value pairs and reconstructing the map. That is, the following diagram
    commutes:

    [map] ----to_list---> [list]
      |                     |
    [map_e f]            [List.map f]
      |                     |
      v                     v
    [map] --- to_list --> [list]
*)
let test_map =
  let open Result_syntax in
  int_map_test "Test that map commutes with mapping over list" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* kvs, ctxt = CM.to_list ctxt map in
  let* map', ctxt = CM.map_e ctxt (fun ctxt _ x -> Ok (x + 1, ctxt)) map in
  let kvs' = List.map (fun (k, v) -> (k, v + 1)) kvs in
  assert_map_contains ctxt map' kvs'

(** Test that folding over an empty map does not invoke the accumulator
    function. *)
let test_fold_empty =
  let open Result_syntax in
  unit_test "Fold empty" @@ fun () ->
  let ctxt = unsafe_new_context () in
  let* x, _ = CM.fold_e ctxt (fun _ctxt _acc _k _v -> dummy_fail) 0 CM.empty in
  Ok (x = 0)

(** Test that folding over a map is equivalent to folding over the corresponding
    list of key-value pairs. That is, the following diagram commutes:

    [map] -- to_list --> [list]
      |                    |
    [fold_e f z]    [List.fold_left f z]
      |                    |
     res <----- id -----> res
*)
let test_fold =
  let open Result_syntax in
  int_map_test "Test that fold commutes with folding over a list" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* kvs, ctxt = CM.to_list ctxt map in
  let sum = List.fold_left (fun sum (k, v) -> k + v + sum) 0 kvs in
  let* sum', _ =
    CM.fold_e ctxt (fun ctxt sum k v -> Ok (k + v + sum, ctxt)) 0 map
  in
  Ok (sum = sum')

(** Test that all key-value pairs can be collected by a fold. And that the
    order is the same as for [to_list]. *)
let test_fold_to_list =
  let open Result_syntax in
  int_map_test "Test that fold collecting the elements agrees with to-list"
  @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* kvs, ctxt = CM.to_list ctxt map in
  let* kvs', _ =
    CM.fold_e ctxt (fun ctxt kvs k v -> Ok ((k, v) :: kvs, ctxt)) [] map
  in
  Ok (kvs = List.rev kvs')

(** Test that mapping with a failing function fails iff the list is non-empty. *)
let test_map_fail =
  int_map_test "Test map with failing function" @@ fun map ->
  let ctxt = unsafe_new_context () in
  Result.ok
    (match CM.map_e ctxt (fun _ctxt _key _val -> dummy_fail) map with
    | Ok _ when CM.size map = 0 -> true
    | Error _ -> true
    | Ok _ -> false)

(** Test that removing an existing key from a map decreases its size by one. *)
let test_size_remove_one =
  let open Result_syntax in
  int_map_test "Remove new element decreases size by one" @@ fun map ->
  let ctxt = unsafe_new_context () in
  let* kvs, ctxt = CM.to_list ctxt map in
  let key = match kvs with (k, _) :: _ -> k | _ -> 42 in
  let* val_opt, ctxt = CM.find ctxt key map in
  let* map', _ctxt = CM.update ctxt key (fun ctxt _ -> Ok (None, ctxt)) map in
  let size = CM.size map in
  let size' = CM.size map' in
  match val_opt with
  | None -> Ok (size' = size)
  | Some _ -> Ok (size' = size - 1)

let qcheck_tests =
  [
    test_size;
    test_to_list_of_list;
    test_empty_left_identity_for_merge;
    test_empty_right_identity_for_merge;
    test_size_merge_self;
    test_size_add_one;
    test_size_remove_one;
    test_merge_against_list;
    test_merge_fail;
    test_find_non_existing;
    test_find_existing;
    test_map;
    test_fold;
    test_fold_to_list;
    test_map_fail;
  ]

let unit_tests =
  [
    test_empty;
    test_update_add;
    test_update_replace;
    test_merge_overlaps_left;
    test_merge_overlaps_right;
    test_merge_overlaps_add;
    test_update_merge;
    test_merge_map_keep_existing;
    test_merge_map_replace_existing;
    test_update_delete;
    test_fold_empty;
  ]

let tests ~rand = qcheck_wrap ~rand qcheck_tests @ unit_tests

let () =
  (* Ensure deterministic results. *)
  let rand = Random.State.make [|0x1337533D; 71287309; 397060904|] in
  Alcotest.run
    "protocol > pbt > carbonated map"
    [(Protocol.name ^ ": Carbonated map", tests ~rand)]
