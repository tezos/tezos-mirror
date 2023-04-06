(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Stdlib
   Invocation:   dune exec src/lib_lwt_result_stdlib/test/main.exe
   Subject:      Test fuzzing list against stdlib
*)

open Test_fuzzing_lib

let init =
  test_of_ty_with_p
    "List.init"
    DSL.([tiny_int; [int] @-> monad data] --> resultmonad (list data))
    (catch_stdlib_error_2 Stdlib.List.init)
    (Support.Lib.List.init ~when_negative_length:())
    (Support.Lib.List.init_s ~when_negative_length:())
    (Support.Lib.List.init_p ~when_negative_length:())
    (Support.Lib.List.init_e ~when_negative_length:())
    (Support.Lib.List.init_es ~when_negative_length:())
    (Support.Lib.List.init_ep ~when_negative_length:())

let find =
  test_of_ty
    "List.find"
    DSL.([[data] @-> monad bool; list data] --> monad (option data))
    Stdlib.List.find_opt
    Support.Lib.List.find
    Support.Lib.List.find_s
    Support.Lib.List.find_e
    Support.Lib.List.find_es

let find_map =
  test_of_ty
    "List.find_map"
    DSL.([[data] @-> monad (option data); list data] --> monad (option data))
    Stdlib.List.find_map
    Support.Lib.List.find_map
    Support.Lib.List.find_map_s
    Support.Lib.List.find_map_e
    Support.Lib.List.find_map_es

let filter =
  test_of_ty_with_p
    "List.filter"
    DSL.([[data] @-> monad bool; list data] --> monad (list data))
    Stdlib.List.filter
    Support.Lib.List.filter
    Support.Lib.List.filter_s
    Support.Lib.List.filter_p
    Support.Lib.List.filter_e
    Support.Lib.List.filter_es
    Support.Lib.List.filter_ep

let filteri =
  test_of_ty_with_p
    "List.filteri"
    DSL.([[int; data] @-> monad bool; list data] --> monad (list data))
    Stdlib.List.filteri
    Support.Lib.List.filteri
    Support.Lib.List.filteri_s
    Support.Lib.List.filteri_p
    Support.Lib.List.filteri_e
    Support.Lib.List.filteri_es
    Support.Lib.List.filteri_ep

let rev_filter =
  test_of_ty
    "List.rev_filter"
    DSL.([[data] @-> monad bool; list data] --> monad (list data))
    (fun f xs -> List.rev (Stdlib.List.filter f xs))
    Support.Lib.List.rev_filter
    Support.Lib.List.rev_filter_s
    Support.Lib.List.rev_filter_e
    Support.Lib.List.rev_filter_es

let rev_filteri =
  test_of_ty
    "List.rev_filteri"
    DSL.([[int; data] @-> monad bool; list data] --> monad (list data))
    (fun f xs -> List.rev (Stdlib.List.filteri f xs))
    Support.Lib.List.rev_filteri
    Support.Lib.List.rev_filteri_s
    Support.Lib.List.rev_filteri_e
    Support.Lib.List.rev_filteri_es

let filter_left =
  test_vanilla_of_ty
    "List.filter_left"
    DSL.([list (either data data)] --> list data)
    (Stdlib.List.filter_map Either.find_left)
    Support.Lib.List.filter_left

let filter_right =
  test_vanilla_of_ty
    "List.filter_right"
    DSL.([list (either data data)] --> list data)
    (Stdlib.List.filter_map Either.find_right)
    Support.Lib.List.filter_right

let filter_ok =
  test_vanilla_of_ty
    "List.filter_ok"
    DSL.([list (result data data)] --> list data)
    (Stdlib.List.filter_map Result.to_option)
    Support.Lib.List.filter_ok

let filter_error =
  test_vanilla_of_ty
    "List.filter_error"
    DSL.([list (result data data)] --> list data)
    (Stdlib.List.filter_map (function Ok _ -> None | Error e -> Some e))
    Support.Lib.List.filter_error

let partition =
  test_of_ty_with_p
    "List.partition"
    DSL.([[data] @-> monad bool; list data] --> monad (list data * list data))
    Stdlib.List.partition
    Support.Lib.List.partition
    Support.Lib.List.partition_s
    Support.Lib.List.partition_p
    Support.Lib.List.partition_e
    Support.Lib.List.partition_es
    Support.Lib.List.partition_ep

let partition_either =
  test_vanilla_of_ty
    "List.partition_either"
    DSL.([list (either data data)] --> monad (list data * list data))
    (Stdlib.List.partition_map Fun.id)
    Support.Lib.List.partition_either

let partition_map =
  test_of_ty_with_p
    "List.partition_map"
    DSL.(
      [[data] @-> monad (either data data); list data]
      --> monad (list data * list data))
    Stdlib.List.partition_map
    Support.Lib.List.partition_map
    Support.Lib.List.partition_map_s
    Support.Lib.List.partition_map_p
    Support.Lib.List.partition_map_e
    Support.Lib.List.partition_map_es
    Support.Lib.List.partition_map_ep

(* NOTE: The testing framework ([Test_fuzzing_lib]) is focused on testing the
   return values of a set of functions. [iter] (and friends) always return [()]
   (modulo the monad). As a result, in a simple test, all calls would be
   equivalent and the simple test would not mean anything. Instead we implement
   [fold_left] on top of [iter] and test this. *)
let iter =
  test_of_ty_with_p
    "List.iter"
    DSL.([data; [data; data] @-> monad data; list data] --> monad data)
    (fun init f xs ->
      let acc = ref init in
      let () =
        Stdlib.List.iter
          (fun x ->
            let y = f !acc x in
            acc := y ;
            ())
          xs
      in
      !acc)
    (fun init f xs ->
      let acc = ref init in
      let () =
        Support.Lib.List.iter
          (fun x ->
            let y = f !acc x in
            acc := y ;
            ())
          xs
      in
      !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter_s
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      (* NOTE: in the case of [iter_p] the order of the application is not
         necessarily the same everytime. However, in the simple case of these
         tests, all the promises are immediately resolved. In addition, Lwt is
         eager: immediately resolved promises don't cause any context switching.
         As a result, this test is possible. *)
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter_p
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter_e
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter_es
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter_ep
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)

let iteri =
  test_of_ty_with_p
    "List.iteri"
    DSL.([data; [data; int; data] @-> monad data; list data] --> monad data)
    (fun init f xs ->
      let acc = ref init in
      let () =
        Stdlib.List.iteri
          (fun i x ->
            let y = f !acc i x in
            acc := y ;
            ())
          xs
      in
      !acc)
    (fun init f xs ->
      let acc = ref init in
      let () =
        Support.Lib.List.iteri
          (fun i x ->
            let y = f !acc i x in
            acc := y ;
            ())
          xs
      in
      !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iteri_s
          (fun i x ->
            let* y = f !acc i x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iteri_p
          (fun i x ->
            let* y = f !acc i x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iteri_e
          (fun i x ->
            let* y = f !acc i x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iteri_es
          (fun i x ->
            let* y = f !acc i x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iteri_ep
          (fun i x ->
            let* y = f !acc i x in
            acc := y ;
            return ())
          xs
      in
      return !acc)

let map =
  test_of_ty_with_p
    "List.map"
    DSL.([[data] @-> monad data; list data] --> monad (list data))
    Stdlib.List.map
    Support.Lib.List.map
    Support.Lib.List.map_s
    Support.Lib.List.map_p
    Support.Lib.List.map_e
    Support.Lib.List.map_es
    Support.Lib.List.map_ep

let mapi =
  test_of_ty_with_p
    "List.mapi"
    DSL.([[int; data] @-> monad data; list data] --> monad (list data))
    Stdlib.List.mapi
    Support.Lib.List.mapi
    Support.Lib.List.mapi_s
    Support.Lib.List.mapi_p
    Support.Lib.List.mapi_e
    Support.Lib.List.mapi_es
    Support.Lib.List.mapi_ep

let rev_map =
  test_of_ty_with_p
    "List.rev_map"
    DSL.([[data] @-> monad data; list data] --> monad (list data))
    Stdlib.List.rev_map
    Support.Lib.List.rev_map
    Support.Lib.List.rev_map_s
    Support.Lib.List.rev_map_p
    Support.Lib.List.rev_map_e
    Support.Lib.List.rev_map_es
    Support.Lib.List.rev_map_ep

let rev_mapi =
  test_of_ty_with_p
    "List.rev_mapi"
    DSL.([[int; data] @-> monad data; list data] --> monad (list data))
    (fun f xs -> List.rev (Stdlib.List.mapi f xs))
    Support.Lib.List.rev_mapi
    Support.Lib.List.rev_mapi_s
    Support.Lib.List.rev_mapi_p
    Support.Lib.List.rev_mapi_e
    Support.Lib.List.rev_mapi_es
    Support.Lib.List.rev_mapi_ep

let filter_map =
  test_of_ty_with_p
    "List.filter_map"
    DSL.([[data] @-> monad (option data); list data] --> monad (list data))
    Stdlib.List.filter_map
    Support.Lib.List.filter_map
    Support.Lib.List.filter_map_s
    Support.Lib.List.filter_map_p
    Support.Lib.List.filter_map_e
    Support.Lib.List.filter_map_es
    Support.Lib.List.filter_map_ep

let concat_map =
  test_of_ty_with_p
    "List.concat_map"
    DSL.([[data] @-> monad (list data); list data] --> monad (list data))
    Stdlib.List.concat_map
    Support.Lib.List.concat_map
    Support.Lib.List.concat_map_s
    Support.Lib.List.concat_map_p
    Support.Lib.List.concat_map_e
    Support.Lib.List.concat_map_es
    Support.Lib.List.concat_map_ep

let fold_left =
  test_of_ty
    "List.fold_left"
    DSL.([[data; data] @-> monad data; data; list data] --> monad data)
    Stdlib.List.fold_left
    Support.Lib.List.fold_left
    Support.Lib.List.fold_left_s
    Support.Lib.List.fold_left_e
    Support.Lib.List.fold_left_es

let fold_left_map =
  test_of_ty
    "List.fold_left_map"
    DSL.(
      [[data; data] @-> monad (data * data); data; list data]
      --> monad (data * list data))
    Stdlib.List.fold_left_map
    Support.Lib.List.fold_left_map
    Support.Lib.List.fold_left_map_s
    Support.Lib.List.fold_left_map_e
    Support.Lib.List.fold_left_map_es

let fold_right =
  test_of_ty
    "List.fold_right"
    DSL.([[data; data] @-> monad data; list data; data] --> monad data)
    Stdlib.List.fold_right
    Support.Lib.List.fold_right
    Support.Lib.List.fold_right_s
    Support.Lib.List.fold_right_e
    Support.Lib.List.fold_right_es

let map2 =
  test_of_ty
    "List.map2"
    DSL.(
      [[data; data] @-> monad data; list data; list data]
      --> resultmonad (list data))
    (catch_stdlib_error_3 Stdlib.List.map2)
    (Support.Lib.List.map2 ~when_different_lengths:())
    (Support.Lib.List.map2_s ~when_different_lengths:())
    (Support.Lib.List.map2_e ~when_different_lengths:())
    (Support.Lib.List.map2_es ~when_different_lengths:())

let rev_map2 =
  test_of_ty
    "List.rev_map2"
    DSL.(
      [[data; data] @-> monad data; list data; list data]
      --> resultmonad (list data))
    (catch_stdlib_error_3 Stdlib.List.rev_map2)
    (Support.Lib.List.rev_map2 ~when_different_lengths:())
    (Support.Lib.List.rev_map2_s ~when_different_lengths:())
    (Support.Lib.List.rev_map2_e ~when_different_lengths:())
    (Support.Lib.List.rev_map2_es ~when_different_lengths:())

let iter2 =
  test_of_ty
    "List.iter2"
    DSL.(
      [data; [data; data; data] @-> monad data; list data; list data]
      --> resultmonad data)
    (fun init f xs ys ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref init in
      let* () =
        catch_stdlib_error_3
          Stdlib.List.iter2
          (fun x y ->
            let z = f !acc x y in
            acc := z ;
            ())
          xs
          ys
      in
      return !acc)
    (fun init f xs ys ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter2
          ~when_different_lengths:()
          (fun x y ->
            let z = f !acc x y in
            acc := z ;
            ())
          xs
          ys
      in
      return !acc)
    (fun init f xs ys ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter2_s
          ~when_different_lengths:()
          (fun x y ->
            let open Support.Lib.Monad.Lwt_syntax in
            let* z = f !acc x y in
            acc := z ;
            return ())
          xs
          ys
      in
      return !acc)
    (fun init f xs ys ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter2_e
          ~when_different_lengths:()
          (fun x y ->
            let* z = f !acc x y in
            acc := z ;
            return ())
          xs
          ys
      in
      return !acc)
    (fun init f xs ys ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.List.iter2_es
          ~when_different_lengths:()
          (fun x y ->
            let* z = f !acc x y in
            acc := z ;
            return ())
          xs
          ys
      in
      return !acc)

let fold_left2 =
  test_of_ty
    "List.fold_left2"
    DSL.(
      [[data; data; data] @-> monad data; data; list data; list data]
      --> resultmonad data)
    (catch_stdlib_error_4 Stdlib.List.fold_left2)
    (Support.Lib.List.fold_left2 ~when_different_lengths:())
    (Support.Lib.List.fold_left2_s ~when_different_lengths:())
    (Support.Lib.List.fold_left2_e ~when_different_lengths:())
    (Support.Lib.List.fold_left2_es ~when_different_lengths:())

let fold_right2 =
  test_of_ty
    "List.fold_right2"
    DSL.(
      [[data; data; data] @-> monad data; list data; list data; data]
      --> resultmonad data)
    (catch_stdlib_error_4 Stdlib.List.fold_right2)
    (Support.Lib.List.fold_right2 ~when_different_lengths:())
    (Support.Lib.List.fold_right2_s ~when_different_lengths:())
    (Support.Lib.List.fold_right2_e ~when_different_lengths:())
    (Support.Lib.List.fold_right2_es ~when_different_lengths:())

let exists =
  test_of_ty_with_p
    "List.exists"
    DSL.([[data] @-> monad bool; list data] --> monad bool)
    Stdlib.List.exists
    Support.Lib.List.exists
    Support.Lib.List.exists_s
    Support.Lib.List.exists_p
    Support.Lib.List.exists_e
    Support.Lib.List.exists_es
    Support.Lib.List.exists_ep

let for_all =
  test_of_ty_with_p
    "List.for_all"
    DSL.([[data] @-> monad bool; list data] --> monad bool)
    Stdlib.List.for_all
    Support.Lib.List.for_all
    Support.Lib.List.for_all_s
    Support.Lib.List.for_all_p
    Support.Lib.List.for_all_e
    Support.Lib.List.for_all_es
    Support.Lib.List.for_all_ep

let exists2 =
  test_of_ty
    "List.exists2"
    DSL.(
      [[data; data] @-> monad bool; list data; list data] --> resultmonad bool)
    (catch_stdlib_error_3 Stdlib.List.exists2)
    (Support.Lib.List.exists2 ~when_different_lengths:())
    (Support.Lib.List.exists2_s ~when_different_lengths:())
    (Support.Lib.List.exists2_e ~when_different_lengths:())
    (Support.Lib.List.exists2_es ~when_different_lengths:())

let for_all2 =
  test_of_ty
    "List.for_all2"
    DSL.(
      [[data; data] @-> monad bool; list data; list data] --> resultmonad bool)
    (catch_stdlib_error_3 Stdlib.List.for_all2)
    (Support.Lib.List.for_all2 ~when_different_lengths:())
    (Support.Lib.List.for_all2_s ~when_different_lengths:())
    (Support.Lib.List.for_all2_e ~when_different_lengths:())
    (Support.Lib.List.for_all2_es ~when_different_lengths:())

let remove_assoc =
  test_vanilla_of_ty
    "List.remove_assoc"
    DSL.([data; list (data * data)] --> list (data * data))
    Stdlib.List.remove_assoc
    (Support.Lib.List.remove_assoc ~equal:Char.equal)

let combine =
  test_vanilla_of_ty
    "List.combine"
    DSL.([list data; list data] --> resultmonad (list (data * data)))
    (catch_stdlib_error_2 Stdlib.List.combine)
    (Support.Lib.List.combine ~when_different_lengths:())

let rev_combine =
  test_vanilla_of_ty
    "List.rev_combine"
    DSL.([list data; list data] --> resultmonad (list (data * data)))
    (catch_stdlib_error_2 (fun xs ys -> List.rev (Stdlib.List.combine xs ys)))
    (Support.Lib.List.rev_combine ~when_different_lengths:())

let compare =
  test_vanilla_of_ty
    "List.compare"
    DSL.([[data; data] @-> int; list data; list data] --> int)
    Stdlib.List.compare
    Support.Lib.List.compare

let equal =
  test_vanilla_of_ty
    "List.equal"
    DSL.([[data; data] @-> bool; list data; list data] --> bool)
    Stdlib.List.equal
    Support.Lib.List.equal

let all_list : unit Alcotest.test_case list =
  [
    init;
    find;
    find_map;
    filter;
    filteri;
    rev_filter;
    rev_filteri;
    filter_left;
    filter_right;
    filter_ok;
    filter_error;
    partition;
    partition_map;
    partition_either;
    iter;
    iteri;
    map;
    mapi;
    rev_map;
    rev_mapi;
    filter_map;
    concat_map;
    fold_left;
    fold_left_map;
    fold_right;
    map2;
    rev_map2;
    iter2;
    fold_left2;
    fold_right2;
    exists;
    for_all;
    exists2;
    for_all2;
    remove_assoc;
    combine;
    rev_combine;
    compare;
    equal;
  ]

let () = Alcotest.run ~__FILE__ "FuzzRef" [("List", all_list)]
