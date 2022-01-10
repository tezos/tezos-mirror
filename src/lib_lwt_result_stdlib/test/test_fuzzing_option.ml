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

open Support.Lib
open Test_fuzzing_helpers

(* First series of tests: testing equivalence with size-1 lists *)

(* First-1: testing equivalence of iter* *)
module TestIter = struct
  open QCheck
  open Monad

  let iter =
    Test.make
      ~name:"{Option,List([01])}.iter"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), init, input) ->
        eq
          (let acc = ref init in
           Option.iter (IterOf.fn acc fn) input ;
           !acc)
          (let acc = ref init in
           List.iter (IterOf.fn acc fn) (Option.to_list input) ;
           !acc))

  let iter_e =
    Test.make
      ~name:"{Option,List([01])}.iter_e"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), init, input) ->
        let open Result_syntax in
        eq_e
          (let acc = ref init in
           let+ () = Option.iter_e (IterEOf.fn acc fn) input in
           !acc)
          (let acc = ref init in
           let+ () = List.iter_e (IterEOf.fn acc fn) (Option.to_list input) in
           !acc))

  let iter_s =
    Test.make
      ~name:"{Option,List([01])}.iter_s"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_syntax in
        eq_s
          (let acc = ref init in
           let+ () = Option.iter_s (IterSOf.fn acc fn) input in
           !acc)
          (let acc = ref init in
           let+ () = List.iter_s (IterSOf.fn acc fn) (Option.to_list input) in
           !acc))

  let iter_es =
    Test.make
      ~name:"{Option,List([01])}.iter_es"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let+ () = Option.iter_es (IterESOf.fn acc fn) input in
           !acc)
          (let acc = ref init in
           let+ () = List.iter_es (IterESOf.fn acc fn) (Option.to_list input) in
           !acc))

  let tests = [iter; iter_e; iter_s; iter_es]
end

(* First-2: testing equivalence of filter* *)
module TestFilter = struct
  open QCheck
  open Monad

  let filter =
    Test.make
      ~name:"{Option,List([01])}.filter"
      (triple Test_fuzzing_helpers.Fn.pred one maybe)
      (fun (fn, const, input) ->
        eq
          (Option.filter (CondOf.fn fn const) input)
          (List.filter (CondOf.fn fn const) (Option.to_list input) |> List.hd))

  let filter_e =
    Test.make
      ~name:"{Option,List([01])}.filter_e"
      (triple Test_fuzzing_helpers.Fn.pred one maybe)
      (fun (fn, const, input) ->
        let open Result_syntax in
        eq_e
          (Option.filter_e (CondEOf.fn fn const) input)
          (let+ xs =
             List.filter_e (CondEOf.fn fn const) (Option.to_list input)
           in
           List.hd xs))

  let filter_s =
    Test.make
      ~name:"{Option,List([01])}.filter_s"
      (triple Test_fuzzing_helpers.Fn.pred one maybe)
      (fun (fn, const, input) ->
        let open Lwt_syntax in
        eq_s
          (Option.filter_s (CondSOf.fn fn const) input)
          (let+ xs =
             List.filter_s (CondSOf.fn fn const) (Option.to_list input)
           in
           List.hd xs))

  let filter_es =
    Test.make
      ~name:"{Option,List([01])}.filter_es"
      (triple Test_fuzzing_helpers.Fn.pred one maybe)
      (fun (fn, const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (Option.filter_es (CondESOf.fn fn const) input)
          (let+ xs =
             List.filter_es (CondESOf.fn fn const) (Option.to_list input)
           in
           List.hd xs))

  let filter_left =
    Test.make
      ~name:(Format.asprintf "Option.{filter,filter_left}")
      (triple Test_fuzzing_helpers.Fn.pred one maybe)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (Option.filter cond input)
          (Option.filter_left
             (Option.map
                (fun x -> if cond x then Either.Left x else Either.Right x)
                input)))

  let filter_right =
    Test.make
      ~name:(Format.asprintf "Option.{filter,Option.filter_right}")
      (triple Test_fuzzing_helpers.Fn.pred one maybe)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (Option.filter cond input)
          (Option.filter_right
             (Option.map
                (fun x -> if cond x then Either.Right x else Either.Left x)
                input)))

  let filter_ok =
    Test.make
      ~name:(Format.asprintf "Option.{filter,filter_ok}")
      (triple Test_fuzzing_helpers.Fn.pred one maybe)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (Option.filter cond input)
          (Option.filter_ok
             (Option.map (fun x -> if cond x then Ok x else Error x) input)))

  let filter_error =
    Test.make
      ~name:(Format.asprintf "Option.{filter,Option.filter_error}")
      (triple Test_fuzzing_helpers.Fn.pred one maybe)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (Option.filter cond input)
          (Option.filter_error
             (Option.map (fun x -> if cond x then Error x else Ok x) input)))

  let tests =
    [
      filter;
      filter_e;
      filter_s;
      filter_es;
      filter_left;
      filter_right;
      filter_ok;
      filter_error;
    ]
end

(* First-3: testing equivalence of filter_map* *)
module TestFilterMap = struct
  open QCheck
  open Monad

  let filter_map =
    Test.make
      ~name:"{Option,List([01])}.filter_map"
      (quad
         Test_fuzzing_helpers.Fn.pred
         Test_fuzzing_helpers.Fn.arith
         one
         maybe)
      (fun (pred, Fun (_, arith), const, input) ->
        eq
          (Option.filter_map (FilterMapOf.fns pred arith const) input)
          (List.filter_map
             (FilterMapOf.fns pred arith const)
             (Option.to_list input)
          |> List.hd))

  let filter_map_e =
    Test.make
      ~name:"{Option,List([01])}.filter_map_e"
      (quad
         Test_fuzzing_helpers.Fn.pred
         Test_fuzzing_helpers.Fn.arith
         one
         maybe)
      (fun (pred, Fun (_, arith), const, input) ->
        let open Result_syntax in
        eq_e
          (Option.filter_map_e (FilterMapEOf.fns pred arith const) input)
          (let+ xs =
             List.filter_map_e
               (FilterMapEOf.fns pred arith const)
               (Option.to_list input)
           in
           List.hd xs))

  let filter_map_s =
    Test.make
      ~name:"{Option,List([01])}.filter_map_s"
      (quad
         Test_fuzzing_helpers.Fn.pred
         Test_fuzzing_helpers.Fn.arith
         one
         maybe)
      (fun (pred, Fun (_, arith), const, input) ->
        let open Lwt_syntax in
        eq_s
          (Option.filter_map_s (FilterMapSOf.fns pred arith const) input)
          (let+ xs =
             List.filter_map_s
               (FilterMapSOf.fns pred arith const)
               (Option.to_list input)
           in
           List.hd xs))

  let filter_map_es =
    Test.make
      ~name:"{Option,List([01])}.filter_map_es"
      (quad
         Test_fuzzing_helpers.Fn.pred
         Test_fuzzing_helpers.Fn.arith
         one
         maybe)
      (fun (pred, Fun (_, arith), const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (Option.filter_map_es (FilterMapESOf.fns pred arith const) input)
          (let+ xs =
             List.filter_map_es
               (FilterMapESOf.fns pred arith const)
               (Option.to_list input)
           in
           List.hd xs))

  let tests = [filter_map; filter_map_e; filter_map_s; filter_map_es]
end

(* First-4: testing equivalence of map* *)
module TestMap = struct
  open QCheck
  open Monad

  let map =
    Test.make
      ~name:"{Option,List([01])}.map"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), const, input) ->
        eq
          (Option.map (MapOf.fn const fn) input)
          (List.map (MapOf.fn const fn) (Option.to_list input) |> List.hd))

  let map_e =
    Test.make
      ~name:"{Option,List([01])}.map_e"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), const, input) ->
        let open Result_syntax in
        eq
          (Option.map_e (MapEOf.fn const fn) input)
          (let+ xs = List.map_e (MapEOf.fn const fn) (Option.to_list input) in
           List.hd xs))

  let map_s =
    Test.make
      ~name:"{Option,List([01])}.map_s"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), const, input) ->
        let open Lwt_syntax in
        eq
          (Option.map_s (MapSOf.fn const fn) input)
          (let+ xs = List.map_s (MapSOf.fn const fn) (Option.to_list input) in
           List.hd xs))

  let map_es =
    Test.make
      ~name:"{Option,List([01])}.map_es"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), const, input) ->
        let open Lwt_result_syntax in
        eq
          (Option.map_es (MapESOf.fn const fn) input)
          (let+ xs = List.map_es (MapESOf.fn const fn) (Option.to_list input) in
           List.hd xs))

  let tests = [map; map_e; map_s; map_es]
end

let () =
  let tests =
    [
      ("iter*", Lib_test.Qcheck_helpers.qcheck_wrap TestIter.tests);
      ("filter*", Lib_test.Qcheck_helpers.qcheck_wrap TestFilter.tests);
      ("filter_map*", Lib_test.Qcheck_helpers.qcheck_wrap TestFilterMap.tests);
      ("map*", Lib_test.Qcheck_helpers.qcheck_wrap TestMap.tests);
    ]
  in
  Alcotest.run "Option" tests
