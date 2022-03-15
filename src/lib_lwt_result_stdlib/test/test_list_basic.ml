(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Support.Lib.Monad

let assert_eq_s pa pb =
  let open Lwt_syntax in
  let* a = pa and* b = pb in
  assert (a = b) ;
  return_unit

let assert_err e = e = Error ()

let assert_err_s e =
  let open Lwt_syntax in
  let* e = e in
  assert (e = Error ()) ;
  return_unit

let assert_err_p e =
  let open Lwt_syntax in
  let* e = e in
  assert (e = Error (Support.Test_trace.make ())) ;
  return_unit

module ListGen = struct
  include Support.Lib.List

  let rec down n : int t = if n < 0 then [] else n :: down (pred n)

  let rec up n i : int t = if i > n then [] else i :: up n (succ i)

  let up n = up n 0
end

open ListGen

module IsEmpty = struct
  let is_empty _ =
    assert (is_empty [] = true) ;
    assert (is_empty ['a'] = false) ;
    assert (is_empty [0] = false) ;
    assert (is_empty [0; 1; 2] = false) ;
    assert (is_empty [[]] = false) ;
    assert (is_empty [None] = false) ;
    assert (is_empty [""] = false) ;
    ()

  let tests = [Alcotest.test_case "is_empty" `Quick is_empty]
end

module Nth = struct
  let nth _ =
    assert (nth (up 10) 0 = Some 0) ;
    assert (nth (up 10) 1 = Some 1) ;
    assert (nth (up 10) 7 = Some 7) ;
    assert (nth (up 10) 10 = Some 10) ;
    assert (nth (up 10) 11 = None) ;
    assert (nth (up 10) 12 = None) ;
    assert (nth [] 0 = None) ;
    assert (nth [] 1 = None) ;
    assert (nth (up 104) max_int = None) ;
    assert (nth (up 104) (-1) = None) ;
    assert (nth (up 1) (-100) = None) ;
    assert (nth (up 0) (-100) = None) ;
    ()

  let tests = [Alcotest.test_case "nth" `Quick nth]
end

module Last = struct
  let last _ =
    assert (last (-1) [] = -1) ;
    assert (last (-1) (up 0) = 0) ;
    assert (last (-1) (up 10) = 10) ;
    ()

  let last_opt _ =
    assert (last_opt [] = None) ;
    assert (last_opt (up 0) = Some 0) ;
    assert (last_opt (up 10) = Some 10) ;
    ()

  let tests =
    [
      Alcotest.test_case "last" `Quick last;
      Alcotest.test_case "last_opt" `Quick last_opt;
    ]
end

module FilterSmthg = struct
  let cond x = x mod 2 = 0

  let filter_some () =
    assert (filter_some [] = []) ;
    assert (filter_some [None] = []) ;
    assert (filter_some [Some 0] = [0]) ;
    assert (
      let base = up 17 in
      let left = base |> filter cond in
      let right =
        base |> map (fun x -> if cond x then Some x else None) |> filter_some
      in
      left = right) ;
    ()

  let filter_ok () =
    assert (filter_ok [] = []) ;
    assert (filter_ok [Error 10] = []) ;
    assert (filter_ok [Ok 0] = [0]) ;
    assert (
      let base = up 17 in
      let left = base |> filter cond in
      let right =
        base
        |> map (fun x -> if cond x then Ok x else Error (4 * x))
        |> filter_ok
      in
      left = right) ;
    ()

  let filter_error () =
    assert (filter_error [] = []) ;
    assert (filter_error [Ok 10] = []) ;
    assert (filter_error [Error 0] = [0]) ;
    assert (
      let base = up 17 in
      let left = base |> filter cond in
      let right =
        base
        |> map (fun x -> if cond x then Error x else Ok (4 * x))
        |> filter_error
      in
      left = right) ;
    ()

  let tests =
    [
      Alcotest.test_case "filter_some" `Quick filter_some;
      Alcotest.test_case "filter_ok" `Quick filter_ok;
      Alcotest.test_case "filter_error" `Quick filter_error;
    ]
end

module Combine = struct
  let combine_error () =
    assert (combine ~when_different_lengths:() [] [0] = Error ()) ;
    assert (combine ~when_different_lengths:() [0] [] = Error ()) ;
    assert (combine ~when_different_lengths:() (up 100) (up 99) = Error ()) ;
    ()

  let combine_ok () =
    assert (combine ~when_different_lengths:() [] [] = Ok []) ;
    assert (combine ~when_different_lengths:() [0] [1] = Ok [(0, 1)]) ;
    assert (
      combine ~when_different_lengths:() (up 100) (down 100)
      = init ~when_negative_length:() 101 (fun i -> (i, 100 - i))) ;
    ()

  let combine_drop () =
    assert (combine_drop [] [] = []) ;
    assert (
      Ok (combine_drop (up 100) (down 100))
      = init ~when_negative_length:() 101 (fun i -> (i, 100 - i))) ;
    assert (combine_drop [0] [1] = [(0, 1)]) ;
    assert (combine_drop [] [0] = []) ;
    assert (combine_drop [0] [] = []) ;
    assert (combine_drop (up 100) (up 99) = map (fun i -> (i, i)) (up 99)) ;
    ()

  let combine_with_leftovers () =
    assert (combine_with_leftovers [] [] = ([], None)) ;
    assert (
      combine_with_leftovers (up 100) (down 100)
      = ( Stdlib.Result.get_ok
          @@ init ~when_negative_length:() 101 (fun i -> (i, 100 - i)),
          None )) ;
    assert (combine_with_leftovers [0] [1] = ([(0, 1)], None)) ;
    assert (combine_with_leftovers [] [0] = ([], Some (Either.Right [0]))) ;
    assert (combine_with_leftovers [0] [] = ([], Some (Either.Left [0]))) ;
    assert (
      combine_with_leftovers (up 100) (up 99)
      = (map (fun i -> (i, i)) (up 99), Some (Either.Left [100]))) ;
    ()

  let tests =
    [
      Alcotest.test_case "combine-error" `Quick combine_error;
      Alcotest.test_case "combine-ok" `Quick combine_ok;
      Alcotest.test_case "combine_drop" `Quick combine_drop;
      Alcotest.test_case "combine_with_leftovers" `Quick combine_with_leftovers;
    ]
end

module Partition = struct
  let cond x = x mod 2 = 0

  let partition_result () =
    assert (partition_result [] = ([], [])) ;
    assert (partition_result [Ok 0] = ([0], [])) ;
    assert (partition_result [Error 0] = ([], [0])) ;
    assert (partition_result (map Result.ok (up 11)) = (up 11, [])) ;
    assert (partition_result (map Result.error (up 11)) = ([], up 11)) ;
    assert (
      let input = map (fun x -> if cond x then Ok x else Error x) (up 101) in
      partition_result input = (filter_ok input, filter_error input)) ;
    ()

  let tests = [Alcotest.test_case "partition-result" `Quick partition_result]
end

module Product = struct
  let empty_single () =
    assert (product [] [] = []) ;
    assert (product [0] [] = []) ;
    assert (product [] [0] = []) ;
    assert (product ['a'] [0] = [('a', 0)]) ;
    ()

  let big () =
    assert (
      sort Stdlib.compare @@ product ['a'; 'b'; 'c'] [0]
      = [('a', 0); ('b', 0); ('c', 0)]) ;
    assert (sort Stdlib.compare @@ product ['a'] [0; 1] = [('a', 0); ('a', 1)]) ;
    assert (
      sort Stdlib.compare @@ product ['a'; 'b'; 'c'] [0; 1]
      = [('a', 0); ('a', 1); ('b', 0); ('b', 1); ('c', 0); ('c', 1)]) ;
    ()

  let tests =
    [
      Alcotest.test_case "empty and singleton" `Quick empty_single;
      Alcotest.test_case "big" `Quick big;
    ]
end

module Shuffle = struct
  let test_small () =
    let rng = Random.State.make_self_init () in
    assert (shuffle ~rng [] = []) ;
    assert (shuffle ~rng [0] = [0]) ;
    assert (shuffle ~rng [0; 0] = [0; 0]) ;
    ()

  let pp_int_list =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
      Format.pp_print_int

  let list_size = QCheck.Gen.int_range 2 50

  let count = 50

  let test_shuffle_preserves_values =
    QCheck.Test.make
      ~name:"shuffle preserves value sets"
      ~count
      QCheck.(pair (list_of_size list_size int) int)
      (fun (l, seed) ->
        let rng = Random.State.make [|seed|] in
        let l1 = sort Int.compare l in
        let l2 = sort Int.compare (shuffle ~rng l) in
        Lib_test.Qcheck_helpers.qcheck_eq'
          ~pp:pp_int_list
          ~eq:( = )
          ~actual:l2
          ~expected:l1
          ())

  let test_determinism_eq seed =
    let rng1 = Random.State.make [|seed|] in
    let rng2 = Random.State.make [|seed|] in
    let input = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let output1 = shuffle ~rng:rng1 input in
    let output2 = shuffle ~rng:rng2 input in
    assert (equal Int.equal output1 output2)

  let test_determinism_eq () =
    test_determinism_eq 0 ;
    test_determinism_eq 1 ;
    test_determinism_eq 3421304 ;
    test_determinism_eq 3021782487 ;
    test_determinism_eq 3421452345304 ;
    test_determinism_eq 30214780913782487 ;
    ()

  let test_determinism_neq seed =
    let rng1 = Random.State.make [|seed|] in
    let rng2 = Random.State.make [|seed + 17|] in
    let input = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let output1 = shuffle ~rng:rng1 input in
    let output2 = shuffle ~rng:rng2 input in
    assert (not (equal Int.equal output1 output2))

  let test_determinism_neq () =
    test_determinism_neq 0 ;
    test_determinism_neq 1 ;
    test_determinism_neq 3421304 ;
    test_determinism_neq 3021782487 ;
    test_determinism_neq 3421452345304 ;
    test_determinism_neq 30214780913782487 ;
    ()

  let tests =
    [
      Alcotest.test_case "small" `Quick test_small;
      QCheck_alcotest.to_alcotest test_shuffle_preserves_values;
      Alcotest.test_case "determinism(eq)" `Quick test_determinism_eq;
      Alcotest.test_case "determinism(neq)" `Quick test_determinism_neq;
    ]
end

let () =
  Alcotest.run
    "list-basic"
    [
      ("is_empty", IsEmpty.tests);
      ("nth", Nth.tests);
      ("last", Last.tests);
      ("filter_*", FilterSmthg.tests);
      ("combine_*", Combine.tests);
      ("partition_*", Partition.tests);
      ("shuffle", Shuffle.tests);
      ("product", Product.tests);
    ]
