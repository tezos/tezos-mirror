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
  let open Lwt.Infix in
  pa >>= fun a ->
  pb >>= fun b ->
  assert (a = b) ;
  Lwt.return_unit

let assert_err e = e = Error ()

let assert_err_s e =
  let open Lwt.Infix in
  e >>= fun e ->
  assert (e = Error ()) ;
  Lwt.return_unit

let assert_err_p e =
  let open Lwt.Infix in
  e >>= fun e ->
  assert (e = Error (Support.Test_trace.make ())) ;
  Lwt.return_unit

module ListGen = struct
  include Support.Lib.List

  let rec down n : int t = if n < 0 then [] else n :: down (pred n)

  let rec up n i : int t = if i > n then [] else i :: up n (succ i)

  let up n = up n 0
end

open ListGen

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

  let tests = [Alcotest_lwt.test_case_sync "nth" `Quick nth]
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
      Alcotest_lwt.test_case_sync "last" `Quick last;
      Alcotest_lwt.test_case_sync "last_opt" `Quick last_opt;
    ]
end

module Init = struct
  let init () =
    assert (assert_err @@ init ~when_negative_length:() (-10) Fun.id) ;
    assert (init ~when_negative_length:() 0 Fun.id = Ok []) ;
    assert (init ~when_negative_length:() 11 Fun.id = Ok (up 10)) ;
    ()

  let init_e () =
    assert (assert_err @@ init_e ~when_negative_length:() (-10) ok) ;
    assert (init_e ~when_negative_length:() 0 ok = nil_e) ;
    assert (init_e ~when_negative_length:() 11 ok = ok @@ up 10) ;
    ()

  let init_s _ () =
    let open Lwt.Infix in
    assert_err_s (init_s ~when_negative_length:() (-10) Lwt.return)
    >>= fun () ->
    assert_eq_s (init_s ~when_negative_length:() 0 Lwt.return) nil_es
    >>= fun () ->
    assert_eq_s
      (init_s ~when_negative_length:() 11 Lwt.return)
      (Lwt.return_ok @@ up 10)
    >>= fun () -> Lwt.return_unit

  let init_es _ () =
    let open Lwt.Infix in
    assert_err_s (init_es ~when_negative_length:() (-10) Lwt.return_ok)
    >>= fun () ->
    assert_eq_s (init_es ~when_negative_length:() 0 Lwt.return_ok) nil_es
    >>= fun () ->
    assert_eq_s
      (init_es ~when_negative_length:() 11 Lwt.return_ok)
      (Lwt.return_ok @@ up 10)
    >>= fun () -> Lwt.return_unit

  let init_p _ () =
    let open Lwt.Infix in
    assert_err_s (init_p ~when_negative_length:() (-10) Lwt.return)
    >>= fun () ->
    assert_eq_s (init_p ~when_negative_length:() 0 Lwt.return) nil_es
    >>= fun () ->
    assert_eq_s
      (init_p ~when_negative_length:() 11 Lwt.return)
      (Lwt.return_ok @@ up 10)
    >>= fun () -> Lwt.return_unit

  let init_ep _ () =
    let open Lwt.Infix in
    assert_err_p (init_ep ~when_negative_length:() (-10) Lwt.return_ok)
    >>= fun () ->
    assert_eq_s (init_ep ~when_negative_length:() 0 Lwt.return_ok) nil_es
    >>= fun () ->
    assert_eq_s
      (init_ep ~when_negative_length:() 11 Lwt.return_ok)
      (Lwt.return_ok @@ up 10)
    >>= fun () -> Lwt.return_unit

  let tests =
    [
      Alcotest_lwt.test_case_sync "init" `Quick init;
      Alcotest_lwt.test_case_sync "init_e" `Quick init_e;
      Alcotest_lwt.test_case "init_s" `Quick init_s;
      Alcotest_lwt.test_case "init_es" `Quick init_es;
      Alcotest_lwt.test_case "init_p" `Quick init_p;
      Alcotest_lwt.test_case "init_ep" `Quick init_ep;
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
      Alcotest_lwt.test_case_sync "filter_some" `Quick filter_some;
      Alcotest_lwt.test_case_sync "filter_ok" `Quick filter_ok;
      Alcotest_lwt.test_case_sync "filter_error" `Quick filter_error;
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
    assert (combine_with_leftovers [] [0] = ([], Some (`Right [0]))) ;
    assert (combine_with_leftovers [0] [] = ([], Some (`Left [0]))) ;
    assert (
      combine_with_leftovers (up 100) (up 99)
      = (map (fun i -> (i, i)) (up 99), Some (`Left [100]))) ;
    ()

  let tests =
    [
      Alcotest_lwt.test_case_sync "combine-error" `Quick combine_error;
      Alcotest_lwt.test_case_sync "combine-ok" `Quick combine_ok;
      Alcotest_lwt.test_case_sync "combine_drop" `Quick combine_drop;
      Alcotest_lwt.test_case_sync
        "combine_with_leftovers"
        `Quick
        combine_with_leftovers;
    ]
end

module Partition = struct
  let cond x = x mod 2 = 0

  let partition_result () =
    assert (partition_result [] = ([], [])) ;
    assert (partition_result [Ok 0] = ([0], [])) ;
    assert (partition_result [Error 0] = ([], [0])) ;
    assert (partition_result (map ok (up 11)) = (up 11, [])) ;
    assert (partition_result (map (fun x -> Error x) (up 11)) = ([], up 11)) ;
    assert (
      let input = map (fun x -> if cond x then Ok x else Error x) (up 101) in
      partition_result input = (filter_ok input, filter_error input)) ;
    ()

  let tests =
    [Alcotest_lwt.test_case_sync "partition-result" `Quick partition_result]
end

let () =
  Alcotest_lwt.run
    "list-basic"
    [
      ("nth", Nth.tests);
      ("last", Last.tests);
      ("init", Init.tests);
      ("filter_*", FilterSmthg.tests);
      ("combine_*", Combine.tests);
      ("partition_*", Partition.tests);
    ]
  |> Lwt_main.run
