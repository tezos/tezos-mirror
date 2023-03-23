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

(* Testing
   -------
   Component:    Stdlib
   Invocation:   dune exec src/lib_lwt_result_stdlib/test/main.exe
   Subject:      Test basic properties of lwt list
*)

open Support.Lib.Monad

let assert_eq_s pa pb =
  let open Lwt_syntax in
  let* a = pa and* b = pb in
  assert (a = b) ;
  return_unit

let assert_err e = e = Error ()

let assert_err_s e =
  let open Lwt_syntax in
  let* e in
  assert (e = Error ()) ;
  return_unit

let assert_err_p e =
  let open Lwt_syntax in
  let* e in
  assert (e = Error (Support.Test_trace.make ())) ;
  return_unit

module ListGen = struct
  include Support.Lib.List

  let rec up n i : int t = if i > n then [] else i :: up n (succ i)

  let up n = up n 0
end

open ListGen

module Init = struct
  let init () =
    assert (assert_err @@ init ~when_negative_length:() (-10) Fun.id) ;
    assert (init ~when_negative_length:() 0 Fun.id = Ok []) ;
    assert (init ~when_negative_length:() 11 Fun.id = Ok (up 10)) ;
    ()

  let init_e () =
    assert (assert_err @@ init_e ~when_negative_length:() (-10) Result.ok) ;
    assert (init_e ~when_negative_length:() 0 Result.ok = nil_e) ;
    assert (init_e ~when_negative_length:() 11 Result.ok = Result.ok @@ up 10) ;
    ()

  let init_s _ () =
    let open Lwt_syntax in
    let* () = assert_err_s (init_s ~when_negative_length:() (-10) Lwt.return) in
    let* () =
      assert_eq_s (init_s ~when_negative_length:() 0 Lwt.return) nil_es
    in
    let* () =
      assert_eq_s
        (init_s ~when_negative_length:() 11 Lwt.return)
        (Lwt.return_ok @@ up 10)
    in
    Lwt.return_unit

  let init_es _ () =
    let open Lwt_syntax in
    let* () =
      assert_err_s (init_es ~when_negative_length:() (-10) Lwt.return_ok)
    in
    let* () =
      assert_eq_s (init_es ~when_negative_length:() 0 Lwt.return_ok) nil_es
    in
    let* () =
      assert_eq_s
        (init_es ~when_negative_length:() 11 Lwt.return_ok)
        (Lwt.return_ok @@ up 10)
    in
    Lwt.return_unit

  let init_p _ () =
    let open Lwt_syntax in
    let* () = assert_err_s (init_p ~when_negative_length:() (-10) Lwt.return) in
    let* () =
      assert_eq_s (init_p ~when_negative_length:() 0 Lwt.return) nil_es
    in
    let* () =
      assert_eq_s
        (init_p ~when_negative_length:() 11 Lwt.return)
        (Lwt.return_ok @@ up 10)
    in
    Lwt.return_unit

  let init_ep _ () =
    let open Lwt_syntax in
    let* () =
      assert_err_p (init_ep ~when_negative_length:() (-10) Lwt.return_ok)
    in
    let* () =
      assert_eq_s (init_ep ~when_negative_length:() 0 Lwt.return_ok) nil_es
    in
    let* () =
      assert_eq_s
        (init_ep ~when_negative_length:() 11 Lwt.return_ok)
        (Lwt.return_ok @@ up 10)
    in
    Lwt.return_unit

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

module ConcatMap = struct
  let concat_map () =
    assert (concat_map Fun.id [] = []) ;
    assert (concat_map Fun.id [[]; []; []; []] = []) ;
    assert (concat_map (fun _ -> assert false) [] = []) ;
    assert (concat_map (fun _ -> []) [1] = []) ;
    assert (concat_map (fun x -> [x]) [1] = [1]) ;
    assert (concat_map (fun x -> [x; 0; x; x]) [1] = [1; 0; 1; 1]) ;
    assert (concat_map (fun _ -> []) [1; 2; 3] = []) ;
    assert (concat_map (fun x -> [x]) [1; 2; 3] = [1; 2; 3]) ;
    assert (
      concat_map (fun x -> [x; x; 2 * x]) [1; 2; 3]
      = [1; 1; 2; 2; 2; 4; 3; 3; 6]) ;
    ()

  let concat_map_e () =
    assert (concat_map_e Result.ok [] = Ok []) ;
    assert (concat_map_e Result.ok [[]; []; []; []] = Ok []) ;
    assert (concat_map_e (fun _ -> assert false) [] = Ok []) ;
    assert (concat_map_e (fun _ -> Ok []) [1] = Ok []) ;
    assert (concat_map_e (fun x -> Ok [x]) [1] = Ok [1]) ;
    assert (concat_map_e (fun x -> Ok [x; 0; x; x]) [1] = Ok [1; 0; 1; 1]) ;
    assert (concat_map_e (fun _ -> Ok []) [1; 2; 3] = Ok []) ;
    assert (concat_map_e (fun x -> Ok [x]) [1; 2; 3] = Ok [1; 2; 3]) ;
    assert (
      concat_map_e (fun x -> Ok [x; x; 2 * x]) [1; 2; 3]
      = Ok [1; 1; 2; 2; 2; 4; 3; 3; 6]) ;
    assert (concat_map_e (fun _ -> Error ()) [] = Ok []) ;
    assert (concat_map_e (fun _ -> Error ()) [[]; []; []; []] = Error ()) ;
    assert (concat_map_e (fun _ -> Error ()) [1] = Error ()) ;
    assert (concat_map_e (fun _ -> Error ()) [1; 2; 3] = Error ()) ;
    assert (concat_map_e (fun x -> Error x) [1; 2; 3] = Error 1) ;
    assert (
      concat_map_e (fun x -> if x <= 1 then Ok [x] else Error x) [1; 2; 3]
      = Error 2) ;
    ()

  let concat_map_s _ () =
    let open Lwt_syntax in
    let* () = assert_eq_s (concat_map_s Lwt.return []) nil_s in
    let* () = assert_eq_s (concat_map_s Lwt.return [[]; []; []; []]) nil_s in
    let* () = assert_eq_s (concat_map_s (fun _ -> assert false) []) nil_s in
    let* () = assert_eq_s (concat_map_s (fun _ -> Lwt.return []) [1]) nil_s in
    let* () =
      assert_eq_s (concat_map_s (fun x -> Lwt.return [x]) [1]) (Lwt.return [1])
    in
    let* () =
      assert_eq_s
        (concat_map_s (fun x -> Lwt.return [x; 0; x; x]) [1])
        (Lwt.return [1; 0; 1; 1])
    in
    let* () =
      assert_eq_s (concat_map_s (fun _ -> Lwt.return []) [1; 2; 3]) nil_s
    in
    let* () =
      assert_eq_s
        (concat_map_s (fun x -> Lwt.return [x]) [1; 2; 3])
        (Lwt.return [1; 2; 3])
    in
    let* () =
      assert_eq_s
        (concat_map_s (fun x -> Lwt.return [x; x; 2 * x]) [1; 2; 3])
        (Lwt.return [1; 1; 2; 2; 2; 4; 3; 3; 6])
    in
    Lwt.return_unit

  let concat_map_es _ () =
    let open Lwt_syntax in
    let* () = assert_eq_s (concat_map_es Lwt.return_ok []) nil_es in
    let* () =
      assert_eq_s (concat_map_es Lwt.return_ok [[]; []; []; []]) nil_es
    in
    let* () = assert_eq_s (concat_map_es (fun _ -> assert false) []) nil_es in
    let* () =
      assert_eq_s (concat_map_es (fun _ -> Lwt.return_ok []) [1]) nil_es
    in
    let* () =
      assert_eq_s
        (concat_map_es (fun x -> Lwt.return_ok [x]) [1])
        (Lwt.return_ok [1])
    in
    let* () =
      assert_eq_s
        (concat_map_es (fun x -> Lwt.return_ok [x; 0; x; x]) [1])
        (Lwt.return_ok [1; 0; 1; 1])
    in
    let* () =
      assert_eq_s (concat_map_es (fun _ -> Lwt.return_ok []) [1; 2; 3]) nil_es
    in
    let* () =
      assert_eq_s
        (concat_map_es (fun x -> Lwt.return_ok [x]) [1; 2; 3])
        (Lwt.return_ok [1; 2; 3])
    in
    let* () =
      assert_eq_s
        (concat_map_es (fun x -> Lwt.return_ok [x; x; 2 * x]) [1; 2; 3])
        (Lwt.return_ok [1; 1; 2; 2; 2; 4; 3; 3; 6])
    in
    let* () =
      assert_eq_s (concat_map_es (fun _ -> Lwt.return_error ()) []) nil_es
    in
    let* () =
      assert_eq_s
        (concat_map_es (fun _ -> Lwt.return_error ()) [[]; []; []; []])
        (Lwt.return_error ())
    in
    let* () =
      assert_eq_s
        (concat_map_es (fun _ -> Lwt.return_error ()) [1])
        (Lwt.return_error ())
    in
    let* () =
      assert_eq_s
        (concat_map_es (fun _ -> Lwt.return_error ()) [1; 2; 3])
        (Lwt.return_error ())
    in
    let* () =
      assert_eq_s
        (concat_map_es (fun x -> Lwt.return_error x) [1; 2; 3])
        (Lwt.return_error 1)
    in
    let* () =
      assert_eq_s
        (concat_map_es
           (fun x -> if x <= 1 then Lwt.return_ok [x] else Lwt.return_error x)
           [1; 2; 3])
        (Lwt.return_error 2)
    in
    Lwt.return_unit

  let tests =
    [
      Alcotest_lwt.test_case_sync "concat_map" `Quick concat_map;
      Alcotest_lwt.test_case_sync "concat_map_e" `Quick concat_map_e;
      Alcotest_lwt.test_case "concat_map_s" `Quick concat_map_s;
      Alcotest_lwt.test_case "concat_map_es" `Quick concat_map_es;
    ]
end

module Fold = struct
  let list = [1; 2; 3; 4]

  let list_sum = 10

  let test_fold_left_map () =
    assert (List.fold_left_map (fun _ _ -> assert false) 0 [] = (0, [])) ;
    assert (List.fold_left_map (fun a b -> (a + b, b)) 0 list = (list_sum, list))

  let test_fold_left_map_e () =
    assert (
      fold_left_map_e (fun a b -> Ok (a + b, b)) 0 list = Ok (list_sum, list)) ;
    assert (fold_left_map_e (fun _ _ -> assert false) 0 [] = Ok (0, [])) ;
    let x =
      fold_left_map_e (fun s x -> if x < 3 then Ok (s, x) else Error x) 0 list
    in
    assert (x = Error 3)

  let test_fold_left_map_s _ () =
    let open Lwt_syntax in
    let* () =
      assert_eq_s
        (fold_left_map_s (fun _ _ -> assert false) 0 [])
        (Lwt.return (0, []))
    in
    let* () =
      assert_eq_s
        (fold_left_map_s (fun a b -> Lwt.return (a + b, b)) 0 list)
        (Lwt.return (list_sum, list))
    in
    let p =
      fold_left_map_s
        (fun a b -> if a < 3 then Lwt.return (a + 1, b) else Lwt.fail Exit)
        0
        list
    in
    assert (Lwt.state p = Lwt.Fail Exit) ;
    Lwt.return ()

  let test_fold_left_map_es _ () =
    let open Lwt_syntax in
    let* () =
      assert_eq_s
        (fold_left_map_es (fun _ _ -> assert false) 0 [])
        (return_ok (0, []))
    in
    let* () =
      assert_eq_s
        (fold_left_map_es (fun a b -> return_ok (a + b, b)) 0 list)
        (return_ok (list_sum, list))
    in
    let open Lwt_result_syntax in
    assert_eq_s
      (fold_left_map_es
         (fun s x -> if x < 3 then return (s, x) else fail x)
         0
         list)
      (fail 3)

  let tests =
    [
      Alcotest_lwt.test_case_sync "fold_left-map" `Quick test_fold_left_map;
      Alcotest_lwt.test_case_sync "fold_left_map_e" `Quick test_fold_left_map_e;
      Alcotest_lwt.test_case "fold_left_map_s" `Quick test_fold_left_map_s;
      Alcotest_lwt.test_case "fold_left_map_es" `Quick test_fold_left_map_es;
    ]
end

let () =
  Alcotest_lwt.run
    "list-basic-lwt"
    [
      ("init", Init.tests);
      ("concat_map_*", ConcatMap.tests);
      ("fold", Fold.tests);
    ]
  |> Lwt_main.run
