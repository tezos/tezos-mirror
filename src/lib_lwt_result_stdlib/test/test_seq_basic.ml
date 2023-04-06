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

(* Testing
   -------
   Component:    Stdlib
   Invocation:   dune exec src/lib_lwt_result_stdlib/test/main.exe
   Subject:      Test basic properties of sequence
*)

let ( >>== ) p v = Lwt_main.run (Lwt.map (( = ) v) p)

let vanilla () =
  let open Support.Lib.Seq in
  assert (List.of_seq empty = []) ;
  assert (List.of_seq (return 0) = [0]) ;
  assert (List.of_seq (cons 0 empty) = [0]) ;
  assert (List.of_seq (append empty empty) = []) ;
  let onetwothree = List.to_seq [1; 2; 3] in
  assert (
    List.of_seq @@ Result.get_ok (drop ~when_negative_length:() 0 empty) = []) ;
  assert (
    List.of_seq @@ Result.get_ok (drop ~when_negative_length:() 0 onetwothree)
    = [1; 2; 3]) ;
  assert (
    List.of_seq @@ Result.get_ok (drop ~when_negative_length:() 1 onetwothree)
    = [2; 3]) ;
  assert (
    List.of_seq @@ Result.get_ok (drop ~when_negative_length:() 2 onetwothree)
    = [3]) ;
  assert (
    List.of_seq @@ Result.get_ok (drop ~when_negative_length:() 3 onetwothree)
    = []) ;
  assert (
    List.of_seq @@ Result.get_ok (drop ~when_negative_length:() 4 onetwothree)
    = []) ;
  assert (drop ~when_negative_length:() (-1) onetwothree = Error ()) ;
  assert (List.of_seq (drop_while (fun _ -> false) empty) = []) ;
  assert (List.of_seq (drop_while (fun _ -> false) onetwothree) = [1; 2; 3]) ;
  assert (List.of_seq (drop_while (fun _ -> true) onetwothree) = []) ;
  assert (List.of_seq (drop_while (fun x -> x <= 1) onetwothree) = [2; 3]) ;
  assert (List.of_seq (drop_while (fun x -> x <= 2) onetwothree) = [3]) ;
  assert (
    List.of_seq @@ Result.get_ok (take ~when_negative_length:() 1 empty) = []) ;
  assert (
    List.of_seq @@ Result.get_ok (take ~when_negative_length:() 0 onetwothree)
    = []) ;
  assert (
    List.of_seq @@ Result.get_ok (take ~when_negative_length:() 1 onetwothree)
    = [1]) ;
  assert (
    List.of_seq @@ Result.get_ok (take ~when_negative_length:() 2 onetwothree)
    = [1; 2]) ;
  assert (
    List.of_seq @@ Result.get_ok (take ~when_negative_length:() 3 onetwothree)
    = [1; 2; 3]) ;
  assert (
    List.of_seq @@ Result.get_ok (take ~when_negative_length:() 4 onetwothree)
    = [1; 2; 3]) ;
  assert (List.of_seq (take_while (fun _ -> true) empty) = []) ;
  assert (List.of_seq (take_while (fun _ -> true) onetwothree) = [1; 2; 3]) ;
  assert (List.of_seq (take_while (fun _ -> false) onetwothree) = []) ;
  assert (List.of_seq (take_while (fun x -> x <= 1) onetwothree) = [1]) ;
  assert (List.of_seq (take_while (fun x -> x <= 2) onetwothree) = [1; 2]) ;
  ()

let seq_e () =
  let open Support.Lib.Seq_e in
  let to_list se =
    List.rev @@ Result.get_ok @@ fold_left (fun xs x -> x :: xs) [] se
  in
  let onetwothree = of_seq @@ List.to_seq [1; 2; 3] in
  assert (to_list @@ Result.get_ok (drop ~when_negative_length:() 0 empty) = []) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 0 onetwothree)
    = [1; 2; 3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 1 onetwothree)
    = [2; 3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 2 onetwothree) = [3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 3 onetwothree) = []) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 4 onetwothree) = []) ;
  assert (to_list (drop_while (fun _ -> false) empty) = []) ;
  assert (to_list (drop_while (fun _ -> false) onetwothree) = [1; 2; 3]) ;
  assert (to_list (drop_while (fun _ -> true) onetwothree) = []) ;
  assert (to_list (drop_while (fun x -> x <= 1) onetwothree) = [2; 3]) ;
  assert (to_list (drop_while (fun x -> x <= 2) onetwothree) = [3]) ;
  assert (to_list @@ Result.get_ok (take ~when_negative_length:() 1 empty) = []) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 0 onetwothree) = []) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 1 onetwothree) = [1]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 2 onetwothree)
    = [1; 2]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 3 onetwothree)
    = [1; 2; 3]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 4 onetwothree)
    = [1; 2; 3]) ;
  assert (to_list (take_while (fun _ -> true) empty) = []) ;
  assert (to_list (take_while (fun _ -> true) onetwothree) = [1; 2; 3]) ;
  assert (to_list (take_while (fun _ -> false) onetwothree) = []) ;
  assert (to_list (take_while (fun x -> x <= 1) onetwothree) = [1]) ;
  assert (to_list (take_while (fun x -> x <= 2) onetwothree) = [1; 2]) ;
  ()

let seq_s () =
  let open Support.Lib.Seq_s in
  let to_list se = Lwt.map List.rev @@ fold_left (fun xs x -> x :: xs) [] se in
  let onetwothree = of_seq @@ List.to_seq [1; 2; 3] in
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 0 empty) >>== []) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 0 onetwothree)
    >>== [1; 2; 3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 1 onetwothree)
    >>== [2; 3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 2 onetwothree)
    >>== [3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 3 onetwothree)
    >>== []) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 4 onetwothree)
    >>== []) ;
  assert (to_list (drop_while (fun _ -> false) empty) >>== []) ;
  assert (to_list (drop_while (fun _ -> false) onetwothree) >>== [1; 2; 3]) ;
  assert (to_list (drop_while (fun _ -> true) onetwothree) >>== []) ;
  assert (to_list (drop_while (fun x -> x <= 1) onetwothree) >>== [2; 3]) ;
  assert (to_list (drop_while (fun x -> x <= 2) onetwothree) >>== [3]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 1 empty) >>== []) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 0 onetwothree)
    >>== []) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 1 onetwothree)
    >>== [1]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 2 onetwothree)
    >>== [1; 2]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 3 onetwothree)
    >>== [1; 2; 3]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 4 onetwothree)
    >>== [1; 2; 3]) ;
  assert (to_list (take_while (fun _ -> true) empty) >>== []) ;
  assert (to_list (take_while (fun _ -> true) onetwothree) >>== [1; 2; 3]) ;
  assert (to_list (take_while (fun _ -> false) onetwothree) >>== []) ;
  assert (to_list (take_while (fun x -> x <= 1) onetwothree) >>== [1]) ;
  assert (to_list (take_while (fun x -> x <= 2) onetwothree) >>== [1; 2]) ;
  ()

let seq_es () =
  let open Support.Lib.Seq_es in
  let to_list se =
    Lwt.map List.rev @@ Lwt.map Result.get_ok
    @@ fold_left (fun xs x -> x :: xs) [] se
  in
  let onetwothree = of_seq @@ List.to_seq [1; 2; 3] in
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 0 empty) >>== []) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 0 onetwothree)
    >>== [1; 2; 3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 1 onetwothree)
    >>== [2; 3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 2 onetwothree)
    >>== [3]) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 3 onetwothree)
    >>== []) ;
  assert (
    to_list @@ Result.get_ok (drop ~when_negative_length:() 4 onetwothree)
    >>== []) ;
  assert (to_list (drop_while (fun _ -> false) empty) >>== []) ;
  assert (to_list (drop_while (fun _ -> false) onetwothree) >>== [1; 2; 3]) ;
  assert (to_list (drop_while (fun _ -> true) onetwothree) >>== []) ;
  assert (to_list (drop_while (fun x -> x <= 1) onetwothree) >>== [2; 3]) ;
  assert (to_list (drop_while (fun x -> x <= 2) onetwothree) >>== [3]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 1 empty) >>== []) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 0 onetwothree)
    >>== []) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 1 onetwothree)
    >>== [1]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 2 onetwothree)
    >>== [1; 2]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 3 onetwothree)
    >>== [1; 2; 3]) ;
  assert (
    to_list @@ Result.get_ok (take ~when_negative_length:() 4 onetwothree)
    >>== [1; 2; 3]) ;
  assert (to_list (take_while (fun _ -> true) empty) >>== []) ;
  assert (to_list (take_while (fun _ -> true) onetwothree) >>== [1; 2; 3]) ;
  assert (to_list (take_while (fun _ -> false) onetwothree) >>== []) ;
  assert (to_list (take_while (fun x -> x <= 1) onetwothree) >>== [1]) ;
  assert (to_list (take_while (fun x -> x <= 2) onetwothree) >>== [1; 2]) ;
  ()

let () =
  Alcotest.run
    ~__FILE__
    "seq"
    [
      ( "basic",
        [
          Alcotest.test_case "vanilla" `Quick vanilla;
          Alcotest.test_case "seq_e" `Quick seq_e;
          Alcotest.test_case "seq_s" `Quick seq_s;
          Alcotest.test_case "seq_es" `Quick seq_es;
        ] );
    ]
