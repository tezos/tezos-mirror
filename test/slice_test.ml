(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* list *)
let list_enc =
  let open Data_encoding in
  def "list_enc" (list (tup2 string int31))

let list_n_enc =
  let open Data_encoding in
  def "list_n_enc" (list_with_length `N (tup2 string int31))

let list_example = [("bonjour", 4); ("au revoir", 2)]

let list_result =
  [
    "32";
    (* list length*)
    "7";
    (* string length*)
    "\"bonjour\"";
    "4";
    "9";
    (* string length*)
    "\"au revoir\"";
    "2";
  ]

let list_n_result =
  [
    "2";
    (* list length*)
    "7";
    (* string length*)
    "\"bonjour\"";
    "4";
    "9";
    (* string length*)
    "\"au revoir\"";
    "2";
  ]

(* record *)
type record = {p : int; q : string; b : bool; f : float}

let record_enc =
  let open Data_encoding in
  def
    "record_enc"
    (conv
       (fun {p; q; b; f} -> (p, q, b, f))
       (fun (p, q, b, f) -> {p; q; b; f})
       (obj4 (req "p" int31) (req "q" string) (req "b" bool) (req "f" float)))

let record_example = {p = 3; q = "foo"; b = true; f = 4.2}

let record_result =
  ["3"; (* p *) "3"; (* length q *) "\"foo\""; (* q *) "true"; (* b *) "4.2"]

(* f *)

(* union *)
type union = A of {arg1 : int; arg2 : bool} | B

let union_enc =
  let open Data_encoding in
  def
    "union_enc"
    (union
       ~tag_size:`Uint8
       [
         case
           ~title:"A"
           (Tag 0)
           (obj1 (req "A" (obj2 (req "arg1" int31) (req "arg2" bool))))
           (function A {arg1; arg2} -> Some (arg1, arg2) | _ -> None)
           (fun (arg1, arg2) -> A {arg1; arg2});
         case
           ~title:"B"
           (Tag 1)
           (obj1 (req "B" unit))
           (function B -> Some () | _ -> None)
           (fun () -> B);
       ])

let union_example1 = A {arg1 = 5; arg2 = true}

let union_result1 = ["0"; (* tag of A *) "5"; (* arg1 *) "true"]

(* arg2 *)

let union_example2 = B

let union_result2 = ["1"] (* tag of B *)

(* option *)
type option_test = int option list option

let option_enc =
  let open Data_encoding in
  def "option_enc" (option (list (option int31)))

let option_n_enc =
  let open Data_encoding in
  def "option_n_enc" (option (list_with_length `N (option int31)))

let option_example1 = Some [Some 5; Some 10; None]

let option_result1 =
  [
    "1";
    (* tag of Some *)
    "11";
    (* list length *)
    "1";
    (* tag of Some *)
    "5";
    "1";
    (* tag of Some *)
    "10";
    "0";
  ]

let option_n_result1 =
  [
    "1";
    (* tag of Some *)
    "3";
    (* list length *)
    "1";
    (* tag of Some *)
    "5";
    "1";
    (* tag of Some *)
    "10";
    "0";
  ]

(* tag of None *)

let option_example2 = Some []

let option_result2 = ["1"; (* tag of Some *) "0"]

let option_n_result2 = ["1"; (* tag of Some *) "0"]

(* list length *)

(* parametered type *)
type ('a, 'b) param = A of 'a | B of 'b | C of 'a * 'b

let parameter_enc =
  let open Data_encoding in
  def
    "parameter_enc"
    ((fun _a_encoding _b_encoding ->
       union
         ~tag_size:`Uint8
         [
           case
             ~title:"A"
             (Tag 0)
             (obj1 (req "A" _a_encoding))
             (function A a -> Some a | _ -> None)
             (fun a -> A a);
           case
             ~title:"B"
             (Tag 1)
             (obj1 (req "B" _b_encoding))
             (function B b -> Some b | _ -> None)
             (fun b -> B b);
           case
             ~title:"C"
             (Tag 2)
             (obj1 (req "C" (tup2 _a_encoding _b_encoding)))
             (function C (c1, c2) -> Some (c1, c2) | _ -> None)
             (fun (c1, c2) -> C (c1, c2));
         ])
       int31
       (list bool))

let parameter_example = C (7, [])

let parameter_result = ["2"; (* tag of C *) "7"; "0"]

(* list length *)

(* type recursive *)
type recursive = R of recursive list

let recursive_enc =
  let open! Data_encoding in
  def
    "recursive_enc"
    (mu "t" (fun t_encoding ->
         conv (fun (R r) -> r) (fun r -> R r) (obj1 (req "R" (list t_encoding)))))

let recursive_n_enc =
  let open! Data_encoding in
  def
    "recursive_n_enc"
    (mu "t" (fun t_encoding ->
         conv
           (fun (R r) -> r)
           (fun r -> R r)
           (obj1 (req "R" (list_with_length `N t_encoding)))))

let recursive_example = R [R []]

let recursive_result = ["4"; (* list length *) "0"]

let recursive_n_result = ["1"; (* list length *) "0"]

(* list length *)

(* qualified type *)
let qualified_enc =
  let open! Data_encoding in
  def "qualified_enc" int31

let qualified_example = 5

let qualified_result = ["5"]

let () =
  Data_encoding.Registration.register list_enc ;
  Data_encoding.Registration.register list_n_enc ;
  Data_encoding.Registration.register record_enc ;
  Data_encoding.Registration.register union_enc ;
  Data_encoding.Registration.register option_enc ;
  Data_encoding.Registration.register option_n_enc ;
  Data_encoding.Registration.register parameter_enc ;
  Data_encoding.Registration.register recursive_enc ;
  Data_encoding.Registration.register recursive_n_enc ;
  Data_encoding.Registration.register qualified_enc

let bin_list = Data_encoding.Binary.to_string_exn list_enc list_example

let bin_list_n = Data_encoding.Binary.to_string_exn list_n_enc list_example

let bin_record = Data_encoding.Binary.to_string_exn record_enc record_example

let bin_union1 = Data_encoding.Binary.to_string_exn union_enc union_example1

let bin_union2 = Data_encoding.Binary.to_string_exn union_enc union_example2

let bin_option1 = Data_encoding.Binary.to_string_exn option_enc option_example1

let bin_option2 = Data_encoding.Binary.to_string_exn option_enc option_example2

let bin_option_n_1 =
  Data_encoding.Binary.to_string_exn option_n_enc option_example1

let bin_option_n_2 =
  Data_encoding.Binary.to_string_exn option_n_enc option_example2

let bin_parameter =
  Data_encoding.Binary.to_string_exn parameter_enc parameter_example

let bin_recursive =
  Data_encoding.Binary.to_string_exn recursive_enc recursive_example

let bin_recursive_n =
  Data_encoding.Binary.to_string_exn recursive_n_enc recursive_example

let bin_qualified =
  Data_encoding.Binary.to_string_exn qualified_enc qualified_example

let rec check_sliced_fields_result fl rl =
  match (fl, rl) with
  | [], [] -> ()
  | {Data_encoding.Binary.Slicer.pretty_printed; _} :: tf, r :: tr ->
      if pretty_printed = r then check_sliced_fields_result tf tr
      else Alcotest.failf "Unexpected slicintg result %s /= %s" pretty_printed r
  | _ -> Alcotest.failf "Unexpected size of resulted list of fields"

let print_field_list l =
  List.iter
    (fun {Data_encoding.Binary.Slicer.pretty_printed; _} ->
      print_string (pretty_printed ^ "\n"))
    l

let slice_test id result expected () =
  match Data_encoding.Registration.find id with
  | None -> Alcotest.failf "Could not find %s" id
  | Some r -> (
      match Data_encoding.Registration.slice r result with
      | Error e ->
          Alcotest.failf "Error %a" Data_encoding.Binary.pp_read_error e
      | Ok l -> check_sliced_fields_result l expected)

let tests =
  [
    ("slice-test-list", `Quick, slice_test "list_enc" bin_list list_result);
    ( "slice-test-list_n",
      `Quick,
      slice_test "list_n_enc" bin_list_n list_n_result );
    ( "slice-test-record",
      `Quick,
      slice_test "record_enc" bin_record record_result );
    ( "slice-test-union1",
      `Quick,
      slice_test "union_enc" bin_union1 union_result1 );
    ( "slice-test-union2",
      `Quick,
      slice_test "union_enc" bin_union2 union_result2 );
    ( "slice-test-option1",
      `Quick,
      slice_test "option_enc" bin_option1 option_result1 );
    ( "slice-test-option2",
      `Quick,
      slice_test "option_enc" bin_option2 option_result2 );
    ( "slice-test-option_n_1",
      `Quick,
      slice_test "option_n_enc" bin_option_n_1 option_n_result1 );
    ( "slice-test-option_n_2",
      `Quick,
      slice_test "option_n_enc" bin_option_n_2 option_n_result2 );
    ( "slice-test-parameter",
      `Quick,
      slice_test "parameter_enc" bin_parameter parameter_result );
    ( "slice-test-legacy-recursive",
      `Quick,
      slice_test "recursive_enc" bin_recursive recursive_result );
    ( "slice-test-recursive_n",
      `Quick,
      slice_test "recursive_n_enc" bin_recursive_n recursive_n_result );
    ( "slice-test-qualified",
      `Quick,
      slice_test "qualified_enc" bin_qualified qualified_result );
  ]
