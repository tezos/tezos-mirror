(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let roundtrip e v =
  let json = Data_encoding.Json.construct e v in
  let w = Data_encoding.Json.destruct e json in
  v = w

let test_roundtrip_safe_option () =
  let safeOption e =
    let open Data_encoding in
    let open Data_encoding.With_JSON_discriminant in
    union
      [
        case
          ~title:"Some"
          (Tag (0, "some"))
          (obj1 (req "value" e))
          Fun.id
          Option.some;
        case
          ~title:"None"
          (Tag (1, "none"))
          empty
          (function None -> Some () | _ -> None)
          (fun () -> None);
      ]
  in
  let e = safeOption (safeOption Data_encoding.int32) in
  assert (roundtrip e None) ;
  assert (roundtrip e (Some None)) ;
  assert (roundtrip e (Some (Some 0l))) ;
  assert (roundtrip e (Some (Some Int32.max_int))) ;
  ()

let test_fails_case_with_non_object () =
  let open Data_encoding in
  let open Data_encoding.With_JSON_discriminant in
  match case ~title:"dummy" (Tag (0, "dummy")) null Option.some Fun.id with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let test_fails_object_with_dup_field_names () =
  let open Data_encoding in
  let open Data_encoding.With_field_name_duplicate_checks in
  match
    obj5
      (req "crash" int8)
      (req "fine" int8)
      (req "ok" int8)
      (req "crash" float)
      (req "fineIguess" null)
  with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let test_fails_object_with_dup_field_names_hidden () =
  let open Data_encoding in
  let open Data_encoding.With_field_name_duplicate_checks in
  match
    merge_objs
      (obj3 (req "crash" int8) (req "fine" int8) (req "ok" int8))
      (def "crashing"
      @@ splitted
           ~binary:(tup2 float null)
           ~json:
             (conv
                (fun (f, ()) -> ((), f))
                (fun ((), f) -> (f, ()))
                (obj2 (req "fineIguess" null) (req "crash" float))))
  with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let test_fails_case_with_object_with_kind_field_name () =
  let open Data_encoding in
  let open Data_encoding.With_JSON_discriminant in
  match
    case
      ~title:"crash"
      (Tag (0, "crash"))
      (obj1 (req "kind" null))
      Option.some
      Fun.id
  with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let test_fails_match_with_object_with_kind_field_name () =
  let open Data_encoding in
  let open Data_encoding.With_JSON_discriminant in
  match matched (0, "crash") (obj1 (req "kind" null)) () with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let test_not_fail_object_with_nested_similar_field_names () =
  let open Data_encoding in
  let open Data_encoding.With_field_name_duplicate_checks in
  let _ =
    obj3
      (req "fine" (obj2 (req "crash" int8) (req "cool" int8)))
      (req "crash" int8)
      (req "cool" (obj2 (req "cool" int8) (req "crash" int8)))
  in
  ()

let test_fails_union_with_equal_json_kind_tags () =
  let open Data_encoding in
  let open Data_encoding.With_JSON_discriminant in
  match
    union
      [
        case ~title:"A" (Tag (0, "x")) empty Option.some Fun.id;
        case ~title:"B" (Tag (1, "x")) empty Option.some Fun.id;
      ]
  with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let tests =
  [
    ("safe-option", `Quick, test_roundtrip_safe_option);
    ("fails when not obj", `Quick, test_fails_case_with_non_object);
    ( "fails when explicit field conflict",
      `Quick,
      test_fails_object_with_dup_field_names );
    ( "fails when explicit field conflict even with indirection",
      `Quick,
      test_fails_object_with_dup_field_names_hidden );
    ( "fails when implicit field conflict",
      `Quick,
      test_fails_case_with_object_with_kind_field_name );
    ( "fails when implicit field conflict in matched",
      `Quick,
      test_fails_match_with_object_with_kind_field_name );
    ( "name clear with nest",
      `Quick,
      test_not_fail_object_with_nested_similar_field_names );
    ( "fails when kind conflict",
      `Quick,
      test_fails_union_with_equal_json_kind_tags );
  ]
