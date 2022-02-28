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

let documentation_mentions_correct_tag_bit_counts () =
  let open Data_encoding.Compact in
  let int = payload Data_encoding.int31 in
  assert (tag_bit_count (payload Data_encoding.unit) = 0) ;
  assert (tag_bit_count (payload Data_encoding.int32) = 0) ;
  assert (tag_bit_count int = 0) ;
  assert (tag_bit_count void = 0) ;
  assert (tag_bit_count (option (payload Data_encoding.unit)) = 1) ;
  assert (tag_bit_count (option int) = 1) ;
  assert (tag_bit_count (tup1 int) = 0) ;
  assert (tag_bit_count (tup1 (option int)) = 1) ;
  assert (tag_bit_count (tup2 (option int) int) = 1) ;
  assert (tag_bit_count (tup2 (option int) (option int)) = 2) ;
  assert (tag_bit_count (obj1 (req "one" int)) = 0) ;
  assert (tag_bit_count (obj1 (opt "one" int)) = 1) ;
  assert (tag_bit_count (obj2 (opt "one" int) (req "two" int)) = 1) ;
  assert (tag_bit_count (obj2 (opt "one" int) (opt "two" int)) = 2) ;
  assert (tag_bit_count int32 = 2) ;
  assert (tag_bit_count int64 = 2) ;
  assert (
    tag_bit_count
      (or_int32 ~int32_title:"i32" ~alt_title:"alt" Data_encoding.unit)
    = 2) ;
  assert (tag_bit_count (list ~bits:0 Data_encoding.int31) = 0) ;
  assert (tag_bit_count (list ~bits:1 Data_encoding.int31) = 1) ;
  assert (tag_bit_count (list ~bits:2 Data_encoding.int31) = 2) ;
  assert (tag_bit_count (list ~bits:3 Data_encoding.int31) = 3) ;
  assert (tag_bit_count (list ~bits:4 Data_encoding.int31) = 4) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:0
         ~cases_tag_bits:0
         [case ~title:"unit" unit Option.some Fun.id])
    = 0) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:1
         ~cases_tag_bits:0
         [case ~title:"unit" unit Option.some Fun.id])
    = 1) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:0
         ~cases_tag_bits:1
         [case ~title:"unit" (option unit) Option.some Fun.id])
    = 1) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:1
         ~cases_tag_bits:1
         [case ~title:"unit" unit Option.some Fun.id])
    = 2) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:3
         ~cases_tag_bits:2
         [case ~title:"unit" unit Option.some Fun.id])
    = 5) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:7
         ~cases_tag_bits:6
         [
           case ~title:"unit" unit Option.some Fun.id;
           void_case ~title:"VOID01";
           void_case ~title:"VOID10";
           void_case ~title:"VOID11";
         ])
    = 13) ;
  assert (
    let either a b =
      union
        [
          case ~title:"Left" a Either.find_left Either.left;
          case ~title:"Right" b Either.find_right Either.right;
        ]
    in
    (* [bool] takes 1 bit of tag (0 bytes of case) *)
    (* [option] takes 1 bit of union-tag + the bits the case needs *)
    (* [(option bool)] takes 2 bits of tag (1 + 1) *)
    (* [either] takes 1 bit of union-tag (2 cases) + the bits the cases needs *)
    (* [(either bool bool)] takes 2 bits of tag (1 + max (1, 1)) *)
    (* the whole encoding takes takes 3 bits of tag (1 + max (2, 2)) *)
    tag_bit_count (either (either bool bool) (option bool)) = 3) ;
  ()

let roundtrip_binary loc encoding1 encoding2 value =
  let blob = Data_encoding.Binary.to_string_exn encoding1 value in
  let value' = Data_encoding.Binary.of_string_exn encoding2 blob in
  if value <> value' then raise (Failure ("Roundtrip failure at " ^ loc))

let no_roundtrip_binary loc encoding1 encoding2 value =
  let blob = Data_encoding.Binary.to_string_exn encoding1 value in
  match Data_encoding.Binary.of_string_exn encoding2 blob with
  | exception Data_encoding.Binary.Read_error _ -> ()
  | value' ->
      if value = value' then
        raise (Failure ("Unexpected successful rountrip at " ^ loc))

let roundtrip_with_voids () =
  let open Data_encoding.Compact in
  let casel =
    case
      ~title:"Left"
      (payload Data_encoding.uint8)
      Either.find_left
      Either.left
  in
  let caser =
    case
      ~title:"Right"
      (payload Data_encoding.uint8)
      Either.find_right
      Either.right
  in
  let all_inputs = List.init 256 Either.left @ List.init 256 Either.right in
  let compatible_unions =
    [
      union ~union_tag_bits:1 [casel; caser];
      union ~union_tag_bits:2 [casel; caser];
      union
        ~union_tag_bits:2
        [casel; caser; void_case ~title:"a"; void_case ~title:"b"];
    ]
  in
  let incompatible_unions =
    [
      union ~union_tag_bits:1 [caser; casel];
      union
        ~union_tag_bits:2
        [void_case ~title:"a"; void_case ~title:"b"; casel; caser];
      union
        ~union_tag_bits:2
        [void_case ~title:"a"; casel; void_case ~title:"b"; caser];
      union
        ~union_tag_bits:2
        [void_case ~title:"a"; casel; caser; void_case ~title:"b"];
    ]
  in
  List.iter
    (fun encoding1 ->
      let encoding1 = make ~tag_size:`Uint8 encoding1 in
      List.iter
        (fun encoding2 ->
          let encoding2 = make ~tag_size:`Uint8 encoding2 in
          List.iter
            (fun value -> roundtrip_binary __LOC__ encoding1 encoding2 value)
            all_inputs)
        compatible_unions)
    compatible_unions ;
  List.iter
    (fun encoding1 ->
      let encoding1 = make ~tag_size:`Uint8 encoding1 in
      List.iter
        (fun encoding2 ->
          let encoding2 = make ~tag_size:`Uint8 encoding2 in
          List.iter
            (fun value ->
              no_roundtrip_binary __LOC__ encoding1 encoding2 value ;
              no_roundtrip_binary __LOC__ encoding2 encoding1 value)
            all_inputs)
        compatible_unions)
    incompatible_unions

let roundtrip_option_bool () =
  let open Data_encoding in
  let encoding =
    let open Compact in
    let either a b =
      union
        [
          case ~title:"Left" a Either.find_left Either.left;
          case ~title:"Right" b Either.find_right Either.right;
        ]
    in
    (* We also check that the whole data is encoding onto exactly one byte using
       this [check_size] combinator. *)
    check_size 1
    @@ make ~tag_size:`Uint8 (either (option bool) (either bool unit))
  in
  let inputs =
    [
      Either.Left None;
      Either.Left (Some true);
      Either.Left (Some false);
      Either.Right (Either.Left true);
      Either.Right (Either.Left false);
      Either.Right (Either.Right ());
    ]
  in
  List.iter (roundtrip_binary __LOC__ encoding encoding) inputs

let tests =
  [
    ( "tag_bit_count documentation",
      `Quick,
      documentation_mentions_correct_tag_bit_counts );
    ("roundtrip (heavy on void)", `Quick, roundtrip_with_voids);
    ("roundtrip (option bool)", `Quick, roundtrip_option_bool);
  ]
