(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test "test simple union" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"simple_union"
      Data_encoding.(
        union
          [
            case ~title:"some" (Tag 0) uint8 Fun.id Option.some;
            case
              ~title:"none"
              ~description:"no data available"
              (Tag 1)
              unit
              (function None -> Some () | Some _ -> None)
              (fun () -> None);
          ])
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: simple_union
      endian: be
    enums:
      simple_union_tag:
        0: some
        1:
          id: none
          doc: no data available
    seq:
    - id: simple_union_tag
      type: u1
      enum: simple_union_tag
    - id: some
      type: u1
      if: (simple_union_tag == simple_union_tag::some)
  |}]

let%expect_test "test medium union" =
  let module M = struct
    type t = A of int | B of int | C of bool | D
  end in
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"more_union"
      Data_encoding.(
        union
          [
            case
              ~title:"A"
              (Tag 0)
              uint8
              (function M.A i -> Some i | _ -> None)
              (fun i -> M.A i);
            case
              ~title:"B"
              (Tag 1)
              uint16
              (function M.B i -> Some i | _ -> None)
              (fun i -> M.B i);
            case
              ~title:"C"
              (Tag 2)
              bool
              (function M.C b -> Some b | _ -> None)
              (fun b -> M.C b);
            case
              ~title:"D"
              (Tag 255)
              unit
              (function M.D -> Some () | _ -> None)
              (fun () -> M.D);
          ])
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: more_union
      endian: be
    enums:
      bool:
        0: false
        255: true
      more_union_tag:
        0: a
        1: b
        2: c
        255: d
    seq:
    - id: more_union_tag
      type: u1
      enum: more_union_tag
    - id: a
      type: u1
      if: (more_union_tag == more_union_tag::a)
    - id: b
      type: u2
      if: (more_union_tag == more_union_tag::b)
    - id: c
      type: u1
      if: (more_union_tag == more_union_tag::c)
      enum: bool
  |}]

let%expect_test "test union with structures inside" =
  let module M = struct
    type t = A of int | B of (int * string) | C of (bool * bool) | D
  end in
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"more_union"
      Data_encoding.(
        union
          [
            case
              ~title:"A"
              (Tag 0)
              uint8
              (function M.A i -> Some i | _ -> None)
              (fun i -> M.A i);
            case
              ~title:"B"
              (Tag 1)
              (tup2 uint16 string)
              (function M.B (i, s) -> Some (i, s) | _ -> None)
              (fun (i, s) -> M.B (i, s));
            case
              ~title:"C"
              (Tag 2)
              (obj2 (req "l" bool) (dft "r" bool false))
              (function M.C (r, l) -> Some (r, l) | _ -> None)
              (fun (r, l) -> M.C (r, l));
            case
              ~title:"D"
              (Tag 255)
              unit
              (function M.D -> Some () | _ -> None)
              (fun () -> M.D);
          ])
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: more_union
      endian: be
    types:
      b:
        seq:
        - id: b_field0
          type: u2
        - id: b_field1
          type: b_field1
      b_field1:
        seq:
        - id: len_b_field1
          type: s4
        - id: b_field1
          size: len_b_field1
      c:
        seq:
        - id: l
          type: u1
          enum: bool
        - id: r
          type: u1
          enum: bool
    enums:
      bool:
        0: false
        255: true
      more_union_tag:
        0: a
        1: b
        2: c
        255: d
    seq:
    - id: more_union_tag
      type: u1
      enum: more_union_tag
    - id: a
      type: u1
      if: (more_union_tag == more_union_tag::a)
    - id: b
      type: b
      if: (more_union_tag == more_union_tag::b)
    - id: c
      type: c
      if: (more_union_tag == more_union_tag::c)
  |}]
