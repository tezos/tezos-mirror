(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test "test basic mu" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"intlist"
      Data_encoding.(
        mu
          "ilist"
          ~title:"Simple integer list"
          ~description:"Using the mu combinator for lists just to test mu"
          (fun ilist ->
            union
              [
                case
                  ~title:"Nil"
                  (Tag 0)
                  unit
                  (function [] -> Some () | _ -> None)
                  (fun () -> []);
                case
                  ~title:"Cons"
                  (Tag 1)
                  (obj2 (req "hd" uint16) (req "tl" ilist))
                  (function hd :: tl -> Some (hd, tl) | _ -> None)
                  (fun (hd, tl) -> hd :: tl);
              ]))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: intlist
      endian: be
    types:
      ilist:
        seq:
        - id: ilist_tag
          type: u1
          enum: ilist_tag
        - id: ilist_cons
          type: ilist_cons
          if: (ilist_tag == ilist_tag::cons)
      ilist_cons:
        seq:
        - id: hd
          type: u2
        - id: tl
          type: ilist
    enums:
      ilist_tag:
        0: nil
        1: cons
    seq:
    - id: ilist
      type: ilist
      doc: ! 'Simple integer list: Using the mu combinator for lists just to test mu'
  |}]

let%expect_test "test more mu" =
  let module M = struct
    type t = Empty | One of bool | Seq of bool * t | Branch of bool * t list
  end in
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"t"
      Data_encoding.(
        mu "mt" (fun mt ->
            union
              [
                case
                  ~title:"Empty"
                  (Tag 0)
                  unit
                  (function M.Empty -> Some () | _ -> None)
                  (fun () -> M.Empty);
                case
                  ~title:"One"
                  (Tag 1)
                  bool
                  (function M.One b -> Some b | _ -> None)
                  (fun b -> M.One b);
                case
                  ~title:"Seq"
                  (Tag 2)
                  (obj2 (req "payload" bool) (req "seq" mt))
                  (function M.Seq (b, t) -> Some (b, t) | _ -> None)
                  (fun (b, t) -> M.Seq (b, t));
                case
                  ~title:"Branch"
                  (Tag 3)
                  (obj2 (req "payload" bool) (req "branches" (list mt)))
                  (function M.Branch (b, t) -> Some (b, t) | _ -> None)
                  (fun (b, t) -> M.Branch (b, t));
              ]))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: t
      endian: be
    types:
      branches:
        seq:
        - id: len_branches
          type: s4
        - id: branches
          type: branches_entries
          size: len_branches
          repeat: eos
      branches_entries:
        seq:
        - id: branches_elt
          type: mt
      mt:
        seq:
        - id: mt_tag
          type: u1
          enum: mt_tag
        - id: mt_one
          type: u1
          if: (mt_tag == mt_tag::one)
          enum: bool
        - id: mt_seq
          type: mt_seq
          if: (mt_tag == mt_tag::seq)
        - id: mt_branch
          type: mt_branch
          if: (mt_tag == mt_tag::branch)
      mt_branch:
        seq:
        - id: payload
          type: u1
          enum: bool
        - id: branches
          type: branches
      mt_seq:
        seq:
        - id: payload
          type: u1
          enum: bool
        - id: seq
          type: mt
    enums:
      bool:
        0: false
        255: true
      mt_tag:
        0: empty
        1: one
        2: seq
        3: branch
    seq:
    - id: mt
      type: mt
  |}]
