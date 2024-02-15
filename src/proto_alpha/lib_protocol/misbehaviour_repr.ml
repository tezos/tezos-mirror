(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type kind = Double_baking | Double_attesting | Double_preattesting

let kind_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        (Tag 0)
        ~title:"Double baking"
        (constant "double baking")
        (function Double_baking -> Some () | _ -> None)
        (fun () -> Double_baking);
      case
        (Tag 1)
        ~title:"Double attesting"
        (constant "double attesting")
        (function Double_attesting -> Some () | _ -> None)
        (fun () -> Double_attesting);
      case
        (Tag 2)
        ~title:"Double preattesting"
        (constant "double preattesting")
        (function Double_preattesting -> Some () | _ -> None)
        (fun () -> Double_preattesting);
    ]

type t = {
  kind : kind;
  level : Raw_level_repr.t;
  round : Round_repr.t;
  slot : Slot_repr.t;
}

let compare_kind a b =
  let to_int = function
    | Double_baking -> 0
    | Double_attesting -> 1
    | Double_preattesting -> 2
  in
  Compare.Int.compare (to_int a) (to_int b)

let equal_kind a b = Compare.Int.equal 0 (compare_kind a b)

let compare a b =
  Compare.or_else (Raw_level_repr.compare a.level b.level) @@ fun () ->
  Compare.or_else (Round_repr.compare a.round b.round) @@ fun () ->
  compare_kind a.kind b.kind

let encoding =
  let open Data_encoding in
  conv
    (fun {kind; level; round; slot} -> (kind, level, round, slot))
    (fun (kind, level, round, slot) -> {kind; level; round; slot})
    (obj4
       (req "kind" kind_encoding)
       (req "level" Raw_level_repr.encoding)
       (req "round" Round_repr.encoding)
       (req "slot" Slot_repr.encoding))
