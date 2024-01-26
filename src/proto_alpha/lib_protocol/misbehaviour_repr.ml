(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type kind = Double_baking | Double_attesting

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
    ]

type t = {
  kind : kind;
  level : Raw_level_repr.t;
  round : Round_repr.t;
  slot : Slot_repr.t;
}

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
