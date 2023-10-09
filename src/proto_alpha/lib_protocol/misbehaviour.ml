(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = Double_baking | Double_attesting

let encoding =
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
