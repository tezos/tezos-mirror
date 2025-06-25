(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type kind = Double_baking | Double_attesting | Double_preattesting

let kind_encoding =
  let open Data_encoding in
  string_enum
    [
      ("preattestation", Double_preattesting);
      ("attestation", Double_attesting);
      ("block", Double_baking);
    ]

type t = {level : Raw_level_repr.t; round : Round_repr.t; kind : kind}

let pp_kind fmt kind =
  Format.pp_print_string
    fmt
    (match kind with
    | Double_baking -> "double-baking"
    | Double_attesting -> "double-attesting"
    | Double_preattesting -> "double-preattesting")

let pp fmt {level; round; kind} =
  Format.fprintf
    fmt
    "%a at level %a, round %a"
    pp_kind
    kind
    Raw_level_repr.pp
    level
    Round_repr.pp
    round

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
    (fun {level; round; kind} -> (level, round, kind))
    (fun (level, round, kind) -> {level; round; kind})
    (obj3
       (req "level" Raw_level_repr.encoding)
       (req "round" Round_repr.encoding)
       (req "kind" kind_encoding))
