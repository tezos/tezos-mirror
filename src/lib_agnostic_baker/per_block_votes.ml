(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** TODO: This is a copy paste of `src/proto_alpha/lib_protocol/per_block_votes.ml`,
    arguably it should be moved to the environment as the agnostic baker needs it. *)

type per_block_vote =
  | Per_block_vote_on
  | Per_block_vote_off
  | Per_block_vote_pass

type per_block_votes = {liquidity_baking_vote : per_block_vote}

let per_block_vote_compact_encoding =
  let open Data_encoding in
  let open Compact in
  union
    ~union_tag_bits:2
    ~cases_tag_bits:0
    [
      case
        ~title:"per_block_vote_on"
        (payload (constant "on"))
        (function Per_block_vote_on -> Some () | _ -> None)
        (fun () -> Per_block_vote_on);
      case
        ~title:"per_block_vote_off"
        (payload (constant "off"))
        (function Per_block_vote_off -> Some () | _ -> None)
        (fun () -> Per_block_vote_off);
      case
        ~title:"per_block_vote_pass"
        (payload (constant "pass"))
        (function Per_block_vote_pass -> Some () | _ -> None)
        (fun () -> Per_block_vote_pass);
    ]

let liquidity_baking_vote_encoding =
  let open Data_encoding in
  def
    "liquidity_baking_vote"
    (Compact.make ~tag_size:`Uint8 per_block_vote_compact_encoding)

let adaptive_issuance_vote_encoding =
  let open Data_encoding in
  def
    "adaptive_issuance_vote"
    (Compact.make ~tag_size:`Uint8 per_block_vote_compact_encoding)
