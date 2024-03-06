(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module is used to load the baker's per block votes
    configurations. When a file is given as configuration, its content
    is expected to be a valid JSON matching the following examples:
    - {v {"liquidity_baking_toggle_vote": "on"} v}
    - {v {"liquidity_baking_toggle_vote": "off"} v}
    - {v {"liquidity_baking_toggle_vote": "pass"} v}
    - {v {"adaptive_issuance_vote": "on"} v}
    - {v {"adaptive_issuance_vote": "off"} v}
    - {v {"adaptive_issuance_vote": "pass"} v}
    - {v {"liquidity_baking_toggle_vote": "on","adaptive_issuance_vote": "on"} v}
    - {v {"liquidity_baking_toggle_vote": "on","adaptive_issuance_vote": "off"} v}
    - {v {"liquidity_baking_toggle_vote": "on","adaptive_issuance_vote": "pass"} v}
    - {v {"liquidity_baking_toggle_vote": "off","adaptive_issuance_vote": "on"} v}
    - {v {"liquidity_baking_toggle_vote": "off","adaptive_issuance_vote": "off"} v}
    - {v {"liquidity_baking_toggle_vote": "off","adaptive_issuance_vote": "pass"} v}
    - {v {"liquidity_baking_toggle_vote": "pass","adaptive_issuance_vote": "on"} v}
    - {v {"liquidity_baking_toggle_vote": "pass","adaptive_issuance_vote": "off"} v}
    - {v {"liquidity_baking_toggle_vote": "pass","adaptive_issuance_vote": "pass"} v}

    Moreover, in order to handle dynamic voting (i.e. change the
    baker's vote without having to restart it), each time a block is
    being built, the baker will try and read the vote file present in
    the config in order to check for updated votes.
*)

open Protocol.Alpha_context

(** Default vote file name that should be looked up when the baker
    starts. *)
val default_vote_json_filename : string

(** Reads the content of [per_block_vote_file] and returns the votes. If
    any error occurs (e.g. Non-existing file, unparsable content,
    etc.), given default values will be used to fill the gaps. *)
val read_per_block_votes_no_fail :
  default:Per_block_votes.per_block_votes ->
  per_block_vote_file:string ->
  Per_block_votes.per_block_votes Lwt.t

(** Load a configuration of per-block votes. Liquidity baking toggle
    vote is mandatory, it has to come from either the per-block vote
    file [per_block_vote_file] or from
    [default_liquidity_baking_vote]. If a vote cannot be determined
    from those values, this function fails. Adaptive issuance feature
    vote is optional. Priority is given to the values in the
    [per_block_vote_file] file for all votes at the time of the block
    (the file is freshly read each time). *)
val load_per_block_votes_config :
  default_liquidity_baking_vote:Per_block_votes.per_block_vote option ->
  default_adaptive_issuance_vote:Per_block_votes.per_block_vote option ->
  per_block_vote_file:string option ->
  Baking_configuration.per_block_votes_config tzresult Lwt.t
