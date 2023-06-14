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
    - {v {"adaptive_inflation_vote": "on"} v}
    - {v {"adaptive_inflation_vote": "off"} v}
    - {v {"adaptive_inflation_vote": "pass"} v}
    - {v {"liquidity_baking_toggle_vote": "on","adaptive_inflation_vote": "on"} v}
    - {v {"liquidity_baking_toggle_vote": "on","adaptive_inflation_vote": "off"} v}
    - {v {"liquidity_baking_toggle_vote": "on","adaptive_inflation_vote": "pass"} v}
    - {v {"liquidity_baking_toggle_vote": "off","adaptive_inflation_vote": "on"} v}
    - {v {"liquidity_baking_toggle_vote": "off","adaptive_inflation_vote": "off"} v}
    - {v {"liquidity_baking_toggle_vote": "off","adaptive_inflation_vote": "pass"} v}
    - {v {"liquidity_baking_toggle_vote": "pass","adaptive_inflation_vote": "on"} v}
    - {v {"liquidity_baking_toggle_vote": "pass","adaptive_inflation_vote": "off"} v}
    - {v {"liquidity_baking_toggle_vote": "pass","adaptive_inflation_vote": "pass"} v}

    Moreover, in order to handle dynamic voting (i.e. change the
    baker's vote without having to restart it), each time a block is
    being built, the baker will try and read the vote file present in
    the config in order to check for updated votes.
*)

open Protocol.Alpha_context

type error += Block_vote_file_not_found of string

type error += Block_vote_file_invalid of string

type error += Block_vote_file_wrong_content of string

type error += Block_vote_file_missing_liquidity_baking_toggle_vote of string

type error += Missing_vote_on_startup

(** Default vote file name that should be looked up when the baker
    starts. *)
val default_vote_json_filename : string

(** Reads the content of [per_block_vote_file] and returns the votes. If
    any error occurs (e.g. Non-existing file, unparsable content,
    etc.), given default values will be used to fill the gaps. *)
val read_toggle_votes_no_fail :
  default:Toggle_votes.toggle_votes ->
  per_block_vote_file:string ->
  Toggle_votes.toggle_votes Lwt.t

(** Load a liquidity baking configuration given two possible
    arguments. If neither are provided, it fails. Otherwise, it tries,
    in priority, to read the [per_block_vote_file_arg] file if it is
    given and loads a config using its content. Otherwise, the
    [toggle_vote_arg] is used. *)
val load_toggle_votes_config :
  default_liquidity_baking_vote:Toggle_votes.toggle_vote option ->
  default_adaptive_inflation_vote:Toggle_votes.toggle_vote option ->
  per_block_vote_file:string option ->
  Baking_configuration.toggle_votes_config tzresult Lwt.t
