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

module Events = Baking_events.Per_block_votes
open Baking_errors

let default_vote_json_filename = "per_block_votes.json"

type per_block_votes = {
  liquidity_baking_toggle_vote :
    Protocol.Alpha_context.Per_block_votes.per_block_vote;
  adaptive_issuance_vote_opt :
    Protocol.Alpha_context.Per_block_votes.per_block_vote option;
}

let vote_file_content_encoding =
  let open Data_encoding in
  def (String.concat "." [Protocol.name; "vote_file_content"])
  @@ conv
       (fun {liquidity_baking_toggle_vote; adaptive_issuance_vote_opt} ->
         (liquidity_baking_toggle_vote, adaptive_issuance_vote_opt))
       (fun (liquidity_baking_toggle_vote, adaptive_issuance_vote_opt) ->
         {liquidity_baking_toggle_vote; adaptive_issuance_vote_opt})
       (obj2
          (req
             "liquidity_baking_toggle_vote"
             Protocol.Alpha_context.Per_block_votes
             .liquidity_baking_vote_encoding)
          (opt
             "adaptive_issuance_vote"
             Protocol.Alpha_context.Per_block_votes
             .adaptive_issuance_vote_encoding))

let check_file_exists file =
  let open Lwt_result_syntax in
  let*! file_exists =
    Lwt.catch (fun () -> Lwt_unix.file_exists file) (fun _ -> Lwt.return_false)
  in
  if file_exists then return_unit else tzfail (Block_vote_file_not_found file)

let read_per_block_votes ~per_block_vote_file : 'a tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*! () = Events.(emit reading_per_block_votes) per_block_vote_file in
  let* () = check_file_exists per_block_vote_file in
  let* votes_json =
    trace
      (Block_vote_file_invalid per_block_vote_file)
      (Lwt_utils_unix.Json.read_file per_block_vote_file)
  in
  let* votes =
    trace
      (Block_vote_file_wrong_content per_block_vote_file)
      (protect (fun () ->
           return
             (Data_encoding.Json.destruct vote_file_content_encoding votes_json)))
  in
  return votes

let read_per_block_votes_no_fail ~default ~per_block_vote_file =
  let open Lwt_syntax in
  let* result = read_per_block_votes ~per_block_vote_file in
  match result with
  | Error errs ->
      let* () = Events.(emit per_block_vote_file_fail) errs in
      return default
  | Ok
      {
        liquidity_baking_toggle_vote;
        adaptive_issuance_vote_opt = Some adaptive_issuance_vote;
      } ->
      return
        Protocol.Alpha_context.Per_block_votes.
          {
            liquidity_baking_vote = liquidity_baking_toggle_vote;
            adaptive_issuance_vote;
          }
  | Ok {liquidity_baking_toggle_vote; adaptive_issuance_vote_opt = None} ->
      return {default with liquidity_baking_vote = liquidity_baking_toggle_vote}

let load_per_block_votes_config ~default_liquidity_baking_vote
    ~default_adaptive_issuance_vote ~per_block_vote_file :
    Baking_configuration.per_block_votes_config tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* If a vote file is given, it takes priority. Otherwise, we expect
     per-block vote arguments to be passed. *)
  let default_adaptive_issuance_vote =
    (* Unlike the vote for liquidity baking, the vote for adaptive
       issuance is not mandatory. *)
    match default_adaptive_issuance_vote with
    | None -> Protocol.Alpha_context.Per_block_votes.Per_block_vote_pass
    | Some default_adaptive_issuance_vote -> default_adaptive_issuance_vote
  in
  let* config =
    match (per_block_vote_file, default_liquidity_baking_vote) with
    | None, None -> tzfail Missing_vote_on_startup
    | None, Some liquidity_baking_vote ->
        return
          {
            Baking_configuration.vote_file = None;
            liquidity_baking_vote;
            adaptive_issuance_vote = default_adaptive_issuance_vote;
          }
    | Some per_block_vote_file, _ -> (
        let*! (res : _ tzresult) = read_per_block_votes ~per_block_vote_file in
        match res with
        | Ok
            {
              liquidity_baking_toggle_vote = liquidity_baking_vote;
              adaptive_issuance_vote_opt;
            } ->
            let adaptive_issuance_vote =
              Option.value
                ~default:default_adaptive_issuance_vote
                adaptive_issuance_vote_opt
            in
            return
              {
                Baking_configuration.vote_file = Some per_block_vote_file;
                liquidity_baking_vote;
                adaptive_issuance_vote;
              }
        | Error errs ->
            let*! () = Events.(emit per_block_vote_file_fail) errs in
            tzfail Missing_vote_on_startup)
  in
  let*! () =
    Events.(emit liquidity_baking_toggle_vote) config.liquidity_baking_vote
  in
  let*! () =
    Events.(emit adaptive_issuance_vote) config.adaptive_issuance_vote
  in
  return config
