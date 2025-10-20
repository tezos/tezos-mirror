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

module Events = Events.Per_block_votes
open Errors

let default_vote_json_filename = "per_block_votes.json"

type per_block_votes = {
  liquidity_baking_toggle_vote : Per_block_votes.per_block_vote;
  adaptive_issuance_vote_opt : Per_block_votes.per_block_vote option;
}

let vote_file_content_encoding =
  let open Data_encoding in
  def "vote_file_content"
  @@ conv
       (fun {liquidity_baking_toggle_vote; adaptive_issuance_vote_opt} ->
         (liquidity_baking_toggle_vote, adaptive_issuance_vote_opt))
       (fun (liquidity_baking_toggle_vote, adaptive_issuance_vote_opt) ->
         {liquidity_baking_toggle_vote; adaptive_issuance_vote_opt})
       (obj2
          (req
             "liquidity_baking_toggle_vote"
             Per_block_votes.liquidity_baking_vote_encoding)
          (opt
             "adaptive_issuance_vote"
             Per_block_votes.adaptive_issuance_vote_encoding))

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
  | Ok {liquidity_baking_toggle_vote; adaptive_issuance_vote_opt} ->
      let* () =
        if Option.is_some adaptive_issuance_vote_opt then
          Events.(emit deprecated_adaptive_issuance_vote_field ())
        else Lwt.return_unit
      in
      return
        Per_block_votes.{liquidity_baking_vote = liquidity_baking_toggle_vote}

let load_per_block_votes_config ~default_liquidity_baking_vote
    ~per_block_vote_file : Configuration.per_block_votes_config tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* If a vote file is given, it takes priority. Otherwise, we expect
     per-block vote arguments to be passed. *)
  let* config =
    match (per_block_vote_file, default_liquidity_baking_vote) with
    | None, None -> tzfail Missing_vote_on_startup
    | None, Some liquidity_baking_vote ->
        return {Configuration.vote_file = None; liquidity_baking_vote}
    | Some per_block_vote_file, _ -> (
        let*! (res : _ tzresult) = read_per_block_votes ~per_block_vote_file in
        match res with
        | Ok
            {
              liquidity_baking_toggle_vote = liquidity_baking_vote;
              adaptive_issuance_vote_opt;
            } ->
            let*! () =
              if Option.is_some adaptive_issuance_vote_opt then
                Events.(emit deprecated_adaptive_issuance_vote_field ())
              else Lwt.return_unit
            in
            return
              {
                Configuration.vote_file = Some per_block_vote_file;
                liquidity_baking_vote;
              }
        | Error errs ->
            let*! () = Events.(emit per_block_vote_file_fail) errs in
            tzfail Missing_vote_on_startup)
  in
  let*! () =
    Events.(emit liquidity_baking_toggle_vote) config.liquidity_baking_vote
  in
  return config

let lookup_default_vote_file_path
    (cctxt : Tezos_client_base.Client_context.full) =
  let open Lwt_syntax in
  let default_filename = default_vote_json_filename in
  let file_exists path =
    Lwt.catch (fun () -> Lwt_unix.file_exists path) (fun _ -> return_false)
  in
  let when_s pred x g =
    let* b = pred x in
    if b then return_some x else g ()
  in
  (* Check in current working directory *)
  when_s file_exists default_filename @@ fun () ->
  (* Check in the baker directory *)
  let base_dir_file = Filename.Infix.(cctxt#get_base_dir // default_filename) in
  when_s file_exists base_dir_file @@ fun () -> return_none
