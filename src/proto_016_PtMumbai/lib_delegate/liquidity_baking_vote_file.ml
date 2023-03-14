(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol_client_context
module Events = Baking_events.Liquidity_baking

type per_block_votes = {
  liquidity_baking_toggle_vote :
    Protocol.Alpha_context.Liquidity_baking.liquidity_baking_toggle_vote option;
}

let per_block_votes_encoding =
  let open Data_encoding in
  def "per_block_votes.alpha"
  @@ conv
       (fun {liquidity_baking_toggle_vote} -> liquidity_baking_toggle_vote)
       (fun liquidity_baking_toggle_vote -> {liquidity_baking_toggle_vote})
       (obj1
          (opt
             "liquidity_baking_toggle_vote"
             Protocol.Alpha_context.Liquidity_baking
             .liquidity_baking_toggle_vote_encoding))

type error += Block_vote_file_not_found of string

type error += Block_vote_file_invalid of string

type error += Block_vote_file_wrong_content of string

type error += Block_vote_file_missing_liquidity_baking_toggle_vote of string

type error += Missing_vote_on_startup

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.block_vote_file_not_found"
    ~title:
      "The provided block vote file path does not point to an existing file."
    ~description:
      "A block vote file path was provided on the command line but the path \
       does not point to an existing file."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file path \"%s\" does not point to an \
         existing file.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_not_found file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_not_found file_path) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.block_vote_file_invalid"
    ~title:
      "The provided block vote file path does not point to a valid JSON file."
    ~description:
      "A block vote file path was provided on the command line but the path \
       does not point to a valid JSON file."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file path \"%s\" does not point to a valid \
         JSON file. The file exists but its content is not valid JSON.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function Block_vote_file_invalid file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_invalid file_path) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.block_vote_file_wrong_content"
    ~title:"The content of the provided block vote file is unexpected."
    ~description:
      "The block vote file is valid JSON but its content is not the expected \
       one."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file \"%s\" is a valid JSON file but its \
         content is unexpected. Expecting a JSON file containing either \
         '{\"liquidity_baking_toggle_vote\": \"on\"}', or \
         '{\"liquidity_baking_toggle_vote\": \"off\"}', or \
         '{\"liquidity_baking_toggle_vote\": \"pass\"}'.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_wrong_content file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_wrong_content file_path) ;
  register_error_kind
    `Permanent
    ~id:
      "Client_baking_forge.block_vote_file_missing_liquidity_baking_toggle_vote"
    ~title:
      "In the provided block vote file, no entry for liquidity baking toggle \
       vote was found"
    ~description:
      "In the provided block vote file, no entry for liquidity baking toggle \
       vote was found."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[In the provided block vote file \"%s\", the \
         \"liquidity_baking_toggle_vote\" boolean field is missing. Expecting \
         a JSON file containing either '{\"liquidity_baking_toggle_vote\": \
         \"on\"}', or '{\"liquidity_baking_toggle_vote\": \"off\"}', or \
         '{\"liquidity_baking_toggle_vote\": \"pass\"}'.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_missing_liquidity_baking_toggle_vote file_path ->
          Some file_path
      | _ -> None)
    (fun file_path ->
      Block_vote_file_missing_liquidity_baking_toggle_vote file_path) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_forge.missing_vote_on_startup"
    ~title:"Missing vote on startup"
    ~description:
      "No CLI flag, file path, or vote file in default location provided on \
       startup"
    ~pp:(fun fmt () ->
      Format.fprintf
        fmt
        "Missing liquidity baking toggle vote, please use either the \
         --liquidity-baking-toggle-vote or --per_block_vote_file option or a \
         vote file in the default location: per_block_votes.json in the \
         current working directory.")
    Data_encoding.empty
    (function Missing_vote_on_startup -> Some () | _ -> None)
    (fun () -> Missing_vote_on_startup)

let traced_option_to_result ~error =
  Option.fold ~some:ok ~none:(Result_syntax.tzfail error)

let check_file_exists file =
  if Sys.file_exists file then Result.return_unit
  else error (Block_vote_file_not_found file)

let read_liquidity_baking_toggle_vote ~per_block_vote_file =
  Events.(emit reading_per_block) per_block_vote_file >>= fun () ->
  check_file_exists per_block_vote_file >>?= fun () ->
  trace (Block_vote_file_invalid per_block_vote_file)
  @@ Lwt_utils_unix.Json.read_file per_block_vote_file
  >>=? fun votes_json ->
  Events.(emit per_block_vote_file_notice) "found" >>= fun () ->
  trace (Block_vote_file_wrong_content per_block_vote_file)
  @@ Error_monad.protect (fun () ->
         return
         @@ Data_encoding.Json.destruct per_block_votes_encoding votes_json)
  >>=? fun votes ->
  Events.(emit per_block_vote_file_notice) "JSON decoded" >>= fun () ->
  traced_option_to_result
    ~error:
      (Block_vote_file_missing_liquidity_baking_toggle_vote per_block_vote_file)
    votes.liquidity_baking_toggle_vote
  >>?= fun liquidity_baking_toggle_vote ->
  Events.(emit reading_liquidity_baking) () >>= fun () ->
  Events.(emit liquidity_baking_toggle_vote) liquidity_baking_toggle_vote
  >>= fun () -> return liquidity_baking_toggle_vote

let read_liquidity_baking_toggle_vote_no_fail ~default ~per_block_vote_file =
  read_liquidity_baking_toggle_vote ~per_block_vote_file >>= function
  | Ok vote -> Lwt.return vote
  | Error errs ->
      Events.(emit per_block_vote_file_fail) errs >>= fun () ->
      Lwt.return default

let read_liquidity_baking_toggle_vote_on_startup ~default ~per_block_vote_file =
  read_liquidity_baking_toggle_vote ~per_block_vote_file >>= function
  | Ok vote -> return (vote, true)
  | Error errs -> (
      match default with
      | None ->
          Events.(emit per_block_vote_file_fail) errs >>= fun () ->
          fail Missing_vote_on_startup
      | Some vote -> return (vote, false))
