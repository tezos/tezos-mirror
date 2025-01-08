(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

open TzCore

(** Extension of the open type [error] with the errors that could be raised by
    the DAL node. *)
type error +=
  | Download_status of {status : Cohttp.Code.status_code}
  | Mismatched_SHA of {expected_sha : Hex.t; computed_sha : Hex.t}

let () =
  register_error_kind
    `Permanent
    ~id:"dal_node.download_error"
    ~title:"Download failed"
    ~description:"Download failed"
    ~pp:(fun ppf error_code ->
      Format.fprintf
        ppf
        "Unable to download file. Error code %ld. Status is: %s"
        error_code
        (Cohttp.Code.reason_phrase_of_code (Int32.to_int error_code)))
    Data_encoding.(obj1 (req "error_code" int32))
    (function
      | Download_status {status} ->
          Some (Int32.of_int (Cohttp.Code.code_of_status status))
      | _ -> None)
    (fun error_code ->
      Download_status
        {status = Cohttp.Code.status_of_code (Int32.to_int error_code)}) ;
  register_error_kind
    `Permanent
    ~id:"dal_node.mismatched_sha"
    ~title:"SHA256 of downloaded file is not the expected one"
    ~description:"SHA256 of downloaded file is not the expected one"
    ~pp:(fun ppf (expected_sha, obtained_sha) ->
      Format.fprintf
        ppf
        "SHA of downloaded file is not the expected one.\n\
         Expected: %s\n\
         Obtained: %s"
        expected_sha
        obtained_sha)
    Data_encoding.(obj2 (req "expected_sha" string) (req "computed_sha" string))
    (function
      | Mismatched_SHA
          {
            expected_sha = `Hex expected_sha_str;
            computed_sha = `Hex computed_sha_str;
          } ->
          Some (expected_sha_str, computed_sha_str)
      | _ -> None)
    (fun (expected_sha_str, computed_sha_str) ->
      Mismatched_SHA
        {
          expected_sha = `Hex expected_sha_str;
          computed_sha = `Hex computed_sha_str;
        })
