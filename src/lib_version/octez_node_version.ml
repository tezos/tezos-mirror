(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type t = {
  version : Version.t;
  network_version : Network_version.t;
  commit_info : commit_info option;
}

and commit_info = {commit_hash : string; commit_date : string}

(** the namespace used for the node metrics *)
let namespace = "octez"

let commit_info_encoding =
  let open Data_encoding in
  conv
    (fun {commit_hash; commit_date} -> (commit_hash, commit_date))
    (fun (commit_hash, commit_date) -> {commit_hash; commit_date})
    (obj2 (req "commit_hash" string) (req "commit_date" string))

(* Locally defined encoding for Version.additional_info *)
let additional_info_encoding =
  let open Data_encoding in
  let open Version in
  union
    [
      case
        (Tag 0)
        ~title:"Dev"
        (constant "dev")
        (function Dev -> Some () | _ -> None)
        (fun () -> Dev);
      case
        (Tag 1)
        ~title:"RC"
        (obj1 (req "rc" int31))
        (function RC n -> Some n | _ -> None)
        (fun n -> RC n);
      case
        (Tag 2)
        ~title:"Release"
        (constant "release")
        (function Release -> Some () | _ -> None)
        (fun () -> Release);
      case
        (Tag 3)
        ~title:"RC_dev"
        (obj1 (req "rc_dev" int31))
        (function RC_dev n -> Some n | _ -> None)
        (fun n -> RC_dev n);
    ]

(* The encoding is defined here to keep [Version] "Data_encoding free"*)
let version_encoding =
  let open Data_encoding in
  conv
    (fun ({product = _; major; minor; additional_info} : Version.t) ->
      (major, minor, additional_info))
    (fun (major, minor, additional_info) ->
      {product = Octez; major; minor; additional_info})
    (obj3
       (req "major" int31)
       (req "minor" int31)
       (req "additional_info" additional_info_encoding))

let encoding =
  let open Data_encoding in
  conv
    (fun {version; network_version; commit_info} ->
      (version, network_version, commit_info))
    (fun (version, network_version, commit_info) ->
      {version; network_version; commit_info})
    (obj3
       (req "version" version_encoding)
       (req "network_version" Network_version.encoding)
       (req "commit_info" (option commit_info_encoding)))
