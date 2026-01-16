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

let commit_info_pp f ({commit_hash; _} : commit_info) =
  Format.fprintf f "%s" commit_hash

let commit_info_pp_short f ({commit_hash; _} : commit_info) =
  Format.fprintf f "%s" (String.sub commit_hash 0 8)

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
      case
        (Tag 4)
        ~title:"Beta"
        (obj1 (req "beta" int31))
        (function Beta n -> Some n | _ -> None)
        (fun n -> Beta n);
      case
        (Tag 5)
        ~title:"Beta_dev"
        (obj1 (req "beta_dev" int31))
        (function Beta_dev n -> Some n | _ -> None)
        (fun n -> Beta_dev n);
      case
        (Tag 6)
        ~title:"Rebuild"
        (obj1 (req "rebuild" int31))
        (function Rebuild n -> Some n | _ -> None)
        (fun n -> Rebuild n);
    ]

(* The encoding is defined here to keep [Version] "Data_encoding free"*)
let version_encoding =
  let open Data_encoding in
  conv
    (fun ({product = _; major; minor; build_number; additional_info} :
           Version.t)
       -> (major, minor, build_number, additional_info))
    (fun (major, minor, build_number, additional_info) ->
      {product = Octez; major; minor; build_number; additional_info})
    (obj4
       (req "major" int31)
       (req "minor" int31)
       (req "build_number" int31)
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

let commit_info_equivalent c1 c2 =
  let c1 = String.lowercase_ascii c1.commit_hash in
  let c2 = String.lowercase_ascii c2.commit_hash in
  String.starts_with ~prefix:c1 c2 || String.starts_with ~prefix:c2 c1

let partially_compare (v1 : Version.t) (c1 : commit_info option)
    (v2 : Version.t) (c2 : commit_info option) =
  let is_dev (v : Tezos_version_parser.t) =
    let open Tezos_version_parser in
    match v.additional_info with
    | Dev | Beta_dev _ | RC_dev _ -> true
    | Beta _ | RC _ | Release | Rebuild _ -> false
  in
  let is_commit_equal =
    match (c1, c2) with
    | None, _ | _, None -> None
    | Some x, Some y -> Some (commit_info_equivalent x y)
  in
  let version_comparison =
    if v1 = v2 then Some 0
    else if v1.product <> v2.product || is_dev v1 || is_dev v2 then None
    else
      let major_comp = Int.compare v1.major v2.major in
      if major_comp <> 0 then Some major_comp
      else
        let minor_comp = Int.compare v1.minor v2.minor in
        if minor_comp <> 0 then Some minor_comp
        else
          match (v1.additional_info, v2.additional_info) with
          | Beta n1, Beta n2 | RC n1, RC n2 | Rebuild n1, Rebuild n2 ->
              Some (Int.compare n1 n2)
          | Beta _, (RC _ | Release)
          | RC _, Release
          | (Release | RC _ | Beta _), Rebuild _ ->
              Some (-1)
          | Release, (RC _ | Beta _)
          | Rebuild _, (Release | RC _ | Beta _)
          | RC _, Beta _ ->
              Some 1
          | _, _ -> None
  in
  match (is_commit_equal, version_comparison) with
  | None, Some 0 ->
      if is_dev v1 then (* dev versions need to have commit_info *) None
      else Some 0
  | Some true, Some 0 -> Some 0
  | Some false, Some 0 -> None
  | _, x -> x
