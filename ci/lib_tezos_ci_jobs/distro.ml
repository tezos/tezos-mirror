(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* "Distribution" would be a more proper name but the "Distro" slang
   makes it more clear that we are talking about a Linux distribution
   and not about the distribution of something else. (Also it's shorter.) *)

type name = Debian | Ubuntu

type t = {name : name; release : string}

let debian release = {name = Debian; release}

let ubuntu year month = {name = Ubuntu; release = sf "%02d.%02d" year month}

let name_for_humans = function Debian -> "Debian" | Ubuntu -> "Ubuntu"

let name_for_scripts distro = String.lowercase_ascii (name_for_humans distro)

let full_name_for_humans distro =
  name_for_humans distro.name ^ " " ^ distro.release

let full_name_with_underscores distro =
  name_for_scripts distro.name
  ^ "_"
  ^ String.map (function '.' -> '_' | c -> c) distro.release

let image distro =
  let open Tezos_ci.Images.Base_images in
  match distro with
  | {name = Debian; release = "bookworm"} -> debian_bookworm
  | {name = Debian; release = "trixie"} -> debian_trixie
  | {name = Ubuntu; release = "22.04"} -> ubuntu_22_04
  | {name = Ubuntu; release = "24.04"} -> ubuntu_24_04
  | {name = Ubuntu; release = "26.04"} -> ubuntu_26_04
  | _ -> failwith ("no base image for " ^ full_name_for_humans distro)

(* Image for jobs that just need one version that works, such as [apt_repo_*]. *)
let main_image = function
  | Debian -> image (debian "trixie")
  | Ubuntu -> image (ubuntu 24 04)

let supported_releases = function
  | Debian -> ["bookworm"; "trixie"]
  | Ubuntu -> ["22.04"; "24.04"; "26.04"]
