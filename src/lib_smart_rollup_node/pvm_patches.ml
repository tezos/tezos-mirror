(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type unsafe_patch = Increase_max_nb_ticks of int64

type t = unsafe_patch list

(* Patches for Etherlink PVM. *)
let etherlink_patches = [Increase_max_nb_ticks 50_000_000_000_000L]

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7148
   Add hardcoded etherlink addresses on various networks. *)
let etherlink_addresses = []

let hardcoded_patches_list =
  List.map (fun addr -> (addr, etherlink_patches)) etherlink_addresses

let make rollup_address patches =
  let hardcoded_patches =
    List.assoc ~equal:Address.equal rollup_address hardcoded_patches_list
    |> Option.value ~default:[]
  in
  hardcoded_patches @ patches

let unsafe_patch_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"increase_max_nb_tick"
        (obj1 (req "increase_max_nb_tick" int64))
        (function Increase_max_nb_ticks ticks -> Some ticks)
        (fun ticks -> Increase_max_nb_ticks ticks);
    ]

let pp_unsafe_patch fmt = function
  | Increase_max_nb_ticks nb ->
      Format.fprintf fmt "Increase maximum number of ticks to %#Ld" nb
