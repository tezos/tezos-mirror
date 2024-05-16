(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type unsafe_patch = Increase_max_nb_ticks of int64

type t = unsafe_patch list

let patch_kinds = function Increase_max_nb_ticks _ -> [Kind.Wasm_2_0_0]

(* Patches for Etherlink PVM. *)
let etherlink_patches = [Increase_max_nb_ticks 50_000_000_000_000L]

(* Add hardcoded etherlink addresses on various networks. *)
let etherlink_addresses =
  ["sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf" (* Etherlink on Mainnet *)]

let hardcoded_patches_list =
  List.map
    (fun addr -> (Address.of_b58check_exn addr, etherlink_patches))
    etherlink_addresses

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

let make kind rollup_address patches =
  let open Result_syntax in
  let hardcoded_patches =
    List.assoc ~equal:Address.equal rollup_address hardcoded_patches_list
    |> Option.value ~default:[]
  in
  let patches = hardcoded_patches @ patches in
  let+ () =
    List.iter_e
      (fun patch ->
        if not @@ List.mem ~equal:Kind.equal kind (patch_kinds patch) then
          error_with
            "Patch \"%a\" is not supported for rollup kind %a"
            pp_unsafe_patch
            patch
            Kind.pp
            kind
        else Ok ())
      patches
  in
  patches
