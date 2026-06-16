(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type unsafe_patch =
  | Increase_max_nb_ticks of int64
  | Patch_durable_storage of {key : string; value : string}
  | Patch_PVM_version of {version : string}

type kind = Hardcoded | User_provided

type t = (unsafe_patch * kind) list

let patch_kinds = function
  | Increase_max_nb_ticks _ -> [Kind.Wasm_2_0_0]
  | Patch_durable_storage _ -> [Kind.Wasm_2_0_0]
  | Patch_PVM_version _ -> [Kind.Wasm_2_0_0]

(* Patches for Etherlink PVM. *)
let etherlink_patches = [(Increase_max_nb_ticks 50_000_000_000_000L, Hardcoded)]

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
        (function Increase_max_nb_ticks ticks -> Some ticks | _ -> None)
        (fun ticks -> Increase_max_nb_ticks ticks);
      case
        (Tag 1)
        ~title:"patch_durable_storage"
        (obj1
           (req
              "patch_durable_storage"
              (obj2 (req "key" string) (req "value" (string' Hex)))))
        (function
          | Patch_durable_storage {key; value} -> Some (key, value) | _ -> None)
        (fun (key, value) -> Patch_durable_storage {key; value});
    ]

let pp_unsafe_patch fmt = function
  | Increase_max_nb_ticks nb ->
      Format.fprintf fmt "Increase maximum number of ticks to %#Ld" nb
  | Patch_durable_storage {key; value} ->
      Format.fprintf
        fmt
        "Rewrites value at %s with %s"
        key
        Hex.(of_string value |> show)
  | Patch_PVM_version {version} ->
      Format.fprintf fmt "Set PVM version to %s" version

let make kind rollup_address user_provided_patches =
  let open Result_syntax in
  let hardcoded_patches =
    List.assoc ~equal:Address.equal rollup_address hardcoded_patches_list
    |> Option.value ~default:[]
  in
  let patches =
    hardcoded_patches
    @ List.map (fun p -> (p, User_provided)) user_provided_patches
  in
  let+ () =
    List.iter_e
      (fun (patch, _kind) ->
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
