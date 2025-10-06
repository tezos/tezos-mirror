(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let file f () =
  match Static_contracts.read f with
  | Some x -> x
  | None -> failwith ("Could not find file " ^ f)

let json_file f () = file f () |> JSON.parse ~origin:f

module Snailtracer = struct
  let abi = file "snailtracer/snailtracer.abi"

  let bin = file "snailtracer/snailtracer.bin"
end
