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

module Michelson = struct
  let stress_INT_nat = file "michelson/stress_INT_nat.tz"

  let stress_DIPN = file "michelson/stress_DIPN.tz"

  (* Map a contract alias to its bundled Michelson source, for the generic
     [--contract] benchmark flag. Every contract here has storage [unit] and
     a default entrypoint taking [unit]. *)
  let by_alias = function
    | "stress_INT_nat" -> Some stress_INT_nat
    | "stress_DIPN" -> Some stress_DIPN
    | _ -> None
end

module UniswapV2 = struct
  module Factory = struct
    let json = json_file "UniSwapV2/compiled/UniswapV2Factory.json"
  end

  module Router02 = struct
    let json = json_file "UniSwapV2/compiled/UniswapV2Router02.json"
  end

  module Multicall = struct
    let json = json_file "UniSwapV2/compiled/Multicall.json"
  end

  module GLDToken = struct
    let json = json_file "UniSwapV2/compiled/GLDToken.json"
  end
end
