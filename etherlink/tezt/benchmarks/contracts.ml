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
