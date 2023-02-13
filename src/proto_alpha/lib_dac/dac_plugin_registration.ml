(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

module Make (Mapper : sig
  val of_bytes : bytes -> Dac_plugin.Dac_hash.t
end) : Dac_plugin.T = struct
  module Dac_hash = struct
    let to_bytes = Dac_plugin.Dac_hash.to_bytes

    let to_reveal_hash dac_hash =
      dac_hash |> to_bytes
      |> Data_encoding.Binary.of_bytes_exn
           Protocol.Sc_rollup_reveal_hash.encoding

    let of_reveal_hash reveal_hash =
      reveal_hash
      |> Data_encoding.Binary.to_bytes_exn
           Protocol.Sc_rollup_reveal_hash.encoding
      |> Mapper.of_bytes

    let encoding =
      Data_encoding.conv
        to_reveal_hash
        of_reveal_hash
        Protocol.Sc_rollup_reveal_hash.encoding
  end

  module Proto = Registerer.Registered
  module RPC = RPC
end

let make_plugin : (bytes -> Dac_plugin.Dac_hash.t) -> (module Dac_plugin.T) =
 fun of_bytes ->
  let module Plugin = Make (struct
    let of_bytes = of_bytes
  end) in
  (module Plugin)

let () = Dac_plugin.register make_plugin
