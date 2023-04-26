(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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
  val of_bytes : bytes -> Dac_plugin.hash
end) : Dac_plugin.T = struct
  let to_bytes = Dac_plugin.hash_to_bytes

  let to_reveal_hash dac_hash =
    dac_hash |> to_bytes
    |> Data_encoding.Binary.of_bytes_exn Protocol.Sc_rollup_reveal_hash.encoding

  let of_reveal_hash reveal_hash =
    reveal_hash
    |> Data_encoding.Binary.to_bytes_exn Protocol.Sc_rollup_reveal_hash.encoding
    |> Mapper.of_bytes

  let of_hex hex =
    Protocol.Sc_rollup_reveal_hash.of_hex hex |> Option.map of_reveal_hash

  let to_hex hash = to_reveal_hash hash |> Protocol.Sc_rollup_reveal_hash.to_hex

  let encoding =
    let binary =
      Data_encoding.conv
        to_reveal_hash
        of_reveal_hash
        Protocol.Sc_rollup_reveal_hash.encoding
    in
    Data_encoding.(
      (* Hexifies the hash when encoding in json. *)
      splitted
        ~binary
        ~json:
          (conv_with_guard
             to_hex
             (fun str ->
               Result.of_option ~error:"Not a valid hash" (of_hex str))
             (string' Plain)))

  let equal h1 h2 =
    Protocol.Sc_rollup_reveal_hash.equal (to_reveal_hash h1) (to_reveal_hash h2)

  let dac_hash_to_proto_supported_hashes = function
    | Dac_plugin.Blake2B -> Protocol.Sc_rollup_reveal_hash.Blake2B

  let proto_to_dac_hash_supported_hashes = function
    | Protocol.Sc_rollup_reveal_hash.Blake2B -> Dac_plugin.Blake2B

  let hash_string ~(scheme : Dac_plugin.supported_hashes) ?key strings =
    Protocol.Sc_rollup_reveal_hash.hash_string
      ~scheme:(dac_hash_to_proto_supported_hashes scheme)
      ?key
      strings
    |> of_reveal_hash

  let hash_bytes ~(scheme : Dac_plugin.supported_hashes) ?key bytes =
    Protocol.Sc_rollup_reveal_hash.hash_bytes
      ~scheme:(dac_hash_to_proto_supported_hashes scheme)
      ?key
      bytes
    |> of_reveal_hash

  let scheme_of_hash hash =
    to_reveal_hash hash |> Protocol.Sc_rollup_reveal_hash.scheme_of_hash
    |> proto_to_dac_hash_supported_hashes

  let size ~scheme =
    Protocol.Sc_rollup_reveal_hash.size
      ~scheme:(dac_hash_to_proto_supported_hashes scheme)

  module Proto = Registerer.Registered

  let raw_to_hash raw_hash =
    let of_bytes_opt =
      Data_encoding.Binary.of_bytes_opt
        Protocol.Sc_rollup_reveal_hash.encoding
        (Dac_plugin.raw_hash_to_bytes raw_hash)
    in
    match of_bytes_opt with
    | Some hash -> Ok (of_reveal_hash hash)
    | None ->
        Result_syntax.tzfail
        @@ Dac_plugin.Cannot_convert_raw_hash_to_hash
             {raw_hash; proto = Proto.hash}
end

let make_plugin : (bytes -> Dac_plugin.hash) -> (module Dac_plugin.T) =
 fun of_bytes ->
  let module Plugin = Make (struct
    let of_bytes = of_bytes
  end) in
  (module Plugin)

let () = Dac_plugin.register make_plugin
