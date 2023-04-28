(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

type hash = bytes

type raw_hash = bytes

let hash_to_bytes = Fun.id

let raw_hash_to_bytes = Fun.id

let hash_to_hex hash = Hex.of_bytes hash

type supported_hashes = Blake2B

let raw_compare = Bytes.compare

let raw_hash_encoding = Data_encoding.bytes' Hex

let hash_to_raw = Fun.id

let raw_hash_to_hex raw_hash =
  let (`Hex hash) = Hex.of_bytes raw_hash in
  hash

let raw_hash_of_hex hex = Hex.to_bytes (`Hex hex)

let raw_hash_rpc_arg =
  let construct = raw_hash_to_hex in
  let destruct hash =
    match raw_hash_of_hex hash with
    | None -> Error "Impossible to parse raw_hash"
    | Some reveal_hash -> Ok reveal_hash
  in
  Tezos_rpc.Arg.make
    ~descr:"A reveal hash"
    ~name:"reveal_hash"
    ~destruct
    ~construct
    ()

type cannot_convert_raw_hash_to_hash = {
  raw_hash : raw_hash;
  proto : Protocol_hash.t;
}

type error += Cannot_convert_raw_hash_to_hash of cannot_convert_raw_hash_to_hash

let () =
  register_error_kind
    `Permanent
    ~id:"cannot_convert_raw_hash_to_hash"
    ~title:"Impossible to retrieve hash from raw_hash"
    ~description:
      "Impossible to validate the provided raw_hash against the protocol"
    ~pp:(fun ppf (raw_hash, proto) ->
      Format.fprintf
        ppf
        "Impossible to validate the provided raw_hash %s against the actual \
         protocol %s and transform it to a valid hash"
        (raw_hash_to_hex raw_hash)
        (Protocol_hash.to_string proto))
    Data_encoding.(
      obj2
        (req "raw_hash" raw_hash_encoding)
        (req "proto" Protocol_hash.encoding))
    (function
      | Cannot_convert_raw_hash_to_hash {raw_hash; proto} ->
          Some (raw_hash, proto)
      | _ -> None)
    (fun (raw_hash, proto) -> Cannot_convert_raw_hash_to_hash {raw_hash; proto})

module type T = sig
  val encoding : hash Data_encoding.t

  val equal : hash -> hash -> bool

  val hash_string :
    scheme:supported_hashes -> ?key:string -> string list -> hash

  val hash_bytes : scheme:supported_hashes -> ?key:bytes -> bytes list -> hash

  val scheme_of_hash : hash -> supported_hashes

  val of_hex : string -> hash option

  val to_hex : hash -> string

  val size : scheme:supported_hashes -> int

  module Proto : Registered_protocol.T
end

type t = (module T)

let raw_to_hash ((module Plugin) : t) raw_hash =
  let of_bytes_opt =
    Data_encoding.Binary.of_bytes_opt Plugin.encoding raw_hash
  in
  match of_bytes_opt with
  | Some hash -> Ok hash
  | None ->
      Result_syntax.tzfail
      @@ Cannot_convert_raw_hash_to_hash {raw_hash; proto = Plugin.Proto.hash}

let table : t Protocol_hash.Table.t = Protocol_hash.Table.create 5

let register (make_plugin : (bytes -> hash) -> t) : unit =
  let dac_plugin = make_plugin Fun.id in
  let module Plugin = (val dac_plugin) in
  assert (not (Protocol_hash.Table.mem table Plugin.Proto.hash)) ;
  Protocol_hash.Table.add table Plugin.Proto.hash (module Plugin)

let get hash = Protocol_hash.Table.find table hash
