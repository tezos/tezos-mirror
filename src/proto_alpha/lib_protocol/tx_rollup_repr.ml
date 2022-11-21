(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Marigold <contact@marigold.dev>                   *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type error += (* `Permanent *) Invalid_rollup_notation of string

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"rollup.invalid_tx_rollup_notation"
    ~title:"Invalid tx rollup notation"
    ~pp:(fun ppf x -> Format.fprintf ppf "Invalid tx rollup notation %S" x)
    ~description:
      "A malformed tx rollup notation was given to an RPC or in a script."
    (obj1 (req "notation" (string Plain)))
    (function Invalid_rollup_notation loc -> Some loc | _ -> None)
    (fun loc -> Invalid_rollup_notation loc)

module Hash = struct
  let rollup_hash = Tx_rollup_prefixes.rollup_address.b58check_prefix

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Rollup_hash"

        let title = "A rollup ID"

        let b58check_prefix = rollup_hash

        let size = Some Tx_rollup_prefixes.rollup_address.hash_size
      end)

  include H

  let () = Tx_rollup_prefixes.(check_encoding rollup_address b58check_encoding)

  include Path_encoding.Make_hex (H)
end

type t = Hash.t

module Compare_impl = Compare.Make (struct
  type nonrec t = t

  let compare r1 r2 = Hash.compare r1 r2
end)

include Compare_impl

let in_memory_size _ =
  let open Cache_memory_helpers in
  header_size +! word_size
  +! string_size_gen Tx_rollup_prefixes.rollup_address.hash_size

let to_b58check rollup = Hash.to_b58check rollup

let of_b58data = function Hash.Data hash -> Some hash | _ -> None

let of_b58check_opt s = Option.bind (Base58.decode s) of_b58data

let of_b58check s =
  match of_b58check_opt s with
  | Some hash -> ok hash
  | _ -> error (Invalid_rollup_notation s)

let pp ppf hash = Hash.pp ppf hash

let encoding =
  let open Data_encoding in
  def
    "tx_rollup_id"
    ~title:"A tx rollup handle"
    ~description:
      "A tx rollup notation as given to an RPC or inside scripts, is a base58 \
       tx rollup hash"
  @@ splitted
       ~binary:Hash.encoding
       ~json:
         (conv
            to_b58check
            (fun s ->
              match of_b58check s with
              | Ok s -> s
              | Error _ -> Json.cannot_destruct "Invalid tx rollup notation.")
            (string Plain))

let originated_tx_rollup nonce =
  let data =
    Data_encoding.Binary.to_bytes_exn Origination_nonce.encoding nonce
  in
  Hash.hash_bytes [data]

let rpc_arg =
  let construct = to_b58check in
  let destruct hash =
    Result.map_error (fun _ -> "Cannot parse tx rollup id") (of_b58check hash)
  in
  RPC_arg.make
    ~descr:"A tx rollup identifier encoded in b58check."
    ~name:"tx_rollup_id"
    ~construct
    ~destruct
    ()

module Index = struct
  type nonrec t = t

  let path_length = 1

  let to_path c l =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding c in
    let (`Hex key) = Hex.of_bytes raw_key in
    key :: l

  let of_path = function
    | [key] ->
        Option.bind
          (Hex.to_bytes (`Hex key))
          (Data_encoding.Binary.of_bytes_opt encoding)
    | _ -> None

  let rpc_arg = rpc_arg

  let encoding = encoding

  let compare = compare
end

module Cmp = struct
  type nonrec t = t

  let compare = compare
end

module Set = Set.Make (Cmp)
module Map = Map.Make (Cmp)
