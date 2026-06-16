(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Address = struct
  let prefix = "epx1"

  let encoded_size = 37

  let decoded_prefix = "\001\023\224\125"

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Zk_rollup_hash"

        let title = "A zk rollup address"

        let b58check_prefix = decoded_prefix

        let size = Some 20
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size

  include Path_encoding.Make_hex (H)

  type error += (* `Permanent *) Error_zk_rollup_address_generation

  let () =
    let open Data_encoding in
    let msg = "Error while generating rollup address" in
    register_error_kind
      `Permanent
      ~id:"rollup.error_zk_rollup_address_generation"
      ~title:msg
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" msg)
      ~description:msg
      unit
      (function Error_zk_rollup_address_generation -> Some () | _ -> None)
      (fun () -> Error_zk_rollup_address_generation)

  let from_nonce nonce =
    let open Result_syntax in
    Data_encoding.Binary.to_bytes_opt Origination_nonce.encoding nonce
    |> function
    | None -> tzfail Error_zk_rollup_address_generation
    | Some nonce -> return @@ hash_bytes [nonce]

  let of_b58data = function H.Data h -> Some h | _ -> None
end

type t = Address.t

let to_scalar x =
  Zk_rollup_scalar.of_bits
    (Data_encoding.Binary.to_string_exn Address.encoding x)

type pending_list =
  | Empty of {next_index : int64}
  | Pending of {next_index : int64; length : int}

let pending_list_encoding : pending_list Data_encoding.t =
  let open Data_encoding in
  let empty_tag, pending_tag = (0, 1) in
  let empty_encoding =
    obj1 (req "next_index" Compact.(make ~tag_size:`Uint8 int64))
  in
  let pending_encoding =
    obj2
      (req "next_index" Compact.(make ~tag_size:`Uint8 int64))
      (req "length" uint16)
  in
  matching
    (function
      | Empty {next_index} -> matched empty_tag empty_encoding next_index
      | Pending {next_index; length} ->
          matched pending_tag pending_encoding (next_index, length))
    [
      case
        ~title:"Empty"
        (Tag empty_tag)
        empty_encoding
        (function Empty {next_index} -> Some next_index | _ -> None)
        (fun next_index -> Empty {next_index});
      case
        ~title:"Pending"
        (Tag pending_tag)
        pending_encoding
        (function
          | Pending {next_index; length} -> Some (next_index, length)
          | _ -> None)
        (fun (next_index, length) -> Pending {next_index; length});
    ]

module Index = struct
  type nonrec t = t

  let path_length = 1

  let to_path c l =
    let raw_key = Data_encoding.Binary.to_bytes_exn Address.encoding c in
    let (`Hex key) = Hex.of_bytes raw_key in
    key :: l

  let of_path = function
    | [key] ->
        Option.bind
          (Hex.to_bytes (`Hex key))
          (Data_encoding.Binary.of_bytes_opt Address.encoding)
    | _ -> None

  let rpc_arg = Address.rpc_arg

  let encoding = Address.encoding

  let compare = Address.compare
end

let in_memory_size (_ : t) =
  let open Cache_memory_helpers in
  h1w +! string_size_gen Address.size

module Internal_for_tests = struct
  let originated_zk_rollup nonce =
    let data =
      Data_encoding.Binary.to_bytes_exn Origination_nonce.encoding nonce
    in
    Address.hash_bytes [data]
end
